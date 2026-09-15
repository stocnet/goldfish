# =========================================================================== #
# Walk handle: a stateful stepper over the merged single-clock walk.
#
# `preprocess_joint()` runs the merged walk in one batch pass and returns one
# `goldfishStat` per fid. A *generative* driver (the DyNES augmenter,
# `simulate()`) instead needs to step the walk: advance the clock, evaluate a
# fid at the live state and parameters, and inject an event (observed or
# sampled)
# that every consumer then sees. This file provides that handle.
#
# The handle owns the same substrate the batch driver builds
# (`build_merged_blocks()`): one shared process state, one per-process compiled
# engine (`build_walk_engine()`), and the shared event schedule. Where the batch
# driver accumulates each engine's statistics into per-fid consumers and
# finalizes them, the handle maintains a LIVE dense statistics matrix per engine
# and reads it on demand:
#
#   * walk_advance(handle, t)  applies the EXOGENOUS covariate events (a layer
#     no process models) up to t, folding their statistics deltas into every
#     engine
#     that reads them and advancing the shared clock. Driver-scheduled events
#     and node-composition changes up to t are applied on the same pass.
#   * walk_schedule(handle, event)  queues an event for a later time; it is
#     applied when walk_advance() reaches it, merged with the exogenous rows.
#     walk_next_breakpoint(handle) names the next time the handle changes
#     state on its own, between which rates are constant.
#   * walk_evaluate(handle, fid, theta)  builds the process-state evaluator's
#     `state` from the fid's engine live statistics (projected to the fid's own
#     columns, intercept prepended for a timed rate) and returns that fid's rate
#     vector / choice matrix at theta -- a thin wrapper over
#     `evaluate_process_state()`, reusing the estimation-path math verbatim.
#   * walk_inject(handle, event)  applies one event (observed or sampled) to the
#     shared state as a covariate update on its layer, so every engine reading
#     that layer sees it on the next evaluation.
#
# Batch-vs-replay equality is the contract: folding the same point / broadcast
# blocks the batch consumers store, the handle's live statistics at a fid's k-th
# event equal `materialize_process_state()` of the batch output at event k, so
# `walk_evaluate()` agrees with the batch path event-for-event.
#
# Three contracts carry over from the merged walk:
#
#   * PER-FID FOCAL. Each engine was compiled against its own focal, so a
#     fid's side/mode/dependent-row resolution rides its own compiled spec_map
#     over the one shared state -- never a stamped shared focal. walk_evaluate
#     resolves the evaluated fid against that per-fid view.
#
#   * GENERATIVE COMPLETENESS. walk_open ASSERTS that every modeled DyNAM flavor
#     carries both a rate and a choice, and aborts pointing to simulate() /
#     estimate_dynes() otherwise. It NEVER performs completion (that transform,
#     complete_generative_spec(), runs once at the consumer entry), so a skipped
#     transform fails loudly here rather than opening a mismatched fid set.
#
#   * INTERNAL SUBSTRATE. The handle is a developer substrate, not a user API;
#     it is documented (@keywords internal) and experimental, not exported.
# =========================================================================== #

# --------------------------------------------------------------------------- #
# Generative-completeness assertion.
# --------------------------------------------------------------------------- #

# Abort unless every process of every specification carries all its expected
# sub-model families (a DyNAM flavor both a rate and a choice; REM a rate). This
# is the walk handle's precondition: completion (complete_generative_spec())
# runs once at the consumer entry, so a spec reaching walk_open with a gap
# means the transform was skipped -- fail loudly, naming the incomplete flavor
# and pointing to the surfaces that DO complete.
assert_generatively_complete <- function(
  joint_spec,
  call = rlang::caller_env()
) {
  gaps <- list()
  for (spec in joint_spec$specifications) {
    needed <- expected_families(spec$model)
    for (proc in spec_processes(spec)) {
      missing <- setdiff(needed, names(proc$submodels))
      for (family in missing) {
        gaps[[length(gaps) + 1L]] <- data.frame(
          layer = spec$focal,
          flavor = proc$flavor,
          family = family,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(gaps) == 0L) {
    return(invisible(NULL))
  }
  g <- do.call(rbind, gaps)
  # No fid exists for a missing sub-model, so render off a throwaway
  # row-indexed map rather than the joint spec's real process_map.
  g$fid <- seq_len(nrow(g))
  labels <- render_process_label(g, g$fid)
  cli::cli_abort(
    c(
      "{.fn walk_open} needs a generatively complete specification.",
      "x" = "Missing sub-model{?s}: {.val {labels}}.",
      "i" = "Run the generative-completion transform first: {.fn simulate} and
             {.fn estimate_dynes} complete a half-specified spec (every modeled
             DyNAM flavor gains both a rate and a choice) at their entry.
             {.fn walk_open} only opens an already-complete spec."
    ),
    call = call,
    class = "goldfish_walk_incomplete_spec"
  )
}

# The sub-models carrying no effects: a completion-supplied uniform choice or
# pinned intercept-only rate. The merged compile parses effect-bearing
# formulas, so a `~ 1` block has nothing for it to compile; it is the
# generative consumers' to evaluate. Detected on the term labels, so it is
# independent of the bundle's internal shape. Returns one row per such
# sub-model, in specification order.
effect_free_submodels <- function(joint_spec) {
  rows <- list()
  for (spec in joint_spec$specifications) {
    for (proc in spec_processes(spec)) {
      for (family in names(proc$submodels)) {
        formula <- proc$submodels[[family]]$formula
        labels <- attr(stats::terms(formula), "term.labels")
        if (length(labels) == 0L) {
          rows[[length(rows) + 1L]] <- data.frame(
            layer = spec$focal,
            flavor = proc$flavor,
            family = family,
            model = spec$model,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      layer = character(0),
      flavor = character(0),
      family = character(0),
      model = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# The label a deferred sub-model is known by on both sides of the partition.
# `render_process_label()` reads a process_map row; the deferred sub-models are
# gone from the walk's map by construction, so their label is assembled from
# the same three fields in the same order.
effect_free_labels <- function(deferred) {
  vapply(
    seq_len(nrow(deferred)),
    function(i) {
      paste(
        c(
          deferred$layer[i],
          if (!is.na(deferred$flavor[i])) deferred$flavor[i],
          deferred$family[i]
        ),
        collapse = " › "
      )
    },
    character(1)
  )
}

# Abort when any sub-model carries no effects. This is the refusal every caller
# but a generative consumer gets: a clear boundary rather than the cryptic parse
# error the merged compile would raise.
assert_walkable_submodels <- function(joint_spec, call = rlang::caller_env()) {
  deferred <- effect_free_submodels(joint_spec)
  if (nrow(deferred) == 0L) {
    return(invisible(NULL))
  }
  empty <- effect_free_labels(deferred)
  cli::cli_abort(
    c(
      "The walk handle does not yet walk effect-free sub-models.",
      "x" = "Zero-effect sub-model{?s}: {.val {empty}}.",
      "i" = "Auto-supplied uniform / pinned defaults are walked by the
             generative consumers ({.fn simulate}, {.fn estimate_dynes}); the
             handle here
             steps effect-bearing sub-models."
    ),
    call = call,
    class = "goldfish_walk_unsupported"
  )
}

# The same specification with its effect-free sub-models removed, so the merged
# compile sees only what it can parse. A process left with no sub-model at all
# is dropped, and the process_map is rebuilt -- which RENUMBERS the fids, so the
# walk's fids are not the completed specification's. The rendered process label
# is the identity across that boundary, exactly as it is for a parameter object
# built before completion.
drop_effect_free_submodels <- function(joint_spec) {
  specs <- lapply(joint_spec$specifications, function(spec) {
    keep_bearing <- function(submodels) {
      bearing <- vapply(
        submodels,
        function(bundle) {
          length(attr(stats::terms(bundle$formula), "term.labels")) > 0L
        },
        logical(1)
      )
      submodels[bearing]
    }
    if (!is.null(spec$processes)) {
      for (fl in names(spec$processes)) {
        spec$processes[[fl]]$submodels <- keep_bearing(
          spec$processes[[fl]]$submodels
        )
      }
      empty <- vapply(
        spec$processes,
        function(p) length(p$submodels) == 0L,
        logical(1)
      )
      spec$processes <- spec$processes[!empty]
    } else {
      spec$submodels <- keep_bearing(spec$submodels)
    }
    spec
  })
  carries <- vapply(
    specs,
    function(spec) {
      if (!is.null(spec$processes)) {
        length(spec$processes) > 0L
      } else {
        length(spec$submodels) > 0L
      }
    },
    logical(1)
  )
  specs <- specs[carries]
  joint_spec$specifications <- specs
  joint_spec$process_map <- build_joint_process_map(
    specs,
    joint_spec$modeled_panel %||% character(0)
  )
  joint_spec
}

# --------------------------------------------------------------------------- #
# Opening the handle.
# --------------------------------------------------------------------------- #

# The process-state evaluator model type for one engine, from its model /
# sub-model shape and whether the fid carries the timed baseline hazard
# (has_intercept). The timed/ordered split is the rate-side distinction the
# evaluators dispatch on; choice / coordination are regime-agnostic.
walk_engine_model_type <- function(engine, has_intercept) {
  if (identical(engine$model, "REM")) {
    return(if (has_intercept) "REM" else "REM-ordered")
  }
  if (engine$is_sender) {
    return(
      if (identical(engine$sub_model, "rate")) {
        "DyNAM-M-Rate"
      } else {
        "DyNAM-M-Rate-ordered"
      }
    )
  }
  if (identical(engine$sub_model, "choice_coordination")) {
    return("DyNAM-MM")
  }
  "DyNAM-M"
}

# Seed one engine's LIVE dense statistics from its walk-start `initial_stats`,
# in the sender-major layout the process-state evaluators use: a sender (rate)
# engine keeps its n1 x nEffects matrix as-is; a dyad engine flattens its
# n1 x n2 x nEffects array to (n1*n2) x nEffects with dyad (i, j) at row
# (i - 1) * n2 + j (matching materialize_process_state()).
walk_seed_live_stats <- function(engine) {
  if (engine$is_sender) {
    return(engine$initial_stats)
  }
  n1 <- engine$ctx$n1
  n2 <- engine$ctx$n2
  n_eff <- engine$ctx$nEffects
  flat <- matrix(0, n1 * n2, n_eff)
  for (e in seq_len(n_eff)) {
    flat[, e] <- as.vector(t(engine$initial_stats[,, e]))
  }
  flat
}

# Adapt a batch walk engine for stepping: replace its per-fid consumers with ONE
# identity recorder (so `merged_covariate_step()` accumulates every block in
# union-gid space, unprojected), seed the live dense statistics, and stamp the
# fold parameters the handle needs. The recorder's pending buffers are drained
# into `live_stats` after each event (walk_fold_engine()).
walk_prepare_engine <- function(engine) {
  recorder <- new_consumer(
    writer_default(),
    gid_lookup = NULL,
    is_exact_time = FALSE
  )
  engine$consumers <- list(recorder)
  engine$rc_consumers <- list()
  engine$recorder <- recorder
  engine$live_stats <- walk_seed_live_stats(engine)
  engine$n1 <- engine$ctx$n1
  engine$n2 <- engine$ctx$n2
  # The receiver stride the flattened statistics fold on: 1 for a sender (rate)
  # engine (its live matrix is n1 x nEffects), the true n2 for a dyad engine.
  engine$fold_n2 <- if (engine$is_sender) 1L else engine$ctx$n2
  # The reflexive rule the fold and the evaluator share: a rate engine reaches
  # every actor, a one-mode dyad engine drops the self-loop cell.
  engine$twomode_or_reflexive <- engine$is_sender ||
    isTRUE(engine$model_spec$is_two_mode)
  # Live presence per node side, seeded from the context. A rate engine keeps
  # its receiver side too: a constrained sender gate counts present receivers.
  engine$presence1 <- as.logical(engine$ctx$active_sender_init)
  engine$presence2 <- as.logical(engine$ctx$active_dyad_init)
  engine$presence1_changes <- walk_presence_changes(
    engine$ctx$active_sender_changes
  )
  engine$presence2_changes <- walk_presence_changes(
    engine$ctx$active_dyad_changes
  )
  engine$presence1_cursor <- 0L
  engine$presence2_cursor <- 0L
  engine
}

# Drain the recorder's pending point / broadcast blocks (accumulated in
# union-gid space by `merged_covariate_step()`) into the engine's live
# statistics,
# then reset the buffers. Reuses the estimation-path fold helpers
# (`.gather_stat_cells` / `.gather_broadcast_blocks`), so the live statistics
# evolve byte-identically to the batch consumers' stored update streams. The
# write stays here rather than inside those helpers so `live_stats` is never
# bound to a second name, which is what made the old fold duplicate the whole
# matrix on every event.
walk_fold_engine <- function(engine) {
  rec <- engine$recorder
  if (rec$pending_dep_cols > 0L) {
    block <- do.call(cbind, rec$pending_dep)
    cells <- .gather_stat_cells(block, 0L, ncol(block), engine$fold_n2)
    if (!is.null(cells)) {
      engine$live_stats[cells$idx] <- cells$value
    }
  }
  if (rec$pending_dep_bc_cols > 0L) {
    bc <- do.call(cbind, rec$pending_dep_bc)
    blocks <- .gather_broadcast_blocks(
      bc,
      0L,
      ncol(bc),
      engine$n1,
      engine$fold_n2,
      engine$twomode_or_reflexive
    )
    for (block in blocks) {
      engine$live_stats[block$rows, block$col] <- block$value
    }
  }
  rec$pending_dep <- list()
  rec$pending_dep_cols <- 0L
  rec$pending_dep_bc <- list()
  rec$pending_dep_bc_cols <- 0L
  invisible(NULL)
}

#' Open, step, evaluate, and inject over the merged single-clock walk
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A stateful developer substrate over the merged multivariate walk, for a
#' generative driver (the DyNES augmenter, a future `simulate()`) that must step
#' the clock, evaluate a process at the live state, and apply sampled or
#' observed events mid-walk. `walk_open()` builds the handle; `walk_advance()`
#' moves the clock applying exogenous events; `walk_evaluate()` returns a fid's
#' rate vector or choice matrix at the current state and parameters (its
#' support applied); `walk_inject()` applies one event to the shared state so
#' every consumer sees it.
#'
#' These are internal building blocks: not exported, committing to no
#' user-facing signature. `walk_open()` **asserts** its specification is
#' generatively complete (every modeled DyNAM flavor carrying both a rate and a
#' choice) and aborts pointing to [simulate()] / `estimate_dynes()` otherwise;
#' it never performs completion itself.
#'
#' @param spec a `goldfishJointSpec` from
#'   [make_joint_specification()] (or a single `goldfishSpec`, wrapped
#'   as a one-process join).
#' @param control_preprocessing preprocessing options, as for
#'   [preprocess_joint()].
#' @param completed what to do with a sub-model carrying no effects — an
#'   auto-supplied uniform choice or pinned intercept-only rate. `"abort"`
#'   refuses it, which is the boundary every caller but a generative consumer
#'   wants; `"defer"` leaves it out of the walk and records it on the handle as
#'   `deferred`, for the caller to evaluate itself. Deferring renumbers the
#'   walk's fids relative to the specification's, so a caller holding both
#'   matches them on the rendered process label.
#' @param call the calling environment, for error reporting.
#'
#' @return `walk_open()` returns a `goldfishWalk` object.
#' @keywords internal
#' @name walk_handle
walk_open <- function(
  spec,
  control_preprocessing = set_preprocessing_opt(),
  completed = c("abort", "defer"),
  call = rlang::caller_env()
) {
  completed <- match.arg(completed)
  lifecycle::signal_stage("experimental", "walk_open()")

  joint_spec <- if (inherits(spec, "goldfishJointSpec")) {
    spec
  } else if (inherits(spec, "goldfishSpec")) {
    single_process_joint(spec)
  } else {
    cli::cli_abort(
      "{.fn walk_open} requires a {.cls goldfishSpec} or
       {.cls goldfishJointSpec}.",
      call = call
    )
  }

  # The generative-completeness assert (never completion): a spec that skipped
  # complete_generative_spec() fails loudly here rather than opening a walk with
  # a mismatched fid set.
  assert_generatively_complete(joint_spec, call)

  # A generatively complete spec MAY carry auto-supplied zero-parameter defaults
  # (a uniform choice, a pinned intercept-only rate). Those effect-free
  # sub-models are the generative consumers' (simulate() / estimate_dynes())
  # domain: the merged compile parses effect-bearing formulas, so evaluating a
  # `~ 1` block on the live walk is deferred to them. `completed = "abort"` is
  # the refusal every other caller gets -- a clear boundary rather than a
  # cryptic parse error. A generative consumer passes `"defer"` and takes the
  # deferred sub-models back on the handle to evaluate itself; the walk then
  # carries only the effect-bearing ones, which is what it could compile all
  # along.
  deferred <- effect_free_submodels(joint_spec)
  walk_spec <- joint_spec
  if (nrow(deferred) > 0L) {
    if (identical(completed, "abort")) {
      assert_walkable_submodels(joint_spec, call)
    }
    walk_spec <- drop_effect_free_submodels(joint_spec)
  }

  merged <- build_merged_blocks(walk_spec, control_preprocessing)

  # What the handle does with the substrate: it hosts window effects (their
  # expiry rows are ordinary covariate rows, exogenous to every process here,
  # stepped through walk_advance() like any other), but it reads no explicit
  # window bounds -- `control_preprocessing` reaches only the imputation
  # policy below. The clock opens at the schedule's first time and no end
  # extent is computed, so an expiry row past the last real event is stepped
  # (the state and the live statistics advance to it) but writes nothing:
  # the engines carry an identity recorder and no right-censoring consumers,
  # so there is no row to emit there. A driver that needs the handle bounded
  # takes the bound from resolve_walk_extent(). Node-composition changes are
  # not schedule rows: they are presence cursors each engine advances with the
  # clock, in walk_advance() and before an injected event is applied.

  props <- build_shared_object_props(
    merged$objects,
    control_preprocessing$impute
  )

  engines <- lapply(
    merged$units,
    function(unit) {
      engine <- build_walk_engine(
        unit,
        merged,
        control_preprocessing,
        progress = FALSE
      )
      walk_prepare_engine(engine)
    }
  )

  # Support constraints (a user's, and the masks a flavored layer derives) are
  # maintained live: their atoms advance with every object event over the
  # shared state, through the same stores the batch walk records into, and a
  # fid's mask is recomputed from the atom entries that moved since it was last
  # read.
  walk_assert_covered_constraints(engines, merged$objects$key, call)
  recorders <- build_walk_recorders(engines, merged$objects$key)
  live_masks <- walk_build_live_masks(engines, recorders)

  schedule <- merged$schedule
  start_time <- if (schedule$n > 0L) min(schedule$time) else 0
  for (engine in engines) {
    engine$last_time <- start_time
  }

  # The layers some process models (its dependent events are injected by the
  # driver, not auto-applied); every other covariate layer is exogenous and
  # advanced through walk_advance().
  focal_layers <- unique(vapply(merged$units, `[[`, character(1), "focal"))
  exo_rows <- which(!schedule$dependent & !(schedule$layer %in% focal_layers))

  handle <- new.env(parent = emptyenv())
  handle$merged <- merged
  handle$engines <- engines
  handle$schedule <- schedule
  handle$state <- merged$state
  handle$props <- props
  handle$recorders <- recorders
  handle$live_masks <- live_masks
  handle$process_map <- merged$process_map
  handle$focal_layers <- focal_layers
  handle$exo_rows <- exo_rows
  handle$exo_cursor <- 0L
  # Driver-scheduled events (walk_schedule()), resolved and kept in time order.
  handle$queue <- list()
  handle$queue_time <- numeric(0)
  handle$current_time <- start_time
  # The sub-models the driver evaluates rather than the walk: one row each,
  # labelled the way the completed specification names them.
  handle$deferred <- deferred
  handle$opened <- TRUE
  structure(handle, class = "goldfishWalk")
}

# --------------------------------------------------------------------------- #
# Applying a covariate event (shared by advance and inject).
# --------------------------------------------------------------------------- #

# Apply one object event to the shared state, once, after folding its statistics
# delta into every engine that reads the object. Mirrors the merged walk's
# covariate branch: each reading engine computes its delta over the pre-update
# state (`merged_covariate_step()`), the handle drains it into that engine's
# live statistics (walk_fold_engine()), then the state is written once so a
# later reader and the next evaluation see the update.
walk_apply_object_event <- function(handle, oid, shape, event_args, t) {
  props <- handle$props
  is_undirected <- props$is_undirected[oid]
  for (engine in handle$engines) {
    loid <- engine$shared_to_local[oid]
    if (is.na(loid)) {
      next
    }
    interval <- t - engine$last_time
    engine$last_time <- t
    engine$i_total <- engine$i_total + 1L
    merged_covariate_step(
      engine,
      loid,
      shape,
      event_args,
      is_undirected,
      interval,
      handle$state
    )
    walk_fold_engine(engine)
  }
  # The constraint atoms read the same pre-update state the effects did.
  key <- props$key[oid]
  advance_recorders_for_event(
    handle$recorders,
    key,
    shape,
    event_args,
    handle$state,
    t
  )
  walk_collect_atom_moves(handle$live_masks, handle$recorders, key)
  handle$state <- merged_apply_state_update(
    handle$state,
    oid,
    shape,
    event_args,
    props
  )
  handle$current_time <- t
  invisible(NULL)
}

# --------------------------------------------------------------------------- #
# walk_advance
# --------------------------------------------------------------------------- #

#' @rdname walk_handle
#' @param handle a `goldfishWalk` from [walk_open()].
#' @param t the clock time to advance to.
#' @return `walk_advance()` returns the handle invisibly.
#' @keywords internal
walk_advance <- function(handle, t, call = rlang::caller_env()) {
  walk_assert_open(handle, call)
  if (t < handle$current_time) {
    cli::cli_abort(
      c(
        "{.fn walk_advance} cannot move the clock backward.",
        "x" = "Current time is {.val {handle$current_time}}; asked for
               {.val {t}}."
      ),
      call = call,
      class = "goldfish_walk_out_of_order"
    )
  }
  schedule <- handle$schedule
  repeat {
    exo_time <- if (handle$exo_cursor < length(handle$exo_rows)) {
      schedule$time[handle$exo_rows[handle$exo_cursor + 1L]]
    } else {
      Inf
    }
    queued_time <- if (length(handle$queue_time) > 0L) {
      handle$queue_time[[1L]]
    } else {
      Inf
    }
    next_time <- min(exo_time, queued_time)
    if (!is.finite(next_time) || next_time > t) {
      break
    }
    # An observed exogenous row goes before a driver-scheduled event sharing
    # its stamp: the observed schedule is fixed at open, and the driver
    # schedules against it.
    if (exo_time <= queued_time) {
      k <- handle$exo_rows[handle$exo_cursor + 1L]
      oid <- schedule$target[k]
      walk_advance_presence(handle, exo_time)
      event_args <- merged_build_event_args(
        schedule,
        k,
        oid,
        handle$props,
        handle$state
      )
      walk_apply_object_event(
        handle,
        oid,
        schedule$shape[k],
        event_args,
        exo_time
      )
      handle$exo_cursor <- handle$exo_cursor + 1L
    } else {
      resolved <- handle$queue[[1L]]
      handle$queue <- handle$queue[-1L]
      handle$queue_time <- handle$queue_time[-1L]
      walk_apply_resolved(handle, resolved)
    }
  }
  walk_advance_presence(handle, t)
  handle$current_time <- max(handle$current_time, t)
  invisible(handle)
}

# --------------------------------------------------------------------------- #
# walk_schedule / walk_next_breakpoint
# --------------------------------------------------------------------------- #

#' @rdname walk_handle
#' @return `walk_schedule()` queues `event` (which must carry a `time`) to be
#'   applied by a later `walk_advance()` reaching its time, as if it were a row
#'   of the observed schedule, and returns the handle invisibly. Its value
#'   resolves against the state at the time it is applied, not when queued.
#' @keywords internal
walk_schedule <- function(handle, event, call = rlang::caller_env()) {
  walk_assert_open(handle, call)
  if (is.null(event$time)) {
    cli::cli_abort(
      "A scheduled {.arg event} must carry a {.field time}.",
      call = call,
      class = "goldfish_walk_bad_event"
    )
  }
  resolved <- walk_resolve_event(handle, event, "walk_schedule", call)
  # Insert after every queued event at or before this time, so events sharing
  # a stamp apply in the order they were scheduled.
  position <- findInterval(resolved$time, handle$queue_time)
  handle$queue <- append(handle$queue, list(resolved), after = position)
  handle$queue_time <- append(
    handle$queue_time,
    resolved$time,
    after = position
  )
  invisible(handle)
}

#' @rdname walk_handle
#' @return `walk_next_breakpoint()` returns the time of the next change the
#'   handle applies on its own clock -- an exogenous row, a scheduled event, or
#'   a node-composition change -- or `Inf` when none remains. Between the
#'   current time and that breakpoint every rate is constant unless the driver
#'   injects an event.
#' @keywords internal
walk_next_breakpoint <- function(handle, call = rlang::caller_env()) {
  walk_assert_open(handle, call)
  exo_time <- if (handle$exo_cursor < length(handle$exo_rows)) {
    handle$schedule$time[handle$exo_rows[handle$exo_cursor + 1L]]
  } else {
    Inf
  }
  queued_time <- if (length(handle$queue_time) > 0L) {
    handle$queue_time[[1L]]
  } else {
    Inf
  }
  presence_time <- vapply(
    handle$engines,
    function(engine) {
      min(
        walk_pending_presence_time(engine, "presence1"),
        walk_pending_presence_time(engine, "presence2")
      )
    },
    numeric(1)
  )
  min(exo_time, queued_time, presence_time, Inf)
}

# --------------------------------------------------------------------------- #
# walk_inject
# --------------------------------------------------------------------------- #

#' @rdname walk_handle
#' @param event a list describing one event to apply: `layer` (the object name),
#'   plus `sender` / `receiver` (a dyadic layer) or `node` (a nodal attribute),
#'   an `increment` or `replace` value, and an optional `time` (defaults to the
#'   handle's current clock).
#' @return `walk_inject()` returns the handle invisibly.
#' @keywords internal
walk_inject <- function(handle, event, call = rlang::caller_env()) {
  walk_assert_open(handle, call)
  resolved <- walk_resolve_event(handle, event, "walk_inject", call)
  walk_apply_resolved(handle, resolved)
  invisible(handle)
}

# Validate an event against the handle and stage it as the one-row schedule the
# merged walk's argument builder reads. Returns the target object id, the
# event time and the staged row; its value is resolved only when applied.
walk_resolve_event <- function(handle, event, fn, call = rlang::caller_env()) {
  if (is.null(event$layer)) {
    cli::cli_abort(
      "{.arg event} must name a {.field layer}.",
      call = call,
      class = "goldfish_walk_bad_event"
    )
  }
  oid <- match(event$layer, handle$merged$objects$name)
  if (is.na(oid)) {
    cli::cli_abort(
      c(
        "{.fn {fn}} cannot resolve layer {.val {event$layer}}.",
        "i" = "The walk knows layers {.val {handle$merged$objects$name}}."
      ),
      call = call,
      class = "goldfish_walk_bad_event"
    )
  }
  t <- event$time %||% handle$current_time
  if (t < handle$current_time) {
    cli::cli_abort(
      c(
        "{.fn {fn}} cannot apply an event before the current clock.",
        "x" = "Current time is {.val {handle$current_time}}; event time is
               {.val {t}}."
      ),
      call = call,
      class = "goldfish_walk_out_of_order"
    )
  }

  shape <- if (!is.null(event$node)) {
    "node"
  } else if (!is.null(event$sender)) {
    "dyad"
  } else {
    "global"
  }
  semantics <- if (!is.null(event$increment)) "increment" else "replace"
  value <- if (identical(semantics, "increment")) {
    event$increment
  } else {
    event$replace
  }
  # Reuse the merged walk's event-arg resolution (increment vs replace against
  # the live shared state, missing-value imputation) by staging a one-row
  # schedule the builder reads, so an injected event resolves identically to a
  # scheduled covariate.
  staged <- list(
    shape = shape,
    semantics = semantics,
    value = list(value),
    node = if (identical(shape, "node")) {
      as.integer(event$node)
    } else {
      NA_integer_
    },
    sender = if (identical(shape, "dyad")) {
      as.integer(event$sender)
    } else {
      NA_integer_
    },
    receiver = if (identical(shape, "dyad")) {
      as.integer(event$receiver)
    } else {
      NA_integer_
    }
  )
  list(oid = oid, time = t, staged = staged)
}

# Apply a resolved event at its time: composition first, so the event lands on
# the presence in force at its stamp, then the value against the live state.
walk_apply_resolved <- function(handle, resolved) {
  walk_advance_presence(handle, resolved$time)
  event_args <- merged_build_event_args(
    resolved$staged,
    1L,
    resolved$oid,
    handle$props,
    handle$state
  )
  walk_apply_object_event(
    handle,
    resolved$oid,
    resolved$staged$shape,
    event_args,
    resolved$time
  )
  invisible(NULL)
}

# --------------------------------------------------------------------------- #
# Node composition.
# --------------------------------------------------------------------------- #

# A composition change list as parallel vectors in time order. `order()` is
# stable, so changes sharing a stamp keep their recorded order.
walk_presence_changes <- function(changes) {
  if (length(changes) == 0L) {
    return(list(time = numeric(0), node = integer(0), replace = logical(0)))
  }
  time <- vapply(changes, `[[`, double(1), "time")
  ordering <- order(time)
  list(
    time = time[ordering],
    node = vapply(changes, `[[`, integer(1), "node")[ordering],
    replace = vapply(changes, `[[`, logical(1), "replace")[ordering]
  )
}

# Apply every composition change stamped at or before `t`, on both node sides
# of every engine. A change at an event's own stamp is in force for that event,
# as in the batch presence stream, which applies every change not after the
# event time.
walk_advance_presence <- function(handle, t) {
  for (engine in handle$engines) {
    for (side in c("presence1", "presence2")) {
      changes <- engine[[paste0(side, "_changes")]]
      cursor_name <- paste0(side, "_cursor")
      cursor <- engine[[cursor_name]]
      n_changes <- length(changes$time)
      while (cursor < n_changes && changes$time[[cursor + 1L]] <= t) {
        cursor <- cursor + 1L
        engine[[side]][changes$node[[cursor]]] <- changes$replace[[cursor]]
      }
      engine[[cursor_name]] <- cursor
    }
  }
  invisible(NULL)
}

# The stamp of one side's next unapplied composition change, or `Inf`.
walk_pending_presence_time <- function(engine, side) {
  changes <- engine[[paste0(side, "_changes")]]
  cursor <- engine[[paste0(side, "_cursor")]]
  if (cursor < length(changes$time)) changes$time[[cursor + 1L]] else Inf
}

# --------------------------------------------------------------------------- #
# walk_evaluate
# --------------------------------------------------------------------------- #

# Build the process-state evaluator `state` for one fid from its engine's LIVE
# dense statistics. The union statistics are projected onto the fid's own
# columns (`effect_map`), the timed baseline hazard prepended as an intercept
# column of ones, and the fid's live risk set attached (presence folded with
# its support constraint, `walk_risk_set()`). Focal-driven side / mode
# resolution rode the engine's own compiled spec_map, so `n_actors1/2`
# are this fid's own.
walk_build_state <- function(handle, fid) {
  map <- handle$process_map
  row <- map[map$fid == fid, , drop = FALSE]
  engine <- walk_engine_of_fid(handle, fid)

  i <- match(fid, engine$fids)
  # The union -> fid projection is the same map the batch consumers ride
  # (`unit$effect_maps` by flavor); a plain / single-flavor engine takes the
  # identity projection.
  unit_effect_maps <- handle$merged$units[[engine$key]]$effect_maps
  effect_map <- if (is.null(unit_effect_maps)) {
    seq_len(ncol(engine$live_stats))
  } else {
    unit_effect_maps[[engine$flavors[i]]]
  }

  has_intercept <- isTRUE(row$has_intercept)
  projected <- engine$live_stats[, effect_map, drop = FALSE]
  if (has_intercept) {
    projected <- cbind(1, projected)
  }

  n1 <- engine$ctx$n1
  is_rate <- engine$is_sender
  n2 <- if (is_rate) 1L else engine$ctx$n2
  tmr <- engine$twomode_or_reflexive
  risk_set <- walk_risk_set(handle, engine, fid)

  list(
    model_type = walk_engine_model_type(engine, has_intercept),
    stat_mat = projected,
    active_sender = risk_set$active_sender,
    active_dyad = risk_set$active_dyad,
    active_dyad_encoding = risk_set$encoding,
    n_actors1 = n1,
    n_actors2 = n2,
    n_parameters = ncol(projected),
    twomode_or_reflexive = tmr,
    is_rate = is_rate,
    event_sender = NA_integer_,
    event_receiver = NA_integer_,
    is_dependent = FALSE,
    timespan = NA_real_
  )
}

#' @rdname walk_handle
#' @param fid the integer formula id to evaluate (a row of the specification's
#'   `process_map`).
#' @param theta the parameter vector for the fid, matching its effect columns
#'   (with a leading intercept for a timed rate).
#' @param sender optionally the integer sender whose alternatives to evaluate,
#'   for a fid whose model is defined per sender (DyNAM-choice). Absent, the
#'   result covers the fid's whole candidate space; supplied, it covers that
#'   sender's receivers alone, which is what one generative step needs once the
#'   sender has been drawn. Supplying it for any other fid is an error: those
#'   models put every alternative in one competing set.
#' @return `walk_evaluate()` returns the process-state evaluator result for the
#'   fid: an `index` naming the candidate rows and the per-alternative `value`
#'   (a rate vector for a sender-block fid, a choice matrix's probabilities for
#'   a dyad-block fid, one sender's receiver distribution under `sender`), the
#'   fid's support applied.
#' @keywords internal
walk_evaluate <- function(
  handle,
  fid,
  theta,
  sender = NULL,
  call = rlang::caller_env()
) {
  walk_assert_open(handle, call)
  map <- handle$process_map
  if (!fid %in% map$fid) {
    cli::cli_abort(
      c(
        "{.fn walk_evaluate} does not know fid {.val {fid}}.",
        "i" = "The walk carries fids {.val {map$fid}}."
      ),
      call = call,
      class = "goldfish_walk_bad_fid"
    )
  }
  state <- walk_build_state(handle, fid)
  if (length(theta) != state$n_parameters) {
    cli::cli_abort(
      c(
        "{.arg theta} has the wrong length for fid {.val {fid}}.",
        "x" = "Expected {.val {state$n_parameters}} parameter{?s}, got
               {.val {length(theta)}}."
      ),
      call = call,
      class = "goldfish_walk_bad_theta"
    )
  }
  # Every evaluator returns the value over the FULL candidate space except
  # DyNAM-choice, which is defined per sender (`.pse_eval_choice` slices the
  # observed sender's receiver block). A generative walk needs the whole choice
  # matrix, so that model is evaluated once per sender and the rows stacked -- a
  # faithful application of the same evaluator, not a reimplementation.
  #
  # Stacking is what the batch-vs-replay oracle compares, but it is not what a
  # step costs: DyNAM factorizes the intensity into a sender hazard and a
  # receiver softmax conditional on that sender, so a step that has already
  # drawn its sender needs one row and pays for n1 of them. `sender` asks for
  # the row, from the same evaluator, so the two paths cannot drift.
  if (identical(state$model_type, "DyNAM-M")) {
    if (is.null(sender)) {
      return(walk_evaluate_choice_matrix(state, theta))
    }
    state$event_sender <- walk_assert_sender(state, fid, sender, call)
    return(evaluate_process_state(state, theta))
  }
  if (!is.null(sender)) {
    cli::cli_abort(
      c(
        "{.arg sender} does not apply to fid {.val {fid}}.",
        "i" = "Only a per-sender model ({.val DyNAM-M}) has a sender's row to
               return; {.val {state$model_type}} puts every alternative in one
               competing set.",
        "i" = "Drop {.arg sender} to evaluate the whole candidate space."
      ),
      call = call,
      class = "goldfish_walk_bad_sender"
    )
  }
  evaluate_process_state(state, theta)
}

# A `sender` argument for a per-sender fid, as a 1-based actor index into the
# fid's sender set. Returns it coerced to integer.
walk_assert_sender <- function(state, fid, sender, call) {
  ok <- length(sender) == 1L &&
    !is.na(sender) &&
    sender >= 1L &&
    sender <= state$n_actors1
  if (!ok) {
    cli::cli_abort(
      c(
        "{.arg sender} is not an actor of fid {.val {fid}}.",
        "x" = "Expected one index in {.val {1L}}:{.val {state$n_actors1}}, got
               {.val {sender}}."
      ),
      call = call,
      class = "goldfish_walk_bad_sender"
    )
  }
  as.integer(sender)
}

# The full n1 x n2 choice-probability matrix at the live state, one row per
# sender: `.pse_eval_choice()` evaluated for each sender in turn (its value is
# that sender's receiver distribution, self-loop excluded), stacked. The value
# is returned as a length n1*n2 sender-major vector alongside the dyad index, so
# a dyad fid's result reads like the other dyad evaluators' (REM, coordination).
walk_evaluate_choice_matrix <- function(state, theta) {
  n1 <- state$n_actors1
  n2 <- state$n_actors2
  value <- numeric(n1 * n2)
  for (s in seq_len(n1)) {
    state$event_sender <- s
    ev <- evaluate_process_state(state, theta)
    value[(s - 1L) * n2 + seq_len(n2)] <- ev$value
  }
  list(
    model_type = state$model_type,
    index = .pse_dyad_index(n1, n2),
    value = value
  )
}

# --------------------------------------------------------------------------- #
# Live risk sets.
# --------------------------------------------------------------------------- #

# Abort when a constraint's atoms read a dynamic object the shared schedule
# never visits (a constraint-only network no formula term reads). The batch
# walk replays such a constraint over a private walk after the fact; a stepping
# handle has no after, and a mask frozen at its seed would silently simulate a
# different model.
walk_assert_covered_constraints <- function(
  engines,
  shared_object_keys,
  call = rlang::caller_env()
) {
  uncovered <- character(0)
  for (engine in engines) {
    for (sub_plan in engine_constraints(engine)) {
      if (!recorder_covers_constraint(sub_plan, shared_object_keys)) {
        uncovered <- c(uncovered, sub_plan$atom_labels)
      }
    }
  }
  if (length(uncovered) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "The walk handle cannot maintain this support constraint.",
      "x" = "Constraint atom{?s} {.val {unique(uncovered)}} read{?s/} a
             changing object no formula term reads, so the shared walk never
             steps its events."
    ),
    call = call,
    class = "goldfish_walk_unsupported"
  )
}

# The compiled constraint one fid's risk set carries: a multi-output engine
# reads it off its consumer specs, a single-output engine off its own plan.
walk_fid_constraint <- function(engine, fid) {
  if (is.null(engine$consumer_specs)) {
    return(engine$ctx$plan$support_constraint)
  }
  engine$consumer_specs[[as.character(fid)]]$constraint
}

# The atom handle `build_mask_maintainer()` reads, over a store's LIVE atom
# buffers rather than a replay of its recorded deltas.
walk_live_atoms <- function(store) {
  list(
    n1 = store$n1,
    n2 = store$n2,
    atom_labels = store$atom_labels,
    atom_kinds = store$atom_kinds,
    atom_value = function(label) {
      get(
        as.character(match(label, store$atom_labels)),
        envir = store$atom_state
      )
    }
  )
}

# One mask maintainer per distinct (store, expression, kind, symmetry), and a
# per-fid pointer to it. A layer's rate and choice read one store and, when
# their expressions agree, one maintainer, as the pooled batch realization
# does. The maintainers are the batch ones, fed the same touched-entry sets, so
# a live mask cannot drift from the stored stream.
#
# `live` carries: `touched` (per store, the atom entries moved since the last
# read), `maintainers` / `maintainer_keys` (per store), and `by_fid` (store,
# maintainer, mask kind and stored kind per constrained fid).
walk_build_live_masks <- function(engines, recorders) {
  live <- new.env(parent = emptyenv())
  live$touched <- lapply(
    recorders$stores,
    function(store) vector("list", store$n_atoms)
  )
  live$maintainers <- lapply(recorders$stores, function(store) list())
  live$maintainer_keys <- lapply(recorders$stores, function(store) character())
  live$by_fid <- list()
  for (engine in engines) {
    ctx <- engine$ctx
    # Only coordination symmetrises its stored mask; the batch requests key
    # the same choice on the sub-model.
    symmetric <- identical(engine$sub_model, "choice_coordination")
    for (fid in engine$fids) {
      sub_plan <- walk_fid_constraint(engine, fid)
      if (is.null(sub_plan)) {
        next
      }
      signature <- paste(sort(sub_plan$atom_labels), collapse = "\r")
      store <- recorders$lookup[[
        paste(engine$model, ctx$nodes, ctx$nodes2, signature, sep = "\v")
      ]]
      s <- which(vapply(recorders$stores, identical, logical(1), store))
      mask_kind <- as.integer(sub_plan$mask_kind)
      stored_kind <- if (symmetric) 0L else mask_kind
      maintainer_key <- paste(
        paste(deparse(sub_plan$expr), collapse = ""),
        paste(sub_plan$atom_labels, collapse = "\r"),
        mask_kind,
        symmetric,
        sep = "\v"
      )
      m <- match(maintainer_key, live$maintainer_keys[[s]])
      if (is.na(m)) {
        maintainer <- build_mask_maintainer(
          walk_live_atoms(store),
          sub_plan$expr,
          sub_plan$atom_labels,
          symmetric,
          mask_kind,
          stored_kind
        )
        live$maintainers[[s]] <- c(live$maintainers[[s]], list(maintainer))
        live$maintainer_keys[[s]] <- c(
          live$maintainer_keys[[s]],
          maintainer_key
        )
        m <- length(live$maintainer_keys[[s]])
      }
      live$by_fid[[as.character(fid)]] <- list(
        store = s,
        maintainer = m,
        mask_kind = mask_kind,
        stored_kind = stored_kind
      )
    }
  }
  live
}

# Fold the atom deltas one object event recorded into each store's pending
# touched set, then drop the record. The batch walk keeps the record to replay
# at finalization; a stepping handle consumes it as it goes, so a long
# simulation does not hold every delta it ever produced.
walk_collect_atom_moves <- function(live, recorders, key) {
  for (s in unique(recorders$by_key[[key]])) {
    store <- recorders$stores[[s]]
    touched <- live$touched[[s]]
    for (r in seq_along(store$record_gid)) {
      entries <- store$record_entries[[r]]
      if (length(entries) == 0L) {
        next
      }
      gid <- store$record_gid[[r]]
      touched[[gid]] <- unique(c(touched[[gid]], entries))
    }
    live$touched[[s]] <- touched
    store$record_time <- numeric(0)
    store$record_gid <- integer(0)
    store$record_entries <- list()
    store$record_values <- list()
  }
  invisible(NULL)
}

# A constrained fid's current mask at its stored kind, or NULL when the fid
# carries no constraint. Every maintainer on the fid's store recomputes from
# the pending moves together, because reading them consumes them.
walk_live_support <- function(handle, fid) {
  live <- handle$live_masks
  entry <- live$by_fid[[as.character(fid)]]
  if (is.null(entry)) {
    return(NULL)
  }
  s <- entry$store
  moved <- live$touched[[s]]
  if (any(lengths(moved) > 0L)) {
    for (maintainer in live$maintainers[[s]]) {
      maintainer$recompute(moved)
    }
    live$touched[[s]] <- vector("list", length(moved))
  }
  list(
    value = live$maintainers[[s]][[entry$maintainer]]$value(),
    mask_kind = entry$mask_kind,
    stored_kind = entry$stored_kind
  )
}

# The fid's live risk set in the shapes the process-state evaluators read,
# folded as the batch finalizers fold it: a rate's sender gate is presence and
# at least one allowed present receiver; REM and coordination read the whole
# dyad grid with both presences in it, coordination symmetrised; a choice
# reads the receiver axis, as a vector when the mask is column-broadcast or
# gates only senders, as a grid when it is genuinely dyadic.
walk_risk_set <- function(handle, engine, fid) {
  n1 <- engine$n1
  n2 <- engine$n2
  p1 <- engine$presence1
  p2 <- engine$presence2
  support <- walk_live_support(handle, fid)

  if (engine$is_sender) {
    gate <- if (is.null(support)) {
      TRUE
    } else {
      sender_gate_from_mask(support$value, support$stored_kind, p2, n1, n2)
    }
    return(list(
      active_sender = p1 & gate,
      active_dyad = rep(TRUE, n1),
      encoding = "alter"
    ))
  }
  if (is.null(support)) {
    return(list(active_sender = p1, active_dyad = p2, encoding = "alter"))
  }

  spec <- engine$spec_map
  support_grid <- support_to_grid(
    support$value,
    support$stored_kind,
    n1,
    n2
  )
  if (risk_set_is_dyadic(spec)) {
    dyads <- outer(p1, p2, "&") & support_grid
    if (risk_set_symmetrize(spec)) {
      dyads <- symmetrize_mask(dyads)
    }
    return(list(active_sender = p1, active_dyad = dyads, encoding = "point"))
  }
  encoding <- active_dyad_encoding_decide(
    risk_set_encoding(spec),
    support$mask_kind
  )
  switch(
    encoding,
    alter = list(
      active_sender = p1,
      active_dyad = p2 & support_grid[1L, ],
      encoding = "alter"
    ),
    # An ego-kind mask gates senders only, and the choice risk set is read
    # from the drawing sender's row, so the receiver axis is presence alone.
    outer = list(active_sender = p1, active_dyad = p2, encoding = "alter"),
    point = list(
      active_sender = p1,
      active_dyad = matrix(p2, n1, n2, byrow = TRUE) & support_grid,
      encoding = "point"
    )
  )
}

# --------------------------------------------------------------------------- #
# Helpers.
# --------------------------------------------------------------------------- #

# The engine (compiled process unit) that owns a fid.
walk_engine_of_fid <- function(handle, fid) {
  for (engine in handle$engines) {
    if (fid %in% engine$fids) {
      return(engine)
    }
  }
  NULL
}

walk_assert_open <- function(handle, call = rlang::caller_env()) {
  if (!inherits(handle, "goldfishWalk") || !isTRUE(handle$opened)) {
    cli::cli_abort(
      "{.arg handle} must be an open {.cls goldfishWalk} from
       {.fn walk_open}.",
      call = call,
      class = "goldfish_walk_not_open"
    )
  }
  invisible(NULL)
}
