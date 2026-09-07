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
#     that reads them and advancing the shared clock.
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
#   * PER-FID FOCAL (D8a). Each engine was compiled against its own focal, so a
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
# Generative-completeness assertion (D9).
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

# Abort when a sub-model carries no effects (a completion-supplied uniform
# choice or pinned intercept-only rate). The merged compile parses
# effect-bearing formulas; an effect-free `~ 1` block is walked by the consumers
# (simulate() / estimate_dynes()), not this substrate. Detected on the term
# labels so it is independent of the bundle's internal shape.
assert_walkable_submodels <- function(joint_spec, call = rlang::caller_env()) {
  empty <- character(0)
  for (spec in joint_spec$specifications) {
    for (proc in spec_processes(spec)) {
      for (family in names(proc$submodels)) {
        formula <- proc$submodels[[family]]$formula
        labels <- attr(stats::terms(formula), "term.labels")
        if (length(labels) == 0L) {
          flavor <- if (is.na(proc$flavor)) {
            NULL
          } else {
            paste0(" (", proc$flavor, ")")
          }
          empty <- c(empty, paste0(spec$focal, flavor, " › ", family))
        }
      }
    }
  }
  if (length(empty) == 0L) {
    return(invisible(NULL))
  }
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
  engine
}

# Drain the recorder's pending point / broadcast blocks (accumulated in
# union-gid space by `merged_covariate_step()`) into the engine's live
# statistics,
# then reset the buffers. Reuses the estimation-path fold helpers
# (`.gather_apply_stat` / `.gather_apply_broadcast`), so the live statistics
# evolve byte-identically to the batch consumers' stored update streams.
walk_fold_engine <- function(engine) {
  rec <- engine$recorder
  if (rec$pending_dep_cols > 0L) {
    block <- do.call(cbind, rec$pending_dep)
    engine$live_stats <- .gather_apply_stat(
      engine$live_stats,
      block,
      0L,
      ncol(block),
      engine$fold_n2
    )
  }
  if (rec$pending_dep_bc_cols > 0L) {
    bc <- do.call(cbind, rec$pending_dep_bc)
    engine$live_stats <- .gather_apply_broadcast(
      engine$live_stats,
      bc,
      0L,
      ncol(bc),
      engine$n1,
      engine$fold_n2,
      engine$twomode_or_reflexive
    )
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
#' @param call the calling environment, for error reporting.
#'
#' @return `walk_open()` returns a `goldfishWalk` object.
#' @keywords internal
#' @name walk_handle
walk_open <- function(
  spec,
  control_preprocessing = set_preprocessing_opt(),
  call = rlang::caller_env()
) {
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
  # `~ 1` block on the live walk is deferred to them. The handle here walks
  # effect-bearing sub-models; a completed default is a clear boundary, not a
  # cryptic parse error.
  assert_walkable_submodels(joint_spec, call)

  merged <- build_merged_blocks(joint_spec, control_preprocessing)

  # The handle's live-state stepping supports the class the merged walk already
  # restricts (no window effects, no explicit window bounds) plus, for now, no
  # user support constraints and no node-composition dynamics: under those the
  # live risk set is the trivial all-active set the evaluators apply, so the
  # live statistics equal the batch materialization exactly. The augmenter /
  # simulate consumers extend this later.
  if (!is.null(merged$support_constraints)) {
    cli::cli_abort(
      "The walk handle does not yet support user support constraints.",
      call = call,
      class = "goldfish_walk_unsupported"
    )
  }

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
      if (
        length(engine$ctx$active_sender_changes) > 0L ||
          length(engine$ctx$active_dyad_changes) > 0L
      ) {
        cli::cli_abort(
          "The walk handle does not yet support node-composition changes.",
          call = call,
          class = "goldfish_walk_unsupported"
        )
      }
      walk_prepare_engine(engine)
    }
  )

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
  handle$process_map <- merged$process_map
  handle$focal_layers <- focal_layers
  handle$exo_rows <- exo_rows
  handle$exo_cursor <- 0L
  handle$current_time <- start_time
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
  merged_apply_state_update(handle$state, oid, shape, event_args, props)
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
  while (handle$exo_cursor < length(handle$exo_rows)) {
    k <- handle$exo_rows[handle$exo_cursor + 1L]
    if (schedule$time[k] > t) {
      break
    }
    oid <- schedule$target[k]
    shape <- schedule$shape[k]
    event_args <- merged_build_event_args(
      schedule,
      k,
      oid,
      handle$props,
      handle$state
    )
    walk_apply_object_event(handle, oid, shape, event_args, schedule$time[k])
    handle$exo_cursor <- handle$exo_cursor + 1L
  }
  handle$current_time <- max(handle$current_time, t)
  invisible(handle)
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
        "{.fn walk_inject} cannot resolve layer {.val {event$layer}}.",
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
        "{.fn walk_inject} cannot apply an event before the current clock.",
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
  event_args <- merged_build_event_args(
    staged,
    1L,
    oid,
    handle$props,
    handle$state
  )
  walk_apply_object_event(handle, oid, shape, event_args, t)
  invisible(handle)
}

# --------------------------------------------------------------------------- #
# walk_evaluate
# --------------------------------------------------------------------------- #

# Build the process-state evaluator `state` for one fid from its engine's LIVE
# dense statistics. The union statistics are projected onto the fid's own
# columns (`effect_map`), the timed baseline hazard prepended as an intercept
# column of ones, and the trivial all-active risk set attached (the supported
# class carries no constraint or composition dynamics). Focal-driven side / mode
# resolution rode the engine's own compiled spec_map (D8a), so `n_actors1/2`
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

  list(
    model_type = walk_engine_model_type(engine, has_intercept),
    stat_mat = projected,
    active_sender = rep(TRUE, n1),
    active_dyad = rep(TRUE, if (is_rate) n1 else n2),
    active_dyad_encoding = "alter",
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
#' @return `walk_evaluate()` returns the process-state evaluator result for the
#'   fid: an `index` data frame and the per-alternative `value` (a rate vector
#'   for a sender-block fid, a choice matrix's probabilities for a dyad-block
#'   fid), the fid's support applied.
#' @keywords internal
walk_evaluate <- function(handle, fid, theta, call = rlang::caller_env()) {
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
  if (identical(state$model_type, "DyNAM-M")) {
    return(walk_evaluate_choice_matrix(state, theta))
  }
  evaluate_process_state(state, theta)
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
