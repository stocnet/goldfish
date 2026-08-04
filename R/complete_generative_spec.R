# =========================================================================== #
# Generative-readiness completion (D9 / D9a).
#
# A specification that GENERATES events (drives the walk handle via simulate()
# or an augmenter) or is estimated JOINTLY (estimate_dynes()) must be
# *generatively complete*: every modeled DyNAM flavor carries BOTH a rate and a
# choice (REM needs only a rate, already enforced). A half-specified flavor -- a
# flavor keyed in one sub-model list and omitted from the other (recorded by
# make_specification() without fabricating a default) -- or a whole choice-only /
# rate-only DyNAM process is filled here with a ZERO-FREE-PARAMETER default:
#
#   * missing choice               -> uniform choice over the support-legal
#                                     alternatives (self-loops disallowed);
#   * missing choice_coordination  -> uniform on both coordination sides;
#   * missing rate, ordered regime -> uniform rate_ordered;
#   * missing rate, timed regime   -> the pinned intercept-only rate
#                                     (intercept-only-rate-spec primitive), so
#                                     the flavor's events land on the shared
#                                     clock.
#
# `complete_generative_spec()` is the ONE transform every consumer runs once at
# its own entry (never inside walk_open, which merely asserts completeness): it
# produces a single completed spec that walk-driven and non-walk-driven paths
# share, and warns at EACH consumer entry (never suppressed on re-entry) so the
# auto-supplied default is never silent. Completion NEVER touches the
# single-process / flavored estimation path (that keeps rate-only / choice-only
# specifications byte-identical under the frozen baselines); it is scoped by type
# to a `joint_specification.goldfish`.
#
# Per D9a the transform stays data-source-agnostic: it installs the pinned-rate
# STRUCTURE, and the (count_w, T_w, |R_w|) a timed pin needs come from ONE shared
# consumer helper (`panel_wave_risk_set()` / `relational_window_risk_set()`),
# so the consumers cannot diverge on the derivation.
# =========================================================================== #

# The expected sub-model families of a process. A DyNAM process needs both a
# rate and a choice; REM needs only a rate (choice is rejected for REM upstream).
expected_families <- function(model) {
  if (identical(model, "REM")) "rate" else c("rate", "choice")
}

# Complete a joint specification for a generative / joint consumer. Returns the
# completed spec with default sub-models installed, the `process_map`'s
# `completed` column set, timed rates pinned (`$completed_rates`), and one
# warning per filled gap. Idempotent: a spec with no gap is returned unchanged
# (aside from a `completed` column of all-FALSE, so downstream code can read it).
#
# `wave_times` is the K+1 period boundaries a timed pin buckets on (the panel
# waves, or a single window). When NULL a single window spanning the data's event
# times is used, so the pin has one plateau.
complete_generative_spec <- function(
  joint_spec,
  consumer = c("estimate_dynes", "simulate"),
  wave_times = NULL,
  call = rlang::caller_env()
) {
  consumer <- match.arg(consumer)
  if (!inherits(joint_spec, "joint_specification.goldfish")) {
    cli::cli_abort(
      c(
        "{.arg joint_spec} must be a {.cls joint_specification.goldfish}.",
        "i" = "Generative completion is scoped to the joint (generative) surface;
               the single-process path keeps rate-only / choice-only
               specifications unchanged."
      ),
      call = call
    )
  }

  modeled_panel <- joint_spec$modeled_panel %||% character(0)

  # Case A (panel-gated): a MODELED PANEL layer whose data carries a flavor the
  # specification keys in NEITHER rate nor choice cannot be completed -- the
  # augmenter must place events of every flavor its wave-diff produces. On an RE
  # focal layer modeling a subset of data flavors stays legal (the unmodeled
  # flavors update state), so this is checked only for modeled panel layers.
  abort_on_unmodeled_panel_flavor(joint_spec, modeled_panel, call = call)

  timed <- is_timed_joint_specification(joint_spec)

  # 1. Fill each process's gaps on a working copy of the specifications, tracking
  #    which (layer, flavor, family) were added so the completed column and the
  #    per-fid warnings can name them.
  specs <- joint_spec$specifications
  added <- list()
  for (si in seq_along(specs)) {
    spec <- specs[[si]]
    fam_needed <- expected_families(spec$model)
    for (proc in spec_processes(spec)) {
      present <- names(proc$submodels)
      for (family in setdiff(fam_needed, present)) {
        default <- resolve_default_sub_model(
          spec,
          flavor = proc$flavor,
          family = family,
          timed = timed
        )
        if (is.null(default)) {
          # Ordered-regime choice-only rate: no clock to place it on, deferred to
          # process-simulation's pseudo-time modes -- left as a gap, not filled.
          next
        }
        specs[[si]] <- install_default_bundle(
          specs[[si]],
          flavor = proc$flavor,
          family = family,
          sub_model = default$sub_model,
          bundle = default$bundle
        )
        added[[length(added) + 1L]] <- data.frame(
          layer = spec$focal,
          flavor = proc$flavor,
          family = family,
          sub_model = default$sub_model,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # 2. Rebuild the joint spec from the completed specifications: the process_map,
  #    coupling, and modeled_panel all fall out of the shared constructor path,
  #    so a completed fid is keyed and coupled exactly like an authored one.
  completed <- rebuild_completed_joint(joint_spec, specs)
  # The `completed` mark lives on the synthesized bundle, so it survives a
  # rebuild and a re-entry: a fid is completed iff its bundle was auto-supplied.
  completed$process_map$completed <- completed_column_from_bundles(completed)

  # 3. Timed rates: mark them pinned (asserting the timed regime -- the first
  #    real caller of the primitive's timed-only guard) and pin every COMPLETED
  #    intercept-only rate from the consumer-supplied (count_w, T_w, |R_w|).
  #    Pinning reads the bundle marks (not this call's `added`), so a re-entry on
  #    an already-completed spec re-pins the same fids deterministically.
  if (timed) {
    completed <- mark_pinned_rates(completed, call = call)
    completed <- pin_completed_rates(
      completed,
      wave_times = wave_times,
      call = call
    )
  }

  # 4. Warn once per filled gap, at this consumer's entry (never suppressed).
  warn_completed_defaults(added, consumer = consumer, call = call)

  completed
}

# The zero-free-parameter default sub-model for a missing family, as a
# `list(sub_model, bundle)`; NULL when the missing family is deliberately NOT
# completed (an ordered-regime missing rate, deferred to process-simulation).
resolve_default_sub_model <- function(spec, flavor, family, timed) {
  data <- spec$data
  model <- spec$model
  layer <- spec$focal
  if (family == "choice") {
    # A half-specified flavor records the intended choice sub-model, so a missing
    # `choice_coordination` completes to a uniform coordination draw on both
    # sides rather than a plain uniform choice; a rate-only process with no such
    # record defaults to a plain uniform choice.
    sub_model <- gap_sub_model(spec, flavor, "choice") %||% "choice"
    return(list(
      sub_model = sub_model,
      bundle = build_default_bundle(data, model, layer, "choice", sub_model)
    ))
  }
  # A missing rate: pinned intercept-only in the timed regime (a shared clock to
  # place events on), deferred in the ordered regime (no clock -- the choice-only
  # flavor's timing is process-simulation's pseudo-time / fixed-template modes).
  if (!timed) {
    return(NULL)
  }
  list(
    sub_model = "rate",
    bundle = build_default_bundle(data, model, layer, "rate", "rate")
  )
}

# The intended sub-model recorded for a half-specified flavor's gap (e.g.
# `choice_coordination` for a coordination process whose choice was omitted),
# read from the specification's `completion_gaps`; NULL when the family is not a
# recorded gap (a whole rate-only / choice-only process has no such record).
gap_sub_model <- function(spec, flavor, family) {
  gaps <- spec$completion_gaps
  if (is.null(gaps) || nrow(gaps) == 0L) {
    return(NULL)
  }
  same_flavor <- if (is.na(flavor)) {
    is.na(gaps$flavor)
  } else {
    !is.na(gaps$flavor) & gaps$flavor == flavor
  }
  hit <- gaps$sub_model[same_flavor & gaps$family == family]
  if (length(hit) == 0L) NULL else hit[[1L]]
}

# Build a `~ 1` default bundle for one family/sub-model of a layer. The bundle is
# SYNTHESIZED rather than parsed: an effect-free `~ 1` has no right-hand side for
# `parse_formula()` to build ("a model without effects cannot be estimated"), and
# a zero-parameter default has nothing to parse anyway. It is the identical shape
# a user-written `~ 1` would reduce to -- an empty `rhs_names`, no window
# derivations, `has_intercept` TRUE only for a (timed) rate -- so the pinned-rate
# classifier and every fid consumer read it exactly like an authored bundle. The
# empty right-hand side makes it uniform (choice / choice_coordination /
# rate_ordered) or intercept-only (a timed rate, which mark_pinned_rates() pins).
build_default_bundle <- function(data, model, layer, family, sub_model) {
  # Only a genuine (timed) waiting-time rate carries the intercept; choice /
  # choice_coordination / rate_ordered force it FALSE, matching the estimation
  # bundle builder's effective-sub_model rule.
  has_intercept <- identical(sub_model, "rate")
  one_sided <- stats::as.formula("~1", env = baseenv())
  full <- stats::as.formula(call("~", as.name(layer), 1), env = baseenv())
  list(
    input_formula = one_sided,
    formula = full,
    sub_model = sub_model,
    parsed = list(
      rhs_names = list(),
      window_derivations = list(),
      has_intercept = has_intercept
    ),
    has_intercept = has_intercept,
    # Durable auto-supplied mark: it rides the bundle through a rebuild and a
    # re-entry, so `completed` never has to be recomputed from a transient diff.
    completed = TRUE
  )
}

# The `completed` logical column, one per process_map row in fid order, read off
# each fid's bundle mark (`bundle$completed`). Persisting the mark on the bundle
# makes completion idempotent: rebuilding or re-entering recovers the column
# without a diff against the pre-completion map.
completed_column_from_bundles <- function(joint_spec) {
  fid_bundles <- joint_fid_bundles(joint_spec)
  map <- joint_spec$process_map
  vapply(
    map$fid,
    function(fid) isTRUE(fid_bundles[[as.character(fid)]]$bundle$completed),
    logical(1)
  )
}

# Install a default bundle into a specification's process, mutating the working
# copy: a multi-flavor spec keys by flavor under `$processes`, a plain /
# single-flavor spec writes `$submodels` directly. The process's existing
# support constraint is untouched, so the completed sub-model inherits the
# layer's constraints and never borrows a sibling flavor's.
install_default_bundle <- function(spec, flavor, family, sub_model, bundle) {
  if (!is.null(spec$processes)) {
    key <- if (is.na(flavor)) names(spec$processes)[[1L]] else flavor
    spec$processes[[key]]$submodels[[family]] <- bundle
    spec$processes[[key]]$submodels <- order_submodels(
      spec$processes[[key]]$submodels
    )
  } else {
    spec$submodels[[family]] <- bundle
    spec$submodels <- order_submodels(spec$submodels)
  }
  # A filled gap is no longer a gap; drop it from the record the estimators read.
  if (!is.null(spec$completion_gaps)) {
    g <- spec$completion_gaps
    keep <- !(g$flavor == (if (is.na(flavor)) g$flavor else flavor) &
      g$family == family)
    spec$completion_gaps <- g[keep, , drop = FALSE]
  }
  spec
}

# Keep a process's sub-models in the canonical rate-before-choice order, so a
# completed fid layout matches an authored spec's (make_specification builds rate
# first). Completion may append the missing family in either order, so normalize.
order_submodels <- function(submodels) {
  ordered <- c("rate", "choice")
  submodels[intersect(ordered, names(submodels))]
}

# Rebuild the joint specification from the completed specifications, so the
# process_map, coupling, and modeled_panel are re-derived by the same path that
# built the original -- a completed fid is indistinguishable in kind from an
# authored one. The regime and cross-process conformance were validated at the
# original construction and are unchanged by adding zero-effect defaults.
rebuild_completed_joint <- function(joint_spec, specs) {
  modeled_panel <- joint_spec$modeled_panel %||% character(0)
  process_map <- build_joint_process_map(specs, modeled_panel)
  structure(
    list(
      specifications = specs,
      process_map = process_map,
      data = joint_spec$data,
      modeled_panel = modeled_panel,
      call = joint_spec$call
    ),
    class = "joint_specification.goldfish"
  )
}

# Pin every COMPLETED timed rate from the consumer-supplied (count_w, T_w,
# |R_w|), storing the frozen intercept-only rate object per fid in
# `$completed_rates`. The numbers come from the shared panel helper (the common
# DyNES / snapshot path); the transform itself reads no event stream. Driven by
# the process_map's `completed` marks (persisted on the bundles), so a re-entry
# on an already-completed spec pins the same fids deterministically.
pin_completed_rates <- function(joint_spec, wave_times, call) {
  map <- joint_spec$process_map
  rate_fids <- map$fid[map$completed & map$family == "rate"]
  if (length(rate_fids) == 0L) {
    joint_spec$completed_rates <- list()
    return(joint_spec)
  }
  pinned <- list()
  for (fid in rate_fids) {
    row <- map[map$fid == fid, , drop = FALSE]
    entity <- rate_entity(joint_spec, row$layer)
    rs <- panel_wave_risk_set(
      joint_spec,
      layer = row$layer,
      flavor = row$flavor,
      entity = entity,
      wave_times = wave_times,
      call = call
    )
    intercept <- pin_intercept_only_rate(
      count = rs$count,
      duration = rs$duration,
      risk_set_size = rs$risk_set_size,
      call = call
    )
    model_type <- pinned_rate_model_type(
      spec_of_layer(joint_spec, row$layer)$model,
      call = call
    )
    pinned[[as.character(fid)]] <- make_intercept_only_rate(
      intercept,
      model_type = model_type,
      wave_times = rs$wave_times,
      call = call
    )
  }
  joint_spec$completed_rates <- pinned
  joint_spec
}

# The flavor's rate entity: active dyads for a tie-oriented (REM) flavor,
# active senders for an actor-oriented (DyNAM) flavor -- the choice of reuse
# helper the |R_w| derivation keys on (D9a).
rate_entity <- function(joint_spec, layer) {
  if (identical(spec_of_layer(joint_spec, layer)$model, "REM")) {
    "dyad"
  } else {
    "sender"
  }
}

spec_of_layer <- function(joint_spec, layer) {
  for (spec in joint_spec$specifications) {
    if (identical(spec$focal, layer)) {
      return(spec)
    }
  }
  NULL
}

# Abort when a MODELED PANEL layer's data carries a flavor the specification
# models in neither rate nor choice (Case A). RE focal layers are exempt (a
# subset of data flavors may be modeled), so only modeled panel layers are
# checked.
abort_on_unmodeled_panel_flavor <- function(joint_spec, modeled_panel, call) {
  if (length(modeled_panel) == 0L) {
    return(invisible(NULL))
  }
  ties <- as.data.frame(joint_spec$data$ties)
  for (spec in joint_spec$specifications) {
    if (!spec$focal %in% modeled_panel) {
      next
    }
    data_flavors <- unique(ties$flavor[ties$layer == spec$focal])
    data_flavors <- data_flavors[!is.na(data_flavors)]
    modeled <- spec$modeled_flavors %||% character(0)
    unmodeled <- setdiff(data_flavors, modeled)
    if (length(unmodeled) > 0L) {
      cli::cli_abort(
        c(
          "A modeled panel layer must model all of its flavors.",
          "x" = "Layer {.val {spec$focal}} carries flavor{?s}
                 {.val {unmodeled}} modeled in neither {.arg rate} nor
                 {.arg choice}.",
          "i" = "A modeled panel layer's augmented path must place events of
                 every flavor its wave-diff produces; model {.val {unmodeled}}
                 or drop it from the panel layer."
        ),
        call = call
      )
    }
  }
  invisible(NULL)
}

# =========================================================================== #
# D9a shared consumer helpers: (count_w, T_w, |R_w|) for a timed pin.
#
# `|R_w|`'s source is keyed on HOW the flavor is observed, not on which consumer
# called: a PANEL flavor uses the wave-endpoint average of its post-constraint
# entity count (the common DyNES / snapshot path, served here); a fully observed
# RELATIONAL flavor uses goldfish's own time-weighted `avg_active_entity`. Both
# reuse existing estimation machinery (`network_state_at()` materializer +
# `assemble_model_mask()` + `active_dyad_count()`; the preprocessed intercept
# scalars) so a consumer never re-derives the risk set by hand.
# =========================================================================== #

# The support kind a flavor's derived mutually-exclusive constraint imposes on
# the state, for the wave-endpoint risk-set count: creation is supportable only
# where no tie exists (`state == 0`), dissolution only where one does
# (`state == 1`); an unconstrained / general-constraint flavor counts every
# (non-self-loop) dyad ("none"). A general user support_constraint is not
# projected here -- the panel/DyNES path this serves uses the derived
# mutually-exclusive masks or none.
flavor_support_kind <- function(spec, flavor) {
  derived <- if (!is.null(spec$processes)) {
    key <- if (is.na(flavor)) names(spec$processes)[[1L]] else flavor
    spec$processes[[key]]$derived_constraint
  } else {
    spec$derived_constraint
  }
  if (is.null(derived)) {
    return("none")
  }
  rhs <- derived[[length(derived)]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("!"))) {
    "creation"
  } else {
    "dissolution"
  }
}

# The single-window fallback boundaries: [min, max] of the pinned layer's own
# timed events, one plateau. A consumer with a wave grid supplies its own
# boundaries instead. Coerced to numeric (`coerce_time()`, `R/state_at.R`) so
# a POSIXct/Date time axis yields a plain numeric range, not a `difftime`.
default_window <- function(data, layer) {
  ties <- as.data.frame(data$ties)
  times <- ties$time[ties$layer == layer]
  times <- times[!is.na(times)]
  if (length(times) == 0L) {
    return(c(0, 1))
  }
  rng <- coerce_time(range(times))
  if (rng[1] == rng[2]) {
    rng[2] <- rng[1] + 1
  }
  rng
}

# Per-period (count_w, T_w, |R_w|) for a PANEL-observed flavor over a wave grid.
# For each of the K+1 boundaries the flavor's layer state is materialized (the
# estimation-path materializer, strictly-before semantics) and its
# post-constraint rate entity counted; `|R_w|` is the endpoint average of the
# two boundaries of period w, `count_w` the net Hamming diff between them (a
# net-change floor), and `T_w` the boundary gap. `wave_times` is returned for the
# pin's half-open period membership (NULL for a single plateau).
panel_wave_risk_set <- function(
  joint_spec,
  layer,
  flavor,
  entity = c("sender", "dyad"),
  wave_times = NULL,
  call = rlang::caller_env()
) {
  entity <- match.arg(entity)
  data <- joint_spec$data
  spec <- spec_of_layer(joint_spec, layer)
  support_kind <- flavor_support_kind(spec, flavor)
  directed <- isTRUE(unname(data$info$directed[layer]))
  info <- data$info %||% list()
  nodes <- as.data.frame(data$nodes)
  layers <- unique(as.data.frame(data$ties)$layer)
  lm <- build_mode_map(info, nodes, layers)$layers[[layer]]
  one_mode <- !isTRUE(lm$is_two_mode)

  wave_times <- wave_times %||% default_window(data, layer)
  if (length(wave_times) < 2L) {
    cli::cli_abort(
      "{.arg wave_times} needs at least two boundaries (one period).",
      call = call
    )
  }
  # Coerce once so an explicit POSIXct/Date grid (and the already-numeric
  # fallback above) both land numeric before `network_state_at()` and
  # `diff()` -- `diff()` on POSIXct/Date returns a non-numeric `difftime`
  # (`is.numeric.difftime` is FALSE by R's own definition), which would
  # otherwise fail `pin_intercept_only_rate()`'s numeric guard downstream.
  wave_times <- coerce_time(wave_times)
  states <- lapply(wave_times, function(w) {
    unname(network_state_at(data, layer, time = w))
  })
  counts <- vapply(
    states,
    function(st) endpoint_entity_count(st, entity, support_kind, one_mode),
    numeric(1)
  )
  n_periods <- length(wave_times) - 1L
  risk <- (counts[seq_len(n_periods)] + counts[-1L]) / 2
  count_w <- vapply(
    seq_len(n_periods),
    function(k) {
      hamming_count(states[[k]], states[[k + 1L]], support_kind, one_mode)
    },
    numeric(1)
  )
  list(
    count = count_w,
    duration = diff(wave_times),
    risk_set_size = risk,
    wave_times = if (n_periods > 1L) wave_times else NULL
  )
}

# The post-constraint support grid at one materialized state: TRUE where the
# flavor's rate entity is legal, self-loops removed for a one-mode layer.
support_grid_at <- function(state, support_kind, one_mode) {
  grid <- switch(
    support_kind,
    none = matrix(TRUE, nrow(state), ncol(state)),
    creation = state == 0,
    dissolution = state == 1
  )
  if (one_mode) {
    diag(grid) <- FALSE
  }
  grid
}

# The flavor's rate entity count at one materialized state: active senders (a
# `rowSums > 0` gate) for an actor-oriented flavor, active dyads for a
# tie-oriented one -- reusing the estimation-path mask assembler and counter.
endpoint_entity_count <- function(state, entity, support_kind, one_mode) {
  grid <- support_grid_at(state, support_kind, one_mode)
  active_1 <- rep(TRUE, nrow(grid))
  active_2 <- rep(TRUE, ncol(grid))
  if (identical(entity, "dyad")) {
    dyad <- assemble_model_mask(grid, active_1, active_2, 0L, model = "REM")
    active_dyad_count("point", dyad)
  } else {
    sum(assemble_model_mask(grid, active_1, active_2, 0L, model = "rate"))
  }
}

# The net Hamming diff between two wave states, restricted to the flavor's
# direction: creation counts 0 -> 1 cells, dissolution 1 -> 0, an unconstrained
# flavor every changed (non-self-loop) cell.
hamming_count <- function(prev, curr, support_kind, one_mode) {
  diff_cells <- switch(
    support_kind,
    none = prev != curr,
    creation = (prev == 0) & (curr == 1),
    dissolution = (prev == 1) & (curr == 0)
  )
  if (one_mode) {
    diag(diff_cells) <- FALSE
  }
  sum(diff_cells)
}

# Single-window (count, T, |R|) for a fully observed RELATIONAL flavor, read from
# goldfish's own preprocessed intercept scalars: `n_dep_events`, `total_time`,
# and the time-weighted `avg_active_entity`. The resulting single-period pin
# satisfies `exp(intercept_1) == n_dep_events / total_time / avg_active_entity`,
# exactly goldfish's intercept-only baseline-rate starting value -- the anchor
# tying the primitive to the estimator.
relational_window_risk_set <- function(data, layer, model = c("DyNAM", "REM")) {
  model <- match.arg(model)
  # The intercept scalars (n_dep_events / total_time / avg_active_entity) are
  # risk-set / event properties, independent of the rate's effects, so any valid
  # rate formula yields the same numbers -- a bare `~ 1` cannot be parsed ("a
  # model without effects cannot be estimated"), so a trivial effect stands in.
  rate <- if (identical(model, "REM")) ~ 1 + inertia else ~ 1 + indeg
  spec <- make_specification(
    rate = rate,
    layer = layer,
    model = model,
    data = data
  )
  prep <- if (identical(model, "REM")) {
    estimate_rem(spec, sub_model = "rate", preprocessing_only = TRUE)
  } else {
    estimate_dynam(spec, sub_model = "rate", preprocessing_only = TRUE)
  }
  list(
    count = prep$n_dep_events,
    duration = prep$total_time,
    risk_set_size = prep$avg_active_entity,
    wave_times = NULL
  )
}

# Warn once per filled gap, worded for the applied default and the consumer.
warn_completed_defaults <- function(added, consumer, call) {
  if (length(added) == 0L) {
    return(invisible(NULL))
  }
  add <- do.call(rbind, added)
  for (i in seq_len(nrow(add))) {
    default <- switch(
      add$sub_model[i],
      choice = "a uniform choice over the support-legal alternatives",
      choice_coordination = "a uniform coordination draw on both sides",
      rate_ordered = "a uniform ordered rate",
      rate = "a pinned intercept-only rate (zero free parameters)",
      "a zero-parameter default"
    )
    flavor_label <- if (is.na(add$flavor[i])) {
      ""
    } else {
      sprintf(" flavor {.val %s}", add$flavor[i])
    }
    cli::cli_warn(
      c(
        "!" = paste0(
          "Layer {.val ",
          add$layer[i],
          "}",
          flavor_label,
          " has no {.field ",
          add$family[i],
          "} sub-model; completing it with ",
          default,
          "."
        ),
        "i" = "The default adds no free parameter; it is auto-supplied for the
               {.field {consumer}} generative surface."
      ),
      class = "goldfish_completed_default_warning",
      call = call
    )
  }
  invisible(NULL)
}
