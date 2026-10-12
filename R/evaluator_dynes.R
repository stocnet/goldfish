# The pool evaluator behind the ABMCEM loop's evaluator contract (driven by
# estimate_dynes()). A pool is a set of endpoint-hitting sequences; each is
# preprocessed once, on pool entry, by one merged single-clock walk over the
# whole joint specification, and its per-fid statistics are stored so that
# re-evaluation at any parameter vector reuses them without re-preprocessing.
# This file owns the pool structure, the entry path, and the evaluation: one
# compiled-engine pass per free sub-model over the stored statistics, with no
# optimizer iteration. The proposal evaluator an MCMC chain reads and
# compute_lik_seq() score a single sequence through the same unit, so the
# importance weights, the acceptance ratio and the user-facing likelihood
# target one density.
#
# The target is the joint single-clock likelihood: a process's own events are
# dependent events for its fids, while every other modeled process's events --
# on another layer or another flavor -- are state changes and right-censoring
# boundaries for the timed rates, each exposed to the end of the observation
# window. Walking one fid at a time would drop both: a rate would see no
# censoring rows and its exposure would stop at its own last event.
#
# The loop is fid-indexed: theta concatenates each fid's free (non-offset)
# sub-model parameters, and every pooled sequence carries one goldfishStat per
# fid, keyed by fid. A sub-model descriptor is pool-level (shared across
# sequences, since only the drawn event stream differs from one sequence to the
# next); the statistics and the reference record are per sequence. The drawn
# data object is not kept once its statistics are: nothing evaluates it again.

# A pool-level sub-model descriptor: the formula/model/sub_model the existing
# engine preprocesses and evaluates, plus `theta_index`, the positions this
# sub-model's free effects occupy in the concatenated parameter vector. A fid's
# fixed effects are the formula's offset() terms, baked into the preprocessed
# statistics, so they never enter `theta_index`.
#
# A zero-length `theta_index` makes the sub-model FIXED-ONLY: a completed
# uniform choice, a completed pinned rate, or an authored fid whose effects are
# all offset(). It claims no slot in theta but still has a log-likelihood, which
# moves with the drawn path (the risk-set exposure and the legal choice sets
# follow the latent events), so it must stay in the target density wherever
# paths are compared. It is θ-free: evaluated once per sequence, never at a new
# theta, and never part of the score or Fisher information. `pinned_rate` is the
# frozen intercept-only rate object a completed timed rate carries in the
# completed specification's `completed_rates`. `fid` is the process-map row the
# sub-model scores, which is where its statistics are filed at pool entry.
new_pool_submodel <- function(
  formula,
  model = c("DyNAM", "REM", "DyNAMi"),
  sub_model = c("choice", "rate", "rate_ordered", "choice_coordination"),
  theta_index,
  pinned_rate = NULL,
  fid = NULL,
  call = rlang::caller_env()
) {
  if (!rlang::is_formula(formula)) {
    cli::cli_abort("{.arg formula} must be a formula.", call = call)
  }
  model <- rlang::arg_match(model)
  sub_model <- rlang::arg_match(sub_model)
  if (
    !is.numeric(theta_index) ||
      anyNA(theta_index) ||
      any(theta_index < 1) ||
      any(theta_index != as.integer(theta_index))
  ) {
    cli::cli_abort(
      "{.arg theta_index} must be a vector of positive integer positions.",
      call = call
    )
  }
  fixed_only <- length(theta_index) == 0
  if (!is.null(pinned_rate)) {
    if (!is_intercept_only_rate(pinned_rate)) {
      cli::cli_abort(
        "{.arg pinned_rate} must be a frozen intercept-only rate object.",
        call = call
      )
    }
    timed <- identical(
      behavior_timing(new_model_spec(model, sub_model)),
      "timed"
    )
    if (!fixed_only || !timed || !is_effect_free_formula(formula)) {
      cli::cli_abort(
        c(
          "{.arg pinned_rate} only applies to a fixed-only rate sub-model.",
          "i" = "A pinned rate has no free slot and no effects: use
                 {.code sub_model = \"rate\"}, an effect-free
                 {.code layer ~ 1} formula, and a zero-length
                 {.arg theta_index}."
        ),
        call = call
      )
    }
  }
  if (!is.null(fid) && !rlang::is_scalar_integerish(fid, finite = TRUE)) {
    cli::cli_abort("{.arg fid} must be a single integer.", call = call)
  }
  list(
    formula = formula,
    model = model,
    sub_model = sub_model,
    theta_index = as.integer(theta_index),
    fixed_only = fixed_only,
    pinned_rate = pinned_rate,
    engine_formula = pool_engine_formula(formula, model, sub_model),
    fid = if (!is.null(fid)) as.integer(fid)
  )
}

# A pool's decomposition: the pool-level sub-model descriptors and the pool
# engine spec their statistics come from. Pool entry walks `engine_spec` once
# per sequence and files each fid's statistics under its fid, so every
# sub-model must name a fid of that spec and score it through the formula the
# walk preprocesses there; a sub-model formula that differs from its fid's
# would evaluate statistics built for another model. A single `goldfishSpec`
# is accepted and held as its one-process join, the form the walk takes.
new_pool_decomposition <- function(
  sub_models,
  engine_spec,
  call = rlang::caller_env()
) {
  if (!is.list(sub_models) || length(sub_models) == 0) {
    cli::cli_abort(
      "{.arg sub_models} must be a non-empty list of sub-model descriptors.",
      call = call
    )
  }
  if (inherits(engine_spec, "goldfishSpec")) {
    engine_spec <- single_process_joint(engine_spec)
  }
  if (!inherits(engine_spec, "goldfishJointSpec")) {
    cli::cli_abort(
      "{.arg engine_spec} must be a {.cls goldfishJointSpec} or
       {.cls goldfishSpec}.",
      call = call
    )
  }
  bundles <- joint_fid_bundles(engine_spec)
  for (j in seq_along(sub_models)) {
    sm <- sub_models[[j]]
    entry <- if (!is.null(sm$fid)) bundles[[as.character(sm$fid)]]
    if (is.null(entry)) {
      cli::cli_abort(
        c(
          "Sub-model {.val {j}} does not name a fid of {.arg engine_spec}.",
          "i" = "Its {.field fid} must be one of {.val {names(bundles)}}."
        ),
        call = call
      )
    }
    if (!identical(format(sm$engine_formula), format(entry$bundle$formula))) {
      cli::cli_abort(
        c(
          "Sub-model {.val {j}} does not score the formula of fid
           {.val {sm$fid}}.",
          "x" = "The sub-model evaluates {.code {format(sm$engine_formula)}};
                 the walk preprocesses
                 {.code {format(entry$bundle$formula)}}."
        ),
        call = call
      )
    }
  }
  structure(
    list(sub_models = sub_models, engine_spec = engine_spec),
    class = c("goldfishDynesDecomposition", "list")
  )
}

# The spec pool entry walks: the completed joint specification with every
# effect-free bundle (a completed uniform choice or pinned rate) swapped for a
# bundle parsed from its pool_engine_formula(), since the walk refuses a model
# with no effect term. Built once, at decomposition. The swapped bundle keeps
# the `completed` mark, so the process map reads the same.
pool_engine_spec <- function(spec) {
  swap <- function(submodels, part) {
    for (family in names(submodels)) {
      bundle <- submodels[[family]]
      if (!is_effect_free_formula(bundle$formula)) {
        next
      }
      engine <- pool_engine_formula(
        bundle$formula,
        part$model,
        bundle$sub_model
      )
      swapped <- build_specification_bundle(
        rlang::new_formula(
          NULL,
          rlang::f_rhs(engine),
          env = rlang::f_env(engine)
        ),
        arg = family,
        model = part$model,
        sub_model = bundle$sub_model,
        layer = part$focal,
        envir = rlang::f_env(engine),
        data = spec$data
      )
      swapped$completed <- bundle$completed
      submodels[[family]] <- swapped
    }
    submodels
  }
  spec$specifications <- lapply(spec$specifications, function(part) {
    if (is.null(part$processes)) {
      part$submodels <- swap(part$submodels, part)
    } else {
      for (flavor in names(part$processes)) {
        part$processes[[flavor]]$submodels <- swap(
          part$processes[[flavor]]$submodels,
          part
        )
      }
    }
    part
  })
  spec
}

# The formula the engine preprocesses and evaluates. The engine refuses an
# effect-free right-hand side ("a model needs at least one effect term"), which
# is exactly what a completed uniform choice or a completed pinned rate carries
# (`layer ~ 1`). Such a sub-model is scored through a single column whose
# coefficient is fixed at 0: every alternative then shares one linear predictor,
# so a choice contributes −log |support(t)| per event and a rate reduces to its
# intercept. With every coefficient fixed the engine evaluates the likelihood
# only and never inverts the information matrix, so the column's values are
# irrelevant; any statistic the layer always supports will do. A timed rate
# keeps its intercept column, which carries the pinned plateau.
pool_engine_formula <- function(formula, model, sub_model) {
  if (!is_effect_free_formula(formula)) {
    return(formula)
  }
  spec <- new_model_spec(model, sub_model)
  lhs <- rlang::f_lhs(formula)
  zero_column <- if (identical(risk_set_axis(spec), "sender")) {
    rlang::expr(offset(outdeg(!!lhs), coef = 0))
  } else {
    rlang::expr(offset(inertia(!!lhs), coef = 0))
  }
  new_rhs <- if (identical(behavior_timing(spec), "timed")) {
    rlang::expr(1 + !!zero_column)
  } else {
    zero_column
  }
  rlang::new_formula(lhs, new_rhs, env = rlang::f_env(formula))
}

is_effect_free_formula <- function(formula) {
  rhs <- rlang::f_rhs(formula)
  identical(rhs, 1) || identical(rhs, 1L)
}

# A precomputed attachment for a pooled sequence: the per-fid statistics and
# the log-likelihood at the theta the sequence was drawn under, already
# materialized elsewhere (e.g. inside an MCMC chain's acceptance-ratio pass,
# where evaluating the target density cannot skip the statistics). Pool entry
# adopts it in place of its own walk only when it is consistent with the pool
# (see attachment_adoptable()); anything else is recomputed from scratch, so a
# stale or foreign attachment costs a walk and never corrupts the pool.
# `theta_ref` is the parameter vector the statistics and `loglik_ref` were
# computed at, and `layout` the pool_layout_tag() of the pool they were
# computed for.
new_pool_attachment <- function(
  stats,
  loglik_ref,
  theta_ref,
  layout,
  call = rlang::caller_env()
) {
  if (!is.list(stats)) {
    cli::cli_abort("{.arg stats} must be a list.", call = call)
  }
  if (!rlang::is_scalar_double(loglik_ref) || !is.finite(loglik_ref)) {
    cli::cli_abort(
      "{.arg loglik_ref} must be a single finite numeric value.",
      call = call
    )
  }
  if (!is.numeric(theta_ref)) {
    cli::cli_abort("{.arg theta_ref} must be a numeric vector.", call = call)
  }
  if (!rlang::is_string(layout)) {
    cli::cli_abort("{.arg layout} must be a single string.", call = call)
  }
  list(
    stats = stats,
    loglik_ref = loglik_ref,
    theta_ref = theta_ref,
    layout = layout
  )
}

# The layout of the statistics a pool's entry walk produces: which fids are
# scored, through which engine formulas, over which preprocessing window. Two
# walks with the same tag file the same statistics under the same fids, so a
# precomputed attachment carrying the pool's tag can stand in for the pool's
# own walk.
pool_layout_tag <- function(sub_models, control_prep) {
  rlang::hash(list(
    sub_models = lapply(sub_models, function(sm) {
      list(
        fid = sm$fid,
        formula = format(sm$engine_formula),
        model = sm$model,
        sub_model = sm$sub_model,
        theta_index = sm$theta_index
      )
    }),
    control_prep = unclass(control_prep)
  ))
}

# An attachment is adopted only when it was computed at the sequence's own
# reference theta (the theta the sequence was drawn under) for this pool's
# layout.
attachment_adoptable <- function(attachment, reference, layout) {
  !is.null(attachment) &&
    identical(attachment$layout, layout) &&
    identical(attachment$theta_ref, reference$theta_ref)
}

# Preprocess one pooled sequence: one merged walk of the pool engine spec
# pointed at the sequence's data, returning a goldfishStat per fid, keyed by
# the character fid. This is the per-sequence unit the entry map seam drives;
# it is the only place preprocess runs, so evaluation (which reuses these
# objects) never re-preprocesses. The walk reads only the joint-level data, but
# every member's copy is rebound too, so no stale copy is left to be read.
preprocess_pool_sequence <- function(
  engine_spec,
  data,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  walked <- preprocess_joint(
    bind_candidate_data(engine_spec, data, call = call),
    control_preprocessing = control_prep,
    progress = FALSE,
    verbose = FALSE
  )
  stats <- unclass(walked)
  attr(stats, "process_map") <- NULL
  stats
}

# The statistics a sequence stores for sub-model `sm`. A missing fid must not
# reach the engine as `preprocessed = NULL`, which would quietly preprocess the
# sub-model on its own and score it per process.
fid_stats <- function(stats, sm) {
  found <- stats[[as.character(sm$fid)]]
  if (is.null(found)) {
    cli::cli_abort(
      "No joint-walk statistics for fid {.val {sm$fid}}.",
      .internal = TRUE
    )
  }
  found
}

# Build a sequence pool: preprocess each sequence once, on entry, through the
# injectable per-sequence map seam (serial lapply by default), and store the
# statistics and the reference record with the sequence.
#
# * `decomposition`: a new_pool_decomposition() -- the sub-model descriptors,
#   shared across sequences, and the pool engine spec each sequence is walked
#   through.
# * `sequences`: a list of per-sequence inputs, each a list with `data` (a
#   stocnet the augmenter drew) and `reference` (a new_reference_record()). An
#   optional `attachment` (new_pool_attachment()) is adopted in place of the
#   entry walk when it is consistent with the pool, and otherwise ignored:
#   the random and simulation augmenters never carry one.
# * `theta_length`: the length of the concatenated parameter vector the pool is
#   evaluated at.
# * `map_fn`: the per-sequence map seam, `function(x, f)`; the serial lapply
#   default requires no optional dependency.
# * `store`: the resident pool store (open_pool_store()) the sequences are
#   sent to. The pool REPLACES whatever the store held. Without one, a serial
#   store over `map_fn` is opened for this pool.
build_sequence_pool <- function(
  decomposition,
  sequences,
  theta_length,
  control_prep = set_preprocessing(),
  map_fn = lapply,
  store = NULL,
  call = rlang::caller_env()
) {
  if (!inherits(decomposition, "goldfishDynesDecomposition")) {
    cli::cli_abort(
      "{.arg decomposition} must be a {.cls goldfishDynesDecomposition}.",
      call = call
    )
  }
  check_pool_sequences(sequences, call = call)
  if (
    !rlang::is_scalar_integerish(theta_length, finite = TRUE) ||
      theta_length < 1
  ) {
    cli::cli_abort(
      "{.arg theta_length} must be a single positive integer.",
      call = call
    )
  }
  theta_length <- as.integer(theta_length)
  for (sm in decomposition$sub_models) {
    if (any(sm$theta_index > theta_length)) {
      cli::cli_abort(
        "A sub-model's {.field theta_index} runs past {.arg theta_length}
         ({.val {theta_length}}).",
        call = call
      )
    }
  }

  if (is.null(store)) {
    # The coefficient layout depends on the formulas, not on the drawn events,
    # so one sequence's data resolves it for the whole pool.
    store <- open_pool_store(
      serial_map_seam(map_fn),
      pool_store_header(
        decomposition,
        theta_length,
        control_prep,
        sequences[[1]]$data,
        call = call
      )
    )
  } else if (!identical(store$header$theta_length, theta_length)) {
    cli::cli_abort(
      "{.arg theta_length} ({.val {theta_length}}) differs from the pool
       store's ({.val {store$header$theta_length}}).",
      call = call
    )
  }

  built <- pool_entries(store, sequences)
  tally_pool_entries(store, built)
  ids <- pool_store_mutate(store, built, replace = TRUE)
  header <- store$header
  structure(
    list(
      sub_models = header$sub_models,
      engine_spec = header$engine_spec,
      sequences = built,
      ids = ids,
      generation = store$generation,
      theta_length = header$theta_length,
      control_prep = header$control_prep,
      store = store
    ),
    class = c("goldfishDynesPool", "list")
  )
}

# Append `sequences` to `pool`: preprocessed on entry like a new pool, sent to
# the pool's store, and added after the existing sequences. Only the current
# pool can grow: appending to one a later mutation superseded would leave the
# store holding sequences this pool does not list.
append_sequence_pool <- function(pool, sequences, call = rlang::caller_env()) {
  check_pool_sequences(sequences, call = call)
  store <- pool$store
  if (!identical(pool$generation, store$generation)) {
    cli::cli_abort(
      "Only the current pool can grow; this one is at generation
       {.val {pool$generation}}, the store at {.val {store$generation}}.",
      class = "goldfish_dynes_stale_pool",
      call = call
    )
  }
  built <- pool_entries(store, sequences)
  tally_pool_entries(store, built)
  ids <- pool_store_mutate(store, built, replace = FALSE)
  pool$sequences <- c(pool$sequences, built)
  pool$ids <- c(pool$ids, ids)
  pool$generation <- store$generation
  pool
}

# The pool restricted to the sequences at `indices`, in that order: an SGD
# batch or the chunk a draw just added. It shares the store and generation, so
# it evaluates the same resident sequences.
pool_subset <- function(pool, indices) {
  pool$sequences <- pool$sequences[indices]
  pool$ids <- pool$ids[indices]
  pool
}

check_pool_sequences <- function(sequences, call) {
  if (!is.list(sequences) || length(sequences) == 0) {
    cli::cli_abort(
      "{.arg sequences} must be a non-empty list of pooled sequences.",
      call = call
    )
  }
  for (i in seq_along(sequences)) {
    seq_i <- sequences[[i]]
    if (!is.list(seq_i) || is.null(seq_i$data) || is.null(seq_i$reference)) {
      cli::cli_abort(
        "Pooled sequence {.val {i}} must be a list with {.field data} and
         {.field reference}.",
        call = call
      )
    }
  }
}

# Preprocess `sequences` once each through the store's map seam, then point
# their start states at the store's: entries built on daemons arrive as
# serialized copies, and a later chunk must share with the earlier ones.
pool_entries <- function(store, sequences) {
  header <- store$header
  built <- store$seam$map(
    sequences,
    pool_entry_task(
      header$sub_models,
      header$engine_spec,
      header$control_prep,
      header$layout
    )
  )
  share_start_state(built, store$start_state)
}

# The per-sequence pool-entry task: preprocess once (or adopt a consistent
# precomputed attachment) and store the statistics and the θ-free fixed-only
# term with the sequence. The attachment is not kept: adopted, its statistics
# are the entry's own; refused, they are stale. Built in its own frame so the
# map seam ships a worker the sub-models and the engine spec and nothing else
# of the pool; the arguments are rebound as plain values, since a forced
# promise can serialize with its caller's frame, which holds every sequence.
# `start_state` is a cache for share_start_state(): on the serial path every
# task of one map runs in this process, so each entry drops its duplicate
# start state as soon as it is built rather than after the whole chunk.
pool_entry_task <- function(sub_models, engine_spec, control_prep, layout) {
  sub_models <- sub_models
  engine_spec <- engine_spec
  control_prep <- control_prep
  layout <- layout
  start_state <- new.env(parent = emptyenv())
  function(seq_i) {
    stats <- if (
      attachment_adoptable(seq_i$attachment, seq_i$reference, layout)
    ) {
      seq_i$attachment$stats
    } else {
      preprocess_pool_sequence(
        engine_spec,
        seq_i$data,
        control_prep = control_prep,
        call = NULL
      )
    }
    entry <- new_pool_entry(sub_models, stats)
    entry$reference <- seq_i$reference
    share_start_state(list(entry), start_state)[[1L]]
  }
}

# A pooled sequence's evaluable part: its per-fid statistics and the θ-free
# term its fixed-only sub-models contribute, scored here, once. `NULL`
# statistics (a walk that failed) make a degenerate entry. An entry records why
# it is degenerate ("walk" or "fixed_term") and, for a fixed term, the failing
# sub-model's label; only the coordinator reads them, to report the losses.
new_pool_entry <- function(sub_models, stats) {
  if (is.null(stats)) {
    return(list(
      stats = NULL,
      fixed_loglik = -Inf,
      fixed_degenerate = TRUE,
      degenerate_cause = "walk",
      degenerate_label = NA_character_
    ))
  }
  fixed <- pool_fixed_loglik(sub_models, stats)
  list(
    stats = stats,
    fixed_loglik = fixed$loglik,
    fixed_degenerate = fixed$degenerate,
    degenerate_cause = fixed$cause,
    degenerate_label = fixed$label
  )
}

# Add a chunk of built entries to the store's run-long tally: every drawn
# sequence, those lost to a fixed-only term, and the sub-models those terms
# came from. It counts sequences a later redraw replaced, so it is a count of
# draws, not of the current pool.
tally_pool_entries <- function(store, built) {
  fixed_term <- vapply(
    built,
    function(e) identical(e$degenerate_cause, "fixed_term"),
    logical(1)
  )
  failed <- vapply(built[fixed_term], `[[`, character(1), "degenerate_label")
  store$tally$built <- store$tally$built + length(built)
  store$tally$fixed_term <- store$tally$fixed_term + sum(fixed_term)
  store$tally$labels <- unique(c(store$tally$labels, failed[!is.na(failed)]))
  invisible(store)
}

# The fields of a fid's statistics that the window's start state and the
# specification fix, not the drawn path: the statistics at t0, the initial
# risk sets, the node tables, and the formula and likelihood spec. Every
# endpoint-hitting sequence opens the window [t0, t_M] in the same observed
# state, so these are identical across the pool; on a choice fid the n x n x p
# `initial_stats` is most of an unshared entry once n reaches the hundreds.
pool_start_state_fields <- c(
  "initial_stats",
  "active_sender_init",
  "active_dyad_init",
  "node_lookup",
  "nodes",
  "nodes2",
  "model_spec",
  "formula"
)

# Point every entry's start-state fields at one object per fid held in
# `cache` (an environment, one per process and pool), so the pool holds each
# window's start state once rather than once per sequence. The first entry to
# reach a fid seeds the cache. A field is only replaced by the cached one when
# the two are equal, so a sequence whose start state differs keeps its own
# rather than being scored on another's.
share_start_state <- function(entries, cache) {
  lapply(entries, function(entry) {
    for (fid in names(entry$stats)) {
      stat <- entry$stats[[fid]]
      held <- cache[[fid]]
      if (is.null(held)) {
        fields <- intersect(pool_start_state_fields, names(stat))
        held <- stat[fields]
        cache[[fid]] <- held[!vapply(held, is.null, logical(1))]
        next
      }
      for (field in names(held)) {
        if (same_start_field(stat[[field]], held[[field]])) {
          stat[[field]] <- held[[field]]
        }
      }
      entry$stats[[fid]] <- stat
    }
    entry
  })
}

# identical() compares environments by address, so a formula or likelihood
# spec that crossed a process boundary never matches the cached one although
# it is the same value; those are compared by their serialization instead.
same_start_field <- function(x, y) {
  identical(x, y) ||
    ((is.language(x) || is.recursive(x)) &&
      identical(serialize(x, NULL), serialize(y, NULL)))
}

# The pool's reference records, in sequence order -- the per-sequence
# (theta_ref, loglik_ref, log_proposal) triples the E-step weighting reads.
pool_references <- function(pool) {
  lapply(pool$sequences, function(s) s$reference)
}

# Attach to every sub-model the layout of its engine coefficient vector
# ([intercept?, effects..., interactions...]): its length, the positions the
# formula leaves free, and the fixed positions with their values. The fixed
# positions come from the same assemble_fixed_parameters() the estimator reads
# (offset() terms, operand-only interaction columns), and the intercept follows
# the estimator's rule that a timed rate always carries one and no other
# sub-model does. The compiled engine takes the whole vector as given, so the
# fixed values must be placed by the caller. A `theta_index` whose length
# differs from the free count would put theta at the wrong coefficients, so it
# aborts here rather than surfacing as a degenerate sequence. A fixed-only
# sub-model claims no slot and is not checked: a pinned rate leaves its
# intercept free in the engine formula and sets it itself.
with_coefficient_masks <- function(sub_models, data, call) {
  lapply(seq_along(sub_models), function(j) {
    sm <- sub_models[[j]]
    parsed <- parse_formula(
      sm$engine_formula,
      realize_windows = FALSE,
      data = data
    )
    has_intercept <- identical(
      behavior_timing(new_model_spec(sm$model, sm$sub_model)),
      "timed"
    )
    n_params <- length(parsed$rhs_names) +
      length(parsed$interactions) +
      has_intercept
    fixed_spec <- suppressWarnings(assemble_fixed_parameters(
      parsed_formula = parsed,
      rhs_names = parsed$rhs_names,
      has_intercept = has_intercept,
      model = sm$model,
      sub_model = sm$sub_model,
      fixed_parameters = NULL,
      offset_coef = NULL
    ))
    free <- setdiff(seq_len(n_params), fixed_spec$idx)
    if (!sm$fixed_only && length(free) != length(sm$theta_index)) {
      cli::cli_abort(
        c(
          "Sub-model {.val {j}} has {length(free)} free coefficient{?s} but
           {length(sm$theta_index)} {.field theta_index} position{?s}.",
          "i" = "{.field theta_index} numbers the free coefficients only:
                 the time intercept of a rate counts, {.fn offset} terms do
                 not."
        ),
        class = "goldfish_theta_index_mismatch",
        call = call
      )
    }
    sm$coefficients <- list(
      n_params = n_params,
      free = free,
      fixed = as.integer(fixed_spec$idx),
      fixed_values = as.numeric(fixed_spec$values),
      has_intercept = has_intercept
    )
    sm
  })
}

# The full engine coefficient vector of a masked sub-model: `theta_block` at
# the free positions and the formula's values at the fixed ones.
pool_engine_parameters <- function(sub_model, theta_block = numeric(0)) {
  coefficients <- sub_model$coefficients
  parameters <- numeric(coefficients$n_params)
  parameters[coefficients$free] <- theta_block
  parameters[coefficients$fixed] <- coefficients$fixed_values
  parameters
}

# One compiled-engine pass of a masked sub-model over its stored statistics at
# the full coefficient vector `parameters`. The closure is built here, at
# evaluation time, and dropped with the call: held with the sequence it would
# keep a dense copy of the start state (`stat_mat_init`) per sequence, which
# on a large-n choice fid outweighs the rest of the pooled sequence. The
# intercept is never seeded -- an evaluation uses the vector verbatim.
pool_engine_pass <- function(sub_model, stats, parameters) {
  engine <- make_engine_evaluator(
    spec = stats$model_spec,
    stats_list = stats,
    parameters = parameters,
    backend = "cpp",
    has_intercept = sub_model$coefficients$has_intercept,
    is_two_mode = isTRUE(stats$model_spec$is_two_mode),
    seed_intercept = FALSE
  )
  engine$evaluate(parameters, need_scores = FALSE)
}

# The log-likelihood, and the score and Fisher information at the free
# positions, of one free sub-model at `theta_block`: one compiled pass, with no
# optimizer iteration. The kernel returns all three on every pass, so no
# quantity is cheaper to skip here. The information matrix is not inverted: a
# singular block with finite entries is an ordinary sequence, since neither its
# importance weight nor the pooled information it adds to needs its own block
# to be invertible. Only a non-finite log-likelihood or a missing score or
# information entry makes the evaluation fail.
pool_cpp_eval <- function(sub_model, stats, theta_block) {
  res <- pool_engine_pass(
    sub_model,
    stats,
    pool_engine_parameters(sub_model, theta_block)
  )
  free <- sub_model$coefficients$free
  score <- as.numeric(res$derivative)[free]
  fisher <- res$fisher[free, free, drop = FALSE]
  if (!is.finite(res$logLikelihood) || anyNA(score) || anyNA(fisher)) {
    cli::cli_abort("The engine could not evaluate the sub-model at theta.")
  }
  list(loglik = res$logLikelihood, score = score, fisher = fisher)
}

# The θ-free log-likelihood a sequence's fixed-only sub-models contribute,
# summed. Computed once per sequence (at pool entry, or once per candidate in
# the proposal evaluator), since no parameter value can change it. The
# sub-models must carry their coefficient masks (with_coefficient_masks()). An
# engine failure, or a term the engine returns non-finite (a pinned rate whose
# plateau is -Inf over events), marks the sequence degenerate, like a free
# sub-model the engine cannot evaluate. Every non-finite term becomes -Inf, the
# only value stable_softmax() is sure to weight zero: +Inf would take the whole
# weight and NaN/NA would poison the normalizer. A degenerate result names its
# `cause` and the `label` of the sub-model whose term failed, so a run can
# report which term it lost sequences to.
pool_fixed_loglik <- function(sub_models, stats) {
  loglik <- 0
  for (sm in sub_models) {
    if (!sm$fixed_only) {
      next
    }
    term <- tryCatch(
      fixed_only_loglik(sm, fid_stats(stats, sm)),
      error = function(e) NULL
    )
    if (is.null(term) || !is.finite(term)) {
      return(list(
        loglik = -Inf,
        degenerate = TRUE,
        cause = "fixed_term",
        label = sm$label %||% NA_character_
      ))
    }
    loglik <- loglik + term
  }
  list(
    loglik = loglik,
    degenerate = FALSE,
    cause = NA_character_,
    label = NA_character_
  )
}

# One fixed-only sub-model's log-likelihood over its stored statistics, at the
# values its formula fixes: one compiled-engine pass, of which only the
# log-likelihood is read. An authored all-offset fid or a completed uniform
# choice is that pass as is. A pinned rate is scored through its per-period
# column instead: see pinned_rate_loglik().
fixed_only_loglik <- function(sub_model, stats) {
  if (!is.null(sub_model$pinned_rate)) {
    return(pinned_rate_loglik(sub_model, stats))
  }
  res <- pool_engine_pass(sub_model, stats, pool_engine_parameters(sub_model))
  res$logLikelihood
}

# A pinned timed rate is not one fixed value: it has one plateau c_w per wave
# period, while the engine carries a single intercept coefficient. Its
# log-likelihood
#
#   Σ_e c_{w(e)} − Σ_w e^{c_w} ∫_w |R(t)| dt
#
# is therefore read off one engine pass at intercept 0, where each interval's
# log-likelihood is exactly minus its exposure ∫ |R(t)| dt (the event term
# log e^0 vanishes and the only other column has coefficient 0). Each interval's
# exposure is then scaled by the plateau of the period it lies in, and each
# dependent event adds the plateau of the period its time falls in -- the
# half-open membership, so an event on an interior boundary belongs to the next
# period. This equals an intercept column holding c_{w} per interval with its
# coefficient fixed at 1. An interval that crosses an interior wave boundary
# has no single plateau, so its exposure is split at the boundary (see
# pinned_rate_compensator()). A single-plateau pin is the K = 1 case.
pinned_rate_loglik <- function(sub_model, stats, call = NULL) {
  rate <- sub_model$pinned_rate
  # The intercept is the engine formula's one free coefficient.
  res <- pool_engine_pass(
    sub_model,
    stats,
    pool_engine_parameters(sub_model, 0)
  )
  exposure <- -as.numeric(res$intervalLogL)
  end <- stats$event_time
  start <- end - stats$intervals

  dependent <- stats$is_dependent == 1L
  event_plateau <- rate$intercept[
    intercept_only_rate_period(rate, end[dependent], call = call)
  ]
  sum(event_plateau) -
    pinned_rate_compensator(rate, start, end, exposure, call = call)
}

# The compensator sum_w e^{c_w} * integral_w |R(t)| dt over the intervals
# (start, end], given each interval's exposure |R_k| * (end - start). The risk
# set only changes at an interval's end, so |R(t)| is constant inside it: an
# interval crossing interior wave boundaries is cut there and each piece takes
# the exposure share of its width at its own period's plateau. This is exactly
# what an event placed on each boundary would give, without re-preprocessing.
pinned_rate_compensator <- function(rate, start, end, exposure, call) {
  interior <- rate$wave_times[-c(1L, length(rate$wave_times))]
  per_interval <- vapply(
    seq_along(start),
    function(k) {
      cuts <- interior[interior > start[[k]] & interior < end[[k]]]
      if (length(cuts) == 0L) {
        plateau <- rate$intercept[
          intercept_only_rate_period(rate, start[[k]], call = call)
        ]
        return(exp(plateau) * exposure[[k]])
      }
      edges <- c(start[[k]], cuts, end[[k]])
      share <- diff(edges) / (end[[k]] - start[[k]])
      plateau <- rate$intercept[
        intercept_only_rate_period(rate, edges[-length(edges)], call = call)
      ]
      sum(exp(plateau) * share) * exposure[[k]]
    },
    numeric(1)
  )
  sum(per_interval)
}

# Evaluate one pooled sequence at the concatenated `theta`: split theta across
# the sub-models, evaluate each over its stored statistics, and reassemble the
# per-sequence quantities on the full free-parameter dimension `p` -- the total
# log-likelihood (summed across sub-models), the concatenated score (each
# sub-model's score placed at its theta_index), and the block-diagonal Fisher
# information (no cross-sub-model terms in this version). Fixed-only sub-models
# are not re-evaluated: their θ-free term, stored at pool entry, is added to the
# log-likelihood and they place nothing in the score or Fisher information,
# so it cancels in any ratio across theta on one path. A sub-model the engine
# cannot evaluate at theta (a non-finite log-likelihood, or a missing score or
# information entry) makes the whole sequence degenerate: its result is
# non-finite (-Inf log-likelihood, so its importance weight vanishes), which the
# caller reports without aborting the pool. A singular information matrix with
# finite entries does not.
#
# Each free sub-model is one compiled pass over a closure built at evaluation
# time (pool_cpp_eval()). `what` is honored at the harvest level: every pass
# computes all three quantities, and only the requested ones (plus
# `degenerate`) are returned. `pool` needs only the masked `sub_models` and
# `theta_length`, so a single candidate is evaluated through the same code.
evaluate_pool_sequence <- function(
  pool,
  sequence,
  theta,
  what = c("loglik", "score", "fisher")
) {
  what <- rlang::arg_match(what, multiple = TRUE)
  sub_models <- pool$sub_models
  p <- pool$theta_length
  # The fixed-only sub-models' θ-free term, stored at pool entry.
  loglik <- sequence$fixed_loglik
  score <- numeric(p)
  fisher <- matrix(0, p, p)
  degenerate <- sequence$fixed_degenerate

  for (sm in sub_models) {
    if (degenerate) {
      break
    }
    if (sm$fixed_only) {
      next
    }
    harvest <- tryCatch(
      pool_cpp_eval(sm, fid_stats(sequence$stats, sm), theta[sm$theta_index]),
      error = function(e) NULL
    )
    if (is.null(harvest)) {
      degenerate <- TRUE
      break
    }
    loglik <- loglik + harvest$loglik
    score[sm$theta_index] <- harvest$score
    fisher[sm$theta_index, sm$theta_index] <- harvest$fisher
  }

  if (degenerate) {
    loglik <- -Inf
    score <- rep(NA_real_, p)
    fisher <- matrix(NA_real_, p, p)
  }

  harvest <- list(loglik = loglik, score = score, fisher = fisher)
  c(harvest[what], list(degenerate = degenerate))
}

# The evaluator contract: evaluate every pooled sequence at the concatenated
# `theta` and return the requested quantities per sequence. `what` is honored
# at the harvest level -- the engine computes what it does, and only the
# requested quantities are returned, so a loglik- or score-only kernel can
# later optimize the harvest without changing any call site. Per-sequence
# granularity is contract: the score is returned per sequence (a K x p matrix,
# never a pooled total) because the covariance estimators need the per-sequence
# outer products. `degenerate` flags the sequences whose evaluation was
# non-finite. The sequences are read from the pool's resident store,
# so each task carries only a sequence id, theta, and the pool generation.
evaluate_sequence_pool <- function(
  pool,
  theta,
  what = c("loglik", "score", "fisher"),
  call = rlang::caller_env()
) {
  if (!inherits(pool, "goldfishDynesPool")) {
    cli::cli_abort(
      "{.arg pool} must be a {.cls goldfishDynesPool}.",
      call = call
    )
  }
  what <- rlang::arg_match(what, multiple = TRUE)
  if (length(theta) != pool$theta_length) {
    cli::cli_abort(
      "{.arg theta} must have length {.val {pool$theta_length}}, not
       {.val {length(theta)}}.",
      call = call
    )
  }

  store <- pool$store
  per_sequence <- store$seam$map(
    as.list(pool$ids),
    store_evaluation_task(store$key, pool$generation, theta)
  )

  out <- list()
  if ("loglik" %in% what) {
    out$loglik <- vapply(per_sequence, function(r) r$loglik, numeric(1))
  }
  if ("score" %in% what) {
    out$score <- do.call(
      rbind,
      lapply(per_sequence, function(r) r$score)
    )
  }
  if ("fisher" %in% what) {
    out$fisher <- lapply(per_sequence, function(r) r$fisher)
  }
  out$degenerate <- vapply(per_sequence, function(r) r$degenerate, logical(1))
  out
}

# The theta_k-closed proposal evaluator the ABEM loop builds once per EM
# iteration and injects into an augmenter's init(), where an MCMC move reads
# f(candidate) for its acceptance ratio: one joint walk plus one likelihood
# pass per candidate, the same pool-entry walk and engine the pool evaluator
# uses, so the acceptance ratio and the importance weights target one density.
# `decomposition` is the one `build_sequence_pool()` takes; the loop supplies
# it at each iteration, closing over the current `theta`.
#
# The returned closure answers by NAME, not position -- `list(loglik, rates)`
# -- so a later fast-follow that reuses a retained MCMC draw's statistics can
# widen it to `list(loglik, rates, stats_handle)` without breaking a caller
# that reads `$loglik`/`$rates`. No reuse logic ships here: a candidate whose
# walk fails or whose evaluation is non-finite reads as `-Inf`, the pool
# evaluator's degenerate sequence (evaluate_candidate()).
#
# `rate_queries` asks for named-pair intensities at positions inside the
# candidate -- what a proposal's reverse density is drawn at. Answering it
# needs `spec`, the completed joint specification the sub-models were
# decomposed from: the candidate is rebound to it (never recompleted, so pinned
# rates cannot move between proposals) and the pairs are read off a walk
# replayed over the candidate, the same evaluation path simulate() draws with.
# The flat theta reaches the walk through the specification's coefficient
# layout, so the sub-models' `theta_index` must number that layout's free slots;
# this is checked here, once, rather than surfacing as rates evaluated at
# parameters the likelihood never saw. The log-likelihood path never reads
# `spec`, and a call without `rate_queries` opens no rate walk: the two paths
# stay separate, one walk each. `loglik = FALSE` answers the rate queries
# alone: an MCMC move reads its forward rates on the current sequence, whose
# log-likelihood it already holds, before it can draw the candidate.
#
# `legality = list(interval, waves)` also decides the candidate's support
# legality over that interval, in the same walk that answers its rate queries
# (candidate_pass()), opened with this evaluator's `control_prep`, the one its
# log-likelihood is scored with. The answer then carries `legal`; an illegal
# candidate gets no rates and no log-likelihood (`NA`). A call without
# `legality` is answered as described above.
make_proposal_evaluator <- function(
  decomposition,
  theta,
  spec = NULL,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  if (!inherits(decomposition, "goldfishDynesDecomposition")) {
    cli::cli_abort(
      "{.arg decomposition} must be a {.cls goldfishDynesDecomposition}.",
      call = call
    )
  }
  if (!is.numeric(theta)) {
    cli::cli_abort("{.arg theta} must be a numeric vector.", call = call)
  }
  theta_by_fid <- NULL
  if (!is.null(spec)) {
    assert_theta_layout(spec, decomposition$sub_models, call = call)
    theta_by_fid <- flat_to_fid_theta(spec, theta, call = call)
  }

  score_candidate <- function(candidate_seq) {
    evaluate_candidate(
      decomposition,
      candidate_seq,
      theta,
      what = "loglik",
      control_prep = control_prep,
      call = call
    )$loglik
  }
  need_spec <- function() {
    if (is.null(spec)) {
      cli::cli_abort(
        c(
          "Rate queries need the completed joint specification.",
          "i" = "Supply {.arg spec} to {.fn make_proposal_evaluator}."
        ),
        call = call
      )
    }
  }

  function(candidate_seq, rate_queries = NULL, loglik = TRUE, legality = NULL) {
    if (!is.null(legality)) {
      need_spec()
      pass <- candidate_pass(
        bind_candidate_data(spec, candidate_seq, call = call),
        theta_by_fid,
        rate_queries %||% list(),
        legality,
        control_prep = control_prep,
        call = call
      )
      if (!pass$legal) {
        return(list(legal = FALSE, rates = NULL, loglik = NA_real_))
      }
      rates <- if (!is.null(rate_queries)) pass$rates
      scored <- if (isTRUE(loglik)) {
        score_candidate(candidate_seq)
      } else {
        NA_real_
      }
      return(list(legal = TRUE, rates = rates, loglik = scored))
    }
    rates <- NULL
    if (!is.null(rate_queries)) {
      need_spec()
      rates <- query_rate_at_position(
        bind_candidate_data(spec, candidate_seq, call = call),
        theta_by_fid,
        rate_queries,
        control_prep = control_prep,
        call = call
      )
    }
    if (!isTRUE(loglik)) {
      return(list(loglik = NA_real_, rates = rates))
    }
    list(loglik = score_candidate(candidate_seq), rates = rates)
  }
}

# One sequence outside any pool, scored exactly as a pooled one: walked once
# (the entry path's walk), its fixed-only term scored, then one pass of
# evaluate_pool_sequence(). A walk that fails, a fixed-only term the engine
# cannot score or scores non-finite, and a non-finite free evaluation all make
# the result degenerate
# (-Inf log-likelihood) rather than an error. A candidate that does not fit
# the engine spec's actors aborts first, outside that guard, so it is never
# read as a -Inf density.
evaluate_candidate <- function(
  decomposition,
  data,
  theta,
  what = c("loglik", "score", "fisher"),
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  engine_spec <- decomposition$engine_spec
  sub_models <- with_coefficient_masks(
    decomposition$sub_models,
    data,
    call = call
  )
  bind_candidate_data(engine_spec, data, call = call)
  stats <- tryCatch(
    preprocess_pool_sequence(
      engine_spec,
      data,
      control_prep = control_prep,
      call = call
    ),
    error = function(e) NULL
  )
  evaluate_pool_sequence(
    list(sub_models = sub_models, theta_length = length(theta)),
    new_pool_entry(sub_models, stats),
    theta,
    what = what
  )
}

#' Log-likelihood of one complete sequence under a joint specification
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `compute_lik_seq()` scores one complete event sequence -- every modeled
#' process observed in continuous time, such as a sequence augmented between
#' panel waves -- under a joint specification at the parameters `theta`. It
#' returns the joint single-clock log-likelihood with its score and Fisher
#' information over the free coefficients: the per-sequence quantities that
#' DyNES estimation weights over its pool of augmented sequences, computed
#' through the same evaluation.
#'
#' @details
#' The specification is completed for joint estimation first, on the same
#' wave grid [estimate_dynes()] resolves from `wave_times` and the `start_time`
#' of `control_prep`, so a pinned rate has one plateau per wave period. A
#' specification that is already completed, such as the one a DyNES fit
#' holds, keeps its pinned rates and the plateaus they were pinned on; each
#' pinned rate is announced again with a warning.
#' Completed defaults (a pinned intercept-only rate, a uniform choice) and
#' `offset()` effects have no free coefficient: they add their log-likelihood
#' but no score or Fisher information.
#'
#' A sequence whose likelihood cannot be evaluated at `theta` -- its
#' preprocessing fails, a fixed-only term, such as a pinned rate, has a
#' non-finite log-likelihood, or a sub-model's log-likelihood, score or Fisher
#' information is non-finite there -- is reported as `degenerate`, with a
#' log-likelihood of `-Inf` and missing score and information, rather than as
#' an error. A singular Fisher information with finite entries, such as an
#' effect whose statistic is zero on the sequence, is not degenerate: the
#' sequence keeps its finite log-likelihood, score and information.
#'
#' @param sequence The complete sequence: a data object of the specification's
#'   actors and layers, whose modeled layers hold every event of the sequence.
#' @param theta The free coefficients, a numeric vector in the order of the
#'   free rows of [coef_layout()]. A named vector is matched to the composite
#'   labels `"<process>: <effect>"` instead.
#' @inheritParams estimate_dynes
#'
#' @return A list with the log-likelihood `loglik`, the named `score` vector,
#'   the Fisher information `fisher` (a matrix named like the score), and the
#'   `degenerate` flag.
#' @seealso [coef_layout()], [make_joint_specification()]
#' @export
compute_lik_seq <- function(
  spec,
  sequence,
  theta,
  control_prep = set_preprocessing(),
  wave_times = NULL
) {
  lifecycle::signal_stage("experimental", "compute_lik_seq()")
  call <- rlang::current_env()
  if (!inherits(spec, "goldfishJointSpec")) {
    cli::cli_abort(
      c(
        "{.arg spec} must be a {.cls goldfishJointSpec}.",
        "i" = "Build one with {.fn make_joint_specification}."
      ),
      call = call
    )
  }
  # An authored spec with a modeled panel layer is pinned on the wave grid
  # estimate_dynes() would infer, not on completion's single default window,
  # so a fit's authored and completed specs score a sequence alike. A
  # completed spec keeps the grid it was pinned on unless one is supplied.
  authored <- is.null(spec$process_map$completed)
  if (!is.null(wave_times) || (authored && length(spec$modeled_panel) > 0L)) {
    wave_times <- panel_wave_grid(
      spec,
      start_time = control_prep$start_time,
      wave_times = wave_times,
      call = call
    )
  }
  completed <- complete_generative_spec(
    spec,
    consumer = "compute_lik_seq",
    wave_times = wave_times
  )
  layout <- coef_layout(completed)
  labels <- paste0(layout$process, ": ", layout$name)[!layout$fixed]
  theta <- check_sequence_theta(theta, labels, call = call)

  res <- evaluate_candidate(
    decompose_joint_spec(completed, call = call),
    sequence,
    theta,
    control_prep = control_prep,
    call = call
  )
  names(res$score) <- labels
  dimnames(res$fisher) <- list(labels, labels)
  res
}

# `theta` as the flat free vector in coef_layout() order: one value per free
# coefficient, matched by label when named.
check_sequence_theta <- function(theta, labels, call) {
  n <- length(labels)
  if (!is.numeric(theta) || anyNA(theta)) {
    cli::cli_abort(
      "{.arg theta} must be a numeric vector without missing values.",
      call = call
    )
  }
  if (length(theta) != n) {
    cli::cli_abort(
      c(
        "{.arg theta} must have one value per free coefficient.",
        "x" = "The specification has {n} free coefficient{?s};
               {.arg theta} has {length(theta)} value{?s}."
      ),
      call = call
    )
  }
  if (is.null(names(theta))) {
    return(unname(theta))
  }
  unknown <- setdiff(names(theta), labels)
  if (length(unknown) > 0L || anyDuplicated(names(theta))) {
    cli::cli_abort(
      c(
        "A named {.arg theta} must name each free coefficient once.",
        "x" = "Not a free coefficient: {.val {unknown}}.",
        "i" = "The free coefficients are {.val {labels}}."
      ),
      call = call
    )
  }
  unname(theta[labels])
}
