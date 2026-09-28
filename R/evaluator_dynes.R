# The prototype-path pool evaluator behind the ABMCEM loop's evaluator contract
# (driven by estimate_dynes()). A pool is a set of endpoint-hitting sequences;
# each is preprocessed once, on pool entry, by one merged single-clock walk
# over the whole joint specification, and its per-fid statistics are stored so
# that re-evaluation at any parameter vector reuses them without
# re-preprocessing. This file owns the pool structure and the entry path; the
# zero-iteration evaluation adapter that reads the stored statistics is defined
# alongside it.
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
# next); the statistics, the data object, and the reference record are per
# sequence.

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

# A precomputed attachment for a pooled sequence: the per-sub-model statistics
# and the log-likelihood at the theta the sequence was evaluated under
# already materialized elsewhere (e.g. inside an MCMC chain's acceptance-ratio
# pass). Accepted on pool entry and ignored in this version -- every routine
# takes the from-scratch preprocess path, which is never slower than reuse
# and needs no agreement on the statistics layout -- present only so a later
# fast-follow that reuses a retained MCMC draw's statistics is a drop-in with
# no signature change on the pool-entry path.
new_pool_attachment <- function(
  stats,
  loglik_ref,
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
  list(stats = stats, loglik_ref = loglik_ref)
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
# statistics, the data object, and the reference record with the sequence.
#
# * `decomposition`: a new_pool_decomposition() -- the sub-model descriptors,
#   shared across sequences, and the pool engine spec each sequence is walked
#   through.
# * `sequences`: a list of per-sequence inputs, each a list with `data` (a
#   stocnet the augmenter drew) and `reference` (a new_reference_record()). An
#   optional `attachment` slot is accepted and ignored in this version -- the
#   from-scratch entry path is uniform for every augmenter -- so a later
#   precomputed-statistics fast path is a drop-in with no signature churn.
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

# Preprocess `sequences` once each through the store's map seam.
pool_entries <- function(store, sequences) {
  header <- store$header
  store$seam$map(
    sequences,
    pool_entry_task(
      header$sub_models,
      header$engine_spec,
      header$control_prep
    )
  )
}

# The per-sequence pool-entry task: preprocess once and store the statistics
# and the θ-free fixed-only term with the sequence. Built in its own frame so
# the map seam ships a worker the sub-models and the engine spec and nothing
# else of the pool; the arguments are rebound as plain values, since a forced
# promise can serialize with its caller's frame, which holds every sequence.
pool_entry_task <- function(sub_models, engine_spec, control_prep) {
  sub_models <- sub_models
  engine_spec <- engine_spec
  control_prep <- control_prep
  function(seq_i) {
    stats <- preprocess_pool_sequence(
      engine_spec,
      seq_i$data,
      control_prep = control_prep,
      call = NULL
    )
    fixed <- pool_fixed_loglik(
      sub_models,
      seq_i$data,
      stats,
      control_prep = control_prep,
      call = NULL
    )
    list(
      data = seq_i$data,
      stats = stats,
      fixed_loglik = fixed$loglik,
      fixed_degenerate = fixed$degenerate,
      reference = seq_i$reference,
      attachment = seq_i$attachment
    )
  }
}

# The pool's reference records, in sequence order -- the per-sequence
# (theta_ref, loglik_ref, log_proposal) triples the E-step weighting reads.
pool_references <- function(pool) {
  lapply(pool$sequences, function(s) s$reference)
}

# One zero-iteration engine evaluation of a single sub-model over its stored
# statistics: run the existing Newton engine for zero optimizer iterations at
# `theta_block` (the sub-model's slice of the concatenated theta over its free
# effects), harvesting the log-likelihood, score, and Fisher information the
# engine computes at that vector. The stored statistics are reused through
# `preprocessed`, so no re-preprocessing happens. `theta_block` covers only the
# free coefficients, while the engine takes one value per coefficient: it is
# placed at the sub-model's free positions (see with_coefficient_masks()), and
# the engine overwrites the fixed positions with the formula's fixed values.
# The score and Fisher information are harvested at the free positions only.
#
# The engine inverts the information matrix unconditionally and stop()s on a
# singular one (even for a likelihood-only request), so a singular sequence
# surfaces here as an error; the per-sequence caller turns that into a
# non-finite result rather than aborting the pool.
pool_engine_eval <- function(
  sub_model,
  data,
  stats,
  theta_block,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  free <- sub_model$coefficients$free
  parameters <- numeric(sub_model$coefficients$n_params)
  parameters[free] <- theta_block
  fit <- estimate_wrapper(
    x = sub_model$engine_formula,
    model = sub_model$model,
    sub_model = sub_model$sub_model,
    data = data,
    preprocessed = stats,
    control_algo = set_algorithm_newton(
      initial_parameters = parameters,
      max_iterations = 0L,
      backend = "r"
    ),
    control_prep = control_prep,
    verbose = FALSE,
    progress = FALSE
  )
  list(
    loglik = fit$log_likelihood,
    score = fit$final_score[free],
    fisher = fit$final_information_matrix[free, free, drop = FALSE]
  )
}

# Attach to every free sub-model the layout of its engine coefficient vector
# ([intercept?, effects..., interactions...]): its length and the positions the
# formula leaves free. The fixed positions come from the same
# assemble_fixed_parameters() the estimator reads (offset() terms, operand-only
# interaction columns), and the intercept follows the estimator's rule that a
# timed rate always carries one and no other sub-model does. A `theta_index`
# whose length differs from the free count would put theta at the wrong
# coefficients, so it aborts here rather than surfacing as a degenerate
# sequence.
with_coefficient_masks <- function(sub_models, data, call) {
  lapply(seq_along(sub_models), function(j) {
    sm <- sub_models[[j]]
    if (sm$fixed_only) {
      return(sm)
    }
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
    if (length(free) != length(sm$theta_index)) {
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
    sm$coefficients <- list(n_params = n_params, free = free)
    sm
  })
}

# The θ-free log-likelihood a sequence's fixed-only sub-models contribute,
# summed. Computed once per sequence (at pool entry, or once per candidate in
# the proposal evaluator), since no parameter value can change it. An engine
# failure marks the sequence degenerate, like a singular free sub-model.
pool_fixed_loglik <- function(
  sub_models,
  data,
  stats,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  loglik <- 0
  for (sm in sub_models) {
    if (!sm$fixed_only) {
      next
    }
    term <- tryCatch(
      fixed_only_loglik(sm, data, fid_stats(stats, sm), control_prep, call),
      error = function(e) NULL
    )
    if (is.null(term)) {
      return(list(loglik = -Inf, degenerate = TRUE))
    }
    loglik <- loglik + term
  }
  list(loglik = loglik, degenerate = FALSE)
}

# One fixed-only sub-model's log-likelihood over its stored statistics, at the
# values its formula fixes. An authored all-offset fid or a completed uniform
# choice is a plain likelihood-only engine pass. A pinned rate is scored
# through its per-period column instead: see pinned_rate_loglik().
fixed_only_loglik <- function(sub_model, data, stats, control_prep, call) {
  if (!is.null(sub_model$pinned_rate)) {
    return(pinned_rate_loglik(sub_model, data, stats, control_prep, call))
  }
  fit <- estimate_wrapper(
    x = sub_model$engine_formula,
    model = sub_model$model,
    sub_model = sub_model$sub_model,
    data = data,
    preprocessed = stats,
    control_algo = set_algorithm_newton(max_iterations = 0L, backend = "r"),
    control_prep = control_prep,
    verbose = FALSE,
    progress = FALSE
  )
  fit$log_likelihood
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
pinned_rate_loglik <- function(sub_model, data, stats, control_prep, call) {
  rate <- sub_model$pinned_rate
  fit <- estimate_wrapper(
    x = sub_model$engine_formula,
    model = sub_model$model,
    sub_model = sub_model$sub_model,
    data = data,
    preprocessed = stats,
    control_algo = set_algorithm_newton(
      initial_parameters = c(Intercept = 0),
      max_iterations = 0L,
      backend = "r",
      diagnostics = "loglik"
    ),
    control_prep = control_prep,
    verbose = FALSE,
    progress = FALSE
  )
  exposure <- -fit$interval_log_lik
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
# so it cancels in any ratio across theta on one path. A sub-model whose
# information matrix is singular at theta makes the whole sequence degenerate:
# its result is non-finite (-Inf log-likelihood, so its importance weight
# vanishes), which the caller reports without aborting the pool.
evaluate_pool_sequence <- function(pool, sequence, theta, call) {
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
      pool_engine_eval(
        sm,
        sequence$data,
        fid_stats(sequence$stats, sm),
        theta[sm$theta_index],
        control_prep = pool$control_prep,
        call = call
      ),
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

  list(loglik = loglik, score = score, fisher = fisher, degenerate = degenerate)
}

# The evaluator contract's v1 implementation: evaluate every pooled sequence at
# the concatenated `theta` and return the requested quantities per sequence.
# `what` is honored at the harvest level -- the engine computes what it does,
# and only the requested quantities are returned, so a batched evaluator can
# later optimize the harvest without changing any call site. Per-sequence
# granularity is contract: the score is returned per sequence (a K x p matrix,
# never a pooled total) because the covariance estimators need the per-sequence
# outer products. `degenerate` flags the sequences a singular-information guard
# marked non-finite. The sequences are read from the pool's resident store,
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
# that reads `$loglik`/`$rates`. No reuse logic ships here: a singular
# candidate reads as `-Inf`, mirroring the pool evaluator's singular-information
# guard in evaluate_pool_sequence().
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
# stay separate, one walk each.
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
  sub_models <- decomposition$sub_models
  engine_spec <- decomposition$engine_spec
  if (!is.numeric(theta)) {
    cli::cli_abort("{.arg theta} must be a numeric vector.", call = call)
  }
  theta_by_fid <- NULL
  if (!is.null(spec)) {
    assert_theta_layout(spec, sub_models, call = call)
    theta_by_fid <- flat_to_fid_theta(spec, theta, call = call)
  }

  function(candidate_seq, rate_queries = NULL) {
    candidate_models <- with_coefficient_masks(
      sub_models,
      candidate_seq,
      call = call
    )
    rates <- NULL
    if (!is.null(rate_queries)) {
      if (is.null(spec)) {
        cli::cli_abort(
          c(
            "Rate queries need the completed joint specification.",
            "i" = "Supply {.arg spec} to {.fn make_proposal_evaluator}."
          ),
          call = call
        )
      }
      rates <- query_rate_at_position(
        bind_candidate_data(spec, candidate_seq, call = call),
        theta_by_fid,
        rate_queries,
        control_prep = control_prep,
        call = call
      )
    }
    # A candidate that does not fit the engine spec's actors aborts here,
    # outside the loglik guard, so it is never read as a -Inf density.
    bind_candidate_data(engine_spec, candidate_seq, call = call)
    harvest <- tryCatch(
      {
        stats <- preprocess_pool_sequence(
          engine_spec,
          candidate_seq,
          control_prep = control_prep,
          call = call
        )
        loglik <- pool_fixed_loglik(
          sub_models,
          candidate_seq,
          stats,
          control_prep = control_prep,
          call = call
        )$loglik
        for (sm in candidate_models) {
          if (sm$fixed_only) {
            next
          }
          loglik <- loglik +
            pool_engine_eval(
              sm,
              candidate_seq,
              fid_stats(stats, sm),
              theta[sm$theta_index],
              control_prep = control_prep,
              call = call
            )$loglik
        }
        loglik
      },
      error = function(e) -Inf
    )
    list(loglik = harvest, rates = rates)
  }
}
