# The prototype-path pool evaluator behind the ABMCEM loop's evaluator contract
# (driven by estimate_dynes()). A pool is a set of endpoint-hitting sequences;
# each is preprocessed once, on pool entry, through the existing estimation
# pipeline, and its per-sub-model statistics are stored so that re-evaluation at
# any parameter vector reuses them without re-preprocessing. This file owns the
# pool structure and the entry path; the zero-iteration evaluation adapter that
# reads the stored statistics is defined alongside it.
#
# The loop is fid-indexed: theta concatenates each fid's free (non-offset)
# sub-model parameters, and every pooled sequence carries one goldfishStat per
# sub-model. A sub-model descriptor is pool-level (shared across sequences,
# since only the drawn event stream differs from one sequence to the next); the
# statistics, the data object, and the reference record are per sequence.

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
# completed specification's `completed_rates`.
new_pool_submodel <- function(
  formula,
  model = c("DyNAM", "REM", "DyNAMi"),
  sub_model = c("choice", "rate", "rate_ordered", "choice_coordination"),
  theta_index,
  pinned_rate = NULL,
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
  list(
    formula = formula,
    model = model,
    sub_model = sub_model,
    theta_index = as.integer(theta_index),
    fixed_only = fixed_only,
    pinned_rate = pinned_rate,
    engine_formula = pool_engine_formula(formula, model, sub_model)
  )
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
# takes the from-scratch preprocess path (design D3) -- present only so a
# later fast-follow that reuses a retained draw's statistics is a drop-in with
# no signature change on the pool-entry path (dynes-augmentation D23).
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

# Preprocess one pooled sequence: run every sub-model's formula through the
# existing pipeline over the sequence's data object and return the stored
# goldfishStat objects, one per sub-model, in sub-model order. This is the
# per-sequence unit the entry map seam drives; it is the only place preprocess
# runs, so evaluation (which reuses these objects) never re-preprocesses.
preprocess_pool_sequence <- function(
  sub_models,
  data,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  lapply(sub_models, function(sm) {
    estimate_wrapper(
      x = sm$engine_formula,
      model = sm$model,
      sub_model = sm$sub_model,
      data = data,
      control_prep = control_prep,
      preprocessing_only = TRUE,
      verbose = FALSE,
      progress = FALSE
    )
  })
}

# Build a sequence pool: preprocess each sequence once, on entry, through the
# injectable per-sequence map seam (serial lapply by default), and store the
# statistics, the data object, and the reference record with the sequence.
#
# * `sub_models`: a list of new_pool_submodel() descriptors, shared across
#   sequences.
# * `sequences`: a list of per-sequence inputs, each a list with `data` (a
#   stocnet the augmenter drew) and `reference` (a new_reference_record()). An
#   optional `attachment` slot is accepted and ignored in this version -- the
#   from-scratch entry path is uniform for every augmenter -- so a later
#   precomputed-statistics fast path is a drop-in with no signature churn.
# * `theta_length`: the length of the concatenated parameter vector the pool is
#   evaluated at.
# * `map_fn`: the per-sequence map seam, `function(x, f)`; the serial lapply
#   default requires no optional dependency.
build_sequence_pool <- function(
  sub_models,
  sequences,
  theta_length,
  control_prep = set_preprocessing(),
  map_fn = lapply,
  call = rlang::caller_env()
) {
  if (!is.list(sub_models) || length(sub_models) == 0) {
    cli::cli_abort(
      "{.arg sub_models} must be a non-empty list of sub-model descriptors.",
      call = call
    )
  }
  if (!is.list(sequences) || length(sequences) == 0) {
    cli::cli_abort(
      "{.arg sequences} must be a non-empty list of pooled sequences.",
      call = call
    )
  }
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
  for (sm in sub_models) {
    if (any(sm$theta_index > theta_length)) {
      cli::cli_abort(
        "A sub-model's {.field theta_index} runs past {.arg theta_length}
         ({.val {theta_length}}).",
        call = call
      )
    }
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

  # The coefficient layout depends on the formulas, not on the drawn events,
  # so one sequence's data resolves it for the whole pool.
  sub_models <- with_coefficient_masks(
    sub_models,
    sequences[[1]]$data,
    call = call
  )

  built <- map_fn(sequences, function(seq_i) {
    stats <- preprocess_pool_sequence(
      sub_models,
      seq_i$data,
      control_prep = control_prep,
      call = call
    )
    fixed <- pool_fixed_loglik(
      sub_models,
      seq_i$data,
      stats,
      control_prep = control_prep,
      call = call
    )
    list(
      data = seq_i$data,
      stats = stats,
      fixed_loglik = fixed$loglik,
      fixed_degenerate = fixed$degenerate,
      reference = seq_i$reference,
      attachment = seq_i$attachment
    )
  })

  structure(
    list(
      sub_models = sub_models,
      sequences = built,
      theta_length = theta_length,
      control_prep = control_prep,
      map_fn = map_fn
    ),
    class = c("goldfishDynesPool", "list")
  )
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
# failure marks the sequence degenerate, like a singular free sub-model; a
# pinned rate whose intervals straddle a wave boundary is a preprocessing
# contract violation and aborts instead of being absorbed.
pool_fixed_loglik <- function(
  sub_models,
  data,
  stats,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  loglik <- 0
  for (j in seq_along(sub_models)) {
    sm <- sub_models[[j]]
    if (!sm$fixed_only) {
      next
    }
    term <- tryCatch(
      fixed_only_loglik(sm, data, stats[[j]], control_prep, call = call),
      error = function(e) {
        if (inherits(e, "goldfish_pinned_rate_straddle")) {
          stop(e)
        }
        NULL
      }
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
# coefficient fixed at 1, and it is exact only when no interval straddles an
# interior wave boundary, which the preprocess must guarantee (per-wave windows
# or an event injected at the boundary). A single-plateau pin is the K = 1 case.
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
  abort_if_interval_straddles_wave(rate, start, end, call = call)

  dependent <- stats$is_dependent == 1L
  event_plateau <- rate$intercept[
    intercept_only_rate_period(rate, end[dependent], call = call)
  ]
  exposure_plateau <- rate$intercept[
    intercept_only_rate_period(rate, start, call = call)
  ]
  sum(event_plateau) - sum(exp(exposure_plateau) * exposure)
}

abort_if_interval_straddles_wave <- function(rate, start, end, call) {
  interior <- rate$wave_times[-c(1L, length(rate$wave_times))]
  if (length(interior) == 0L) {
    return(invisible(NULL))
  }
  straddles <- vapply(
    seq_along(start),
    function(k) any(start[[k]] < interior & end[[k]] > interior),
    logical(1)
  )
  if (!any(straddles)) {
    return(invisible(NULL))
  }
  k <- which(straddles)[[1L]]
  cli::cli_abort(
    c(
      "A pinned rate cannot score an interval that crosses a wave boundary.",
      "x" = "Interval {.val {k}} runs from {.val {start[[k]]}} to
             {.val {end[[k]]}}, across an interior boundary.",
      "i" = "Its exposure has no single plateau. Preprocess each wave period
             separately, or place an event at every interior boundary."
    ),
    class = "goldfish_pinned_rate_straddle",
    call = call
  )
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

  for (j in seq_along(sub_models)) {
    sm <- sub_models[[j]]
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
        sequence$stats[[j]],
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
# marked non-finite.
evaluate_sequence_pool <- function(
  pool,
  theta,
  what = c("loglik", "score", "fisher"),
  map_fn = pool$map_fn,
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

  per_sequence <- map_fn(pool$sequences, function(sequence) {
    evaluate_pool_sequence(pool, sequence, theta, call = call)
  })

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
# iteration and injects into an augmenter's init() (design D3,
# dynes-augmentation D20): one preprocess plus one likelihood pass per
# candidate, over this change's own evaluation primitive. `sub_models` is the
# same pool-level descriptor list `build_sequence_pool()` takes; the loop
# supplies it decomposed from the joint spec at each iteration, closing over
# the current `theta`.
#
# The returned closure answers by NAME, not position -- `list(loglik, rates)`
# -- so the MCMC-reuse fast-follow (dynes-augmentation D23) can widen it to
# `list(loglik, rates, stats_handle)` without breaking a caller that reads
# `$loglik`/`$rates`. No reuse logic ships here: a singular candidate reads as
# `-Inf`, mirroring the pool evaluator's own D-ROBUST guard.
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
# `spec`, and a call without `rate_queries` opens no walk.
make_proposal_evaluator <- function(
  sub_models,
  theta,
  spec = NULL,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  if (!is.list(sub_models) || length(sub_models) == 0) {
    cli::cli_abort(
      "{.arg sub_models} must be a non-empty list of sub-model descriptors.",
      call = call
    )
  }
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
    harvest <- tryCatch(
      {
        stats <- preprocess_pool_sequence(
          sub_models,
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
        for (j in seq_along(candidate_models)) {
          sm <- candidate_models[[j]]
          if (sm$fixed_only) {
            next
          }
          loglik <- loglik +
            pool_engine_eval(
              sm,
              candidate_seq,
              stats[[j]],
              theta[sm$theta_index],
              control_prep = control_prep,
              call = call
            )$loglik
        }
        loglik
      },
      error = function(e) {
        if (inherits(e, "goldfish_pinned_rate_straddle")) {
          stop(e)
        }
        -Inf
      }
    )
    list(loglik = harvest, rates = rates)
  }
}
