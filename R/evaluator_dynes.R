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
new_pool_submodel <- function(
  formula,
  model = c("DyNAM", "REM", "DyNAMi"),
  sub_model = c("choice", "rate", "rate_ordered", "choice_coordination"),
  theta_index,
  call = rlang::caller_env()
) {
  if (!rlang::is_formula(formula)) {
    cli::cli_abort("{.arg formula} must be a formula.", call = call)
  }
  model <- rlang::arg_match(model)
  sub_model <- rlang::arg_match(sub_model)
  if (
    !is.numeric(theta_index) ||
      length(theta_index) == 0 ||
      anyNA(theta_index) ||
      any(theta_index < 1) ||
      any(theta_index != as.integer(theta_index))
  ) {
    cli::cli_abort(
      "{.arg theta_index} must be a vector of positive integer positions.",
      call = call
    )
  }
  list(
    formula = formula,
    model = model,
    sub_model = sub_model,
    theta_index = as.integer(theta_index)
  )
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
      x = sm$formula,
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

  built <- map_fn(sequences, function(seq_i) {
    stats <- preprocess_pool_sequence(
      sub_models,
      seq_i$data,
      control_prep = control_prep,
      call = call
    )
    list(
      data = seq_i$data,
      stats = stats,
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
# `preprocessed`, so no re-preprocessing happens. Fixed effects are the
# formula's offsets, already baked into the statistics, so `theta_block` covers
# only the free effects.
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
  fit <- estimate_wrapper(
    x = sub_model$formula,
    model = sub_model$model,
    sub_model = sub_model$sub_model,
    data = data,
    preprocessed = stats,
    control_algo = set_algorithm_newton(
      initial_parameters = theta_block,
      max_iterations = 0L,
      backend = "r"
    ),
    control_prep = control_prep,
    verbose = FALSE,
    progress = FALSE
  )
  list(
    loglik = fit$log_likelihood,
    score = fit$final_score,
    fisher = fit$final_information_matrix
  )
}

# Evaluate one pooled sequence at the concatenated `theta`: split theta across
# the sub-models, evaluate each over its stored statistics, and reassemble the
# per-sequence quantities on the full free-parameter dimension `p` -- the total
# log-likelihood (summed across sub-models), the concatenated score (each
# sub-model's score placed at its theta_index), and the block-diagonal Fisher
# information (no cross-sub-model terms in this version). A sub-model whose
# information matrix is singular at theta makes the whole sequence degenerate:
# its result is non-finite (-Inf log-likelihood, so its importance weight
# vanishes), which the caller reports without aborting the pool.
evaluate_pool_sequence <- function(pool, sequence, theta, call) {
  sub_models <- pool$sub_models
  p <- pool$theta_length
  loglik <- 0
  score <- numeric(p)
  fisher <- matrix(0, p, p)
  degenerate <- FALSE

  for (j in seq_along(sub_models)) {
    sm <- sub_models[[j]]
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
# `-Inf`, mirroring the pool evaluator's own D-ROBUST guard, and a real
# `rate_queries` request aborts naming what is missing rather than returning a
# fabricated value under that name -- the named-pair rate-at-position query
# needs a `what` extension this change does not ship (compute_lik_seq() sugar
# ships with the batched evaluator, not here).
make_proposal_evaluator <- function(
  sub_models,
  theta,
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

  function(candidate_seq, rate_queries = NULL) {
    if (!is.null(rate_queries)) {
      cli::cli_abort(
        c(
          "Named-pair rate-at-position queries are not yet implemented.",
          "i" = "Only {.field loglik} is available from this version's
                 proposal evaluator."
        ),
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
        loglik <- 0
        for (j in seq_along(sub_models)) {
          sm <- sub_models[[j]]
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
      error = function(e) -Inf
    )
    list(loglik = harvest, rates = NULL)
  }
}
