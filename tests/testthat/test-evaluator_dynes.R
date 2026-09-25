# The prototype-path pool evaluator behind the ABMCEM loop's evaluator contract.
# A pool is a set of endpoint-hitting sequences: each is preprocessed once, on
# entry, through the existing estimation pipeline, and its per-sub-model
# statistics are stored so that re-evaluation at any parameter vector reuses them
# without re-preprocessing. These tests cover the entry path (structure,
# preprocess-once, reference-record attachment, the serial map seam).

# A minimal single-sub-model pool over the bundled social_evolution data. Both
# sequences share the same data object, which is enough to observe the
# preprocess-once bookkeeping: preprocessing is deterministic, so identical data
# still exercises "one preprocess per pooled sequence".
make_pool_fixture <- function(n_sequences = 2L) {
  data("social_evolution", envir = environment())
  sub_models <- list(
    new_pool_submodel(
      calls ~ inertia + recip + trans,
      model = "DyNAM",
      sub_model = "choice",
      theta_index = 1:3
    )
  )
  sequences <- lapply(seq_len(n_sequences), function(i) {
    list(
      data = social_evolution,
      reference = new_reference_record(
        theta_ref = c(0, 0, 0),
        loglik_ref = -100 - i,
        log_proposal = -100 - i
      )
    )
  })
  list(sub_models = sub_models, sequences = sequences)
}

test_that("the pool preprocesses once per sequence on entry", {
  fx <- make_pool_fixture(n_sequences = 2L)

  # Count invocations of the per-sequence preprocessing seam, delegating to the
  # real implementation so the stored statistics are genuine.
  preprocess_calls <- 0L
  real_preprocess <- goldfish:::preprocess_pool_sequence
  local_mocked_bindings(
    preprocess_pool_sequence = function(...) {
      preprocess_calls <<- preprocess_calls + 1L
      real_preprocess(...)
    }
  )

  pool <- build_sequence_pool(
    fx$sub_models,
    fx$sequences,
    theta_length = 3L
  )

  # Exactly one preprocessing pass per pooled sequence, on entry.
  expect_identical(preprocess_calls, length(fx$sequences))
})

test_that("pool entry stores reusable per-sub-model statistics", {
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  expect_s3_class(pool, "goldfishDynesPool")
  expect_length(pool$sequences, 2L)
  # One stored goldfishStat per sub-model, kept with the sequence for reuse.
  stored <- pool$sequences[[1]]$stats
  expect_length(stored, length(fx$sub_models))
  expect_s3_class(stored[[1]], "goldfishStat")
  expect_identical(pool$theta_length, 3L)
})

test_that("pool entry attaches each sequence's reference record", {
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  # Reference records travel with their sequence, in pool order.
  expect_equal(pool$sequences[[1]]$reference$loglik_ref, -101)
  expect_equal(pool$sequences[[2]]$reference$loglik_ref, -102)
  # The pool exposes the stored log proposal densities in sequence order (the
  # importance-weight denominators the E-step reads).
  expect_equal(pool_references(pool) |> pool_log_proposal(), c(-101, -102))
})

test_that("per-sequence entry runs through the injectable map seam", {
  fx <- make_pool_fixture(n_sequences = 3L)

  # A custom map seam (the serial lapply default is replaceable, e.g. by a
  # parallel backend) is used for the per-sequence entry loop.
  mapped <- 0L
  counting_map <- function(x, f) {
    mapped <<- mapped + length(x)
    lapply(x, f)
  }
  pool <- build_sequence_pool(
    fx$sub_models,
    fx$sequences,
    theta_length = 3L,
    map_fn = counting_map
  )
  expect_identical(mapped, 3L)
  expect_length(pool$sequences, 3L)
})

# The zero-iteration evaluation adapter: evaluate_sequence_pool() splits the
# concatenated theta across sub-models, harvests log-likelihood / score / Fisher
# information from the existing engine at zero optimizer iterations over the
# stored statistics, and honors `what` at the harvest level. A per-sequence
# tryCatch guard keeps a singular sequence from aborting the whole pool.

# Direct zero-iteration engine evaluation of one sub-model, the reference the
# adapter must reproduce.
direct_zero_iteration <- function(data, formula, theta, sub_model = "choice") {
  estimate_wrapper(
    x = formula,
    model = "DyNAM",
    sub_model = sub_model,
    data = data,
    control_algo = set_algorithm_newton(
      initial_parameters = theta,
      max_iterations = 0L,
      backend = "r"
    ),
    verbose = FALSE,
    progress = FALSE
  )
}

test_that("the adapter matches direct zero-iteration engine evaluation", {
  fx <- make_pool_fixture(n_sequences = 1L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)
  theta <- c(1.0, 0.5, -0.2)

  res <- evaluate_sequence_pool(
    pool,
    theta,
    what = c("loglik", "score", "fisher")
  )
  direct <- direct_zero_iteration(
    fx$sequences[[1]]$data,
    calls ~ inertia + recip + trans,
    theta
  )

  expect_equal(res$loglik[[1]], direct$log_likelihood, tolerance = 1e-10)
  expect_equal(res$score[1, ], direct$final_score, tolerance = 1e-10)
  expect_equal(
    res$fisher[[1]],
    direct$final_information_matrix,
    tolerance = 1e-10
  )
  expect_false(res$degenerate[[1]])
})

test_that("evaluation reuses stored statistics without re-preprocessing", {
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  # Once the pool is built, no evaluation triggers preprocessing again.
  preprocess_calls <- 0L
  local_mocked_bindings(
    preprocess_pool_sequence = function(...) {
      preprocess_calls <<- preprocess_calls + 1L
      list()
    }
  )
  evaluate_sequence_pool(pool, c(0, 0, 0), what = "loglik")
  evaluate_sequence_pool(pool, c(0.3, -0.1, 0.2), what = "loglik")
  expect_identical(preprocess_calls, 0L)
})

test_that("what is honored at the harvest level", {
  fx <- make_pool_fixture(n_sequences = 1L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  loglik_only <- evaluate_sequence_pool(pool, c(0, 0, 0), what = "loglik")
  expect_named(loglik_only, c("loglik", "degenerate"), ignore.order = TRUE)
  expect_null(loglik_only$score)
  expect_null(loglik_only$fisher)

  score_only <- evaluate_sequence_pool(pool, c(0, 0, 0), what = "score")
  expect_named(score_only, c("score", "degenerate"), ignore.order = TRUE)
  expect_null(score_only$loglik)
})

test_that("the adapter concatenates sub-model scores by theta_index", {
  data("social_evolution", envir = environment())
  sub_models <- list(
    new_pool_submodel(
      calls ~ inertia + recip,
      model = "DyNAM",
      sub_model = "choice",
      theta_index = 1:2
    ),
    new_pool_submodel(
      calls ~ trans,
      model = "DyNAM",
      sub_model = "choice",
      theta_index = 3L
    )
  )
  sequences <- list(list(
    data = social_evolution,
    reference = new_reference_record(c(0, 0, 0), -100, -100)
  ))
  pool <- build_sequence_pool(sub_models, sequences, theta_length = 3L)
  theta <- c(0.4, -0.2, 0.1)

  res <- evaluate_sequence_pool(
    pool,
    theta,
    what = c("loglik", "score", "fisher")
  )
  d1 <- direct_zero_iteration(
    social_evolution,
    calls ~ inertia + recip,
    theta[1:2]
  )
  d2 <- direct_zero_iteration(social_evolution, calls ~ trans, theta[3])

  # Per-sequence loglik is the sum across sub-models; the score is placed into
  # each sub-model's theta_index positions; the Fisher information is block
  # diagonal across sub-models (no cross terms in this version).
  expect_equal(
    res$loglik[[1]],
    d1$log_likelihood + d2$log_likelihood,
    tolerance = 1e-10
  )
  expect_equal(res$score[1, 1:2], d1$final_score, tolerance = 1e-10)
  expect_equal(res$score[1, 3], d2$final_score, tolerance = 1e-10)
  expect_equal(
    res$fisher[[1]][1:2, 1:2],
    d1$final_information_matrix,
    tolerance = 1e-10
  )
  expect_equal(
    res$fisher[[1]][3, 3],
    as.numeric(d2$final_information_matrix),
    tolerance = 1e-10
  )
  expect_true(all(res$fisher[[1]][1:2, 3] == 0))
})

test_that("a singular sequence yields a non-finite result, not a pool abort", {
  # A parameter vector large enough to saturate the choice probabilities makes
  # the engine's unconditional information-matrix inversion singular; the guard
  # must catch the engine stop() and mark the sequence non-finite rather than
  # aborting the whole pool.
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  res <- expect_no_error(
    evaluate_sequence_pool(pool, c(50, 50, 50), what = c("loglik", "score"))
  )
  expect_true(all(res$degenerate))
  expect_false(any(is.finite(res$loglik)))
})

# The v1 attachment-optional conformance pin (design D3, dynes-augmentation
# D23): the pool-entry path always takes the from-scratch preprocess, whether
# or not a precomputed attachment is supplied -- a supplied one is stored but
# never consulted, so the pin is that its presence changes nothing observable.
# make_proposal_evaluator() is the other half of the same seam: its return is
# a NAMED list, pinned by name rather than position, so a later widen from
# `(loglik, rates)` to add a stats handle cannot break a caller that reads it
# by name.

test_that("pool entry succeeds with no attachment and takes the from-scratch path", {
  fx <- make_pool_fixture(n_sequences = 1L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  expect_null(pool$sequences[[1]]$attachment)
  expect_s3_class(pool$sequences[[1]]$stats[[1]], "goldfishStat")
})

test_that("a supplied attachment is ignored -- byte-identical to no attachment", {
  fx <- make_pool_fixture(n_sequences = 2L)
  baseline <- build_sequence_pool(
    fx$sub_models,
    fx$sequences,
    theta_length = 3L
  )

  # Deliberately bogus attachment content: if the pool-entry path ever
  # consulted it instead of preprocessing from scratch, the stored statistics
  # would differ from the baseline built with no attachment at all.
  attached_sequences <- Map(
    function(seq_i, i) {
      seq_i$attachment <- new_pool_attachment(
        stats = list("bogus"),
        loglik_ref = -999 - i
      )
      seq_i
    },
    fx$sequences,
    seq_along(fx$sequences)
  )
  attached <- build_sequence_pool(
    fx$sub_models,
    attached_sequences,
    theta_length = 3L
  )

  expect_identical(
    lapply(attached$sequences, `[[`, "stats"),
    lapply(baseline$sequences, `[[`, "stats")
  )
  theta <- c(0.4, -0.2, 0.1)
  expect_identical(
    evaluate_sequence_pool(attached, theta, what = "loglik"),
    evaluate_sequence_pool(baseline, theta, what = "loglik")
  )
})

test_that("new_pool_attachment validates its stats and loglik_ref", {
  expect_snapshot(
    new_pool_attachment(stats = "not-a-list", loglik_ref = -1),
    error = TRUE
  )
  expect_snapshot(
    new_pool_attachment(stats = list(), loglik_ref = NA_real_),
    error = TRUE
  )
})

test_that("make_proposal_evaluator returns a named-list contract", {
  fx <- make_pool_fixture(n_sequences = 1L)
  theta <- c(1.0, 0.5, -0.2)
  eval_fn <- make_proposal_evaluator(fx$sub_models, theta)
  out <- eval_fn(fx$sequences[[1]]$data)

  # Named, not positional: a widen from (loglik, rates) to also carry a
  # stats handle must not shift what `out$loglik` reads.
  expect_named(out, c("loglik", "rates"))
  expect_null(out$rates)

  direct <- direct_zero_iteration(
    fx$sequences[[1]]$data,
    calls ~ inertia + recip + trans,
    theta
  )
  expect_equal(out$loglik, direct$log_likelihood, tolerance = 1e-10)
})

# Rate queries: given the completed joint specification, the closure binds each
# candidate to it and reads the named pairs' intensities off a walk replayed
# over the candidate, at the flat theta mapped onto the specification's fids.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

rq_evaluator_theta <- function() c(-1, 0.4, 0.5, 0.8, -2, 0.6)

rq_evaluator_queries <- function() {
  list(
    list(after = 2L, layer = "calls", sender = "N3", receiver = "N4"),
    list(after = 0L, layer = "emails", sender = "N2", receiver = "N1"),
    list(after = 4L, layer = "calls", sender = "N5", receiver = "N1")
  )
}

test_that("make_proposal_evaluator answers rate_queries at the mapped theta", {
  spec <- rq_layout_spec()
  theta <- rq_evaluator_theta()
  candidate <- rq_candidate_data()
  queries <- rq_evaluator_queries()
  eval_fn <- make_proposal_evaluator(rq_layout_submodels(), theta, spec = spec)

  out <- suppressWarnings(
    expect_no_error(eval_fn(candidate, rate_queries = queries))
  )

  expect_named(out, c("loglik", "rates"))
  expect_length(out$rates, length(queries))
  expected <- suppressWarnings(
    query_rate_at_position(
      bind_candidate_data(spec, candidate),
      flat_to_fid_theta(spec, theta),
      queries
    )
  )
  expect_identical(out$rates, expected)
  expect_true(all(out$rates > 0))
})

test_that("rate_queries without a specification aborts naming spec", {
  local_cli_context()
  fx <- make_pool_fixture(n_sequences = 1L)
  eval_fn <- make_proposal_evaluator(fx$sub_models, c(0, 0, 0))
  queries <- list(
    list(after = 1L, layer = "calls", sender = "1", receiver = "2")
  )

  expect_error(
    eval_fn(fx$sequences[[1]]$data, rate_queries = queries),
    "spec"
  )
  expect_snapshot(
    eval_fn(fx$sequences[[1]]$data, rate_queries = queries),
    error = TRUE
  )
})

test_that("a sub-model layout off the specification aborts at construction", {
  expect_error(
    make_proposal_evaluator(
      rq_layout_submodels(calls_choice = c(3L, 7L)),
      rq_evaluator_theta(),
      spec = rq_layout_spec()
    ),
    class = "goldfish_theta_layout_mismatch"
  )
})

test_that("an absent rate_queries returns NULL rates and opens no walk", {
  opened <- 0L
  real_open <- walk_open
  local_mocked_bindings(
    walk_open = function(...) {
      opened <<- opened + 1L
      real_open(...)
    }
  )
  eval_fn <- make_proposal_evaluator(
    rq_layout_submodels(),
    rq_evaluator_theta(),
    spec = rq_layout_spec()
  )
  candidate <- rq_candidate_data()

  omitted <- suppressWarnings(eval_fn(candidate))
  explicit <- suppressWarnings(eval_fn(candidate, rate_queries = NULL))

  expect_null(omitted$rates)
  expect_null(explicit$rates)
  expect_identical(omitted, explicit)
  expect_identical(opened, 0L)
})

test_that("a degenerate sequence does not stop the rest of the pool", {
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$sub_models, fx$sequences, theta_length = 3L)

  # Make only the second sequence's engine call singular; the first must still
  # return a finite result and the pool must complete.
  seam_calls <- 0L
  real_engine <- goldfish:::pool_engine_eval
  local_mocked_bindings(
    pool_engine_eval = function(...) {
      seam_calls <<- seam_calls + 1L
      if (seam_calls == 2L) {
        stop(
          "For DyNAM choice estimation:\n\tMatrix cannot be inverted; ",
          "probably due to collinearity between parameters."
        )
      }
      real_engine(...)
    }
  )

  res <- expect_no_error(
    evaluate_sequence_pool(pool, c(0.2, 0.1, -0.1), what = c("loglik", "score"))
  )
  expect_false(res$degenerate[[1]])
  expect_true(res$degenerate[[2]])
  expect_true(is.finite(res$loglik[[1]]))
  expect_false(is.finite(res$loglik[[2]]))
})

# Fixed-only sub-models: a fid with no free slot -- a completed pinned rate, a
# completed uniform choice, or an authored fid whose effects are all offset()
# -- still has a log-likelihood. It is θ-free, so the pool adds it to each
# sequence's total and gives it no score or Fisher block.
#
# Four actors, all present, one directed `calls` layer. The default path puts
# events at 1, 3, 6 (first wave period [0, 10)) and at 10, 12, 15 (second
# period [10, 20]); the event exactly at t = 10 closes the interval (6, 10], so
# no interval straddles the interior wave boundary.
fixed_only_data <- function(calls_time = c(1, 3, 6, 10, 12, 15)) {
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(calls = "increment"),
      directed = c(calls = TRUE),
      observation = c(calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = data.frame(
      from = c(1L, 2L, 1L, 3L, 4L, 1L),
      to = c(2L, 1L, 2L, 4L, 3L, 2L),
      time = calls_time,
      layer = "calls"
    )
  )
}

fixed_only_free_submodel <- function() {
  new_pool_submodel(
    calls ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    theta_index = 1:2
  )
}

# A two-plateau pin, the object `spec$completed_rates` holds for a completed
# timed rate: log-hazard -1 on [0, 10), -2 on [10, 20].
fixed_only_pinned_rate <- function() {
  make_intercept_only_rate(
    c(-1, -2),
    model_type = "DyNAM-M-Rate",
    wave_times = c(0, 10, 20)
  )
}

fixed_only_pinned_submodel <- function() {
  new_pool_submodel(
    calls ~ 1,
    model = "DyNAM",
    sub_model = "rate",
    theta_index = integer(0),
    pinned_rate = fixed_only_pinned_rate()
  )
}

fixed_only_pool <- function(sub_models, data = fixed_only_data()) {
  build_sequence_pool(
    sub_models,
    list(list(
      data = data,
      reference = new_reference_record(c(0, 0), -10, -10)
    )),
    theta_length = 2L
  )
}

test_that("new_pool_submodel accepts a zero-length theta_index", {
  sm <- expect_no_error(fixed_only_pinned_submodel())
  expect_identical(sm$theta_index, integer(0))

  authored <- expect_no_error(
    new_pool_submodel(
      calls ~ offset(inertia, coef = 0.5),
      model = "DyNAM",
      sub_model = "choice",
      theta_index = integer(0)
    )
  )
  expect_identical(authored$theta_index, integer(0))
})

test_that("a pinned rate is only accepted on a fixed-only rate sub-model", {
  expect_error(
    new_pool_submodel(
      calls ~ inertia,
      model = "DyNAM",
      sub_model = "choice",
      theta_index = integer(0),
      pinned_rate = fixed_only_pinned_rate()
    ),
    "only applies to a fixed-only rate"
  )
  expect_error(
    new_pool_submodel(
      calls ~ 1 + outdeg,
      model = "DyNAM",
      sub_model = "rate",
      theta_index = 1:2,
      pinned_rate = fixed_only_pinned_rate()
    ),
    "only applies to a fixed-only rate"
  )
})

test_that("a fixed-only fid adds a log-likelihood, no score or Fisher block", {
  theta <- c(0.3, -0.2)
  free_only <- fixed_only_pool(list(fixed_only_free_submodel()))
  with_pinned <- fixed_only_pool(
    list(fixed_only_free_submodel(), fixed_only_pinned_submodel())
  )

  base <- evaluate_sequence_pool(free_only, theta)
  res <- evaluate_sequence_pool(with_pinned, theta)

  # Dimensions stay at the free-slot count: the pinned fid claims no slot.
  expect_identical(dim(res$score), c(1L, 2L))
  expect_identical(dim(res$fisher[[1]]), c(2L, 2L))
  expect_equal(res$score, base$score, tolerance = 1e-12)
  expect_equal(res$fisher, base$fisher, tolerance = 1e-12)
  expect_false(res$degenerate[[1]])

  # Hand-computed pinned-rate term, Σ c_w(e) − Σ_w e^{c_w} ∫_w |R(t)| dt, with
  # the exposure integrated over the engine's window (first event onward) and
  # |R(t)| = 4 throughout. Period 1: events at 1, 3, 6, exposure 4 * (10 − 1).
  # Period 2: events at 10 (an interior boundary belongs to the next period),
  # 12, 15, exposure 4 * (15 − 10).
  c1 <- -1
  c2 <- -2
  pinned <- 3 * c1 + 3 * c2 - exp(c1) * 4 * 9 - exp(c2) * 4 * 5
  expect_equal(res$loglik[[1]], base$loglik[[1]] + pinned, tolerance = 1e-10)
})

test_that("the fixed-only term is path-dependent but cancels in the reweight", {
  theta <- c(0.3, -0.2)
  theta_ref <- c(0, 0)
  both <- list(fixed_only_free_submodel(), fixed_only_pinned_submodel())

  # Path B moves the event at t = 12 to t = 8, across the wave boundary: the
  # same exposure per period, but one event's log-hazard changes from c2 to c1.
  path_a <- fixed_only_data()
  path_b <- fixed_only_data(calls_time = c(1, 3, 6, 10, 8, 15))
  path_b$ties <- path_b$ties[order(path_b$ties$time), ]

  fixed_term <- function(data) {
    with_pinned <- evaluate_sequence_pool(fixed_only_pool(both, data), theta)
    free_only <- evaluate_sequence_pool(
      fixed_only_pool(list(fixed_only_free_submodel()), data),
      theta
    )
    with_pinned$loglik[[1]] - free_only$loglik[[1]]
  }
  # c1 − c2 = (−1) − (−2) = 1.
  expect_equal(fixed_term(path_b) - fixed_term(path_a), 1, tolerance = 1e-10)

  # On one path, the reweight log f(Ω;θ) − log f(Ω;θ_ref) is unchanged by the
  # θ-free term.
  log_ratio <- function(sub_models) {
    pool <- fixed_only_pool(sub_models, path_a)
    evaluate_sequence_pool(pool, theta, what = "loglik")$loglik -
      evaluate_sequence_pool(pool, theta_ref, what = "loglik")$loglik
  }
  expect_equal(
    log_ratio(both),
    log_ratio(list(fixed_only_free_submodel())),
    tolerance = 1e-10
  )
})

test_that("an authored all-offset fid is scored the same way", {
  theta <- c(0.3, -0.2)
  authored <- new_pool_submodel(
    calls ~ offset(inertia, coef = 0.5),
    model = "DyNAM",
    sub_model = "choice",
    theta_index = integer(0)
  )
  base <- evaluate_sequence_pool(
    fixed_only_pool(list(fixed_only_free_submodel())),
    theta
  )
  res <- evaluate_sequence_pool(
    fixed_only_pool(list(fixed_only_free_submodel(), authored)),
    theta
  )

  direct <- estimate_wrapper(
    x = calls ~ offset(inertia, coef = 0.5),
    model = "DyNAM",
    sub_model = "choice",
    data = fixed_only_data(),
    control_algo = set_algorithm_newton(max_iterations = 0L, backend = "r"),
    verbose = FALSE,
    progress = FALSE
  )
  expect_equal(
    res$loglik[[1]],
    base$loglik[[1]] + direct$log_likelihood,
    tolerance = 1e-10
  )
  expect_equal(res$score, base$score, tolerance = 1e-12)
  expect_equal(res$fisher, base$fisher, tolerance = 1e-12)
})

# A completed uniform choice has no statistic columns. The engine refuses an
# effect-free formula outright, so the pool scores it through a single column
# whose coefficient is fixed at 0: every alternative then carries the same
# linear predictor, each event contributes −log |support(t)|, and an all-fixed
# evaluation is likelihood-only (no information-matrix inversion), so the
# column's values never matter.
test_that("the engine refuses an effect-free choice formula", {
  expect_error(
    suppressWarnings(
      estimate_wrapper(
        x = calls ~ 1,
        model = "DyNAM",
        sub_model = "choice",
        data = fixed_only_data(),
        control_algo = set_algorithm_newton(max_iterations = 0L, backend = "r"),
        verbose = FALSE,
        progress = FALSE
      )
    ),
    "at least one effect term"
  )
})

test_that("a zero-coefficient column scores -log |support(t)| per event", {
  direct <- estimate_wrapper(
    x = calls ~ offset(inertia(calls), coef = 0),
    model = "DyNAM",
    sub_model = "choice",
    data = fixed_only_data(),
    control_algo = set_algorithm_newton(
      max_iterations = 0L,
      backend = "r",
      diagnostics = "loglik"
    ),
    verbose = FALSE,
    progress = FALSE
  )
  # Four actors, no self-ties: three legal receivers at every event.
  expect_equal(direct$interval_log_lik, rep(-log(3), 6), tolerance = 1e-12)
})

test_that("a completed uniform choice enters the pool as a fixed-only fid", {
  theta <- c(0.3, -0.2)
  uniform <- new_pool_submodel(
    calls ~ 1,
    model = "DyNAM",
    sub_model = "choice",
    theta_index = integer(0)
  )
  base <- evaluate_sequence_pool(
    fixed_only_pool(list(fixed_only_free_submodel())),
    theta
  )
  res <- expect_no_error(
    evaluate_sequence_pool(
      fixed_only_pool(list(fixed_only_free_submodel(), uniform)),
      theta
    )
  )
  expect_equal(
    res$loglik[[1]],
    base$loglik[[1]] - 6 * log(3),
    tolerance = 1e-10
  )
  expect_equal(res$score, base$score, tolerance = 1e-12)
})

test_that("a pinned rate refuses an interval that straddles a wave boundary", {
  # Moving the boundary event from t = 10 to t = 11 leaves the interval
  # (6, 11] across the interior boundary at 10: its exposure has no single
  # plateau, so the per-period column cannot score it exactly.
  straddling <- fixed_only_data(calls_time = c(1, 3, 6, 11, 12, 15))
  expect_error(
    fixed_only_pool(
      list(fixed_only_free_submodel(), fixed_only_pinned_submodel()),
      straddling
    ),
    class = "goldfish_pinned_rate_straddle"
  )
})

# A free sub-model may also carry offset() terms: theta covers only its free
# coefficients, while the engine lays out every coefficient, offsets included.
# The adapter must place theta at the free positions and harvest the score and
# Fisher information at those positions only.
test_that("a sub-model mixing free and offset terms evaluates at its free slots", {
  data("social_evolution", envir = environment())
  formula <- calls ~ inertia + offset(recip, coef = 0.5) + trans
  sub_models <- list(
    new_pool_submodel(formula, "DyNAM", "choice", theta_index = 1:2)
  )
  pool <- build_sequence_pool(
    sub_models,
    list(list(
      data = social_evolution,
      reference = new_reference_record(c(0, 0), -100, -100)
    )),
    theta_length = 2L
  )
  theta <- c(0.4, -0.1)

  res <- evaluate_sequence_pool(pool, theta)
  # The offset's value is fixed by the formula; the full vector's middle entry
  # is overwritten by it.
  direct <- direct_zero_iteration(social_evolution, formula, c(0.4, 0, -0.1))

  expect_false(res$degenerate[[1]])
  expect_equal(res$loglik[[1]], direct$log_likelihood, tolerance = 1e-10)
  expect_equal(res$score[1, ], direct$final_score[c(1, 3)], tolerance = 1e-10)
  expect_equal(
    res$fisher[[1]],
    direct$final_information_matrix[c(1, 3), c(1, 3)],
    tolerance = 1e-10
  )
})

test_that("a rate sub-model's free slots include its time intercept", {
  data("social_evolution", envir = environment())
  formula <- calls ~ 1 + offset(indeg, coef = 0.2) + outdeg
  sub_models <- list(
    new_pool_submodel(formula, "DyNAM", "rate", theta_index = 1:2)
  )
  pool <- build_sequence_pool(
    sub_models,
    list(list(
      data = social_evolution,
      reference = new_reference_record(c(0, 0), -100, -100)
    )),
    theta_length = 2L
  )
  theta <- c(-3, 0.05)

  res <- evaluate_sequence_pool(pool, theta)
  direct <- direct_zero_iteration(
    social_evolution,
    formula,
    c(-3, 0, 0.05),
    sub_model = "rate"
  )

  expect_false(res$degenerate[[1]])
  expect_equal(res$loglik[[1]], direct$log_likelihood, tolerance = 1e-10)
  expect_equal(res$score[1, ], direct$final_score[c(1, 3)], tolerance = 1e-10)
})

test_that("the proposal evaluator scores a sub-model with offset terms", {
  data("social_evolution", envir = environment())
  formula <- calls ~ inertia + offset(recip, coef = 0.5) + trans
  sub_models <- list(
    new_pool_submodel(formula, "DyNAM", "choice", theta_index = 1:2)
  )
  eval_fn <- make_proposal_evaluator(sub_models, c(0.4, -0.1))
  out <- eval_fn(social_evolution)

  direct <- direct_zero_iteration(social_evolution, formula, c(0.4, 0, -0.1))
  expect_equal(out$loglik, direct$log_likelihood, tolerance = 1e-10)
})

test_that("a theta_index not matching the free coefficients aborts at entry", {
  data("social_evolution", envir = environment())
  sub_models <- list(
    new_pool_submodel(
      calls ~ inertia + offset(recip, coef = 0.5) + trans,
      "DyNAM",
      "choice",
      theta_index = 1:3
    )
  )
  expect_error(
    build_sequence_pool(
      sub_models,
      list(list(
        data = social_evolution,
        reference = new_reference_record(c(0, 0, 0), -100, -100)
      )),
      theta_length = 3L
    ),
    class = "goldfish_theta_index_mismatch"
  )
})
