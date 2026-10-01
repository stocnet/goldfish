# The prototype-path pool evaluator behind the ABMCEM loop's evaluator contract.
# A pool is a set of endpoint-hitting sequences: each is preprocessed once, on
# entry, by one joint walk, and its per-fid statistics are stored so that
# re-evaluation at any parameter vector reuses them without re-preprocessing.
# These tests cover the entry path (structure, preprocess-once,
# reference-record attachment, the serial map seam).

# A minimal single-sub-model pool over the bundled social_evolution data. Both
# sequences share the same data object, which is enough to observe the
# preprocess-once bookkeeping: preprocessing is deterministic, so identical data
# still exercises "one preprocess per pooled sequence".
make_pool_fixture <- function(n_sequences = 2L) {
  data("social_evolution", envir = environment())
  decomposition <- hand_decomposition(
    social_evolution,
    choice = ~ inertia + recip + trans,
    theta_index = list(choice = 1:3)
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
  list(decomposition = decomposition, sequences = sequences)
}

test_that("the pool runs one joint walk per sequence on entry", {
  # Four fids, so a per-sub-model preprocess would show as four passes.
  spec <- dynes_multiwave_spec()
  decomposition <- decompose_joint_spec(spec)
  sequences <- lapply(1:2, function(i) {
    list(data = spec$data, reference = new_reference_record(c(0, 0, 0), -1, -1))
  })

  # Count the walks, delegating to the real one so the stored statistics are
  # genuine, and any preprocess that bypasses the walk.
  walks <- 0L
  bypasses <- 0L
  real_walk <- preprocess_joint
  real_wrapper <- estimate_wrapper
  local_mocked_bindings(
    preprocess_joint = function(...) {
      walks <<- walks + 1L
      real_walk(...)
    },
    estimate_wrapper = function(..., preprocessed = NULL) {
      if (is.null(preprocessed)) {
        bypasses <<- bypasses + 1L
      }
      real_wrapper(..., preprocessed = preprocessed)
    }
  )

  build_sequence_pool(decomposition, sequences, theta_length = 3L)

  expect_identical(walks, length(sequences))
  expect_identical(bypasses, 0L)
})

test_that("pool entry stores reusable per-sub-model statistics", {
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)

  expect_s3_class(pool, "goldfishDynesPool")
  expect_length(pool$sequences, 2L)
  # One stored goldfishStat per fid, kept with the sequence for reuse.
  stored <- pool$sequences[[1]]$stats
  expect_named(stored, "1")
  expect_s3_class(stored[[1]], "goldfishStat")
  expect_identical(pool$theta_length, 3L)
})

test_that("pool entry attaches each sequence's reference record", {
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)

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
    fx$decomposition,
    fx$sequences,
    theta_length = 3L,
    map_fn = counting_map
  )
  expect_identical(mapped, 3L)
  expect_length(pool$sequences, 3L)
})

# Pool evaluation: evaluate_sequence_pool() splits the concatenated theta
# across sub-models, harvests log-likelihood / score / Fisher information from
# one compiled pass per sub-model over the stored statistics, and honors `what`
# at the harvest level. A per-sequence guard keeps a singular sequence from
# aborting the whole pool.

# Direct zero-iteration engine evaluation of one sub-model, the reference the
# pool evaluator must reproduce.
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

test_that("pool evaluation matches direct zero-iteration engine evaluation", {
  fx <- make_pool_fixture(n_sequences = 1L)
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)
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
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)

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
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)

  loglik_only <- evaluate_sequence_pool(pool, c(0, 0, 0), what = "loglik")
  expect_named(loglik_only, c("loglik", "degenerate"), ignore.order = TRUE)
  expect_null(loglik_only$score)
  expect_null(loglik_only$fisher)

  score_only <- evaluate_sequence_pool(pool, c(0, 0, 0), what = "score")
  expect_named(score_only, c("score", "degenerate"), ignore.order = TRUE)
  expect_null(score_only$loglik)
})

test_that("pool evaluation concatenates sub-model scores by theta_index", {
  data("social_evolution", envir = environment())
  # The choice takes slots 1:2 and the rate 3:4, so the concatenation is not
  # simply fid order.
  decomposition <- hand_decomposition(
    social_evolution,
    rate = ~ 1 + outdeg,
    choice = ~ inertia + recip,
    theta_index = list(rate = 3:4, choice = 1:2)
  )
  sequences <- list(list(
    data = social_evolution,
    reference = new_reference_record(c(0, 0, 0, 0), -100, -100)
  ))
  pool <- build_sequence_pool(decomposition, sequences, theta_length = 4L)
  theta <- c(0.4, -0.2, -4, 0.1)

  res <- evaluate_sequence_pool(
    pool,
    theta,
    what = c("loglik", "score", "fisher")
  )
  # One process on one layer: its own walk is the joint walk.
  d1 <- direct_zero_iteration(
    social_evolution,
    calls ~ inertia + recip,
    theta[1:2]
  )
  d2 <- direct_zero_iteration(
    social_evolution,
    calls ~ 1 + outdeg,
    theta[3:4],
    sub_model = "rate"
  )

  # Per-sequence loglik is the sum across sub-models; the score is placed into
  # each sub-model's theta_index positions; the Fisher information is block
  # diagonal across sub-models (no cross terms in this version).
  expect_equal(
    res$loglik[[1]],
    d1$log_likelihood + d2$log_likelihood,
    tolerance = 1e-10
  )
  expect_equal(res$score[1, 1:2], d1$final_score, tolerance = 1e-10)
  expect_equal(res$score[1, 3:4], d2$final_score, tolerance = 1e-10)
  expect_equal(
    res$fisher[[1]][1:2, 1:2],
    d1$final_information_matrix,
    tolerance = 1e-10
  )
  expect_equal(
    res$fisher[[1]][3:4, 3:4],
    d2$final_information_matrix,
    tolerance = 1e-10
  )
  expect_true(all(res$fisher[[1]][1:2, 3:4] == 0))
})

test_that("a singular sequence yields a non-finite result, not a pool abort", {
  # A parameter vector large enough to saturate the choice probabilities makes
  # the engine's unconditional information-matrix inversion singular; the guard
  # must catch the engine stop() and mark the sequence non-finite rather than
  # aborting the whole pool.
  fx <- make_pool_fixture(n_sequences = 2L)
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)

  res <- expect_no_error(
    evaluate_sequence_pool(pool, c(50, 50, 50), what = c("loglik", "score"))
  )
  expect_true(all(res$degenerate))
  expect_false(any(is.finite(res$loglik)))
})

# The attachment-optional conformance pin: the pool-entry path takes the
# from-scratch preprocess unless a precomputed attachment is supplied AND is
# consistent with the pool (same reference theta, same layout tag); an
# inconsistent one is never consulted, so its presence changes nothing
# observable. Adoption of a consistent one is pinned in test-pool_entry_dynes.R.
# make_proposal_evaluator() is the other half of the same seam: its return is
# a NAMED list, pinned by name rather than position, so a later widen from
# `(loglik, rates)` to add a stats handle cannot break a caller that reads it
# by name.

test_that("pool entry succeeds with no attachment and takes the from-scratch path", {
  fx <- make_pool_fixture(n_sequences = 1L)
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)

  expect_null(pool$sequences[[1]]$attachment)
  expect_s3_class(pool$sequences[[1]]$stats[["1"]], "goldfishStat")
})

test_that("an inconsistent attachment is ignored -- identical to none", {
  fx <- make_pool_fixture(n_sequences = 2L)
  baseline <- build_sequence_pool(
    fx$decomposition,
    fx$sequences,
    theta_length = 3L
  )

  # Deliberately bogus attachment content under a layout tag no pool has: if
  # the pool-entry path ever consulted it instead of preprocessing from
  # scratch, the stored statistics would differ from the baseline built with
  # no attachment at all.
  attached_sequences <- Map(
    function(seq_i, i) {
      seq_i$attachment <- new_pool_attachment(
        stats = list("bogus"),
        loglik_ref = -999 - i,
        theta_ref = seq_i$reference$theta_ref,
        layout = "bogus"
      )
      seq_i
    },
    fx$sequences,
    seq_along(fx$sequences)
  )
  attached <- build_sequence_pool(
    fx$decomposition,
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
  eval_fn <- make_proposal_evaluator(fx$decomposition, theta)
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
  eval_fn <- make_proposal_evaluator(
    rq_layout_decomposition(),
    theta,
    spec = spec
  )

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
  eval_fn <- make_proposal_evaluator(fx$decomposition, c(0, 0, 0))
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
      rq_layout_decomposition(calls_choice = c(3L, 7L)),
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
    rq_layout_decomposition(),
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
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)

  # Make only the second sequence's engine call singular; the first must still
  # return a finite result and the pool must complete.
  seam_calls <- 0L
  real_engine <- pool_cpp_eval
  local_mocked_bindings(
    pool_cpp_eval = function(...) {
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

# A free choice alone (slots 1:2), and the same choice beside a fixed-only
# rate; or a free rate alone (slots 1:2), and the same rate beside a
# fixed-only choice. A fixed-only fid adds no censoring row, so both pools of a
# pair walk to the same free statistics.
fixed_only_free_choice <- function(data = fixed_only_data()) {
  hand_decomposition(
    data,
    choice = ~ inertia + recip,
    theta_index = list(choice = 1:2)
  )
}

fixed_only_free_rate <- function(data = fixed_only_data()) {
  hand_decomposition(
    data,
    rate = ~ 1 + outdeg,
    theta_index = list(rate = 1:2)
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

fixed_only_with_pinned <- function(data = fixed_only_data()) {
  hand_decomposition(
    data,
    rate = ~1,
    choice = ~ inertia + recip,
    theta_index = list(choice = 1:2),
    pinned_rate = fixed_only_pinned_rate()
  )
}

fixed_only_with_choice <- function(choice, data = fixed_only_data()) {
  hand_decomposition(
    data,
    rate = ~ 1 + outdeg,
    choice = choice,
    theta_index = list(rate = 1:2)
  )
}

fixed_only_pool <- function(decomposition, data = fixed_only_data()) {
  build_sequence_pool(
    decomposition,
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
  free_only <- fixed_only_pool(fixed_only_free_choice())
  with_pinned <- fixed_only_pool(fixed_only_with_pinned())

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

  # Path B moves the event at t = 12 to t = 8, across the wave boundary: the
  # same exposure per period, but one event's log-hazard changes from c2 to c1.
  path_a <- fixed_only_data()
  path_b <- fixed_only_data(calls_time = c(1, 3, 6, 10, 8, 15))
  path_b$ties <- path_b$ties[order(path_b$ties$time), ]

  fixed_term <- function(data) {
    with_pinned <- evaluate_sequence_pool(
      fixed_only_pool(fixed_only_with_pinned(), data),
      theta
    )
    free_only <- evaluate_sequence_pool(
      fixed_only_pool(fixed_only_free_choice(), data),
      theta
    )
    with_pinned$loglik[[1]] - free_only$loglik[[1]]
  }
  # c1 − c2 = (−1) − (−2) = 1.
  expect_equal(fixed_term(path_b) - fixed_term(path_a), 1, tolerance = 1e-10)

  # On one path, the reweight log f(Ω;θ) − log f(Ω;θ_ref) is unchanged by the
  # θ-free term.
  log_ratio <- function(decomposition) {
    pool <- fixed_only_pool(decomposition, path_a)
    evaluate_sequence_pool(pool, theta, what = "loglik")$loglik -
      evaluate_sequence_pool(pool, theta_ref, what = "loglik")$loglik
  }
  expect_equal(
    log_ratio(fixed_only_with_pinned()),
    log_ratio(fixed_only_free_choice()),
    tolerance = 1e-10
  )
})

test_that("an authored all-offset fid is scored the same way", {
  theta <- c(-2, 0.3)
  base <- evaluate_sequence_pool(fixed_only_pool(fixed_only_free_rate()), theta)
  res <- evaluate_sequence_pool(
    fixed_only_pool(fixed_only_with_choice(~ offset(inertia, coef = 0.5))),
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
  theta <- c(-2, 0.3)
  base <- evaluate_sequence_pool(fixed_only_pool(fixed_only_free_rate()), theta)
  res <- expect_no_error(
    evaluate_sequence_pool(fixed_only_pool(fixed_only_with_choice(~1)), theta)
  )
  expect_equal(
    res$loglik[[1]],
    base$loglik[[1]] - 6 * log(3),
    tolerance = 1e-10
  )
  expect_equal(res$score, base$score, tolerance = 1e-12)
})

test_that("a pinned rate splits an interval that straddles a wave boundary", {
  # Moving the boundary event from t = 10 to t = 11 leaves the interval
  # (6, 11] across the interior boundary at 10. The risk set is constant
  # within an interval, so its exposure splits at the boundary exactly as if
  # an event sat there: 4 * 4 at c1 and 4 * 1 at c2.
  straddling <- fixed_only_data(calls_time = c(1, 3, 6, 11, 12, 15))
  theta <- c(0.3, -0.2)
  base <- evaluate_sequence_pool(
    fixed_only_pool(fixed_only_free_choice(straddling), straddling),
    theta
  )
  res <- evaluate_sequence_pool(
    expect_no_error(
      fixed_only_pool(fixed_only_with_pinned(straddling), straddling)
    ),
    theta
  )

  c1 <- -1
  c2 <- -2
  pinned <- 3 * c1 + 3 * c2 - exp(c1) * 4 * 9 - exp(c2) * 4 * 5
  expect_equal(res$loglik[[1]], base$loglik[[1]] + pinned, tolerance = 1e-10)
})

# A free sub-model may also carry offset() terms: theta covers only its free
# coefficients, while the engine lays out every coefficient, offsets included.
# The adapter must place theta at the free positions and harvest the score and
# Fisher information at those positions only.
test_that("a sub-model mixing free and offset terms uses its free slots", {
  data("social_evolution", envir = environment())
  formula <- calls ~ inertia + offset(recip, coef = 0.5) + trans
  decomposition <- hand_decomposition(
    social_evolution,
    choice = ~ inertia + offset(recip, coef = 0.5) + trans,
    theta_index = list(choice = 1:2)
  )
  pool <- build_sequence_pool(
    decomposition,
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
  decomposition <- hand_decomposition(
    social_evolution,
    rate = ~ 1 + offset(indeg, coef = 0.2) + outdeg,
    theta_index = list(rate = 1:2)
  )
  pool <- build_sequence_pool(
    decomposition,
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
  decomposition <- hand_decomposition(
    social_evolution,
    choice = ~ inertia + offset(recip, coef = 0.5) + trans,
    theta_index = list(choice = 1:2)
  )
  eval_fn <- make_proposal_evaluator(decomposition, c(0.4, -0.1))
  out <- eval_fn(social_evolution)

  direct <- direct_zero_iteration(social_evolution, formula, c(0.4, 0, -0.1))
  expect_equal(out$loglik, direct$log_likelihood, tolerance = 1e-10)
})

test_that("a theta_index not matching the free coefficients aborts at entry", {
  data("social_evolution", envir = environment())
  decomposition <- hand_decomposition(
    social_evolution,
    choice = ~ inertia + offset(recip, coef = 0.5) + trans,
    theta_index = list(choice = 1:3)
  )
  expect_error(
    build_sequence_pool(
      decomposition,
      list(list(
        data = social_evolution,
        reference = new_reference_record(c(0, 0, 0), -100, -100)
      )),
      theta_length = 3L
    ),
    class = "goldfish_theta_index_mismatch"
  )
})

# --- Joint single-clock likelihood ------------------------------------------

# Pool entry scores a sequence under the joint single-clock likelihood: one
# merged walk per sequence over the pool engine spec (the completed spec with
# each effect-free bundle swapped for its pool_engine_formula()), stored by
# fid. A process's own events are dependent events for its fids; any other
# modeled process's events are state changes and right-censoring boundaries
# for the timed rates, which are exposed to the end of the observation window.
#
# dynes_multiwave_spec() has four fids over six actors, all present:
#   1 friendship rate, completed and pinned: log(1/12) on [0, 2), log(1/6) on
#     [2, 5] (one event in two time units, then three in three, six actors);
#   2 friendship choice, inertia free, recip offset at -0.5;
#   3 calls rate, intercept and indeg(friendship) free;
#   4 calls choice, completed uniform over the five other actors.
# Friendship events fall at t = 1..4 and calls events at t = 1..5, so the walk
# runs from t = 1 to the window end at t = 5.

multiwave_sequence <- function(data) {
  list(data = data, reference = new_reference_record(c(0, 0, 0), -1, -1))
}

multiwave_pool <- function(spec = dynes_multiwave_spec(), data = spec$data) {
  build_sequence_pool(
    decompose_joint_spec(spec),
    list(multiwave_sequence(data)),
    theta_length = 3L
  )
}

# One fid's log-likelihood, score and Fisher information at `parameters` (one
# value per engine coefficient, offsets included), through the engine at zero
# iterations over `stats` -- the reference every pool quantity is checked
# against, computed without the pool.
zero_iteration_fit <- function(formula, sub_model, data, stats, parameters) {
  estimate_wrapper(
    x = formula,
    model = "DyNAM",
    sub_model = sub_model,
    data = data,
    preprocessed = stats,
    control_algo = set_algorithm_newton(
      initial_parameters = parameters,
      max_iterations = 0L,
      backend = "r"
    ),
    verbose = FALSE,
    progress = FALSE
  )
}

# The free slots coef_layout() numbers for `fid`.
layout_slots <- function(spec, fid) {
  layout <- coef_layout(spec)
  layout$index[layout$fid == fid & !layout$fixed]
}

# The θ-free part of dynes_multiwave_spec()'s joint log-likelihood, by hand:
# the pinned friendship rate over the whole window plus the uniform calls
# choice, -log 5 at each of the five calls events.
multiwave_fixed_loglik <- function() {
  # The wave snapshots at 0, 2 and 5 hold 0, 2 and 4 friendship ties (the row
  # stamped at t = 2 belongs to wave 2), so each period pins 2 flips over the
  # six-actor risk set: 2 / (2 * 6) and 2 / (3 * 6).
  c1 <- log(1 / 6)
  c2 <- log(1 / 9)
  # Events at t = 1 (period 1) and t = 2, 3, 4 (period 2); the six-actor risk
  # set is exposed over (1, 2] at c1 and over (2, 5] at c2, the last unit being
  # the interval after the last friendship event that the per-process walk
  # dropped.
  pinned <- c1 + 3 * c2 - 6 * (exp(c1) * 1 + exp(c2) * 3)
  pinned - 5 * log(5)
}

test_that("pool-entry statistics are the engine spec's joint walk, by fid", {
  spec <- dynes_multiwave_spec()
  decomposition <- decompose_joint_spec(spec)
  pool <- build_sequence_pool(
    decomposition,
    list(multiwave_sequence(spec$data)),
    theta_length = 3L
  )

  walked <- preprocess_joint(
    decomposition$engine_spec,
    control_preprocessing = set_preprocessing()
  )
  stored <- pool$sequences[[1]]$stats

  expect_named(stored, as.character(spec$process_map$fid))
  for (fid in names(walked)) {
    expect_equal(stored[[fid]], walked[[fid]])
  }
})

test_that("the fixed-only term exposes a pinned rate to the window end", {
  spec <- dynes_multiwave_spec()
  expect_equal(
    spec$completed_rates[["1"]]$intercept,
    c(log(1 / 6), log(1 / 9))
  )

  pool <- multiwave_pool(spec)

  # The per-process path stopped the friendship rate at its last event (t = 4)
  # and scored this term exactly 2/3 higher: exp(log(1/9)) * 6 actors * (4, 5].
  expect_equal(
    pool$sequences[[1]]$fixed_loglik,
    multiwave_fixed_loglik(),
    tolerance = 1e-10
  )
})

test_that("a multi-process likelihood carries cross-process censoring", {
  stored <- multiwave_pool()$sequences[[1]]$stats

  # The calls rate: dependent at its own five events, censored at the
  # friendship events that fall between them, exposed over (1, 5].
  calls_rate <- stored[["3"]]
  dependent <- calls_rate$is_dependent == 1L
  expect_equal(calls_rate$event_time[dependent], c(1, 2, 3, 4, 5))
  expect_equal(calls_rate$event_time[!dependent], c(2, 3, 4))
  expect_equal(sum(calls_rate$intervals), 4)

  # The pinned friendship rate: dependent at its own four events, then a
  # censoring row at the window end rather than stopping at t = 4.
  friendship_rate <- stored[["1"]]
  dependent <- friendship_rate$is_dependent == 1L
  expect_equal(friendship_rate$event_time[dependent], c(1, 2, 3, 4))
  expect_equal(friendship_rate$event_time[!dependent], 5)
  expect_equal(sum(friendship_rate$intervals), 4)
})

# A friendship choice whose free effect varies across the friendship events,
# so the likelihood can be evaluated at theta (see dynes_multiwave_spec()).
indeg_friendship_choice <- ~ indeg + offset(recip, coef = -0.5)

# The free fids of dynes_multiwave_spec(indeg_friendship_choice) at `theta` over
# `data`, from the raw (uncompleted) spec's own joint walk: it holds the same
# friendship choice and calls rate, and needs no pool engine spec because every
# raw fid has an effect.
multiwave_free_fits <- function(spec, data, theta) {
  raw <- bind_candidate_data(
    dynes_pinned_raw_spec(indeg_friendship_choice),
    data
  )
  walk <- preprocess_joint(raw, control_preprocessing = set_preprocessing())
  list(
    friendship = zero_iteration_fit(
      friendship ~ indeg + offset(recip, coef = -0.5),
      "choice",
      data,
      walk[["1"]],
      c(theta[layout_slots(spec, 2L)], -0.5)
    ),
    calls = zero_iteration_fit(
      calls ~ 1 + indeg(friendship),
      "rate",
      data,
      walk[["2"]],
      theta[layout_slots(spec, 3L)]
    )
  )
}

test_that("pool evaluation is the joint likelihood at theta", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  theta <- c(0.3, -1.2, 0.4)
  pool <- multiwave_pool(spec)

  res <- evaluate_sequence_pool(pool, theta)

  fits <- multiwave_free_fits(spec, spec$data, theta)
  friendship_slot <- layout_slots(spec, 2L)
  calls_slots <- layout_slots(spec, 3L)
  expect_equal(
    res$loglik[[1]],
    multiwave_fixed_loglik() +
      fits$friendship$log_likelihood +
      fits$calls$log_likelihood,
    tolerance = 1e-10
  )
  expect_equal(
    res$score[1, friendship_slot],
    fits$friendship$final_score[1],
    tolerance = 1e-10
  )
  expect_equal(
    res$score[1, calls_slots],
    fits$calls$final_score,
    tolerance = 1e-10
  )
  expect_equal(
    res$fisher[[1]][calls_slots, calls_slots],
    fits$calls$final_information_matrix,
    tolerance = 1e-10
  )
})

test_that("the proposal evaluator scores on the pool's joint likelihood", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  decomposition <- decompose_joint_spec(spec)
  theta <- c(0.3, -1.2, 0.4)
  # Calls events moved off the friendship times; the window still ends at 5
  # and the calls choice still has five events, so the fixed-only term is
  # unchanged.
  candidate <- dynes_multiwave_data("panel")
  candidate$ties$time[candidate$ties$layer == "calls"] <- c(1, 2.5, 3, 4.5, 5)

  pool <- build_sequence_pool(
    decomposition,
    list(multiwave_sequence(candidate)),
    theta_length = 3L
  )
  reference <- evaluate_sequence_pool(pool, theta, what = "loglik")$loglik
  eval_fn <- make_proposal_evaluator(decomposition, theta, spec = spec)
  loglik <- eval_fn(candidate)$loglik

  # The MCMC ratio f'/f and the importance weights must target one density,
  # and it is the joint one.
  expect_equal(loglik, reference, tolerance = 1e-10)
  fits <- multiwave_free_fits(spec, candidate, theta)
  expect_equal(
    loglik,
    multiwave_fixed_loglik() +
      fits$friendship$log_likelihood +
      fits$calls$log_likelihood,
    tolerance = 1e-10
  )
})

test_that("a joint-walk interval across a wave time is cut per plateau", {
  # Wave times 0, 3.5, 5: friendship events at 1, 2, 3 pin log(3 / (3.5 * 6))
  # and the one at 4 pins log(1 / (1.5 * 6)).
  spec <- suppressWarnings(complete_generative_spec(
    dynes_pinned_raw_spec(),
    wave_times = c(0, 3.5, 5)
  ))
  c1 <- log(3 / (3.5 * 6))
  c2 <- log(1 / (1.5 * 6))
  expect_equal(spec$completed_rates[["1"]]$intercept, c(c1, c2))

  pool <- multiwave_pool(spec)

  # The joint walk gives the pinned rate the intervals (1, 2], (2, 3], (3, 4]
  # and (4, 5]; the calls events share the friendship times or fall at the
  # window end, so no other process adds a cut at 3.5. (3, 4] straddles 3.5
  # and is split half at c1, half at c2, as if an event sat on the boundary.
  pinned <- 3 * c1 + c2 - 6 * (exp(c1) * 2.5 + exp(c2) * 1.5)
  expect_equal(
    pool$sequences[[1]]$fixed_loglik,
    pinned - 5 * log(5),
    tolerance = 1e-10
  )
})

test_that("the joint walk reads the joint-level data, not member copies", {
  spec <- dynes_multiwave_spec()
  engine_spec <- decompose_joint_spec(spec)$engine_spec
  moved <- dynes_multiwave_data("panel")
  moved$ties$time[moved$ties$layer == "calls"] <- c(1, 2.5, 3, 4.5, 5)

  # Only the joint-level data is swapped; each member keeps its stale copy.
  joint_only <- engine_spec
  joint_only$data <- moved
  walk <- function(s) {
    preprocess_joint(s, control_preprocessing = set_preprocessing())
  }

  expect_equal(walk(joint_only), walk(bind_candidate_data(engine_spec, moved)))
  expect_false(isTRUE(all.equal(walk(joint_only), walk(engine_spec))))
})

# --- Flavored processes ------------------------------------------------------

# dynes_flavored_spec(): fids 1-4 are the calls creation rate, creation choice,
# dissolution rate and dissolution choice; fid 5 the emails choice.

test_that("an augmented flavored sequence routes each event to its flavor", {
  spec <- dynes_flavored_spec()
  # A different draw on the same actors, its flavor keys stamped by
  # add_flavor() as an augmented sequence's must be.
  augmented <- dynes_flavored_data(seed = 11L)
  timed <- augmented$ties$layer == "calls" & !is.na(augmented$ties$time)
  expect_false(anyNA(augmented$ties$flavor[timed]))

  walked <- suppressWarnings(preprocess_joint(
    bind_candidate_data(spec, augmented),
    control_preprocessing = set_preprocessing()
  ))

  map <- spec$process_map
  for (i in which(map$layer == "calls")) {
    own <- timed & augmented$ties$flavor == map$flavor[[i]]
    stats <- walked[[as.character(map$fid[[i]])]]
    expect_equal(
      stats$event_time[stats$is_dependent == 1L],
      sort(augmented$ties$time[own]),
      label = paste("fid", map$fid[[i]], "dependent event times")
    )
  }
})

test_that("flavored fids are scored as on the ordinary flavored path", {
  spec <- dynes_flavored_spec()
  map <- spec$process_map
  theta <- c(-3, 0.2, 0.5, -0.3, -2.5, 0.1, 0.4, 0.6)
  decomposition <- decompose_joint_spec(spec)
  pool <- suppressWarnings(build_sequence_pool(
    decomposition,
    list(list(
      data = spec$data,
      reference = new_reference_record(theta, -1, -1)
    )),
    theta_length = length(theta)
  ))

  res <- evaluate_sequence_pool(pool, theta)

  # The ordinary flavored path: estimate_flavored()'s one preprocessing pass
  # (keyed family-major, so matched here by flavor and family), then the
  # engine at zero iterations per process.
  preps <- suppressWarnings(preprocess_flavored(dynes_flavored_calls_spec()))
  flavored_map <- attr(preps, "process_map")
  loglik <- 0
  for (i in which(map$layer == "calls")) {
    key <- flavored_map$fid[
      flavored_map$flavor == map$flavor[[i]] &
        flavored_map$family == map$family[[i]]
    ]
    prep <- preps[[as.character(key)]]
    slots <- layout_slots(spec, map$fid[[i]])
    ref <- zero_iteration_fit(
      prep$formula,
      map$family[[i]],
      spec$data,
      prep,
      theta[slots]
    )
    loglik <- loglik + ref$log_likelihood
    expect_equal(res$score[1, slots], ref$final_score, tolerance = 1e-10)
    expect_equal(
      res$fisher[[1]][slots, slots, drop = FALSE],
      ref$final_information_matrix,
      tolerance = 1e-10
    )
  }
  # The emails choice reads only calls state, so its ordinary single-process
  # preprocessing is the reference too.
  emails <- zero_iteration_fit(
    emails ~ tie(calls),
    "choice",
    spec$data,
    estimate_wrapper(
      x = emails ~ tie(calls),
      model = "DyNAM",
      sub_model = "choice",
      data = spec$data,
      preprocessing_only = TRUE
    ),
    theta[layout_slots(spec, 5L)]
  )
  loglik <- loglik + emails$log_likelihood

  expect_false(res$degenerate[[1]])
  expect_equal(res$loglik[[1]], loglik, tolerance = 1e-10)
})

# --- The cpp-backend pool evaluator ------------------------------------------

# Pool evaluation on the compiled engine: one per-sequence loop over
# make_engine_evaluator(backend = "cpp") closures built at evaluation time over
# the stored statistics, harvesting the log-likelihood, score and Fisher
# information behind `what`. Its reference is the retired zero-iteration
# adapter, kept here as zero_iteration_reference(), at 1e-10 on the joint
# single-clock likelihood.

# Every pooled sequence of `pool` through the compiled engine, one at a time.
cpp_pool_results <- function(
  pool,
  theta,
  what = c("loglik", "score", "fisher")
) {
  lapply(pool$sequences, function(s) {
    evaluate_pool_sequence(pool, s, theta, what = what)
  })
}

# The retired adapter: each free sub-model through the Newton engine at zero
# optimizer iterations over the stored statistics, on the sequence's `data`
# (pool entries no longer hold it), plus the stored fixed-only term.
zero_iteration_reference <- function(pool, data, theta) {
  p <- pool$theta_length
  lapply(seq_along(pool$sequences), function(i) {
    s <- pool$sequences[[i]]
    out <- list(loglik = s$fixed_loglik, score = numeric(p))
    out$fisher <- matrix(0, p, p)
    for (sm in pool$sub_models) {
      if (sm$fixed_only) {
        next
      }
      free <- sm$coefficients$free
      parameters <- numeric(sm$coefficients$n_params)
      parameters[free] <- theta[sm$theta_index]
      fit <- estimate_wrapper(
        x = sm$engine_formula,
        model = sm$model,
        sub_model = sm$sub_model,
        data = data[[i]],
        preprocessed = fid_stats(s$stats, sm),
        control_algo = set_algorithm_newton(
          initial_parameters = parameters,
          max_iterations = 0L,
          backend = "r"
        ),
        control_prep = pool$control_prep,
        verbose = FALSE,
        progress = FALSE
      )
      out$loglik <- out$loglik + fit$log_likelihood
      out$score[sm$theta_index] <- fit$final_score[free]
      out$fisher[sm$theta_index, sm$theta_index] <-
        fit$final_information_matrix[free, free]
    }
    out
  })
}

expect_matches_adapter <- function(pool, theta, data) {
  reference <- zero_iteration_reference(pool, data, theta)
  cpp <- cpp_pool_results(pool, theta)
  for (i in seq_along(cpp)) {
    expect_false(cpp[[i]]$degenerate)
    expect_equal(cpp[[i]]$loglik, reference[[i]]$loglik, tolerance = 1e-10)
    expect_equal(cpp[[i]]$score, reference[[i]]$score, tolerance = 1e-10)
    expect_equal(cpp[[i]]$fisher, reference[[i]]$fisher, tolerance = 1e-10)
  }
}

test_that("the cpp evaluator matches the adapter over a pool of drawn paths", {
  withr::local_seed(20260930)
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  theta <- c(0.3, -1.2, 0.4)
  drawn <- new_multiwave_augmenter()$draw(1L, 1:10, theta)
  sequences <- lapply(drawn, function(d) {
    list(data = d$data, reference = new_reference_record(theta, -1, -1))
  })
  pool <- build_sequence_pool(
    decompose_joint_spec(spec),
    sequences,
    theta_length = 3L
  )

  expect_matches_adapter(pool, theta, lapply(drawn, `[[`, "data"))
})

test_that("the cpp evaluator places each sub-model at its theta_index", {
  data("social_evolution", envir = environment())
  decomposition <- hand_decomposition(
    social_evolution,
    rate = ~ 1 + offset(indeg, coef = 0.2) + outdeg,
    choice = ~ inertia + offset(recip, coef = 0.5) + trans,
    theta_index = list(rate = 3:4, choice = 1:2)
  )
  pool <- build_sequence_pool(
    decomposition,
    list(list(
      data = social_evolution,
      reference = new_reference_record(numeric(4), -100, -100)
    )),
    theta_length = 4L
  )

  expect_matches_adapter(pool, c(0.4, -0.1, -4, 0.05), list(social_evolution))
})

test_that("the cpp evaluator scores flavored fids on their own events", {
  spec <- dynes_flavored_spec()
  theta <- c(-3, 0.2, 0.5, -0.3, -2.5, 0.1, 0.4, 0.6)
  pool <- suppressWarnings(build_sequence_pool(
    decompose_joint_spec(spec),
    list(list(
      data = spec$data,
      reference = new_reference_record(theta, -1, -1)
    )),
    theta_length = length(theta)
  ))

  expect_matches_adapter(pool, theta, list(spec$data))
})

test_that("the cpp evaluator runs no adapter and builds closures per call", {
  fx <- make_pool_fixture(n_sequences = 1L)
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)
  sequence <- pool$sequences[[1]]

  builds <- list()
  real_engine <- make_engine_evaluator
  local_mocked_bindings(
    estimate_wrapper = function(...) stop("the adapter was called"),
    make_engine_evaluator = function(..., backend) {
      builds[[length(builds) + 1L]] <<- backend
      real_engine(..., backend = backend)
    }
  )
  first <- evaluate_pool_sequence(pool, sequence, c(1, 0.5, -0.2))
  evaluate_pool_sequence(pool, sequence, c(0, 0, 0))

  expect_false(first$degenerate)
  # No closure is held with the sequence: one build per free fid per call.
  expect_identical(builds, list("cpp", "cpp"))
  expect_null(sequence$engine)
})

test_that("the cpp evaluator harvests only what is requested", {
  fx <- make_pool_fixture(n_sequences = 1L)
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)
  sequence <- pool$sequences[[1]]
  theta <- c(1, 0.5, -0.2)
  full <- evaluate_pool_sequence(pool, sequence, theta)

  score_only <- evaluate_pool_sequence(
    pool,
    sequence,
    theta,
    what = "score"
  )
  expect_named(score_only, c("score", "degenerate"), ignore.order = TRUE)
  expect_identical(score_only$score, full$score)

  fisher_only <- evaluate_pool_sequence(
    pool,
    sequence,
    theta,
    what = "fisher"
  )
  expect_named(fisher_only, c("fisher", "degenerate"), ignore.order = TRUE)
  expect_identical(fisher_only$fisher, full$fisher)

  loglik_only <- evaluate_pool_sequence(
    pool,
    sequence,
    theta,
    what = "loglik"
  )
  expect_named(loglik_only, c("loglik", "degenerate"), ignore.order = TRUE)
  expect_identical(loglik_only$loglik, full$loglik)
})

test_that("the cpp evaluator keeps the singular-information guard", {
  # The compiled closure never inverts the information matrix, but the
  # evaluator contract marks a singular sequence non-finite, as the adapter
  # does, so both backends weight it out of the pool.
  fx <- make_pool_fixture(n_sequences = 1L)
  pool <- build_sequence_pool(fx$decomposition, fx$sequences, theta_length = 3L)

  res <- expect_no_error(
    evaluate_pool_sequence(
      pool,
      pool$sequences[[1]],
      c(50, 50, 50),
      what = c("loglik", "score")
    )
  )
  expect_true(res$degenerate)
  expect_false(is.finite(res$loglik))
  expect_true(all(is.na(res$score)))
})

test_that("the cpp evaluator scores a rate's censored tail to the window end", {
  # The calls rate alone, so the two paths differ only in its likelihood.
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  decomposition <- decompose_joint_spec(spec)
  calls_rate <- new_pool_decomposition(
    Filter(function(sm) identical(sm$fid, 3L), decomposition$sub_models),
    decomposition$engine_spec
  )
  # Calls stop at 3.8; the last friendship event closes the window at
  # `t_end`, leaving the calls rate a censored tail (3.8, t_end].
  tail_path <- function(t_end) {
    data <- dynes_multiwave_data("panel")
    data$ties$time[data$ties$layer == "calls"] <- c(1, 2, 3, 3.5, 3.8)
    data$ties$time[data$ties$layer == "friendship"] <- c(1, 2, 3, t_end)
    data
  }
  theta <- c(0.3, -1.2, 0.4)
  scored <- lapply(c(4, 4.6), function(t_end) {
    pool <- build_sequence_pool(
      calls_rate,
      list(multiwave_sequence(tail_path(t_end))),
      theta_length = 3L
    )
    stats <- pool$sequences[[1]]$stats[["3"]]
    last <- length(stats$event_time)
    expect_identical(stats$is_dependent[[last]], 0L)
    expect_equal(stats$intervals[[last]], t_end - 3.8)
    evaluate_pool_sequence(
      pool,
      pool$sequences[[1]],
      theta,
      what = "loglik"
    )$loglik
  })

  # Friendship ties 1->2, 2->3, 3->4 stand over the tail, so three of the six
  # senders have indeg(friendship) = 1: Λ = e^{b0} (3 e^{b1} + 3), and the
  # longer tail adds −0.6 Λ to the log-likelihood.
  b <- theta[layout_slots(spec, 3L)]
  lambda <- exp(b[[1]]) * (3 * exp(b[[2]]) + 3)
  expect_equal(scored[[2]] - scored[[1]], -0.6 * lambda, tolerance = 1e-10)
})

test_that("fixed-only terms are scored on the compiled engine", {
  # The fixed-only terms carry the same values as before, now computed without
  # the zero-iteration adapter: estimate_wrapper() is never reached.
  offset_choice <- estimate_wrapper(
    x = calls ~ offset(inertia, coef = 0.5),
    model = "DyNAM",
    sub_model = "choice",
    data = fixed_only_data(),
    control_algo = set_algorithm_newton(max_iterations = 0L, backend = "r"),
    verbose = FALSE,
    progress = FALSE
  )$log_likelihood
  local_mocked_bindings(
    estimate_wrapper = function(...) stop("the adapter was called")
  )
  fixed_term <- function(decomposition) {
    fixed_only_pool(decomposition)$sequences[[1]]$fixed_loglik
  }

  # The two-plateau pinned rate, read off its per-period offset column.
  c1 <- -1
  c2 <- -2
  pinned <- 3 * c1 + 3 * c2 - exp(c1) * 4 * 9 - exp(c2) * 4 * 5
  expect_equal(fixed_term(fixed_only_with_pinned()), pinned, tolerance = 1e-10)
  # The completed uniform choice, with no statistic column: −log 3 per event.
  expect_equal(
    fixed_term(fixed_only_with_choice(~1)),
    -6 * log(3),
    tolerance = 1e-10
  )
  # An authored all-offset fid.
  expect_equal(
    fixed_term(fixed_only_with_choice(~ offset(inertia, coef = 0.5))),
    offset_choice,
    tolerance = 1e-10
  )
  # The pinned rate's censored tail to the window end in the joint walk.
  expect_equal(
    multiwave_pool()$sequences[[1]]$fixed_loglik,
    multiwave_fixed_loglik(),
    tolerance = 1e-10
  )
})

test_that("the cpp evaluator gives a fixed-only fid no score or Fisher block", {
  theta <- c(0.3, -0.2)
  base <- fixed_only_pool(fixed_only_free_choice())
  with_pinned <- fixed_only_pool(fixed_only_with_pinned())

  res <- cpp_pool_results(with_pinned, theta)[[1]]
  ref <- cpp_pool_results(base, theta)[[1]]

  expect_identical(length(res$score), 2L)
  expect_identical(dim(res$fisher), c(2L, 2L))
  expect_equal(res$score, ref$score, tolerance = 1e-12)
  expect_equal(res$fisher, ref$fisher, tolerance = 1e-12)
  expect_equal(
    res$loglik - ref$loglik,
    with_pinned$sequences[[1]]$fixed_loglik,
    tolerance = 1e-10
  )
  expect_matches_adapter(with_pinned, theta, list(fixed_only_data()))
})

# --- Importance weights as wired to the compiled evaluator -------------------
# The adapter retires: every target-density path (pool evaluation, the proposal
# evaluator, compute_lik_seq()) runs on the compiled engine, so the weights the
# loop assembles from the stored reference records are those of the compiled
# per-sequence quantities. The zero-iteration fits the reference values come
# from are computed before the adapter is shut off.

local_no_adapter <- function(env = parent.frame()) {
  local_mocked_bindings(
    estimate_wrapper = function(...) stop("the adapter was called"),
    .env = env
  )
}

# dynes_multiwave_spec(indeg_friendship_choice) on two paths that differ in
# one friendship event, moved from the interior wave t = 2 (which belongs to
# the second period) to t = 1.5, in the first: the pinned term then differs
# between them.
weights_wired_paths <- function() {
  moved <- dynes_multiwave_data()
  moved$ties$time[moved$ties$layer == "friendship"] <- c(1, 1.5, 3, 4)
  moved$ties <- moved$ties[order(moved$ties$time), ]
  list(dynes_multiwave_data(), moved)
}

test_that("pool evaluation never reaches the zero-iteration adapter", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  theta <- c(0.3, -1.2, 0.4)
  fits <- multiwave_free_fits(spec, spec$data, theta)
  pool <- multiwave_pool(spec)
  local_no_adapter()

  res <- evaluate_sequence_pool(pool, theta)

  expect_false(res$degenerate[[1]])
  expect_equal(
    res$loglik[[1]],
    multiwave_fixed_loglik() +
      fits$friendship$log_likelihood +
      fits$calls$log_likelihood,
    tolerance = 1e-10
  )
  calls_slots <- layout_slots(spec, 3L)
  expect_equal(
    res$score[1, calls_slots],
    fits$calls$final_score,
    tolerance = 1e-10
  )
  expect_equal(
    res$fisher[[1]][calls_slots, calls_slots],
    fits$calls$final_information_matrix,
    tolerance = 1e-10
  )
})

test_that("the proposal evaluator never reaches the zero-iteration adapter", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  theta <- c(0.3, -1.2, 0.4)
  expected <- evaluate_sequence_pool(multiwave_pool(spec), theta)$loglik[[1]]
  local_no_adapter()

  eval_fn <- make_proposal_evaluator(decompose_joint_spec(spec), theta)

  expect_equal(eval_fn(spec$data)$loglik, expected, tolerance = 1e-10)
})

test_that("the evaluator has one engine: no backend argument, no adapter", {
  expect_false("backend" %in% names(formals(evaluate_pool_sequence)))
  expect_false(exists("pool_engine_eval", envir = asNamespace("goldfish")))
})

test_that("a resident pool entry holds no data object", {
  pool <- multiwave_pool(dynes_multiwave_spec(indeg_friendship_choice))

  entry <- pool$sequences[[1]]
  expect_null(entry$data)
  expect_setequal(
    names(entry),
    c("stats", "fixed_loglik", "fixed_degenerate", "reference")
  )
  resident <- resident_pool_store(pool$store$key)$entries
  for (e in resident) {
    expect_null(e$data)
  }
})

test_that("the loop's reference records feed the weights the adapter gives", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  paths <- weights_wired_paths()
  theta0 <- c(0.3, -1.2, 0.4)
  log_proposal <- c(-4, -6)
  # The reference: each path's joint log-likelihood at theta0 from the
  # zero-iteration fits plus its θ-free fixed-only term at entry.
  fixed <- vapply(
    paths,
    function(d) multiwave_pool(spec, d)$sequences[[1]]$fixed_loglik,
    numeric(1)
  )
  free <- vapply(
    paths,
    function(d) {
      fits <- multiwave_free_fits(spec, d, theta0)
      fits$friendship$log_likelihood + fits$calls$log_likelihood
    },
    numeric(1)
  )
  expected_loglik <- fixed + free
  expected_weights <- stable_softmax(
    expected_loglik - log_proposal
  )$probabilities

  local_no_adapter()
  local_scripted_optimizer(function(theta) theta)
  res <- run_abem_loop(
    theta0 = theta0,
    decomposition = decompose_joint_spec(spec),
    augmenter = new_stub_augmenter(paths, log_proposal = log_proposal),
    control = set_algorithm_em(
      n_sequences = 2L,
      augmenter = set_augmenter_options(routine = "random")
    ),
    spec = spec
  )

  references <- pool_references(res$pool)
  for (i in 1:2) {
    expect_equal(references[[i]]$theta_ref, theta0)
    expect_equal(references[[i]]$log_proposal, log_proposal[[i]])
    expect_equal(
      references[[i]]$loglik_ref,
      expected_loglik[[i]],
      tolerance = 1e-10
    )
  }
  expect_equal(res$weights, expected_weights, tolerance = 1e-10)
  expect_equal(
    res$em_trace$ess[[1]],
    effective_sample_size(expected_weights),
    tolerance = 1e-10
  )
})

test_that("the reference record's fixed-only term is computed once, at entry", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  paths <- weights_wired_paths()
  theta0 <- c(0.3, -1.2, 0.4)
  theta_new <- c(0.5, -1, 0.2)
  free_loglik <- function(theta) {
    vapply(
      paths,
      function(d) {
        fits <- multiwave_free_fits(spec, d, theta)
        fits$friendship$log_likelihood + fits$calls$log_likelihood
      },
      numeric(1)
    )
  }
  free_ref <- free_loglik(theta0)
  free_new <- free_loglik(theta_new)
  real_fixed <- fixed_only_loglik
  fixed_calls <- 0L
  local_mocked_bindings(
    fixed_only_loglik = function(...) {
      fixed_calls <<- fixed_calls + 1L
      real_fixed(...)
    }
  )
  local_no_adapter()
  local_scripted_optimizer(function(theta) theta)

  res <- run_abem_loop(
    theta0 = theta0,
    decomposition = decompose_joint_spec(spec),
    augmenter = new_stub_augmenter(paths, log_proposal = c(-4, -6)),
    control = set_algorithm_em(
      n_sequences = 2L,
      augmenter = set_augmenter_options(routine = "random")
    ),
    spec = spec
  )

  # Two fixed-only fids (the pinned friendship rate and the uniform calls
  # choice) on each of two sequences, scored once each at entry although the
  # pool is evaluated at theta0 and at the proposal, and again below.
  expect_identical(fixed_calls, 4L)
  fixed <- vapply(res$pool$sequences, `[[`, numeric(1), "fixed_loglik")
  # Path dependent: the moved event reads the first period's plateau.
  expect_gt(abs(diff(fixed)), 0.1)
  # Carried in the stored log-likelihood at theta_ref ...
  loglik_ref <- vapply(
    pool_references(res$pool),
    `[[`,
    numeric(1),
    "loglik_ref"
  )
  expect_equal(loglik_ref, fixed + free_ref, tolerance = 1e-10)
  # ... so it moves the normalized weights across the two paths ...
  with_fixed <- compute_pool_weights(loglik_ref, c(-4, -6))
  without_fixed <- compute_pool_weights(free_ref, c(-4, -6))
  expect_gt(max(abs(with_fixed - without_fixed)), 1e-3)
  # ... and cancels in a path's reweight across theta.
  lambda <- evaluate_sequence_pool(
    res$pool,
    theta_new,
    what = "loglik"
  )$loglik -
    loglik_ref
  expect_equal(lambda, free_new - free_ref, tolerance = 1e-10)
  expect_identical(fixed_calls, 4L)
})

# --- compute_lik_seq(): per-sequence sugar over the pool evaluator -----------

test_that("compute_lik_seq() is one sequence of the pool evaluator", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  theta <- c(0.3, -1.2, 0.4)
  sequence <- weights_wired_paths()[[2]]
  pooled <- evaluate_sequence_pool(multiwave_pool(spec, sequence), theta)
  local_no_adapter()

  res <- expect_no_warning(compute_lik_seq(spec, sequence, theta))

  layout <- coef_layout(spec)
  labels <- paste0(layout$process, ": ", layout$name)[!layout$fixed]
  expect_named(res, c("loglik", "score", "fisher", "degenerate"))
  expect_false(res$degenerate)
  expect_equal(res$loglik, pooled$loglik[[1]], tolerance = 1e-10)
  expect_equal(unname(res$score), pooled$score[1, ], tolerance = 1e-10)
  expect_named(res$score, labels)
  expect_equal(unname(res$fisher), pooled$fisher[[1]], tolerance = 1e-10)
  expect_identical(dimnames(res$fisher), list(labels, labels))
})

test_that("compute_lik_seq() and the proposal evaluator share one density", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  theta <- c(0.3, -1.2, 0.4)
  sequence <- weights_wired_paths()[[2]]
  eval_fn <- make_proposal_evaluator(decompose_joint_spec(spec), theta)

  expect_equal(
    compute_lik_seq(spec, sequence, theta)$loglik,
    eval_fn(sequence)$loglik,
    tolerance = 1e-10
  )
})

test_that("compute_lik_seq() reads a named theta by the coefficient labels", {
  spec <- dynes_multiwave_spec(indeg_friendship_choice)
  theta <- c(0.3, -1.2, 0.4)
  layout <- coef_layout(spec)
  labels <- paste0(layout$process, ": ", layout$name)[!layout$fixed]
  named <- rev(stats::setNames(theta, labels))

  expect_equal(
    compute_lik_seq(spec, spec$data, named),
    compute_lik_seq(spec, spec$data, theta)
  )
})

test_that("compute_lik_seq() validates its specification and theta", {
  local_cli_context()
  spec <- dynes_multiwave_spec(indeg_friendship_choice)

  expect_snapshot(error = TRUE, {
    compute_lik_seq(list(), spec$data, c(0, 0, 0))
    compute_lik_seq(spec, spec$data, c(0, 0))
    compute_lik_seq(spec, spec$data, "a")
    compute_lik_seq(spec, spec$data, c(a = 0, b = 0, c = 0))
  })
})

test_that("compute_lik_seq() names itself in the completion warning", {
  local_cli_context()
  spec <- dynes_pinned_raw_spec(indeg_friendship_choice)
  sequence <- weights_wired_paths()[[2]]
  theta <- c(0.3, -1.2, 0.4)

  warned <- character()
  withCallingHandlers(
    compute_lik_seq(spec, sequence, theta),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warned, 2L)
  expect_match(warned, "compute_lik_seq", all = TRUE)
  expect_snapshot(res <- compute_lik_seq(spec, sequence, theta))
})
