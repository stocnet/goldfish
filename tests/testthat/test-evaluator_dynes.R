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
