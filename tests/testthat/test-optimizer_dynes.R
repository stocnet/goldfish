# Task 3.1a/3.1b (abmcem): the SGD M-step optimizer `optimize_abem_sgd()` that
# owns its inner loop (design D6). The EM loop hands it an injected score-only
# batch evaluator plus the normalized selection weights; the optimizer draws
# batches, requests scores, and updates until its stopping rule fires. Tests use
# an analytic stub evaluator (design D11): a separable quadratic per-sequence
# log-likelihood L_i(theta) = -0.5 * ||theta - y_i||^2 whose score is s_i(theta)
# = y_i - theta, so the weighted objective sum_i w_i L_i(theta) is maximized at
# the known weighted mean theta* = sum_i w_i y_i (gradient sum_i w_i (y_i -
# theta) zero there). The stub returns per-sequence score vectors, honoring the
# evaluator contract's per-sequence granularity.

# A score-only evaluator over targets `y`: score_fn(theta, indices) returns the
# per-sequence scores y_i - theta for the requested pool indices, one row each.
make_quadratic_score_fn <- function(targets) {
  function(theta, indices) {
    sweep(targets[indices, , drop = FALSE], 2, theta, "-")
  }
}

test_that("full-variant SGD ascends the stub to the weighted optimum", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  optimum <- colSums(weights * targets)
  score_fn <- make_quadratic_score_fn(targets)

  res <- optimize_abem_sgd(
    theta = c(0, 0),
    score_fn = score_fn,
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_size = 0.5,
      step_schedule = "constant",
      sgd_tolerance = 1e-8,
      max_iterations = 500L
    )
  )

  expect_equal(res$parameters, optimum, tolerance = 1e-5)
  expect_true(res$converged)
  expect_true(res$gradient_norm < 1e-8)
})

test_that("minibatch weighted SGD converges to the weighted optimum", {
  withr::local_seed(20240918L)
  targets <- matrix(rnorm(60), ncol = 2)
  weights <- runif(30)
  weights <- weights / sum(weights)
  optimum <- colSums(weights * targets)
  score_fn <- make_quadratic_score_fn(targets)

  # A decreasing (Bottou) step run to a fixed budget is the Robbins-Monro
  # regime, so the noisy minibatch iterate settles onto the weighted optimum
  # rather than hovering in the O(step) neighborhood a constant step leaves.
  res <- optimize_abem_sgd(
    theta = c(3, -3),
    score_fn = score_fn,
    weights = weights,
    control = set_sgd_options(
      variant = "minibatch",
      batch_size = 8L,
      batch_scheme = "weighted",
      step_size = 0.3,
      step_schedule = "decay",
      convergence = "iterations",
      max_iterations = 20000L
    )
  )

  expect_equal(res$parameters, optimum, tolerance = 1e-2)
})

test_that("cyclic and weighted batch gradients share an expectation", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4, 2, 1), ncol = 2, byrow = TRUE)
  weights <- c(0.1, 0.2, 0.3, 0.4)
  theta <- c(0.5, -0.5)
  scores_all <- sweep(targets, 2, theta, "-")
  full_grad <- colSums(weights * scores_all)

  # Cyclic: two fixed batches partition the pool, and the K/B-scaled importance
  # gradients average exactly to the full-pool weighted gradient over one cycle.
  pool_size <- 4L
  batch_size <- 2L
  g1 <- abem_batch_gradient(
    scores_all[1:2, ],
    weights[1:2],
    TRUE,
    pool_size,
    batch_size
  )
  g2 <- abem_batch_gradient(
    scores_all[3:4, ],
    weights[3:4],
    TRUE,
    pool_size,
    batch_size
  )
  expect_equal((g1 + g2) / 2, full_grad)

  # Weighted: unweighted means over batches drawn with replacement proportional
  # to the weights have the same full-pool weighted gradient in expectation.
  withr::local_seed(11L)
  draws <- replicate(30000, {
    pos <- sample.int(pool_size, batch_size, replace = TRUE, prob = weights)
    abem_batch_gradient(
      scores_all[pos, , drop = FALSE],
      weights[pos],
      FALSE,
      pool_size,
      batch_size
    )
  })
  expect_equal(rowMeans(draws), full_grad, tolerance = 2e-2)
})

test_that("a cyclic-scheme run reaches the same weighted optimum", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4, 2, 1), ncol = 2, byrow = TRUE)
  weights <- c(0.1, 0.2, 0.3, 0.4)
  optimum <- colSums(weights * targets)
  score_fn <- make_quadratic_score_fn(targets)

  res <- optimize_abem_sgd(
    theta = c(0, 0),
    score_fn = score_fn,
    weights = weights,
    control = set_sgd_options(
      variant = "minibatch",
      batch_size = 2L,
      batch_scheme = "cyclic",
      step_size = 0.3,
      step_schedule = "decay",
      convergence = "iterations",
      max_iterations = 20000L
    )
  )

  # The K/B-scaled cyclic gradients average to the full-pool weighted gradient
  # over each cycle, so a decreasing step drives the iterate onto the optimum.
  expect_equal(res$parameters, optimum, tolerance = 5e-3)
})

test_that("fixed parameters stay immobile through the M-step", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  score_fn <- make_quadratic_score_fn(targets)
  free_optimum <- sum(weights * targets[, 2])

  res <- optimize_abem_sgd(
    theta = c(5, 0),
    score_fn = score_fn,
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_size = 0.5,
      step_schedule = "constant",
      sgd_tolerance = 1e-8,
      max_iterations = 500L
    ),
    fixed = 1L
  )

  # The masked coordinate never moves; the free one finds its optimum.
  expect_identical(res$parameters[1], 5)
  expect_equal(res$parameters[2], free_optimum, tolerance = 1e-5)
  expect_true(res$converged)
})

test_that("optimizer accumulators reset every M-step", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  score_fn <- make_quadratic_score_fn(targets)
  run <- function() {
    optimize_abem_sgd(
      theta = c(0, 0),
      score_fn = score_fn,
      weights = weights,
      control = set_sgd_options(
        variant = "full",
        step_schedule = "adam",
        step_size = 0.1,
        convergence = "iterations",
        max_iterations = 200L
      )
    )
  }

  first <- run()
  second <- run()
  # Two independent M-steps from the same start take an identical trajectory:
  # no adaptive-schedule accumulator leaked across the calls.
  expect_equal(first$parameters, second$parameters)
  expect_equal(first$iterations, second$iterations)
})

test_that("every step schedule ascends the analytic stub", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  optimum <- colSums(weights * targets)
  score_fn <- make_quadratic_score_fn(targets)

  for (schedule in c("constant", "decay", "adagrad", "adam", "momentum")) {
    # Per-schedule stable step sizes (momentum's effective rate is
    # step_size / (1 - 0.9), so it needs the smallest); run the fixed budget so
    # each schedule is judged purely on where it lands, not its stop rule.
    step_size <- switch(
      schedule,
      adagrad = 0.2,
      adam = 0.2,
      momentum = 0.1,
      0.4
    )
    res <- optimize_abem_sgd(
      theta = c(0, 0),
      score_fn = score_fn,
      weights = weights,
      control = set_sgd_options(
        variant = "full",
        step_size = step_size,
        step_schedule = schedule,
        convergence = "iterations",
        max_iterations = 20000L
      )
    )
    expect_equal(res$parameters, optimum, tolerance = 1e-2, info = schedule)
  }
})

test_that("hitting the iteration cap with the gradient criterion unmet warns", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  score_fn <- make_quadratic_score_fn(targets)

  expect_warning(
    optimize_abem_sgd(
      theta = c(0, 0),
      score_fn = score_fn,
      weights = weights,
      control = set_sgd_options(
        variant = "full",
        step_size = 1e-4,
        step_schedule = "constant",
        sgd_tolerance = 1e-8,
        max_iterations = 3L,
        convergence = "gradient"
      )
    ),
    "gradient"
  )
})

test_that("the iterations mode runs the budget without a convergence warning", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  score_fn <- make_quadratic_score_fn(targets)

  res <- expect_no_warning(
    optimize_abem_sgd(
      theta = c(0, 0),
      score_fn = score_fn,
      weights = weights,
      control = set_sgd_options(
        variant = "full",
        step_size = 0.5,
        step_schedule = "constant",
        max_iterations = 5L,
        convergence = "iterations"
      )
    )
  )
  expect_equal(res$iterations, 5L)
  expect_false(res$converged)
})

test_that("resampling draws over the multiset with unweighted gradients", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  multiset <- c(1L, 3L, 3L, 2L, 3L)
  score_fn <- make_quadratic_score_fn(targets)
  # Under resampling the objective is the plain mean over the multiset.
  optimum <- colMeans(targets[multiset, , drop = FALSE])

  res <- optimize_abem_sgd(
    theta = c(0, 0),
    score_fn = score_fn,
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_size = 0.5,
      step_schedule = "constant",
      sgd_tolerance = 1e-8,
      max_iterations = 500L
    ),
    use = "resampling",
    resample_indices = multiset
  )

  expect_equal(res$parameters, optimum, tolerance = 1e-5)
})
