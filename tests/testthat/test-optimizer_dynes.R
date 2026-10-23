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

# The stub's information request: every sequence's information is the
# identity, which normalized weights leave at the identity, and the scores of
# the whole pool.
make_quadratic_information_fn <- function(targets) {
  function(theta) {
    list(
      information = diag(ncol(targets)),
      scores = sweep(targets, 2, theta, "-")
    )
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
    information_fn = make_quadratic_information_fn(targets),
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
    information_fn = make_quadratic_information_fn(targets),
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
    information_fn = make_quadratic_information_fn(targets),
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
    information_fn = make_quadratic_information_fn(targets),
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
      information_fn = make_quadratic_information_fn(targets),
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
      information_fn = make_quadratic_information_fn(targets),
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
      information_fn = make_quadratic_information_fn(targets),
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
      information_fn = make_quadratic_information_fn(targets),
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
    information_fn = make_quadratic_information_fn(targets),
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

# The information request. The M-step asks once, at its start, for the pool's
# weighted complete-data information P and the per-sequence scores there, and
# reports its Newton step in standard-error units,
# max_j |(P^-1 g)_j| / sqrt((P^-1)_jj). The stub below has a non-diagonal
# metric A: L_i(theta) = -0.5 (theta - y_i)' A (theta - y_i), so the score is
# A (y_i - theta) and every sequence's information is A, which normalized
# weights (or a multiset mean) leave at A.
metric_a <- matrix(c(2, 0.6, 0.6, 1), 2)
metric_targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)

make_metric_score_fn <- function(targets, metric, seen = NULL) {
  function(theta, indices) {
    if (!is.null(seen)) {
      seen$thetas[[length(seen$thetas) + 1L]] <- theta
    }
    sweep(targets[indices, , drop = FALSE], 2, theta, "-") %*% metric
  }
}

make_metric_information_fn <- function(targets, metric, seen = NULL) {
  scores <- make_metric_score_fn(targets, metric)
  function(theta) {
    if (!is.null(seen)) {
      seen$information_thetas[[length(seen$information_thetas) + 1L]] <- theta
    }
    list(information = metric, scores = scores(theta, seq_len(nrow(targets))))
  }
}

new_metric_seen <- function() {
  seen <- new.env(parent = emptyenv())
  seen$thetas <- list()
  seen$information_thetas <- list()
  seen
}

newton_se_of <- function(gradient, information) {
  inverse <- solve(information)
  max(abs(inverse %*% gradient) / sqrt(diag(inverse)))
}

metric_gradient <- function(theta, weights) {
  colSums(weights * (sweep(metric_targets, 2, theta, "-") %*% metric_a))
}

test_that("the M-step requires an information closure", {
  expect_error(
    optimize_abem_sgd(
      theta = c(0, 0),
      score_fn = make_quadratic_score_fn(metric_targets),
      weights = c(0.2, 0.3, 0.5)
    ),
    "information_fn"
  )
})

test_that("the M-step reports its Newton step in standard-error units", {
  weights <- c(0.2, 0.3, 0.5)
  seen <- new_metric_seen()
  start <- c(0, 0)

  res <- optimize_abem_sgd(
    theta = start,
    score_fn = make_metric_score_fn(metric_targets, metric_a, seen),
    information_fn = make_metric_information_fn(metric_targets, metric_a, seen),
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_size = 0.2,
      step_schedule = "constant",
      convergence = "iterations",
      max_iterations = 4L
    )
  )

  expect_contains(
    names(res),
    c(
      "parameters",
      "iterations",
      "converged",
      "start_newton_se",
      "newton_se",
      "rank_deficient"
    )
  )
  expect_length(seen$information_thetas, 1L)
  expect_identical(seen$information_thetas[[1L]], start)
  expect_equal(
    res$start_newton_se,
    newton_se_of(metric_gradient(start, weights), metric_a)
  )
  # The final statistic is read at the last iterate a gradient was taken at.
  last <- seen$thetas[[length(seen$thetas)]]
  expect_equal(
    res$newton_se,
    newton_se_of(metric_gradient(last, weights), metric_a)
  )
  expect_false(res$rank_deficient)
})

test_that("a fixed mask tests the free block of the information alone", {
  weights <- c(0.2, 0.3, 0.5)
  start <- c(0.5, -0.5)

  res <- optimize_abem_sgd(
    theta = start,
    score_fn = make_metric_score_fn(metric_targets, metric_a),
    information_fn = make_metric_information_fn(metric_targets, metric_a),
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_size = 0.2,
      step_schedule = "constant",
      convergence = "iterations",
      max_iterations = 1L
    ),
    fixed = 1L
  )

  gradient <- metric_gradient(start, weights)
  free_block <- newton_se_of(gradient[2], metric_a[2, 2, drop = FALSE])
  # Not the full inverse with the fixed coordinate's entry zeroed, which
  # leaks the cross term into the free coordinate.
  leaked <- abs(solve(metric_a)[2, 2] * gradient[2]) /
    sqrt(solve(metric_a)[2, 2])
  expect_false(isTRUE(all.equal(free_block, leaked)))
  expect_equal(res$start_newton_se, free_block)
})

test_that("the first full-variant gradient reuses the request's scores", {
  weights <- c(0.2, 0.3, 0.5)
  multiset <- c(1L, 3L, 3L, 2L, 3L)
  start <- c(0.4, -0.3)
  one_step <- function(use, resample_indices = NULL) {
    seen <- new_metric_seen()
    res <- optimize_abem_sgd(
      theta = start,
      score_fn = make_metric_score_fn(metric_targets, metric_a, seen),
      information_fn = make_metric_information_fn(metric_targets, metric_a),
      weights = weights,
      control = set_sgd_options(
        variant = "full",
        step_size = 0.2,
        step_schedule = "constant",
        convergence = "iterations",
        max_iterations = 1L
      ),
      use = use,
      resample_indices = resample_indices
    )
    list(res = res, seen = seen)
  }
  fresh_scores <- make_metric_score_fn(metric_targets, metric_a)(start, 1:3)

  importance <- one_step("importance")
  expect_length(importance$seen$thetas, 0L)
  expect_equal(
    importance$res$parameters,
    start + 0.2 * colSums(weights * fresh_scores)
  )

  # Duplicates repeat their row; every member weighs 1/m.
  resampling <- one_step("resampling", multiset)
  expect_length(resampling$seen$thetas, 0L)
  expect_equal(
    resampling$res$parameters,
    start + 0.2 * colMeans(fresh_scores[multiset, , drop = FALSE])
  )
})

test_that("the minibatch variant ignores the request's scores", {
  withr::local_seed(5L)
  weights <- c(0.2, 0.3, 0.5)
  run <- function(information_fn) {
    withr::with_seed(9L, {
      optimize_abem_sgd(
        theta = c(0, 0),
        score_fn = make_metric_score_fn(metric_targets, metric_a),
        information_fn = information_fn,
        weights = weights,
        control = set_sgd_options(
          variant = "minibatch",
          batch_size = 2L,
          step_size = 0.1,
          step_schedule = "constant",
          convergence = "iterations",
          max_iterations = 5L
        )
      )
    })
  }
  honest <- make_metric_information_fn(metric_targets, metric_a)
  scrambled <- function(theta) {
    out <- honest(theta)
    out$scores[] <- 1e6
    out
  }

  expect_identical(run(scrambled)$parameters, run(honest)$parameters)
})
