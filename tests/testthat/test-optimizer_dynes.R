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
  expect_lt(res$newton_se, 1e-8)
})

test_that("constant, adagrad and adam ascend the stub with step_size and max_iterations unset", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  optimum <- colSums(weights * targets)
  start <- c(0, 0)

  for (schedule in c("constant", "adagrad", "adam")) {
    # The first-order schedules reach their cap here; the cap warning is the
    # expected diagnostic, not what this test checks.
    res <- suppressWarnings(optimize_abem_sgd(
      theta = start,
      score_fn = make_quadratic_score_fn(targets),
      information_fn = make_quadratic_information_fn(targets),
      weights = weights,
      control = set_sgd_options(variant = "full", step_schedule = schedule)
    ))
    expect_lt(
      sqrt(sum((res$parameters - optimum)^2)),
      sqrt(sum((start - optimum)^2)),
      label = schedule
    )
  }
})

test_that("a direction without information is not stepped along", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  optimum <- colSums(weights * targets)
  start <- c(1, -1)
  # Information of rank one: the combination theta1 - theta2 has none, while
  # the scores still carry a gradient along it.
  range_direction <- c(1, 1) / sqrt(2)
  null_direction <- c(1, -1) / sqrt(2)
  information_fn <- function(theta) {
    list(
      information = 2 * tcrossprod(range_direction),
      scores = sweep(targets, 2, theta, "-")
    )
  }

  res <- optimize_abem_sgd(
    theta = start,
    score_fn = make_quadratic_score_fn(targets),
    information_fn = information_fn,
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_size = 0.5,
      step_schedule = "constant",
      sgd_tolerance = 1e-6,
      max_iterations = 500L
    )
  )

  expected <- range_direction *
    sum(range_direction * optimum) +
    null_direction * sum(null_direction * start)
  expect_equal(res$parameters, expected, tolerance = 1e-5)
  expect_equal(
    sum(null_direction * res$parameters),
    sum(null_direction * start)
  )
  expect_true(res$converged)
  expect_true(res$rank_deficient)
  expect_lt(res$newton_se, 1e-6)
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

test_that("hitting the iteration cap with the stop criterion unmet warns", {
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
    "sgd_tolerance"
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

# The stop rule tests the Newton step in standard-error units against
# `sgd_tolerance`, at the iterate the gradient was taken at, before stepping.
# The scaled stub below is the per-sequence objective of `scale` times the
# events: L_i(theta) = -scale/2 (theta - mu_i)^2 with mu_i = center +
# offsets_i / sqrt(scale), so the score is scale (mu_i - theta), the
# information scale, and a standard error 1 / sqrt(scale).
scaled_stub <- function(scale, offsets, center = 0) {
  mu <- center + offsets / sqrt(scale)
  list(
    mu = mu,
    score_fn = function(theta, indices) {
      matrix(scale * (mu[indices] - theta), ncol = 1L)
    },
    information_fn = function(theta) {
      list(
        information = matrix(scale),
        scores = matrix(scale * (mu - theta), ncol = 1L)
      )
    }
  )
}

test_that("the stop rule reads the same distance whatever the score's scale", {
  offsets <- c(-1.2, 0.3, 0.9, 1.6)
  weights <- c(0.1, 0.2, 0.3, 0.4)
  run <- function(scale, variant) {
    stub <- scaled_stub(scale, offsets)
    optimum <- sum(weights * stub$mu)
    withr::with_seed(3L, {
      optimize_abem_sgd(
        # Three standard errors from the optimum, and a step that contracts
        # the distance in standard-error units by the same factor at either
        # scale.
        theta = optimum + 3 / sqrt(scale),
        score_fn = stub$score_fn,
        information_fn = stub$information_fn,
        weights = weights,
        control = set_sgd_options(
          variant = variant,
          batch_size = 2L,
          step_schedule = "constant",
          step_size = 0.3 / scale,
          sgd_tolerance = 0.05,
          max_iterations = 200L
        )
      )
    })
  }

  for (variant in c("full", "minibatch")) {
    small <- run(1, variant)
    large <- run(1000, variant)
    expect_true(small$converged, info = variant)
    expect_identical(large$converged, small$converged, info = variant)
    expect_identical(large$iterations, small$iterations, info = variant)
    expect_equal(large$newton_se, small$newton_se, tolerance = 1e-6)
  }
})

test_that("a minibatch M-step started at the optimum stops under its noise", {
  withr::local_seed(11L)
  scale <- 1000
  p <- 4L
  mu <- matrix(stats::rnorm(40 * p, sd = 0.5), ncol = p) / sqrt(scale)
  weights <- rep(1 / 40, 40)
  optimum <- colSums(weights * mu)
  score_fn <- function(theta, indices) {
    scale * sweep(mu[indices, , drop = FALSE], 2, theta, "-")
  }

  res <- optimize_abem_sgd(
    theta = optimum,
    score_fn = score_fn,
    information_fn = function(theta) {
      list(information = scale * diag(p), scores = score_fn(theta, 1:40))
    },
    weights = weights,
    control = set_sgd_options(
      variant = "minibatch",
      batch_size = 10L,
      step_schedule = "constant",
      step_size = 0.05 / scale,
      sgd_tolerance = 0.1,
      max_iterations = 100L
    )
  )

  # The smoothed batch-gradient noise is a few hundredths of a standard error
  # per coordinate here, but its raw norm is sqrt(scale) times that, never
  # under a raw tolerance of 0.1.
  expect_true(res$converged)
  expect_lt(res$iterations, 100L)
})

test_that("a start already within tolerance is returned unchanged", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4), ncol = 2, byrow = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  start <- colSums(weights * targets) + c(0.01, -0.01)
  score_fn <- make_quadratic_score_fn(targets)

  for (schedule in c("constant", "decay", "adagrad", "adam", "momentum")) {
    for (variant in c("full", "minibatch")) {
      res <- optimize_abem_sgd(
        theta = start,
        score_fn = score_fn,
        information_fn = make_quadratic_information_fn(targets),
        weights = weights,
        control = set_sgd_options(
          variant = variant,
          batch_size = 3L,
          batch_scheme = "cyclic",
          step_schedule = schedule,
          step_size = 0.1,
          sgd_tolerance = 0.05
        )
      )
      info <- paste(schedule, variant)
      expect_identical(res$parameters, start, info = info)
      expect_true(res$converged, info = info)
      expect_identical(res$iterations, 1L, info = info)
    }
  }
})

# The Fisher-scoring schedule steps by step_size * P^-1 g, with P the
# information held from the M-step's single request at its start.
test_that("a Fisher-scoring step is step_size times the held P^-1 g", {
  weights <- c(0.2, 0.3, 0.5)
  start <- c(0.4, -0.3)

  res <- optimize_abem_sgd(
    theta = start,
    score_fn = make_metric_score_fn(metric_targets, metric_a),
    information_fn = make_metric_information_fn(metric_targets, metric_a),
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_schedule = "fisher_scoring",
      step_size = 0.5,
      convergence = "iterations",
      max_iterations = 1L
    )
  )

  expect_equal(
    res$parameters,
    start + 0.5 * drop(solve(metric_a, metric_gradient(start, weights)))
  )
})

test_that("a full Fisher-scoring step of size 1 reaches a quadratic's optimum at once", {
  weights <- c(0.2, 0.3, 0.5)
  optimum <- colSums(weights * metric_targets)

  res <- optimize_abem_sgd(
    theta = c(0, 0),
    score_fn = make_metric_score_fn(metric_targets, metric_a),
    information_fn = make_metric_information_fn(metric_targets, metric_a),
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_schedule = "fisher_scoring",
      step_size = 1
    )
  )

  expect_equal(res$parameters, optimum, tolerance = 1e-10)
  expect_true(res$converged)
  # One step, then the test at the optimum.
  expect_identical(res$iterations, 2L)
})

test_that("a Fisher-scoring step on a fixed mask solves the free block alone", {
  weights <- c(0.2, 0.3, 0.5)
  start <- c(0.5, -0.5)

  res <- optimize_abem_sgd(
    theta = start,
    score_fn = make_metric_score_fn(metric_targets, metric_a),
    information_fn = make_metric_information_fn(metric_targets, metric_a),
    weights = weights,
    control = set_sgd_options(
      variant = "full",
      step_schedule = "fisher_scoring",
      step_size = 1,
      convergence = "iterations",
      max_iterations = 1L
    ),
    fixed = 1L
  )

  gradient <- metric_gradient(start, weights)
  expect_identical(res$parameters[1], start[1])
  # Not the full inverse's second row, which leaks the cross term.
  expect_equal(res$parameters[2], start[2] + gradient[2] / metric_a[2, 2])
})

test_that("a Fisher-scoring trajectory in standard errors ignores the score's scale", {
  offsets <- c(-1.2, 0.3, 0.9, 1.6)
  weights <- c(0.1, 0.2, 0.3, 0.4)
  trajectory <- function(scale) {
    stub <- scaled_stub(scale, offsets)
    optimum <- sum(weights * stub$mu)
    seen <- list()
    score_fn <- function(theta, indices) {
      seen[[length(seen) + 1L]] <<- theta
      stub$score_fn(theta, indices)
    }
    res <- optimize_abem_sgd(
      theta = optimum + 3 / sqrt(scale),
      score_fn = score_fn,
      information_fn = stub$information_fn,
      weights = weights,
      control = set_sgd_options(
        variant = "full",
        step_schedule = "fisher_scoring",
        step_size = 0.5
      )
    )
    list(
      res = res,
      se_units = (c(unlist(seen), res$parameters) - optimum) * sqrt(scale)
    )
  }

  small <- trajectory(1)
  large <- trajectory(1000)
  expect_gt(length(small$se_units), 2L)
  expect_equal(large$se_units, small$se_units, tolerance = 1e-8)
  expect_identical(large$res$iterations, small$res$iterations)
  expect_identical(large$res$converged, small$res$converged)
})

test_that("a minibatch Fisher-scoring step reads the bias-corrected EMA", {
  targets <- matrix(c(1, -2, 3, 0, -1, 4, 2, 1), ncol = 2, byrow = TRUE)
  weights <- rep(0.25, 4)
  start <- c(0, 0)
  step_size <- 0.5

  res <- optimize_abem_sgd(
    theta = start,
    score_fn = make_metric_score_fn(targets, metric_a),
    information_fn = make_metric_information_fn(targets, metric_a),
    weights = weights,
    control = set_sgd_options(
      variant = "minibatch",
      batch_size = 2L,
      batch_scheme = "cyclic",
      step_schedule = "fisher_scoring",
      step_size = step_size,
      convergence = "iterations",
      max_iterations = 2L
    )
  )

  # Cyclic batches {1, 2} then {3, 4}; uniform weights make each batch
  # gradient the batch mean.
  batch_gradient <- function(theta, rows) {
    colMeans(sweep(targets[rows, , drop = FALSE], 2, theta, "-") %*% metric_a)
  }
  g1 <- batch_gradient(start, 1:2)
  theta1 <- start + step_size * drop(solve(metric_a, g1))
  g2 <- batch_gradient(theta1, 3:4)
  ema <- 0.9 * (0.1 * g1) + 0.1 * g2
  theta2 <- theta1 + step_size * drop(solve(metric_a, ema / (1 - 0.9^2)))
  expect_equal(res$parameters, theta2)
})

test_that("every schedule and variant pair is selectable and ascends the stub", {
  withr::local_seed(17L)
  weights <- c(0.2, 0.3, 0.5)
  optimum <- colSums(weights * metric_targets)
  start <- c(0, 0)
  schedules <- c(
    "fisher_scoring",
    "constant",
    "decay",
    "adagrad",
    "adam",
    "momentum"
  )

  for (schedule in schedules) {
    for (variant in c("full", "minibatch")) {
      control <- set_sgd_options(
        variant = variant,
        batch_size = 3L,
        step_schedule = schedule,
        convergence = "iterations",
        max_iterations = 50L
      )
      expect_identical(control$step_schedule, schedule)
      expect_identical(control$variant, variant)
      res <- optimize_abem_sgd(
        theta = start,
        score_fn = make_metric_score_fn(metric_targets, metric_a),
        information_fn = make_metric_information_fn(metric_targets, metric_a),
        weights = weights,
        control = control
      )
      expect_lt(
        sqrt(sum((res$parameters - optimum)^2)),
        sqrt(sum((start - optimum)^2)),
        label = paste(schedule, variant)
      )
    }
  }
})

# The damping of the full Fisher-scoring step. A rate-like stub whose
# information grows like exp(theta): L_i(theta) = n (y_i theta - exp(theta)),
# score n (y_i - exp(theta)), information n exp(theta), with the weighted
# target mean 1 so the optimum is theta = 0. From 1.5 below it, P held at the
# start is exp(1.5) times too small at the optimum, so a step of size 1
# overshoots and the statistic rises.
exp_stub <- function(n = 100, seen = NULL) {
  targets <- c(0.4, 0.8, 1.2, 1.6)
  score_fn <- function(theta, indices) {
    if (!is.null(seen)) {
      seen$thetas[[length(seen$thetas) + 1L]] <- theta
    }
    matrix(n * (targets[indices] - exp(theta)), ncol = 1L)
  }
  list(
    weights = rep(0.25, 4),
    score_fn = score_fn,
    information_fn = function(theta) {
      list(
        information = matrix(n * exp(theta)),
        scores = score_fn(theta, seq_along(targets))
      )
    },
    targets = targets,
    n = n
  )
}

# The expected damped trajectory, replayed by hand: the statistic at each
# iterate, a halving whenever it rises over the previous one, and the step
# taken from the current iterate.
exp_stub_trajectory <- function(stub, start, tolerance, cap) {
  information <- stub$n * exp(start)
  theta <- start
  thetas <- numeric(0)
  step_size <- 1
  previous <- Inf
  halvings <- 0L
  for (iteration in seq_len(cap)) {
    thetas <- c(thetas, theta)
    gradient <- sum(stub$weights * stub$n * (stub$targets - exp(theta)))
    statistic <- abs(gradient) / sqrt(information)
    if (statistic < tolerance) {
      break
    }
    if (statistic > previous) {
      step_size <- step_size / 2
      halvings <- halvings + 1L
    }
    previous <- statistic
    theta <- theta + step_size * gradient / information
  }
  list(thetas = thetas, halvings = halvings)
}

test_that("a rising Fisher-scoring statistic halves the full variant's step", {
  seen <- new_metric_seen()
  stub <- exp_stub(seen = seen)
  expect_equal(sum(stub$weights * stub$targets), 1)
  start <- -1.5

  res <- optimize_abem_sgd(
    theta = start,
    score_fn = stub$score_fn,
    information_fn = stub$information_fn,
    weights = stub$weights,
    control = set_sgd_options(
      variant = "full",
      step_schedule = "fisher_scoring"
    )
  )

  expected <- exp_stub_trajectory(stub, start, tolerance = 0.05, cap = 20L)
  expect_true(res$converged)
  expect_lte(res$iterations, 20L)
  expect_identical(res$damped, expected$halvings)
  expect_gte(res$damped, 2L)
  # The first iterate is the start, where the information request evaluates
  # the scores; every later one continues from the iterate before it, never
  # backtracking.
  expect_equal(unlist(seen$thetas), expected$thetas)
  expect_equal(res$parameters, 0, tolerance = 0.01)
})

test_that("an undamped Fisher-scoring step diverges from the same start", {
  stub <- exp_stub()
  local_mocked_bindings(mstep_damps = function(control) FALSE)

  res <- suppressWarnings(optimize_abem_sgd(
    theta = -1.5,
    score_fn = stub$score_fn,
    information_fn = stub$information_fn,
    weights = stub$weights,
    control = set_sgd_options(
      variant = "full",
      step_schedule = "fisher_scoring"
    )
  ))

  expect_false(res$converged)
  expect_identical(res$iterations, 20L)
  expect_identical(res$damped, 0L)
})

test_that("only the full Fisher-scoring step is damped", {
  withr::local_seed(23L)
  stub <- exp_stub()
  schedules <- c(
    "fisher_scoring",
    "constant",
    "decay",
    "adagrad",
    "adam",
    "momentum"
  )

  for (schedule in schedules) {
    for (variant in c("full", "minibatch")) {
      if (schedule == "fisher_scoring" && variant == "full") {
        next
      }
      res <- suppressWarnings(optimize_abem_sgd(
        theta = -1.5,
        score_fn = stub$score_fn,
        information_fn = stub$information_fn,
        weights = stub$weights,
        control = set_sgd_options(
          variant = variant,
          batch_size = 2L,
          step_schedule = schedule
        )
      ))
      expect_identical(res$damped, 0L, info = paste(schedule, variant))
    }
  }
})

# The default M-step on a pool carrying the information of 10,000 events, its
# standard error 0.01.
test_that("the default does not overshoot a large summed score", {
  scale <- 10000
  offsets <- c(-1.2, 0.3, 0.9, 1.6)
  weights <- c(0.1, 0.2, 0.3, 0.4)
  stub <- scaled_stub(scale, offsets)
  optimum <- sum(weights * stub$mu)
  se <- 1 / sqrt(scale)
  visited <- numeric(0)
  score_fn <- function(theta, indices) {
    visited <<- c(visited, theta)
    stub$score_fn(theta, indices)
  }

  withr::local_seed(29L)
  res <- optimize_abem_sgd(
    theta = optimum - se,
    score_fn = score_fn,
    information_fn = stub$information_fn,
    weights = weights
  )

  expect_true(res$converged)
  expect_lt(abs(res$parameters - optimum) / se, set_sgd_options()$sgd_tolerance)
  expect_true(all(c(visited, res$parameters) - optimum <= se))
})

test_that("a default M-step started at the optimum stays there", {
  scale <- 10000
  offsets <- c(-1.2, 0.3, 0.9, 1.6)
  weights <- c(0.1, 0.2, 0.3, 0.4)
  stub <- scaled_stub(scale, offsets)
  optimum <- sum(weights * stub$mu)

  withr::local_seed(31L)
  res <- optimize_abem_sgd(
    theta = optimum,
    score_fn = stub$score_fn,
    information_fn = stub$information_fn,
    weights = weights
  )

  expect_true(res$converged)
  expect_lt(
    abs(res$parameters - optimum) * sqrt(scale),
    set_sgd_options()$sgd_tolerance
  )
})

test_that("the default stop rule reads the same distance whatever the score's scale", {
  offsets <- c(-1.2, 0.3, 0.9, 1.6)
  weights <- c(0.1, 0.2, 0.3, 0.4)
  run <- function(scale) {
    stub <- scaled_stub(scale, offsets)
    withr::with_seed(37L, {
      optimize_abem_sgd(
        theta = sum(weights * stub$mu) + 3 / sqrt(scale),
        score_fn = stub$score_fn,
        information_fn = stub$information_fn,
        weights = weights
      )
    })
  }

  small <- run(1)
  large <- run(1000)
  expect_true(small$converged)
  expect_identical(large$converged, small$converged)
  expect_equal(large$newton_se, small$newton_se, tolerance = 1e-6)
})
