# One evaluation pass at a supplied parameter vector. The evaluator runs the
# same engine estimation iterates -- the point of the shared closure -- so at
# the maximum it must reproduce the fit's own numbers, and away from it must
# report the full model's score and information rather than the estimated
# model's zeros.

evaluate_fixture <- function(backend = "cpp", sub_model = "choice") {
  data("social_evolution", envir = environment())
  formula <- if (identical(sub_model, "choice")) {
    calls ~ inertia + recip + trans
  } else {
    calls ~ 1 + indeg + outdeg
  }
  suppressMessages(estimate_dynam(
    formula,
    sub_model = sub_model,
    data = social_evolution,
    return_preprocessed = TRUE,
    control_algo = set_algorithm_newton(backend = backend)
  ))
}

test_that("evaluation at the MLE reproduces the fit", {
  fit <- evaluate_fixture()
  out <- evaluate_model(fit, return = c("loglik", "score", "information"))

  expect_equal(out$loglik, as.numeric(stats::logLik(fit)), tolerance = 1e-10)
  # Free coefficients are at a maximum, so their score is at the convergence
  # tolerance, not merely small.
  expect_lt(max(abs(out$score)), 1e-4)
  # Named by the coefficient labels the model reports, the same ones `at`
  # matches against.
  expect_named(out$score, names(stats::coef(fit)))
  expect_equal(dim(out$information), dim(fit$final_information_matrix))
  expect_equal(out$information, fit$final_information_matrix, tolerance = 1e-10)
})

test_that("evaluation at a constrained vector reports the full model", {
  fit <- evaluate_fixture()
  constrained <- stats::coef(fit)
  constrained[3] <- 0
  out <- evaluate_model(
    fit,
    at = constrained,
    return = c("loglik", "score", "information")
  )

  # Full dimensions: the constrained coefficient's own score is what a score
  # test reads, so it is reported rather than zeroed.
  expect_length(out$score, length(constrained))
  expect_equal(dim(out$information), rep(length(constrained), 2))
  expect_gt(abs(out$score[[3]]), 1e-6)
  expect_lt(out$loglik, as.numeric(stats::logLik(fit)))
  expect_equal(out$at, unname(constrained))
})

test_that("a named `at` seeds on top of the fitted coefficients", {
  fit <- evaluate_fixture()
  named <- evaluate_model(fit, at = c(trans = 0), return = "loglik")
  positional <- stats::coef(fit)
  positional[["trans"]] <- 0
  expect_equal(
    named$loglik,
    evaluate_model(fit, at = positional, return = "loglik")$loglik
  )
})

test_that("the evaluation runs on the fit's backend and reproduces it", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  for (backend in c("cpp", "r", "gather")) {
    fit <- evaluate_fixture(backend, sub_model = "rate")
    out <- evaluate_model(
      fit,
      return = c("loglik", "interval_loglik", "total_rate", "margins")
    )
    expect_equal(out$backend, backend)
    expect_equal(
      out$interval_loglik,
      fit$interval_log_lik,
      tolerance = 1e-10,
      info = backend
    )
    expect_equal(
      out$total_rate,
      fit$total_rate,
      tolerance = 1e-10,
      info = backend
    )
    # Margins are labeled and scale-marked exactly as stored ones are.
    expect_named(
      out$margins,
      c("observed", "expected", "expected_probability"),
      info = backend
    )
    expect_equal(
      attr(out$margins$expected, "scale"),
      "expected_count",
      info = backend
    )
    expect_false(is.null(names(out$margins$observed)))
  }
})

test_that("ranks come back without materializing a probability matrix", {
  fit <- evaluate_fixture()
  out <- evaluate_model(fit, return = "ranks")
  expect_named(out, c("ranks", "backend", "at"))
  expect_type(out$ranks, "integer")
  expect_length(out$ranks, fit$n_events)
})

test_that("evaluated ranks equal the ranks enumerated from probabilities", {
  fit <- evaluate_fixture()
  out <- evaluate_model(fit, return = c("ranks", "probabilities"))
  observed <- fit$interval_log_lik
  enumerated <- vapply(
    seq_along(out$probabilities),
    function(i) {
      1L + sum(out$probabilities[[i]] > exp(observed[i]) * (1 + 1e-12))
    },
    integer(1)
  )
  expect_equal(out$ranks, enumerated)
})

test_that("recall is the share of events ranked within each threshold", {
  fit <- evaluate_fixture()
  out <- evaluate_model(fit, return = c("ranks", "recall"), recall_at = c(1, 5))
  expect_named(out$recall, c("recall_at_1", "recall_at_5"))
  expect_equal(out$recall[["recall_at_1"]], mean(out$ranks <= 1))
  expect_lte(out$recall[["recall_at_1"]], out$recall[["recall_at_5"]])
})

test_that("the returned list carries exactly the requested quantities", {
  fit <- evaluate_fixture()
  out <- evaluate_model(fit, return = c("score", "loglik"))
  # Vocabulary order, not request order, plus the two provenance components.
  expect_named(out, c("loglik", "score", "backend", "at"))
})

test_that("evaluate_model rejects what it cannot evaluate", {
  fit <- evaluate_fixture()
  expect_snapshot(evaluate_model(fit, return = "residuals"), error = TRUE)
  expect_snapshot(evaluate_model(fit, at = c(1, 2)), error = TRUE)
  expect_snapshot(evaluate_model(fit, at = c(nonesuch = 1)), error = TRUE)
  expect_snapshot(evaluate_model(1:3), error = TRUE)
})

test_that("evaluation needs statistics from the fit or from the caller", {
  data("social_evolution", envir = environment())
  fit <- estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution
  )
  expect_snapshot(evaluate_model(fit, return = "loglik"), error = TRUE)

  supplied <- compute_statistics(
    calls ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = social_evolution
  )
  out <- evaluate_model(fit, return = "loglik", preprocessed = supplied)
  expect_equal(out$loglik, as.numeric(stats::logLik(fit)), tolerance = 1e-10)
})

# Weighted per-interval information (task 4.5). The accumulation happens beside
# the running Fisher inside the one pass, so the acceptance criterion is that a
# column of ones reproduces the total: the weighted sum and the total must be
# formed from the same per-interval block, or every downstream statistic is
# weighting something other than what it reports.

test_that("a ones column reproduces the information matrix", {
  fit <- evaluate_fixture()
  n <- length(fit$preprocessed$is_dependent)
  out <- evaluate_model(
    fit,
    return = c("information", "weighted_information"),
    weights = matrix(1, n, 1, dimnames = list(NULL, "all"))
  )

  expect_equal(dim(out$weighted_information), c(3L, 3L, 1L))
  expect_equal(dimnames(out$weighted_information)[[3]], "all")
  expect_equal(out$weighted_information[,, "all"], out$information)
})

test_that("an arbitrary column reproduces the hand-computed weighted sum", {
  fit <- evaluate_fixture()
  n <- length(fit$preprocessed$is_dependent)
  # Two columns whose sum is the ones column: their two slices must therefore
  # add back to the total, which pins the weighting per interval rather than
  # only in aggregate.
  ramp <- seq_len(n) / (n + 1)
  weights <- cbind(ramp = ramp, complement = 1 - ramp)
  out <- evaluate_model(
    fit,
    return = c("information", "weighted_information"),
    weights = weights
  )

  expect_equal(dimnames(out$weighted_information)[[3]], c("ramp", "complement"))
  expect_equal(
    out$weighted_information[,, "ramp"] +
      out$weighted_information[,, "complement"],
    out$information
  )
  # The ramp slice is not the total scaled by a constant, so the test is not
  # satisfied by a kernel that ignored the weights.
  expect_false(isTRUE(all.equal(
    out$weighted_information[,, "ramp"],
    out$information * mean(ramp)
  )))
})

test_that("disjoint indicators give the per-group information blocks", {
  fit <- evaluate_fixture()
  n <- length(fit$preprocessed$is_dependent)
  cut <- floor(n / 2)
  groups <- cbind(
    early = as.numeric(seq_len(n) <= cut),
    late = as.numeric(seq_len(n) > cut)
  )
  out <- evaluate_model(
    fit,
    return = c("information", "weighted_information"),
    weights = groups
  )

  expect_equal(
    out$weighted_information[,, "early"] + out$weighted_information[,, "late"],
    out$information
  )
  # A group's block is the information of that segment alone, which the
  # ones-column identity on a truncated weight matrix cannot fake.
  expect_false(isTRUE(all.equal(
    out$weighted_information[,, "early"],
    out$weighted_information[,, "late"]
  )))
})

test_that("the per-interval trace sums to the trace of the information", {
  fit <- evaluate_fixture()
  out <- evaluate_model(
    fit,
    return = c("information", "event_information_trace")
  )

  expect_length(out$event_information_trace, length(fit$interval_log_lik))
  expect_equal(
    sum(out$event_information_trace),
    sum(diag(out$information))
  )
  # Every interval contributes a positive semi-definite block.
  expect_true(all(out$event_information_trace >= 0))
})

test_that("weighted information agrees across the three backends", {
  fit <- evaluate_fixture()
  n <- length(fit$preprocessed$is_dependent)
  weights <- cbind(all = rep(1, n), ramp = seq_len(n) / n)
  quantities <- c("weighted_information", "event_information_trace")

  reference <- evaluate_model(
    fit,
    return = quantities,
    weights = weights,
    backend = "cpp"
  )
  for (backend in c("r", "gather")) {
    other <- evaluate_model(
      fit,
      return = quantities,
      weights = weights,
      backend = backend
    )
    expect_equal(
      other$weighted_information,
      reference$weighted_information,
      tolerance = 1e-10
    )
    expect_equal(
      other$event_information_trace,
      reference$event_information_trace,
      tolerance = 1e-10
    )
  }
})

test_that("an exact-time fit weights the compensator-scaled block", {
  # The rate and REM families scale the per-interval block by the timespan
  # before it reaches the running Fisher, and a right-censored interval carries
  # a block of its own -- so the ones-column identity is what checks that the
  # weighting saw the same scaled block, over every interval and not only the
  # dependent ones. The exogenous layer is what opens those intervals.
  fit <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    return_preprocessed = TRUE
  )
  n <- length(fit$preprocessed$is_dependent)
  expect_gt(n, sum(fit$preprocessed$is_dependent))

  out <- evaluate_model(
    fit,
    return = c(
      "information",
      "weighted_information",
      "event_information_trace"
    ),
    weights = matrix(1, n, 1, dimnames = list(NULL, "all"))
  )
  expect_equal(out$weighted_information[,, "all"], out$information)
  expect_equal(sum(out$event_information_trace), sum(diag(out$information)))
})

test_that("weighted information rejects a missing or misshapen weight matrix", {
  fit <- evaluate_fixture()
  expect_snapshot(
    evaluate_model(fit, return = "weighted_information"),
    error = TRUE
  )
  expect_snapshot(
    evaluate_model(
      fit,
      return = "weighted_information",
      weights = matrix(1, 5, 1)
    ),
    error = TRUE
  )
  expect_snapshot(
    evaluate_model(
      fit,
      return = "weighted_information",
      weights = matrix("a", length(fit$preprocessed$is_dependent), 1)
    ),
    error = TRUE
  )
})
