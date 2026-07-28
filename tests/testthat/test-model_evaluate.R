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
