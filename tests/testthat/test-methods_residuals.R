# Residuals from stored primitives. Every type here is arithmetic on what
# estimation already stored, so each test pins the identity rather than a
# reference value: the point is that no type quietly re-evaluates the model,
# and that the ones with an algebraic property at the maximum have it.

residual_fixture <- function(sub_model = "choice", ...) {
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
    ...
  ))
}

test_that("deviance residuals are minus twice the stored loglik", {
  fit <- residual_fixture()
  expect_equal(residuals(fit), -2 * fit$interval_log_lik)
  expect_length(residuals(fit), fit$n_events)
})

test_that("schoenfeld residuals are the score rows on a multinomial fit", {
  fit <- residual_fixture()
  schoenfeld <- residuals(fit, type = "schoenfeld")
  expect_identical(schoenfeld, residuals(fit, type = "score"))
  expect_identical(schoenfeld, fit$event_scores)
  # Summing the rows reproduces the gradient, which convergence drove to zero.
  expect_lt(max(abs(colSums(schoenfeld))), 1e-4)
  expect_equal(
    colSums(schoenfeld),
    fit$final_score,
    ignore_attr = TRUE,
    tolerance = 1e-8
  )
})

test_that("exact-time schoenfeld residuals say why they are unavailable", {
  fit <- residual_fixture("rate")
  expect_snapshot(residuals(fit, type = "schoenfeld"), error = TRUE)
  # The score rows the fit does store are offered instead, and exist.
  expect_equal(dim(residuals(fit, type = "score")), dim(fit$event_scores))
})

test_that("influence measures are the one-step deletion formulas", {
  fit <- residual_fixture()
  inverse <- solve(fit$final_information_matrix)
  scores <- fit$event_scores

  expect_equal(
    residuals(fit, type = "dfbeta"),
    scores %*% inverse,
    ignore_attr = TRUE
  )
  expect_equal(
    residuals(fit, type = "dfbetas"),
    sweep(scores %*% inverse, 2, fit$standard_errors, "/"),
    ignore_attr = TRUE
  )
  expect_equal(
    residuals(fit, type = "cooks"),
    rowSums((scores %*% inverse) * scores)
  )
  # Self-influence is a quadratic form in a positive-definite inverse.
  expect_true(all(residuals(fit, type = "cooks") >= 0))
})

test_that("a fixed coefficient carries no influence", {
  data("social_evolution", envir = environment())
  fit <- suppressMessages(estimate_dynam(
    calls ~ inertia + offset(recip, coef = 0.5) + trans,
    sub_model = "choice",
    data = social_evolution
  ))
  is_fixed <- GetFixed(fit)
  expect_true(any(is_fixed))

  dfbeta <- residuals(fit, type = "dfbeta")
  expect_true(all(dfbeta[, is_fixed] == 0))
  expect_false(all(dfbeta[, !is_fixed] == 0))
  # And the quadratic form runs over the estimated block only, so it exists at
  # all despite the fixed column.
  expect_length(residuals(fit, type = "cooks"), fit$n_events)
})

test_that("a type names the primitive it needs when the fit lacks it", {
  fit <- residual_fixture(
    control_algo = set_algorithm_newton(
      diagnostics = "loglik"
    )
  )
  expect_equal(residuals(fit), -2 * fit$interval_log_lik)
  expect_snapshot(residuals(fit, type = "score"), error = TRUE)

  no_loglik <- residual_fixture(
    control_algo = set_algorithm_newton(
      diagnostics = "scores"
    )
  )
  expect_snapshot(residuals(no_loglik, type = "deviance"), error = TRUE)
})

test_that("residuals of a rate fit cover every interval", {
  fit <- residual_fixture("rate")
  # One value per likelihood interval, right-censored ones included: they
  # contribute a timing term and so a deviance and a score.
  expect_length(residuals(fit), fit$n_events)
  expect_false(anyNA(residuals(fit)))
  expect_equal(nrow(residuals(fit, type = "score")), fit$n_events)
})
