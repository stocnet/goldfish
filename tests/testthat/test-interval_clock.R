# The interval clock a fitted model carries: `intervals`, `start_time` and
# `end_time`, beside the event times and the right-censoring indicator it
# already had. Not a diagnostic -- it is the clock the likelihood itself ran
# on -- so it rides on every fit regardless of what was requested.

fit_clock <- function(formula = depNetwork ~ 1 + indeg + outdeg, ...) {
  estimate_wrapper(
    formula,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    ...
  )
}

test_that("the clock is present without the replay object", {
  fit <- fit_clock(control_algo = set_algorithm_newton(diagnostics = "loglik"))

  expect_null(fit$preprocessed)
  expect_type(fit$intervals, "double")
  expect_length(fit$intervals, length(fit$interval_log_lik))
  expect_length(fit$start_time, 1L)
  expect_length(fit$end_time, 1L)
  # Even with no diagnostics at all: the clock is not one.
  bare <- fit_clock(control_algo = set_algorithm_newton(diagnostics = FALSE))
  expect_length(bare$intervals, length(bare$right_censored_events))
})

test_that("the clock spans the observation window", {
  fit <- fit_clock()

  expect_equal(fit$start_time + sum(fit$intervals), fit$end_time)
  expect_true(all(fit$event_time >= fit$start_time))
  expect_true(all(fit$event_time <= fit$end_time))
  # Accumulating from the start reaches each event's own time, which is what
  # lets a per-event quantity be placed on the observed axis directly.
  expect_equal(fit$start_time + cumsum(fit$intervals), fit$event_time)
})

test_that("Cox-Snell residuals need no replay object", {
  fit <- fit_clock(control_algo = set_algorithm_newton(diagnostics = "loglik"))
  compensator <- fit$intervals * fit$total_rate

  expect_null(fit$preprocessed)
  expect_length(compensator, length(fit$interval_log_lik))
  # At the maximum the compensators sum to the dependent-event count: that is
  # the time intercept's own score equation.
  expect_equal(
    sum(compensator),
    sum(!fit$right_censored_events),
    tolerance = 1e-4
  )
  # On a right-censored interval the compensator IS the whole likelihood
  # contribution, so the identity there is exact rather than asymptotic.
  censored <- fit$right_censored_events
  expect_equal(
    compensator[censored],
    -fit$interval_log_lik[censored]
  )
})

test_that("the dependence indicator has one spelling", {
  fit <- fit_clock(return_preprocessed = TRUE)

  expect_type(fit$right_censored_events, "logical")
  expect_null(fit$is_dependent)
  # The preprocessed object carries the negation under its own name; the two
  # are documented against each other rather than duplicated onto the fit.
  expect_equal(
    fit$right_censored_events,
    fit$preprocessed$is_dependent == 0L
  )
})

test_that("adding the clock does not move the format epoch", {
  fit <- fit_clock()

  # The epoch names a *released* layout and moves once per release whose layout
  # differs; it must not move for a component added inside a development line,
  # or every dev-line fit would be refused for a purely additive change.
  expect_identical(fit$fit_version, FIT_VERSION)
  expect_identical(FIT_VERSION, 2L)

  # An object that predates the clock is still current in stamp, and the
  # surfaces that do not read the clock still accept it.
  older <- fit
  older$intervals <- NULL
  older$start_time <- NULL
  older$end_time <- NULL
  expect_false(abort_if_stale_result(older, "a log-likelihood"))
  expect_equal(as.numeric(logLik(older)), as.numeric(logLik(fit)))
})

test_that("a consumer of a missing component says what it needs", {
  fit <- fit_clock()
  older <- fit
  older$intervals <- NULL

  expect_equal(
    fit_component(fit, "intervals", "This diagnostic"),
    fit$intervals
  )
  expect_snapshot(
    error = TRUE,
    fit_component(older, "intervals", "The Cox-Snell residual")
  )
})
