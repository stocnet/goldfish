# `augment()`: the modeled events with the model's per-interval quantities
# beside them, in the order the likelihood ran. The ordering is the point of
# half these tests: every per-interval column is in interval order, so a table
# that is not would pair each column with the wrong event.

augment_fixture <- function(
  formula = depNetwork ~ inertia + recip,
  sub_model = "choice",
  model = "DyNAM"
) {
  estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest
  )
}

# A rate process read over an exogenous layer too, so its events open
# right-censored intervals interleaved among the dependent ones.
augment_fixture_censored <- function() {
  augment_fixture(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    sub_model = "rate"
  )
}

test_that("augment carries the broom columns", {
  fit <- augment_fixture()
  augmented <- augment(fit)

  expect_s3_class(augmented, "tbl_df")
  expect_contains(names(augmented), c(".fitted", ".resid"))
  expect_equal(augmented$.fitted, exp(fit$interval_log_lik))
  expect_equal(augmented$.resid, -2 * fit$interval_log_lik)
  # The columns that were there before are unchanged.
  expect_equal(augmented$interval_log_lik, fit$interval_log_lik)
  expect_contains(names(augmented), c("time", "sender", "receiver"))
})

test_that("augment dispatches like the other broom generics", {
  fit <- augment_fixture()

  # Through the re-exported generic, not a bare exported method: the same
  # wiring `tidy()` and `glance()` have.
  expect_equal(augment(fit), generics::augment(fit))
  expect_false("augment.result.goldfish" %in% getNamespaceExports("goldfish"))
  expect_true("augment" %in% getNamespaceExports("goldfish"))
})

test_that("rows are dependent events, and the span count carries the rest", {
  fit <- augment_fixture_censored()
  augmented <- augment(fit)

  # One row per dependent event, not per likelihood interval. The censored
  # intervals are not rows of their own: each belongs to the waiting time of
  # the event that follows it and is accumulated into that event's span.
  expect_gt(sum(fit$right_censored_events), 0)
  expect_equal(nrow(augmented), fit$n_events)
  expect_lt(nrow(augmented), length(fit$interval_log_lik))

  # `n_intervals` is the only place the interval structure stays visible once
  # every other surface is per event, and it reconstructs the fit's own count.
  expect_equal(sum(augmented$n_intervals), fit$n_intervals)
  expect_true(all(augmented$n_intervals >= 1L))

  # The reported log-likelihood is the span's, so it totals the fit's.
  expect_equal(
    sum(augmented$interval_log_lik),
    sum(fit$interval_log_lik)
  )
  # And the table aligns row-for-row with the per-event residual series, which
  # is the pairing this whole change exists to make true.
  expect_equal(nrow(augmented), length(residuals(fit, type = "deviance")))
})

test_that("a censored remainder has no fitted outcome", {
  # A span that closes no waiting time realizes nothing, so it has no fitted
  # probability and no deviance for one. It is the only such row, and it
  # appears only where the observation window outlives the last event.
  fit <- augment_fixture_censored()
  augmented <- augment(fit)
  censored <- augmented$right_censored_event

  expect_lte(sum(censored), 1L)
  if (any(censored)) {
    expect_identical(which(censored), nrow(augmented))
    expect_true(all(is.na(augmented$.fitted[censored])))
    expect_true(all(is.na(augmented$.resid[censored])))
    expect_true(all(is.na(augmented$sender[censored])))
  }
  # Every other row realizes an outcome, so `.fitted` is the span's density
  # contribution and `.resid` its deviance.
  expect_false(anyNA(augmented$.fitted[!censored]))
  expect_equal(
    augmented$.fitted[!censored],
    exp(augmented$interval_log_lik[!censored])
  )
  expect_equal(
    augmented$.resid[!censored],
    -2 * augmented$interval_log_lik[!censored]
  )
})

test_that("the diagnose_* surfaces read the augmented table", {
  fit <- augment_fixture()

  outliers <- diagnose_outliers(fit, method = "Top", threshold = 3)
  expect_contains(names(outliers), c(".fitted", ".resid", "outlier"))
  # The three flagged intervals are the three least likely ones, which is the
  # same ordering `.resid` expresses with the opposite sign.
  expect_equal(
    which(outliers$outlier),
    sort(order(fit$interval_log_lik)[1:3])
  )
  expect_equal(
    which(outliers$outlier),
    sort(order(outliers$.resid, decreasing = TRUE)[1:3])
  )
})
