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

test_that("rows are in interval order, so the columns line up", {
  fit <- augment_fixture_censored()
  augmented <- augment(fit)

  expect_equal(nrow(augmented), length(fit$interval_log_lik))
  # The censored intervals are interleaved in time, not appended: the table's
  # own time column IS the fit's, row for row.
  expect_equal(augmented$time, fit$event_time)
  expect_equal(augmented$right_censored_event, fit$right_censored_events)
  expect_equal(augmented$interval_log_lik, fit$interval_log_lik)
  expect_gt(sum(fit$right_censored_events), 0)
  expect_false(identical(
    which(augmented$right_censored_event),
    seq.int(
      nrow(augmented) - sum(fit$right_censored_events) + 1L,
      nrow(augmented)
    )
  ))
})

test_that("a right-censored interval has no fitted outcome", {
  fit <- augment_fixture_censored()
  augmented <- augment(fit)
  censored <- augmented$right_censored_event

  # It realizes nothing, so a fitted outcome probability and its deviance are
  # undefined -- while the log-likelihood contribution it does make is kept.
  expect_true(all(is.na(augmented$.fitted[censored])))
  expect_true(all(is.na(augmented$.resid[censored])))
  expect_false(anyNA(augmented$interval_log_lik))
  expect_equal(
    augmented$.fitted[!censored],
    exp(fit$interval_log_lik[!censored])
  )
  # And no event identity, which the fit does not have for it either.
  expect_true(all(is.na(augmented$sender[censored])))
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
