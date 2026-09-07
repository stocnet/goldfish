# Which intervals `diagnose_outliers()` / `diagnose_changepoints()` analyze,
# and the classed-table contract they return. A rate or REM fit carries two
# structurally different quantities in one per-interval log-likelihood vector,
# and pooling them makes every statistic here describe the censoring pattern
# rather than the fit.

# A rate process read over an exogenous layer too, so its events open
# right-censored intervals interleaved among the dependent ones.
fit_censored <- function() {
  estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest
  )
}

test_that("rows are dependent events, and only a remainder is uncandidated", {
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Top", threshold = 3)

  # One row per dependent event. The censored intervals are not rows of their
  # own -- each was accumulated into the waiting time of the event it precedes.
  expect_gt(sum(fit$right_censored_events), 0)
  expect_equal(nrow(outliers), fit$n_events)
  expect_lt(nrow(outliers), length(fit$interval_log_lik))

  # A censored row survives only where the window outlives the last event, and
  # is never flagged: it realizes no outcome to be surprising about.
  expect_lte(sum(outliers$censored), 1L)
  expect_false(any(outliers$outlier[outliers$censored]))
  expect_equal(sum(outliers$outlier), 3L)
})

test_that("include_censored is deprecated and changes nothing", {
  # It selected between a per-event series and a pooled per-interval one.
  # There is no pooled alternative now: every row already carries the censored
  # intervals of its own span, so the argument has nothing left to select.
  fit <- fit_censored()
  default <- diagnose_outliers(fit, method = "Top", threshold = 3)

  expect_snapshot(
    pooled <- diagnose_outliers(
      fit,
      method = "Top",
      threshold = 3,
      include_censored = TRUE
    )
  )
  withr::local_options(lifecycle_verbosity = "quiet")
  pooled <- diagnose_outliers(
    fit,
    method = "Top",
    threshold = 3,
    include_censored = TRUE
  )
  expect_equal(pooled$outlier, default$outlier)
  expect_equal(pooled$.series, default$.series)
  expect_equal(nrow(pooled), nrow(default))

  expect_snapshot(
    cpt <- diagnose_changepoints(fit, include_censored = TRUE)
  )
})

test_that("the accumulated series has no censoring square wave to segment", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- baselines_social_evolution_data()
  fit <- estimate_dynam(
    calls_dependent ~ 1 + indeg(call_network, window = "15 minutes"),
    sub_model = "rate",
    data = data
  )

  # The mechanism that made a pooled series misleading: a censored interval
  # contributes only its timing term, so the per-interval series alternates
  # between two regimes and its spread is far wider than the events' own.
  dependent <- !fit$right_censored_events
  expect_gt(
    stats::IQR(fit$interval_log_lik),
    2 * stats::IQR(fit$interval_log_lik[dependent])
  )

  # Accumulation removes the alternation rather than filtering it out: each
  # event's row carries its own censored intervals, so there is no second
  # regime left in the series the describers read.
  series <- diagnose_outliers(fit, method = "IQR")$.series
  expect_equal(length(series), fit$n_events)
  expect_false(anyNA(series))
})


test_that("a windowed fit does not report one changepoint per closure", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- baselines_social_evolution_data()
  fit <- estimate_dynam(
    calls_dependent ~ 1 + indeg(call_network, window = "15 minutes"),
    sub_model = "rate",
    data = data
  )
  closures <- sum(fit$right_censored_events)

  changepoints <- diagnose_changepoints(fit, moment = "mean", method = "PELT")

  # The window opens at each event and closes 15 minutes later, so closures are
  # about as numerous as events. Segmenting a series that alternated between
  # them reported a changepoint at nearly every closure; the accumulated series
  # cannot, there being one row per event and no alternation in it.
  expect_gt(closures, 0.4 * fit$n_events)
  expect_equal(nrow(changepoints), fit$n_events)
  expect_lt(sum(changepoints$cpt), 0.25 * closures)
})


test_that("a multinomial sub-model is untouched", {
  # No censored intervals there, so each span is one interval and the
  # accumulation is the identity.
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest
  )

  expect_false(any(fit$right_censored_events))
  outliers <- diagnose_outliers(fit, method = "Top", threshold = 2)
  expect_equal(nrow(outliers), length(fit$interval_log_lik))
  expect_equal(outliers$.series, fit$interval_log_lik)
  expect_equal(
    outliers$outlier,
    diagnose_outliers(
      fit,
      method = "Top",
      threshold = 2,
      include_censored = TRUE
    )$outlier
  )
})


test_that("the tables carry the diagnostic metadata contract", {
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Hampel", threshold = 3)
  changepoints <- diagnose_changepoints(fit, moment = "variance")

  expect_s3_class(outliers, c("goldfishOutliers", "tbl_df"))
  expect_s3_class(changepoints, c("goldfishChangepoints", "tbl_df"))
  expect_identical(attr(outliers, "diagnostic"), "goldfishOutliers")
  expect_type(outliers$outlier, "logical")
  expect_type(changepoints$cpt, "logical")
  # The parameters that produced the table, so a saved object explains itself.
  expect_equal(attr(outliers, "params")$method, "Hampel")
  expect_equal(attr(outliers, "params")$threshold, 3)
  # `include_censored` is gone from the recorded parameters, not recorded as
  # FALSE: it no longer selects anything, so preserving it would say a choice
  # was made where none exists.
  expect_null(attr(outliers, "params")$include_censored)
  expect_equal(attr(changepoints, "params")$moment, "variance")
  expect_equal(attr(outliers, "context")$sub_model, "rate")
  expect_equal(
    attr(outliers, "version"),
    as.character(utils::packageVersion("goldfish"))
  )
})

test_that("class and metadata survive subsetting", {
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Top", threshold = 2)

  subset <- outliers[1:5, ]
  expect_s3_class(subset, "goldfishOutliers")
  expect_identical(attr(subset, "context"), attr(outliers, "context"))
  expect_identical(attr(subset, "params"), attr(outliers, "params"))
  columns <- outliers[, c("time", "outlier", ".series")]
  expect_s3_class(columns, "goldfishOutliers")
})

test_that("removing a defining column demotes the table", {
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Top", threshold = 2)
  changepoints <- diagnose_changepoints(fit, moment = "mean")

  # The flag column a print counts and the series a plot draws. Without them
  # the table cannot answer what it exists to answer, so it comes back as an
  # ordinary tibble rather than as an object whose methods read a column that
  # is gone.
  expect_identical(attr(outliers, "defining"), c("outlier", ".series"))
  dropped <- outliers[, c("time", "sender", ".series")]
  expect_false(inherits(dropped, "goldfishOutliers"))
  expect_s3_class(dropped, "tbl_df")
  expect_null(attr(dropped, "context"))
  expect_null(attr(dropped, "params"))
  expect_null(attr(dropped, "diagnostic"))
  expect_false(inherits(
    changepoints[, c("time", "cpt")],
    "goldfishChangepoints"
  ))

  # A row operation leaves every defining column in place, so it keeps both
  # the class and the count the header reports.
  flagged <- outliers[outliers$outlier, ]
  expect_s3_class(flagged, "goldfishOutliers")
  expect_equal(sum(flagged$outlier), sum(outliers$outlier))
  expect_identical(attr(flagged, "params"), attr(outliers, "params"))
})

test_that("the print methods report scope, not just counts", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- fit_censored()
  # Only the header is goldfish's: the table below it is tibble's print, and
  # pinning that would make this a regression test on pillar.
  header <- function(x) cat(head(capture.output(print(x)), 2), sep = "\n")

  expect_snapshot(header(diagnose_outliers(fit, method = "Top", threshold = 2)))
  expect_snapshot(
    header(diagnose_changepoints(fit, moment = "mean", method = "PELT"))
  )
})

test_that("the print lists the flagged rows, and only those", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Top", threshold = 2)

  # Header, scope, and the dimension line of what was listed: the count and
  # the rows come from one column, so the snapshot would catch a header
  # disagreeing with the listing. The columns below are tibble's print, and
  # pinning those would make this a regression test on pillar.
  expect_snapshot(cat(head(capture.output(print(outliers)), 3), sep = "\n"))

  # Nothing flagged prints the header alone. The schema does not move with the
  # result: the full series is still in the object, one row per event.
  clean <- diagnose_outliers(fit, method = "IQR", threshold = 1000)
  expect_snapshot(print(clean))
  expect_false(any(clean$outlier))
  expect_equal(dim(clean), dim(outliers))
})

test_that("the series is NA only on a row that took no part", {
  # Under the per-event contract the only row that can be NA is a censored
  # remainder -- a span closing no waiting time. Every event row is in the
  # series, so the NA pattern no longer describes the censoring structure.
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Top", threshold = 3)
  censored <- outliers$censored

  expect_lte(sum(censored), 1L)
  expect_true(all(is.na(outliers$.series[censored])))
  expect_false(anyNA(outliers$.series[!censored]))
  # And the series is the accumulated log-likelihood the augmented table
  # carries, so the two describe one thing.
  expect_equal(
    outliers$.series[!censored],
    augment(fit)$event_log_lik[!censored]
  )
})
