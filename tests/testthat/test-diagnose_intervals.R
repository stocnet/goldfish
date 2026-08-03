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

test_that("censored intervals are present but never flagged", {
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Top", threshold = 3)

  expect_equal(nrow(outliers), length(fit$interval_log_lik))
  expect_gt(sum(fit$right_censored_events), 0)
  expect_false(any(outliers$outlier[outliers$right_censored_event]))
  # The threshold came from the dependent intervals alone, so the flagged set
  # is the worst of those rather than the worst of the pooled series.
  dependent <- which(!fit$right_censored_events)
  expect_equal(
    which(outliers$outlier),
    sort(dependent[order(fit$interval_log_lik[dependent])[1:3]])
  )
})

test_that("the setting changes candidacy, never the row count", {
  fit <- fit_censored()
  default <- diagnose_outliers(fit, method = "Top", threshold = 3)
  pooled <- diagnose_outliers(
    fit,
    method = "Top",
    threshold = 3,
    include_censored = TRUE
  )

  expect_equal(nrow(pooled), nrow(default))
  expect_equal(pooled$interval_log_lik, default$interval_log_lik)
  expect_equal(
    attr(default, "context")$n_analyzed,
    sum(!fit$right_censored_events)
  )
  expect_equal(attr(pooled, "context")$n_analyzed, nrow(pooled))
})

test_that("pooling shifts the threshold the statistic is read against", {
  skip_on_cran()
  data <- baselines_social_evolution_data()
  fit <- estimate_dynam(
    calls_dependent ~ 1 + indeg(call_network, window = "15 minutes"),
    sub_model = "rate",
    data = data
  )
  dependent <- !fit$right_censored_events

  # This is the mechanism, and it is visible before any flag is set: a
  # censored interval contributes only its timing term, so the pooled series
  # has a different center and a much wider spread than the events do.
  expect_gt(
    stats::IQR(fit$interval_log_lik),
    2 * stats::IQR(fit$interval_log_lik[dependent])
  )
  expect_false(identical(
    which(diagnose_outliers(fit, method = "IQR")$outlier),
    which(
      diagnose_outliers(fit, method = "IQR", include_censored = TRUE)$outlier
    )
  ))
  # `method = "Top"` is the one that does not move on an exact-time fit: the
  # censored intervals sit at the HIGH end of the log-likelihood, so they
  # never enter the bottom k however the series is pooled.
  expect_equal(
    which(diagnose_outliers(fit, method = "Top", threshold = 5)$outlier),
    which(
      diagnose_outliers(
        fit,
        method = "Top",
        threshold = 5,
        include_censored = TRUE
      )$outlier
    )
  )
})

test_that("a windowed fit does not report one changepoint per closure", {
  skip_on_cran()
  data <- baselines_social_evolution_data()
  fit <- estimate_dynam(
    calls_dependent ~ 1 + indeg(call_network, window = "15 minutes"),
    sub_model = "rate",
    data = data
  )
  closures <- sum(fit$right_censored_events)

  default <- diagnose_changepoints(fit, moment = "mean", method = "PELT")
  pooled <- diagnose_changepoints(
    fit,
    moment = "mean",
    method = "PELT",
    include_censored = TRUE
  )

  # The window opens at each event and closes 15 minutes later, so the two
  # kinds of interval alternate almost one for one: segmenting the pooled
  # series reports a changepoint at nearly every closure.
  expect_gt(closures, 0.4 * nrow(default))
  expect_gt(sum(pooled$cpt), 0.9 * closures)
  expect_lt(sum(default$cpt), 0.25 * sum(pooled$cpt))
  expect_false(any(default$cpt[default$right_censored_event]))
})

test_that("the two settings agree on a multinomial sub-model", {
  fit <- estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest
  )

  expect_false(any(fit$right_censored_events))
  expect_equal(
    diagnose_outliers(fit, method = "Top", threshold = 2)$outlier,
    diagnose_outliers(
      fit,
      method = "Top",
      threshold = 2,
      include_censored = TRUE
    )$outlier
  )
  expect_equal(
    diagnose_changepoints(fit)$cpt,
    diagnose_changepoints(fit, include_censored = TRUE)$cpt
  )
})

test_that("the tables carry the diagnostic metadata contract", {
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Hampel", threshold = 3)
  changepoints <- diagnose_changepoints(fit, moment = "variance")

  expect_s3_class(outliers, c("diagnose_outliers", "tbl_df"))
  expect_s3_class(changepoints, c("diagnose_changepoints", "tbl_df"))
  expect_identical(attr(outliers, "diagnostic"), "diagnose_outliers")
  expect_type(outliers$outlier, "logical")
  expect_type(changepoints$cpt, "logical")
  # The parameters that produced the table, so a saved object explains itself.
  expect_equal(attr(outliers, "params")$method, "Hampel")
  expect_equal(attr(outliers, "params")$threshold, 3)
  expect_false(attr(outliers, "params")$include_censored)
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
  expect_s3_class(subset, "diagnose_outliers")
  expect_identical(attr(subset, "context"), attr(outliers, "context"))
  expect_identical(attr(subset, "params"), attr(outliers, "params"))
  columns <- outliers[, c("time", "outlier", ".series")]
  expect_s3_class(columns, "diagnose_outliers")
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
  expect_false(inherits(dropped, "diagnose_outliers"))
  expect_s3_class(dropped, "tbl_df")
  expect_null(attr(dropped, "context"))
  expect_null(attr(dropped, "params"))
  expect_null(attr(dropped, "diagnostic"))
  expect_false(inherits(
    changepoints[, c("time", "cpt")],
    "diagnose_changepoints"
  ))

  # A row operation leaves every defining column in place, so it keeps both
  # the class and the count the header reports.
  flagged <- outliers[outliers$outlier, ]
  expect_s3_class(flagged, "diagnose_outliers")
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
  expect_snapshot(
    header(diagnose_outliers(
      fit,
      method = "Top",
      threshold = 1,
      include_censored = TRUE
    ))
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
  # result: the full series is still in the object, one row per interval.
  clean <- diagnose_outliers(fit, method = "IQR", threshold = 1000)
  expect_snapshot(print(clean))
  expect_false(any(clean$outlier))
  expect_equal(dim(clean), dim(outliers))
})

test_that("the series is NA on the intervals that took no part", {
  fit <- fit_censored()
  outliers <- diagnose_outliers(fit, method = "Top", threshold = 3)
  dependent <- !fit$right_censored_events

  expect_true(all(is.na(outliers$.series[!dependent])))
  expect_equal(outliers$.series[dependent], fit$interval_log_lik[dependent])
  # Admitting the censored intervals is exactly what makes their rows part of
  # the series, so the NA pattern moves with the setting.
  pooled <- diagnose_outliers(
    fit,
    method = "Top",
    threshold = 3,
    include_censored = TRUE
  )
  expect_false(anyNA(pooled$.series))
  expect_equal(pooled$.series, fit$interval_log_lik)
})
