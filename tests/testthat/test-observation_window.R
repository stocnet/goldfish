# The observation window a fit was estimated on. A fit records the dependent
# events inside its resolved window, so the events it reports and the
# per-interval components it carries describe the same likelihood.
#
# `dataTest`'s focal events fall at 1 6 9 13 15 16 19 23 28 29 32 36, so
# `start_time = 10` drops three from the front and `end_time = 30` two from the
# back.

test_that("an unwindowed fit records every dependent event", {
  fit <- estimate_dynam(
    depNetwork ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )

  expect_equal(
    as.numeric(fit$dependent_events$time),
    as.numeric(fit$event_time[!fit$right_censored_events])
  )
})

test_that("a windowed fit records only the events it modeled", {
  # Row identity, not row count: a filter that kept the wrong rows would keep
  # the right *number* of them, so the times are what has to match.
  windows <- list(
    start = list(start_time = 10),
    end = list(end_time = 30),
    both = list(start_time = 10, end_time = 30)
  )

  for (sub_model in c("rate", "choice")) {
    formula <- if (sub_model == "rate") {
      depNetwork ~ 1 + indeg + outdeg
    } else {
      depNetwork ~ inertia + recip
    }

    for (window in names(windows)) {
      fit <- estimate_dynam(
        formula,
        sub_model = sub_model,
        data = dataTest,
        control_prep = do.call(set_preprocessing, windows[[window]]),
        control_algo = set_algorithm_newton(diagnostics = "loglik")
      )
      label <- paste(sub_model, window)

      expect_equal(
        as.numeric(fit$dependent_events$time),
        as.numeric(fit$event_time[!fit$right_censored_events]),
        info = label
      )
      expect_equal(
        nrow(fit$dependent_events),
        sum(!fit$right_censored_events),
        info = label
      )
      # The bounds are inclusive, matching the walk.
      bounds <- windows[[window]]
      if (!is.null(bounds$start_time)) {
        expect_true(
          all(as.numeric(fit$dependent_events$time) >= bounds$start_time),
          info = label
        )
      }
      if (!is.null(bounds$end_time)) {
        expect_true(
          all(as.numeric(fit$dependent_events$time) <= bounds$end_time),
          info = label
        )
      }
    }
  }
})

test_that("the window actually excludes events", {
  # Guards the assertions above against passing vacuously on a filter that
  # dropped nothing.
  full <- estimate_dynam(
    depNetwork ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )
  windowed <- estimate_dynam(
    depNetwork ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = dataTest,
    control_prep = set_preprocessing(start_time = 10, end_time = 30),
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )

  expect_lt(nrow(windowed$dependent_events), nrow(full$dependent_events))
  expect_false(any(as.numeric(windowed$dependent_events$time) < 10))
})

test_that("the augmented table and its describers reach a windowed fit", {
  # The regression this change exists for: an unfiltered dependent-events table
  # cannot be interleaved with the per-interval components, and `augment()`
  # aborted on the length mismatch rather than returning a table.
  fit <- estimate_dynam(
    depNetwork ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = dataTest,
    control_prep = set_preprocessing(start_time = 10),
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )

  augmented <- augment(fit)
  expect_s3_class(augmented, "tbl_df")
  expect_equal(nrow(augmented), length(fit$interval_log_lik))
  expect_equal(sum(!augmented$censored), nrow(fit$dependent_events))

  expect_no_error(diagnose_outliers(fit))
  expect_no_error(diagnose_changepoints(fit))
})
