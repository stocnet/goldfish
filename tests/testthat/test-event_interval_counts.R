# A fit carries two counts, because they are two different things, and the name
# `n_events` used to hold the wrong one.
#
# A right-censored interval is opened by any non-dependent event falling inside
# the observation window with a positive interval. Three sources, of very
# different size: a windowed effect materializes a dissolve pseudo-event per
# event, an exogenous stream contributes its own changes, and the window's
# closing boundary adds one. On the multinomial families none of this happens
# and the two counts coincide, which is why the conflation survived.
#
# The consequence is not cosmetic: BIC and AICc are penalties in the sample
# size, so a fit counting intervals is penalized for its own censoring. The
# exogenous case is the dangerous one precisely because it is small -- a
# two-interval discrepancy looks like nothing.

test_that("the two counts are what they say they are", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())

  fit <- estimate_dynam(
    calls ~ 1 + indeg + outdeg + indeg(friendship),
    sub_model = "rate",
    data = social_evolution
  )

  expect_identical(fit$n_events, sum(!fit$right_censored_events))
  expect_identical(fit$n_intervals, length(fit$right_censored_events))
  # The whole point: on this fit they differ.
  expect_gt(fit$n_intervals, fit$n_events)
})

test_that("an exogenous stream diverges the counts with no window at all", {
  # The case the design first missed. A windowed effect is the conspicuous
  # source; an exogenous stream is the quiet one.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())

  endogenous <- estimate_dynam(
    calls ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = social_evolution
  )
  exogenous <- estimate_dynam(
    calls ~ 1 + indeg + outdeg + indeg(friendship),
    sub_model = "rate",
    data = social_evolution
  )

  # Same event stream, so the same events are modeled either way.
  expect_identical(endogenous$n_events, exogenous$n_events)
  # Reading only the focal layer, there is no non-dependent event to open a
  # censored interval.
  expect_identical(endogenous$n_intervals, endogenous$n_events)
  expect_gt(exogenous$n_intervals, exogenous$n_events)
})

test_that("the sample size reported to BIC is the event count", {
  # Two fits on one event stream, one carrying censored intervals and one not.
  # They model the same events, so they must report the same sample size --
  # which is the comparability the information criteria depend on.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())

  plain <- estimate_dynam(
    calls ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = social_evolution
  )
  censored <- estimate_dynam(
    calls ~ 1 + indeg + outdeg + indeg(friendship),
    sub_model = "rate",
    data = social_evolution
  )

  expect_identical(
    attr(logLik(plain), "nobs"),
    attr(logLik(censored), "nobs")
  )
  expect_identical(attr(logLik(censored), "nobs"), censored$n_events)
  expect_identical(generics::glance(censored)$nobs, censored$n_events)

  # `avgPerEvent` divides by events, which is what its name promises.
  expect_equal(
    logLik(censored, avgPerEvent = TRUE),
    as.numeric(logLik(censored)) / censored$n_events
  )
})

test_that("a multinomial fit has one count, reported twice", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())

  fit <- estimate_dynam(
    calls ~ inertia + trans + tie(friendship),
    sub_model = "choice",
    data = social_evolution
  )

  # No compensator, so no censored rows, so nothing to tell apart.
  expect_identical(fit$n_events, fit$n_intervals)
  expect_false(any(fit$right_censored_events))
})

test_that("the per-interval components keep their own length", {
  # The other direction of the rename: these were reading `n_events` correctly
  # under its old meaning and belong to `n_intervals` now.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())

  fit <- estimate_dynam(
    calls ~ 1 + indeg + outdeg + indeg(friendship),
    sub_model = "rate",
    data = social_evolution,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )

  expect_length(fit$intervals, fit$n_intervals)
  expect_length(fit$total_rate, fit$n_intervals)
  expect_length(fit$interval_log_lik, fit$n_intervals)
  expect_equal(nrow(fit$event_scores), fit$n_intervals)
})
