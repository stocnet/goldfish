# The observation window closes at `end_time` even when the event schedule runs
# out first. The walk used to open the closing interval only in the branch it
# enters on meeting an event past the boundary, so an `end_time` beyond the last
# event was a complete no-op: the exposure between the last event and the end of
# the window left the likelihood silently, biasing the baseline rate upward.
#
# Two ways the window can close, tested apart. `dataTest`'s focal events run to
# time 36, so an `end_time` of 40 lies past the end of its schedule and one of
# 30 lies inside it -- the branch that already worked.

# A rate fixture whose only covariate is a node attribute that never changes.
# That is what makes the direction readable: with time-constant rates the
# trailing exposure scales every actor's intensity by the same factor, which the
# intercept absorbs whole, so the baseline rate is the intercept rather than
# something the covariates have redistributed.
static_rate_data <- function() {
  as_goldfish(list(
    info = list(
      name = "static",
      focal = "calls",
      update = c(calls = "increment"),
      directed = c(calls = TRUE),
      observation = c(calls = "event")
    ),
    nodes = data.frame(
      label = sprintf("Actor %d", seq_len(5)),
      weight = c(1, 2, 3, 2, 1)
    ),
    ties = data.frame(
      from = c(1, 2, 3, 4, 5, 1, 4, 2, 5, 3, 1, 5),
      to = c(2, 3, 4, 5, 1, 4, 2, 5, 3, 1, 5, 4),
      time = seq_len(12),
      layer = "calls"
    )
  ))
}

static_rate_fit <- function(data, end_time = NULL) {
  control <- if (is.null(end_time)) {
    set_preprocessing()
  } else {
    set_preprocessing(end_time = end_time)
  }
  estimate_dynam(
    calls ~ 1 + ego(weight),
    sub_model = "rate",
    data = data,
    control_prep = control
  )
}

test_that("exposure past the last event enters an exact-time rate fit", {
  # Events run 1..12, so the window without an end time spans 11 time units and
  # one closing at 20 spans 19.
  data <- static_rate_data()
  unbounded <- static_rate_fit(data)
  bounded <- static_rate_fit(data, end_time = 20)

  # This fit used to be byte-identical to the unbounded one.
  expect_gt(
    length(bounded$right_censored_events),
    length(unbounded$right_censored_events)
  )
  expect_true(tail(bounded$right_censored_events, 1))

  # Eight time units of exposure carrying no event lower the baseline rate, and
  # by exactly the ratio of the two exposures: a longer window with the same
  # events divides through. Pinning the amount, not only the direction, is what
  # catches a trailing interval of the wrong length.
  expect_lt(coef(bounded)[["Intercept"]], coef(unbounded)[["Intercept"]])
  expect_equal(
    coef(bounded)[["Intercept"]] - coef(unbounded)[["Intercept"]],
    log(11 / 19)
  )
  # The covariate is untouched: exposure rescales every actor alike.
  expect_equal(coef(bounded)[["ego"]], coef(unbounded)[["ego"]])

  # The accumulated intervals now reach the end of the window rather than the
  # last event, which is the clock invariant `test-interval_clock.R` asserts
  # for the unbounded case.
  expect_equal(bounded$start_time + sum(bounded$intervals), bounded$end_time)
})

test_that("a later end_time counts more exposure", {
  data <- static_rate_data()
  near <- static_rate_fit(data, end_time = 20)
  far <- static_rate_fit(data, end_time = 40)

  expect_lt(coef(far)[["Intercept"]], coef(near)[["Intercept"]])
  expect_equal(
    coef(far)[["Intercept"]] - coef(near)[["Intercept"]],
    log(19 / 39)
  )
  expect_equal(far$start_time + sum(far$intervals), far$end_time)
  expect_equal(
    sum(far$intervals) - sum(near$intervals),
    far$end_time - near$end_time
  )
})

test_that("a rate fit with covariates is no longer a no-op past its events", {
  # The same fix where the covariates do move, so the intercept is not the
  # baseline rate on its own and only the inequality is asserted.
  unbounded <- estimate_dynam(
    depNetwork ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = dataTest
  )
  bounded <- estimate_dynam(
    depNetwork ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = dataTest,
    control_prep = set_preprocessing(end_time = 40)
  )

  expect_false(isTRUE(all.equal(coef(unbounded), coef(bounded))))
  expect_lt(as.numeric(logLik(bounded)), as.numeric(logLik(unbounded)))
  expect_equal(bounded$start_time + sum(bounded$intervals), bounded$end_time)
})

test_that("a multinomial sub-model stores no trailing row", {
  # A choice likelihood has no compensator, so a censored row at the boundary
  # would contribute exactly zero while changing the interval count and every
  # per-interval diagnostic built on it.
  unbounded <- estimate_dynam(
    depNetwork ~ inertia + recip,
    sub_model = "choice",
    data = dataTest
  )
  bounded <- estimate_dynam(
    depNetwork ~ inertia + recip,
    sub_model = "choice",
    data = dataTest,
    control_prep = set_preprocessing(end_time = 40)
  )

  expect_equal(as.numeric(logLik(bounded)), as.numeric(logLik(unbounded)))
  expect_equal(
    length(bounded$right_censored_events),
    length(unbounded$right_censored_events)
  )
  expect_false(any(bounded$right_censored_events))
})

test_that("the boundary row carries no borrowed identity", {
  # With `end_time = 30` the walk stops on meeting the event at time 32, and
  # the closing row used to report that event's sender and receiver -- an
  # out-of-window event, which `augment()` would show as a real observation.
  prep <- compute_statistics(
    depNetwork ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    output = "preprocessed",
    control_prep = set_preprocessing(end_time = 30)
  )

  boundary <- length(prep$is_dependent)
  expect_identical(prep$is_dependent[[boundary]], 0L)
  expect_equal(prep$event_time[[boundary]], 30)
  expect_true(is.na(prep$event_sender[[boundary]]))
  expect_true(is.na(prep$event_receiver[[boundary]]))
})

test_that("an end_time inside the event stream is unchanged", {
  # The only branch the suite exercised before this change: the walk meets an
  # out-of-window event and closes there. Closing the window a second way must
  # not disturb it, so the values are pinned as measured beforehand.
  fit <- estimate_dynam(
    depNetwork ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = dataTest,
    control_prep = set_preprocessing(end_time = 30)
  )

  expect_equal(
    unname(coef(fit)),
    c(-3.0788893918, 0.5857707195, -0.2372662594),
    tolerance = 1e-7
  )
  expect_equal(as.numeric(logLik(fit)), -34.2785521347, tolerance = 1e-7)
  expect_equal(length(fit$right_censored_events), 11L)
  expect_equal(sum(!fit$right_censored_events), 10L)
  expect_equal(sum(fit$intervals), 29)
  expect_equal(fit$start_time + sum(fit$intervals), fit$end_time)
})
