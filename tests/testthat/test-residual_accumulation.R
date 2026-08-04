# A residual is per event, not per interval.
#
# The per-interval types accumulate over each event's waiting time, and the
# direction is forced by what the quantity means rather than chosen: Cox-Snell
# is the compensator over the span from event k-1 to event k, so an interval
# closes the waiting time of the event that FOLLOWS it. Grouping the other way
# would make an event's residual the exposure that came after it.
#
# Two identities are the whole point of accumulating rather than dropping the
# censored rows, and both are asserted below: the accumulated score sums equal
# the all-interval sums (so the score equation still holds at the maximum,
# which it does not if the censored rows are simply discarded), and the
# accumulated Cox-Snell values total the dependent-event count.

accumulation_fit <- function(formula) {
  data("social_evolution", package = "goldfish", envir = environment())
  estimate_dynam(
    formula,
    sub_model = "rate",
    data = social_evolution,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )
}

test_that("accumulated scores keep the score equation the interval rows have", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- accumulation_fit(
    calls ~ 1 + indeg(calls) + indeg(calls, window = 300) + indeg(friendship)
  )

  accumulated <- residuals(fit, type = "score")
  # Every interval's contribution is still present, only regrouped. Compared on
  # an absolute scale: both sums are ~1e-7, so a relative tolerance would
  # reject a 1e-13 difference in summation order.
  expect_lt(
    max(abs(colSums(accumulated) - colSums(fit$event_scores))),
    1e-9
  )
  # At the maximum that sum is zero, which is what makes it the score equation.
  expect_true(all(abs(colSums(accumulated)) < 1e-4))

  # Dropping the censored rows instead of accumulating them does not preserve
  # it -- this is the comparison that motivates the whole change.
  dependent_only <- fit$event_scores[!fit$right_censored_events, , drop = FALSE]
  expect_false(all(abs(colSums(dependent_only)) < 1e-4))
})

test_that("accumulated cox_snell totals the dependent-event count", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- accumulation_fit(
    calls ~ 1 + indeg(calls) + indeg(calls, window = 300) + indeg(friendship)
  )

  residual <- residuals(fit, type = "cox_snell")
  expect_equal(sum(residual), fit$n_events, tolerance = 1e-6)
  expect_true(all(residual >= 0))
})

test_that("a censored remainder is returned and flagged, not folded or dropped", {
  # Intervals after the last dependent event close no waiting time. Folding
  # them into the last event would report exposure that came after it; dropping
  # them would lose it from the totals above.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  with_tail <- accumulation_fit(calls ~ 1 + indeg(calls) + indeg(friendship))

  residual <- residuals(with_tail, type = "cox_snell")
  censored <- attr(residual, "right_censored")

  expect_length(residual, with_tail$n_events + 1L)
  expect_length(censored, with_tail$n_events + 1L)
  # Only the final entry, and only ever the final entry.
  expect_identical(which(censored), length(residual))
})

test_that("a fit whose last interval is an event has no remainder", {
  # A windowed effect never produces a trailing span: its dissolve
  # pseudo-events are bounded by the observation window. Only exogenous
  # streams and an `end_time` past the last event reach beyond it.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  windowed <- accumulation_fit(
    calls ~ 1 + indeg(calls) + indeg(calls, window = 300)
  )

  expect_gt(windowed$n_intervals, windowed$n_events)
  residual <- residuals(windowed, type = "cox_snell")
  expect_length(residual, windowed$n_events)
  expect_false(any(attr(residual, "right_censored")))
})

test_that("every accumulating type returns one value per event", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  windowed <- accumulation_fit(
    calls ~ 1 + indeg(calls) + indeg(calls, window = 300)
  )

  for (type in c("deviance", "cox_snell")) {
    expect_length(residuals(windowed, type = type), windowed$n_events)
  }
  for (type in c("score", "dfbeta", "dfbetas")) {
    expect_equal(
      nrow(residuals(windowed, type = type)),
      windowed$n_events,
      info = type
    )
  }
  # cooks is a scalar per event, evaluated on the accumulated row.
  expect_length(residuals(windowed, type = "cooks"), windowed$n_events)
})

test_that("cooks is the influence of the whole event, not a sum of parts", {
  # The quadratic form does not commute with accumulation, so this is the one
  # type that must be computed FROM the accumulated row rather than summed
  # over the intervals inside it. The two differ whenever a span holds more
  # than one interval.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  windowed <- accumulation_fit(
    calls ~ 1 + indeg(calls) + indeg(calls, window = 300)
  )

  accumulated <- residuals(windowed, type = "cooks")
  per_interval <- rowsum(
    goldfish:::influence_rows(windowed, windowed$event_scores, "cooks"),
    goldfish:::accumulation_index(windowed)
  )
  expect_false(isTRUE(all.equal(accumulated, as.numeric(per_interval))))
})

test_that("a multinomial fit is untouched by the accumulation", {
  # Every group holds exactly one interval there, so the regrouping is the
  # identity and the returned series is what it always was.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data("social_evolution", package = "goldfish", envir = environment())
  fit <- estimate_dynam(
    calls ~ inertia + trans,
    sub_model = "choice",
    data = social_evolution,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )

  expect_identical(fit$n_events, fit$n_intervals)
  expect_equal(
    residuals(fit, type = "deviance"),
    -2 * fit$interval_log_lik,
    ignore_attr = TRUE
  )
  expect_equal(
    residuals(fit, type = "score"),
    fit$event_scores,
    ignore_attr = TRUE
  )
})
