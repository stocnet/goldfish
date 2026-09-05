# `diagnose_onset()`: what the left-censored start of the sequence did to the
# estimate. Everything here reads stored score rows and the fit's information
# matrix, so the first thing to hold is that nothing else is touched.

onset_fixture <- function(
  formula = depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
  ...
) {
  estimate_wrapper(
    formula,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    ...
  )
}

# The cold/warm pair: the same model over the same calls, differing only in
# whether the network was warm-started by linking the events that precede the
# observation window. `skip` events become history rather than outcomes.
#
# Built with the pre-stocnet constructors, which is what makes a subset of a
# layer's events the dependent ones; their deprecation warnings are silenced
# because this fixture is about warm-starting, not about the builders.
onset_calls_fit <- function(skip) {
  withr::local_options(lifecycle_verbosity = "quiet")
  data("Social_Evolution", envir = environment())
  actors <- get("actors", envir = environment())
  calls <- get("calls", envir = environment())
  call_network <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(call_network, calls, nodes = actors)
  dependent <- make_dependent_events(
    events = if (skip > 0) calls[-seq_len(skip), ] else calls,
    nodes = actors,
    default_network = call_network
  )
  estimate_dynam(
    dependent ~ inertia + recip,
    sub_model = "choice",
    data = make_data(dependent, call_network, calls, actors),
    control_algo = set_algorithm_newton(diagnostics = "scores")
  )
}

test_that("the paths and the accrual curve need no pass and no replay", {
  fit <- onset_fixture()
  expect_null(fit$preprocessed)
  # If either curve reached for an evaluation pass this would abort, which is
  # the whole claim: both are arithmetic on the stored rows.
  local_mocked_bindings(
    evaluate_model = function(...) stop("an evaluation pass was triggered")
  )
  onset <- diagnose_onset(fit)

  expect_s3_class(onset, "goldfishOnset")
  expect_named(onset, c("path", "accrual", "summary"))
  for (component in onset) {
    expect_s3_class(component, "tbl_df")
  }
  expect_identical(attr(onset, "diagnostic"), "goldfishOnset")
  expect_identical(attr(onset, "params")$information, "opg")
  expect_identical(
    attr(onset, "context")$n_intervals,
    length(fit$interval_log_lik)
  )
  expect_identical(
    attr(onset, "context")$n_events,
    sum(!fit$right_censored_events)
  )
  expect_identical(
    attr(onset, "version"),
    as.character(utils::packageVersion("goldfish"))
  )
})

test_that("the path is a bridge from the estimate back to it", {
  fit <- onset_fixture()
  onset <- diagnose_onset(fit)
  estimate <- unname(coef(fit, complete = TRUE))

  # Dropping nothing is the estimate exactly; dropping everything returns to
  # it, the total score being zero at the maximum. Only the excursion between
  # the two is read.
  first <- onset$path$estimate[onset$path$dropped_intervals == 0]
  last <- onset$path$estimate[
    onset$path$dropped_intervals == max(onset$path$dropped_intervals)
  ]
  expect_equal(first, estimate)
  expect_equal(last, estimate, tolerance = 1e-6)
  expect_equal(
    nrow(onset$path),
    (nrow(fit$event_scores) + 1) * length(estimate)
  )
  expect_equal(onset$accrual$share[1], 0)
  expect_equal(onset$accrual$share[nrow(onset$accrual)], 1)
  expect_false(is.unsorted(onset$accrual$share))
  # The axis counts history, not window closures: this fit has right-censored
  # intervals, so the two indexes part company.
  expect_gt(sum(fit$right_censored_events), 0)
  expect_lt(max(onset$path$dropped_events), max(onset$path$dropped_intervals))
})

test_that("a coefficient held fixed does not move", {
  fit <- onset_fixture(depNetwork ~ 1 + indeg + offset(outdeg, coef = 0.5))
  onset <- diagnose_onset(fit)

  expect_identical(sum(onset$summary$fixed), 1L)
  expect_equal(unique(onset$path$estimate[onset$path$fixed]), 0.5)
  expect_equal(onset$summary$max_drift[onset$summary$fixed], 0)
  expect_equal(onset$summary$stabilized_at[onset$summary$fixed], 0L)
})

test_that("a cold start drifts and accrues nothing; a warm one does not", {
  skip_on_cran()
  cold <- diagnose_onset(onset_calls_fit(0))
  warm <- diagnose_onset(onset_calls_fit(150))

  # The mechanism, before any summary: with an empty history every alternative
  # looks alike, so the first events' score rows are *exactly* zero and the
  # accrual curve does not move at all. Warm-started, the first event already
  # carries information.
  leading_zeros <- function(x) sum(cumprod(x$accrual$share[-1] == 0))
  expect_gt(leading_zeros(cold), 0)
  expect_equal(leading_zeros(warm), 0)

  # Over the first 20 events the cold fit has accrued well under its
  # proportional share of the information; the warm fit is close to it.
  share_at <- function(x, k) x$accrual$share[x$accrual$dropped_events == k][1]
  proportional <- function(x) 20 / attr(x, "context")$n_events
  expect_lt(share_at(cold, 20), 0.5 * proportional(cold))
  expect_gt(share_at(warm, 20), 0.75 * proportional(warm))

  # And the paths: drift that settles late against drift that settles at once.
  expect_gt(max(cold$summary$stabilized_at), max(warm$summary$stabilized_at))
})

test_that("bad tuning is named, not substituted", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- onset_fixture()
  expect_snapshot(diagnose_onset(fit, tolerance = -1), error = TRUE)
})

test_that("the expected-information curve is the per-interval Fisher trace", {
  # `"opg"` reduces the stored score rows; `"expected"` costs one evaluation
  # pass, so it needs the statistics and says so without them. The two are
  # different estimators of the same accrual, so they must agree on the shape
  # of the curve without being the same numbers.
  fit <- onset_fixture(return_preprocessed = TRUE)
  opg <- diagnose_onset(fit, information = "opg")
  expected <- diagnose_onset(fit, information = "expected")

  expect_equal(nrow(expected$accrual), nrow(opg$accrual))
  # A cumulative share: starts at nothing, ends at everything, never decreases.
  expect_equal(expected$accrual$share[1], 0)
  expect_equal(expected$accrual$share[nrow(expected$accrual)], 1)
  expect_true(all(diff(expected$accrual$share) >= 0))
  expect_equal(attr(expected, "params")$information, "expected")
  # Same curve, different estimator -- so strongly agreeing but not identical.
  expect_gt(stats::cor(expected$accrual$share, opg$accrual$share), 0.95)
  expect_false(isTRUE(all.equal(expected$accrual$share, opg$accrual$share)))

  # Only the path is arithmetic on the stored rows; the expected curve is not.
  expect_snapshot(
    diagnose_onset(onset_fixture(), information = "expected"),
    error = TRUE
  )
})

test_that("a fit without the score rows says which primitive to store", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- onset_fixture(
    control_algo = set_algorithm_newton(
      diagnostics = "loglik"
    )
  )
  expect_null(fit$event_scores)
  expect_snapshot(diagnose_onset(fit), error = TRUE)
})

test_that("print reports the excursion, and says it is descriptive", {
  withr::local_options(cli.width = 80, cli.unicode = FALSE, cli.num_colors = 1)
  fit <- onset_fixture()
  # Only the header is goldfish's: the summary below it is tibble's print, and
  # pinning that would make this a regression test on pillar.
  header <- function(x) {
    out <- capture.output(print(x))
    cat(out[seq_len(which(startsWith(out, "# A tibble"))[1] - 1L)], sep = "\n")
  }
  expect_snapshot(header(diagnose_onset(fit)))
})
