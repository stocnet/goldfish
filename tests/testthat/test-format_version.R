# The layout epoch a fit is stamped with. It exists so that recognizing an older
# object does not rest on reasoning from which components happen to be absent --
# the mode of failure it replaces returned a log-likelihood with no degrees of
# freedom and let AIC() misreport in silence.

test_that("a newly fitted object records the layout epoch it was built with", {
  data("social_evolution", envir = environment())
  choice <- estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )

  # Present and non-empty before asserting the value: a missing component reads
  # as NULL, and every comparison against NULL below would hold vacuously.
  expect_contains(names(choice), "format_version")
  expect_length(choice$format_version, 1L)
  expect_identical(choice$format_version, goldfish_result_format)
  expect_type(choice$format_version, "integer")
})

test_that("the epoch is stamped on every estimator, not one code path", {
  data("social_evolution", envir = environment())
  # Each public estimator reaches the same finalization through
  # `estimate_wrapper()`. These cover the two sub-model geometries (timed rate
  # with an intercept, ordinal choice without one) and the tie-oriented model, so
  # a stamp attached to one assembly rather than the funnel fails here.
  rate <- estimate_dynam(
    calls ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )
  rem <- estimate_rem(
    calls ~ 1 + inertia + recip,
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )

  for (fit in list(rate = rate, rem = rem)) {
    expect_length(fit$format_version, 1L)
    expect_identical(fit$format_version, goldfish_result_format)
  }
})
