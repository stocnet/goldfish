# `residuals(type = "cox_snell", level = "actor")`: each actor's compensators
# over its OWN consecutive events.
#
# The event-level series asks whether the waiting time to the next event, by
# anyone, was what the model expected. This asks the same of one actor's own
# spacing, which is what makes it a shape reading rather than a second level
# reading -- `margin_table()` already reports level, and an actor whose events
# are correctly counted but clustered is calibrated on every column it carries.
#
# The reconciliation identities are the whole check: an actor's spans sum to its
# stored expected margin, and its uncensored spans number its observed one. They
# hold because an actor's rate is the total rate times that actor's share of it,
# so the stratified contributions are the unstratified compensator split across
# the risk set rather than a second quantity.

actor_fit <- function() {
  data("social_evolution", package = "goldfish", envir = environment())
  estimate_dynam(
    calls ~ 1 + indeg(calls),
    sub_model = "rate",
    data = social_evolution,
    return_preprocessed = TRUE,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "margins"))
  )
}

test_that("the event level is untouched by the actor level existing", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- actor_fit()

  # `level` defaults per type, so the default here is still "event". A single
  # global default would have silently switched this call to the stratified
  # reading, `level`'s first value having been "actor" for `"martingale"`.
  expect_equal(
    residuals(fit, type = "cox_snell"),
    residuals(fit, type = "cox_snell", level = "event")
  )
  expect_length(residuals(fit, type = "cox_snell"), fit$n_events)
})

test_that("an actor's spans reconcile with its stored margins", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- actor_fit()

  spans <- residuals(fit, type = "cox_snell", level = "actor")
  expect_length(spans, length(fit$margins$expected))
  expect_named(spans, names(fit$margins$expected))

  # Sums to the expected margin: the stratified contributions are the
  # unstratified compensator split across the risk set, so nothing is lost or
  # double-counted by the split.
  expect_equal(
    unname(vapply(spans, sum, numeric(1))),
    unname(as.numeric(fit$margins$expected))
  )
  # And the uncensored spans number the observed count, each closing one of
  # that actor's own events.
  closed <- vapply(
    spans,
    function(x) sum(!attr(x, "right_censored")),
    numeric(1)
  )
  expect_equal(unname(closed), unname(as.numeric(fit$margins$observed)))
})

test_that("every actor carries a censoring flag, at most one span long", {
  # Attached even where nothing is censored, unlike the event-level series
  # where the attribute's presence is itself the signal. There is one series
  # there; here there are as many as there are actors, and a caller reading all
  # of them should not branch on which actor sent the final event.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  spans <- residuals(actor_fit(), type = "cox_snell", level = "actor")

  for (actor in spans) {
    censored <- attr(actor, "right_censored")
    expect_false(is.null(censored))
    expect_length(censored, length(actor))
    expect_lte(sum(censored), 1L)
    if (any(censored)) {
      expect_identical(which(censored), length(actor))
    }
    expect_true(all(is.finite(actor) & actor >= 0))
  }
})

test_that("an actor that never acts is exposure and nothing else", {
  # The fixture has actors who send no calls at all. They are not dropped: they
  # were at risk throughout, and their whole series is the one censored span
  # that says so.
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- actor_fit()
  spans <- residuals(fit, type = "cox_snell", level = "actor")

  silent <- which(as.numeric(fit$margins$observed) == 0)
  expect_gt(length(silent), 0L)
  for (i in silent) {
    expect_length(spans[[i]], 1L)
    expect_true(attr(spans[[i]], "right_censored"))
  }
})

test_that("a tie-oriented fit reports both sides", {
  skip_on_cran()
  fit <- suppressWarnings(estimate_wrapper(
    depNetwork ~ 1 + indeg,
    model = "REM",
    sub_model = "rate",
    data = dataTest,
    return_preprocessed = TRUE,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "margins"))
  ))

  spans <- residuals(fit, type = "cox_snell", level = "actor")
  # The names `residuals(type = "martingale")` already uses on this family.
  expect_named(spans, c("sender", "receiver"))
  for (side in c("sender", "receiver")) {
    expect_equal(
      unname(vapply(spans[[side]], sum, numeric(1))),
      unname(as.numeric(fit$margins[[paste0("expected_", side)]]))
    )
  }
})

test_that("the level vocabulary is checked per type", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- actor_fit()

  testthat::local_reproducible_output(
    width = 80,
    crayon = FALSE,
    unicode = TRUE
  )
  # A Cox-Snell residual has no second axis to aggregate over.
  expect_snapshot(
    error = TRUE,
    residuals(fit, type = "cox_snell", level = "dyad")
  )
  # And a type with one reading says so rather than ignoring the argument,
  # which is what it used to do.
  expect_snapshot(
    error = TRUE,
    residuals(fit, type = "deviance", level = "actor")
  )
})

test_that("a family with no waiting time refuses the actor level too", {
  # The stratification does not create a compensator where the likelihood
  # defines none, so the guard is the unstratified one, reached first.
  skip_on_cran()
  fit <- estimate_wrapper(
    depNetwork ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    return_preprocessed = TRUE,
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )
  expect_error(
    residuals(fit, type = "cox_snell", level = "actor"),
    "exact-time sub-models only"
  )
})
