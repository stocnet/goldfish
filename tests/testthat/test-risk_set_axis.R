# The risk-set axis is what makes a per-event index interpretable. It was always
# stored inside `model_spec`, but only reachable through an unexported accessor,
# so a user writing a diagnostic had to call `goldfish:::risk_set_axis()` or
# re-derive the geometry from `model` / `sub_model`. These tests pin it as
# documented, exported surface on the fit itself.

test_that("same-length per-event vectors from two axes are distinguishable", {
  data("social_evolution", envir = environment())
  # `"probabilities"` is NOT in the default `diagnostics` set, and the stored
  # component is spelled `event_probabilities` while its neighbours are
  # snake_case. Reading the wrong name gives NULL, and `NULL[[1]]` is NULL
  # rather than an error, so the length comparison below would silently compare
  # 0 to 0 and prove nothing. Both are load-bearing here.
  with_probabilities <- set_algorithm_newton(
    diagnostics = c("loglik", "scores", "probabilities")
  )
  # suppressWarnings() covers exactly one expected condition: the
  # storage-footprint guardrail warns by design whenever probabilities are
  # requested. These tests assert nothing about warnings.
  rate <- suppressWarnings(estimate_dynam(
    calls ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = social_evolution,
    control_algo = with_probabilities,
    progress = FALSE,
    verbose = FALSE
  ))
  choice <- suppressWarnings(estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    control_algo = with_probabilities,
    progress = FALSE,
    verbose = FALSE
  ))

  # The premise: identical, non-trivial length, so length alone cannot tell the
  # two axes apart.
  rate_length <- length(rate$event_probabilities[[1]])
  expect_gt(rate_length, 1L)
  expect_equal(length(choice$event_probabilities[[1]]), rate_length)
  expect_identical(risk_set_axis(rate), "sender")
  expect_identical(risk_set_axis(choice), "receiver_given_sender")
})

test_that("the axis is readable without reaching into internals", {
  data("social_evolution", envir = environment())
  fit <- estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )

  expect_contains(getNamespaceExports("goldfish"), "risk_set_axis")
  # The value is on the fit, so no consumer needs to index model_spec.
  expect_identical(fit$risk_set_axis, risk_set_axis(fit))
})

test_that("the axis names the geometry, not the model family", {
  data("social_evolution", envir = environment())
  rem <- estimate_rem(
    calls ~ 1 + inertia + recip,
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )
  coordination <- estimate_dynam(
    calls ~ inertia + trans,
    sub_model = "choice_coordination",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )

  # Both read the same dyad grid, so both name the same axis. Coordination's
  # reduction to unordered pairs is a property of its likelihood, and a
  # consumer separates them on that -- still without inspecting model /
  # sub_model.
  expect_identical(risk_set_axis(rem), "dyad")
  expect_identical(risk_set_axis(coordination), "dyad")
})

test_that("the axis reads off a preprocessed object and a model spec too", {
  data("social_evolution", envir = environment())
  prep <- compute_statistics(
    calls ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    data = social_evolution,
    output = "preprocessed",
    progress = FALSE,
    verbose = FALSE
  )
  expect_identical(risk_set_axis(prep), "receiver_given_sender")
  expect_identical(risk_set_axis(prep$model_spec), "receiver_given_sender")
})

test_that("a fit predating the recorded axis reads as NULL, not an error", {
  # The `backend` convention: an old object reports an unknown value rather than
  # failing, so a consumer can branch on it.
  expect_null(risk_set_axis(structure(list(), class = "goldfishFit")))
})
