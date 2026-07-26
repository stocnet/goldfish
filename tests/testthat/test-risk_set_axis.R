# The risk-set axis is what makes a per-event index interpretable. It was always
# stored inside `model_spec`, but only reachable through an unexported accessor,
# so a user writing a diagnostic had to call `goldfish:::risk_set_axis()` or
# re-derive the geometry from `model` / `sub_model`. These tests pin it as
# documented, exported surface on the fit itself.

test_that("same-length per-event vectors from two axes are distinguishable", {
  data("social_evolution", envir = environment())
  rate <- estimate_dynam(
    calls ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )
  choice <- estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )

  # The premise: identical length, so length alone cannot tell them apart.
  expect_equal(
    length(rate$event_probabilities[[1]]),
    length(choice$event_probabilities[[1]])
  )
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

  # The ordered dyad grid and the unordered pair list, without the consumer
  # inspecting model / sub_model.
  expect_identical(risk_set_axis(rem), "dyad")
  expect_identical(risk_set_axis(coordination), "dyad_symmetric")
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
  expect_null(risk_set_axis(structure(list(), class = "result.goldfish")))
})
