test_that("constructors return the documented class vectors", {
  expect_identical(
    class(dynam_rate_spec(nodes = "actors")),
    c("goldfishKindDnRate", "goldfishAxisSender", "goldfishKind")
  )
  expect_identical(
    class(dynam_rate_ordered_spec(nodes = "actors")),
    c("goldfishKindDnCox", "goldfishAxisSender", "goldfishKind")
  )
  expect_identical(
    class(dynam_choice_spec(nodes = "actors")),
    c("goldfishKindDnChoice", "goldfishAxisDyad", "goldfishKind")
  )
  expect_identical(
    class(dynam_choice_coord_spec(nodes = "actors")),
    c("goldfishKindDnCoord", "goldfishAxisDyad", "goldfishKind")
  )
  expect_identical(
    class(dynami_rate_spec(nodes = "actors")),
    c("goldfishKindDniRate", "goldfishAxisSender", "goldfishKind")
  )
  expect_identical(
    class(dynami_rate_ordered_spec(nodes = "actors")),
    c("goldfishKindDniCox", "goldfishAxisSender", "goldfishKind")
  )
  expect_identical(
    class(dynami_choice_spec(nodes = "actors")),
    c("goldfishKindDniChoice", "goldfishAxisDyad", "goldfishKind")
  )
  expect_identical(
    class(rem_rate_spec(nodes = "actors")),
    c("goldfishKindRemRate", "goldfishAxisDyad", "goldfishKind")
  )
  expect_identical(
    class(rem_rate_ordered_spec(nodes = "actors")),
    c("goldfishKindRemCox", "goldfishAxisDyad", "goldfishKind")
  )
})

test_that("sender constructors are one-mode with nodes2 defaulting to nodes", {
  spec <- dynam_rate_spec(nodes = "actors")
  expect_false(spec$is_two_mode)
  expect_identical(spec$nodes2, "actors")
})

test_that("a two-mode sender spec carries its receiver side and the flag", {
  # A rate model is sender-indexed, but the network it reads still spans two
  # modes: the receiver side must survive so n2 (and rowSums dims) stay right.
  spec <- new_model_spec(
    "DyNAM",
    "rate",
    is_two_mode = TRUE,
    nodes = "actors",
    nodes2 = "events"
  )
  expect_true(inherits(spec, "goldfishAxisSender"))
  expect_true(spec$is_two_mode)
  expect_identical(spec$nodes, "actors")
  expect_identical(spec$nodes2, "events")
})

test_that("constructors store extra fields passed through dots", {
  spec <- rem_rate_spec(nodes = "actors", has_intercept = TRUE)
  expect_true(spec$has_intercept)
})

test_that("new_model_spec resolves the spec class from model and sub_model", {
  spec <- new_model_spec("DyNAM", "rate", nodes = "actors")
  expect_identical(class(spec)[1], "goldfishKindDnRate")
})

test_that("new_model_spec sender-indexed spec is not dyad-indexed", {
  spec <- new_model_spec("DyNAM", "rate_ordered", nodes = "actors")
  expect_true(inherits(spec, "goldfishAxisSender"))
  expect_false(inherits(spec, "goldfishAxisDyad"))
})

test_that("new_model_spec dyad-indexed spec is not sender-indexed", {
  spec <- new_model_spec("REM", "rate", nodes = "actors")
  expect_true(inherits(spec, "goldfishAxisDyad"))
  expect_false(inherits(spec, "goldfishAxisSender"))
})

test_that("new_model_spec accepts all 9 valid variant combinations", {
  combinations <- list(
    c("DyNAM", "rate"),
    c("DyNAM", "rate_ordered"),
    c("DyNAM", "choice"),
    c("DyNAM", "choice_coordination"),
    c("DyNAMi", "rate"),
    c("DyNAMi", "rate_ordered"),
    c("DyNAMi", "choice"),
    c("REM", "rate"),
    c("REM", "rate_ordered")
  )
  for (combination in combinations) {
    spec <- new_model_spec(combination[1], combination[2], nodes = "actors")
    expect_s3_class(spec, "goldfishKind")
  }
})

test_that("new_model_spec rejects invalid model and sub_model values", {
  expect_error(
    new_model_spec("SAOM", "rate", nodes = "actors"),
    "not a valid model"
  )
  expect_error(
    new_model_spec("REM", "choice_coordination", nodes = "actors"),
    "not a valid sub model"
  )
})

test_that("new_model_spec two-mode requires both node sets", {
  expect_error(
    new_model_spec(
      "DyNAM",
      "choice",
      is_two_mode = TRUE,
      nodes = "actors",
      nodes2 = NULL
    ),
    "nodes2"
  )
  expect_error(
    new_model_spec(
      "REM",
      "rate",
      is_two_mode = TRUE,
      nodes = "actors",
      nodes2 = "actors"
    ),
    "distinct node sets"
  )
  spec <- new_model_spec(
    "DyNAM",
    "choice",
    is_two_mode = TRUE,
    nodes = "actors",
    nodes2 = "clubs"
  )
  expect_true(spec$is_two_mode)
  expect_identical(spec$nodes2, "clubs")
})

test_that("every spec class carries the documented behavioral descriptor", {
  expected <- list(
    dynam_rate = list(
      axis = "sender",
      timing = "timed",
      likelihood = "poisson",
      input_shape = "standard",
      distribution = "exponential",
      fold_target = "active_sender",
      encoding = NA_character_
    ),
    dynam_rate_ordered = list(
      axis = "sender",
      timing = "ordinal",
      likelihood = "multinomial",
      input_shape = "standard",
      distribution = "exponential",
      fold_target = "active_sender",
      encoding = NA_character_
    ),
    dynam_choice = list(
      axis = "receiver_given_sender",
      timing = "ordinal",
      likelihood = "multinomial",
      input_shape = "standard",
      distribution = "exponential",
      fold_target = "active_dyad",
      encoding = "alter"
    ),
    dynam_choice_coord = list(
      axis = "dyad_symmetric",
      timing = "ordinal",
      likelihood = "coordination",
      input_shape = "standard",
      distribution = "exponential",
      fold_target = "active_dyad",
      encoding = "outer"
    ),
    dynami_rate = list(
      axis = "sender",
      timing = "timed",
      likelihood = "poisson",
      input_shape = "grouped",
      distribution = "exponential",
      fold_target = "active_sender",
      encoding = NA_character_
    ),
    dynami_rate_ordered = list(
      axis = "sender",
      timing = "ordinal",
      likelihood = "multinomial",
      input_shape = "grouped",
      distribution = "exponential",
      fold_target = "active_sender",
      encoding = NA_character_
    ),
    dynami_choice = list(
      axis = "receiver_given_sender",
      timing = "ordinal",
      likelihood = "multinomial",
      input_shape = "grouped",
      distribution = "exponential",
      fold_target = "active_dyad",
      encoding = "alter"
    ),
    rem_rate = list(
      axis = "dyad",
      timing = "timed",
      likelihood = "poisson",
      input_shape = "standard",
      distribution = "exponential",
      fold_target = "active_dyad",
      encoding = "outer"
    ),
    rem_rate_ordered = list(
      axis = "dyad",
      timing = "ordinal",
      likelihood = "multinomial",
      input_shape = "standard",
      distribution = "exponential",
      fold_target = "active_dyad",
      encoding = "outer"
    )
  )
  constructors <- list(
    dynam_rate = dynam_rate_spec,
    dynam_rate_ordered = dynam_rate_ordered_spec,
    dynam_choice = dynam_choice_spec,
    dynam_choice_coord = dynam_choice_coord_spec,
    dynami_rate = dynami_rate_spec,
    dynami_rate_ordered = dynami_rate_ordered_spec,
    dynami_choice = dynami_choice_spec,
    rem_rate = rem_rate_spec,
    rem_rate_ordered = rem_rate_ordered_spec
  )
  for (variant in names(expected)) {
    spec <- constructors[[variant]](nodes = "actors")
    want <- expected[[variant]]
    expect_identical(spec$behavior, want, info = variant)
    expect_identical(risk_set_axis(spec), want$axis, info = variant)
    expect_identical(behavior_timing(spec), want$timing, info = variant)
    expect_identical(behavior_likelihood(spec), want$likelihood, info = variant)
    expect_identical(
      behavior_input_shape(spec),
      want$input_shape,
      info = variant
    )
    expect_identical(
      risk_set_fold_target(spec),
      want$fold_target,
      info = variant
    )
    expect_identical(risk_set_encoding(spec), want$encoding, info = variant)
    # Symmetrization is the coordination likelihood's reduction of the dyad
    # grid to unordered pairs, so it follows that field and needs no other.
    expect_identical(
      risk_set_symmetrize(spec),
      identical(want$likelihood, "coordination"),
      info = variant
    )
  }
})

test_that("a spec carries one behavioral object, not two", {
  spec <- dynam_choice_spec(nodes = "actors")
  expect_setequal(
    names(spec),
    c("model", "sub_model", "is_two_mode", "nodes", "nodes2", "behavior")
  )
  # `model` and `sub_model` survive as provenance -- what the user asked for --
  # so their presence above is expected; a second descriptor beside `behavior`
  # is what must not come back.
  expect_false("risk_set" %in% names(spec))
})

test_that("an unmapped combination aborts rather than yielding NA fields", {
  # REM has no choice sub-model: reaching a consumer with a half-filled
  # descriptor is the failure mode this abort exists to prevent.
  expect_error(
    behavior_descriptor("goldfishAxisDyad", "REM", "choice", FALSE),
    "No behavioral descriptor is defined"
  )
  expect_error(
    behavior_descriptor("goldfishAxisSender", "DyNAM", "meeting", FALSE),
    "No behavioral descriptor is defined"
  )
  expect_error(
    behavior_descriptor("goldfishAxisPair", "DyNAM", "rate", FALSE),
    "must be one of"
  )
})

test_that("risk_set_is_dyadic tracks the axis (dyad and symmetric-dyad)", {
  expect_true(risk_set_is_dyadic(rem_rate_spec(nodes = "actors")))
  expect_true(risk_set_is_dyadic(rem_rate_ordered_spec(nodes = "actors")))
  expect_true(risk_set_is_dyadic(dynam_choice_coord_spec(nodes = "actors")))
  expect_false(risk_set_is_dyadic(dynam_choice_spec(nodes = "actors")))
  expect_false(risk_set_is_dyadic(dynam_rate_spec(nodes = "actors")))
})

test_that("coordination symmetrize follows one-mode vs two-mode", {
  # One-mode coordination symmetrizes the dyad for the mutual likelihood; a
  # two-mode risk set (rejected before construction elsewhere) would not.
  one_mode <- dynam_choice_coord_spec(nodes = "actors")
  expect_identical(risk_set_axis(one_mode), "dyad_symmetric")
  expect_true(risk_set_symmetrize(one_mode))
  two_mode <- dynam_choice_coord_spec(
    is_two_mode = TRUE,
    nodes = "actors",
    nodes2 = "clubs"
  )
  expect_identical(risk_set_axis(two_mode), "dyad")
  expect_false(risk_set_symmetrize(two_mode))
})

test_that("estimate_dynam constructs and forwards the typed spec", {
  fitChoice <- estimate_dynam(
    depNetwork ~ inertia + recip,
    sub_model = "choice",
    data = dataTest
  )
  expect_s3_class(fitChoice$model_spec, "goldfishKindDnChoice")
  prepCoord <- estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice_coordination",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepCoord$model_spec, "goldfishKindDnCoord")
  prepRate <- estimate_dynam(
    depNetwork ~ 1 + indeg,
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepRate$model_spec, "goldfishKindDnRate")
  expect_true(prepRate$model_spec$has_intercept)
  # A no-intercept `rate` formula gets the intercept added (waiting-time model),
  # not the ordinal spec.
  prepRateAdded <- suppressMessages(estimate_dynam(
    depNetwork ~ indeg,
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  ))
  expect_s3_class(prepRateAdded$model_spec, "goldfishKindDnRate")
  expect_true(prepRateAdded$model_spec$has_intercept)
})

test_that("explicit rate_ordered is ordinal; implicit rate adds an intercept", {
  prepExplicit <- estimate_dynam(
    depNetwork ~ indeg,
    sub_model = "rate_ordered",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepExplicit$model_spec, "goldfishKindDnCox")
  expect_identical(prepExplicit$sub_model, "rate")
  expect_false(isTRUE(prepExplicit$model_spec$has_intercept))
  # Dropping auto-ordinal: a no-intercept `rate` formula is now a waiting-time
  # model with the intercept added, NOT the ordinal spec.
  prepImplicit <- suppressMessages(estimate_dynam(
    depNetwork ~ indeg,
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  ))
  expect_s3_class(prepImplicit$model_spec, "goldfishKindDnRate")
  expect_true(prepImplicit$model_spec$has_intercept)
  expect_warning(
    estimate_dynam(
      depNetwork ~ 1 + indeg,
      sub_model = "rate_ordered",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "ignores the time intercept"
  )
})

test_that("estimate_rem accepts explicit rate and rate_ordered sub_models", {
  prepRate <- estimate_rem(
    depNetwork ~ 1 + inertia,
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepRate$model_spec, "goldfishKindRemRate")
  expect_identical(prepRate$sub_model, "choice")
  prepOrdered <- estimate_rem(
    depNetwork ~ inertia,
    sub_model = "rate_ordered",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepOrdered$model_spec, "goldfishKindRemCox")
})

test_that("estimate_rem constructs and forwards the typed spec", {
  fitRem <- estimate_rem(depNetwork ~ 1 + inertia, data = dataTest)
  expect_s3_class(fitRem$model_spec, "goldfishKindRemRate")
  expect_true(fitRem$model_spec$has_intercept)
  # A no-intercept REM rate formula adds the intercept (waiting times), not the
  # ordinal spec; ordinal requires explicit `rate_ordered`.
  prepRemAdded <- suppressMessages(estimate_rem(
    depNetwork ~ inertia,
    data = dataTest,
    preprocessing_only = TRUE
  ))
  expect_s3_class(prepRemAdded$model_spec, "goldfishKindRemRate")
  expect_true(prepRemAdded$model_spec$has_intercept)
})

test_that("estimate_dynami constructs and forwards the typed spec", {
  snet <- as_goldfish(make_stocnet_fixture_dynami())
  prepRate <- estimate_dynami(
    interactions ~ 1 + intercept(interactions, joining = -1),
    sub_model = "rate",
    data = snet,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepRate$model_spec, "goldfishKindDniRate")
  prepChoice <- estimate_dynami(
    interactions ~ inertia(past, weighted = TRUE, sub_type = "count"),
    sub_model = "choice",
    data = snet,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepChoice$model_spec, "goldfishKindDniChoice")
})

test_that("new_model_spec sender-indexed specs carry a two-mode receiver", {
  expect_no_warning(
    spec <- new_model_spec(
      "DyNAM",
      "rate",
      is_two_mode = TRUE,
      nodes = "actors",
      nodes2 = "clubs"
    )
  )
  expect_true(spec$is_two_mode)
  expect_identical(spec$nodes2, "clubs")
})
