test_that("constructors return the documented class vectors", {
  expect_identical(
    class(dynam_rate_spec(nodes = "actors")),
    c("dynam_rate_spec", "sender_spec", "model_spec")
  )
  expect_identical(
    class(dynam_rate_ordered_spec(nodes = "actors")),
    c("dynam_rate_ordered_spec", "sender_spec", "model_spec")
  )
  expect_identical(
    class(dynam_choice_spec(nodes = "actors")),
    c("dynam_choice_spec", "dyad_spec", "model_spec")
  )
  expect_identical(
    class(dynam_choice_coord_spec(nodes = "actors")),
    c("dynam_choice_coord_spec", "dyad_spec", "model_spec")
  )
  expect_identical(
    class(dynami_rate_spec(nodes = "actors")),
    c("dynami_rate_spec", "sender_spec", "model_spec")
  )
  expect_identical(
    class(dynami_rate_ordered_spec(nodes = "actors")),
    c("dynami_rate_ordered_spec", "sender_spec", "model_spec")
  )
  expect_identical(
    class(dynami_choice_spec(nodes = "actors")),
    c("dynami_choice_spec", "dyad_spec", "model_spec")
  )
  expect_identical(
    class(rem_rate_spec(nodes = "actors")),
    c("rem_rate_spec", "dyad_spec", "model_spec")
  )
  expect_identical(
    class(rem_rate_ordered_spec(nodes = "actors")),
    c("rem_rate_ordered_spec", "dyad_spec", "model_spec")
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
  expect_true(inherits(spec, "sender_spec"))
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
  expect_identical(class(spec)[1], "dynam_rate_spec")
})

test_that("new_model_spec sender-indexed spec is not dyad-indexed", {
  spec <- new_model_spec("DyNAM", "rate_ordered", nodes = "actors")
  expect_true(inherits(spec, "sender_spec"))
  expect_false(inherits(spec, "dyad_spec"))
})

test_that("new_model_spec dyad-indexed spec is not sender-indexed", {
  spec <- new_model_spec("REM", "rate", nodes = "actors")
  expect_true(inherits(spec, "dyad_spec"))
  expect_false(inherits(spec, "sender_spec"))
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
    expect_s3_class(spec, "model_spec")
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

test_that("new_model_spec accepts engine = 'default' and rejects others", {
  expect_no_error(
    new_model_spec("REM", "rate", nodes = "actors", engine = "default")
  )
  expect_error(
    new_model_spec("REM", "rate", nodes = "actors", engine = "incremental"),
    "not yet supported"
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

test_that("legacy_model_type maps every spec class to its legacy string", {
  expected <- c(
    dynam_rate = "DyNAM-M-Rate",
    dynam_rate_ordered = "DyNAM-M-Rate-ordered",
    dynam_choice = "DyNAM-M",
    dynam_choice_coord = "DyNAM-MM",
    dynami_rate = "DyNAM-M-Rate",
    dynami_rate_ordered = "DyNAM-M-Rate-ordered",
    dynami_choice = "DyNAM-M",
    rem_rate = "REM",
    rem_rate_ordered = "REM-ordered"
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
    expect_identical(
      legacy_model_type(constructors[[variant]](nodes = "actors")),
      unname(expected[variant])
    )
  }
  expect_error(
    legacy_model_type(structure(list(), class = "unknown_spec")),
    "No legacy model type"
  )
})

test_that("every spec class carries the documented risk-set descriptor", {
  expected <- list(
    dynam_rate = list(
      axis = "sender",
      fold_target = "active_sender",
      encoding = NA_character_,
      symmetrize = FALSE
    ),
    dynam_rate_ordered = list(
      axis = "sender",
      fold_target = "active_sender",
      encoding = NA_character_,
      symmetrize = FALSE
    ),
    dynam_choice = list(
      axis = "receiver_given_sender",
      fold_target = "active_dyad",
      encoding = "alter",
      symmetrize = FALSE
    ),
    dynam_choice_coord = list(
      axis = "dyad_symmetric",
      fold_target = "active_dyad",
      encoding = "outer",
      symmetrize = TRUE
    ),
    dynami_rate = list(
      axis = "sender",
      fold_target = "active_sender",
      encoding = NA_character_,
      symmetrize = FALSE
    ),
    dynami_rate_ordered = list(
      axis = "sender",
      fold_target = "active_sender",
      encoding = NA_character_,
      symmetrize = FALSE
    ),
    dynami_choice = list(
      axis = "receiver_given_sender",
      fold_target = "active_dyad",
      encoding = "alter",
      symmetrize = FALSE
    ),
    rem_rate = list(
      axis = "dyad",
      fold_target = "active_dyad",
      encoding = "outer",
      symmetrize = FALSE
    ),
    rem_rate_ordered = list(
      axis = "dyad",
      fold_target = "active_dyad",
      encoding = "outer",
      symmetrize = FALSE
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
    expect_identical(spec$risk_set, expected[[variant]], info = variant)
    expect_identical(risk_set_axis(spec), expected[[variant]]$axis)
    expect_identical(
      risk_set_fold_target(spec),
      expected[[variant]]$fold_target
    )
    expect_identical(risk_set_encoding(spec), expected[[variant]]$encoding)
    expect_identical(
      risk_set_symmetrize(spec),
      expected[[variant]]$symmetrize
    )
  }
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
  expect_s3_class(fitChoice$model_spec, "dynam_choice_spec")
  prepCoord <- estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice_coordination",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepCoord$model_spec, "dynam_choice_coord_spec")
  prepRate <- estimate_dynam(
    depNetwork ~ 1 + indeg,
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepRate$model_spec, "dynam_rate_spec")
  expect_true(prepRate$model_spec$has_intercept)
  expect_warning(
    prepRateOrdered <- estimate_dynam(
      depNetwork ~ indeg,
      sub_model = "rate",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "rate_ordered"
  )
  expect_s3_class(prepRateOrdered$model_spec, "dynam_rate_ordered_spec")
})

test_that("estimate_dynam accepts the explicit rate_ordered sub_model", {
  prepExplicit <- estimate_dynam(
    depNetwork ~ indeg,
    sub_model = "rate_ordered",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepExplicit$model_spec, "dynam_rate_ordered_spec")
  expect_identical(prepExplicit$sub_model, "rate")
  prepImplicit <- suppressWarnings(estimate_dynam(
    depNetwork ~ indeg,
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  ))
  expect_equal(prepExplicit, prepImplicit)
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
  expect_s3_class(prepRate$model_spec, "rem_rate_spec")
  expect_identical(prepRate$sub_model, "choice")
  prepOrdered <- estimate_rem(
    depNetwork ~ inertia,
    sub_model = "rate_ordered",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepOrdered$model_spec, "rem_rate_ordered_spec")
})

test_that("estimate_rem constructs and forwards the typed spec", {
  fitRem <- estimate_rem(depNetwork ~ 1 + inertia, data = dataTest)
  expect_s3_class(fitRem$model_spec, "rem_rate_spec")
  expect_true(fitRem$model_spec$has_intercept)
  prepRemOrdered <- estimate_rem(
    depNetwork ~ inertia,
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepRemOrdered$model_spec, "rem_rate_ordered_spec")
})

test_that("estimate_dynami constructs and forwards the typed spec", {
  snet <- as_goldfish(make_stocnet_fixture_dynami())
  prepRate <- estimate_dynami(
    interactions ~ 1 + intercept(interactions, joining = -1),
    sub_model = "rate",
    data = snet,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepRate$model_spec, "dynami_rate_spec")
  prepChoice <- estimate_dynami(
    interactions ~ inertia(past, weighted = TRUE, sub_type = "count"),
    sub_model = "choice",
    data = snet,
    preprocessing_only = TRUE
  )
  expect_s3_class(prepChoice$model_spec, "dynami_choice_spec")
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
