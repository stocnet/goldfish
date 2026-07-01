# Per-(model, sub_model) main-effect validity matrix (design D3, group 3).

test_that("effect_variation classifies effects by variation axis", {
  expect_equal(effect_variation("global", ""), "global")
  expect_equal(effect_variation("ego", ""), "ego")
  expect_equal(effect_variation("alter", ""), "alter")
  expect_equal(effect_variation("indeg", "ego"), "ego")
  expect_equal(effect_variation("outdeg", "alter"), "alter")
  # a degree effect with no explicit type is model-dependent, never rejected
  expect_equal(effect_variation("indeg", ""), "degree")
  expect_equal(effect_variation("inertia", ""), "other")
})

test_that("validate_effects enforces the D3 rule matrix when estimating", {
  ok <- function(model, sm, names, types = rep("", length(names))) {
    expect_no_error(validate_effects(
      model,
      sm,
      names,
      types,
      estimating = TRUE
    ))
  }
  bad <- function(model, sm, names, types = rep("", length(names))) {
    expect_error(
      validate_effects(model, sm, names, types, estimating = TRUE),
      "Unsupported main effect"
    )
  }

  # DyNAM choice / choice_coordination: reject global + ego, accept dyadic/alter
  ok("DyNAM", "choice", c("inertia", "recip"))
  ok("DyNAM", "choice", c("inertia", "indeg")) # default degree = alter here
  bad("DyNAM", "choice", c("inertia", "global"))
  bad("DyNAM", "choice", c("inertia", "indeg"), c("", "ego"))
  bad("DyNAM", "choice_coordination", "global")

  # DyNAM rate: ego/global allowed, alter has no receiver axis
  ok("DyNAM", "rate", c("indeg", "global"))
  ok("DyNAM", "rate", c("indeg", "ego"))
  bad("DyNAM", "rate", c("indeg", "alter"))

  # DyNAM rate_ordered: also drops the constant global
  ok("DyNAM", "rate_ordered", c("indeg", "ego"))
  bad("DyNAM", "rate_ordered", c("indeg", "global"))

  # REM rate accepts everything; rate_ordered drops only global (alter is valid)
  ok("REM", "rate", c("inertia", "global", "alter"))
  ok("REM", "rate_ordered", c("inertia", "alter"))
  bad("REM", "rate_ordered", c("inertia", "global"))
})

test_that("validate_effects reports every offender at once", {
  expect_error(
    validate_effects(
      "DyNAM",
      "choice",
      c("global", "inertia", "indeg"),
      c("", "", "ego"),
      estimating = TRUE
    ),
    "global"
  )
  err <- tryCatch(
    validate_effects(
      "DyNAM",
      "choice",
      c("global", "indeg"),
      c("", "ego"),
      estimating = TRUE
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "global")
  expect_match(err, "ego-perspective")
})

test_that("preprocessing keeps computable columns; only estimation rejects them", {
  # ego in choice is computable (design D3 design-column use), so estimating =
  # FALSE permits it; estimating = TRUE rejects the unidentified main effect.
  expect_no_error(
    validate_effects("DyNAM", "choice", "indeg", "ego", estimating = FALSE)
  )
  expect_error(
    validate_effects("DyNAM", "choice", "indeg", "ego", estimating = TRUE),
    "Unsupported main effect"
  )
  # global in choice is computable too (task 1.5): permitted in preprocessing,
  # rejected only when estimating.
  expect_no_error(
    validate_effects("DyNAM", "choice", "global", "", estimating = FALSE)
  )
  expect_error(
    validate_effects("DyNAM", "choice", "global", "", estimating = TRUE),
    "Unsupported main effect"
  )
})

test_that("choice type = 'ego' is rejected at estimation but computable (D3)", {
  # estimation rejects the unidentified ego main effect ...
  expect_error(
    suppressWarnings(estimate_dynam(
      depNetwork ~ inertia + indeg(networkState, type = "ego"),
      sub_model = "choice",
      data = dataTest
    )),
    "Unsupported main effect"
  )
  # ... but the statistic is still produced by compute_stats (a design column).
  prep <- compute_stats(
    depNetwork ~ inertia + indeg(networkState, type = "ego"),
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  expect_s3_class(prep, "preprocessed.goldfish")
})

test_that("REM rejects a global main effect only in the ordinal sub-model", {
  seasons <- make_global_attributes(data.frame(winter = 0))
  season_change <- data.frame(time = 15, replace = 1)
  seasons <- link_events(seasons, season_change)
  dataGlobal <- make_data(depNetwork, seasons)

  # REM rate (dyadic hazard) accepts a global main effect ...
  expect_no_error(suppressWarnings(estimate_rem(
    depNetwork ~ 1 + inertia + global(seasons$winter),
    sub_model = "rate",
    data = dataGlobal
  )))
  # ... the ordinal case does not (constant across dyads cancels).
  expect_error(
    suppressWarnings(estimate_rem(
      depNetwork ~ inertia + global(seasons$winter),
      sub_model = "rate_ordered",
      data = dataGlobal
    )),
    "Unsupported main effect"
  )
})

test_that("choice global is computable and equals the REM expansion (task 1.5)", {
  seasons <- make_global_attributes(data.frame(winter = 0))
  season_change <- data.frame(time = 15, replace = 1)
  seasons <- link_events(seasons, season_change)
  dataGlobal <- make_data(depNetwork, seasons)
  form <- depNetwork ~ global(seasons$winter)

  choice <- compute_stats(
    form,
    data = dataGlobal,
    model = "DyNAM",
    sub_model = "choice"
  )
  # REM rate_ordered uses the same dependent-only dyad loop + the same global
  # effect functions the new choice wrappers delegate to, so the statistic is
  # identical (mirrors the Group-1 ego equivalence, task 1.3).
  rem <- compute_stats(
    form,
    data = dataGlobal,
    model = "REM",
    sub_model = "rate_ordered"
  )
  expect_s3_class(choice, "preprocessed.goldfish")
  expect_equal(choice$initialStats, rem$initialStats, tolerance = 1e-6)
  expect_equal(
    ReducePreprocess(choice),
    ReducePreprocess(rem),
    tolerance = 1e-6
  )
  # estimation still rejects the bare global main effect (not identified).
  expect_error(
    estimate_dynam(form, sub_model = "choice", data = dataGlobal),
    "Unsupported main effect"
  )
})
