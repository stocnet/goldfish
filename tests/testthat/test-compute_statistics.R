test_that("the compute_stats name is gone, with no stub", {
  # Deleted outright rather than deprecated: the name only ever existed in the
  # unreleased 2.0.0 development line, so no released user is served by a stub.
  expect_false("compute_stats" %in% getNamespaceExports("goldfish"))
  expect_false(exists("compute_stats", envir = asNamespace("goldfish")))
})

test_that("max_length bounds the produced statistic-column names", {
  gathered <- compute_statistics(
    depNetwork ~ inertia(networkState) + recip,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather",
    max_length = 8L
  )
  expect_lte(max(nchar(gathered$namesEffects)), 8L)
  expect_identical(anyDuplicated(gathered$namesEffects), 0L)
})

test_that("compute_statistics returns a preprocessed.goldfish object", {
  prep <- compute_statistics(
    depNetwork ~ inertia + recip,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  expect_s3_class(prep, "preprocessed.goldfish")
  expect_s3_class(prep$model_spec, "dynam_choice_spec")
})

test_that("compute_statistics matches the estimate preprocessing only output", {
  formulaTest <- depNetwork ~ inertia + recip
  prep <- compute_statistics(
    formulaTest,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  prepEstimate <- estimate_dynam(
    formulaTest,
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(prep, prepEstimate)
})

test_that("compute_statistics output is usable for estimation", {
  formulaTest <- depNetwork ~ inertia + recip
  prep <- compute_statistics(
    formulaTest,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  fitInit <- estimate_dynam(
    formulaTest,
    sub_model = "choice",
    data = dataTest,
    preprocessed = prep
  )
  fitDirect <- estimate_dynam(
    formulaTest,
    sub_model = "choice",
    data = dataTest
  )
  expect_equal(coef(fitInit), coef(fitDirect))
})

test_that("compute_statistics validates the output argument", {
  gathered <- compute_statistics(
    depNetwork ~ inertia,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  expect_true(!is.null(gathered$stat_all_events))
  expect_true(!is.null(gathered$selected))
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "DyNAM",
      sub_model = "choice",
      output = "db"
    ),
    "DBI connection"
  )
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "DyNAM",
      sub_model = "choice",
      output = "data.frame"
    )
  )
})

test_that("compute_statistics validates model and sub_model values", {
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "SAOM",
      sub_model = "choice"
    )
  )
  expect_error(
    compute_statistics(
      depNetwork ~ inertia,
      data = dataTest,
      model = "REM",
      sub_model = "choice_coordination"
    )
  )
})

test_that("preprocessed objects carry the format version", {
  prep <- compute_statistics(
    depNetwork ~ inertia,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  expect_identical(prep$version, 4L)
  expect_identical(prep$active_dyad_encoding, "alter")
  oldFormat <- prep
  oldFormat$version <- 3L
  expect_error(
    estimate_dynam(
      depNetwork ~ inertia,
      sub_model = "choice",
      data = dataTest,
      preprocessed = oldFormat
    ),
    "outdated preprocessing format"
  )
})
