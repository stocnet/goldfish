test_that("compute_stats returns a preprocessed.goldfish object", {
  prep <- compute_stats(
    depNetwork ~ inertia + recip,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  expect_s3_class(prep, "preprocessed.goldfish")
  expect_s3_class(prep$model_spec, "dynam_choice_spec")
})

test_that("compute_stats matches the estimate preprocessing only output", {
  formulaTest <- depNetwork ~ inertia + recip
  prep <- compute_stats(
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

test_that("compute_stats output is usable for estimation", {
  formulaTest <- depNetwork ~ inertia + recip
  prep <- compute_stats(
    formulaTest,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  fitInit <- estimate_dynam(
    formulaTest,
    sub_model = "choice",
    data = dataTest,
    preprocessing_init = prep
  )
  fitDirect <- estimate_dynam(
    formulaTest,
    sub_model = "choice",
    data = dataTest
  )
  expect_equal(coef(fitInit), coef(fitDirect))
})

test_that("compute_stats validates the output argument", {
  gathered <- compute_stats(
    depNetwork ~ inertia,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice",
    output = "gather"
  )
  expect_true(!is.null(gathered$stat_all_events))
  expect_true(!is.null(gathered$selected))
  expect_error(
    compute_stats(
      depNetwork ~ inertia,
      data = dataTest,
      model = "DyNAM",
      sub_model = "choice",
      output = "db"
    ),
    "DBI connection"
  )
  expect_error(
    compute_stats(
      depNetwork ~ inertia,
      data = dataTest,
      model = "DyNAM",
      sub_model = "choice",
      output = "data.frame"
    )
  )
})

test_that("compute_stats validates model and sub_model values", {
  expect_error(
    compute_stats(
      depNetwork ~ inertia,
      data = dataTest,
      model = "SAOM",
      sub_model = "choice"
    )
  )
  expect_error(
    compute_stats(
      depNetwork ~ inertia,
      data = dataTest,
      model = "REM",
      sub_model = "choice_coordination"
    )
  )
})

test_that("preprocessed objects carry the format version", {
  prep <- compute_stats(
    depNetwork ~ inertia,
    data = dataTest,
    model = "DyNAM",
    sub_model = "choice"
  )
  expect_identical(prep$version, 3L)
  oldFormat <- prep
  oldFormat$version <- NULL
  expect_error(
    estimate_dynam(
      depNetwork ~ inertia,
      sub_model = "choice",
      data = dataTest,
      preprocessing_init = oldFormat
    ),
    "outdated preprocessing format"
  )
})
