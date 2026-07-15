test_that("DyNAM rate sub_model with no-intercept formula warns", {
  expect_warning(
    prep <- estimate_dynam(
      depNetwork ~ indeg,
      sub_model = "rate",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "rate_ordered"
  )
  expect_s3_class(prep$model_spec, "dynam_rate_ordered_spec")
  expect_warning(
    compute_stats(
      depNetwork ~ indeg,
      data = dataTest,
      model = "DyNAM",
      sub_model = "rate"
    ),
    "rate_ordered"
  )
})

test_that("DyNAM rate sub_model with intercept formula does not warn", {
  expect_no_warning(
    estimate_dynam(
      depNetwork ~ 1 + indeg,
      sub_model = "rate",
      data = dataTest,
      preprocessing_only = TRUE
    )
  )
  expect_no_warning(
    estimate_dynam(
      depNetwork ~ indeg,
      sub_model = "rate_ordered",
      data = dataTest,
      preprocessing_only = TRUE
    )
  )
})

test_that("REM choice sub_model warns and behaves as rate", {
  expect_warning(
    prep <- estimate_rem(
      depNetwork ~ 1 + inertia,
      sub_model = "choice",
      data = dataTest,
      preprocessing_only = TRUE
    ),
    "rate"
  )
  expect_s3_class(prep$model_spec, "rem_rate_spec")
  expect_warning(
    compute_stats(
      depNetwork ~ 1 + inertia,
      data = dataTest,
      model = "REM",
      sub_model = "choice"
    ),
    "rate"
  )
  prepRate <- estimate_rem(
    depNetwork ~ 1 + inertia,
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(prep, prepRate)
})

test_that("REM rate and rate_ordered sub_models do not warn", {
  expect_no_warning(
    estimate_rem(
      depNetwork ~ 1 + inertia,
      sub_model = "rate",
      data = dataTest,
      preprocessing_only = TRUE
    )
  )
  expect_no_warning(
    estimate_rem(
      depNetwork ~ inertia,
      sub_model = "rate_ordered",
      data = dataTest,
      preprocessing_only = TRUE
    )
  )
})
