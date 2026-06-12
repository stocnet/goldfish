test_that("ignore_repetitions = TRUE aborts before preprocessing", {
  expect_error(
    estimate_dynam(
      depNetwork ~ inertia + indeg(networkState, ignore_repetitions = TRUE),
      sub_model = "choice",
      data = dataTest
    ),
    "ignore_repetitions"
  )
  expect_error(
    estimate_rem(
      depNetwork ~ inertia(networkState, ignore_repetitions = TRUE),
      data = dataTest
    ),
    "ignore_repetitions"
  )
  expect_error(
    compute_stats(
      depNetwork ~ inertia + indeg(networkState, ignore_repetitions = TRUE),
      data = dataTest,
      model = "DyNAM", sub_model = "choice"
    ),
    "ignore_repetitions"
  )
})
