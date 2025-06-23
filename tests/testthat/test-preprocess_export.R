test_that("Args check", {
  expect_error(
    gather_model_data(
      depNetwork ~ inertia(networkState, ignore_repetitions = TRUE),
      data = dataTest
    )
  )
  expect_warning(
    gather_model_data(
      depNetwork ~ 1 + inertia(networkState),
      data = dataTest
    )
  )
  expect_error(
    gather_model_data(
      depNetwork ~ 1 + inertia(networkState),
      model = "smh",
      data = dataTest)
  )
  expect_error(
    gather_model_data(
      depNetwork ~ 1 + inertia(networkState),
      subModel = "smh",
      data = dataTest
    )
  )
})
test_that("Printing", {
  expect_output(
    gather_model_data(
      depNetwork ~ inertia(networkState),,
      data = dataTest,
      progress = TRUE),
    "Preprocessing events."
  )
})
test_that("Output", {
  out <- gather_model_data(
    depNetwork ~ inertia(networkState),
    data = dataTest
  )
  expect_type(out, "list")
  expect_length(out, 8)
})
