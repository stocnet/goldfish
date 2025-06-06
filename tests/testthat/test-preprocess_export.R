test_that("Args check", {
  expect_warning(
    gather_model_data(
      depNetwork ~ inertia(networkState),
      control_preprocessing = list(smth = 1)
    )
  )
  expect_warning(
    gather_model_data(
      depNetwork ~ inertia(networkState),
      control_preprocessing = list(opportunitiesList = 1)
    )
  )
  expect_error(
    gather_model_data(
      depNetwork ~ inertia(networkState, ignore_repetitions = TRUE)
    )
  )
  expect_warning(
    gather_model_data(depNetwork ~ 1 + inertia(networkState))
  )
  expect_error(
    gather_model_data(depNetwork ~ 1 + inertia(networkState), model = "smh")
  )
  expect_error(
    gather_model_data(
      depNetwork ~ 1 + inertia(networkState),
      subModel = "smh"
    )
  )
})
test_that("Printing", {
  expect_output(
    gather_model_data(depNetwork ~ inertia(networkState), progress = TRUE),
    "Preprocessing events."
  )
})
test_that("Output", {
  out <- gather_model_data(depNetwork ~ inertia(networkState))
  expect_type(out, "list")
  expect_length(out, 8)
})
