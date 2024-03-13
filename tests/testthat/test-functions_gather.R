test_that("Args check", {
  expect_warning(
    GatherPreprocessing(
      depNetwork ~ inertia(networkState),
      preprocessArgs = list(smth = 1)
    )
  )
  expect_warning(
    GatherPreprocessing(
      depNetwork ~ inertia(networkState),
      preprocessArgs = list(opportunitiesList = 1)
    )
  )
  expect_error(
    GatherPreprocessing(depNetwork ~ inertia(networkState, ignoreRep = TRUE))
  )
  expect_warning(
    GatherPreprocessing(depNetwork ~ 1 + inertia(networkState))
  )
  expect_error(
    GatherPreprocessing(depNetwork ~ 1 + inertia(networkState), model = "smh")
  )
  expect_error(
    GatherPreprocessing(
      depNetwork ~ 1 + inertia(networkState),
      subModel = "smh"
    )
  )
})
test_that("Printing", {
  expect_output(
    GatherPreprocessing(depNetwork ~ inertia(networkState), progress = TRUE),
    "Preprocessing events."
  )
})
test_that("Output", {
  out <- GatherPreprocessing(depNetwork ~ inertia(networkState))
  expect_type(out, "list")
  expect_length(out, 8)
})
