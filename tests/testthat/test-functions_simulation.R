test_that("Args check", {
  expect_error(
    simulate(
      depNetwork ~ inertia(networkState),
      c(1),
      model = "dynam")
  )
  expect_error(
    simulate(
      depNetwork ~ inertia(networkState),
      c(1),
      model = "DyNAM",
      subModel = "choice_coordination")
  )
  expect_error(
    simulate(
      "depNetwork ~ inertia(networkState)",
      c("1"))
  )
  expect_error(
    simulate(
      depNetwork ~ indeg(networkState),
      c(1, 2),
      model = "REM"
    )
  )
  expect_error(
    simulate(
      depNetwork ~ indeg(networkState),
      c(1),
      depNetwork ~ inertia(networkState),
      c(1, 2)
    )
  )
  expect_error(
    simulate(
      depNetwork ~ 1 + indeg(networkState),
      c(1, 1),
      dependent.depevents_DyNAMi ~ inertia,
      c(1))
  )
  expect_error(
    simulate(
      depNetwork ~ indeg(networkState),
      c(1),
      depNetwork ~ inertia,
      c(1))
  )
  expect_warning(
    GatherPreprocessing(
      depNetwork ~ inertia(networkState),
      preprocessArgs = list(opportunitiesList = 1))
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
      depNetwork ~ 1 + inertia(networkState), subModel = "smh"
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
