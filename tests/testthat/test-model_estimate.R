test_that("preprocess init", {
  formulaTest <- depNetwork ~ outdeg(networkState, weighted = TRUE) +
    outdeg(networkExog, weighted = TRUE) + inertia + recip
  preproData <- estimate(
    formulaTest,
    model = "DyNAM", subModel = "choice",
    preprocessingOnly = TRUE
  )
  toCompare <- c(
    "parameters", "standardErrors", "logLikelihood", "finalScore",
    "finalInformationMatrix", "convergence", "nIterations", "nEvents", "names",
    "formula", "model", "subModel", "rightCensored", "nParams"
  )
  expect_equal(
    estimate(formulaTest)[toCompare],
    estimate(formulaTest, preprocessingInit = preproData)[toCompare]
  )
  formulaTest <- depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
    outdeg(networkExog, weighted = TRUE)
  preproData <- estimate(
    formulaTest,
    model = "DyNAM", subModel = "rate",
    preprocessingOnly = TRUE
  )
  expect_equal(
    estimate(formulaTest, subModel = "rate")[toCompare],
    estimate(
      formulaTest,
      subModel = "rate", preprocessingInit = preproData
    )[toCompare]
  )
})
