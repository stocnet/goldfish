test_that("preprocess init", {
  formulaTest <- depNetwork ~ outdeg(networkState, weighted = TRUE) +
    outdeg(networkExog, weighted = TRUE) + inertia + recip
  preproData <- estimate(
    formulaTest,
    model = "DyNAM", sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  toCompare <- c(
    "parameters", "standardErrors", "logLikelihood", "finalScore",
    "finalInformationMatrix", "convergence", "nIterations", "nEvents", "names",
    "formula", "model", "subModel", "rightCensored", "nParams"
  )
  expect_equal(
    estimate(formulaTest, data = dataTest)[toCompare],
    estimate(formulaTest, data = dataTest,
      preprocessing_init = preproData)[toCompare]
  )
  formulaTest <- depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
    outdeg(networkExog, weighted = TRUE)
  preproData <- estimate(
    formulaTest,
    model = "DyNAM", sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(
    estimate(formulaTest, data = dataTest, sub_model = "rate")[toCompare],
    estimate(
      formulaTest, data = dataTest,
      sub_model = "rate", preprocessing_init = preproData
    )[toCompare]
  )
})
