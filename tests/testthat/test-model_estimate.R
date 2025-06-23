test_that("preprocess init", {
  formulaTest <- depNetwork ~ outdeg(networkState, weighted = TRUE) +
    outdeg(networkExog, weighted = TRUE) + inertia + recip
  preproData <- estimate_wrapper(
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
    estimate_wrapper(formulaTest, data = dataTest)[toCompare],
    estimate_wrapper(formulaTest, data = dataTest,
      preprocessing_init = preproData)[toCompare]
  )
  formulaTest <- depNetwork ~ 1 + outdeg(networkState, weighted = TRUE) +
    outdeg(networkExog, weighted = TRUE)
  preproData <- estimate_wrapper(
    formulaTest,
    model = "DyNAM", sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(
    estimate_wrapper(formulaTest, data = dataTest, sub_model = "rate")[toCompare],
    estimate_wrapper(
      formulaTest, data = dataTest,
      sub_model = "rate", preprocessing_init = preproData
    )[toCompare]
  )
})
