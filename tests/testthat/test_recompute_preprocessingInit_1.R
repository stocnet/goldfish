context("Diff preprocessingInit objects(Social Evolution)")

test_that("Using DyNAM-choice with data Social Evolution", {
  prep <- estimate(callsDependent ~ inertia + recip,
                   model = "DyNAM", subModel = "choice", preprocessingOnly = TRUE, silent = TRUE)
  time_1_withPreprocessingInit <- system.time(
    res_1_withPreprocessingInit <- estimate(callsDependent ~ inertia + recip + trans,
                                            model = "DyNAM", subModel = "choice",
                                            preprocessingInit = prep, silent = TRUE)
  )
  time_2_withPreprocessingInit <- system.time(
    res_2_withPreprocessingInit <- estimate(callsDependent ~ inertia,
                                            model = "DyNAM", subModel = "choice",
                                            preprocessingInit = prep, silent = TRUE)
  )
  time_1 <- system.time(
    res_1 <- estimate(callsDependent ~ inertia + recip + trans, model = "DyNAM", subModel = "choice", silent = TRUE)
  )
  time_2 <- system.time(
    res_2 <- estimate(callsDependent ~ inertia, model = "DyNAM", subModel = "choice", silent = TRUE)
  )
  # Test the correctness
  expect_equal(res_1$parameters, res_1_withPreprocessingInit$parameters) # They should be the same
  expect_equal(res_2$parameters, res_2_withPreprocessingInit$parameters) # They should be the same
  # Test whether it's faster
  # expect_true(as.numeric(time_1_withPreprocessingInit[3]) <= as.numeric(time_1[3]) )
  # using PreprocessingInit is not slower
  # expect_true(as.numeric(time_2_withPreprocessingInit[3]) <= as.numeric(time_2[3]) )
  #using PreprocessingInit is not slower
})

test_that("Using DyNAM-choice with data Social Evolution", {
  prep <- estimate(callsDependent ~ inertia + recip, model = "DyNAM", subModel = "choice",
                   preprocessingOnly = TRUE, silent = TRUE)
  time_1_withPreprocessingInit <- system.time(
    res_1_withPreprocessingInit <- estimate(callsDependent ~ inertia + recip + trans,
                                            model = "DyNAM", subModel = "choice",
                                            preprocessingInit = prep, silent = TRUE)
  )
  time_2_withPreprocessingInit <- system.time(
    res_2_withPreprocessingInit <- estimate(callsDependent ~ inertia,
                                            model = "DyNAM", subModel = "choice",
                                            preprocessingInit = prep, silent = TRUE)
  )
  time_1 <- system.time(
    res_1 <- estimate(callsDependent ~ inertia + recip + trans, model = "DyNAM", subModel = "choice", silent = TRUE)
  )
  time_2 <- system.time(
    res_2 <- estimate(callsDependent ~ inertia, model = "DyNAM", subModel = "choice", silent = TRUE)
  )
  # Test the correctness
  expect_equal(res_1$parameters, res_1_withPreprocessingInit$parameters) # They should be the same
  expect_equal(res_2$parameters, res_2_withPreprocessingInit$parameters) # They should be the same
  # Test whether it's faster
  # expect_true(as.numeric(time_1_withPreprocessingInit[3]) <= as.numeric(time_1[3]) )
  # Using PreprocessingInit should not make the estimation slower
  # expect_true(as.numeric(time_2_withPreprocessingInit[3]) <= as.numeric(time_2[3]) )
  # Using PreprocessingInit should not make the estimation slower
})
