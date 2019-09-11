context("The fixing parameters feature")

test_that("fixing parameters in C-implementation", {
  prep  <- estimate(formula, model = model, subModel = subModel, preprocessingOnly = TRUE, silent = TRUE)
  res_1 <- estimate(formula, model = model, subModel = subModel,
                    estimationInit = list(engine = "default_c"), preprocessingInit = prep, silent = TRUE)
  fixedParameters <- res_1$parameters
  res_2 <- estimate(formula, model = model, subModel = subModel,
                    estimationInit = list(engine = "default_c", fixedParameters = fixedParameters),
                    preprocessingInit = prep, silent = TRUE)
  fixedParameters[3] <- NA
  res_3 <- estimate(formula, model = model, subModel = subModel,
                    estimationInit = list(engine = "default_c", fixedParameters = fixedParameters),
                    preprocessingInit = prep, silent = TRUE)
  expect_equal(res_1$parameters, res_3$parameters,  tolerance = 1e-3)
  expect_equal(res_1$log.likelihood, res_2$log.likelihood,  tolerance = 1e-3)
  expect_equal(res_2$log.likelihood, res_3$log.likelihood,  tolerance = 1e-3)
})

test_that("fixing parameters in R-implementation", {
  prep  <- estimate(formula, model = model, subModel = subModel, preprocessingOnly = TRUE, silent = TRUE)
  res_1 <- estimate(formula, model = model, subModel = subModel, preprocessingInit = prep, silent = TRUE)
  fixedParameters <- res_1$parameters
  res_2 <- estimate(formula, model = model, subModel = subModel,
                    estimationInit = list(fixedParameters = fixedParameters), preprocessingInit = prep, silent = TRUE)
  fixedParameters[3] <- NA
  res_3 <- estimate(formula, model = model, subModel = subModel,
                    estimationInit = list(fixedParameters = fixedParameters), preprocessingInit = prep, silent = TRUE)
  expect_equal(res_1$parameters, res_3$parameters, tolerance = 1e-3)
  expect_equal(res_1$log.likelihood, res_2$log.likelihood,  tolerance = 1e-3)
  expect_equal(res_2$log.likelihood, res_3$log.likelihood,  tolerance = 1e-3)
})
