baselinesGlobal <- readRDS(
  test_path("_baselines", "global_v1", "coefficient_baselines_global.rds")
)
baselinesGlobalGrid <- baselines_global_model_grid()
baselinesGlobalDataEnv <- new.env()

baselines_get_global_data <- function(dataset) {
  if (is.null(baselinesGlobalDataEnv[[dataset]])) {
    baselinesGlobalDataEnv[[dataset]] <- baselines_global_data()
  }
  mget(dataset, envir = baselinesGlobalDataEnv)
}

for (modelName in names(baselinesGlobalGrid)) {
  for (engine in baselines_engines) {
    test_that(
      sprintf("global baseline coefficients: %s engine %s", modelName, engine),
      {
        skip_on_cran()
        spec <- baselinesGlobalGrid[[modelName]]
        dataList <- baselines_get_global_data(spec$dataset)
        fit <- suppressWarnings(baselines_fit(spec, engine, dataList))
        expected <- baselinesGlobal[[modelName]][[engine]]
        expect_true(fit$convergence$isConverged)
        expect_equal(
          coef(fit), expected$coef,
          tolerance = 1e-6, ignore_attr = TRUE
        )
        expect_equal(
          as.numeric(logLik(fit)), expected$logLik,
          tolerance = 1e-6
        )
      }
    )
  }
}
