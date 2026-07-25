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

baselinesGlobalFits <- baselines_precompute_fits(
  baselinesGlobalGrid,
  baselines_get_global_data
)

for (modelName in names(baselinesGlobalGrid)) {
  for (backend in baselines_backends) {
    test_that(
      sprintf(
        "global baseline coefficients: %s backend %s",
        modelName,
        backend
      ),
      {
        skip_on_cran()
        fit <- baselines_fits_cell(baselinesGlobalFits, modelName, backend)
        # Keyed by the engine token frozen into the baseline file.
        token <- BACKEND_ENGINE_TOKENS[[backend]]
        expected <- baselinesGlobal[[modelName]][[token]]
        expect_true(fit$convergence$isConverged)
        expect_equal(
          coef(fit),
          expected$coef,
          tolerance = 1e-6,
          ignore_attr = TRUE
        )
        expect_equal(
          as.numeric(logLik(fit)),
          expected$logLik,
          tolerance = 1e-6
        )
      }
    )
  }
}
