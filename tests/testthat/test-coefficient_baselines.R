baselines <- readRDS(test_path("_baselines", "coefficient_baselines_v1.rds"))
baselinesGrid <- baselines_model_grid()
baselinesDataEnv <- new.env()

baselines_get_data <- function(dataset) {
  if (is.null(baselinesDataEnv[[dataset]])) {
    baselinesDataEnv[[dataset]] <- switch(
      dataset,
      social_evolution = baselines_social_evolution_data(),
      fisheries = suppressWarnings(baselines_fisheries_data())
    )
  }
  mget(dataset, envir = baselinesDataEnv)
}

for (modelName in names(baselinesGrid)) {
  for (engine in baselines_engines) {
    test_that(
      sprintf("baseline coefficients: %s engine %s", modelName, engine),
      {
        skip_on_cran()
        spec <- baselinesGrid[[modelName]]
        dataList <- baselines_get_data(spec$dataset)
        fit <- suppressWarnings(baselines_fit(spec, engine, dataList))
        expected <- baselines[[modelName]][[engine]]
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
