devtools::load_all(".")
source(file.path("tests", "testthat", "helper-baselines.R"))

dataList <- list(
  social_evolution = baselines_social_evolution_data(),
  fisheries = baselines_fisheries_data()
)
grid <- baselines_model_grid()

baselines <- list()
for (modelName in names(grid)) {
  for (engine in baselines_engines) {
    fit <- baselines_fit(grid[[modelName]], engine, dataList)
    stopifnot(isTRUE(fit$convergence$isConverged))
    baselines[[modelName]][[engine]] <- list(
      coef = coef(fit),
      logLik = as.numeric(logLik(fit))
    )
    cat(
      sprintf(
        "%s [%s] logLik: %.8f\n",
        modelName, engine, as.numeric(logLik(fit))
      )
    )
  }
}

saveRDS(
  baselines,
  file.path("tests", "testthat", "_baselines", "coefficient_baselines_v1.rds"),
  version = 2
)
