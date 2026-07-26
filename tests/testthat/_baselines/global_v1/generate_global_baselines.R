devtools::load_all(".")
source(file.path("tests", "testthat", "helper-baselines.R"))

dataList <- list(social_evolution_global = baselines_global_data())
grid <- baselines_global_model_grid()

baselines <- list()
for (modelName in names(grid)) {
  for (backend in baselines_backends) {
    # The stored keys stay the legacy engine tokens this file was written with,
    # so a regeneration reproduces the existing key scheme rather than silently
    # rewriting it; the fit itself runs under the current backend vocabulary.
    engine <- BACKEND_ENGINE_TOKENS[[backend]]
    fit <- baselines_fit(grid[[modelName]], backend, dataList)
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
  file.path(
    "tests", "testthat", "_baselines", "global_v1",
    "coefficient_baselines_global.rds"
  ),
  version = 2
)
