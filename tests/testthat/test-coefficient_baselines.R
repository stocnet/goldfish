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

# Fit every (model, backend) cell up front, in parallel; the blocks below only
# assert (fast, serial). skip_on_cran() stays authoritative — see
# baselines_precompute_fits().
baselinesFits <- baselines_precompute_fits(baselinesGrid, baselines_get_data)

for (modelName in names(baselinesGrid)) {
  for (backend in baselines_backends) {
    test_that(
      sprintf("baseline coefficients: %s backend %s", modelName, backend),
      {
        skip_on_cran()
        fit <- baselines_fits_cell(baselinesFits, modelName, backend)
        # The frozen baseline file is keyed by the engine token the fits
        # carried when it was written; the backend translates to it.
        token <- BACKEND_ENGINE_TOKENS[[backend]]
        expected <- baselines[[modelName]][[token]]
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
