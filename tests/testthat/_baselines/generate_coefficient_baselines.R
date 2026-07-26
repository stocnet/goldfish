# Unified coefficient-baseline generator.
#
# v2 extends v1 to the gather backend. It does NOT re-freeze r and cpp: their
# entries are copied from `coefficient_baselines_v1.rds` verbatim, so the
# historical 1e-6 floor set at commit b890cd0 survives bit-identically and any
# future drift against it stays detectable. Only the gather column, which v1
# never carried at all (`baselines_backends` was c("r", "cpp"), the fact design
# D4 of backend-parity relied on), is computed here.
#
# Regenerating the r/cpp columns instead would absorb the ~2.4e-07 relative
# drift that has accumulated since v1 was written and make any later regression
# smaller than that drift invisible. Do not "simplify" this script by fitting
# all three backends.
#
# v2 is keyed by BACKEND value ("r", "cpp", "gather"). v1's legacy engine-token
# keys ("default", "default_c") are translated on the way in: a new artifact
# should not record a vocabulary the package has retired.
devtools::load_all(".")
source(file.path("tests", "testthat", "helper-baselines.R"))

v1Path <- file.path(
  "tests", "testthat", "_baselines", "coefficient_baselines_v1.rds"
)
v1 <- readRDS(v1Path)

dataEnv <- new.env()
getData <- function(dataset) {
  if (is.null(dataEnv[[dataset]])) {
    dataEnv[[dataset]] <- switch(
      dataset,
      social_evolution = baselines_social_evolution_data(),
      fisheries = suppressWarnings(baselines_fisheries_data())
    )
  }
  mget(dataset, envir = dataEnv)
}

grid <- baselines_model_grid()

baselines <- list()
for (modelName in names(grid)) {
  spec <- grid[[modelName]]

  # Carried forward, never recomputed.
  for (backend in c("r", "cpp")) {
    token <- BACKEND_ENGINE_TOKENS[[backend]]
    carried <- v1[[modelName]][[token]]
    stopifnot(!is.null(carried))
    baselines[[modelName]][[backend]] <- carried
  }

  # The one new column.
  fit <- suppressWarnings(baselines_fit(spec, "gather", getData(spec$dataset)))
  stopifnot(isTRUE(fit$convergence$isConverged))
  baselines[[modelName]][["gather"]] <- list(
    coef = coef(fit),
    logLik = as.numeric(logLik(fit))
  )
  cat(sprintf(
    "%-28s gather logLik: %.8f  (r/cpp carried from v1)\n",
    modelName,
    as.numeric(logLik(fit))
  ))
}

saveRDS(
  baselines,
  file.path(
    "tests", "testthat", "_baselines", "coefficient_baselines_v2.rds"
  ),
  version = 2
)
