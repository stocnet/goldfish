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
# all three backends -- the `carry_from_v1` policy below is what prevents it.
#
# v2 is keyed by BACKEND value ("r", "cpp", "gather"). v1's legacy engine-token
# keys ("default", "default_c") are translated on the way in: a new artifact
# should not record a vocabulary the package has retired.
#
# The build loop lives in tests/testthat/helper-baselines.R, which every
# baseline test sources, so a rename breaks a test instead of rotting here --
# which is what happened to the `baselines_engines` reference this script used
# to carry.
devtools::load_all(".")
source(file.path("tests", "testthat", "helper-baselines.R"))

v1_path <- file.path(
  "tests", "testthat", "_baselines", "coefficient_baselines_v1.rds"
)
v1 <- readRDS(v1_path)

data_env <- new.env()
get_data <- function(dataset) {
  if (is.null(data_env[[dataset]])) {
    data_env[[dataset]] <- switch(
      dataset,
      social_evolution = baselines_social_evolution_data(),
      fisheries = suppressWarnings(baselines_fisheries_data())
    )
  }
  mget(dataset, envir = data_env)
}

# r and cpp are carried forward, never recomputed; gather is the one new column.
carry_from_v1 <- function(model_name, backend) {
  if (!backend %in% c("r", "cpp")) {
    return(NULL)
  }
  carried <- v1[[model_name]][[BACKEND_ENGINE_TOKENS[[backend]]]]
  stopifnot(!is.null(carried))
  carried
}

baselines <- baselines_build(
  grid = baselines_model_grid(),
  get_data = get_data,
  backends = baselines_backends,
  carry = carry_from_v1
)

saveRDS(
  baselines,
  file.path("tests", "testthat", "_baselines", "coefficient_baselines_v2.rds"),
  version = 2
)
