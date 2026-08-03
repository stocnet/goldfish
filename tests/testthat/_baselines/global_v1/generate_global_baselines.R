# Generator for the separately versioned `global_v1` baseline set.
#
# This set holds DyNAM-rate and REM models carrying a `global()` effect. Unlike
# the frozen v1/v2 set it MAY be regenerated with documented justification
# (design D18 of refactor-preprocess-estimate), because the global-attribute
# feature shipped immediately before that refactor.
#
# Two policies distinguish it from the main generator, and both are passed as
# arguments rather than reimplemented:
#   - it covers `baselines_backends_global` (r and cpp), NOT the main set's
#     `baselines_backends`, which has since grown a `gather` column. Extending
#     this set to gather would be its own deliberate regeneration.
#   - the stored keys stay the legacy engine tokens this file was written with,
#     so a regeneration reproduces the existing key scheme rather than silently
#     rewriting it; the fits themselves run under the current backend vocabulary.
#
# The build loop lives in tests/testthat/helper-baselines.R, which every
# baseline test sources, so a rename breaks a test instead of rotting here.
devtools::load_all(".")
source(file.path("tests", "testthat", "helper-baselines.R"))

data_list <- list(social_evolution_global = baselines_global_data())

baselines <- baselines_build(
  grid = baselines_global_model_grid(),
  get_data = function(dataset) data_list,
  backends = baselines_backends_global,
  key = function(backend) BACKEND_ENGINE_TOKENS[[backend]]
)

saveRDS(
  baselines,
  file.path(
    "tests", "testthat", "_baselines", "global_v1",
    "coefficient_baselines_global.rds"
  ),
  version = 2
)
