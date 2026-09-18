# Characterization fixture for the simulation event collector: the events
# frame and latent path of two seeded runs, captured from the collector that
# built one data frame per event and bound them at the end.
#
# The collector that replaced it writes typed columns by index; its output
# must be identical. The runs cover an unflavored two-process specification
# whose parameter provider records a latent path, and a flavored layer whose
# events carry a flavor and a -1 update.
#
# Regenerate only when the drawn sequence changes on purpose (a change to the
# clock, the marks or the random stream), from the package root:
#
#   Rscript tests/testthat/fixtures/make-simulated-events.R

devtools::load_all(".", quiet = TRUE)
source("tests/testthat/helper-simulate.R")
source("tests/testthat/helper-flavored-fixtures.R")

saveRDS(
  simulated_events_runs(),
  "tests/testthat/fixtures/simulated_events.rds",
  version = 3
)
