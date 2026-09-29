# Prints the walk cost measured on this machine, in the layout of
# walk_cost.csv, for comparison with the recorded budget.
#
# From the package root:
#
#   Rscript tests/testthat/_budgets/record-walk-cost.R
#
# It writes nothing. Raising a recorded value is a decision that the walk is
# allowed to cost more, and it needs the maintainer's approval: copy the
# printed rows into walk_cost.csv by hand, in a commit whose message says
# what made the walk more expensive and why that is accepted. Lowering a
# value after an improvement locks the gain in and needs no justification.

suppressMessages(devtools::load_all(".", quiet = TRUE))
testthat::local_edition(3)
source(file.path("tests", "testthat", "helper-walk-cost.R"))

data <- college_msg_data(
  file.path("tests", "testthat", "fixtures", "college_msg_5000.txt")
)
measured <- measure_walk_cost(walk_cost_specs(data))

by_cell <- function(column) stats::setNames(measured[[column]], measured$cell)
seconds <- by_cell("seconds")

rows <- data.frame(
  metric = c(
    rep(c("alloc_mb", "size_mb", "seconds"), each = nrow(measured)),
    "time_ratio"
  ),
  cell = c(rep(measured$cell, times = 3L), "constrained_over_unconstrained"),
  recorded = signif(
    c(
      measured$alloc_mb,
      measured$size_mb,
      measured$seconds,
      seconds[["constrained"]] / seconds[["unconstrained"]]
    ),
    4L
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(rows, stdout(), row.names = FALSE, quote = FALSE)
