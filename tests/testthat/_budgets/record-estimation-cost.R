# Prints the estimation cost measured on this machine, in the layout of
# estimation_cost.csv, for comparison with the recorded budget.
#
# From the package root, on an optimized build (the cpp-recompile skill):
#
#   Rscript tests/testthat/_budgets/record-estimation-cost.R
#
# It writes nothing. Raising a recorded value is a decision that estimation is
# allowed to cost more, and it needs the maintainer's approval: copy the
# printed rows into estimation_cost.csv by hand, in a commit whose message says
# what made estimation more expensive and why that is accepted. Lowering a
# value after an improvement locks the gain in and needs no justification.

suppressMessages(devtools::load_all(".", quiet = TRUE))
testthat::local_edition(3)
for (helper in c("baselines", "walk-cost", "estimation-cost")) {
  source(file.path("tests", "testthat", paste0("helper-", helper, ".R")))
}
options(lifecycle_verbosity = "quiet")

data_list <- estimation_cost_data(
  file.path("tests", "testthat", "fixtures", "college_msg_5000.txt")
)
fits <- measure_estimation_cost(data_list)
evaluations <- measure_evaluation_cost(data_list)
allocating <- !is.na(fits$alloc_mb)

rows <- rbind(
  data.frame(
    metric = "alloc_mb",
    cell = fits$cell[allocating],
    backend = fits$backend[allocating],
    recorded = signif(fits$alloc_mb[allocating], 4L)
  ),
  data.frame(
    metric = "iterations",
    cell = fits$cell,
    backend = fits$backend,
    recorded = fits$iterations
  ),
  data.frame(
    metric = "seconds",
    cell = c(fits$cell, paste0(evaluations$cell, "_evaluation")),
    backend = c(fits$backend, rep("cpp", nrow(evaluations))),
    recorded = signif(c(fits$seconds, evaluations$seconds), 4L)
  ),
  data.frame(
    metric = "diagnostics_ratio",
    cell = evaluations$cell,
    backend = "cpp",
    recorded = signif(evaluations$diagnostics_ratio, 4L)
  )
)
utils::write.csv(rows, stdout(), row.names = FALSE, quote = FALSE)
