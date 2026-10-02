# The frozen coefficient baselines and the per-event pins hold the numbers
# still; nothing in them moves when a fit becomes slower or allocates more.
# This is the gate for that: the cost of estimation against a recorded budget,
# the twin of the preprocessing walk's (test-walk_cost_budget.R).

test_that("estimation stays within its recorded cost budget", {
  skip_on_cran()
  skip_if_not_installed("bench")
  withr::local_options(lifecycle_verbosity = "quiet")

  budget <- estimation_cost_budget()
  check_seconds <- identical(Sys.getenv("GOLDFISH_WALK_COST_SECONDS"), "true")
  data_list <- estimation_cost_data()
  fits <- measure_estimation_cost(
    data_list,
    n_timed = if (check_seconds) 3L else 0L
  )
  evaluations <- measure_evaluation_cost(data_list)

  for (i in seq_len(nrow(fits))) {
    cell <- fits$cell[[i]]
    backend <- fits$backend[[i]]
    label <- paste("the", cell, "fit on", backend)
    expect_equal(
      fits$iterations[[i]],
      estimation_cost_row(budget, "iterations", cell, backend)$recorded,
      label = paste("iterations of", label)
    )
    if (!is.na(fits$alloc_mb[[i]])) {
      expect_lte(
        fits$alloc_mb[[i]],
        estimation_cost_ceiling(budget, "alloc_mb", cell, backend),
        label = paste("megabytes allocated by", label)
      )
      # Allocation repeats exactly run to run, so a value under the floor is
      # an improvement the budget has not been lowered to keep.
      expect_gte(
        fits$alloc_mb[[i]],
        estimation_cost_floor(budget, "alloc_mb", cell, backend),
        label = paste("megabytes allocated by", label)
      )
    }
  }

  for (i in seq_len(nrow(evaluations))) {
    cell <- evaluations$cell[[i]]
    expect_lte(
      evaluations$diagnostics_ratio[[i]],
      estimation_cost_ceiling(budget, "diagnostics_ratio", cell, "cpp"),
      label = paste(
        "evaluation with over without diagnostics on the",
        cell,
        "cell"
      )
    )
  }

  # Seconds are one machine's number, so they are compared only where the
  # budget was recorded.
  skip_if_not(
    check_seconds,
    paste(
      "absolute estimation time is checked only with",
      "GOLDFISH_WALK_COST_SECONDS=true"
    )
  )
  seconds <- c(
    stats::setNames(fits$seconds, paste(fits$cell, fits$backend)),
    stats::setNames(
      evaluations$seconds,
      paste0(evaluations$cell, "_evaluation cpp")
    )
  )
  for (key in names(seconds)) {
    parts <- strsplit(key, " ", fixed = TRUE)[[1]]
    expect_lte(
      seconds[[key]],
      estimation_cost_ceiling(budget, "seconds", parts[[1]], parts[[2]]),
      label = paste("median seconds of", key)
    )
  }
})
