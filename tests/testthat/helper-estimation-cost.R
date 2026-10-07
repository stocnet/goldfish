# Estimation cost budget: the cells, the measurement and the recorded values.
# Shared by test-likelihood_cost_budget.R and by
# tests/testthat/_budgets/record-estimation-cost.R, so the test compares
# against numbers taken by the same protocol it runs. The walk budget
# (helper-walk-cost.R) is the twin for preprocessing.

# The fit cells: three Social Evolution fits (84 actors, 439 events), small
# enough for every backend. A cell lists the backends it is measured on.
estimation_cost_fit_cells <- function() {
  grid <- baselines_model_grid()
  list(
    se_rem = list(entry = grid$se_rem, backends = c("cpp", "r", "gather")),
    se_choice = list(
      entry = grid$se_dynam_choice,
      backends = c("cpp", "r", "gather")
    ),
    se_rate = list(
      entry = grid$se_dynam_rate,
      backends = c("cpp", "r", "gather")
    )
  )
}

# The evaluation cells: one compiled REM evaluation, timed with and without
# the default diagnostics. Social Evolution REM at its optimum, and the first
# 200 CollegeMsg messages on the full 1899-actor node set, where the dyad grid
# (3.6 M dyads) dominates the per-event cost. The large-grid cell is an
# evaluation rather than a fit because a fit there takes minutes on the
# compiled backend (about 25 ms per event per evaluation) and cannot run at
# all on the other two: the gather stack would hold a row per dyad per event,
# and the r backend copies the statistics array per event.
estimation_cost_evaluation_cells <- function() {
  grid <- baselines_model_grid()
  list(
    se_rem = list(entry = grid$se_rem, n_timed = 15L),
    college_rem_200 = list(
      entry = list(
        dataset = "college_msg_200",
        model = "REM",
        formula = msg ~ 1 + inertia + recip
      ),
      # A fixed vector, not an optimum: the cost under test does not depend
      # on where the evaluation is, and fitting first would take minutes.
      at = c(-8, 0.5, 0.5),
      n_timed = 3L
    )
  )
}

estimation_cost_data <- function(
  college_path = testthat::test_path("fixtures", "college_msg_5000.txt")
) {
  suppressWarnings(list(
    social_evolution = baselines_social_evolution_data(),
    college_msg_200 = estimation_cost_college_data(200L, college_path)
  ))
}

estimation_cost_college_data <- function(n_messages, path) {
  data <- college_msg_data(path)
  data$ties <- data$ties[seq_len(n_messages), ]
  data
}

# One warm fit with the default diagnostics, as a user's fit runs.
estimation_cost_fit <- function(entry, backend, data_list) {
  suppressWarnings(suppressMessages(
    baselines_fit(entry, backend, data_list)
  ))
}

# Measures every (fit cell, backend): median warm seconds over `n_timed` fits,
# Newton iterations, and, where the allocation is on R's heap (r and gather),
# megabytes allocated by one fit. The compiled backend's work buffers are
# allocated by Armadillo outside R's heap, so its allocation would read as the
# R-side bookkeeping only and is not recorded.
#
# Returns a data frame with one row per (cell, backend): cell, backend,
# seconds, iterations, alloc_mb (NA on cpp).
measure_estimation_cost <- function(
  data_list = estimation_cost_data(),
  cells = estimation_cost_fit_cells(),
  n_timed = 3L
) {
  rows <- list()
  for (cell in names(cells)) {
    for (backend in cells[[cell]]$backends) {
      entry <- cells[[cell]]$entry
      # The first fit warms the code path, so the measured one does not count
      # the allocations of compiling it, which depend on what ran before.
      fit <- estimation_cost_fit(entry, backend, data_list)
      alloc_mb <- if (identical(backend, "cpp")) {
        NA_real_
      } else {
        as.numeric(
          bench::bench_memory(
            estimation_cost_fit(entry, backend, data_list)
          )$mem_alloc
        ) /
          2^20
      }
      seconds <- vapply(
        seq_len(n_timed),
        function(run) {
          invisible(gc())
          system.time(estimation_cost_fit(entry, backend, data_list))[[
            "elapsed"
          ]]
        },
        numeric(1)
      )
      rows[[length(rows) + 1L]] <- data.frame(
        cell = cell,
        backend = backend,
        seconds = stats::median(seconds),
        iterations = fit$n_iterations,
        alloc_mb = alloc_mb,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# Measures every evaluation cell on the compiled backend: the median seconds
# of one evaluation without diagnostics, and the diagnostics ratio, the median
# of evaluations with the default diagnostics over the median without. A fit
# pays its diagnostics once, so a ratio between two fits would shrink toward
# one as the iteration count grows and stop seeing a per-evaluation tax; the
# ratio between two evaluations is where the cost is paid. The two kinds are
# timed alternately in one session, so machine load lands on both sides.
#
# Returns a data frame with one row per cell: cell, seconds,
# diagnostics_ratio.
measure_evaluation_cost <- function(
  data_list = estimation_cost_data(),
  cells = estimation_cost_evaluation_cells()
) {
  rows <- lapply(names(cells), function(cell) {
    entry <- cells[[cell]]$entry
    at <- cells[[cell]]$at
    if (is.null(at)) {
      at <- unname(estimation_cost_fit(entry, "cpp", data_list)$parameters)
    }
    prep <- suppressWarnings(suppressMessages(compute_statistics(
      entry$formula,
      model = entry$model,
      sub_model = "rate",
      data = data_list[[entry$dataset]],
      output = "preprocessed"
    )))
    engine <- make_engine_evaluator(
      spec = prep$model_spec,
      stats_list = prep,
      parameters = at,
      backend = "cpp",
      has_intercept = isTRUE(prep$has_intercept),
      is_rate_model = FALSE,
      seed_intercept = FALSE
    )
    # `diagnostics = c("loglik", "scores")`: the event scores and the total
    # rate with its conditional component.
    with_diagnostics <- function() {
      engine$evaluate(at, need_scores = TRUE, need_total_rate = TRUE)
    }
    without_diagnostics <- function() {
      engine$evaluate(at, need_scores = FALSE)
    }
    n_timed <- cells[[cell]]$n_timed
    seconds <- matrix(NA_real_, n_timed, 2L)
    for (run in seq_len(n_timed)) {
      seconds[run, 1L] <- system.time(with_diagnostics())[["elapsed"]]
      seconds[run, 2L] <- system.time(without_diagnostics())[["elapsed"]]
    }
    data.frame(
      cell = cell,
      seconds = stats::median(seconds[, 2L]),
      diagnostics_ratio = stats::median(seconds[, 1L]) /
        stats::median(seconds[, 2L]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# The recorded budget: one row per (metric, cell, backend) with columns
# metric, cell, backend, recorded, limit. `limit` is a factor on `recorded`
# except for `diagnostics_ratio`, where it is the ceiling itself, and for
# `iterations`, which must match exactly.
estimation_cost_budget <- function(
  path = testthat::test_path("_budgets", "estimation_cost.csv")
) {
  utils::read.csv(path, comment.char = "#", stringsAsFactors = FALSE)
}

estimation_cost_row <- function(budget, metric, cell, backend) {
  row <- budget[
    budget$metric == metric &
      budget$cell == cell &
      budget$backend == backend,
  ]
  if (nrow(row) != 1L) {
    cli::cli_abort(
      "The estimation cost budget needs exactly one row for {.val {metric}} /
       {.val {cell}} / {.val {backend}}, found {nrow(row)}."
    )
  }
  row
}

estimation_cost_ceiling <- function(budget, metric, cell, backend) {
  row <- estimation_cost_row(budget, metric, cell, backend)
  if (metric == "diagnostics_ratio") row$limit else row$recorded * row$limit
}

# The mirror of the ceiling, as in the walk budget: allocation repeats exactly
# run to run, so a value under the floor is an improvement the budget has not
# been lowered to keep.
estimation_cost_floor <- function(budget, metric, cell, backend) {
  row <- estimation_cost_row(budget, metric, cell, backend)
  row$recorded * (2 - row$limit)
}
