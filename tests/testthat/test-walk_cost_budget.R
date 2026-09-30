# The frozen coefficient baselines hold the numbers still; nothing in them
# moves when the walk becomes slower or its output larger. This is the gate
# for that: the cost of one preprocessing walk against a recorded budget.

test_that("the preprocessing walk stays within its recorded cost budget", {
  skip_on_cran()
  skip_if_not_installed("bench")
  skip_if_not_installed("lobstr")

  budget <- walk_cost_budget()
  measured <- measure_walk_cost(walk_cost_specs())
  cells <- measured$cell

  for (i in seq_along(cells)) {
    for (metric in c("alloc_mb", "size_mb")) {
      expect_lte(
        measured[[metric]][[i]],
        walk_cost_ceiling(budget, metric, cells[[i]]),
        label = paste(metric, "of the", cells[[i]], "walk")
      )
      # Allocation and size repeat exactly run to run, so a value under the
      # floor is an improvement the budget has not been lowered to keep.
      expect_gte(
        measured[[metric]][[i]],
        walk_cost_floor(budget, metric, cells[[i]]),
        label = paste(metric, "of the", cells[[i]], "walk")
      )
    }
  }

  seconds <- stats::setNames(measured$seconds, cells)
  expect_lte(
    seconds[["constrained"]] / seconds[["unconstrained"]],
    walk_cost_ceiling(
      budget,
      "time_ratio",
      "constrained_over_unconstrained"
    ),
    label = "constrained over unconstrained walk time"
  )

  # Seconds are one machine's number, so they are compared only where the
  # budget was recorded.
  skip_if_not(
    identical(Sys.getenv("GOLDFISH_WALK_COST_SECONDS"), "true"),
    "absolute walk time is checked only with GOLDFISH_WALK_COST_SECONDS=true"
  )
  for (cell in cells) {
    expect_lte(
      seconds[[cell]],
      walk_cost_ceiling(budget, "seconds", cell),
      label = paste("median seconds of the", cell, "walk")
    )
  }
})

test_that("the walk cost fixture keeps the full node set", {
  data <- college_msg_data()
  expect_equal(nrow(data$nodes), college_msg_n_actors)
  expect_equal(nrow(data$ties), 5000L)
})
