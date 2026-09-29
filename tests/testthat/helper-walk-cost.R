# Walk cost budget: the fixture, the measurement and the recorded values.
# Shared by test-walk_cost_budget.R and by
# tests/testthat/_budgets/record-walk-cost.R, so the test compares against
# numbers taken by the same protocol it runs.

# CollegeMsg has 1899 actors numbered 1 to 1899. The extract's first events
# reach only some of them; the node set is the full one regardless, because
# the cost under test scales with the node set and not with who has spoken.
college_msg_n_actors <- 1899L

college_msg_data <- function(
  path = testthat::test_path("fixtures", "college_msg_5000.txt")
) {
  messages <- utils::read.table(
    path,
    header = FALSE,
    sep = " ",
    col.names = c("sender", "receiver", "time"),
    colClasses = c("character", "character", "numeric")
  )
  messages$increment <- 1
  actors <- data.frame(
    label = as.character(seq_len(college_msg_n_actors)),
    present = TRUE,
    stringsAsFactors = FALSE
  )
  data <- suppressMessages(
    manynet::as_stocnet(messages) |>
      manynet::join_nodes(actors) |>
      manynet::rename_nodes()
  )
  # `from_ties()` returns a lone network unchanged, so a single-layer stocnet
  # never gains the `layer` column the validator requires.
  data$ties$layer <- "msg"
  manynet::add_info(
    data,
    focal = "msg",
    update = c(msg = "increment"),
    directed = c(msg = TRUE),
    observation = c(msg = "event")
  )
}

# The two models the budget is recorded for: the same rate and choice
# formulas, without and with a dyadic support constraint. The pair is what
# makes the time gate machine-independent: their ratio is taken within one
# session.
walk_cost_specs <- function(data = college_msg_data()) {
  list(
    unconstrained = make_specification(
      rate = ~ 1 + indeg + outdeg,
      choice = ~ inertia + recip + trans,
      layer = "msg",
      data = data
    ),
    constrained = make_specification(
      rate = ~ 1 + indeg + outdeg,
      choice = ~ inertia + recip + trans,
      support_constraint = ~ !tie(msg),
      layer = "msg",
      data = data
    )
  )
}

# Measures every spec: median warm seconds, megabytes allocated by one walk,
# and megabytes held by the preprocessed object.
#
# Timed runs are interleaved across the specs rather than taken spec by spec,
# so a drift in machine load during the measurement lands on both sides of
# the ratio instead of on one.
#
# Returns a data frame with one row per spec: cell, seconds, alloc_mb, size_mb.
measure_walk_cost <- function(specs, n_timed = 7L) {
  # `~ !tie(msg)` excludes the repeat messages CollegeMsg contains, which the
  # constraint validation rightly refuses. The cost of maintaining and reading
  # a dyadic mask that flips is what is measured, and that does not depend on
  # the validation's verdict.
  testthat::local_mocked_bindings(
    validate_support_constraint = function(...) invisible(NULL),
    .package = "goldfish"
  )
  walk <- function(spec) {
    suppressWarnings(suppressMessages(preprocess_joint(spec)))
  }

  size_mb <- vapply(
    specs,
    function(spec) as.numeric(utils::object.size(walk(spec))) / 2^20,
    numeric(1)
  )
  seconds <- matrix(
    NA_real_,
    nrow = n_timed,
    ncol = length(specs),
    dimnames = list(NULL, names(specs))
  )
  for (run in seq_len(n_timed)) {
    for (cell in names(specs)) {
      # One walk leaves gigabytes of garbage behind; collecting it here keeps
      # the previous run's collection out of this run's time.
      invisible(gc())
      seconds[run, cell] <- system.time(walk(specs[[cell]]))[["elapsed"]]
    }
  }
  alloc_mb <- vapply(
    specs,
    function(spec) {
      as.numeric(bench::bench_memory(walk(spec))$mem_alloc) / 2^20
    },
    numeric(1)
  )

  data.frame(
    cell = names(specs),
    seconds = apply(seconds, 2L, stats::median),
    alloc_mb = alloc_mb,
    size_mb = size_mb,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

# The recorded budget: one row per (metric, cell) with columns metric, cell,
# recorded, limit. `limit` is a factor on `recorded` for every metric except
# `time_ratio`, where it is the ceiling itself.
walk_cost_budget <- function(
  path = testthat::test_path("_budgets", "walk_cost.csv")
) {
  utils::read.csv(path, comment.char = "#", stringsAsFactors = FALSE)
}

walk_cost_ceiling <- function(budget, metric, cell) {
  row <- budget[budget$metric == metric & budget$cell == cell, ]
  if (nrow(row) != 1L) {
    cli::cli_abort(
      "The walk cost budget needs exactly one row for {.val {metric}} /
       {.val {cell}}, found {nrow(row)}."
    )
  }
  if (metric == "time_ratio") row$limit else row$recorded * row$limit
}
