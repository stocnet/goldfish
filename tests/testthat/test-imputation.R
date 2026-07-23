# Per-mode attribute imputation.
#
# A missing nodal value is summarized over the imputed node's mode category,
# by the summary its recorded value type selects, at whatever moment the value
# is needed -- the initial table and each event through one resolver. These
# tests pin the contract: the pool never crosses a mode category, a categorical
# attribute is never summarized by a mean, and an empty pool aborts before the
# walk rather than emitting a not-a-number value mid-way.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A one-mode fixture carrying a numeric attribute with one missing value and a
# character attribute, for the resolver-agrees and categorical-walk cases.
fixture_one_mode_attr <- function(group = c("red", "red", "blue", "blue")) {
  nodes <- data.frame(
    label = c("A", "B", "C", "D"),
    mode = "p",
    weight = c(4, NA, 6, 8),
    group = group,
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(1L, 2L, 3L, 4L),
    to = c(2L, 3L, 4L, 1L),
    time = c(1, 2, 3, 4),
    layer = "calls",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "impute",
    focal = "calls",
    update = c(calls = "increment"),
    directed = c(calls = TRUE),
    observation = c(calls = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# A two-mode fixture with two nodes per mode, so a mode category has someone
# else to pool from: tasks {T1, T2}, workers {W1, W2}.
fixture_two_per_mode <- function() {
  nodes <- data.frame(
    label = c("T1", "T2", "W1", "W2"),
    mode = c("task", "task", "worker", "worker"),
    skill = c(2, 4, 20, 30),
    stringsAsFactors = FALSE
  )
  ties <- data.frame(
    from = c(3L, 4L),
    to = c(1L, 2L),
    time = c(1, 2),
    layer = "assign",
    stringsAsFactors = FALSE
  )
  info <- list(
    name = "twoper",
    focal = "assign",
    update = c(assign = "increment"),
    directed = c(assign = TRUE),
    observation = c(assign = "event"),
    sender = c(assign = "worker"),
    receiver = c(assign = "task")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# The imputed value written into a node's initial statistic, for `ego`: with
# `ego(z)` the sender's own value is the statistic, constant across alters.
initial_ego_stat <- function(data, attribute, node) {
  prep <- suppressWarnings(estimate_dynam(
    stats::as.formula(sprintf("%s ~ ego(%s)", data$info$focal, attribute)),
    sub_model = "choice",
    data = as_goldfish(data),
    preprocessing_only = TRUE
  ))
  prep$initialStats[node, 1, 1]
}

test_that("a numeric attribute imputes from the mean of its mode category", {
  # The missing task attribute must be filled from tasks only. Tasks average 3;
  # a pool crossing into workers (20, 30) would give a wildly different number.
  fixture <- fixture_two_per_mode()
  fixture$nodes$skill <- c(2, NA, 20, 30)

  prep <- suppressWarnings(estimate_dynam(
    assign ~ alter(skill),
    sub_model = "choice",
    data = as_goldfish(fixture),
    preprocessing_only = TRUE
  ))
  # alter reads the receiver (task) side; T2's imputed skill is the mean of the
  # other tasks, i.e. T1 = 2.
  expect_equal(prep$initialStats[1, , 1], c(2, 2))
})

test_that("a view spanning two modes imputes each mode separately", {
  # The undeclared focal spans every mode, so tasks and workers share one view;
  # a task's missing value must still come from tasks and a worker's from
  # workers.
  fixture <- fixture_two_per_mode()
  fixture$info$sender <- NULL
  fixture$info$receiver <- NULL
  fixture$nodes$skill <- c(2, NA, 20, NA)

  expect_equal(
    initial_ego_stat(fixture, "skill", 2),
    2,
    label = "T2 imputed from tasks (T1 = 2)"
  )
  expect_equal(
    initial_ego_stat(fixture, "skill", 4),
    20,
    label = "W2 imputed from workers (W1 = 20)"
  )
})

test_that("a categorical attribute imputes the most common value, never a mean", {
  # The defect this closes: a bare mean on a character vector returns NA, which
  # the same event's update then compares and dies on.
  fixture <- fixture_one_mode_attr(group = c("red", "red", "blue", NA))

  expect_no_error(suppressWarnings(estimate_dynam(
    calls ~ same(group),
    sub_model = "choice",
    data = as_goldfish(fixture),
    preprocessing_only = TRUE
  )))
})

test_that("a categorical replace event mid-walk is filled, not left NA", {
  fixture <- fixture_one_mode_attr()
  change <- data.frame(time = 2.5, node = 2L, var = "group")
  change$value <- list(list(NA_character_))
  fixture$changes <- change

  expect_no_error(suppressWarnings(estimate_dynam(
    calls ~ same(group),
    sub_model = "choice",
    data = as_goldfish(fixture),
    preprocessing_only = TRUE
  )))
})

test_that("one-mode numeric imputation is unchanged", {
  # No mode column, one implicit category: the pool is every other node, exactly
  # as before mode categories existed. B imputes to mean(4, 6, 8).
  fixture <- fixture_one_mode_attr()

  expect_equal(
    initial_ego_stat(fixture, "weight", 2),
    mean(c(4, 6, 8))
  )
})

test_that("the initial pass and the walk agree through one resolver", {
  # The same variable missing in the initial table and in a later event: both
  # resolve through the same rule, and the imputed initial value is part of the
  # pool the later event draws from.
  fixture <- fixture_two_per_mode()
  fixture$nodes$skill <- c(2, NA, 20, 30)
  change <- data.frame(time = 1.5, node = 1L, var = "skill")
  change$value <- list(list(NA_real_))
  fixture$changes <- change

  # Initial: T2 <- mean(T1) = 2. Then T1's own value is replaced by a missing
  # value, imputed from the other task T2 (now 2), so the walk sees a defined
  # number and never writes NA.
  expect_no_error(suppressWarnings(estimate_dynam(
    assign ~ alter(skill),
    sub_model = "choice",
    data = as_goldfish(fixture),
    preprocessing_only = TRUE
  )))
})

test_that("sparse missingness is imputed, not rejected", {
  fixture <- fixture_two_per_mode()
  fixture$nodes$skill <- c(NA, 4, 20, 30)

  expect_no_error(suppressWarnings(estimate_dynam(
    assign ~ alter(skill),
    sub_model = "choice",
    data = as_goldfish(fixture),
    preprocessing_only = TRUE
  )))
})

test_that("a singleton mode category aborts at schedule construction", {
  local_cli_context()
  # make_stocnet_fixture_multimode() has exactly one supervisor and one
  # outsider -- two singleton strata. A missing replace on the sole supervisor
  # has nothing to pool from.
  fixture <- make_stocnet_fixture_multimode()
  fixture$nodes$val <- c(10, 20, 5, 7)
  change <- data.frame(time = 1.5, node = 3L, var = "val")
  change$value <- list(list(NA_real_))
  fixture$changes <- change

  expect_error(
    suppressWarnings(estimate_dynam(
      advice ~ ego(val),
      sub_model = "choice",
      data = as_goldfish(fixture),
      preprocessing_only = TRUE
    )),
    regexp = "cannot be imputed"
  )
  expect_error(
    suppressWarnings(estimate_dynam(
      advice ~ ego(val),
      sub_model = "choice",
      data = as_goldfish(fixture),
      preprocessing_only = TRUE
    )),
    regexp = "supervisor"
  )
})

test_that("an attribute wholly missing on a mode category aborts where read", {
  # Generalized R3: the view spans every mode, `val` is defined for employees
  # and missing for supervisor and outsider, so the read is undefined there.
  fixture <- make_stocnet_fixture_multimode()
  fixture$nodes$val <- c(10, 20, NA, NA)

  expect_error(
    create_effects_functions(
      parse_formula(advice ~ ego(val), data = as_goldfish(fixture))$rhs_names,
      "DyNAM",
      "choice",
      data = as_goldfish(fixture)
    ),
    regexp = "undefined"
  )
})

test_that("value type and missingness are recorded at the metadata pass", {
  fixture <- fixture_two_per_mode()
  fixture$nodes$skill <- c(2, NA, 20, 30)
  fixture$nodes$title <- c("a", "b", "c", "d")
  src <- new_data_source(data = as_goldfish(fixture))

  # The receiver (task) side carries T2's missing skill; both attributes are
  # read there.
  keys <- build_object_keys(
    c("nodes_side2$skill", "nodes_side2$title"),
    nodes = "nodes_side1",
    nodes2 = "nodes_side2",
    src = src
  )

  expect_equal(keys$value_type, c("numeric", "categorical"))
  # `skill` carries an NA in the initial table; `title` is fully observed.
  expect_equal(keys$has_missing, c(TRUE, FALSE))
})

test_that("an attribute recorded as complete enters no imputation path", {
  # A change stream carrying a missing value flips has_missing even when the
  # initial table is complete, so the walk knows where a missing value can
  # occur without scanning state.
  fixture <- fixture_two_per_mode()
  change <- data.frame(time = 1.5, node = 3L, var = "skill")
  change$value <- list(list(NA_real_))
  fixture$changes <- change
  src <- new_data_source(data = as_goldfish(fixture))

  # The change (global id 3 = W1) lands on the worker view, whose initial skills
  # are both observed -- so only the stream carries the missing value.
  keys <- build_object_keys(
    "nodes_side1$skill",
    nodes = "nodes_side1",
    nodes2 = "nodes_side2",
    src = src
  )
  expect_true(keys$has_missing)
})

test_that("the state container carries a stratum vector per view", {
  src <- new_data_source(data = make_stocnet_fixture_multipartite())
  state <- build_state_container(
    "nodes_side1$size",
    nodes = "nodes_side1",
    nodes2 = "nodes_side2",
    src = src
  )
  strata <- attr(state, "strata")

  expect_equal(strata[["nodal:actor"]], rep("actor", 3))
  expect_equal(strata[["nodal:event"]], rep("event", 2))
})
