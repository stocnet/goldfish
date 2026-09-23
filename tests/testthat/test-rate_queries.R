# Rate queries over a candidate sequence: replaying the candidate's own
# schedule through the walk handle by row position, so a query reads the state
# the batch path would have at the same point, stamp ties included.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# calls is modeled; friendship and emails are exogenous layers the formulas
# read. Stamps are tied on purpose. The schedule at t = 2 runs: the dependent
# rows of calls events 2 and 3 (rows 4, 5), friendship 3 -> 4 (row 6), the
# realizations of calls events 2 and 3 (rows 7, 8), emails 2 -> 1 (row 9).
rq_tied_data <- function() {
  ties <- rbind(
    data.frame(
      from = c(1L, 3L, 4L),
      to = c(2L, 4L, 5L),
      time = c(1, 2, 3),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 2, 3),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 5L),
      to = c(1L, 2L),
      time = c(2, 3),
      layer = "emails"
    )
  )
  list(
    info = list(
      name = "toy",
      focal = "calls",
      update = c(
        friendship = "increment",
        calls = "increment",
        emails = "increment"
      ),
      directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
      observation = c(friendship = "panel", calls = "event", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
}

rq_tied_spec <- function() {
  make_specification(
    rate = ~ 1 + indeg(friendship),
    choice = ~ inertia + tie(friendship) + tie(emails),
    layer = "calls",
    model = "DyNAM",
    data = rq_tied_data()
  )
}

rq_open <- function() {
  suppressWarnings(walk_open(rq_tied_spec()))
}

test_that("the tied fixture's schedule has the layout the tests rely on", {
  schedule <- rq_open()$schedule
  expect_identical(
    schedule$layer[4:9],
    c("calls", "calls", "friendship", "calls", "calls", "emails")
  )
  expect_identical(
    schedule$dependent[4:9],
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(unique(schedule$time[4:9]), 2)
})

test_that("walk_replay_to stops at the requested schedule row", {
  handle <- rq_open()
  walk_replay_to(handle, 8L)
  networks <- handle$state$networks

  # Events 1 and 2 are realized; event 3's realization is row 8 itself.
  expect_identical(networks[["calls"]][1, 2], 1)
  expect_identical(networks[["calls"]][2, 3], 1)
  expect_identical(networks[["calls"]][3, 4], 0)
  expect_identical(networks[["friendship"]][3, 4], 1)
  # Row 9 shares the stamp of row 7 but follows the requested row.
  expect_identical(networks[["emails"]][2, 1], 0)
})

test_that("a dependent row is read before the exogenous rows at its stamp", {
  handle <- rq_open()
  walk_replay_to(handle, 4L)

  expect_identical(handle$state$networks[["friendship"]][1, 2], 1)
  expect_identical(handle$state$networks[["friendship"]][3, 4], 0)
  # indeg(friendship) of N4 is still 0, so its rate is the bare intercept.
  rate <- walk_evaluate(handle, 1L, c(0, 1))$value
  expect_equal(rate[4], 1)
})

test_that("walk_replay_to resumes forward and matches a fresh replay", {
  applied <- 0L
  real_inject <- walk_inject
  local_mocked_bindings(
    walk_inject = function(...) {
      applied <<- applied + 1L
      real_inject(...)
    }
  )

  resumed <- rq_open()
  walk_replay_to(resumed, 4L)
  applied <- 0L
  walk_replay_to(resumed, 12L)
  # Only event 2 and 3's realizations lie between rows 4 and 12.
  expect_identical(applied, 2L)

  fresh <- rq_open()
  walk_replay_to(fresh, 12L)

  expect_identical(resumed$state$networks, fresh$state$networks)
  expect_identical(
    lapply(resumed$engines, `[[`, "live_stats"),
    lapply(fresh$engines, `[[`, "live_stats")
  )
  expect_equal(
    walk_evaluate(resumed, 2L, c(0.3, 0.5, -0.2), sender = 4L)$value,
    walk_evaluate(fresh, 2L, c(0.3, 0.5, -0.2), sender = 4L)$value
  )
})

test_that("walk_replay_to refuses to move backward", {
  local_cli_context()
  handle <- rq_open()
  walk_replay_to(handle, 8L)
  expect_error(walk_replay_to(handle, 4L), class = "goldfish_walk_out_of_order")
  expect_snapshot(walk_replay_to(handle, 4L), error = TRUE)
})

# A joint specification over the tied fixture, with `data` swapped in when a
# candidate is wanted.
rq_joint_spec <- function(data = rq_tied_data()) {
  make_joint_specification(
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia + tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + inertia,
      layer = "emails",
      model = "REM",
      data = data
    ),
    data = data
  )
}

# The same data with the calls ties moved in time: what an augmented
# candidate changes.
rq_candidate_data <- function() {
  data <- rq_tied_data()
  is_calls <- data$ties$layer == "calls"
  data$ties$time[is_calls] <- data$ties$time[is_calls] + 0.5
  data
}

test_that("a candidate differing only in its ties is bound everywhere", {
  spec <- rq_joint_spec()
  candidate <- rq_candidate_data()
  bound <- bind_candidate_data(spec, candidate)

  expect_identical(bound$data, candidate)
  for (part in bound$specifications) {
    expect_identical(part$data, candidate)
  }
  expect_identical(bound$process_map, spec$process_map)
  expect_identical(bound$completed_rates, spec$completed_rates)
  expect_false(identical(bound$data$ties, spec$data$ties))
})

test_that("a candidate whose info differs is refused naming info", {
  local_cli_context()
  candidate <- rq_candidate_data()
  candidate$info$directed[["calls"]] <- FALSE

  expect_error(
    bind_candidate_data(rq_joint_spec(), candidate),
    class = "goldfish_candidate_mismatch"
  )
  expect_snapshot(bind_candidate_data(rq_joint_spec(), candidate), error = TRUE)
})

test_that("a candidate whose nodes differ is refused naming nodes", {
  local_cli_context()
  candidate <- rq_candidate_data()
  candidate$nodes$label[1] <- "Z1"

  expect_error(
    bind_candidate_data(rq_joint_spec(), candidate),
    class = "goldfish_candidate_mismatch"
  )
  expect_snapshot(bind_candidate_data(rq_joint_spec(), candidate), error = TRUE)
})
