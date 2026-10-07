# The time-anchored variant: the observed event times are held and only the
# marks are redrawn. What is checked here is that a run walks the modeled
# layers' observed stamps, keeps each stamp's layer and flavor, redraws what
# the model draws (sender and receiver, the receiver alone for a choice-only
# DyNAM, the dyad for REM), and skips and counts a stamp the simulated state
# cannot support.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# The observed stamps of the given layers, as `layer@time`, in time order.
observed_stamps <- function(data, layers) {
  ties <- as.data.frame(data$ties)
  ties <- ties[ties$layer %in% layers & !is.na(ties$time), ]
  ties <- ties[order(ties$time), ]
  paste(ties$layer, ties$time, sep = "@")
}

test_that("an anchored run keeps each observed stamp and its layer", {
  js <- sim_two_process()

  out <- suppressMessages(simulate(
    js,
    seed = 1,
    coef = sim_two_process_parameters(js),
    times = "observed"
  ))

  stamps <- paste(out$events$layer, out$events$time, sep = "@")
  expect_setequal(
    stamps,
    observed_stamps(sim_fixture_data(), c("calls", "emails"))
  )
  expect_length(stamps, 8L)
  expect_false(is.unsorted(out$events$time))
})

test_that("an anchored run redraws the sender and the receiver", {
  js <- sim_two_process()
  observed <- as.data.frame(sim_fixture_data()$ties)
  observed <- observed[observed$layer == "calls", ]

  out <- suppressMessages(simulate(
    js,
    seed = 2,
    coef = sim_two_process_parameters(js),
    times = "observed"
  ))

  calls <- out$events[out$events$layer == "calls", ]
  expect_false(identical(calls$sender, as.integer(observed$from)))
  expect_false(identical(calls$receiver, as.integer(observed$to)))
})

test_that("an anchored run reports the observed stop and no clock", {
  js <- sim_two_process()

  out <- suppressMessages(simulate(
    js,
    seed = 1,
    coef = sim_two_process_parameters(js),
    times = "observed"
  ))

  expect_identical(out$diagnostics$stop_reason, "observed")
  expect_false(out$capped)
  expect_identical(out$diagnostics$end_time, 5)
  expect_identical(nrow(out$diagnostics$trajectory), 0L)
  expect_identical(out$diagnostics$n_stamps_skipped, 0L)
  expect_identical(out$times, "observed")
  expect_identical(out$times_source, "requested")
})

test_that("a choice-only DyNAM holds the observed sender by default", {
  data <- sim_fixture_data()
  spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  observed <- as.data.frame(data$ties)
  observed <- observed[observed$layer == "calls", ]

  expect_no_warning(out <- simulate(spec, seed = 1, coef = c(0.2, 0.3)))

  expect_identical(out$times, "observed")
  expect_identical(out$times_source, "specification")
  expect_identical(out$process_map$regime, "modeled")
  expect_identical(out$events$time, as.numeric(observed$time))
  expect_identical(out$events$sender, as.integer(observed$from))
  expect_false(identical(out$events$receiver, as.integer(observed$to)))
})

test_that("an anchored REM run redraws the dyad at each stamp", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "REM",
    data = data
  )
  observed <- as.data.frame(data$ties)
  observed <- observed[observed$layer == "calls", ]

  out <- suppressMessages(simulate(
    spec,
    seed = 1,
    coef = c(-1, 0.1),
    times = "observed"
  ))

  expect_identical(out$events$time, as.numeric(observed$time))
  expect_false(identical(
    paste(out$events$sender, out$events$receiver),
    paste(observed$from, observed$to)
  ))
})

test_that("a fitted model anchors on the data it is given", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "REM",
    data = data
  )
  fit <- suppressMessages(estimate_rem(spec, sub_model = "rate"))
  observed <- as.data.frame(data$ties)
  observed <- observed[observed$layer == "calls", ]

  expect_message(
    out <- simulate(fit, seed = 3, data = data, times = "observed"),
    class = "goldfish_sim_times_override"
  )

  expect_identical(out$events$time, as.numeric(observed$time))
  expect_identical(out$diagnostics$stop_reason, "observed")
})

test_that("an unmodeled flavor is replayed under anchoring", {
  data <- flavored_fixture_data()
  js <- single_process_joint(make_specification(
    rate = list(creation ~ 1 + indeg),
    choice = list(creation ~ trans),
    model = "DyNAM",
    data = data
  ))
  parameters <- set_parameters(
    js,
    `calls › creation › rate` = c(-3, 0.1),
    `calls › creation › choice` = 0.2
  )
  ties <- as.data.frame(data$ties)
  created_at <- ties$time[ties$flavor %in% "creation"]

  out <- suppressMessages(simulate(
    js,
    seed = 1,
    coef = parameters,
    times = "observed"
  ))

  expect_in(out$events$time, as.numeric(created_at))
  expect_identical(nrow(out$events) + out$diagnostics$n_stamps_skipped, 40L)
  expect_setequal(out$events$flavor, "creation")
  expect_identical(out$diagnostics$n_replayed, 40L)
})

test_that("a stamp whose redraw has no support is skipped and counted", {
  data <- flavored_fixture_data()
  js <- single_process_joint(make_specification(
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  ))
  parameters <- set_parameters(
    js,
    `calls › creation › choice` = 0.2,
    `calls › dissolution › choice` = 0.2
  )
  # Holding every observed sender keeps each sender's out-degree equal to the
  # observed one, so the flavor masks alone never empty a held sender's row.
  # The mark step empties sender 1's row instead, as a support constraint on
  # state the simulation changes could.
  # The steps' dry call passes a one-actor toy with no layer, which the default
  # mark cannot name, so the step answers it directly.
  no_support_for_one <- function(fid, evaluation, handle) {
    if (is.null(evaluation$layer)) {
      return(list(layer = "toy"))
    }
    if (identical(which(evaluation$rate > 0), 1L)) {
      evaluation$choice <- function(sender) rep(0, evaluation$n_actors2)
    }
    default_mark(fid, evaluation, handle)
  }
  ties <- as.data.frame(data$ties)
  sender_one_stamps <- sum(ties$from[!is.na(ties$time)] == 1L)

  out <- simulate(
    js,
    seed = 1,
    coef = parameters,
    steps = set_simulation_steps(mark = no_support_for_one)
  )

  expect_identical(out$times, "observed")
  expect_gt(sender_one_stamps, 0L)
  expect_identical(out$diagnostics$n_stamps_skipped, sender_one_stamps)
  expect_identical(nrow(out$events) + out$diagnostics$n_stamps_skipped, 80L)
  expect_false(1L %in% out$events$sender)
  expect_false(anyNA(out$events$receiver))
})

test_that("an anchored run refuses targets and guards", {
  local_cli_context()
  js <- sim_two_process()
  parameters <- sim_two_process_parameters(js)

  expect_snapshot(
    simulate(js, coef = parameters, times = "observed", n_events = 2),
    error = TRUE
  )
  expect_snapshot(
    simulate(
      js,
      coef = parameters,
      times = "observed",
      horizon = 3,
      control_sim = set_simulation_guard(max_events = 10)
    ),
    error = TRUE
  )
})

test_that("a coordination specification is refused until it simulates", {
  local_cli_context()
  data <- sim_fixture_data()
  coordination <- make_specification(
    choice = ~inertia,
    choice_sub_model = "choice_coordination",
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  expect_snapshot(simulate(coordination, coef = 1), error = TRUE)
})

test_that("an anchored run prints its stamps and skips", {
  local_cli_context()
  data <- flavored_fixture_data()
  js <- single_process_joint(make_specification(
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  ))
  parameters <- set_parameters(
    js,
    `calls › creation › choice` = 0.2,
    `calls › dissolution › choice` = 0.2
  )

  out <- simulate(js, seed = 1, coef = parameters)
  pool <- simulate(js, nsim = 3, seed = 1, coef = parameters)

  expect_snapshot(print(out))
  expect_snapshot(print(pool))
  summary <- summary(pool)
  expect_identical(typeof(summary$n_stamps_skipped), "integer")
  expect_identical(
    summary$n_stamps_skipped,
    vapply(pool, function(run) run$diagnostics$n_stamps_skipped, integer(1))
  )
})

test_that("a free-running run walks no stamps", {
  js <- sim_two_process()

  out <- simulate(
    js,
    seed = 1,
    coef = sim_two_process_parameters(js),
    n_events = 3
  )

  expect_identical(out$diagnostics$n_stamps_skipped, NA_integer_)
})

test_that("a stamp is drawn from the mask strictly before it", {
  # `emails` 2 -> 4 is created at 1.5 and dissolved at N2's own stamp, 2;
  # 2 -> 5 is created at 2. The observed call at t = 2 is evaluated over the
  # atoms strictly before its stamp, as the batch evaluates it: 4 is still
  # excluded there and 5 still allowed.
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls",
      weight = 1
    ),
    data.frame(
      from = c(2L, 2L, 2L),
      to = c(4L, 4L, 5L),
      time = c(1.5, 2, 2),
      layer = "emails",
      weight = c(1, -1, 1)
    )
  )
  data <- list(
    info = list(
      name = "tie",
      focal = "calls",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "event", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
  spec <- make_specification(
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ !tie(emails)
  )
  pool <- suppressWarnings(suppressMessages(
    simulate(spec, nsim = 60, seed = 4, coef = 0)
  ))
  at_tie <- unlist(lapply(pool, function(run) {
    run$events$receiver[run$events$time == 2]
  }))
  expect_length(at_tie, 60L)
  expect_false(any(at_tie == 4L))
  expect_true(any(at_tie == 5L))
})

# A mark step that redraws nothing: each stamp takes its observed receiver,
# in stamp order per layer, so the state every later stamp reads is the
# observed one. It records the choice row of every stamp it marks.
observed_mark_step <- function(ties, seen) {
  cursor <- c(calls = 0L, emails = 0L)
  function(fid, evaluation, handle) {
    if (is.null(evaluation$layer)) {
      return(list(layer = "toy"))
    }
    layer <- evaluation$layer
    cursor[[layer]] <<- cursor[[layer]] + 1L
    observed <- ties[ties$layer == layer, ][cursor[[layer]], ]
    seen$rows[[length(seen$rows) + 1L]] <- list(
      layer = layer,
      time = observed$time,
      choice = evaluation$choice(observed$from)
    )
    list(
      layer = layer,
      sender = observed$from,
      receiver = observed$to,
      increment = 1
    )
  }
}

tied_stamp_data <- function(ties) {
  list(
    info = list(
      name = "tie",
      focal = "calls",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "event", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:6),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
}

test_that("a tied event of another process is not in force at a stamp", {
  # The emails event 2 -> 5 shares N2's call stamp, t = 2, and is scheduled
  # ahead of it. Every event at a stamp is drawn over the state strictly
  # before it, so the call still allows 5 there, as the batch does.
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls",
      weight = 1
    ),
    data.frame(
      from = c(2L, 2L, 3L),
      to = c(4L, 5L, 1L),
      time = c(1.5, 2, 2.5),
      layer = "emails",
      weight = 1
    )
  )
  data <- tied_stamp_data(ties)
  emails <- make_specification(
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  calls <- make_specification(
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ !tie(emails)
  )
  js <- make_joint_specification(emails, calls, data = data)
  seen <- new.env()
  seen$rows <- list()

  out <- suppressWarnings(suppressMessages(simulate(
    js,
    seed = 1,
    coef = set_parameters(
      js,
      `emails › choice` = 0,
      `calls › choice` = 0
    ),
    times = "observed",
    steps = set_simulation_steps(mark = observed_mark_step(ties, seen))
  )))

  at_tie <- Filter(
    function(row) row$layer == "calls" && row$time == 2,
    seen$rows
  )
  expect_length(at_tie, 1L)
  expect_identical(which(at_tie[[1L]]$choice > 0), c(1L, 3L, 5L, 6L))
  expect_identical(nrow(out$events), 8L)
})

test_that("a tied event of the same process is not in force at a stamp", {
  # N2 calls 3, then 5, both at t = 2, under a constraint on the calls layer
  # itself. The second call is drawn over the state strictly before t = 2,
  # so 3 is still allowed there, as the batch has it.
  ties <- data.frame(
    from = c(1L, 2L, 2L, 4L, 5L),
    to = c(2L, 3L, 5L, 5L, 1L),
    time = c(1, 2, 2, 4, 5),
    layer = "calls",
    weight = 1
  )
  data <- tied_stamp_data(ties)
  data$info$update <- c(calls = "increment")
  data$info$directed <- c(calls = TRUE)
  data$info$observation <- c(calls = "event")
  spec <- make_specification(
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ !tie(calls)
  )
  seen <- new.env()
  seen$rows <- list()

  out <- suppressWarnings(suppressMessages(simulate(
    spec,
    seed = 1,
    coef = 0,
    times = "observed",
    steps = set_simulation_steps(mark = observed_mark_step(ties, seen))
  )))

  at_tie <- Filter(function(row) row$time == 2, seen$rows)
  expect_length(at_tie, 2L)
  expect_identical(which(at_tie[[1L]]$choice > 0), c(1L, 3L, 4L, 5L, 6L))
  expect_identical(which(at_tie[[2L]]$choice > 0), c(1L, 3L, 4L, 5L, 6L))
  expect_identical(nrow(out$events), 5L)
})
