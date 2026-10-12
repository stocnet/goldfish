# Rate queries over a candidate sequence: replaying the candidate's own
# schedule through the walk handle by row position, so a query reads the state
# the batch path would have at the same point, stamp ties included.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
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

# A composition row for N3 leaving at time 2.5, in the encodings an authored
# or a rewritten `changes` can carry.
rq_with_composition <- function(
  data = rq_tied_data(),
  time = 2.5,
  node = "N3",
  value = list(list(FALSE)),
  extra = FALSE
) {
  changes <- data.frame(time = time, node = node, var = "active")
  changes$value <- value
  if (extra) {
    changes <- rbind(
      changes,
      data.frame(time = 1, node = "N1", var = "age", value = I(list(30)))
    )
  }
  data$changes <- changes
  data
}

test_that("a candidate with another composition is refused naming it", {
  local_cli_context()
  spec <- rq_joint_spec(rq_with_composition())
  candidate <- rq_with_composition(rq_candidate_data(), time = 1.5)

  expect_error(
    bind_candidate_data(spec, candidate),
    class = "goldfish_candidate_mismatch"
  )
  expect_snapshot(bind_candidate_data(spec, candidate), error = TRUE)
})

test_that("a candidate with composition against none is refused", {
  expect_error(
    bind_candidate_data(rq_joint_spec(), rq_with_composition()),
    class = "goldfish_candidate_mismatch"
  )
  expect_error(
    bind_candidate_data(
      rq_joint_spec(rq_with_composition()),
      rq_candidate_data()
    ),
    class = "goldfish_candidate_mismatch"
  )
})

test_that("a candidate differing in encoding alone is bound", {
  spec <- rq_joint_spec(rq_with_composition())
  encodings <- list(
    index = rq_with_composition(node = 3L),
    posix = rq_with_composition(
      time = as.POSIXct(2.5, origin = "1970-01-01", tz = "GMT")
    ),
    bare = rq_with_composition(value = list(FALSE)),
    attribute = rq_with_composition(extra = TRUE)
  )
  for (encoding in names(encodings)) {
    candidate <- encodings[[encoding]]
    candidate$ties <- rq_candidate_data()$ties
    expect_identical(
      bind_candidate_data(spec, candidate)$data,
      candidate,
      label = encoding
    )
  }

  none <- rq_candidate_data()
  none$changes <- data.frame(
    time = numeric(),
    node = character(),
    var = character()
  )
  none$changes$value <- list()
  expect_identical(
    bind_candidate_data(rq_joint_spec(), none)$data,
    none
  )
})

test_that("flat theta maps onto each fid's full coefficient vector", {
  spec <- rq_layout_spec()
  theta <- c(0.1, 0.2, 0.3, -0.4, -1, 0.7)
  by_fid <- flat_to_fid_theta(spec, theta)

  expect_named(by_fid, c("1", "2", "3", "4"))
  expect_identical(by_fid[["1"]], c(0.1, 0.2))
  expect_identical(by_fid[["2"]], c(0.3, -0.5, -0.4))
  expect_identical(by_fid[["3"]], c(-1, 0.7))
  # The completed choice has no coefficient; its inputs never come from theta.
  expect_identical(by_fid[["4"]], numeric(0))

  params <- set_parameters(
    spec,
    `calls › rate` = c(0.1, 0.2),
    `calls › choice` = c(0.3, NA, -0.4),
    `emails › rate` = c(-1, 0.7)
  )
  expect_identical(
    by_fid,
    stats::setNames(lapply(params$full, unname), as.character(1:4))
  )
})

test_that("each mapped vector has the length the walk evaluates with", {
  spec <- rq_layout_spec()
  by_fid <- flat_to_fid_theta(spec, c(0.1, 0.2, 0.3, -0.4, -1, 0.7))
  handle <- suppressWarnings(walk_open(spec))
  walk_replay_to(handle, 12L)

  for (fid in 1:3) {
    expect_no_error(walk_evaluate(handle, fid, by_fid[[as.character(fid)]]))
  }
  # The offset slot is the friendship column: N3's only friendship tie, to N4,
  # is the one alternative the offset moves.
  choice <- walk_evaluate(handle, 2L, c(0, -0.5, 0), sender = 3L)$value
  expect_equal(choice[4] / choice[2], exp(-0.5))
  expect_equal(choice[1], choice[2])
})

test_that("a theta of the wrong length is refused", {
  expect_error(
    flat_to_fid_theta(rq_layout_spec(), c(0.1, 0.2)),
    class = "goldfish_theta_layout_mismatch"
  )
})

test_that("sub-model positions matching the free layout pass the check", {
  expect_no_error(assert_theta_layout(rq_layout_spec(), rq_layout_submodels()))
})

test_that("sub-model positions off the free layout abort naming the mismatch", {
  local_cli_context()
  spec <- rq_layout_spec()
  shifted <- rq_layout_submodels(calls_choice = c(3L, 7L))

  expect_error(
    assert_theta_layout(spec, shifted),
    class = "goldfish_theta_layout_mismatch"
  )
  expect_snapshot(assert_theta_layout(spec, shifted), error = TRUE)
})

# A pair query at the end of the replayed schedule of `spec`, labels and all.
rq_pair <- function(spec, theta, layer, sender, receiver, flavor = NA) {
  handle <- suppressWarnings(walk_open(spec))
  walk_replay_to(handle, handle$schedule$n + 1L)
  pair_intensity(
    handle,
    simulation_routing(spec, handle),
    theta,
    process = list(layer = layer, flavor = flavor),
    sender = sender,
    receiver = receiver,
    pinned = spec$completed_rates
  )
}

rq_replayed <- function(spec) {
  handle <- suppressWarnings(walk_open(spec))
  walk_replay_to(handle, handle$schedule$n + 1L)
  handle
}

# calls is a modeled DyNAM process, emails a modeled REM process.
rq_pair_spec <- function() {
  suppressWarnings(complete_generative_spec(rq_joint_spec()))
}

rq_pair_theta <- function() {
  list(`1` = c(-1, 0.4), `2` = c(0.5, 0.8), `3` = c(-2, 0.6))
}

# emails is choice-only, so completion pins an intercept-only rate for it.
rq_pinned_spec <- function() {
  data <- rq_tied_data()
  spec <- make_joint_specification(
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia + tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
  suppressWarnings(complete_generative_spec(spec))
}

test_that("a DyNAM pair is the sender's rate times the receiver's choice", {
  spec <- rq_pair_spec()
  theta <- rq_pair_theta()
  handle <- rq_replayed(spec)
  rate <- walk_evaluate(handle, 1L, theta[["1"]])$value
  choice <- walk_evaluate(handle, 2L, theta[["2"]], sender = 2L)$value

  expect_equal(
    rq_pair(spec, theta, "calls", "N2", "N3"),
    rate[2] * choice[3]
  )
})

test_that("a REM pair is the dyad's own rate", {
  spec <- rq_pair_spec()
  theta <- rq_pair_theta()
  rate <- walk_evaluate(rq_replayed(spec), 3L, theta[["3"]])$value

  # Dyad (i, j) sits at (i - 1) * n2 + j.
  expect_equal(
    rq_pair(spec, theta, "emails", "N5", "N2"),
    rate[(5 - 1) * 6 + 2]
  )
})

test_that("an excluded pair returns an exact zero", {
  spec <- rq_pair_spec()
  theta <- rq_pair_theta()

  expect_identical(rq_pair(spec, theta, "calls", "N2", "N2"), 0)
  expect_identical(rq_pair(spec, theta, "emails", "N4", "N4"), 0)
})

test_that("a completed intercept-only rate is evaluated at its pin", {
  spec <- rq_pinned_spec()
  theta <- list(
    `1` = c(-1, 0.4),
    `2` = c(0.5, 0.8),
    `3` = numeric(0),
    `4` = 0.5
  )
  choice <- walk_evaluate(rq_replayed(spec), 4L, 0.5, sender = 5L)$value
  intercept <- spec$completed_rates[["3"]]$intercept

  expect_equal(
    rq_pair(spec, theta, "emails", "N5", "N2"),
    exp(intercept) * choice[2]
  )
})

test_that("a completed uniform choice spreads the rate over the receivers", {
  spec <- rq_layout_spec()
  theta <- flat_to_fid_theta(spec, c(0.1, 0.2, 0.3, -0.4, -1, 0.7))
  rate <- walk_evaluate(rq_replayed(spec), 3L, theta[["3"]])$value

  # Five receivers are open to N4: every node but itself.
  expect_equal(rq_pair(spec, theta, "emails", "N4", "N1"), rate[4] / 5)
})

test_that("an unknown actor label aborts naming the query", {
  local_cli_context()
  spec <- rq_pair_spec()
  theta <- rq_pair_theta()

  expect_error(
    rq_pair(spec, theta, "calls", "N2", "N9"),
    class = "goldfish_rate_query_unresolved"
  )
  expect_snapshot(rq_pair(spec, theta, "calls", "N2", "N9"), error = TRUE)
})

test_that("an unknown process aborts naming the query", {
  local_cli_context()
  spec <- rq_pair_spec()
  theta <- rq_pair_theta()

  expect_error(
    rq_pair(spec, theta, "friendship", "N1", "N2"),
    class = "goldfish_rate_query_unresolved"
  )
  expect_snapshot(rq_pair(spec, theta, "friendship", "N1", "N2"), error = TRUE)
})

test_that("an ordered or coordination process aborts naming its regime", {
  local_cli_context()
  data <- rq_tied_data()
  ordered <- make_joint_specification(
    make_specification(
      rate = ~ indeg(friendship),
      choice = ~inertia,
      rate_sub_model = "rate_ordered",
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~indeg,
      choice = ~inertia,
      rate_sub_model = "rate_ordered",
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
  coordination <- make_joint_specification(
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~inertia,
      choice_sub_model = "choice_coordination",
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg,
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )

  expect_error(
    rq_pair(ordered, list(), "calls", "N1", "N2"),
    class = "goldfish_rate_query_unresolved"
  )
  expect_snapshot(rq_pair(ordered, list(), "calls", "N1", "N2"), error = TRUE)
  expect_error(
    rq_pair(coordination, list(), "calls", "N1", "N2"),
    class = "goldfish_rate_query_unresolved"
  )
  expect_snapshot(
    rq_pair(coordination, list(), "calls", "N1", "N2"),
    error = TRUE
  )
})

# calls is a modeled DyNAM process that reads emails; texts is a modeled REM
# process; friendship and emails stay exogenous. Candidate events, counted in
# realization order: calls 1 (row 3), texts 1 (row 5), calls 2 and 3 tied at
# t = 2 (rows 9, 10), texts 2 (row 13), calls 4 (row 16). The emails row 2 -> 1
# (row 11) shares event 4's stamp and follows it.
rq_exo_data <- function() {
  data <- rq_tied_data()
  data$ties <- rbind(
    data$ties,
    data.frame(
      from = c(6L, 1L),
      to = c(1L, 6L),
      time = c(1.5, 2.5),
      layer = "texts"
    )
  )
  data$info$update[["texts"]] <- "increment"
  data$info$directed[["texts"]] <- TRUE
  data$info$observation[["texts"]] <- "event"
  data
}

rq_exo_spec <- function() {
  data <- rq_exo_data()
  spec <- make_joint_specification(
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ inertia + tie(emails),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + inertia,
      layer = "texts",
      model = "REM",
      data = data
    ),
    data = data
  )
  suppressWarnings(complete_generative_spec(spec))
}

rq_exo_theta <- function() {
  list(`1` = c(-1, 0.4), `2` = c(0.5, 0.8), `3` = c(-2, 0.6))
}

rq_query <- function(after, layer, sender, receiver) {
  list(after = after, layer = layer, sender = sender, receiver = receiver)
}

# The pair intensity read by hand: replay to `row`, then read the pair.
rq_manual <- function(spec, theta, row, layer, sender, receiver) {
  handle <- suppressWarnings(walk_open(spec))
  walk_replay_to(handle, row)
  pair_intensity(
    handle,
    simulation_routing(spec, handle),
    theta,
    process = list(layer = layer, flavor = NA),
    sender = sender,
    receiver = receiver,
    pinned = spec$completed_rates
  )
}

test_that("the exogenous fixture's schedule has the layout the tests rely on", {
  handle <- suppressWarnings(walk_open(rq_exo_spec()))
  schedule <- handle$schedule
  realized <- which(
    !schedule$dependent & schedule$layer %in% handle$focal_layers
  )
  expect_identical(realized, c(3L, 5L, 9L, 10L, 13L, 16L))
  expect_identical(schedule$layer[11], "emails")
  expect_identical(schedule$time[c(9, 10, 11)], c(2, 2, 2))
})

test_that("a single query equals a pair read after a manual replay", {
  spec <- rq_exo_spec()
  theta <- rq_exo_theta()

  expect_equal(
    query_rate_at_position(spec, theta, list(rq_query(2, "calls", "N2", "N3"))),
    rq_manual(spec, theta, 6L, "calls", "N2", "N3")
  )
  expect_equal(
    query_rate_at_position(spec, theta, list(rq_query(0, "texts", "N1", "N6"))),
    rq_manual(spec, theta, 1L, "texts", "N1", "N6")
  )
})

test_that("a batch of queries opens one handle and applies each row once", {
  spec <- rq_exo_spec()
  opened <- 0L
  injected <- 0L
  stops <- integer(0)
  real_open <- walk_open
  real_inject <- walk_inject
  real_replay <- walk_replay_to
  local_mocked_bindings(
    walk_open = function(...) {
      opened <<- opened + 1L
      real_open(...)
    },
    walk_inject = function(...) {
      injected <<- injected + 1L
      real_inject(...)
    },
    walk_replay_to = function(handle, row, ...) {
      stops <<- c(stops, row)
      real_replay(handle, row, ...)
    }
  )

  query_rate_at_position(
    spec,
    rq_exo_theta(),
    list(
      rq_query(5, "calls", "N1", "N2"),
      rq_query(1, "texts", "N6", "N1"),
      rq_query(5, "texts", "N1", "N6"),
      rq_query(3, "calls", "N3", "N4")
    )
  )

  expect_identical(opened, 1L)
  # Replayed forward once per distinct position; the five realizations up to
  # event 5 are each injected exactly once.
  expect_identical(stops, c(4L, 10L, 14L))
  expect_identical(injected, 5L)
})

test_that("results return in the caller's order, not position order", {
  spec <- rq_exo_spec()
  theta <- rq_exo_theta()
  queries <- list(
    rq_query(6, "calls", "N1", "N2"),
    rq_query(0, "texts", "N6", "N1"),
    rq_query(4, "calls", "N3", "N4")
  )

  expect_equal(
    query_rate_at_position(spec, theta, queries),
    c(
      rq_manual(spec, theta, 17L, "calls", "N1", "N2"),
      rq_manual(spec, theta, 1L, "texts", "N6", "N1"),
      rq_manual(spec, theta, 11L, "calls", "N3", "N4")
    )
  )
})

test_that("a position excludes a later row sharing the event's stamp", {
  spec <- rq_exo_spec()
  theta <- rq_exo_theta()
  # tie(emails) moves N2's choice of N1 once the emails row 11 is applied.
  at_event_4 <- query_rate_at_position(
    spec,
    theta,
    list(rq_query(4, "calls", "N2", "N1"))
  )

  expect_equal(at_event_4, rq_manual(spec, theta, 11L, "calls", "N2", "N1"))
  expect_false(isTRUE(all.equal(
    at_event_4,
    rq_manual(spec, theta, 12L, "calls", "N2", "N1")
  )))
})

test_that("tied candidate events are addressable one at a time", {
  spec <- rq_exo_spec()
  theta <- rq_exo_theta()
  # Events 3 and 4 share t = 2; inertia on N3 -> N4 appears only with event 4.
  both <- query_rate_at_position(
    spec,
    theta,
    list(rq_query(3, "calls", "N3", "N4"), rq_query(4, "calls", "N3", "N4"))
  )

  expect_equal(both[1], rq_manual(spec, theta, 10L, "calls", "N3", "N4"))
  expect_equal(both[2], rq_manual(spec, theta, 11L, "calls", "N3", "N4"))
  expect_false(isTRUE(all.equal(both[1], both[2])))
})

# Replay `handle` to the state right after candidate event `after`'s
# realization, the position query_rate_at_position() reads at.
rq_replay_after <- function(handle, after) {
  schedule <- handle$schedule
  realized <- which(
    !schedule$dependent & schedule$layer %in% handle$focal_layers
  )
  walk_replay_to(handle, c(1L, realized + 1L)[after + 1L])
  handle
}

test_that("each of two tied events is its own position", {
  # Events 3 and 4 are calls 2 -> 3 and 3 -> 4, both realized at t = 2; the
  # emails row 2 -> 1 shares the stamp and sorts after them.
  first <- rq_replay_after(suppressWarnings(walk_open(rq_exo_spec())), 3L)
  networks <- first$state$networks
  expect_identical(networks[["calls"]][2, 3], 1)
  expect_identical(networks[["calls"]][3, 4], 0)
  expect_identical(networks[["emails"]][2, 1], 0)

  both <- rq_replay_after(suppressWarnings(walk_open(rq_exo_spec())), 4L)
  networks <- both$state$networks
  expect_identical(networks[["calls"]][2, 3], 1)
  expect_identical(networks[["calls"]][3, 4], 1)
  expect_identical(networks[["emails"]][2, 1], 0)
})

# calls models creation and dissolution; the t = 3 renewal 2 -> 3 belongs to
# no process and shares its stamp with the dissolution 3 -> 4. The calls
# realizations are, in schedule order: creation 3 -> 4 (t = 1), dissolution
# 1 -> 2 (t = 2), renewal 2 -> 3 and dissolution 3 -> 4 (t = 3).
rq_flavor_tie_spec <- function() {
  data <- add_flavor(
    list(
      info = list(
        name = "toy4",
        focal = "calls",
        update = c(calls = "increment"),
        directed = c(calls = TRUE),
        observation = c(calls = "event")
      ),
      nodes = data.frame(
        label = c("A", "B", "C", "D"),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = data.frame(
        from = c(1L, 3L, 1L, 2L, 3L),
        to = c(2L, 4L, 2L, 3L, 4L),
        time = c(NA, 1, 2, 3, 3),
        layer = "calls",
        weight = c(1, 1, -1, 1, -1),
        stringsAsFactors = FALSE
      )
    ),
    layer = "calls",
    values_equivalence = c(creation = 1, dissolution = -1)
  )
  data$ties$flavor[data$ties$from == 2L] <- "renewal"
  make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg + outdeg),
    choice = list(creation ~ inertia, dissolution ~ inertia),
    model = "DyNAM",
    data = data
  )
}

test_that("an unmodeled flavor's row is a realization at every position", {
  spec <- rq_flavor_tie_spec()
  opened <- suppressWarnings(walk_open(spec))
  schedule <- opened$schedule
  realized <- which(
    !schedule$dependent & schedule$layer %in% opened$focal_layers
  )
  expect_identical(schedule$time[realized], c(1, 2, 3, 3))

  expected <- opened$state$networks[["calls"]]
  for (k in seq(0L, length(realized))) {
    if (k > 0L) {
      row <- realized[k]
      cell <- cbind(schedule$sender[row], schedule$receiver[row])
      expected[cell] <- expected[cell] + schedule$value[[row]]
    }
    handle <- rq_replay_after(suppressWarnings(walk_open(spec)), k)
    expect_identical(
      handle$state$networks[["calls"]],
      expected,
      label = paste("after =", k)
    )
  }
})

test_that("the rate-query replay steps the handle only through its own API", {
  callers <- character(0)
  real_apply <- walk_apply_object_event
  local_mocked_bindings(
    walk_apply_object_event = function(...) {
      callers <<- c(callers, rlang::call_name(sys.call(-1L)))
      real_apply(...)
    }
  )

  query_rate_at_position(
    rq_exo_spec(),
    rq_exo_theta(),
    list(rq_query(3, "calls", "N2", "N1"), rq_query(6, "calls", "N1", "N2"))
  )

  # Only the handle's own steps write an object: walk_advance() for an
  # exogenous row, walk_apply_resolved() for an injected realization.
  expect_gt(length(callers), 0L)
  expect_in(callers, c("walk_advance", "walk_apply_resolved"))
})

test_that("completed rates do not move with the candidate", {
  spec <- rq_pinned_spec()
  theta <- list(
    `1` = c(-1, 0.4),
    `2` = c(0.5, 0.8),
    `3` = numeric(0),
    `4` = 0.5
  )
  shifted <- bind_candidate_data(spec, rq_candidate_data())
  query <- list(rq_query(0, "emails", "N5", "N2"))

  expect_identical(shifted$completed_rates, spec$completed_rates)

  at_open <- query_rate_at_position(spec, theta, query)
  expect_equal(query_rate_at_position(shifted, theta, query), at_open)

  # At open the choice is the same under both candidates, so the intensity is
  # the pinned intercept times that choice.
  choice <- walk_evaluate(
    suppressWarnings(walk_open(spec)),
    4L,
    0.5,
    sender = 5L
  )$value
  expect_equal(at_open, exp(spec$completed_rates[["3"]]$intercept) * choice[2])
})

test_that("a position outside the candidate's events aborts naming it", {
  local_cli_context()
  spec <- rq_exo_spec()
  queries <- list(rq_query(7, "calls", "N1", "N2"))

  expect_error(
    query_rate_at_position(spec, rq_exo_theta(), queries),
    class = "goldfish_rate_query_unresolved"
  )
  expect_snapshot(
    query_rate_at_position(spec, rq_exo_theta(), queries),
    error = TRUE
  )
})

# A fit's candidates all bind to one specification, so its composition form is
# computed once and each candidate adds one more.
test_that("the specification's composition form is computed once per bind set", {
  rm(list = ls(composition_memo), envir = composition_memo)
  forms <- 0L
  original <- composition_normal_form
  testthat::local_mocked_bindings(
    composition_normal_form = function(data) {
      forms <<- forms + 1L
      original(data)
    }
  )
  spec <- rq_joint_spec(rq_with_composition())
  candidate <- rq_with_composition()
  candidate$ties <- rq_candidate_data()$ties

  bind_candidate_data(spec, candidate)
  bind_candidate_data(spec, candidate)
  expect_identical(forms, 3L)
})
