# augment_seq_sim(): the model-driven augmenter, run as supplied steps on the
# simulation driver. Each step selects among the observed flips still to place
# (support-applicable) and the next observed relational event in proportion to
# their rates, draws a receiver among the pair's remaining observed receivers,
# and draws the waiting time from the pair's rate: censored at a breakpoint the
# walk reports, truncated at the next relational event or the wave end. Its
# recorded density is checked against hand-computed values on small fixtures.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# The completed specification, its recipe on the observation-start grid, and
# the sim augmenter over them, built as estimate_dynes() builds it.
sim_augmenter <- function(spec) {
  waves <- panel_wave_grid(spec, start_time = 0)
  # Pinned panel rates warn on completion.
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  recipe <- compile_augmentation_recipe(completed, waves)
  control <- set_algorithm_em(
    augmenter = set_augmenter_options(routine = "sim"),
    seed = 11L
  )
  list(
    spec = completed,
    recipe = recipe,
    waves = waves,
    augmenter = augment_seq_sim(completed, recipe, control)
  )
}

draw_sim <- function(built, n, theta, seed = 1L) {
  withr::with_seed(seed, {
    lapply(seq_len(n), function(k) built$augmenter$draw(1L, k, theta)[[1L]])
  })
}

layer_rows <- function(draw, layer) {
  ties <- as.data.frame(draw$data$ties)
  ties[ties$layer == layer & !is.na(ties$time), , drop = FALSE]
}

# The log density of a truncated exponential at rate `rate` on (0, width).
log_trunc_exp <- function(wait, rate, width) {
  log(rate) - rate * wait - log(-expm1(-rate * width))
}

# --- Fixtures -----------------------------------------------------------------

# A count panel `calls` (one rise 1 -> 2 by wave 5, observation start 0) whose
# rate reads the outdegree of an unmodeled `visits` layer, beside a modeled
# `emails` stream. The visit 1 -> 3 at `visit_time` is an exogenous row: a
# breakpoint of the walk when it falls inside the interval. The email is at
# `email_time`, after the last wave by default.
breakpoint_data <- function(visit_time = 2, email_time = 7) {
  list(
    info = list(
      name = "breakpoint",
      focal = "emails",
      update = c(
        calls = "increment",
        emails = "increment",
        visits = "increment"
      ),
      directed = c(calls = TRUE, emails = TRUE, visits = TRUE),
      observation = c(calls = "panel", emails = "event", visits = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:3),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(
      data.frame(from = 1L, to = 2L, time = 5, weight = 1, layer = "calls"),
      data.frame(
        from = 2L,
        to = 3L,
        time = email_time,
        weight = 1,
        layer = "emails"
      ),
      data.frame(
        from = 1L,
        to = 3L,
        time = visit_time,
        weight = 1,
        layer = "visits"
      )
    )
  )
}

# theta: calls intercept, outdeg(visits), calls inertia, emails intercept,
# emails inertia.
breakpoint_spec <- function(data = breakpoint_data()) {
  make_joint_specification(
    make_specification(
      rate = ~ 1 + outdeg(visits),
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# A count panel `friendship` (waves 5 and 10, observation start 0), choice
# only, so its rate is completed and pinned per wave period, beside a calls
# stream observed after the last wave. 1 -> 2 rises by wave 5; 3 -> 4 and
# 4 -> 1 by wave 10, so the two periods pin different plateaus.
pinned_data <- function() {
  friendship <- data.frame(
    from = c(2L, 1L, 3L, 4L),
    to = c(3L, 2L, 4L, 1L),
    time = c(NA, 5, 10, 10),
    weight = 1,
    layer = "friendship"
  )
  calls <- data.frame(
    from = 1L,
    to = 3L,
    time = 12,
    weight = 1,
    layer = "calls"
  )
  list(
    info = list(
      name = "pinned",
      focal = "calls",
      update = c(friendship = "increment", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = "panel", calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(friendship, calls)
  )
}

# theta: friendship indeg, calls intercept, calls inertia.
pinned_spec <- function(data = pinned_data()) {
  make_joint_specification(
    make_specification(
      choice = ~indeg,
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# A reciprocity-coupled friendship panel: N1 may create 1 -> 2 only while
# 2 -> 1 stands, and N2 dissolves 2 -> 1 in the same interval. A greedy draw
# that places the dissolution first strands the creation, so the first
# interval completes only when the creation is selected first: with the pinned
# plateaus e^-3 (creation) and e^log(1/5) (dissolution) that is
# 0.05 / (0.05 + 0.2) = 0.2. N1 dissolves 1 -> 2 by wave 10, which nothing
# couples. Calls at 2 and 7 are observed relational events. With `holder` 3
# the initial tie is 3 -> 1 instead, so 2 -> 1 never stands and no draw of the
# first interval completes.
coupled_data <- function(holder = 2L) {
  friendship <- data.frame(
    from = c(holder, holder, 1L, 1L),
    to = c(1L, 1L, 2L, 2L),
    time = c(NA, 5, 5, 10),
    weight = c(1, -1, 1, -1),
    layer = "friendship"
  )
  calls <- data.frame(
    from = c(3L, 4L),
    to = c(4L, 1L),
    time = c(2, 7),
    weight = 1,
    layer = "calls"
  )
  data <- list(
    info = list(
      name = "coupled",
      focal = "calls",
      update = c(friendship = "increment", calls = "increment"),
      directed = c(friendship = TRUE, calls = TRUE),
      observation = c(friendship = "panel", calls = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(friendship, calls)
  )
  add_flavor(
    data,
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

# theta: creation indeg, dissolution indeg, calls intercept, calls inertia.
coupled_spec <- function(data = coupled_data(), coupled = TRUE) {
  constraint <- if (coupled) ~ tie(friendship) | recip(friendship)
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data,
      support_constraint = constraint
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# --- Contract --------------------------------------------------------------

test_that("the sim routine is registered and builds an augmenter", {
  expect_identical(dynes_augmenter_constructors()$sim, augment_seq_sim)
  built <- sim_augmenter(breakpoint_spec())
  expect_s3_class(built$augmenter, "goldfishAugmenter")
  expect_identical(built$augmenter$routine, "sim")

  control <- set_algorithm_em(
    augmenter = set_augmenter_options(routine = "sim")
  )
  resolved <- resolve_dynes_augmenter(built$spec, built$recipe, control)
  expect_s3_class(resolved, "goldfishAugmenter")
})

test_that("each draw returns one sequence per draw index", {
  built <- sim_augmenter(breakpoint_spec())
  theta <- c(-1, 1, 0, 0, 0)
  draws <- withr::with_seed(1L, built$augmenter$draw(1L, 3:4, theta))
  expect_length(draws, 2L)
  expect_contains(names(draws[[1L]]), c("data", "log_proposal", "restarted"))
  expect_true(is.finite(draws[[1L]]$log_proposal))
})

test_that("the draws run as supplied steps on the simulation driver", {
  built <- sim_augmenter(breakpoint_spec())
  steps <- NULL
  local_mocked_bindings(
    simulate_replicate = function(spec, provider, steps, ...) {
      steps <<- steps
      stop("reached the driver")
    }
  )
  expect_error(draw_sim(built, 1L, c(-1, 1, 0, 0, 0)), "reached the driver")
  expect_s3_class(steps, "goldfishSimSteps")
  expect_true(is.function(steps$mark))
  expect_true(is.function(steps$clock))
  expect_true(is.function(steps$accept))
  # The package evaluator is left in place: the proposal's rates are the
  # target's.
  expect_null(steps$evaluate)
})

# --- Endpoint hitting ---------------------------------------------------------

test_that("every draw hits every wave with times strictly inside", {
  built <- sim_augmenter(coupled_spec(coupled = FALSE))
  diff <- diff_panel_waves(built$spec$data, "friendship", c(0, 5, 10))
  for (draw in draw_sim(built, 10L, c(0.2, 0.2, -1, 0.5))) {
    expect_no_error(validate_endpoint_hitting(draw$data, diff))
    rows <- layer_rows(draw, "friendship")
    rows <- rows[rows$time > 0, , drop = FALSE]
    expect_false(any(rows$time %in% c(0, 5, 10)))
    # The observed calls keep their times.
    expect_equal(layer_rows(draw, "calls")$time, c(2, 7))
  }
})

# --- Proposal density ---------------------------------------------------------

test_that("a breakpoint-free interval draws one truncated exponential", {
  # The visit falls after the last wave, so the calls rate is e^theta1 over
  # the whole interval (0, 5), and the only remaining receiver of N1 is N2.
  built <- sim_augmenter(breakpoint_spec(breakpoint_data(visit_time = 7)))
  theta <- c(-1, 1, 0.3, 0, 0)
  rate <- exp(theta[[1L]])
  for (draw in draw_sim(built, 10L, theta)) {
    t <- layer_rows(draw, "calls")$time
    expect_gt(t, 0)
    expect_lt(t, 5)
    expect_equal(draw$log_proposal, log_trunc_exp(t, rate, 5))
  }
})

test_that("a breakpoint inside the interval censors the draw and redraws", {
  # The visit at 2 raises N1's calls rate from e^theta1 to e^(theta1 +
  # theta2). A draw is an untruncated exponential censored at the breakpoint,
  # then a truncated exponential at the post-breakpoint rate on (2, 5).
  built <- sim_augmenter(breakpoint_spec())
  theta <- c(-1, 1, 0.3, 0, 0)
  before <- exp(theta[[1L]])
  after <- exp(theta[[1L]] + theta[[2L]])
  draws <- draw_sim(built, 30L, theta)
  times <- vapply(draws, function(d) layer_rows(d, "calls")$time, numeric(1))
  # Both segments are reached: P(censored) = e^-(2 e^-1) is about 0.48.
  expect_true(any(times < 2))
  expect_true(any(times > 2))
  for (k in seq_along(draws)) {
    t <- times[[k]]
    expected <- if (t < 2) {
      log(before) - before * t
    } else {
      -before * 2 + log_trunc_exp(t - 2, after, 3)
    }
    expect_equal(draws[[k]]$log_proposal, expected)
  }
})

test_that("the draw reads the rates a replay of the sequence reads", {
  built <- sim_augmenter(breakpoint_spec())
  theta <- c(-1, 1, 0.3, 0, 0)
  context <- new_augmenter_context(built$spec, built$recipe)
  draw <- draw_sim(built, 1L, theta)[[1L]]
  t <- layer_rows(draw, "calls")$time

  # A walk opened on the delivered sequence and replayed to each segment's
  # start gives the rates the recorded density was computed at.
  handle <- open_augmenter_walk(context, draw$data)
  by_fid <- flat_to_fid_theta(built$spec, theta)
  before <- walk_evaluate(handle, 1L, by_fid[["1"]])$value[[1L]]
  walk_advance(handle, 2)
  after <- walk_evaluate(handle, 1L, by_fid[["1"]])$value[[1L]]
  expected <- if (t < 2) {
    log(before) - before * t
  } else {
    -before * 2 + log_trunc_exp(t - 2, after, 3)
  }
  expect_equal(draw$log_proposal, expected)
})

test_that("a relational-event selection enters the density", {
  # The email at 3 competes with N1's call from the start: the call first
  # with probability e^theta1 / (e^theta1 + e^theta4), truncated at the email;
  # else the email, then the call truncated at the wave end.
  built <- sim_augmenter(breakpoint_spec(breakpoint_data(7, email_time = 3)))
  theta <- c(-0.5, 1, 0.3, -0.2, 0)
  call_rate <- exp(theta[[1L]])
  email_rate <- exp(theta[[4L]])
  total <- call_rate + email_rate
  draws <- draw_sim(built, 30L, theta)
  times <- vapply(draws, function(d) layer_rows(d, "calls")$time, numeric(1))
  expect_true(any(times < 3))
  expect_true(any(times > 3))
  for (k in seq_along(draws)) {
    t <- times[[k]]
    expected <- if (t < 3) {
      log(call_rate / total) + log_trunc_exp(t, call_rate, 3)
    } else {
      log(email_rate / total) + log_trunc_exp(t - 3, call_rate, 2)
    }
    expect_equal(draws[[k]]$log_proposal, expected)
  }
})

test_that("the proposal normalizes over observed flips, the target over all", {
  # N1's inertia choice is uniform over N2 and N3, both structurally
  # supported, but only 1 -> 2 is an observed flip. The proposal places the
  # receiver with probability 1 (no receiver term in the density); the
  # target's choice probability of the same receiver is 1/2. Their ratio is
  # the observed share of the full support.
  built <- sim_augmenter(breakpoint_spec(breakpoint_data(visit_time = 7)))
  theta <- c(-1, 1, 0.3, 0, 0)
  draw <- draw_sim(built, 1L, theta)[[1L]]
  t <- layer_rows(draw, "calls")$time
  expect_equal(draw$log_proposal, log_trunc_exp(t, exp(theta[[1L]]), 5))

  context <- new_augmenter_context(built$spec, built$recipe)
  handle <- open_augmenter_walk(context, draw$data)
  choice <- walk_evaluate(handle, 2L, theta[[3L]], sender = 1L)$value
  full <- choice[[2L]] / sum(choice)
  observed <- choice[[2L]] / sum(choice[2L])
  expect_equal(full, 0.5)
  expect_equal(full / observed, sum(choice[2L]) / sum(choice))
  expect_false(isTRUE(all.equal(full / observed, 1)))
})

# --- Pinned-fid time pin ------------------------------------------------------

test_that("a pinned rate's waiting times use the period's plateau, not theta", {
  built <- sim_augmenter(pinned_spec())
  pin <- built$spec$completed_rates
  map <- built$spec$process_map
  rate_fid <- map$fid[map$family == "rate" & map$layer == "friendship"]
  plateau <- exp(pin[[as.character(rate_fid)]]$intercept)
  # The two periods pin different plateaus, so reading the wrong one shows.
  expect_false(isTRUE(all.equal(plateau[[1L]], plateau[[2L]])))

  expected_log_q <- function(draw) {
    rows <- layer_rows(draw, "friendship")
    first <- rows$time[rows$time > 0 & rows$time < 5]
    second <- sort(rows$time[rows$time > 5 & rows$time < 10])
    # Period 2: two senders at the same plateau, 1/2 to be first, then the
    # other truncated at the wave end from the first's time.
    log_trunc_exp(first, plateau[[1L]], 5) +
      log(1 / 2) +
      log_trunc_exp(second[[1L]] - 5, plateau[[2L]], 5) +
      log_trunc_exp(
        second[[2L]] - second[[1L]],
        plateau[[2L]],
        10 - second[[1L]]
      )
  }
  draws <- draw_sim(built, 10L, c(0.4, -1, 0.5))
  for (draw in draws) {
    rows <- layer_rows(draw, "friendship")
    rows <- rows[rows$time > 0, , drop = FALSE]
    expect_length(rows$time, 3L)
    expect_equal(draw$log_proposal, expected_log_q(draw))
    # The wave-end truncation keeps each draw within one plateau: the
    # interval-1 flip in [0, 5), the interval-2 flips in [5, 10).
    expect_equal(sum(rows$time < 5), 1L)
    expect_true(all(rows$time[rows$time > 5] < 10))
  }

  # Other parameters move no pinned waiting time: the density of the same
  # draws is the same at any theta.
  other <- draw_sim(built, 10L, c(-2, 1, -1))
  expect_equal(
    vapply(other, `[[`, numeric(1), "log_proposal"),
    vapply(other, expected_log_q, numeric(1))
  )
})

# --- Reject-and-restart -------------------------------------------------------

test_that("a coupling-free specification never restarts", {
  built <- sim_augmenter(coupled_spec(coupled = FALSE))
  draws <- draw_sim(built, 10L, c(0.2, 0.2, -1, 0.5))
  for (draw in draws) {
    expect_false(draw$restarted)
    expect_equal(draw$completion$rate, c(1, 1))
    expect_equal(draw$log_proposal, draw$log_proposal_raw)
  }
  expect_length(built$augmenter$coupling, 0L)
})

test_that("a stranded draw restarts and its density is restart-normalized", {
  built <- sim_augmenter(coupled_spec())
  expect_match(built$augmenter$coupling, "recip(friendship)", fixed = TRUE)
  theta <- c(0.2, 0.2, -1, 0.5)
  draws <- draw_sim(built, 60L, theta)
  for (draw in draws) {
    # Every delivered draw is support-legal: the creation precedes the
    # dissolution that would disable it.
    rows <- layer_rows(draw, "friendship")
    create <- rows$time[rows$from == 1L & rows$to == 2L & rows$time < 5]
    dissolve <- rows$time[rows$from == 2L & rows$to == 1L & rows$time > 0]
    expect_lt(create, dissolve)

    completion <- draw$completion
    expect_equal(completion$interval, 1:2)
    expect_equal(completion$rate, completion$successes / completion$attempts)
    expect_equal(completion$theta_ref, theta)
    # The second interval has no coupling to strand on.
    expect_equal(completion$rate[[2L]], 1)
    expect_equal(
      draw$log_proposal,
      draw$log_proposal_raw - sum(log(completion$rate))
    )
    expect_identical(draw$restarted, any(completion$attempts > 1L))
  }
  # Pooled, the acceptance rate estimates the completion probability 0.2.
  attempts <- vapply(draws, function(d) d$completion$attempts[[1L]], 1L)
  expect_equal(length(draws) / sum(attempts), 0.2, tolerance = 0.35)
  expect_true(any(vapply(draws, `[[`, logical(1), "restarted")))
})

test_that("a draw emits no warning of its own; the loop owns it", {
  built <- sim_augmenter(coupled_spec())
  expect_no_warning(draw_sim(built, 10L, c(0.2, 0.2, -1, 0.5)))
})

# --- Stamp ties and wave-time events ----------------------------------------

# A zero-width truncated draw never leaves its redraw loop. The cap on
# `runif()` calls makes such a draw fail instead of hanging; a completing draw
# makes a few dozen calls.
local_runif_cap <- function(cap = 1e4L, env = parent.frame()) {
  runif <- stats::runif
  calls <- 0L
  local_mocked_bindings(
    runif = function(...) {
      calls <<- calls + 1L
      if (calls > cap) {
        stop("runif() called more than ", cap, " times")
      }
      runif(...)
    },
    .package = "stats",
    .env = env
  )
}

# breakpoint_data() with a second email at `time`, N3 -> N1.
tied_email_data <- function(email_time, visit_time = 7) {
  data <- breakpoint_data(visit_time = visit_time, email_time = email_time)
  data$ties <- rbind(
    data$ties,
    data.frame(
      from = 3L,
      to = 1L,
      time = email_time,
      weight = 1,
      layer = "emails"
    )
  )
  data
}

test_that("a draw over tied relational events with a flip left completes", {
  local_runif_cap()
  # Both emails at 3 compete with N1's call; once the first email is placed,
  # the call has no room before its tied sibling.
  built <- sim_augmenter(breakpoint_spec(tied_email_data(email_time = 3)))
  draws <- draw_sim(built, 20L, c(0, 1, 0, 0, 0))
  for (draw in draws) {
    expect_true(is.finite(draw$log_proposal))
    expect_equal(layer_rows(draw, "emails")$time, c(3, 3))
    call <- layer_rows(draw, "calls")$time
    expect_true(call > 0 && call < 5 && call != 3)
  }
})

test_that("a draw with a relational event at the wave end completes", {
  local_runif_cap()
  # The email at w_1 = 5 belongs to interval (0, 5]; selected before the
  # call, it would leave the call no room before the wave.
  built <- sim_augmenter(breakpoint_spec(breakpoint_data(7, email_time = 5)))
  draws <- draw_sim(built, 20L, c(0, 1, 0, 0, 0))
  for (draw in draws) {
    expect_true(is.finite(draw$log_proposal))
    expect_equal(layer_rows(draw, "emails")$time, 5)
    call <- layer_rows(draw, "calls")$time
    expect_true(call > 0 && call < 5)
    # The email is placed last with certainty: no selection term, only the
    # call's truncated exponential at rate e^0 on (0, 5).
    expect_equal(draw$log_proposal, log_trunc_exp(call, 1, 5))
  }
})

test_that("an interval no draw can complete aborts at the ceiling", {
  local_cli_context()
  local_mocked_bindings(restart_ceiling = function() 5L)
  # 2 -> 1 never stands, so N1's creation of 1 -> 2 never has support.
  built <- sim_augmenter(coupled_spec(coupled_data(holder = 3L)))
  expect_snapshot(
    draw_sim(built, 1L, c(0.2, 0.2, -1, 0.5)),
    error = TRUE
  )
  expect_error(
    draw_sim(built, 1L, c(0.2, 0.2, -1, 0.5)),
    class = "goldfish_dynes_restart_ceiling"
  )
})

test_that("the last-call support check reads the walk after its advance", {
  local_cli_context()
  local_mocked_bindings(restart_ceiling = function() 5L)
  # N4 leaves at 2, the stamp of the call 3 -> 4, and is back at 2.5. Only
  # that leave lies between the state the call is selected at and the one
  # strictly before its stamp, so a single clock call selects the call,
  # advances to the stamp and checks it. Read after the advance, the call's
  # support is gone, and every attempt at the interval strands.
  data <- coupled_data()
  data$nodes$active <- TRUE
  data$changes <- data.frame(time = c(2, 2.5), node = "N4", var = "active")
  data$changes$value <- list(list(FALSE), list(TRUE))
  built <- sim_augmenter(coupled_spec(data))
  theta <- c(0.2, 0.2, -1, 0.5)
  expect_error(
    draw_sim(built, 1L, theta),
    class = "goldfish_dynes_restart_ceiling"
  )
  # A cache cleared only on entering the clock answers the check from the
  # selection's state, where N4 is present, and the draw completes.
  local_mocked_bindings(
    sim_move = function(state, handle, to, events = NULL) {
      if (!is.null(events)) {
        replay_stamps(handle, events)
      }
      walk_advance(handle, to, inclusive = FALSE)
    }
  )
  expect_true(is.finite(draw_sim(built, 1L, theta)[[1L]]$log_proposal))
})

# coupled_data()'s friendship panel (unconstrained here) beside the calls
# `calls` and an unmodeled `visits` stream `visits`, whose rows are exogenous
# breakpoints of the walk.
stamp_data <- function(calls, visits = NULL) {
  friendship <- data.frame(
    from = c(2L, 2L, 1L, 1L),
    to = c(1L, 1L, 2L, 2L),
    time = c(NA, 5, 5, 10),
    weight = c(1, -1, 1, -1),
    layer = "friendship"
  )
  calls$weight <- 1
  calls$layer <- "calls"
  ties <- rbind(friendship, calls)
  if (!is.null(visits)) {
    visits$layer <- "visits"
    ties <- rbind(ties, visits)
  }
  data <- list(
    info = list(
      name = "stamp",
      focal = "calls",
      update = c(
        friendship = "increment",
        calls = "increment",
        visits = "increment"
      ),
      directed = c(friendship = TRUE, calls = TRUE, visits = TRUE),
      observation = c(friendship = "panel", calls = "event", visits = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = ties
  )
  add_flavor(
    data,
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

# theta: creation indeg, dissolution indeg, calls intercept, calls inertia.
# Only the calls carry a support constraint, so every strand is theirs.
stamp_spec <- function(data, constraint) {
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data,
      support_constraint = constraint
    ),
    data = data
  )
}

test_that("a tied sibling's support is read strictly before the stamp", {
  local_mocked_bindings(restart_ceiling = function() 20L)
  # N3 calls N4 and N1 at 2 under a one-call-per-sender constraint: once the
  # first call is injected, N3 has used its call, but the second is read at
  # the state strictly before 2, where N3 has made none.
  data <- stamp_data(
    calls = data.frame(
      from = c(3L, 3L, 4L),
      to = c(4L, 1L, 1L),
      time = c(2, 2, 7)
    )
  )
  built <- sim_augmenter(
    stamp_spec(data, ~ outdeg(calls, type = "ego") < 1)
  )
  draws <- draw_sim(built, 5L, c(0.2, 0.2, -1, 0.5))
  for (draw in draws) {
    expect_false(draw$restarted)
    calls <- layer_rows(draw, "calls")
    expect_equal(calls$time[calls$from == 3L], c(2, 2))
  }
})

test_that("a tied sibling is placed with no selection", {
  # Between the first email's placement and its sibling's, no weight is read
  # and nothing is sampled: the sibling adds no term to the density.
  weight <- sim_relational_weight
  sample <- sample_index
  accept <- sim_accept
  log <- character()
  local_mocked_bindings(
    sim_relational_weight = function(...) {
      log <<- c(log, "weight")
      weight(...)
    },
    sample_index = function(...) {
      log <<- c(log, "sample")
      sample(...)
    },
    sim_accept = function(state, event) {
      if (identical(state$pending$kind, "relational")) {
        log <<- c(log, "placed")
      }
      accept(state, event)
    }
  )
  built <- sim_augmenter(breakpoint_spec(tied_email_data(email_time = 3)))
  draw_sim(built, 10L, c(0, 1, 0, 0, 0))
  # Both emails sit at 3, so the placements come in tied pairs.
  placed <- which(log == "placed")
  expect_length(placed, 20L)
  for (pair in seq(1L, length(placed), by = 2L)) {
    between <- log[seq.int(placed[[pair]], placed[[pair + 1L]])]
    expect_false(any(between %in% c("weight", "sample")))
  }
})

# The calls 3 -> 4 at 2 and 4 -> 1 at 7 under `~ !tie(visits)`, beside the
# visits `visits`.
visit_spec <- function(visits) {
  data <- stamp_data(
    calls = data.frame(from = c(3L, 4L), to = c(4L, 1L), time = c(2, 7)),
    visits = visits
  )
  sim_augmenter(stamp_spec(data, ~ !tie(visits)))
}

test_that("a call whose support returns before its stamp is placed", {
  # The visit 3 -> 4 at 1.2 rules the call out, and its removal at 1.6 rules
  # it back in. A call selected before 1.2 is clipped at both rows; only the
  # last clock call, past 1.6, reads its support, so it is placed. (A
  # selection inside (1.2, 1.6) weighs the call 0, as any selection does.)
  built <- visit_spec(
    data.frame(from = 3L, to = 4L, time = c(1.2, 1.6), weight = c(1, -1))
  )
  next_step <- sim_next_step
  accept <- sim_accept
  log <- character()
  local_mocked_bindings(
    sim_next_step = function(state, t, handle) {
      step <- next_step(state, t, handle)
      if (
        identical(step$kind, "relational") &&
          state$relational$time[[step$event]] == 2 &&
          t < 1.2
      ) {
        log <<- c(log, "selected early")
      }
      step
    },
    sim_accept = function(state, event) {
      if (isTRUE(event$stranded)) {
        log <<- c(log, "stranded")
      } else if (identical(state$pending$kind, "relational")) {
        log <<- c(log, "placed")
      }
      accept(state, event)
    }
  )
  draw_sim(built, 10L, c(0.2, 0.2, -1, 0.5))
  early <- which(log == "selected early")
  expect_gt(length(early), 0L)
  for (at in early) {
    expect_identical(log[[at + 1L]], "placed")
  }
})

test_that("a selected call whose support goes before its stamp strands", {
  local_mocked_bindings(restart_ceiling = function() 20L)
  # The visit 3 -> 4 at 1.5 rules the call out. A call selected before 1.5
  # was supported then, but not on the last clock call before 2, so no
  # attempt places it.
  built <- visit_spec(data.frame(from = 3L, to = 4L, time = 1.5, weight = 1))
  expect_error(
    draw_sim(built, 1L, c(0.2, 0.2, -1, 0.5)),
    class = "goldfish_dynes_restart_ceiling"
  )
})

test_that("the clock reaches a stamp without falling short of it", {
  # The walk already stands at the stamp when the driver injects the event
  # there, so a clock one unit short would inject behind it. From these
  # values (a 1.1 coupled reference draw) `t + (s - t)` falls short.
  t <- 0.13212533219080139
  s <- 0.43566131209412967
  expect_lt(t + (s - t), s)
  expect_gte(t + sim_wait_until(t, s), s)
  withr::with_seed(1L, {
    from <- stats::runif(1000L, 0, 0.4)
    to <- stats::runif(1000L, 0.4, 5)
  })
  wait <- mapply(sim_wait_until, from, to)
  expect_true(all(from + wait >= to))
  expect_true(all(from + wait - to <= to * .Machine$double.eps))
  # A sibling whose clock already stands past the stamp waits 0.
  expect_identical(sim_wait_until(s + s * .Machine$double.eps, s), 0)
})
