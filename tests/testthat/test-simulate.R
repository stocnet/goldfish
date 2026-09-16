# The simulation driver: one loop over the merged single-clock walk, with the
# plug points the model variants replace. What is checked here is that the loop
# draws from the model it was given (targets, guard, competing processes, the
# state feeding back), that every accepted `coef` form resolves to the same
# run, and that a supplied step replaces the driver's own without the package's
# evaluator ever seeing the variant's parameter shape.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("a fixed-count run generates exactly that many events", {
  js <- sim_two_process()
  out <- simulate(
    js,
    nsim = 1,
    seed = 1,
    coef = sim_two_process_parameters(js),
    n_events = 12
  )

  expect_s3_class(out, "goldfishSim")
  expect_equal(nrow(out$events), 12)
  expect_false(out$capped)
  expect_identical(out$diagnostics$stop_reason, "n_events")
  # Drawn from a clock, so the sequence is strictly ordered in time.
  expect_false(is.unsorted(out$events$time))
  # Both processes compete on the one clock.
  expect_setequal(unique(out$events$layer), c("calls", "emails"))
})

test_that("drawn marks stay inside the risk set", {
  js <- sim_two_process()
  out <- simulate(
    js,
    nsim = 1,
    seed = 7,
    coef = sim_two_process_parameters(js),
    n_events = 40
  )

  expect_true(all(out$events$sender %in% 1:6))
  expect_true(all(out$events$receiver %in% 1:6))
  # One-mode, non-reflexive: an actor never calls itself.
  expect_true(all(out$events$sender != out$events$receiver))
})

test_that("a drawn dissolution removes an existing tie", {
  data <- flavored_fixture_data()
  js <- single_process_joint(make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(creation ~ trans, dissolution ~ trans),
    model = "DyNAM",
    data = data
  ))
  parameters <- set_parameters(
    js,
    `calls › creation › rate` = c(-3, 0.1),
    `calls › creation › choice` = 0.2,
    `calls › dissolution › rate` = c(-3, 0.1),
    `calls › dissolution › choice` = 0.2
  )
  out <- simulate(js, nsim = 1, seed = 1, coef = parameters, n_events = 60)
  events <- out$events

  expect_setequal(events$flavor, c("creation", "dissolution"))
  expect_identical(
    events$increment,
    ifelse(events$flavor == "creation", 1, -1)
  )
  # Replayed over the observed history, each creation lands where no tie is
  # and each dissolution where one is: the value and the masks agree.
  ties <- as.data.frame(data$ties)
  history <- ties[is.na(ties$time), ]
  state <- matrix(0, 12L, 12L)
  state[cbind(history$from, history$to)] <- 1
  tie_held <- logical(nrow(events))
  for (k in seq_len(nrow(events))) {
    cell <- cbind(events$sender[k], events$receiver[k])
    tie_held[k] <- state[cell] == 1
    state[cell] <- state[cell] + events$increment[k]
  }
  expect_identical(tie_held, events$flavor == "dissolution")
})

test_that("a seed makes a run reproducible", {
  js <- sim_two_process()
  pars <- sim_two_process_parameters(js)
  first <- simulate(js, nsim = 1, seed = 99, coef = pars, n_events = 15)
  second <- simulate(js, nsim = 1, seed = 99, coef = pars, n_events = 15)

  expect_equal(first$events, second$events)
})

test_that("nsim > 1 returns one result per replicate", {
  js <- sim_two_process()
  out <- simulate(
    js,
    nsim = 3,
    seed = 3,
    coef = sim_two_process_parameters(js),
    n_events = 5
  )

  expect_length(out, 3)
  expect_true(all(vapply(out, inherits, logical(1), "goldfishSim")))
  expect_equal(vapply(out, function(x) nrow(x$events), integer(1)), rep(5L, 3))
  # Independent draws, not the same sequence three times.
  expect_false(isTRUE(all.equal(out[[1]]$events, out[[2]]$events)))
})

test_that("a constant provider reproduces the plain coef path", {
  js <- sim_two_process()
  theta <- sim_two_process_theta()
  provider <- set_parameter_provider(
    at = function(k, t, handle, latent) list(parameters = theta)
  )

  plain <- simulate(
    js,
    nsim = 1,
    seed = 11,
    coef = sim_two_process_parameters(js),
    n_events = 15
  )
  supplied <- simulate(js, nsim = 1, seed = 11, coef = provider, n_events = 15)

  expect_equal(plain$events, supplied$events)
})

test_that("a horizon stops the run at its time", {
  js <- sim_two_process()
  out <- simulate(
    js,
    nsim = 1,
    seed = 5,
    coef = sim_two_process_parameters(js),
    horizon = 3
  )

  expect_true(all(out$events$time <= 3))
  expect_identical(out$diagnostics$stop_reason, "horizon")
  expect_identical(out$diagnostics$end_time, 3)
})

test_that("the max_events guard caps and flags the run", {
  js <- sim_two_process()
  expect_warning(
    out <- simulate(
      js,
      nsim = 1,
      seed = 5,
      coef = sim_two_process_parameters(js),
      n_events = 500,
      max_events = 20
    ),
    class = "goldfish_sim_guard_stop"
  )

  expect_true(out$capped)
  expect_identical(out$diagnostics$stop_reason, "max_events")
  expect_lte(nrow(out$events), 20)
})

test_that("a supplied evaluate step replaces the package's own", {
  js <- sim_two_process()
  theta <- sim_two_process_theta()
  # Row-wise product over an n_ego x p parameter matrix -- the per-actor
  # random-effect shape, defined here rather than carried by goldfish.
  per_actor_evaluate <- function(stats, theta, risk, meta) {
    ego <- rep(seq_len(nrow(theta)), each = nrow(stats) / nrow(theta))
    linear <- rowSums(stats * theta[ego, , drop = FALSE])
    values <- numeric(length(linear))
    if (identical(meta$family, "rate")) {
      values[risk$active_sender] <- exp(linear[risk$active_sender])
      return(values)
    }
    block <- (meta$sender - 1L) * meta$n_actors2 + seq_len(meta$n_actors2)
    probabilities <- exp(linear[block])
    probabilities[meta$sender] <- 0
    probabilities / sum(probabilities)
  }
  as_matrix_theta <- function(theta, n_actors) {
    lapply(theta, function(v) matrix(v, n_actors, length(v), byrow = TRUE))
  }
  matrix_theta <- as_matrix_theta(theta, 6L)

  steps <- set_simulation_steps(evaluate = per_actor_evaluate)
  provider <- set_parameter_provider(
    at = function(k, t, handle, latent) list(parameters = matrix_theta)
  )

  # The package's evaluator must not be reached: the matrix is the caller's
  # model, and goldfish's six evaluator sites carry the vector case only.
  local_mocked_bindings(
    default_evaluate = function(...) {
      stop("the package evaluator was handed the variant's parameters")
    }
  )
  out <- simulate(
    js,
    nsim = 1,
    seed = 21,
    coef = provider,
    steps = steps,
    n_events = 10
  )

  expect_equal(nrow(out$events), 10)
  expect_true(all(out$events$sender != out$events$receiver))
})

test_that("a supplied evaluate step replaces only the modeled processes'", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  families_seen <- character(0)
  rate_only_evaluate <- function(stats, theta, risk, meta) {
    families_seen <<- c(families_seen, meta$family)
    linear <- as.numeric(stats %*% theta)
    values <- numeric(length(linear))
    values[risk$active_sender] <- exp(linear[risk$active_sender])
    values
  }
  steps <- set_simulation_steps(evaluate = rate_only_evaluate)
  # Construction dry-runs the step once; only the run's calls count.
  families_seen <- character(0)

  out <- suppressWarnings(
    simulate(spec, seed = 5, coef = c(-1, 0.1), steps = steps, n_events = 10)
  )

  # The completed uniform choice is the package's to draw, never the step's.
  expect_identical(unique(families_seen), "rate")
  expect_equal(nrow(out$events), 10)
  expect_true(all(out$events$sender != out$events$receiver))
})

test_that("an authored intercept-only rate reads its intercept from coef", {
  data <- sim_fixture_data()
  js <- single_process_joint(make_specification(
    rate = ~1,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  ))
  parameters <- set_parameters(
    js,
    `calls › rate` = -1.5,
    `calls › choice` = c(0.2, 0.3)
  )
  totals <- numeric(0)
  recording_clock <- function(rates, t, handle) {
    totals <<- c(totals, sum(rates))
    list(wait = stats::rexp(1L, sum(rates)), fid = 1L, kind = "event")
  }
  steps <- set_simulation_steps(clock = recording_clock)
  totals <- numeric(0)

  expect_no_warning(
    out <- simulate(
      js,
      seed = 2,
      coef = parameters,
      steps = steps,
      n_events = 3
    ),
    class = "goldfish_pinned_rate_warning"
  )

  expect_equal(nrow(out$events), 3)
  expect_identical(out$process_map$regime, c("modeled", "modeled"))
  # Six present senders at the authored intercept, before any event.
  expect_equal(totals[[1L]], 6 * exp(-1.5))
})

test_that("a per-actor intercept deviation scales only that actor's rate", {
  js <- sim_two_process()
  handle <- walk_open(js)
  common <- c(-1, 0.1)
  deviation <- 0.7
  per_actor <- matrix(common, 6L, 2L, byrow = TRUE)
  per_actor[4L, 1L] <- per_actor[4L, 1L] + deviation
  row_wise <- function(stats, theta, risk, meta) {
    ego <- rep(seq_len(nrow(theta)), each = nrow(stats) / nrow(theta))
    linear <- rowSums(stats * theta[ego, , drop = FALSE])
    values <- numeric(length(linear))
    values[risk$active_sender] <- exp(linear[risk$active_sender])
    values
  }

  shared <- goldfish:::evaluate_fid(
    handle,
    1L,
    matrix(common, 6L, 2L, byrow = TRUE),
    row_wise
  )
  deviated <- goldfish:::evaluate_fid(handle, 1L, per_actor, row_wise)

  expect_equal(deviated[4L], shared[4L] * exp(deviation))
  expect_equal(deviated[-4L], shared[-4L])
})

test_that("a two-regime provider switches the parameters it returns", {
  js <- sim_two_process()
  theta <- sim_two_process_theta()
  # P1 only: the regime flips at every step and the parameters follow it. The
  # realized path is what the result records per event.
  provider <- set_parameter_provider(
    init = function(replicate, handle) list(regime = 1L),
    at = function(k, t, handle, latent) {
      regime <- if (k %% 2L == 0L) 1L else 2L
      scaled <- theta
      scaled[["1"]] <- theta[["1"]] * c(1, if (regime == 1L) 1 else -1)
      list(parameters = scaled, latent = list(regime = regime))
    }
  )

  out <- simulate(js, nsim = 1, seed = 4, coef = provider, n_events = 8)

  expect_equal(nrow(out$events), 8)
  expect_length(out$latent, 8)
  expect_setequal(
    unique(vapply(out$latent, function(x) x$regime, integer(1))),
    c(1L, 2L)
  )
})

test_that("simulate rejects parameters it cannot resolve", {
  local_cli_context()
  js <- sim_two_process()

  expect_error(simulate(js), class = "goldfish_sim_bad_coef")
  # A numeric vector names one process; this specification has four.
  expect_error(
    simulate(js, coef = c(0, 0), n_events = 2),
    class = "goldfish_sim_bad_coef"
  )
  expect_error(
    simulate(js, coef = "nope", n_events = 2),
    class = "goldfish_sim_bad_coef"
  )
  expect_error(
    simulate(js, coef = sim_two_process_parameters(js), steps = list()),
    class = "goldfish_sim_bad_step"
  )
})

test_that("a provider leaving a process unparameterized aborts", {
  local_cli_context()
  js <- sim_two_process()
  provider <- set_parameter_provider(
    at = function(k, t, handle, latent) {
      list(parameters = sim_two_process_theta()[1:2])
    }
  )

  expect_error(
    simulate(js, coef = provider, n_events = 2),
    class = "goldfish_sim_bad_parameters"
  )
})

test_that("anchoring a timed model is announced, then refused for now", {
  local_cli_context()
  js <- sim_two_process()

  expect_snapshot(
    simulate(
      js,
      coef = sim_two_process_parameters(js),
      times = "observed",
      n_events = 2
    ),
    error = TRUE
  )
})

test_that("an explicit times equal to the default is silent and recorded", {
  js <- sim_two_process()
  parameters <- sim_two_process_parameters(js)

  expect_no_message(
    requested <- suppressWarnings(simulate(
      js,
      seed = 1,
      coef = parameters,
      times = "generated",
      n_events = 2
    ))
  )
  derived <- suppressWarnings(
    simulate(js, seed = 1, coef = parameters, n_events = 2)
  )

  expect_identical(requested$times, "generated")
  expect_identical(requested$times_source, "requested")
  expect_identical(derived$times, "generated")
  expect_identical(derived$times_source, "specification")
  expect_equal(requested$events, derived$events)
})

test_that("a model with no estimated clock cannot generate times", {
  local_cli_context()
  data <- sim_fixture_data()
  ordered <- make_specification(
    rate = ~indeg,
    rate_sub_model = "rate_ordered",
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  coordination <- make_specification(
    choice = ~inertia,
    choice_sub_model = "choice_coordination",
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  expect_snapshot(
    simulate(ordered, coef = 1, times = "generated", n_events = 2),
    error = TRUE
  )
  expect_snapshot(
    simulate(coordination, coef = 1, times = "generated", n_events = 2),
    error = TRUE
  )
})

test_that("a single-process specification simulates through the joint path", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  out <- simulate(
    spec,
    nsim = 1,
    seed = 2,
    coef = set_parameters(
      single_process_joint(spec),
      `calls › rate` = c(-1, 0.1),
      `calls › choice` = c(0.2, 0.3)
    ),
    n_events = 6
  )

  expect_s3_class(out, "goldfishSim")
  expect_equal(nrow(out$events), 6)
  expect_true(all(out$events$layer == "calls"))
})

test_that("a rate-only DyNAM gains a uniform choice it draws from", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  # The completion warns once; no extra coefficient is asked for.
  expect_warning(
    out <- simulate(spec, nsim = 1, seed = 3, coef = c(-1, 0.1), n_events = 30),
    "uniform choice"
  )

  regime <- out$process_map$regime
  family <- out$process_map$family
  expect_identical(regime[family == "rate"], "modeled")
  expect_identical(regime[family == "choice"], "completed")
  # Drawn equiprobably over the risk set, so no self-loops and every other
  # actor is reachable.
  expect_true(all(out$events$sender != out$events$receiver))
  expect_gt(length(unique(out$events$receiver)), 1)
})

test_that("an intercept-only rate with no choice is refused at entry", {
  local_cli_context()
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~1,
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  # Refused before completion, so no uniform-choice warning fires first.
  expect_snapshot(
    simulate(spec, coef = -1, n_events = 2),
    error = TRUE
  )
})

test_that("a flavored gap's completed choice draws only existing ties", {
  data <- flavored_fixture_data()
  js <- single_process_joint(make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    choice = list(creation ~ trans),
    model = "DyNAM",
    data = data
  ))
  parameters <- set_parameters(
    js,
    `calls › creation › rate` = c(-3, 0.1),
    `calls › creation › choice` = 0.2,
    `calls › dissolution › rate` = c(-3, 0.1)
  )
  out <- suppressWarnings(
    simulate(js, seed = 8, coef = parameters, n_events = 60)
  )
  events <- out$events

  expect_contains(events$flavor, "dissolution")
  expect_identical(
    out$process_map$regime[out$process_map$flavor == "dissolution"],
    c("modeled", "completed")
  )
  ties <- as.data.frame(data$ties)
  history <- ties[is.na(ties$time), ]
  state <- matrix(0, 12L, 12L)
  state[cbind(history$from, history$to)] <- 1
  tie_held <- logical(nrow(events))
  for (k in seq_len(nrow(events))) {
    cell <- cbind(events$sender[k], events$receiver[k])
    tie_held[k] <- state[cell] == 1
    state[cell] <- state[cell] + events$increment[k]
  }
  expect_identical(tie_held, events$flavor == "dissolution")
})

test_that("a deferred choice draws inside its process's support mask", {
  data <- sim_fixture_data()
  # The walk registers the objects a formula term reads, plus the focal layer
  # the run writes to, so the constraint's object still needs a term:
  # `outdeg(friendship)` steps the mask.
  spec <- suppressMessages(make_specification(
    rate = ~ 1 + indeg + outdeg(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data,
    support_constraint = ~ tie(friendship)
  ))

  out <- suppressMessages(suppressWarnings(
    simulate(spec, seed = 9, coef = c(-1, 0.1, 0.1), n_events = 40)
  ))

  # friendship holds 1 -> 2, 2 -> 3, 3 -> 4 and 4 -> 5, so the mask allows each
  # sender exactly one receiver and leaves senders 5 and 6 out of the rate.
  expect_equal(nrow(out$events), 40)
  expect_in(out$events$sender, 1:4)
  expect_identical(out$events$receiver, out$events$sender + 1L)
})

test_that("a specification of only exogenous covariates simulates", {
  data("social_evolution", envir = environment())
  spec <- make_specification(
    rate = ~ 1 + ego(floor),
    choice = ~ alter(floor),
    layer = "calls",
    model = "DyNAM",
    data = social_evolution
  )
  coef <- set_parameters(
    single_process_joint(spec),
    `calls › rate` = c(-5, 0.1),
    `calls › choice` = 0.2
  )

  out <- simulate(spec, nsim = 1, seed = 4, coef = coef, n_events = 6)

  expect_equal(nrow(out$events), 6)
  expect_true(all(out$events$layer == "calls"))
})

test_that("drawn events land on a focal layer no term reads", {
  data("social_evolution", envir = environment())
  spec <- make_specification(
    rate = ~ 1 + ego(floor),
    choice = ~ alter(floor),
    layer = "calls",
    model = "DyNAM",
    data = social_evolution
  )
  handle <- walk_open(spec)
  before <- handle$state$networks$calls[1, 2]

  walk_inject(
    handle,
    list(layer = "calls", sender = 1L, receiver = 2L, increment = 1)
  )

  expect_equal(handle$state$networks$calls[1, 2], before + 1)
})

test_that("a flavored layer read by no term draws under its derived masks", {
  data <- flavored_fixture_data()
  data$nodes$score <- rep(c(0, 1), length.out = nrow(data$nodes))
  js <- single_process_joint(make_specification(
    rate = list(creation ~ 1 + ego(score), dissolution ~ 1 + ego(score)),
    choice = list(creation ~ alter(score), dissolution ~ alter(score)),
    model = "DyNAM",
    data = data
  ))
  parameters <- set_parameters(
    js,
    `calls › creation › rate` = c(-3, 0.1),
    `calls › creation › choice` = 0.2,
    `calls › dissolution › rate` = c(-3, 0.1),
    `calls › dissolution › choice` = 0.2
  )

  out <- simulate(js, nsim = 1, seed = 1, coef = parameters, n_events = 40)
  events <- out$events

  expect_setequal(events$flavor, c("creation", "dissolution"))
  # The masks are derived from `calls`, which only the drawn events update:
  # each creation lands where no tie is and each dissolution where one is.
  ties <- as.data.frame(data$ties)
  history <- ties[is.na(ties$time), ]
  state <- matrix(0, 12L, 12L)
  state[cbind(history$from, history$to)] <- 1
  tie_held <- logical(nrow(events))
  for (k in seq_len(nrow(events))) {
    cell <- cbind(events$sender[k], events$receiver[k])
    tie_held[k] <- state[cell] == 1
    state[cell] <- state[cell] + events$increment[k]
  }
  expect_identical(tie_held, events$flavor == "dissolution")
})

test_that("a choice-only DyNAM defaults to time-anchored", {
  local_cli_context()
  data <- sim_fixture_data()
  spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  # No rate is fabricated: no completion warning precedes the refusal of the
  # time-anchored run, which does not exist yet.
  expect_snapshot(
    simulate(spec, coef = c(0.2, 0.3), n_events = 2),
    error = TRUE
  )
})

test_that("a choice-only DyNAM generates only when asked", {
  local_cli_context()
  data <- sim_fixture_data()
  spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  # The warning names the override, and the completed rate is pinned at the
  # crude rate of the observed events.
  expect_snapshot(
    out <- simulate(
      spec,
      seed = 3,
      coef = c(0.2, 0.3),
      times = "generated",
      n_events = 6
    )
  )

  expect_equal(nrow(out$events), 6)
  expect_identical(out$process_map$regime, c("completed", "modeled"))
  expect_identical(out$times, "generated")
  expect_identical(out$times_source, "requested")
  expect_snapshot(print(out))
})

test_that("a run with no target stops at the observed horizon", {
  js <- sim_two_process()
  parameters <- sim_two_process_parameters(js)

  # The calls end at 5, after the last friendship change at 4: running on past
  # that change is running on observed state, so nothing warns.
  expect_no_warning(
    runs <- lapply(1:4, function(seed) {
      simulate(js, nsim = 1, seed = seed, coef = parameters)
    })
  )

  for (out in runs) {
    expect_identical(out$diagnostics$stop_reason, "horizon")
    expect_false(out$capped)
    expect_identical(out$diagnostics$end_time, 5)
    expect_lte(max(out$events$time), 5)
  }
  # The count is drawn, not fixed at the eight observed events.
  counts <- vapply(runs, function(out) nrow(out$events), integer(1))
  expect_gt(length(unique(counts)), 1)
})

test_that("a window's expiry rows do not extend the default horizon", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg(calls, window = 2),
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  parameters <- set_parameters(
    single_process_joint(spec),
    `calls › rate` = c(-1, 0.1),
    `calls › choice` = c(0.2, 0.3)
  )

  # The last call at 5 expires at 7; the window still ends at 5.
  expect_no_warning(
    out <- simulate(spec, nsim = 1, seed = 2, coef = parameters)
  )

  expect_identical(out$diagnostics$stop_reason, "horizon")
  expect_identical(out$diagnostics$end_time, 5)
  expect_lte(max(out$events$time), 5)
})

test_that("a clock that cannot advance stops and flags the replicate", {
  local_cli_context()
  js <- sim_two_process()
  draws <- 0L
  stalling_clock <- function(rates, t, handle) {
    draws <<- draws + 1L
    # Three ordinary waits, then one too small to move the clock.
    wait <- if (draws <= 3L) 0.01 else 0
    list(wait = wait, fid = 1L, kind = "event")
  }
  steps <- set_simulation_steps(clock = stalling_clock)
  # Construction dry-runs the step once; only the run's draws count.
  draws <- 0L

  expect_warning(
    out <- simulate(
      js,
      nsim = 1,
      seed = 1,
      coef = sim_two_process_parameters(js),
      steps = steps,
      n_events = 50
    ),
    class = "goldfish_sim_guard_stop"
  )

  expect_true(out$capped)
  expect_identical(out$diagnostics$stop_reason, "clock_resolution")
  # No event is recorded at the stalled timestamp.
  expect_equal(nrow(out$events), 3)
  expect_false(anyDuplicated(out$events$time) > 0)
  expect_s3_class(out$diagnostics$trajectory, "data.frame")
  expect_named(out$diagnostics$trajectory, c("time", "total_rate"))
  expect_snapshot(print(out))
})

test_that("a runaway REM fit is flagged at a guard, not stamped", {
  data("social_evolution", envir = environment())
  spec <- suppressMessages(make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "REM",
    data = social_evolution
  ))

  # The estimates of this model on these data: every call raises the
  # receiver's rate by a factor of three, and the total rate runs away.
  expect_warning(
    out <- simulate(spec, nsim = 1, seed = 1, coef = c(-18.657, 1.150)),
    class = "goldfish_sim_guard_stop"
  )

  expect_true(out$capped)
  expect_identical(out$diagnostics$stop_reason, "rate_trajectory")
  expect_lt(nrow(out$events), 4390)
  expect_false(anyDuplicated(out$events$time) > 0)
  trajectory <- out$diagnostics$trajectory
  expect_gt(max(trajectory$total_rate), trajectory$total_rate[1])
})

test_that("one runaway replicate does not end the call", {
  js <- sim_two_process()
  calm <- sim_two_process_theta()
  runaway <- calm
  runaway[["1"]] <- c(-1, 3)
  # The second replicate's calls feed back on their own rate.
  provider <- set_parameter_provider(
    init = function(replicate, handle) list(runaway = replicate == 2L),
    at = function(k, t, handle, latent) {
      list(parameters = if (latent$runaway) runaway else calm)
    }
  )
  warnings <- list()

  out <- withCallingHandlers(
    simulate(js, nsim = 3, seed = 1, coef = provider, horizon = 4),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_length(out, 3)
  capped <- vapply(out, function(run) run$capped, logical(1))
  expect_identical(capped, c(FALSE, TRUE, FALSE))
  expect_length(warnings, 1)
  expect_s3_class(warnings[[1]], "goldfish_sim_guard_stop")
})

test_that("a call warns once about guard stops and the frozen state", {
  local_cli_context()
  js <- sim_two_process()
  parameters <- sim_two_process_parameters(js)
  warnings <- list()

  # Thirty events outrun the window, which ends at 5, and the guard.
  out <- withCallingHandlers(
    simulate(
      js,
      nsim = 3,
      seed = 4,
      coef = parameters,
      n_events = 200,
      max_events = 30
    ),
    warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1)
  expect_s3_class(warnings[[1]], "goldfish_sim_guard_stop")
  expect_s3_class(warnings[[1]], "goldfish_sim_frozen_exogenous")
  expect_snapshot(
    simulate(
      js,
      nsim = 3,
      seed = 4,
      coef = parameters,
      n_events = 200,
      max_events = 30
    ) |>
      invisible()
  )
})

test_that("a fitted model simulates on the data it is given", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  fit <- estimate_dynam(spec, sub_model = "rate")

  expect_warning(
    out <- simulate(fit, nsim = 1, seed = 8, data = data, n_events = 5),
    "uniform choice"
  )
  expect_s3_class(out, "goldfishSim")
  expect_equal(nrow(out$events), 5)
  expect_true(all(out$events$layer == "calls"))
})

test_that("a fitted timed rate rebuilds with the intercept it estimated", {
  data <- sim_fixture_data()
  spec <- suppressMessages(make_specification(
    rate = ~indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  ))
  fit <- suppressMessages(estimate_dynam(spec, sub_model = "rate"))

  # Estimation added the intercept; the stored formula does not write it.
  expect_identical(deparse1(fit$formula), "calls ~ indeg")
  expect_no_message(
    out <- suppressWarnings(
      simulate(fit, nsim = 1, seed = 8, data = data, n_events = 5)
    )
  )
  expect_equal(nrow(out$events), 5)
})

test_that("a fit with an interaction before a main effect keeps its terms", {
  data("social_evolution", envir = environment())
  spec <- make_specification(
    rate = ~ 1 + indeg:ego(floor) + outdeg,
    layer = "calls",
    model = "DyNAM",
    data = social_evolution
  )
  fit <- estimate_dynam(spec, sub_model = "rate")

  # Simulating the fit is simulating its specification at its estimates: each
  # coefficient reaches the statistic it was estimated on.
  from_fit <- suppressWarnings(simulate(
    fit,
    seed = 1,
    data = social_evolution,
    n_events = 30
  ))
  from_spec <- suppressWarnings(simulate(
    spec,
    seed = 1,
    coef = fit$parameters,
    n_events = 30
  ))

  expect_identical(from_fit$events, from_spec$events)
})

test_that("a fitted model without data says so", {
  local_cli_context()
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  fit <- estimate_dynam(spec, sub_model = "rate")

  # A goldfishFit stores its formula and estimates, never the stocnet.
  expect_error(simulate(fit, n_events = 2), class = "goldfish_sim_no_data")
})

test_that("a fitted REM model simulates on its focal layer", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~ 1 + indeg,
    layer = "calls",
    model = "REM",
    data = data
  )
  fit <- suppressMessages(estimate_rem(spec, sub_model = "rate"))

  expect_identical(as.character(times_of(fit)), "generated")
  # The rebuilt specification keeps the fit's intercept, so the walk has no
  # intercept to add and nothing to announce.
  expect_no_message(
    out <- simulate(fit, nsim = 1, seed = 3, data = data, n_events = 4)
  )
  expect_s3_class(out, "goldfishSim")
  expect_equal(nrow(out$events), 4)
  expect_true(all(out$events$layer == "calls"))
})

test_that("a fitted ordered rate rebuilds ordered and refuses a clock", {
  data <- sim_fixture_data()
  spec <- make_specification(
    rate = ~indeg,
    rate_sub_model = "rate_ordered",
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  fit <- estimate_dynam(spec, sub_model = "rate")

  times <- times_of(fit)
  expect_identical(as.character(times), "observed")
  expect_identical(attr(times, "reason"), "ordered rate")
  # Rebuilt timed, the rate would draw waiting times from a clock the fit
  # never estimated.
  expect_error(
    simulate(fit, data = data, times = "generated", n_events = 2),
    class = "goldfish_sim_no_clock"
  )
})
