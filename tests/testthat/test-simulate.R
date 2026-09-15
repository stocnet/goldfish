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
  expect_identical(out$diagnostics$stop_reason, "target")
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
  expect_identical(out$diagnostics$stop_reason, "target")
})

test_that("the max_events guard caps and flags the run", {
  js <- sim_two_process()
  out <- simulate(
    js,
    nsim = 1,
    seed = 5,
    coef = sim_two_process_parameters(js),
    n_events = 500,
    max_events = 20
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

test_that("time-anchored simulation is refused until it exists", {
  local_cli_context()
  js <- sim_two_process()

  expect_error(
    simulate(
      js,
      coef = sim_two_process_parameters(js),
      times = "observed",
      n_events = 2
    ),
    class = "goldfish_sim_unsupported"
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
  # The walk registers only the objects a formula term reads, so both the
  # constraint's object and the focal layer the run writes to need a term:
  # `outdeg(friendship)` steps the mask, `indeg` gives `calls` a place.
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

test_that("a choice-only DyNAM has no clock to run free", {
  local_cli_context()
  data <- sim_fixture_data()
  spec <- make_specification(
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )

  # Deliberately not rate-completed: no baseline hazard is fabricated.
  expect_error(
    simulate(spec, coef = c(0.2, 0.3), n_events = 2),
    class = "goldfish_sim_no_clock"
  )
})

test_that("a run with no target generates the observed event count", {
  js <- sim_two_process()
  out <- suppressWarnings(
    simulate(js, nsim = 1, seed = 6, coef = sim_two_process_parameters(js))
  )

  # Five observed calls and three observed emails.
  expect_equal(nrow(out$events), 8)
  expect_identical(out$diagnostics$stop_reason, "target")
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
