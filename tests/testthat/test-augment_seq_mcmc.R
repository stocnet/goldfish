# augment_seq_mcmc(): one serial chain over the whole sequence, moved by
# within-interval permute and shift proposals whose times are redrawn from
# truncated exponentials at the moved pairs' rates, frozen at the state after
# the preceding panel event of the state the move starts from. Forward rates
# are read on the current sequence, reverse rates on the candidate, both
# through the injected proposal evaluator. Densities are checked against
# hand-computed windows and rates, the acceptance logic against a stub target,
# and the reverse rates against the walk on a real completed specification.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# The completed specification, its recipe on the observation-start grid and
# the context every augmenter builds, as estimate_dynes() builds them.
mcmc_build <- function(spec) {
  waves <- panel_wave_grid(spec, start_time = 0)
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  recipe <- compile_augmentation_recipe(completed, waves)
  list(
    spec = completed,
    recipe = recipe,
    waves = waves,
    context = new_augmenter_context(completed, recipe)
  )
}

mcmc_control <- function(burn_in = 0L, thinning = 1L, ...) {
  set_algorithm_em(
    augmenter = set_augmenter_options(
      routine = "mcmc",
      burn_in = burn_in,
      thinning = thinning,
      ...
    ),
    seed = 7L
  )
}

# A stub proposal evaluator: `score(data)` is the candidate's log-likelihood,
# `rate(query)` a query's intensity and `legal(data, legality)` its legality
# when the chain asks. Every call is logged; an illegal one as unscored.
stub_evaluator <- function(
  score = function(data) 0,
  rate = stub_rate,
  legal = function(data, legality) TRUE
) {
  log <- new.env(parent = emptyenv())
  log$calls <- list()
  evaluator <- function(
    candidate_seq,
    rate_queries = NULL,
    loglik = TRUE,
    legality = NULL
  ) {
    if (!is.null(legality) && !legal(candidate_seq, legality)) {
      log$calls[[length(log$calls) + 1L]] <- list(
        data = candidate_seq,
        queries = rate_queries,
        loglik = FALSE
      )
      return(list(legal = FALSE, rates = NULL, loglik = NA_real_))
    }
    log$calls[[length(log$calls) + 1L]] <- list(
      data = candidate_seq,
      queries = rate_queries,
      loglik = loglik
    )
    list(
      loglik = if (loglik) score(candidate_seq) else NA_real_,
      rates = if (!is.null(rate_queries)) {
        vapply(rate_queries, rate, numeric(1))
      }
    )
  }
  list(evaluator = evaluator, log = log)
}

# A stub's legality answered by the support-legality replay over the
# candidate's interval, as the real evaluator decides it.
replay_legality <- function(context) {
  checker <- new_legality_checker(context)
  events <- context$recipe$events
  function(data, legality) {
    times <- mcmc_times_from_data(context, data)
    k <- legality$interval
    checker$legal(times, k, times[events$event[events$interval == k]])
  }
}

# A rate that encodes both the pair's sender and the queried position, so a
# wrong pair or a wrong ordinal changes the density.
stub_rate <- function(query) {
  0.1 * as.integer(sub("N", "", query$sender)) + 0.01 * query$after
}

stub_loglik_calls <- function(stub) {
  Filter(function(call) isTRUE(call$loglik), stub$log$calls)
}

latent_times <- function(data, layer, waves) {
  ties <- as.data.frame(data$ties)
  ties[
    ties$layer == layer &
      !is.na(ties$time) &
      ties$time > waves[[1L]] &
      ties$time < waves[[length(waves)]],
    ,
    drop = FALSE
  ]
}

log_te <- function(t, rate, lower, upper) {
  log(rate) - rate * (t - lower) - log(-expm1(-rate * (upper - lower)))
}

# --- Fixtures -----------------------------------------------------------------

# A count panel `calls` over one interval (0, 10), four actors: 1 -> 2 rises
# by two (a two-step chain), 2 -> 3 and 3 -> 4 by one. An emails stream is
# observed after the wave, so no observed modeled event falls inside it.
count_data <- function() {
  list(
    info = list(
      name = "count",
      focal = "emails",
      update = c(calls = "increment", emails = "increment"),
      directed = c(calls = TRUE, emails = TRUE),
      observation = c(calls = "panel", emails = "event")
    ),
    nodes = data.frame(
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = data.frame(
      from = c(1L, 2L, 3L, 2L),
      to = c(2L, 3L, 4L, 1L),
      time = c(10, 10, 10, 12),
      weight = c(2, 1, 1, 1),
      layer = c("calls", "calls", "calls", "emails")
    )
  )
}

count_spec <- function() {
  data <- count_data()
  make_joint_specification(
    make_specification(
      rate = ~1,
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~1,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# Recipe event ids of the count fixture: the chain 1 -> 2 (two steps) and the
# single events 2 -> 3 and 3 -> 4.
count_events <- function(recipe) {
  events <- recipe$events
  a <- events$event[events$from == 1L & events$to == 2L]
  list(
    a1 = a[[1L]],
    a2 = a[[2L]],
    b = events$event[events$from == 2L & events$to == 3L],
    c = events$event[events$from == 3L & events$to == 4L]
  )
}

count_times <- function(recipe, a1 = 1, a2 = 4, b = 6, c = 8) {
  ids <- count_events(recipe)
  times <- numeric(nrow(recipe$events))
  times[c(ids$a1, ids$a2, ids$b, ids$c)] <- c(a1, a2, b, c)
  times
}

# A flavored friendship panel (waves 5 and 10, observation start 0) whose
# choices both vary, beside an observed calls stream at 2 and 7. N1 creates
# 1 -> 2 and N2 dissolves 2 -> 1 by wave 5; N1 dissolves 1 -> 2 by wave 10.
# Both friendship rates are completed and pinned per period. N1 joins the
# dissolution risk set when it creates 1 -> 2, so moving that creation moves
# the pinned dissolution compensator.
flavor_data <- function() {
  friendship <- data.frame(
    from = c(2L, 2L, 4L, 1L, 2L, 1L),
    to = c(1L, 3L, 1L, 2L, 1L, 2L),
    time = c(NA, NA, NA, 5, 5, 10),
    weight = c(1, 1, 1, 1, -1, -1),
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
      name = "flavor",
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

# theta: creation indeg, dissolution indeg, calls intercept, calls outdeg.
flavor_spec <- function(data = flavor_data()) {
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + outdeg,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

flavor_event <- function(recipe, from, to, interval) {
  events <- recipe$events
  events$event[
    events$from == from & events$to == to & events$interval == interval
  ]
}

# A max-outdegree-1 coupling: N1 swaps 1 -> 2 for 1 -> 3 by wave 5, and may
# create only while it holds no tie, so the first interval is legal exactly
# when the dissolution comes first. 3 -> 4 is created by wave 10.
legal_data <- function() {
  friendship <- data.frame(
    from = c(1L, 2L, 1L, 1L, 3L),
    to = c(2L, 4L, 2L, 3L, 4L),
    time = c(NA, NA, 5, 5, 10),
    weight = c(1, 1, -1, 1, 1),
    layer = "friendship"
  )
  calls <- data.frame(
    from = c(2L, 4L),
    to = c(3L, 1L),
    time = c(2, 7),
    weight = 1,
    layer = "calls"
  )
  data <- list(
    info = list(
      name = "legal",
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

legal_spec <- function() {
  data <- legal_data()
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data,
      support_constraint = ~ tie(friendship) |
        outdeg(friendship, type = "ego") < 1
    ),
    make_specification(
      rate = ~ 1 + outdeg,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# A chain over `built` at `times`, scored by `evaluator`.
started_chain <- function(built, times, evaluator, control = mcmc_control()) {
  chain <- new_mcmc_chain(built$context, control)
  mcmc_start(chain, times)
  mcmc_attach(chain, evaluator)
  chain
}

# --- Contract -----------------------------------------------------------------

test_that("the mcmc routine is registered and builds an augmenter", {
  expect_identical(dynes_augmenter_constructors()$mcmc, augment_seq_mcmc)
  built <- mcmc_build(count_spec())
  augmenter <- augment_seq_mcmc(built$spec, built$recipe, mcmc_control())
  expect_s3_class(augmenter, "goldfishAugmenter")
  expect_identical(augmenter$routine, "mcmc")
  expect_true(is.function(augmenter$init))
  expect_true(is.function(augmenter$diagnostics))
})

test_that("unset burn-in and thinning take the routine's defaults", {
  built <- mcmc_build(count_spec())
  control <- set_algorithm_em(
    augmenter = set_augmenter_options(routine = "mcmc")
  )
  chain <- new_mcmc_chain(built$context, control)
  expect_identical(chain$burn_in, 10L)
  expect_identical(chain$thinning, 1L)
})

# --- Move set -----------------------------------------------------------------

test_that("permute never pairs two steps of one chain", {
  built <- mcmc_build(count_spec())
  ids <- count_events(built$recipe)
  catalog <- mcmc_move_catalog(built$recipe)
  pairs <- catalog$pairs
  same_chain <- built$recipe$events$chain[pairs$first] ==
    built$recipe$events$chain[pairs$second]
  expect_false(any(same_chain))
  # Every other same-interval pair is there: C(4, 2) - C(2, 2) = 5, a count
  # that does not depend on the current order.
  expect_identical(nrow(pairs), 5L)
  expect_false(any(
    pairs$first == ids$a1 & pairs$second == ids$a2
  ))
  expect_identical(catalog$n_events, 4L)
})

test_that("a cross-dyad support-illegal swap stays in the move set", {
  built <- mcmc_build(legal_spec())
  pairs <- mcmc_move_catalog(built$recipe)$pairs
  create <- flavor_event(built$recipe, 1L, 3L, 1L)
  dissolve <- flavor_event(built$recipe, 1L, 2L, 1L)
  expect_true(any(
    (pairs$first == dissolve & pairs$second == create) |
      (pairs$first == create & pairs$second == dissolve)
  ))
})

# --- Proposal densities -------------------------------------------------------

test_that("a non-adjacent permute redraws both times in the unified windows", {
  built <- mcmc_build(count_spec())
  ids <- count_events(built$recipe)
  stub <- stub_evaluator()
  chain <- started_chain(built, count_times(built$recipe), stub$evaluator)
  current <- chain$data
  record <- withr::with_seed(
    3L,
    mcmc_step(chain, move = list(type = "permute", events = c(ids$a2, ids$c)))
  )
  # Slots a1 @ 1, a2 @ 4, b @ 6, c @ 8; the pair is slots 2 and 4. c moves
  # into slot 2 over (1, 6), at its rate after a1 (event 1); a2 moves into
  # slot 4 over (6, 10), at its rate after b (event 3).
  t_x <- record$proposed[[ids$c]]
  t_y <- record$proposed[[ids$a2]]
  expect_gt(t_x, 1)
  expect_lt(t_x, 6)
  expect_gt(t_y, 6)
  expect_lt(t_y, 10)
  expect_equal(
    record$forward$log_q,
    log_te(t_x, 0.31, 1, 6) + log_te(t_y, 0.13, 6, 10)
  )
  # Back from the candidate: a2 into slot 2 at 4 over (1, 6), c into slot 4
  # at 8 over (6, 10), each at its rate after the same preceding events there.
  expect_equal(
    record$reverse$log_q,
    log_te(4, 0.11, 1, 6) + log_te(8, 0.33, 6, 10)
  )

  forward <- stub$log$calls[[2L]]
  expect_false(forward$loglik)
  expect_identical(forward$data, current)
  expect_equal(
    forward$queries,
    list(
      list(
        after = 1L,
        layer = "calls",
        flavor = NA_character_,
        sender = "N3",
        receiver = "N4"
      ),
      list(
        after = 3L,
        layer = "calls",
        flavor = NA_character_,
        sender = "N1",
        receiver = "N2"
      )
    )
  )
  reverse <- stub$log$calls[[3L]]
  expect_true(reverse$loglik)
  expect_identical(reverse$data, record$data)
  expect_equal(
    vapply(reverse$queries, `[[`, integer(1), "after"),
    c(1L, 3L)
  )
  expect_identical(
    vapply(reverse$queries, `[[`, character(1), "sender"),
    c("N1", "N3")
  )
})

test_that("an adjacent permute bounds the second draw at the first", {
  built <- mcmc_build(count_spec())
  ids <- count_events(built$recipe)
  chain <- started_chain(
    built,
    count_times(built$recipe),
    stub_evaluator()$evaluator
  )
  record <- withr::with_seed(
    5L,
    mcmc_step(chain, move = list(type = "permute", events = c(ids$b, ids$c)))
  )
  # Slots 3 and 4: c into slot 3 over (4, 10) after a2 (event 2), then b
  # into slot 4 over (t_c, 10), also after a2.
  t_x <- record$proposed[[ids$c]]
  t_y <- record$proposed[[ids$b]]
  expect_gt(t_x, 4)
  expect_gt(t_y, t_x)
  expect_lt(t_y, 10)
  expect_equal(
    record$forward$log_q,
    log_te(t_x, 0.32, 4, 10) + log_te(t_y, 0.22, t_x, 10)
  )
  expect_equal(
    record$reverse$log_q,
    log_te(6, 0.22, 4, 10) + log_te(8, 0.32, 6, 10)
  )
})

test_that("a permute keeps same-dyad chain order by relabeling the steps", {
  built <- mcmc_build(count_spec())
  ids <- count_events(built$recipe)
  chain <- started_chain(
    built,
    count_times(built$recipe),
    stub_evaluator()$evaluator
  )
  record <- withr::with_seed(
    9L,
    mcmc_step(chain, move = list(type = "permute", events = c(ids$a1, ids$b)))
  )
  # b takes slot 1 over (0, 4) at its rate at the start; the 1 -> 2 step it
  # displaces takes slot 3 over (4, 8) at its rate after the 1 -> 2 step at 4.
  # The chain's steps are then its times in order: 4, then the new time.
  proposed <- record$proposed
  expect_lt(proposed[[ids$b]], 4)
  expect_equal(proposed[[ids$a1]], 4)
  expect_gt(proposed[[ids$a2]], 4)
  expect_lt(proposed[[ids$a2]], 8)
  t_x <- proposed[[ids$b]]
  t_y <- proposed[[ids$a2]]
  expect_equal(
    record$forward$log_q,
    log_te(t_x, 0.2, 0, 4) + log_te(t_y, 0.12, 4, 8)
  )
  expect_equal(
    record$reverse$log_q,
    log_te(1, 0.1, 0, 4) + log_te(6, 0.22, 4, 8)
  )
})

test_that("a shift's density ratio is exp(rate * (new - old))", {
  built <- mcmc_build(count_spec())
  ids <- count_events(built$recipe)
  stub <- stub_evaluator()
  chain <- started_chain(built, count_times(built$recipe), stub$evaluator)
  record <- withr::with_seed(
    2L,
    mcmc_step(chain, move = list(type = "shift", event = ids$b))
  )
  # b redraws inside (4, 8), at its rate after a2 (event 2) on both sides.
  t_new <- record$proposed[[ids$b]]
  expect_gt(t_new, 4)
  expect_lt(t_new, 8)
  expect_equal(record$forward$log_q, log_te(t_new, 0.22, 4, 8))
  expect_equal(record$reverse$log_q, log_te(6, 0.22, 4, 8))
  expect_equal(
    record$reverse$log_q - record$forward$log_q,
    0.22 * (t_new - 6)
  )
  expect_equal(stub$log$calls[[3L]]$queries[[1L]]$after, 2L)
})

test_that("a single-event interval still moves under shift", {
  built <- mcmc_build(flavor_spec())
  lone <- flavor_event(built$recipe, 1L, 2L, 2L)
  times <- c(1, 3, 8)
  chain <- started_chain(built, times, stub_evaluator()$evaluator)
  expect_identical(nrow(built$recipe$pairs[[2L]]), 0L)
  moved <- vapply(
    1:10,
    function(k) {
      withr::with_seed(k, {
        mcmc_step(chain, move = list(type = "shift", event = lone))
      })$proposed[[lone]]
    },
    numeric(1)
  )
  expect_true(all(moved > 5 & moved < 10))
  expect_gt(length(unique(moved)), 1L)
})

# --- Acceptance ---------------------------------------------------------------

test_that("the acceptance ratio is f'/f times q_rev/q_fwd", {
  built <- mcmc_build(count_spec())
  score <- function(data) {
    -0.3 * sum(latent_times(data, "calls", built$waves)$time)
  }
  chain <- started_chain(
    built,
    count_times(built$recipe),
    stub_evaluator(score)$evaluator
  )
  seen <- c(accepted = FALSE, rejected = FALSE)
  withr::with_seed(4L, {
    for (k in 1:40) {
      before <- list(times = chain$times, loglik = chain$loglik)
      record <- mcmc_step(chain)
      expect_equal(
        record$log_alpha,
        score(record$data) -
          before$loglik +
          record$reverse$log_q -
          record$forward$log_q
      )
      # The chain's state, its data and its log-likelihood move together.
      if (record$accepted) {
        expect_equal(chain$times, record$proposed)
        seen[["accepted"]] <- TRUE
      } else {
        expect_equal(chain$times, before$times)
        seen[["rejected"]] <- TRUE
      }
      expect_equal(chain$loglik, score(chain$data))
      expect_identical(
        chain$data,
        emit_augmented_data(built$context, chain$times)
      )
    }
  })
  expect_true(all(seen))
})

test_that("the chain leaves the target invariant", {
  # A flat target makes the times uniform over the endpoint-hitting paths:
  # the four slots are the order statistics of four uniforms on (0, 10), and
  # 2 -> 3 precedes 3 -> 4 half the time. The stub's rates are far from flat,
  # so a chain that dropped q_rev / q_fwd would drift from both.
  built <- mcmc_build(count_spec())
  ids <- count_events(built$recipe)
  run <- function(score, n = 3000L) {
    chain <- started_chain(
      built,
      count_times(built$recipe),
      stub_evaluator(score)$evaluator
    )
    withr::with_seed(11L, {
      samples <- vapply(
        seq_len(n),
        function(k) {
          mcmc_step(chain)
          chain$times
        },
        numeric(4)
      )
    })
    samples[, -seq_len(200L), drop = FALSE]
  }

  flat <- run(function(data) 0)
  slots <- apply(flat, 2L, sort)
  expect_equal(rowMeans(slots), c(2, 4, 6, 8), tolerance = 0.08)
  expect_equal(mean(flat[ids$b, ] < flat[ids$c, ]), 0.5, tolerance = 0.12)

  # Tilted three to one towards 2 -> 3 first.
  tilted <- run(function(data) {
    ties <- latent_times(data, "calls", built$waves)
    b <- ties$time[ties$from == 2L & ties$to == 3L]
    c <- ties$time[ties$from == 3L & ties$to == 4L]
    if (b < c) log(3) else 0
  })
  expect_equal(mean(tilted[ids$b, ] < tilted[ids$c, ]), 0.75, tolerance = 0.1)
})

# --- Chain lifecycle ----------------------------------------------------------

test_that("burn-in runs at every chain start, thinning between draws", {
  built <- mcmc_build(count_spec())
  augmenter <- augment_seq_mcmc(
    built$spec,
    built$recipe,
    mcmc_control(burn_in = 2L, thinning = 1L)
  )
  first <- stub_evaluator()
  augmenter$init(first$evaluator, 0)
  withr::with_seed(1L, augmenter$draw(1L, 1:3, 0))
  diagnostics <- augmenter$diagnostics()
  # One sweep is as many proposals as panel events (4).
  expect_identical(diagnostics$burn_in_sweeps, 2L)
  expect_identical(diagnostics$sweeps, 5L)
  expect_identical(sum(diagnostics$proposals), 20L)

  # Within-iteration growth continues the same chain: no new burn-in.
  last <- withr::with_seed(2L, augmenter$draw(1L, 4L, 0))[[1L]]
  diagnostics <- augmenter$diagnostics()
  expect_identical(diagnostics$burn_in_sweeps, 2L)
  expect_identical(diagnostics$sweeps, 6L)

  # A new EM iteration warm-starts from the last draw under the new target,
  # and burns in again.
  second <- stub_evaluator(function(data) -1)
  augmenter$init(second$evaluator, 0)
  warm <- stub_loglik_calls(second)[[1L]]
  expect_identical(warm$data, last$data)
  withr::with_seed(3L, augmenter$draw(2L, 1L, 0))
  diagnostics <- augmenter$diagnostics()
  expect_identical(diagnostics$burn_in_sweeps, 4L)
  expect_identical(diagnostics$sweeps, 9L)
})

test_that("the chain is initialized once, not at every iteration", {
  built <- mcmc_build(count_spec())
  starts <- 0L
  initial <- mcmc_initial_times
  local_mocked_bindings(
    mcmc_initial_times = function(...) {
      starts <<- starts + 1L
      initial(...)
    }
  )
  augmenter <- augment_seq_mcmc(built$spec, built$recipe, mcmc_control())
  for (iteration in 1:3) {
    augmenter$init(stub_evaluator()$evaluator, 0)
    withr::with_seed(iteration, augmenter$draw(iteration, 1:2, 0))
  }
  expect_identical(starts, 1L)
})

test_that("a draw before any init returns the chain's start", {
  built <- mcmc_build(count_spec())
  augmenter <- augment_seq_mcmc(built$spec, built$recipe, mcmc_control())
  draw <- withr::with_seed(1L, augmenter$draw(0L, 1L, 0))[[1L]]
  expect_true(is.finite(draw$log_proposal))
  expect_false(draw$restarted)
  diff <- diff_panel_waves(built$spec$data, "calls", built$waves)
  expect_no_error(validate_endpoint_hitting(draw$data, diff))
  expect_identical(augmenter$diagnostics()$sweeps, 0L)
})

test_that("a user-supplied initial sequence starts the chain", {
  built <- mcmc_build(count_spec())
  start <- emit_augmented_data(built$context, count_times(built$recipe))
  augmenter <- augment_seq_mcmc(
    built$spec,
    built$recipe,
    mcmc_control(initial_sequence = start)
  )
  stub <- stub_evaluator()
  augmenter$init(stub$evaluator, 0)
  withr::with_seed(1L, augmenter$draw(1L, 1L, 0))
  expect_identical(stub$log$calls[[1L]]$data, start)
})

test_that("an initial sequence that misses the waves aborts", {
  local_cli_context()
  built <- mcmc_build(count_spec())
  start <- emit_augmented_data(built$context, count_times(built$recipe))
  # The 3 -> 4 rise moved onto the wave.
  ties <- start$ties
  ties$time[ties$layer == "calls" & ties$from == 3L & ties$to == 4L] <- 10
  start$ties <- ties
  expect_snapshot(
    augment_seq_mcmc(
      built$spec,
      built$recipe,
      mcmc_control(initial_sequence = start)
    ),
    error = TRUE
  )
  expect_error(
    augment_seq_mcmc(
      built$spec,
      built$recipe,
      mcmc_control(initial_sequence = start)
    ),
    class = "goldfish_dynes_invalid_initial_sequence"
  )
})

# --- Retained draws (accepted, not last) --------------------------------------

test_that("a retained draw carries the accepted state's log-likelihood", {
  built <- mcmc_build(count_spec())
  # A steep target rejects often, so many retention points follow a
  # rejected proposal.
  score <- function(data) -sum(latent_times(data, "calls", built$waves)$time)
  augmenter <- augment_seq_mcmc(
    built$spec,
    built$recipe,
    mcmc_control(burn_in = 1L, thinning = 1L)
  )
  augmenter$init(stub_evaluator(score)$evaluator, 0)
  draws <- withr::with_seed(8L, augmenter$draw(1L, 1:25, 0))
  for (draw in draws) {
    expect_equal(draw$log_proposal, score(draw$data))
    expect_equal(draw$loglik, draw$log_proposal)
    expect_false(draw$restarted)
  }
  diagnostics <- augmenter$diagnostics()
  expect_lt(sum(diagnostics$accepted), sum(diagnostics$proposals))
})

# --- Cross-dyad support legality ----------------------------------------------

test_that("a support-illegal permute is proposed and rejected with no redraw", {
  built <- mcmc_build(legal_spec())
  dissolve <- flavor_event(built$recipe, 1L, 2L, 1L)
  create <- flavor_event(built$recipe, 1L, 3L, 1L)
  later <- flavor_event(built$recipe, 3L, 4L, 2L)
  times <- numeric(3)
  times[c(dissolve, create, later)] <- c(1, 3, 8)
  start <- emit_augmented_data(built$context, times)
  augmenter <- augment_seq_mcmc(
    built$spec,
    built$recipe,
    mcmc_control(
      thinning = 1L,
      initial_sequence = start,
      move_probs = c(permute = 1, shift = 0)
    )
  )
  stub <- stub_evaluator(
    function(data) -2,
    legal = replay_legality(built$context)
  )
  augmenter$init(stub$evaluator, 0)
  draws <- withr::with_seed(1L, augmenter$draw(1L, 1:2, 0))

  # Every permute swaps the only same-interval pair, which puts the creation
  # first: each is proposed, found illegal and rejected, and the chain stays.
  diagnostics <- augmenter$diagnostics()
  expect_identical(diagnostics$proposals[["permute"]], 6L)
  expect_identical(diagnostics$rejected_illegal, 6L)
  expect_identical(diagnostics$accepted[["permute"]], 0L)
  for (draw in draws) {
    expect_identical(draw$data, start)
    # No normalizer: the density is the target's, as for any retained draw.
    expect_equal(draw$log_proposal, -2)
    expect_false(draw$restarted)
  }
  # No doomed candidate was scored: the only likelihood call is the start's.
  expect_length(stub_loglik_calls(stub), 1L)
  expect_identical(stub_loglik_calls(stub)[[1L]]$data, start)
})

test_that("the evaluator's one pass checks a flavored event under its flavor", {
  # A realization of the friendship layer carries no flavor, so a check read
  # off it would find no process to constrain and accept the creation first.
  built <- mcmc_build(legal_spec())
  dissolve <- flavor_event(built$recipe, 1L, 2L, 1L)
  create <- flavor_event(built$recipe, 1L, 3L, 1L)
  later <- flavor_event(built$recipe, 3L, 4L, 2L)
  evaluator <- make_proposal_evaluator(
    decompose_joint_spec(built$spec),
    c(0.2, 0.3, -1, 0.1),
    spec = built$spec,
    control_prep = set_preprocessing(start_time = 0, end_time = 10)
  )
  query <- list(
    after = 1L,
    layer = "friendship",
    flavor = "creation",
    sender = "N1",
    receiver = "N3"
  )
  pass <- function(dissolve_at, create_at) {
    times <- numeric(3)
    times[c(dissolve, create, later)] <- c(dissolve_at, create_at, 8)
    suppressWarnings(evaluator(
      emit_augmented_data(built$context, times),
      list(query),
      legality = list(interval = 1L, waves = built$waves)
    ))
  }

  creation_first <- pass(dissolve_at = 3, create_at = 1)
  expect_false(creation_first$legal)
  expect_null(creation_first$rates)
  expect_identical(creation_first$loglik, NA_real_)

  dissolution_first <- pass(dissolve_at = 1, create_at = 3)
  expect_true(dissolution_first$legal)
  expect_length(dissolution_first$rates, 1L)
  expect_true(is.finite(dissolution_first$loglik))
})

# --- Real proposal evaluator --------------------------------------------------

test_that("the rates-only evaluator call skips the likelihood", {
  built <- mcmc_build(flavor_spec())
  evaluator <- make_proposal_evaluator(
    decompose_joint_spec(built$spec),
    c(0.2, 0.3, -1, 0.1),
    spec = built$spec,
    control_prep = set_preprocessing(start_time = 0, end_time = 10)
  )
  data <- emit_augmented_data(built$context, c(1, 3, 8))
  query <- list(
    after = 0L,
    layer = "friendship",
    flavor = "creation",
    sender = "N1",
    receiver = "N2"
  )
  local_mocked_bindings(
    preprocess_pool_sequence = function(...) stop("preprocessed")
  )
  result <- evaluator(data, list(query), loglik = FALSE)
  expect_true(is.na(result$loglik))
  expect_length(result$rates, 1L)
  expect_gt(result$rates, 0)
})

test_that("q_rev reads the walk rates at the candidate's preceding event", {
  built <- mcmc_build(flavor_spec())
  theta <- c(0.2, 0.3, -1, 0.1)
  control_prep <- set_preprocessing(start_time = 0, end_time = 10)
  evaluator <- make_proposal_evaluator(
    decompose_joint_spec(built$spec),
    theta,
    spec = built$spec,
    control_prep = control_prep
  )
  create <- flavor_event(built$recipe, 1L, 2L, 1L)
  dissolve <- flavor_event(built$recipe, 2L, 1L, 1L)
  lone <- flavor_event(built$recipe, 1L, 2L, 2L)
  times <- numeric(3)
  times[c(create, dissolve, lone)] <- c(1, 3, 8)
  chain <- suppressWarnings(started_chain(built, times, evaluator))
  by_fid <- flat_to_fid_theta(built$spec, theta)
  walk_rate <- function(data, after, flavor, sender, receiver) {
    query_rate_at_position(
      bind_candidate_data(built$spec, data),
      by_fid,
      list(list(
        after = after,
        layer = "friendship",
        flavor = flavor,
        sender = sender,
        receiver = receiver
      )),
      control_prep = control_prep
    )
  }

  # Permute the first interval's pair (adjacent, both after the wave start,
  # event 0): back from the candidate, the creation returns to 1 over (0, 5)
  # and the dissolution to 3 over (1, 5).
  permute <- suppressWarnings(withr::with_seed(
    1L,
    mcmc_step(
      chain,
      move = list(type = "permute", events = c(create, dissolve))
    )
  ))
  candidate <- permute$data
  expect_equal(
    permute$reverse$log_q,
    log_te(1, walk_rate(candidate, 0L, "creation", "N1", "N2"), 0, 5) +
      log_te(3, walk_rate(candidate, 0L, "dissolution", "N2", "N1"), 1, 5)
  )
  expect_equal(
    permute$reverse$rates,
    c(
      walk_rate(candidate, 0L, "creation", "N1", "N2"),
      walk_rate(candidate, 0L, "dissolution", "N2", "N1")
    )
  )

  # Shift the second interval's lone dissolution: its window opens at wave 5,
  # after the call at 2 and both first-interval flips, the candidate's third
  # event.
  shift <- suppressWarnings(withr::with_seed(
    2L,
    mcmc_step(chain, move = list(type = "shift", event = lone))
  ))
  rate <- walk_rate(shift$data, 3L, "dissolution", "N1", "N2")
  expect_equal(shift$reverse$rates, rate)
  expect_equal(
    shift$reverse$log_q,
    log_te(8, rate, 5, 10)
  )
})

test_that("a pinned rate's compensator enters f'/f", {
  built <- mcmc_build(flavor_spec())
  evaluator <- make_proposal_evaluator(
    decompose_joint_spec(built$spec),
    c(0.2, 0.3, -1, 0.1),
    spec = built$spec,
    control_prep = set_preprocessing(start_time = 0, end_time = 10)
  )
  create <- flavor_event(built$recipe, 1L, 2L, 1L)
  dissolve <- flavor_event(built$recipe, 2L, 1L, 1L)
  lone <- flavor_event(built$recipe, 1L, 2L, 2L)
  at <- function(t_create) {
    times <- numeric(3)
    times[c(create, dissolve, lone)] <- c(t_create, 3, 8)
    suppressWarnings(evaluator(emit_augmented_data(built$context, times)))
  }
  # Moving the creation from 1 to 1.5 keeps every event's order, so only the
  # exposures move: N1 enters the pinned dissolution risk set half a unit
  # later. The creation risk set holds every actor throughout, and the calls
  # rate is constant, so neither moves.
  map <- built$spec$process_map
  dissolution_rate <- map$fid[
    map$flavor %in% "dissolution" & map$family == "rate"
  ]
  plateau <- built$spec$completed_rates[[as.character(dissolution_rate)]]
  expect_equal(
    at(1.5)$loglik - at(1)$loglik,
    exp(plateau$intercept[[1L]]) * 0.5
  )
})
