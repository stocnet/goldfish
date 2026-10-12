# The support-legality replay augment_seq_random() and augment_seq_mcmc()
# gate their candidates with: its cost per check, and that it reads each
# event's support at the state the likelihood scores it at.

# A flavored friendship panel (waves 5 and 10, observation start 0) modeled
# beside an observed calls stream and an emails stream. `n_actors` actors;
# `n_calls` calls and as many emails spread over the first interval, the
# emails on dyads no call uses, so the calls constraint (no call along an
# email tie) holds on the observed data.
legality_cost_context <- function(n_actors, n_calls = 80L) {
  friendship <- data.frame(
    from = c(8L, 9L, 1:6, 8L, 9L),
    to = c(9L, 10L, 2:7, 9L, 10L),
    time = c(NA, NA, rep(5, 6L), 5, 5),
    weight = c(1, 1, rep(1, 6L), -1, -1),
    layer = "friendship"
  )
  senders <- rep_len(seq_len(n_actors), n_calls)
  calls <- data.frame(
    from = senders,
    to = senders %% n_actors + 1L,
    time = seq(0.1, 4.9, length.out = n_calls),
    weight = 1,
    layer = "calls"
  )
  emails <- data.frame(
    from = senders,
    to = (senders + 2L) %% n_actors + 1L,
    time = seq(0.13, 4.93, length.out = n_calls),
    weight = 1,
    layer = "emails"
  )
  data <- list(
    info = list(
      name = "legality_cost",
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
      label = paste0("N", seq_len(n_actors)),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(friendship, calls, emails)
  )
  data <- add_flavor(
    data,
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
  spec <- make_joint_specification(
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
      support_constraint = ~ !tie(emails)
    ),
    data = data
  )
  waves <- panel_wave_grid(spec, start_time = 0)
  # Pinned panel rates warn on completion.
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  new_augmenter_context(
    completed,
    compile_augmentation_recipe(completed, waves)
  )
}

# The first interval's latent events at distinct times off every observed
# stamp, the later intervals left to the checker's placeholders.
first_interval_candidate <- function(context) {
  events <- context$recipe$events
  ids <- events$event[events$interval == 1L]
  list(
    times = rep(NA_real_, nrow(events)),
    candidate = seq(0.21, 4.81, length.out = length(ids))
  )
}

# Four actors: a flavored friendship panel (waves 5 and 10, observation start
# 0) in which N1 swaps 1 -> 2 for 1 -> 3 by wave 5 and 3 -> 4 appears by wave
# 10, modeled beside an observed calls stream and an emails stream. The calls
# constraint is `constraint`; by default no call goes along an email tie.
stamp_tie_context <- function(calls, emails, constraint = ~ !tie(emails)) {
  friendship <- data.frame(
    from = c(1L, 2L, 1L, 1L, 3L),
    to = c(2L, 4L, 2L, 3L, 4L),
    time = c(NA, NA, 5, 5, 10),
    weight = c(1, 1, -1, 1, 1),
    layer = "friendship"
  )
  calls$weight <- 1
  calls$layer <- "calls"
  emails$weight <- 1
  emails$layer <- "emails"
  data <- list(
    info = list(
      name = "stamp_tie",
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
      label = paste0("N", 1:4),
      mode = "p",
      stringsAsFactors = FALSE
    ),
    ties = rbind(friendship, calls, emails)
  )
  data <- add_flavor(
    data,
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
  spec <- make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data,
      support_constraint = constraint
    ),
    data = data
  )
  waves <- panel_wave_grid(spec, start_time = 0)
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  new_augmenter_context(
    completed,
    compile_augmentation_recipe(completed, waves)
  )
}

# The first interval's dissolution before its creation, both off every
# observed stamp: legal on its own.
legal_first_interval <- function(checker, context) {
  events <- context$recipe$events
  ids <- events$event[events$interval == 1L]
  times <- rep(NA_real_, nrow(events))
  candidate <- ifelse(events$flavor[ids] == "dissolution", 1, 3)
  checker$legal(times, 1L, candidate)
}

# --- Cost ---------------------------------------------------------------------

test_that("a legality check opens one walk", {
  context <- legality_cost_context(12L, n_calls = 20L)
  checker <- new_legality_checker(context)
  drawn <- first_interval_candidate(context)
  opens <- 0L
  open <- walk_open
  local_mocked_bindings(
    walk_open = function(...) {
      opens <<- opens + 1L
      open(...)
    }
  )
  for (check in 1:3) {
    expect_true(checker$legal(drawn$times, 1L, drawn$candidate))
  }
  expect_identical(opens, 3L)
})

# The replay's own allocation per replayed event: the slope of a check's
# allocation over the number of replayed calls, at a fixed actor count. The
# walk's steps are mocked out, since their cost is the walk's budget
# (test-walk_cost_budget.R), not the replay's; so is anything paid once per
# check (emitting the candidate, opening its walk, the support's first read),
# which the slope cancels. A per-event buffer of n1, n2 or n1 x n2 entries
# would make the slope grow with the actor count: one double per extra actor
# is 8 * 180 bytes per event between 60 and 240 actors.
test_that("the legality replay allocates nothing per event that grows with n", {
  skip_on_cran()
  skip_if_not_installed("bench")
  check_bytes <- function(n_actors, n_calls) {
    context <- legality_cost_context(n_actors, n_calls)
    checker <- new_legality_checker(context)
    drawn <- first_interval_candidate(context)
    expect_true(checker$legal(drawn$times, 1L, drawn$candidate))
    local_mocked_bindings(
      walk_advance = function(handle, ...) invisible(handle),
      walk_inject = function(handle, ...) invisible(handle)
    )
    memory <- bench::bench_memory(
      checker$legal(drawn$times, 1L, drawn$candidate)
    )
    as.numeric(memory$mem_alloc)
  }
  slope <- function(n_actors) {
    (check_bytes(n_actors, 160L) - check_bytes(n_actors, 40L)) / 120
  }
  expect_lt(slope(240L) - slope(60L), 8 * (240 - 60) / 4)
})

# --- Stamp semantics ----------------------------------------------------------

test_that("an exogenous row sharing an event's stamp is not applied first", {
  # The email 2 -> 3 at t = 2 would exclude the call 2 -> 3 stamped with it.
  # The likelihood reads the call strictly before t = 2, where it is
  # supported.
  context <- stamp_tie_context(
    calls = data.frame(from = c(2L, 4L), to = c(3L, 1L), time = c(2, 7)),
    emails = data.frame(from = 2L, to = 3L, time = 2)
  )
  checker <- new_legality_checker(context)
  expect_true(legal_first_interval(checker, context))
})

test_that("an exogenous row before an event's stamp still excludes it", {
  context <- stamp_tie_context(
    calls = data.frame(from = c(2L, 4L), to = c(3L, 1L), time = c(2, 7)),
    emails = data.frame(from = 2L, to = 3L, time = 1.5)
  )
  checker <- new_legality_checker(context)
  expect_false(legal_first_interval(checker, context))
})

test_that("tied events are checked against the same state", {
  # Each call by N2 ends its turn to call: injecting 2 -> 3 would exclude
  # 2 -> 4 stamped with it, which the likelihood reads strictly before t = 2.
  context <- stamp_tie_context(
    calls = data.frame(
      from = c(2L, 2L, 4L),
      to = c(3L, 4L, 1L),
      time = c(2, 2, 7)
    ),
    emails = data.frame(from = 3L, to = 1L, time = 6),
    constraint = ~ outdeg(calls, type = "ego") < 1
  )
  checker <- new_legality_checker(context)
  expect_true(legal_first_interval(checker, context))
})

test_that("an event after another's stamp still reads it", {
  context <- stamp_tie_context(
    calls = data.frame(
      from = c(2L, 2L, 4L),
      to = c(3L, 4L, 1L),
      time = c(2, 2.5, 7)
    ),
    emails = data.frame(from = 3L, to = 1L, time = 6),
    constraint = ~ outdeg(calls, type = "ego") < 1
  )
  checker <- new_legality_checker(context)
  expect_false(legal_first_interval(checker, context))
})
