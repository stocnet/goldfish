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
stamp_tie_spec <- function(calls, emails, constraint = ~ !tie(emails)) {
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
  make_joint_specification(
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
}

# The augmenter context over stamp_tie_spec(), completed as estimate_dynes()
# completes it.
stamp_tie_context <- function(...) {
  spec <- stamp_tie_spec(...)
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

# --- Configuration ------------------------------------------------------------

test_that("every legality walk opens with the likelihood's control_prep", {
  local_mocked_bindings(legal_pilot_attempts = function() 2L)
  opened <- list()
  open <- walk_open
  scored <- NULL
  local_mocked_bindings(
    walk_open = function(spec, control_preprocessing, ...) {
      opened[[length(opened) + 1L]] <<- control_preprocessing
      open(spec, control_preprocessing, ...)
    },
    # The control every candidate's log-likelihood is scored with, which
    # estimate_dynes() hands the warm start and the estimation loop alike.
    dynes_initial_theta = function(
      control,
      spec,
      labels,
      decomposition,
      augmenter,
      control_prep,
      call
    ) {
      scored <<- control_prep
      rlang::abort("scored", class = "legality_control_seen")
    }
  )
  spec <- stamp_tie_spec(
    calls = data.frame(from = c(2L, 4L), to = c(3L, 1L), time = c(2, 7)),
    emails = data.frame(from = 3L, to = 1L, time = 6)
  )
  expect_error(
    suppressWarnings(estimate_dynes(
      spec,
      set_algorithm_em(
        seed = 1L,
        augmenter = set_augmenter_options(routine = "random")
      ),
      control_prep = set_preprocessing(start_time = 0)
    )),
    class = "legality_control_seen"
  )
  expect_identical(scored$start_time, 0)
  expect_identical(scored$end_time, 10)
  expect_gt(length(opened), 0L)
  expect_identical(unique(opened), list(scored))
})

# --- The fused candidate pass -------------------------------------------------

# The proposal evaluator's one pass over a candidate (`legality =`) against
# the separate legality checker and rate-query walk it replaces: interval 1 of
# `context`'s recipe at `candidate`, later intervals at the checker's
# placeholders, with `queries` as `list(after, layer, flavor, sender,
# receiver)`. Returns the fused answer, the separate ones, and how many
# log-likelihoods the fused call scored.
fused_against_separate <- function(context, candidate, queries) {
  recipe <- context$recipe
  spec <- context$spec
  ids <- recipe$events$event[recipe$events$interval == 1L]
  times <- legality_placeholder_times(recipe)
  times[ids] <- candidate
  data <- emit_augmented_data(context, times)
  layout <- coef_layout(spec)
  theta <- 0.1 * seq_len(sum(!layout$fixed))
  evaluator <- make_proposal_evaluator(
    decompose_joint_spec(spec),
    theta,
    spec = spec
  )
  scored <- 0L
  score <- evaluate_candidate
  local_mocked_bindings(
    evaluate_candidate = function(...) {
      scored <<- scored + 1L
      score(...)
    }
  )
  fused <- suppressWarnings(evaluator(
    data,
    rate_queries = queries,
    legality = list(interval = 1L, waves = recipe$wave_times)
  ))
  list(
    fused = fused,
    scored = scored,
    legal = new_legality_checker(context)$legal(times, 1L, candidate),
    rates = query_rate_at_position(
      bind_candidate_data(spec, data),
      flat_to_fid_theta(spec, theta),
      queries
    )
  )
}

calls_query <- function(after, sender, receiver) {
  list(
    after = after,
    layer = "calls",
    flavor = NA_character_,
    sender = sender,
    receiver = receiver
  )
}

# The first interval's dissolution (at 1) before its creation (at 3), as
# legal_first_interval() places them.
dissolution_first <- function(context) {
  events <- context$recipe$events
  ids <- events$event[events$interval == 1L]
  ifelse(events$flavor[ids] == "dissolution", 1, 3)
}

test_that("the fused pass agrees with the separate walks off any tie", {
  context <- stamp_tie_context(
    calls = data.frame(from = c(2L, 4L), to = c(3L, 1L), time = c(2, 7)),
    emails = data.frame(from = 3L, to = 1L, time = 6)
  )
  queries <- list(
    calls_query(0L, "N2", "N3"),
    calls_query(1L, "N1", "N3"),
    list(
      after = 2L,
      layer = "friendship",
      flavor = "creation",
      sender = "N1",
      receiver = "N3"
    ),
    calls_query(3L, "N4", "N1")
  )
  answer <- fused_against_separate(context, dissolution_first(context), queries)
  expect_true(answer$legal)
  expect_true(answer$fused$legal)
  expect_identical(answer$fused$rates, answer$rates)
  expect_type(answer$fused$loglik, "double")
  expect_identical(answer$scored, 1L)
})

test_that("the fused pass agrees with the separate walks at a stamp tie", {
  # The email 2 -> 3 shares the call's stamp: legal, read strictly before it.
  context <- stamp_tie_context(
    calls = data.frame(from = c(2L, 4L), to = c(3L, 1L), time = c(2, 7)),
    emails = data.frame(from = 2L, to = 3L, time = 2)
  )
  queries <- list(calls_query(1L, "N2", "N3"), calls_query(2L, "N2", "N3"))
  answer <- fused_against_separate(context, dissolution_first(context), queries)
  expect_true(answer$legal)
  expect_true(answer$fused$legal)
  expect_identical(answer$fused$rates, answer$rates)
})

test_that("the fused pass reads tied events as two positions", {
  context <- stamp_tie_context(
    calls = data.frame(
      from = c(2L, 2L, 4L),
      to = c(3L, 4L, 1L),
      time = c(2, 2, 7)
    ),
    emails = data.frame(from = 3L, to = 1L, time = 6),
    constraint = ~ outdeg(calls, type = "ego") < 1
  )
  queries <- list(
    calls_query(3L, "N1", "N3"),
    calls_query(2L, "N1", "N3"),
    calls_query(4L, "N1", "N3")
  )
  answer <- fused_against_separate(context, dissolution_first(context), queries)
  expect_true(answer$legal)
  expect_true(answer$fused$legal)
  expect_identical(answer$fused$rates, answer$rates)
})

test_that("the fused pass stops at the first illegal stamp with no rates", {
  untied <- stamp_tie_context(
    calls = data.frame(from = c(2L, 4L), to = c(3L, 1L), time = c(2, 7)),
    emails = data.frame(from = 2L, to = 3L, time = 1.5)
  )
  tied <- stamp_tie_context(
    calls = data.frame(
      from = c(2L, 2L, 4L),
      to = c(3L, 4L, 1L),
      time = c(2, 2.5, 7)
    ),
    emails = data.frame(from = 3L, to = 1L, time = 6),
    constraint = ~ outdeg(calls, type = "ego") < 1
  )
  for (context in list(untied, tied)) {
    queries <- list(calls_query(0L, "N2", "N3"), calls_query(3L, "N4", "N1"))
    answer <- fused_against_separate(
      context,
      dissolution_first(context),
      queries
    )
    expect_false(answer$legal)
    expect_false(answer$fused$legal)
    expect_null(answer$fused$rates)
    expect_identical(answer$fused$loglik, NA_real_)
    expect_identical(answer$scored, 0L)
  }
})
