# augment_seq_random(): iid-uniform times per interval, same-dyad chains
# sorted, a closed-form proposal density, and a support-legality
# reject-and-redraw under cross-dyad coupling whose normalizer is a theta-free
# constant.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A flavored friendship panel (waves 5 and 10, observation start 0) beside an
# observed calls stream, four actors. N1 holds 1 -> 2 at the start and swaps
# it for 1 -> 3 by wave 5; 3 -> 4 is created by wave 10. Under `coupled`
# (a sender may create only while it has no tie), the first interval is legal
# exactly when the dissolution comes first: P(legal) = 1/2. The second
# interval is always legal.
coupled_data <- function() {
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

coupled_spec <- function(data = coupled_data(), coupled = TRUE) {
  constraint <- if (coupled) {
    ~ tie(friendship) | outdeg(friendship, type = "ego") < 1
  }
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data,
      support_constraint = constraint
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~ tie(friendship),
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# An unflavored count panel (calls, cumulative counts at waves 5 and 10)
# beside an observed emails stream: (1, 2) rises 0 -> 2 -> 5 and (2, 3)
# 0 -> 0 -> 1, so chains of 2, then 3 and 1.
count_spec <- function() {
  data <- list(
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
    ties = rbind(
      data.frame(
        from = c(1L, 1L, 2L),
        to = c(2L, 2L, 3L),
        time = c(5, 10, 10),
        weight = c(2, 3, 1),
        layer = "calls"
      ),
      data.frame(
        from = c(1L, 3L),
        to = c(3L, 4L),
        time = c(2, 7),
        weight = 1,
        layer = "emails"
      )
    )
  )
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
      choice = ~inertia,
      layer = "emails",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# The completed specification, its recipe on the observation-start grid, and
# the random augmenter over them, built as estimate_dynes() builds it.
random_augmenter <- function(spec, seed = 11L) {
  waves <- panel_wave_grid(spec, start_time = 0)
  # Pinned panel rates warn on completion.
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  recipe <- compile_augmentation_recipe(completed, waves)
  control <- set_algorithm_em(
    augmenter = set_augmenter_options(routine = "random"),
    seed = seed
  )
  list(
    spec = completed,
    recipe = recipe,
    augmenter = augment_seq_random(completed, recipe, control)
  )
}

# A shorter legality pilot: each attempt replays the sequence on a new walk.
local_short_pilot <- function(env = parent.frame()) {
  local_mocked_bindings(legal_pilot_attempts = function() 40L, .env = env)
}

draw_random <- function(built, n, seed = 1L) {
  withr::with_seed(seed, {
    lapply(seq_len(n), function(k) built$augmenter$draw(1L, k, NULL)[[1L]])
  })
}

layer_rows <- function(draw, layer) {
  ties <- as.data.frame(draw$data$ties)
  ties[ties$layer == layer & !is.na(ties$time), , drop = FALSE]
}

# --- Contract --------------------------------------------------------------

test_that("the random routine is registered and builds an augmenter", {
  expect_identical(dynes_augmenter_constructors()$random, augment_seq_random)
  built <- random_augmenter(coupled_spec(coupled = FALSE))
  expect_s3_class(built$augmenter, "goldfishAugmenter")
  expect_identical(built$augmenter$routine, "random")

  control <- set_algorithm_em(
    augmenter = set_augmenter_options(routine = "random")
  )
  resolved <- resolve_dynes_augmenter(built$spec, built$recipe, control)
  expect_s3_class(resolved, "goldfishAugmenter")
})

test_that("each draw returns one sequence per draw index", {
  built <- random_augmenter(coupled_spec(coupled = FALSE))
  draws <- withr::with_seed(1L, built$augmenter$draw(1L, 3:4, NULL))
  expect_length(draws, 2L)
  expect_named(draws[[1L]], c("data", "log_proposal", "restarted"))
})

# --- Coupling-free draws -----------------------------------------------------

test_that("draws hit every wave with times strictly inside their interval", {
  built <- random_augmenter(coupled_spec(coupled = FALSE))
  diff <- diff_panel_waves(built$spec$data, "friendship", c(0, 5, 10))
  for (draw in draw_random(built, 10L)) {
    expect_no_error(validate_endpoint_hitting(draw$data, diff))
    rows <- layer_rows(draw, "friendship")
    rows <- rows[rows$time > 0, , drop = FALSE]
    expect_false(any(rows$time %in% c(0, 5, 10)))
  }
})

test_that("orderings vary across draws", {
  built <- random_augmenter(coupled_spec(coupled = FALSE))
  first <- vapply(
    draw_random(built, 20L),
    function(draw) {
      rows <- layer_rows(draw, "friendship")
      rows <- rows[rows$time > 0 & rows$time < 5, , drop = FALSE]
      rows$flavor[which.min(rows$time)]
    },
    character(1)
  )
  expect_setequal(first, c("creation", "dissolution"))
})

test_that("the proposal density is the closed form, the same for every draw", {
  # Three single-step chains: two in the first interval, one in the second,
  # each of length 5: -3 log 5.
  built <- random_augmenter(coupled_spec(coupled = FALSE))
  log_q <- vapply(draw_random(built, 5L), `[[`, numeric(1), "log_proposal")
  expect_equal(log_q, rep(-3 * log(5), 5L))
})

test_that("a count panel's chains are sorted and counted in the density", {
  # Chains of 2 in the first interval, of 3 and 1 in the second:
  # log 2! + log 3! - 6 log 5.
  built <- random_augmenter(count_spec())
  diff <- diff_panel_waves(built$spec$data, "calls", c(0, 5, 10))
  for (draw in draw_random(built, 5L)) {
    expect_equal(draw$log_proposal, log(2) + log(6) - 6 * log(5))
    expect_no_error(validate_endpoint_hitting(draw$data, diff))
  }
})

test_that("a coupling-free specification never redraws or replays", {
  built <- random_augmenter(coupled_spec(coupled = FALSE))
  expect_equal(built$augmenter$diagnostics()$legal$rate, c(1, 1))
  local_mocked_bindings(
    open_augmenter_walk = function(...) stop("a legality replay opened a walk")
  )
  draws <- draw_random(built, 10L)
  expect_false(any(vapply(draws, `[[`, logical(1), "restarted")))
})

# --- Support-legality reject-and-redraw ---------------------------------------

test_that("a coupled draw is always support-legal", {
  local_short_pilot()
  built <- random_augmenter(coupled_spec())
  draws <- draw_random(built, 20L)
  for (draw in draws) {
    rows <- layer_rows(draw, "friendship")
    dissolve <- rows$time[rows$from == 1L & rows$to == 2L & rows$time > 0]
    create <- rows$time[rows$from == 1L & rows$to == 3L]
    expect_lt(dissolve, create)
  }
  # Half the uniform orderings are illegal, so twenty draws all legal at the
  # first attempt has probability 2^-20.
  expect_true(any(vapply(draws, `[[`, logical(1), "restarted")))
})

test_that("the recorded density carries -log P(legal) at the observed rate", {
  local_short_pilot()
  built <- random_augmenter(coupled_spec())
  legal <- built$augmenter$diagnostics()$legal
  expect_equal(legal$interval, 1:2)
  expect_equal(legal$rate, legal$successes / legal$attempts)
  # Only the first interval is coupled in fact; its rate estimates 1/2.
  expect_equal(legal$rate[[2L]], 1)
  expect_equal(legal$rate[[1L]], 0.5, tolerance = 0.3)

  log_q <- vapply(draw_random(built, 10L), `[[`, numeric(1), "log_proposal")
  expect_equal(log_q, rep(-3 * log(5) - sum(log(legal$rate)), 10L))
})

test_that("a coupled draw emits no coupling bias warning", {
  local_short_pilot()
  built <- random_augmenter(coupled_spec())
  expect_no_warning(draw_random(built, 10L))
})

test_that("the legal-ordering estimate is fixed by the seed, not the draws", {
  local_short_pilot()
  a <- random_augmenter(coupled_spec(), seed = 3L)
  b <- random_augmenter(coupled_spec(), seed = 3L)
  expect_identical(a$augmenter$diagnostics(), b$augmenter$diagnostics())

  # Building the augmenter leaves the session's random stream untouched.
  withr::local_seed(99L)
  before <- .Random.seed
  random_augmenter(coupled_spec(), seed = 3L)
  expect_identical(.Random.seed, before)
})

test_that("a panel no legal ordering reaches aborts at the ceiling", {
  local_cli_context()
  local_mocked_bindings(restart_ceiling = function() 5L)
  # N1 gains 1 -> 3 in the first interval and drops 1 -> 2 only in the
  # second: no ordering of the first satisfies the coupling.
  data <- coupled_data()
  late <- data$ties$layer == "friendship" &
    data$ties$from == 1L &
    data$ties$to == 2L &
    !is.na(data$ties$time)
  data$ties$time[late] <- 10

  expect_snapshot(random_augmenter(coupled_spec(data)), error = TRUE)
  expect_error(
    random_augmenter(coupled_spec(data)),
    class = "goldfish_dynes_restart_ceiling"
  )
})
