# The augmentation_recipe: the wave grid inferred from the panel data, the
# static theta-free plan compiled once at estimation entry, and the mutable
# per-draw cursor an augmenter holds over it.

local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# A flavored state panel (friendship) modeled choice-only beside an observed
# calls stream, six actors. Friendship changes are stamped at the waves 5 and
# 10 that first record them; the history rows are the starting network.
# Interval 1 creates (3, 4) and (4, 5) and dissolves (1, 2); interval 2
# creates (5, 6) and dissolves (2, 3).
recipe_state_data <- function() {
  friendship <- data.frame(
    from = c(1L, 2L, 3L, 4L, 1L, 5L, 2L),
    to = c(2L, 3L, 4L, 5L, 2L, 6L, 3L),
    time = c(NA, NA, 5, 5, 5, 10, 10),
    weight = c(1, 1, 1, 1, -1, 1, -1),
    layer = "friendship"
  )
  calls <- data.frame(
    from = c(1L, 2L, 3L, 6L),
    to = c(3L, 4L, 5L, 1L),
    time = c(1, 3, 6, 8),
    weight = 1,
    layer = "calls"
  )
  add_flavor(
    list(
      info = list(
        name = "toy",
        focal = "calls",
        update = c(friendship = "increment", calls = "increment"),
        directed = c(friendship = TRUE, calls = TRUE),
        observation = c(friendship = "panel", calls = "event")
      ),
      nodes = data.frame(
        label = paste0("N", 1:6),
        mode = "p",
        stringsAsFactors = FALSE
      ),
      ties = rbind(friendship, calls)
    ),
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

recipe_state_spec <- function(data = recipe_state_data()) {
  make_joint_specification(
    make_specification(
      choice = list(creation ~ indeg, dissolution ~ indeg),
      layer = "friendship",
      model = "DyNAM",
      data = data
    ),
    make_specification(
      rate = ~ 1 + indeg(friendship),
      choice = ~inertia,
      layer = "calls",
      model = "DyNAM",
      data = data
    ),
    data = data
  )
}

# An unflavored count panel (calls, cumulative counts at waves 5 and 10)
# beside an observed emails stream, four actors: (1, 2) rises 0 -> 2 -> 5 and
# (2, 3) 0 -> 0 -> 1.
recipe_count_data <- function() {
  list(
    info = list(
      name = "toy",
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
}

recipe_count_spec <- function(data = recipe_count_data()) {
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

# --- Wave grid --------------------------------------------------------------

test_that("the wave grid is the distinct times of the panel data", {
  # With no observation start given, the first recorded wave starts the grid.
  expect_identical(panel_wave_grid(recipe_state_spec()), c(5, 10))
})

test_that("a given observation start opens the grid before the first wave", {
  expect_identical(
    panel_wave_grid(recipe_state_spec(), start_time = 0),
    c(0, 5, 10)
  )
  # Waves at or before the start are history, folded into the initial state.
  expect_identical(
    panel_wave_grid(recipe_state_spec(), start_time = 5),
    c(5, 10)
  )
})

test_that("explicit wave times override the inferred grid", {
  expect_identical(
    panel_wave_grid(recipe_state_spec(), wave_times = c(0, 5, 7, 10)),
    c(0, 5, 7, 10)
  )
  expect_error(
    panel_wave_grid(recipe_state_spec(), wave_times = c(5, 0)),
    class = "goldfish_invalid_wave_grid"
  )
})

test_that("only the modeled panel layers' times make the grid", {
  # An exogenous panel covariate on another grid adds no boundary: a boundary
  # where the modeled layer has no snapshot would force its flips aside.
  data <- recipe_state_data()
  data$info$update <- c(data$info$update, school = "increment")
  data$info$directed <- c(data$info$directed, school = TRUE)
  data$info$observation <- c(data$info$observation, school = "panel")
  data$ties <- rbind(
    data$ties,
    data.frame(
      from = 1L,
      to = 4L,
      time = 7,
      weight = 1,
      layer = "school",
      flavor = NA_character_
    )
  )
  expect_identical(
    panel_wave_grid(recipe_state_spec(data), start_time = 0),
    c(0, 5, 10)
  )
})

test_that("a panel with a single wave and no start has no interval", {
  local_cli_context()
  data <- recipe_count_data()
  data$ties <- data$ties[data$ties$layer != "calls" | data$ties$time == 5, ]

  expect_snapshot(panel_wave_grid(recipe_count_spec(data)), error = TRUE)
  expect_error(
    panel_wave_grid(recipe_count_spec(data)),
    class = "goldfish_invalid_wave_grid"
  )
})

# --- Recipe -----------------------------------------------------------------

test_that("a state panel compiles into chains, flavors and derived support", {
  recipe <- compile_augmentation_recipe(recipe_state_spec(), c(0, 5, 10))

  expect_s3_class(recipe, "goldfishAugRecipe")
  expect_identical(recipe$wave_times, c(0, 5, 10))
  expect_named(recipe$layers, "friendship")
  layer <- recipe$layers$friendship
  expect_identical(layer$kind, "state")
  expect_identical(layer$transitions$flavor, c("creation", "dissolution"))
  expect_identical(
    layer$support,
    c(creation = "creation", dissolution = "dissolution")
  )
  # Membership only: which dyads flip, and their chain order.
  ev <- recipe$events
  expect_identical(nrow(ev), 5L)
  expect_identical(ev$event, 1:5)
  expect_identical(tabulate(ev$interval, 2L), c(3L, 2L))
  expect_identical(
    recipe$chains[c("interval", "from", "to", "length")],
    data.frame(
      interval = c(1L, 1L, 1L, 2L, 2L),
      from = c(1L, 3L, 4L, 2L, 5L),
      to = c(2L, 4L, 5L, 3L, 6L),
      length = 1L
    )
  )
})

test_that("a count panel compiles into multiplicities, no flavor or support", {
  recipe <- compile_augmentation_recipe(recipe_count_spec(), c(0, 5, 10))

  layer <- recipe$layers$calls
  expect_identical(layer$kind, "count")
  expect_null(layer$transitions)
  expect_identical(layer$support, c(calls = "none"))
  expect_true(all(is.na(recipe$events$flavor)))
  # Per-dyad remaining counts: the chain lengths.
  expect_identical(
    recipe$chains[c("interval", "from", "to", "length")],
    data.frame(
      interval = c(1L, 2L, 2L),
      from = c(1L, 1L, 2L),
      to = c(2L, 2L, 3L),
      length = c(2L, 3L, 1L)
    )
  )
})

test_that("the pair graph joins same-interval events of different chains", {
  recipe <- compile_augmentation_recipe(recipe_count_spec(), c(0, 5, 10))

  # Interval 1 is one two-event chain: a same-dyad swap is an identity, so no
  # pair. Interval 2 is a three-event chain and a one-event chain: 3 pairs,
  # C(4, 2) - C(3, 2).
  expect_identical(vapply(recipe$pairs, nrow, integer(1)), c(0L, 3L))
  ev <- recipe$events
  pairs <- recipe$pairs[[2L]]
  expect_true(all(ev$interval[pairs] == 2L))
  expect_true(all(ev$chain[pairs[, 1L]] != ev$chain[pairs[, 2L]]))

  state <- compile_augmentation_recipe(recipe_state_spec(), c(0, 5, 10))
  expect_identical(vapply(state$pairs, nrow, integer(1)), c(3L, 1L))
})

test_that("the per-wave start states are materialized once, per layer", {
  spec <- recipe_state_spec()
  recipe <- compile_augmentation_recipe(spec, c(0, 5, 10))

  expect_length(recipe$start_states, 3L)
  expect_named(recipe$start_states[[1L]], c("friendship", "calls"))
  diff <- diff_panel_waves(spec$data, "friendship", c(0, 5, 10))
  for (k in 1:3) {
    expect_identical(
      recipe$start_states[[k]]$friendship,
      diff$snapshots[[k]]
    )
  }
  # The observed stream's state is inclusive at the wave too: the call at 3
  # is in the state at wave 5, the call at 6 is not.
  expect_identical(recipe$start_states[[2L]]$calls[2, 4], 1)
  expect_identical(recipe$start_states[[2L]]$calls[3, 5], 0)
})

test_that("the injective flavor map is validated once per panel layer", {
  calls <- 0L
  check <- check_flavor_map_injective
  local_mocked_bindings(
    check_flavor_map_injective = function(...) {
      calls <<- calls + 1L
      check(...)
    }
  )
  recipe <- compile_augmentation_recipe(recipe_state_spec(), c(0, 5, 10))
  for (i in 1:5) {
    cursor <- new_recipe_cursor(recipe)
    while (!cursor$done()) {
      cursor$take(cursor$remaining()$chain[[1L]])
    }
  }

  expect_identical(calls, 1L)
})

test_that("a non-injective flavor map aborts at compilation", {
  local_cli_context()
  spec <- recipe_state_spec()
  # Two flavors on one transition: a wave change could not name its flavor.
  spec$data$info$values_equivalence$friendship <- c(creation = 1, renewal = 1)

  expect_snapshot(
    compile_augmentation_recipe(spec, c(0, 5, 10)),
    error = TRUE
  )
  expect_error(
    compile_augmentation_recipe(spec, c(0, 5, 10)),
    class = "goldfish_noninjective_flavor_map"
  )
})

# --- Cursor -----------------------------------------------------------------

test_that("a cursor takes each chain's events in step order", {
  recipe <- compile_augmentation_recipe(recipe_count_spec(), c(0, 5, 10))
  cursor <- new_recipe_cursor(recipe)
  chain <- recipe$chains$chain[recipe$chains$length == 3L]

  taken <- vapply(1:3, function(i) cursor$take(chain), integer(1))

  expect_identical(recipe$events$step[taken], 1:3)
  expect_true(all(recipe$events$chain[taken] == chain))
  expect_error(cursor$take(chain), class = "goldfish_recipe_cursor_exhausted")
})

test_that("a cursor lists the chains with events left, per interval", {
  recipe <- compile_augmentation_recipe(recipe_count_spec(), c(0, 5, 10))
  cursor <- new_recipe_cursor(recipe)

  expect_identical(cursor$remaining(1L)$left, 2L)
  expect_identical(cursor$remaining(2L)$left, c(3L, 1L))
  cursor$take(cursor$remaining(1L)$chain)
  cursor$take(cursor$remaining(1L)$chain)
  expect_identical(nrow(cursor$remaining(1L)), 0L)
  expect_true(cursor$done(1L))
  expect_false(cursor$done())
})

test_that("cursors are independent and never mutate the recipe", {
  recipe <- compile_augmentation_recipe(recipe_state_spec(), c(0, 5, 10))
  frozen <- rlang::duplicate(recipe)
  a <- new_recipe_cursor(recipe)
  b <- new_recipe_cursor(recipe)

  while (!a$done()) {
    a$take(a$remaining()$chain[[1L]])
  }

  expect_identical(nrow(b$remaining()), nrow(recipe$chains))
  expect_identical(recipe, frozen)
  a$reset()
  expect_identical(a$remaining(), b$remaining())
})

# --- Estimation entry -------------------------------------------------------

test_that("estimate_dynes compiles the recipe once and hands it over", {
  local_analytic_evaluator()
  proposals <- c(-0.5, 0.5, 0.5 + 1e-5)
  pass <- 0L
  local_scripted_optimizer(function(theta) {
    pass <<- pass + 1L
    rep(proposals[[pass]], length(theta))
  })
  compiled <- 0L
  compile <- compile_augmentation_recipe
  received <- NULL
  local_mocked_bindings(
    compile_augmentation_recipe = function(...) {
      compiled <<- compiled + 1L
      compile(...)
    },
    resolve_dynes_augmenter = function(spec, recipe, control, call) {
      received <<- recipe
      new_keyed_stub_augmenter()
    }
  )

  suppressWarnings(estimate_dynes(
    dynes_surface_spec(),
    set_algorithm_em(
      n_sequences = 4L,
      seed = 42L,
      augmenter = set_augmenter_options(routine = "random")
    )
  ))

  expect_identical(compiled, 1L)
  expect_s3_class(received, "goldfishAugRecipe")
})

test_that("estimate_dynes compiles on the start time and explicit waves", {
  received <- NULL
  local_mocked_bindings(
    resolve_dynes_augmenter = function(spec, recipe, control, call) {
      received <<- recipe
      rlang::abort("stop", class = "recipe_seen")
    }
  )
  spec <- recipe_state_spec()
  control <- set_algorithm_em(
    n_sequences = 4L,
    augmenter = set_augmenter_options(routine = "random")
  )

  expect_error(
    suppressWarnings(estimate_dynes(
      spec,
      control,
      control_prep = set_preprocessing(start_time = 0)
    )),
    class = "recipe_seen"
  )
  expect_identical(received$wave_times, c(0, 5, 10))

  expect_error(
    suppressWarnings(estimate_dynes(spec, control, wave_times = c(0, 5, 10))),
    class = "recipe_seen"
  )
  expect_identical(received$wave_times, c(0, 5, 10))
})
