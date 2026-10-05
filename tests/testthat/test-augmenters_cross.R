# Properties every augmenter shares, checked on all three routines over one
# two-flavor fixture: a draw is a pure function of the random number stream
# injected into it, and the sequence it emits carries the flavor keys
# add_flavor() stamps, so the pool entry routes each event to its own flavor.

# A flavored friendship panel (waves 5 and 10, observation start 0) beside an
# observed calls stream at 2 and 7, four actors, no support constraint.
# N1 creates 1 -> 2 and N2 dissolves 2 -> 1 by wave 5; N1 dissolves 1 -> 2 by
# wave 10, so both flavors carry latent events.
cross_data <- function() {
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
      name = "cross",
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
  cross_flavor(data)
}

cross_flavor <- function(data) {
  add_flavor(
    data,
    layer = "friendship",
    values_equivalence = c(creation = 1, dissolution = -1),
    flavor_style = "mutually_exclusive"
  )
}

# The specification completed as estimate_dynes() completes it (the pinned
# friendship rates warn), its recipe, and one augmenter per routine over them.
# theta: creation indeg, dissolution indeg, calls intercept, calls outdeg.
cross_build <- function(
  spec = cross_spec(),
  waves = panel_wave_grid(spec, start_time = 0)
) {
  completed <- suppressWarnings(complete_generative_spec(
    spec,
    consumer = "estimate_dynes",
    wave_times = waves
  ))
  list(
    spec = completed,
    recipe = compile_augmentation_recipe(completed, waves),
    waves = waves,
    theta = c(0.2, -0.2, -1, 0.3)
  )
}

cross_spec <- function(data = cross_data()) {
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

cross_augmenter <- function(built, routine) {
  control <- set_algorithm_em(
    augmenter = set_augmenter_options(
      routine = routine,
      burn_in = if (routine == "mcmc") 0L,
      thinning = if (routine == "mcmc") 1L
    ),
    seed = 5L
  )
  dynes_augmenter_constructors()[[routine]](built$spec, built$recipe, control)
}

# An L'Ecuyer-CMRG stream state, as the map seam injects one.
cross_stream <- function(seed) {
  withr::with_seed(seed, .rng_kind = "L'Ecuyer-CMRG", get_rng_state())
}

# A stub target for the MCMC chain that penalizes late friendship events, so
# the chain both accepts and rejects, with rates read off the pair's sender.
cross_evaluator <- function(waves) {
  function(candidate_seq, rate_queries = NULL, loglik = TRUE) {
    ties <- as.data.frame(candidate_seq$ties)
    latent <- ties$layer == "friendship" &
      !is.na(ties$time) &
      ties$time > waves[[1L]] &
      ties$time < waves[[length(waves)]]
    list(
      loglik = if (loglik) -0.8 * sum(ties$time[latent]) else NA_real_,
      rates = if (!is.null(rate_queries)) {
        vapply(
          rate_queries,
          function(q) 0.2 * as.integer(sub("N", "", q$sender)),
          numeric(1)
        )
      }
    )
  }
}

# One run of an augmenter on the injected `state`, as the map seam runs it:
# `random` and `sim` draw once per injected substream; `mcmc` runs its whole
# chain -- the start draw, the target injection and three retained draws --
# on one chain stream. The augmenter is built fresh, so nothing carries over
# from an earlier run.
cross_run <- function(built, routine, state, iteration = 1L, draw_index = 1L) {
  augmenter <- cross_augmenter(built, routine)
  withr::local_preserve_seed()
  set_rng_state(state)
  if (routine != "mcmc") {
    return(augmenter$draw(iteration, draw_index, built$theta)[[1L]])
  }
  start <- augmenter$draw(0L, 1L, built$theta)
  augmenter$init(cross_evaluator(built$waves), built$theta)
  list(
    start = start,
    draws = augmenter$draw(iteration, draw_index + 0:2, built$theta),
    diagnostics = augmenter$diagnostics()
  )
}

cross_routines <- c("random", "sim", "mcmc")

cross_draws <- function(run) {
  if (is.null(run$draws)) list(run) else c(run$start, run$draws)
}

# --- Injected-stream determinism --------------------------------------------

test_that("the same injected stream reproduces a draw bit for bit", {
  built <- cross_build()
  state <- cross_stream(9L)
  for (routine in cross_routines) {
    first <- withr::with_seed(1L, cross_run(built, routine, state))
    again <- withr::with_seed(2L, cross_run(built, routine, state))
    expect_identical(again, first, label = routine)
  }
})

test_that("a draw reads its stream, never the session's RNG state", {
  built <- cross_build()
  state <- cross_stream(9L)
  for (routine in cross_routines) {
    reference <- cross_run(built, routine, state)
    # A different session RNG kind and state before the injection.
    moved <- withr::with_seed(
      42L,
      .rng_kind = "Mersenne-Twister",
      {
        stats::runif(3)
        cross_run(built, routine, state)
      }
    )
    expect_identical(moved, reference, label = routine)
  }
})

test_that("a draw does not read its key, only the stream it is given", {
  built <- cross_build()
  state <- cross_stream(9L)
  for (routine in cross_routines) {
    expect_identical(
      cross_run(built, routine, state, iteration = 7L, draw_index = 31L),
      cross_run(built, routine, state),
      label = routine
    )
  }
})

test_that("a different injected stream gives a different draw", {
  built <- cross_build()
  state <- cross_stream(9L)
  for (routine in cross_routines) {
    one <- cross_draws(cross_run(built, routine, state))
    other <- cross_draws(
      cross_run(built, routine, parallel::nextRNGSubStream(state))
    )
    times <- function(draws) {
      lapply(draws, function(draw) {
        ties <- as.data.frame(draw$data$ties)
        ties$time[ties$layer == "friendship" & !is.na(ties$time)]
      })
    }
    expect_false(identical(times(one), times(other)), label = routine)
  }
})

test_that("the mcmc run both accepts and rejects on its stream", {
  run <- cross_run(cross_build(), "mcmc", cross_stream(9L))
  rate <- run$diagnostics$acceptance_rate
  expect_gt(rate, 0)
  expect_lt(rate, 1)
})

# --- Flavor keys --------------------------------------------------------------

test_that("every drawn sequence carries the flavor keys add_flavor() stamps", {
  built <- cross_build()
  for (routine in cross_routines) {
    for (draw in cross_draws(cross_run(built, routine, cross_stream(9L)))) {
      data <- draw$data
      timed <- data$ties$layer == "friendship" & !is.na(data$ties$time)
      expect_false(anyNA(data$ties$flavor[timed]), label = routine)

      unflavored <- data
      unflavored$ties$flavor <- NULL
      expect_identical(
        data$ties$flavor,
        cross_flavor(unflavored)$ties$flavor,
        label = routine
      )
    }
  }
})

test_that("the pool entry routes every drawn event to its own flavor", {
  built <- cross_build()
  engine_spec <- decompose_joint_spec(built$spec)$engine_spec
  map <- engine_spec$process_map
  for (routine in cross_routines) {
    for (draw in cross_draws(cross_run(built, routine, cross_stream(9L)))) {
      ties <- as.data.frame(draw$data$ties)
      timed <- ties$layer == "friendship" & !is.na(ties$time)
      stats <- suppressWarnings(
        preprocess_pool_sequence(engine_spec, draw$data)
      )
      for (i in which(map$layer == "friendship")) {
        own <- timed & ties$flavor %in% map$flavor[[i]]
        fid_stats <- stats[[as.character(map$fid[[i]])]]
        expect_equal(
          fid_stats$event_time[fid_stats$is_dependent == 1L],
          sort(ties$time[own]),
          label = paste(routine, "fid", map$fid[[i]], "dependent event times")
        )
      }
    }
  }
})

# --- Pins and draws share one grid ------------------------------------------

# What estimate_dynes() hands the augmenters: the specification it completed
# and the recipe it compiled, at the observation start 0.
cross_entry <- function() {
  entry <- NULL
  local_mocked_bindings(
    resolve_dynes_augmenter = function(spec, recipe, control, call) {
      entry <<- list(spec = spec, recipe = recipe)
      rlang::abort("entry reached", class = "cross_entry_seen")
    }
  )
  expect_error(
    suppressWarnings(estimate_dynes(
      cross_spec(),
      set_algorithm_em(augmenter = set_augmenter_options(routine = "random")),
      control_prep = set_preprocessing(start_time = 0)
    )),
    class = "cross_entry_seen"
  )
  entry
}

test_that("no draw places a pinned process's event in its zero-count period", {
  entry <- cross_entry()
  spec <- entry$spec
  map <- spec$process_map
  creation <- map$fid[
    map$completed & map$family == "rate" & map$flavor %in% "creation"
  ]
  # The premise: creation records no flip between waves 5 and 10, so its
  # second plateau is a zero hazard.
  intercept <- as.numeric(
    spec$completed_rates[[as.character(creation)]]$intercept
  )
  expect_length(intercept, 2L)
  expect_true(is.finite(intercept[[1]]))
  expect_identical(intercept[[2]], -Inf)
  built <- list(
    spec = spec,
    recipe = entry$recipe,
    waves = entry$recipe$wave_times,
    theta = c(0.2, -0.2, -1, 0.3)
  )
  decomposition <- decompose_joint_spec(spec)
  prep <- set_preprocessing(start_time = 0, end_time = 10)

  for (routine in cross_routines) {
    for (seed in 1:3) {
      for (draw in cross_draws(cross_run(built, routine, cross_stream(seed)))) {
        ties <- as.data.frame(draw$data$ties)
        late_creation <- ties$layer == "friendship" &
          ties$flavor %in% "creation" &
          !is.na(ties$time) &
          ties$time > 5
        expect_false(any(late_creation), label = routine)

        stats <- suppressWarnings(
          preprocess_pool_sequence(
            decomposition$engine_spec,
            draw$data,
            control_prep = prep
          )
        )
        fixed <- pool_fixed_loglik(
          with_coefficient_masks(decomposition$sub_models, draw$data),
          stats
        )
        expect_false(fixed$degenerate, label = routine)
        expect_true(is.finite(fixed$loglik), label = routine)
      }
    }
  }
})

# --- An interval without changes ---------------------------------------------

# cross_data() without the dissolution stamped at wave 10: the panel is
# unchanged between waves 5 and 10, so that interval has no latent event. The
# grid is passed explicitly, as estimate_dynes() passes the one it resolves;
# inferred from the panel rows alone it would end at wave 5.
quiet_interval_build <- function() {
  data <- cross_data()
  data$ties <- data$ties[
    !(data$ties$layer == "friendship" & data$ties$time %in% 10),
  ]
  cross_build(cross_spec(data), waves = c(0, 5, 10))
}

test_that("an interval without changes draws no event and hits every wave", {
  built <- quiet_interval_build()
  diff <- diff_panel_waves(built$spec$data, "friendship", built$waves)
  for (routine in cross_routines) {
    run <- cross_run(built, routine, cross_stream(9L))
    for (draw in cross_draws(run)) {
      expect_no_error(validate_endpoint_hitting(draw$data, diff))
      ties <- as.data.frame(draw$data$ties)
      latent <- ties$layer == "friendship" & !is.na(ties$time)
      expect_false(any(ties$time[latent] > 5 & ties$time[latent] < 10))
    }
  }
})
