# Task 1.1a/1.1b (abmcem): the three child option-bundle constructors nested
# under set_algorithm_em() -- set_augmenter_options(), set_weights_options(),
# set_sgd_options(). Each validates only its own arguments (design D2); the
# cross-object validity matrix lives in set_algorithm_em() (task 1.2).

test_that("set_augmenter_options works correctly", {
  expected_names <- c(
    "routine",
    "burn_in",
    "thinning",
    "initialize",
    "initial_sequence",
    "warm_start",
    "move_probs"
  )

  default_opts <- set_augmenter_options()
  expect_s3_class(
    default_opts,
    c("goldfishAugmenterCtrl", "list"),
    exact = TRUE
  )
  expect_true(all(expected_names %in% names(default_opts)))

  expect_equal(default_opts$routine, "mcmc")
  expect_equal(default_opts$initialize, "random")
  expect_null(default_opts$burn_in)
  expect_null(default_opts$thinning)
  expect_null(default_opts$initial_sequence)
  expect_false(default_opts$warm_start)
  expect_equal(default_opts$move_probs, c(permute = 0.5, shift = 0.5))

  custom_opts <- set_augmenter_options(
    routine = "random",
    burn_in = 50L,
    thinning = 2L,
    initialize = "sim",
    warm_start = TRUE,
    move_probs = c(permute = 3, shift = 1)
  )
  expect_equal(custom_opts$routine, "random")
  expect_equal(custom_opts$burn_in, 50L)
  expect_equal(custom_opts$thinning, 2L)
  expect_equal(custom_opts$initialize, "sim")
  expect_true(custom_opts$warm_start)
  # move_probs is normalized internally.
  expect_equal(custom_opts$move_probs, c(permute = 0.75, shift = 0.25))
})

test_that("set_augmenter_options rejects invalid own arguments", {
  local_reproducible_output()
  expect_snapshot(set_augmenter_options(routine = "exact"), error = TRUE)
  expect_snapshot(set_augmenter_options(initialize = "exact"), error = TRUE)
  expect_error(set_augmenter_options(burn_in = -1L))
  expect_error(set_augmenter_options(burn_in = c(1L, 2L)))
  expect_error(set_augmenter_options(thinning = 0L))
  expect_error(set_augmenter_options(warm_start = "yes"))
  expect_error(set_augmenter_options(warm_start = c(TRUE, FALSE)))
  expect_error(set_augmenter_options(move_probs = c(permute = -1, shift = 1)))
  expect_error(set_augmenter_options(move_probs = c(permute = 0, shift = 0)))
  expect_error(set_augmenter_options(move_probs = c(unknown = 1)))
  expect_error(set_augmenter_options(move_probs = c(1, 2)))
})

test_that("set_weights_options works correctly", {
  expected_names <- c(
    "weighting",
    "use",
    "resampling_scheme",
    "transformation",
    "refresh",
    "ess_threshold"
  )

  default_opts <- set_weights_options()
  expect_s3_class(default_opts, c("goldfishWeightsCtrl", "list"), exact = TRUE)
  expect_true(all(expected_names %in% names(default_opts)))

  expect_equal(default_opts$weighting, "importance")
  expect_equal(default_opts$use, "importance")
  expect_equal(default_opts$resampling_scheme, "stratified")
  expect_equal(default_opts$transformation, "identity")
  expect_false(default_opts$refresh)
  expect_equal(default_opts$ess_threshold, 0.5)

  custom_opts <- set_weights_options(
    weighting = "uniform",
    use = "resampling",
    resampling_scheme = "residual",
    transformation = "clipping",
    refresh = TRUE,
    ess_threshold = 0.3
  )
  expect_equal(custom_opts$weighting, "uniform")
  expect_equal(custom_opts$use, "resampling")
  expect_equal(custom_opts$resampling_scheme, "residual")
  expect_equal(custom_opts$transformation, "clipping")
  expect_true(custom_opts$refresh)
  expect_equal(custom_opts$ess_threshold, 0.3)
})

test_that("set_weights_options rejects invalid own arguments", {
  local_reproducible_output()
  expect_snapshot(set_weights_options(weighting = "exact"), error = TRUE)
  expect_snapshot(set_weights_options(use = "exact"), error = TRUE)
  expect_snapshot(
    set_weights_options(resampling_scheme = "exact"),
    error = TRUE
  )
  # transformation is a named enum: a non-member is rejected, not accepted as
  # a user-supplied closure (design D2/D5, Q4 resolution).
  expect_snapshot(set_weights_options(transformation = "exact"), error = TRUE)
  expect_error(set_weights_options(transformation = identity))
  expect_error(set_weights_options(refresh = "yes"))
  expect_error(set_weights_options(ess_threshold = 0))
  expect_error(set_weights_options(ess_threshold = 1.5))
  expect_error(set_weights_options(ess_threshold = c(0.3, 0.5)))
})

test_that("set_sgd_options works correctly", {
  expected_names <- c(
    "variant",
    "batch_size",
    "batch_scheme",
    "step_size",
    "step_schedule",
    "max_iterations",
    "sgd_tolerance",
    "convergence"
  )

  default_opts <- set_sgd_options()
  expect_s3_class(default_opts, c("goldfishSgdCtrl", "list"), exact = TRUE)
  expect_true(all(expected_names %in% names(default_opts)))

  expect_equal(default_opts$variant, "minibatch")
  expect_null(default_opts$batch_size)
  expect_equal(default_opts$batch_scheme, "weighted")
  expect_equal(default_opts$step_size, 0.001)
  expect_equal(default_opts$step_schedule, "constant")
  expect_equal(default_opts$max_iterations, 100L)
  expect_equal(default_opts$sgd_tolerance, 1e-3)
  expect_equal(default_opts$convergence, "gradient")

  custom_opts <- set_sgd_options(
    variant = "full",
    batch_size = 20L,
    batch_scheme = "cyclic",
    step_size = 0.01,
    step_schedule = "decay",
    max_iterations = 200L,
    sgd_tolerance = 1e-4,
    convergence = "iterations"
  )
  expect_equal(custom_opts$variant, "full")
  expect_equal(custom_opts$batch_size, 20L)
  expect_equal(custom_opts$batch_scheme, "cyclic")
  expect_equal(custom_opts$step_size, 0.01)
  expect_equal(custom_opts$step_schedule, "decay")
  expect_equal(custom_opts$max_iterations, 200L)
  expect_equal(custom_opts$sgd_tolerance, 1e-4)
  expect_equal(custom_opts$convergence, "iterations")
})

test_that("set_sgd_options rejects invalid own arguments", {
  local_reproducible_output()
  expect_snapshot(set_sgd_options(variant = "exact"), error = TRUE)
  expect_snapshot(set_sgd_options(batch_scheme = "exact"), error = TRUE)
  expect_snapshot(set_sgd_options(step_schedule = "exact"), error = TRUE)
  expect_snapshot(set_sgd_options(convergence = "exact"), error = TRUE)
  expect_error(set_sgd_options(batch_size = 0L))
  expect_error(set_sgd_options(batch_size = c(1L, 2L)))
  expect_error(set_sgd_options(step_size = 0))
  expect_error(set_sgd_options(step_size = -0.1))
  expect_error(set_sgd_options(max_iterations = 0L))
  expect_error(set_sgd_options(max_iterations = -1L))
  expect_error(set_sgd_options(sgd_tolerance = 0))
  expect_error(set_sgd_options(sgd_tolerance = -1e-3))
})

test_that("sgd_tolerance is named distinctly from the EM-stop tolerance", {
  # set_sgd_options() carries the M-step gradient-norm tolerance under
  # `sgd_tolerance`, never `tolerance` -- design D2/D6: the two scales
  # (gradient-norm vs Q-scale) must never be set by the same argument name.
  expect_false("tolerance" %in% names(set_sgd_options()))
  expect_true("sgd_tolerance" %in% names(set_sgd_options()))
})
