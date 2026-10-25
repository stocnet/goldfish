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
  expect_equal(default_opts$sgd_tolerance, 0.05)
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

test_that("set_sgd_options resolves an unset step_size and max_iterations by schedule", {
  step_sizes <- c(
    constant = 0.001,
    decay = 0.001,
    momentum = 0.001,
    adagrad = 0.01,
    adam = 0.01
  )
  for (schedule in names(step_sizes)) {
    opts <- set_sgd_options(step_schedule = schedule)
    expect_equal(opts$step_size, step_sizes[[schedule]], info = schedule)
    expect_identical(opts$max_iterations, 100L, info = schedule)
  }
})

test_that("set_sgd_options keeps an explicit step_size and max_iterations", {
  for (schedule in c("constant", "decay", "adagrad", "adam", "momentum")) {
    opts <- set_sgd_options(
      step_schedule = schedule,
      step_size = 0.3,
      max_iterations = 7L
    )
    expect_equal(opts$step_size, 0.3, info = schedule)
    expect_identical(opts$max_iterations, 7L, info = schedule)
  }
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

# --------------------------------------------------------------------------- #
# Task 1.2a/1.2b (abmcem): set_algorithm_em() nests the three child option
# bundles and enforces the cross-object validity matrix + precedence table
# (design D2/D4/D5/D6/D7). It validates its own arguments and the nested
# controls' classes; the child constructors validate their own options.
# Out of scope here: the cold-start construction guard (task 1.3), and the
# per-fid warm-start *fit* + its singular-fit fallback, a runtime behavior
# deferred to the loop (task 5.2) since set_algorithm_em() never sees the spec.
# --------------------------------------------------------------------------- #

# A lightweight goldfishParams stand-in: set_algorithm_em() only checks the
# class at construction (it stores the object for the loop to project later), so
# a classed stub exercises the precedence branch without building a joint spec.
make_em_params <- function() {
  structure(
    list(free = c("calls › rate: indeg" = 0.3)),
    class = "goldfishParams"
  )
}

test_that("set_algorithm_em assembles defaults and nests the child controls", {
  ctrl <- set_algorithm_em()
  expect_s3_class(
    ctrl,
    c("goldfishAlgoEm", "goldfishAlgo", "list"),
    exact = TRUE
  )

  expect_equal(ctrl$n_sequences, 100L)
  expect_equal(ctrl$max_iterations, 50L)
  expect_equal(ctrl$accept_quantile, 0.2)
  expect_equal(ctrl$growth_quantile, 0.2)
  expect_equal(ctrl$stop_quantile, 0.1)
  expect_equal(ctrl$tolerance, 1e-3)
  expect_equal(ctrl$stop_count, 1L)
  expect_equal(ctrl$max_retries, 20L)
  expect_equal(ctrl$max_pool, 5000L)
  expect_null(ctrl$seed)
  expect_null(ctrl$initial_parameters)
  expect_false(ctrl$em_trace_se)
  expect_equal(ctrl$n_cores, 1L)

  expect_s3_class(ctrl$augmenter, "goldfishAugmenterCtrl")
  expect_s3_class(ctrl$weights, "goldfishWeightsCtrl")
  expect_s3_class(ctrl$optimizer, "goldfishSgdCtrl")
})

test_that("set_algorithm_em resolves a NULL batch_size to min(n_sequences, 10) once", {
  # default n_sequences = 100 -> batch of 10 (design D6).
  expect_equal(set_algorithm_em()$optimizer$batch_size, 10L)
  # a tiny pool floors the batch at n_sequences so it never exceeds the pool.
  expect_equal(set_algorithm_em(n_sequences = 4L)$optimizer$batch_size, 4L)
  # an explicit batch_size is used verbatim, never overridden.
  expect_equal(
    set_algorithm_em(
      n_sequences = 100L,
      optimizer = set_sgd_options(batch_size = 25L)
    )$optimizer$batch_size,
    25L
  )
})

test_that("set_algorithm_em control carries no n_cores_explicit field", {
  # Serial is the default (n_cores = 1L); the map seam now tests n_cores <= 1L
  # directly (task 6.5), so there is no explicit-vs-default flag to lose track
  # of when a control is copied or rebuilt.
  expect_false("n_cores_explicit" %in% names(set_algorithm_em()))
  expect_false("n_cores_explicit" %in% names(set_algorithm_em(n_cores = 2L)))
})

test_that("set_algorithm_em rejects invalid own arguments", {
  local_reproducible_output()
  expect_snapshot(set_algorithm_em(n_sequences = 0L), error = TRUE)
  expect_snapshot(set_algorithm_em(max_iterations = 0L), error = TRUE)
  expect_snapshot(set_algorithm_em(accept_quantile = 0), error = TRUE)
  expect_snapshot(set_algorithm_em(tolerance = 0), error = TRUE)
  expect_snapshot(set_algorithm_em(stop_count = 0L), error = TRUE)
  expect_snapshot(set_algorithm_em(max_pool = 0L), error = TRUE)
  expect_snapshot(set_algorithm_em(n_cores = 0L), error = TRUE)
  expect_error(set_algorithm_em(accept_quantile = 1))
  expect_error(set_algorithm_em(growth_quantile = 1.5))
  expect_error(set_algorithm_em(stop_quantile = -0.1))
  expect_error(set_algorithm_em(max_retries = 0L))
  expect_error(set_algorithm_em(seed = c(1L, 2L)))
  expect_error(set_algorithm_em(em_trace_se = "yes"))
})

test_that("set_algorithm_em rejects a nested control of the wrong class", {
  local_reproducible_output()
  expect_snapshot(set_algorithm_em(augmenter = list()), error = TRUE)
  expect_snapshot(set_algorithm_em(weights = set_sgd_options()), error = TRUE)
  expect_snapshot(
    set_algorithm_em(optimizer = set_weights_options()),
    error = TRUE
  )
})

test_that("set_algorithm_em accepts only a goldfishParams for initial_parameters", {
  local_reproducible_output()
  # A bare numeric vector / list is rejected, the error naming set_parameters().
  expect_snapshot(
    set_algorithm_em(initial_parameters = c(0.1, 0.2)),
    error = TRUE
  )
  expect_error(
    set_algorithm_em(initial_parameters = list(0.1)),
    "set_parameters"
  )
  # A goldfishParams is stored verbatim for the loop to project.
  p <- make_em_params()
  expect_identical(
    set_algorithm_em(initial_parameters = p)$initial_parameters,
    p
  )
})

test_that("uniform weighting off the mcmc+refresh cell warns and switches to importance", {
  local_reproducible_output()
  # random + uniform is invalid -> warn, corrected to importance (spec scenario).
  expect_snapshot(
    ctrl <- set_algorithm_em(
      augmenter = set_augmenter_options(routine = "random"),
      weights = set_weights_options(weighting = "uniform", refresh = TRUE)
    )
  )
  expect_equal(ctrl$weights$weighting, "importance")

  # mcmc + uniform but no refresh is still invalid -> warn, corrected.
  expect_warning(
    ctrl2 <- set_algorithm_em(
      augmenter = set_augmenter_options(routine = "mcmc"),
      weights = set_weights_options(weighting = "uniform", refresh = FALSE)
    )
  )
  expect_equal(ctrl2$weights$weighting, "importance")

  # mcmc + refresh + uniform is the one valid cell -> no warning, kept as-is.
  expect_no_warning(
    ctrl3 <- set_algorithm_em(
      augmenter = set_augmenter_options(routine = "mcmc"),
      weights = set_weights_options(weighting = "uniform", refresh = TRUE)
    )
  )
  expect_equal(ctrl3$weights$weighting, "uniform")
})

test_that("refresh makes staleness transformations inapplicable (warn + reset)", {
  local_reproducible_output()
  expect_snapshot(
    ctrl <- set_algorithm_em(
      weights = set_weights_options(transformation = "clipping", refresh = TRUE)
    )
  )
  expect_equal(ctrl$weights$transformation, "identity")
})

test_that("a non-default ess_threshold under refresh warns (ESS guard inapplicable)", {
  local_reproducible_output()
  expect_snapshot(
    ctrl <- set_algorithm_em(
      weights = set_weights_options(refresh = TRUE, ess_threshold = 0.3)
    )
  )
  expect_true(ctrl$weights$refresh)
})

test_that("a non-identity transformation with resampling warns (truncated-IS caveat)", {
  local_reproducible_output()
  expect_snapshot(
    ctrl <- set_algorithm_em(
      weights = set_weights_options(
        transformation = "clipping",
        use = "resampling"
      )
    )
  )
  # Warn-only: the transformation is kept, not reset.
  expect_equal(ctrl$weights$transformation, "clipping")
})

test_that("cyclic batches under resampling warn (importance-weighting inapplicable)", {
  local_reproducible_output()
  expect_snapshot(
    ctrl <- set_algorithm_em(
      weights = set_weights_options(use = "resampling"),
      optimizer = set_sgd_options(batch_scheme = "cyclic")
    )
  )
})

test_that("a supplied initial_parameters warn-and-ignores warm_start (precedence)", {
  local_reproducible_output()
  expect_snapshot(
    ctrl <- set_algorithm_em(
      initial_parameters = make_em_params(),
      augmenter = set_augmenter_options(warm_start = TRUE)
    )
  )
  # The explicit start wins: warm_start is disabled on the stored augmenter.
  expect_false(ctrl$augmenter$warm_start)

  # warm_start alone (no initial_parameters) passes through untouched.
  expect_no_warning(
    ctrl2 <- set_algorithm_em(
      augmenter = set_augmenter_options(warm_start = TRUE)
    )
  )
  expect_true(ctrl2$augmenter$warm_start)
})

# --------------------------------------------------------------------------- #
# Task 1.3a/1.3b (abmcem): the cold-start construction guard (design D4,
# dynes-augmentation D1/D14) -- routine = "random" combined with a non-zero
# theta0 (initial_parameters) or warm_start = TRUE warns that uniform draws
# sit far from the target; no abort, and silent at theta0 = 0.
# --------------------------------------------------------------------------- #

test_that("random routine + non-zero theta0 warns; random + theta0 = 0 is silent", {
  local_reproducible_output()

  # Non-zero initial_parameters with routine = "random" warns.
  expect_snapshot(
    ctrl <- set_algorithm_em(
      initial_parameters = make_em_params(),
      augmenter = set_augmenter_options(routine = "random")
    )
  )
  expect_equal(ctrl$augmenter$routine, "random")

  # warm_start = TRUE alone (no initial_parameters) also warns.
  expect_snapshot(
    ctrl2 <- set_algorithm_em(
      augmenter = set_augmenter_options(routine = "random", warm_start = TRUE)
    )
  )
  expect_true(ctrl2$augmenter$warm_start)

  # theta0 = 0 via the default NULL initial_parameters (no warm_start) is silent.
  expect_no_warning(
    set_algorithm_em(augmenter = set_augmenter_options(routine = "random"))
  )

  # theta0 = 0 via an explicit all-zero initial_parameters is also silent.
  zero_params <- structure(
    list(free = c("calls › rate: indeg" = 0)),
    class = "goldfishParams"
  )
  expect_no_warning(
    set_algorithm_em(
      initial_parameters = zero_params,
      augmenter = set_augmenter_options(routine = "random")
    )
  )

  # A non-random routine with a non-zero theta0 does not trigger this guard.
  expect_no_warning(
    set_algorithm_em(
      initial_parameters = make_em_params(),
      augmenter = set_augmenter_options(routine = "mcmc")
    )
  )
})
