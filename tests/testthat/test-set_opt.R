test_that("set_algorithm_newton works correctly", {
  expected_est_names <- c(
    "initial_parameters",
    "fixed_parameters",
    "max_iterations",
    "score_tol",
    "step_tol",
    "initial_damping",
    "damping_increase_factor",
    "damping_decrease_factor",
    "return_interval_loglik",
    "return_probabilities",
    "engine"
  )

  # Test defaults
  default_opts <- set_algorithm_newton()
  expect_s3_class(
    default_opts,
    c("algorithm_newton.goldfish", "algorithm.goldfish", "list"),
    exact = TRUE
  )
  expect_true(is.list(default_opts))
  # Check that all expected names are present
  expect_true(all(expected_est_names %in% names(default_opts)))

  expect_equal(default_opts$engine, "default_c") # Default backend is cpp
  expect_equal(default_opts$max_iterations, 20)
  expect_equal(default_opts$score_tol, 1e-6)
  expect_equal(default_opts$step_tol, 1e-8)
  expect_null(default_opts$initial_damping) # Defaults to NULL
  expect_equal(default_opts$damping_increase_factor, 2)
  expect_equal(default_opts$damping_decrease_factor, 3)
  expect_true(default_opts$return_interval_loglik)
  expect_false(default_opts$return_probabilities)

  # Test setting specific parameters
  withr::local_options(lifecycle_verbosity = "quiet")
  custom_opts <- set_algorithm_newton(
    max_iterations = 50,
    backend = "r", # Change from cpp
    score_tol = 1e-7,
    step_tol = 1e-9,
    return_interval_loglik = TRUE,
    return_probabilities = TRUE,
    initial_damping = 15
  )
  expect_s3_class(
    custom_opts,
    c("algorithm_newton.goldfish", "algorithm.goldfish", "list"),
    exact = TRUE
  )
  expect_true(is.list(custom_opts))
  # Check that all expected names are present
  expect_true(all(expected_est_names %in% names(custom_opts)))
  expect_equal(custom_opts$max_iterations, 50)
  expect_equal(custom_opts$engine, "default")
  expect_equal(custom_opts$score_tol, 1e-7)
  expect_equal(custom_opts$step_tol, 1e-9)
  expect_true(custom_opts$return_interval_loglik)
  expect_true(custom_opts$return_probabilities)
  expect_equal(custom_opts$diagnostics, c("loglik", "probabilities"))
  expect_equal(custom_opts$initial_damping, 15)
  # Check a default value that wasn't changed is still there
  expect_equal(custom_opts$damping_increase_factor, 2)
})
test_that("set_algorithm_newton throw errors", {
  expect_error(set_algorithm_newton(backend = "invalid"))
  expect_error(set_algorithm_newton(max_iterations = -1))
  expect_error(set_algorithm_newton(score_tol = -1))
  expect_error(set_algorithm_newton(step_tol = -1))
  expect_error(set_algorithm_newton(initial_damping = -1))
  expect_error(set_algorithm_newton(damping_increase_factor = -1))
  expect_error(set_algorithm_newton(damping_decrease_factor = -1))
  expect_error(set_algorithm_newton(return_interval_loglik = -1))
  expect_error(set_algorithm_newton(return_probabilities = -1))
  expect_error(set_algorithm_newton(fixed_parameters = character(3)))
  expect_error(set_algorithm_newton(initial_parameters = character(3)))
})

test_that("backend resolves to the engine token estimation reads", {
  expect_equal(set_algorithm_newton()$engine, "default_c")
  expect_equal(set_algorithm_newton(backend = "cpp")$engine, "default_c")
  expect_equal(set_algorithm_newton(backend = "r")$engine, "default")
  expect_equal(
    set_algorithm_newton(backend = "gather")$engine,
    "gather_compute"
  )
})

test_that("the engine sentinel selects the same path as its backend", {
  withr::local_options(lifecycle_verbosity = "quiet")
  for (legacy in c("default_c", "default", "gather_compute")) {
    expect_equal(
      set_algorithm_newton(engine = legacy)$engine,
      set_algorithm_newton(backend = LEGACY_ENGINE_BACKENDS[[legacy]])$engine
    )
  }
})

test_that("convergence_criterion is deprecated in favor of score_tol", {
  expect_snapshot(invisible(set_algorithm_newton(convergence_criterion = 1e-4)))
})

test_that("deprecation messages name the current constructor", {
  # No two-hop chains: a message must never send users to a name that is itself
  # deprecated, so these render against set_algorithm_newton(), not the alias.
  expect_snapshot({
    invisible(set_algorithm_newton(fixed_parameters = c(NA, 2)))
    invisible(set_preprocessing(opportunities_list = list(c("A", "B"))))
  })
})
test_that("set_algorithm_newton resolves the diagnostics vocabulary", {
  expect_equal(set_algorithm_newton()$diagnostics, c("loglik", "scores"))
  expect_equal(
    set_algorithm_newton(diagnostics = TRUE)$diagnostics,
    c("loglik", "scores")
  )
  expect_equal(
    set_algorithm_newton(diagnostics = FALSE)$diagnostics,
    character(0)
  )
  expect_equal(
    set_algorithm_newton(diagnostics = character(0))$diagnostics,
    character(0)
  )
  expect_equal(
    set_algorithm_newton(diagnostics = "all")$diagnostics,
    c("loglik", "scores", "ranks", "margins", "probabilities")
  )
  expect_equal(
    set_algorithm_newton(
      diagnostics = c("scores", "scores", "ranks")
    )$diagnostics,
    c("scores", "ranks")
  )
})

test_that("set_algorithm_newton rejects invalid diagnostics", {
  expect_snapshot(
    set_algorithm_newton(diagnostics = c("loglik", "devianc")),
    error = TRUE
  )
  expect_snapshot(set_algorithm_newton(diagnostics = NA), error = TRUE)
  expect_snapshot(set_algorithm_newton(diagnostics = 1L), error = TRUE)
})

test_that("legacy return_* flags soft-deprecate onto diagnostics", {
  expect_snapshot(invisible(set_algorithm_newton(
    return_interval_loglik = TRUE
  )))
  expect_snapshot(invisible(set_algorithm_newton(return_probabilities = TRUE)))
  expect_snapshot(invisible(set_algorithm_newton(return_event_scores = TRUE)))
})

test_that("legacy flags map onto diagnostics and the derived flags", {
  withr::local_options(lifecycle_verbosity = "quiet")
  opt <- set_algorithm_newton(return_event_scores = TRUE)
  expect_equal(opt$diagnostics, c("loglik", "scores"))
  expect_true(opt$return_event_scores)
  expect_false(opt$return_probabilities)

  opt2 <- set_algorithm_newton(return_interval_loglik = FALSE)
  expect_equal(opt2$diagnostics, character(0))
  expect_false(opt2$return_interval_loglik)

  opt3 <- set_algorithm_newton(
    return_interval_loglik = TRUE,
    return_probabilities = TRUE
  )
  expect_equal(opt3$diagnostics, c("loglik", "probabilities"))
})

test_that("diagnostics default drives the storage flags (scores on)", {
  opt <- set_algorithm_newton()
  # `diagnostics = c("loglik", "scores")` is the single source of truth, so the
  # derived storage flags follow it: loglik and scores on, probabilities off.
  expect_true(opt$return_interval_loglik)
  expect_false(opt$return_probabilities)
  expect_true(opt$return_event_scores)
  # Default-sourced scores are not an explicit request (gather_compute drops
  # them silently rather than aborting).
  expect_false(opt$scores_explicit)
})

test_that("scores_explicit tracks explicit score requests", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_true(
    set_algorithm_newton(diagnostics = c("loglik", "scores"))$scores_explicit
  )
  expect_true(set_algorithm_newton(return_event_scores = TRUE)$scores_explicit)
  expect_false(set_algorithm_newton(diagnostics = "loglik")$scores_explicit)
  expect_false(set_algorithm_newton(diagnostics = FALSE)$scores_explicit)
})

test_that("mixing diagnostics with a legacy flag aborts", {
  expect_snapshot(
    set_algorithm_newton(diagnostics = "loglik", return_event_scores = TRUE),
    error = TRUE
  )
})

test_that("set_preprocessing works correctly", {
  expected_prep_names <- c(
    "start_time",
    "end_time",
    "opportunities_list",
    "impute"
  )

  # Test defaults
  default_opts <- set_preprocessing()
  expect_s3_class(
    default_opts,
    c("preprocessing.goldfish", "list"),
    exact = TRUE
  )
  expect_true(is.list(default_opts))
  # Check that all expected names are present
  expect_true(all(expected_prep_names %in% names(default_opts)))
  expect_null(default_opts$start_time)
  expect_null(default_opts$end_time)
  expect_null(default_opts$opportunities_list)

  # Test setting specific parameters (opportunities_list is soft-deprecated)
  withr::local_options(lifecycle_verbosity = "quiet")
  dummy_opportunities <- list(c("A", "B"), c("C", "D"))
  custom_opts <- set_preprocessing(
    start_time = 10,
    end_time = 100,
    opportunities_list = dummy_opportunities
  )
  expect_s3_class(
    custom_opts,
    c("preprocessing.goldfish", "list"),
    exact = TRUE
  )
  expect_true(is.list(custom_opts))
  # Check that all expected names are present
  expect_true(all(expected_prep_names %in% names(custom_opts)))
  expect_equal(custom_opts$start_time, 10)
  expect_equal(custom_opts$end_time, 100)
  expect_equal(custom_opts$opportunities_list, dummy_opportunities)
})
test_that("set_preprocessing throw errors", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(set_preprocessing(start_time = character(3)))
  expect_error(set_preprocessing(end_time = character(3)))
  expect_error(set_preprocessing(opportunities_list = -1))
})

test_that("opportunities_list is deprecated in favour of support_constraint", {
  # Fires the once-per-session lifecycle warning pointing to support_constraint.
  withr::local_options(lifecycle_verbosity = "warning")
  expect_snapshot(
    invisible(set_preprocessing(opportunities_list = list(c("A", "B"))))
  )
  # It still works (soft deprecation): the value is retained.
  withr::local_options(lifecycle_verbosity = "quiet")
  opt <- set_preprocessing(opportunities_list = list(c("A", "B")))
  expect_equal(opt$opportunities_list, list(c("A", "B")))
})

# cli error snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

test_that("the impute policy is stored and defaults to NULL", {
  expect_null(set_preprocessing()$impute)
  opt <- set_preprocessing(impute = c(party = "as_category"))
  expect_equal(opt$impute, c(party = "as_category"))
})

test_that("a valid summary or as_category policy is accepted", {
  expect_no_error(set_preprocessing(impute = c(x = "summary")))
  expect_no_error(
    set_preprocessing(impute = c(x = "as_category", y = "summary"))
  )
})

test_that("an unnamed impute vector aborts", {
  local_cli_context()
  expect_snapshot(set_preprocessing(impute = "as_category"), error = TRUE)
})

test_that("an unknown impute policy value aborts, listing supported values", {
  local_cli_context()
  expect_snapshot(set_preprocessing(impute = c(x = "bogus")), error = TRUE)
})

test_that("the reserved locf policy aborts as unimplemented", {
  local_cli_context()
  expect_snapshot(set_preprocessing(impute = c(x = "locf")), error = TRUE)
})
