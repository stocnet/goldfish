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
    "backend"
  )

  # Test defaults
  default_opts <- set_algorithm_newton()
  expect_s3_class(
    default_opts,
    c("goldfishAlgoNewton", "goldfishAlgo", "list"),
    exact = TRUE
  )
  expect_true(is.list(default_opts))
  # Check that all expected names are present
  expect_true(all(expected_est_names %in% names(default_opts)))

  expect_equal(default_opts$backend, "cpp") # Default backend is cpp
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
    c("goldfishAlgoNewton", "goldfishAlgo", "list"),
    exact = TRUE
  )
  expect_true(is.list(custom_opts))
  # Check that all expected names are present
  expect_true(all(expected_est_names %in% names(custom_opts)))
  expect_equal(custom_opts$max_iterations, 50)
  expect_equal(custom_opts$backend, "r")
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

test_that("the control object carries backend and no engine component", {
  expect_equal(set_algorithm_newton()$backend, "cpp")
  for (b in BACKEND_VALUES) {
    expect_equal(set_algorithm_newton(backend = b)$backend, b)
  }
  expect_false("engine" %in% names(set_algorithm_newton(backend = "cpp")))
})

test_that("the engine sentinel selects the same path as its backend", {
  withr::local_options(lifecycle_verbosity = "quiet")
  for (legacy in c("default_c", "default", "gather_compute")) {
    expect_equal(
      set_algorithm_newton(engine = legacy)$backend,
      set_algorithm_newton(backend = LEGACY_ENGINE_BACKENDS[[legacy]])$backend
    )
  }
})

test_that("a legacy value on backend selects the backend that replaced it", {
  withr::local_options(lifecycle_verbosity = "quiet")
  for (legacy in names(LEGACY_ENGINE_BACKENDS)) {
    expect_equal(
      set_algorithm_newton(backend = legacy)$backend,
      LEGACY_ENGINE_BACKENDS[[legacy]]
    )
  }
})

test_that("both arguments supplied resolves to backend", {
  withr::local_options(lifecycle_verbosity = "quiet")
  opts <- set_algorithm_newton(engine = "default", backend = "gather")
  expect_equal(opts$backend, "gather")
})

test_that("the engine argument and the legacy values are deprecated", {
  local_reproducible_output()
  withr::local_options(lifecycle_verbosity = "warning")
  # The old argument alone: one warning naming `backend`.
  expect_snapshot(invisible(set_algorithm_newton(engine = "cpp")))
  # The old argument with an old value: still one warning, naming both halves
  # of the rename rather than the deprecated intermediate `backend =
  # "gather_compute"`.
  expect_snapshot(invisible(set_algorithm_newton(engine = "gather_compute")))
  # The half-migrated call: the new argument with an old value.
  expect_snapshot(invisible(set_algorithm_newton(backend = "default")))
  # Both arguments, new value on `backend`: one warning, and `backend` wins.
  expect_snapshot(
    invisible(set_algorithm_newton(engine = "default", backend = "gather"))
  )
})

test_that("an unknown backend aborts naming the vocabulary", {
  local_reproducible_output()
  expect_snapshot(set_algorithm_newton(backend = "fortran"), error = TRUE)
})

test_that("a pre-2.0.0 control object resolves its legacy engine token", {
  # A control list built by any pre-2.0.0 constructor carries only `engine`
  # (e.g. restored from an .rds); the read shim resolves it silently.
  for (token in names(LEGACY_ENGINE_BACKENDS)) {
    legacy <- list(engine = token)
    expect_equal(algo_backend(legacy), LEGACY_ENGINE_BACKENDS[[token]])
    expect_no_warning(algo_backend(legacy))
  }
  # A 2.0.0 object answers from its own component.
  expect_equal(algo_backend(list(backend = "gather")), "gather")
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
    c(
      "loglik",
      "scores",
      "ranks",
      "margins",
      "availability",
      "conditional_scores",
      "probabilities"
    )
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
})

test_that("the never-public score flag is gone", {
  # `return_event_scores` was introduced on the development branch after the
  # v1.7.0 tag and never reached a public release, so 2.0.0 removes it outright
  # rather than spending a deprecation cycle on it.
  expect_snapshot(
    set_algorithm_newton(return_event_scores = TRUE),
    error = TRUE
  )
})

test_that("legacy flags map onto diagnostics and the derived flags", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # A legacy flag rebuilds the whole vector from the flag view, so the scores
  # the default would have stored drop out with it.
  opt <- set_algorithm_newton(return_probabilities = TRUE)
  expect_equal(opt$diagnostics, c("loglik", "probabilities"))
  expect_false(opt$return_event_scores)
  expect_true(opt$return_probabilities)

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
})

test_that("the control object does not track how scores were requested", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # `scores_explicit` existed only so the gather backend could abort on an
  # explicit score request and silently drop a default-sourced one. Gather
  # computes per-event scores now, both branches are gone, and a request is
  # honored or refused rather than partially honored -- so the distinction has
  # nothing left to decide.
  expect_null(
    set_algorithm_newton(diagnostics = c("loglik", "scores"))$scores_explicit
  )
  expect_null(
    set_algorithm_newton(return_probabilities = TRUE)$scores_explicit
  )
})

test_that("mixing diagnostics with a legacy flag aborts", {
  expect_snapshot(
    set_algorithm_newton(diagnostics = "loglik", return_probabilities = TRUE),
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
    c("goldfishPrepCtrl", "list"),
    exact = TRUE
  )
  expect_true(is.list(default_opts))
  # The control objects print through methods registered on their classes, so
  # each is checked for dispatch as well as for its class vector: a rename that
  # moves the class but not the method leaves both inherits() checks passing
  # and the object printing as a bare list.
  expect_output(print(default_opts), "Preprocessing Control Options")
  expect_output(
    print(set_algorithm_newton()),
    "Estimation Algorithm Options"
  )
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
    c("goldfishPrepCtrl", "list"),
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

# The (backend, primitive) capability table. Every shipped cell is supported --
# each primitive is a reduction of the same per-event weight vector, so it is
# computable wherever that vector is formed -- which is why the refusal path
# below is exercised through a synthetic table. The mechanism has to keep
# working for the next primitive that is not universal on day one; that is the
# reason the table exists rather than being collapsed into a constant.
test_that("every primitive is available on every backend", {
  for (primitive in DIAGNOSTIC_PRIMITIVES) {
    expect_setequal(DIAGNOSTIC_BACKEND_SUPPORT[[primitive]], BACKEND_VALUES)
  }
})

test_that("an unsupported primitive aborts naming the backends that produce it", {
  local_cli_context()
  support <- list(loglik = BACKEND_VALUES, scores = c("cpp", "r"))
  expect_snapshot(
    check_diagnostic_support(
      c("loglik", "scores"),
      "gather",
      support = support
    ),
    error = TRUE
  )
  expect_no_error(
    check_diagnostic_support(c("loglik", "scores"), "cpp", support = support)
  )
})

test_that("the support verdict does not depend on request order", {
  # The ordering artifact this change exists to remove was exactly a verdict
  # that moved with what else was requested alongside, so the check reports the
  # first offender in the vocabulary's order rather than the caller's.
  support <- list(ranks = "cpp", margins = "r")
  message_for <- function(diagnostics) {
    tryCatch(
      check_diagnostic_support(diagnostics, "gather", support = support),
      error = conditionMessage
    )
  }
  expect_equal(
    message_for(c("ranks", "margins")),
    message_for(c("margins", "ranks"))
  )
})
