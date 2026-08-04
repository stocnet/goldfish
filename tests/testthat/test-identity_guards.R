# Three surfaces where something the caller asked for is absent, and the three
# do not get the same treatment, because the acts are not the same.
#
# Requesting `"conditional_scores"` at estimation is a *preference*: on a family
# whose likelihood is already conditional the score rows carry no exposure term
# and so ARE the conditional rows, nothing is missing, and the fit is complete.
# That is an identity, and it is announced once rather than left silent -- a
# silence does not read as an identity, it reads as nothing at all.
#
# Asking `evaluate_model()` FOR the same quantity is a *demand*, and handing back
# nothing under a name the caller supplied would be a lie, so it aborts, exactly
# as `"exposure"` already does.
#
# Reaching for a risk-set axis on a flavored container is neither: the container
# is the wrong object to ask, holding one fit per process.

# cli abort snapshots are pinned to a reproducible width/no-color context so the
# rendered bullets stay stable across machines.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

conditional_control <- function() {
  set_algorithm_newton(
    diagnostics = c("loglik", "scores", "conditional_scores")
  )
}

test_that("the conditional-scores identity is announced, not left silent", {
  local_cli_context()

  expect_snapshot(
    invisible(suppressWarnings(estimate_dynam(
      depNetwork ~ inertia,
      sub_model = "choice",
      data = dataTest,
      control_algo = conditional_control()
    )))
  )
})

test_that("the identity message is a message, not a warning", {
  # The property that distinguishes it from a warning, and the reason the
  # choice matters: a warning here would become an error for anyone running
  # with warnings-as-errors, and would surface in R CMD check for any example
  # requesting the full primitive set. The fit is correct, so there is nothing
  # to warn about.
  withr::local_options(warn = 2)

  expect_no_error(suppressWarnings(suppressMessages(estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    control_algo = conditional_control()
  ))))
})

test_that("the identity message fires only where the identity holds", {
  # Silent on the exact-time families, where the primitive is a real thing that
  # gets stored and is not derivable from the score rows.
  expect_no_message(estimate_dynam(
    depNetwork ~ 1 + indeg,
    sub_model = "rate",
    data = dataTest,
    control_algo = conditional_control()
  ))

  expect_message(
    suppressWarnings(estimate_dynam(
      depNetwork ~ inertia,
      sub_model = "choice_coordination",
      data = dataTest,
      control_algo = conditional_control()
    )),
    "already-conditional"
  )
})

test_that("the stored event scores are unchanged by the message", {
  # The message is the whole behaviour change: nothing about what the fit
  # carries moves.
  quiet <- suppressWarnings(suppressMessages(estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )))
  asked <- suppressWarnings(suppressMessages(estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    control_algo = conditional_control()
  )))

  expect_equal(asked$event_scores, quiet$event_scores)
  expect_equal(coef(asked), coef(quiet))
  # Nothing is stored under the requested name, which is the fact the message
  # exists to state.
  expect_null(asked$conditional_scores)
})

test_that("evaluate_model demands a value and so aborts", {
  # The deliberate asymmetry with the estimation-time request above.
  local_cli_context()
  fit <- suppressWarnings(suppressMessages(estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(diagnostics = c("loglik", "scores"))
  )))
  prep <- compute_statistics(
    depNetwork ~ inertia,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    output = "preprocessed"
  )

  expect_snapshot(
    error = TRUE,
    evaluate_model(fit, return = "conditional_scores", preprocessed = prep)
  )
  # The rows the caller wanted are reachable under the name that does define
  # them, which is what the abort points at.
  expect_no_error(evaluate_model(fit, return = "score", preprocessed = prep))
})

test_that("a flavored container is the wrong object to ask for a risk-set axis", {
  local_cli_context()
  data <- flavored_fixture_data()
  container <- suppressWarnings(estimate_dynam(make_specification(
    rate = list(creation ~ 1 + indeg, dissolution ~ 1 + indeg),
    model = "DyNAM",
    data = data
  )))

  expect_snapshot(error = TRUE, risk_set_axis(container))
  # The route the abort names resolves.
  expect_type(risk_set_axis(container$results[[1]]), "character")
})
