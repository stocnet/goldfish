test_that("preprocess init", {
  formulaTest <- depNetwork ~ outdeg(networkState, weighted = TRUE) +
    outdeg(networkExog, weighted = TRUE) +
    inertia +
    recip
  preproData <- estimate_wrapper(
    formulaTest,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  toCompare <- c(
    "parameters",
    "standard_errors",
    "log_likelihood",
    "final_score",
    "final_information_matrix",
    "convergence",
    "n_iterations",
    "n_events",
    "names",
    "formula",
    "model",
    "sub_model",
    "right_censored",
    "n_params"
  )
  expect_equal(
    estimate_wrapper(formulaTest, data = dataTest)[toCompare],
    estimate_wrapper(
      formulaTest,
      data = dataTest,
      preprocessed = preproData
    )[toCompare]
  )
  formulaTest <- depNetwork ~ 1 +
    outdeg(networkState, weighted = TRUE) +
    outdeg(networkExog, weighted = TRUE)
  preproData <- estimate_wrapper(
    formulaTest,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    preprocessing_only = TRUE
  )
  expect_equal(
    estimate_wrapper(formulaTest, data = dataTest, sub_model = "rate")[
      toCompare
    ],
    estimate_wrapper(
      formulaTest,
      data = dataTest,
      sub_model = "rate",
      preprocessed = preproData
    )[toCompare]
  )
})

test_that("probabilities guardrail warns with the estimated footprint", {
  prep <- list(
    active_sender_init = rep(TRUE, 10),
    active_dyad_init = rep(TRUE, 10),
    is_dependent = rep(1L, 100)
  )
  expect_snapshot(
    warn_probabilities_footprint(prep, list(risk_set = list(axis = "dyad")))
  )
  expect_snapshot(
    warn_probabilities_footprint(prep, list(risk_set = list(axis = "sender")))
  )
  expect_snapshot(
    warn_probabilities_footprint(
      prep,
      list(risk_set = list(axis = "receiver_given_sender"))
    )
  )
})

test_that("probabilities guardrail skips when dims are unavailable", {
  prep <- list(
    active_sender_init = logical(0),
    active_dyad_init = logical(0),
    is_dependent = integer(0)
  )
  expect_no_warning(
    warn_probabilities_footprint(prep, list(risk_set = list(axis = "dyad")))
  )
})

test_that("the fit records the backend that produced it", {
  fit_on <- function(backend, ...) {
    suppressWarnings(estimate_wrapper(
      depNetwork ~ inertia + recip,
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      control_algo = set_algorithm_newton(backend = backend, ...)
    ))
  }
  for (backend in BACKEND_VALUES) {
    expect_equal(fit_on(backend)$backend, backend)
  }
  # Requesting probabilities used to move the fit onto r; every backend now
  # produces them natively, so the chosen backend is the one that runs.
  expect_equal(fit_on("cpp", diagnostics = "probabilities")$backend, "cpp")
  # `opportunities_list` still redirects (a preprocessing restriction, not a
  # per-event primitive), and there the component must name what actually ran,
  # not what was asked for -- a diagnostic gating on it needs the backend that
  # made the numbers.
  opportunities <- rep(list(seq_len(nrow(actors_ex))), nrow(eventsIncrement))
  redirected <- suppressWarnings(estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(backend = "cpp"),
    control_prep = set_preprocessing(opportunities_list = opportunities)
  ))
  expect_equal(redirected$backend, "r")
})

test_that("a fit without a backend component still works", {
  # Objects fitted before 2.0.0 carry no `backend`; nothing in the package's own
  # post-estimation surface may error on its absence.
  fit <- suppressWarnings(estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(backend = "cpp")
  ))
  fit$backend <- NULL
  expect_null(fit$backend)
  expect_no_error(print(fit))
  expect_no_error(summary(fit))
  expect_no_error(coef(fit))
  expect_no_error(logLik(fit))
  expect_no_error(vcov(fit))
})

test_that("backends without an opportunity list redirect to r", {
  withr::local_options(cli.num_colors = 1L)
  local_reproducible_output()
  # One entry per dependent event; the redirect fires before the list is read,
  # but estimation continues on the r backend and consumes it.
  opportunities <- rep(list(seq_len(nrow(actors_ex))), nrow(eventsIncrement))
  expect_snapshot(invisible(estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(backend = "cpp"),
    control_prep = set_preprocessing(opportunities_list = opportunities)
  )))
})

test_that("estimation emits the probabilities guardrail once per call", {
  formulaTest <- depNetwork ~ inertia + recip
  expect_warning(
    estimate_wrapper(
      formulaTest,
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      control_algo = set_algorithm_newton(diagnostics = "probabilities")
    ),
    regexp = "per-event probabilities"
  )
})
