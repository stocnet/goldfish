# The conditional (partial-likelihood) score rows of an exact-time sub-model --
# the Schoenfeld residuals. They complete the which/when split the `loglik`
# primitive already carries:
#
#   exact-time log-likelihood = conditional ("which") + timing ("when")
#   exact-time score row      = conditional score     + exposure term
#
# so the multinomial families need nothing: their likelihood is already
# conditional and their stored `event_scores` ARE these rows.

conditional_control <- function(backend = "cpp") {
  set_algorithm_newton(
    diagnostics = c("loglik", "scores", "conditional_scores"),
    backend = backend
  )
}

# A rate process read over an exogenous layer too, so its events open
# right-censored intervals -- the ones with no observed alternative to
# condition on, where the rows are NA by design.
fit_rate_conditional <- function(backend = "cpp") {
  estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = conditional_control(backend),
    return_preprocessed = TRUE
  )
}

test_that("the conditional rows drop the exposure term", {
  fit <- fit_rate_conditional()
  dependent <- !fit$right_censored_events
  compensator <- fit$preprocessed$intervals * fit$total_rate

  # The time intercept is the statistic that is 1 for every alternative, so on
  # its column the two rows are readable in closed form: the risk-set mean is
  # 1, the conditional row is 1 - 1 = 0, and the score row is 1 - Dt * T. That
  # is the exposure term, and the two rows coincide exactly where it is one --
  # where the interval's expected count is one event.
  expect_equal(
    unname(fit$conditional_scores[dependent, 1]),
    rep(0, sum(dependent))
  )
  expect_equal(
    unname(fit$event_scores[dependent, 1]),
    1 - compensator[dependent]
  )
  expect_false(any(abs(compensator[dependent] - 1) < 1e-8))
})

test_that("the conditional rows are the multinomial score rows", {
  exact <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = conditional_control()
  )
  # The conditional component of the exact-time likelihood IS the ordinal
  # likelihood, so its score rows are the ordinal sub-model's -- evaluated at
  # the same non-intercept coefficients, without re-estimating.
  ordinal <- estimate_wrapper(
    depNetwork ~ indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate_ordered",
    data = dataTest,
    control_algo = set_algorithm_newton(
      diagnostics = c("loglik", "scores"),
      initial_parameters = unname(coef(exact)[-1]),
      max_iterations = 0
    )
  )
  dependent <- !exact$right_censored_events

  expect_equal(
    unname(exact$conditional_scores[dependent, -1]),
    unname(ordinal$event_scores)
  )
  # A constant statistic cancels in the risk-set mean, so the time intercept's
  # conditional column is identically zero -- there is no ordinal counterpart
  # for it, and none is needed.
  expect_equal(
    unname(exact$conditional_scores[dependent, 1]),
    rep(0, sum(dependent))
  )
})

test_that("requesting the rows off exact-time is a silent no-op", {
  expect_no_warning(
    fit <- estimate_wrapper(
      depNetwork ~ inertia + recip,
      model = "DyNAM",
      sub_model = "choice",
      data = dataTest,
      control_algo = conditional_control()
    )
  )

  expect_null(fit$conditional_scores)
  # The stored score rows are unchanged, and they are the conditional rows: a
  # multinomial likelihood is already conditional.
  expect_equal(dim(fit$event_scores), c(fit$n_events, 2L))
})

test_that("censored intervals carry NA, dependent ones do not", {
  fit <- fit_rate_conditional()
  censored <- fit$right_censored_events

  expect_gt(sum(censored), 0)
  expect_true(all(is.na(fit$conditional_scores[censored, ])))
  expect_false(anyNA(fit$conditional_scores[!censored, ]))
  # `event_scores` is a perfectly good number on the same rows, which is what
  # makes this the first component whose by-design NAs differ from a sibling's.
  expect_false(anyNA(fit$event_scores))
})

test_that("the by-design NAs are not read as a numerical failure", {
  # Both NA guards -- the initial-parameters check and the Newton loop's step
  # acceptance -- read this list. Forgetting the component here rejects a valid
  # step as "Estimation not possible with initial parameters" on a model that
  # estimated fine before the flag existed.
  expect_true("conditional_scores" %in% diagnostic_components_with_na)

  pass <- list(
    logLikelihood = -10,
    conditional_scores = matrix(c(1, NA_real_), nrow = 2, ncol = 1)
  )
  expect_false(has_unexpected_na(pass))
  expect_true(has_unexpected_na(c(pass, list(fisher = NA_real_))))

  # And through a fit started away from zero, which is the branch the
  # initial-parameters guard actually gates.
  fit <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton(
      diagnostics = c("loglik", "scores", "conditional_scores"),
      initial_parameters = c(-3, 0.1, 0.1, 0.1)
    )
  )
  expect_true(fit$convergence$is_converged)
})

test_that("every backend produces the same rows", {
  rows <- lapply(
    c("cpp", "gather", "r"),
    function(backend) unname(fit_rate_conditional(backend)$conditional_scores)
  )

  expect_equal(rows[[2]], rows[[1]], tolerance = 1e-10)
  expect_equal(rows[[3]], rows[[1]], tolerance = 1e-10)
})

test_that("a fit that stored nothing can still be evaluated", {
  stored <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = conditional_control()
  )
  plain <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton(),
    return_preprocessed = TRUE
  )

  expect_null(plain$conditional_scores)
  evaluated <- evaluate_model(plain, return = "conditional_scores")
  expect_equal(
    unname(evaluated$conditional_scores),
    unname(stored$conditional_scores)
  )
})
