# The residual types that are not a stored score matrix: the two Schoenfeld
# forms, the compensator, the per-alternative response, and the per-actor and
# per-dyad martingale maps. Two of them span both tiers -- read off a stored
# primitive when the fit carries it, recomputed through one evaluation pass
# when it does not -- which is what most of these tests pin.

residual_control <- function(...) {
  set_algorithm_newton(
    diagnostics = c("loglik", "scores", "margins", ...)
  )
}

fit_rate <- function(diagnostics = "conditional_scores", ...) {
  estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = residual_control(diagnostics),
    ...
  )
}

fit_choice <- function(...) {
  estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    control_algo = residual_control(),
    ...
  )
}

test_that("cox_snell is the compensator, from stored components only", {
  fit <- fit_rate()

  residual <- residuals(fit, type = "cox_snell")
  expect_null(fit$preprocessed)
  expect_equal(residual, fit$intervals * fit$total_rate)
  # At the maximum the compensators total the dependent-event count: the time
  # intercept's own score equation.
  expect_equal(
    sum(residual),
    sum(!fit$right_censored_events),
    tolerance = 1e-4
  )
  # On a right-censored interval the compensator IS the whole log-likelihood
  # contribution, so there the identity is exact rather than asymptotic.
  censored <- fit$right_censored_events
  expect_equal(residual[censored], -fit$interval_log_lik[censored])
})

test_that("cox_snell says so where there is no compensator", {
  expect_snapshot(error = TRUE, residuals(fit_choice(), type = "cox_snell"))
})

test_that("cox_snell residuals are unit exponential under the model", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # A correctly specified null: every actor carries the same constant rate, so
  # the total rate is constant and the waiting times are exponential with it.
  # Then Dt * T is Exp(1) exactly, and the fitted intercept recovers T.
  set.seed(20260729)
  n_actors <- 8L
  n_events <- 600L
  total_rate <- n_actors * exp(-1.5)
  times <- cumsum(stats::rexp(n_events, rate = total_rate))
  events <- data.frame(
    time = times,
    sender = sprintf("A%d", sample.int(n_actors, n_events, replace = TRUE)),
    receiver = sprintf("A%d", sample.int(n_actors, n_events, replace = TRUE)),
    increment = 1,
    stringsAsFactors = FALSE
  )
  events <- events[events$sender != events$receiver, ]
  rownames(events) <- NULL
  actors <- make_nodes(data.frame(
    label = sprintf("A%d", seq_len(n_actors)),
    present = TRUE,
    stringsAsFactors = FALSE
  ))
  network <- make_network(nodes = actors, directed = TRUE)
  network <- link_events(network, events, nodes = actors)
  dependent <- make_dependent_events(
    events = events,
    nodes = actors,
    default_network = network
  )
  simulated <- make_data(dependent, network, actors)

  # `outdeg` carries a true coefficient of zero here, so the fitted model is
  # correctly specified and the compensators are Exp(1) up to estimation error.
  fit <- estimate_dynam(
    dependent ~ 1 + outdeg,
    sub_model = "rate",
    data = simulated,
    control_algo = set_algorithm_newton(diagnostics = "loglik")
  )
  residual <- residuals(fit, type = "cox_snell")

  expect_gt(stats::ks.test(residual, "pexp", rate = 1)$p.value, 0.01)
})

test_that("schoenfeld reads the conditional rows an exact-time fit stored", {
  fit <- fit_rate()

  expect_equal(residuals(fit, type = "schoenfeld"), fit$conditional_scores)
  # They are NOT the score rows: those carry the exposure term, and dropping it
  # is why these do not sum to zero at the maximum even for free parameters.
  expect_false(identical(
    residuals(fit, type = "schoenfeld"),
    residuals(fit, type = "score")
  ))
  expect_true(all(is.na(
    residuals(fit, type = "schoenfeld")[fit$right_censored_events, ]
  )))
})

test_that("schoenfeld is the score row on a multinomial sub-model", {
  fit <- fit_choice()

  expect_equal(residuals(fit, type = "schoenfeld"), fit$event_scores)
  # There the free-parameter columns DO sum to zero at the maximum: the
  # expected statistic is the risk-set probability-weighted mean already.
  expect_equal(
    unname(colSums(residuals(fit, type = "schoenfeld"))),
    rep(0, ncol(fit$event_scores)),
    tolerance = 1e-4
  )
})

test_that("schoenfeld recomputes when the fit stored no conditional rows", {
  stored <- fit_rate()
  plain <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton(),
    return_preprocessed = TRUE
  )

  expect_null(plain$conditional_scores)
  expect_equal(
    unname(residuals(plain, type = "schoenfeld")),
    unname(stored$conditional_scores)
  )
})

test_that("schoenfeld names both routes when neither is available", {
  fit <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton()
  )

  expect_null(fit$conditional_scores)
  expect_null(fit$preprocessed)
  expect_snapshot(error = TRUE, residuals(fit, type = "schoenfeld"))
})

test_that("scaled_schoenfeld centers on the estimate it scales toward", {
  skip_on_cran()
  data <- baselines_social_evolution_data()
  fit <- estimate_dynam(
    calls_dependent ~ inertia + recip + trans,
    sub_model = "choice",
    data = data,
    control_algo = residual_control()
  )

  scaled <- residuals(fit, type = "scaled_schoenfeld")
  expect_equal(dim(scaled), dim(fit$event_scores))
  expect_equal(colnames(scaled), colnames(fit$event_scores))
  # theta_hat + n I^-1 s_k averages back to theta_hat exactly to the extent the
  # rows sum to zero, which on a multinomial sub-model they do at the maximum.
  expect_equal(unname(colMeans(scaled)), unname(coef(fit)), tolerance = 1e-4)
  # The constant is the DEPENDENT-event count of this sub-model, not the
  # interval count and not another sub-model's.
  expect_equal(
    unname(scaled[1, ]),
    unname(coef(fit)) +
      sum(!fit$right_censored_events) *
        drop(
          solve(fit$final_information_matrix) %*%
            residuals(fit, type = "schoenfeld")[1, ]
        )
  )
})

test_that("response residuals are observed minus fitted per alternative", {
  fit <- fit_rate(return_preprocessed = TRUE)

  residual <- residuals(fit, type = "response")
  expect_length(residual, length(fit$right_censored_events))
  # Each event's probabilities total one over the risk set, so subtracting a
  # single observed indicator leaves a vector summing to zero.
  dependent <- which(!fit$right_censored_events)
  expect_equal(sum(residual[[dependent[1]]]), 0)
  # A right-censored interval realizes nothing, so the residual is minus the
  # fitted mass and totals -1.
  censored <- which(fit$right_censored_events)
  expect_equal(sum(residual[[censored[1]]]), -1)
})

test_that("martingale at the actor level is the margins' own difference", {
  fit <- fit_rate()

  martingale <- residuals(fit, type = "martingale")
  expect_equal(
    as.numeric(martingale),
    as.numeric(fit$margins$observed) - as.numeric(fit$margins$expected)
  )
  expect_equal(names(martingale), names(fit$margins$observed))
  # A difference is on neither scale, so the expected vector's marker does not
  # ride along through the subtraction.
  expect_null(attr(martingale, "scale"))
  expect_equal(attr(fit$margins$expected, "scale"), "expected_count")
  # A tie-oriented fit reports both sides, under the margins' own names.
  rem <- estimate_wrapper(
    depNetwork ~ 1 + inertia + recip,
    model = "REM",
    sub_model = "rate",
    data = dataTest,
    control_algo = residual_control()
  )
  expect_named(residuals(rem, type = "martingale"), c("sender", "receiver"))
})

test_that("martingale at the dyad level sums to the actor level", {
  fit <- fit_choice(return_preprocessed = TRUE)

  map <- residuals(fit, type = "martingale", level = "dyad")
  expect_equal(dim(map), c(nrow(actors_ex), nrow(actors_ex)))
  # The margins ARE this map's marginals, which is why it is not stored: a
  # choice sub-model marginalises over receivers.
  expect_equal(
    unname(colSums(map)),
    unname(residuals(fit, type = "martingale")),
    tolerance = 1e-8
  )
})

test_that("the dyad level says so where there is no second axis", {
  expect_snapshot(
    error = TRUE,
    residuals(fit_rate(), type = "martingale", level = "dyad")
  )
})
