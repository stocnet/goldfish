# The goldfishDynesFit result class: the covariance pass at the returned
# estimate, the flat coef()/vcov() over the free coefficients, the per-process
# coef_layout() with completed fids as fixed rows, and the verdicts the
# fit-class-hierarchy capability records for the class (override or refuse on
# every generic, inherit on none). The loop runs over the analytic evaluator
# (helper-abem_loop.R): each pooled sequence's data mu_i scores as
# -sum((theta - mu_i)^2) / 2, so the score is mu_i - theta and every Fisher
# information the identity, and both covariances are closed-form.

# accept 0 -> 0.5, then stop on a proposal that does not move.
local_accept_then_stop <- function(env = parent.frame()) {
  proposals <- list(function(theta) theta + 0.5, function(theta) theta + 1e-6)
  pass <- 0L
  local_scripted_optimizer(
    function(theta) {
      pass <<- pass + 1L
      proposals[[min(pass, 2L)]](theta)
    },
    env = env
  )
}

fit_dynes_control <- function(...) {
  args <- list(
    n_sequences = 4L,
    seed = 42L,
    augmenter = set_augmenter_options(routine = "random")
  )
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(set_algorithm_em, args)
}

fit_dynes_toy <- function(...) {
  suppressWarnings(estimate_dynes(
    dynes_pinned_raw_spec(),
    fit_dynes_control(...)
  ))
}

# --- Class and flat coefficients ------------------------------------------

test_that("estimate_dynes returns a goldfishDynesFit under the fit parent", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()

  expect_s3_class(fit, c("goldfishDynesFit", "goldfishBaseFit"), exact = TRUE)
  expect_true(fit$converged)
})

test_that("coef() is flat over the free coefficients, by composite label", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()

  expect_identical(
    names(coef(fit)),
    c(
      "friendship › choice: inertia/friendship",
      "calls › rate: Intercept",
      "calls › rate: indeg/friendship"
    )
  )
  expect_equal(unname(coef(fit)), fit$parameters, ignore_attr = TRUE)
})

# --- Covariance pass ------------------------------------------------------

test_that("vcov() is populated when max_iterations ends the run", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 0.3)
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy(max_iterations = 2L)

  expect_false(fit$converged)
  v <- vcov(fit)
  expect_false(is.null(v))
  expect_identical(dim(v), c(3L, 3L))
  expect_identical(dimnames(v), list(names(coef(fit)), names(coef(fit))))
  expect_true(all(is.finite(v)))
  expect_true(all(is.finite(fit$mc_covariance)))
})

# Four sequences whose data spread by well under one per coordinate around a
# mean near 1, so a step up from 0 ascends, and the weighted score variance
# stays below the identity information in every direction: the observed
# information is positive definite. The log proposal densities differ, so
# the importance weights are unequal but none collapses.
louis_stub_augmenter <- function() {
  new_stub_augmenter(
    list(
      c(1.2, 0.7, 1.5),
      c(0.6, 1.6, 1.1),
      c(1.7, 1.2, 0.4),
      c(0.9, 0.5, 1.3)
    ),
    log_proposal = c(-1.2, -1.5, -1.3, -1.4)
  )
}

# The Louis forms over the analytic evaluator at `theta` with weights `w`:
# each sequence's score is mu_i - theta and its information the identity, so
# I_c = sum(w) I and the missing information is the weighted score variance.
analytic_louis <- function(mu, theta, w) {
  scores <- t(vapply(mu, function(m) m - theta, numeric(length(theta))))
  complete <- diag(length(theta)) * sum(w)
  mean_score <- colSums(scores * w)
  missing <- crossprod(scores * sqrt(w)) - tcrossprod(mean_score)
  observed <- complete - missing
  bread <- solve(observed)
  centered <- sweep(scores, 2L, mean_score)
  list(
    complete = complete,
    missing = missing,
    observed = observed,
    fraction = max(eigen(solve(complete, missing))$values),
    covariance = bread,
    mc_covariance = bread %*% crossprod(centered * w) %*% bread
  )
}

pool_data <- function(fit) {
  lapply(fit$pool$sequences, function(s) s$data)
}

test_that("the standard and MC covariances match the hand-computed forms", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  fit <- fit_dynes_toy()

  louis <- analytic_louis(pool_data(fit), unname(coef(fit)), fit$weights)
  # The fixture's premise: the paths differ, and I_obs stays positive definite.
  expect_gt(max(abs(louis$observed - louis$complete)), 0.01)
  expect_gt(min(eigen(louis$observed, symmetric = TRUE)$values), 0)
  expect_equal(unname(vcov(fit)), louis$covariance, tolerance = 1e-10)
  expect_equal(
    unname(fit$mc_covariance),
    louis$mc_covariance,
    tolerance = 1e-10
  )
  expect_identical(dimnames(fit$mc_covariance), dimnames(vcov(fit)))
  expect_identical(fit$covariance_status, "ok")
})

test_that("equal scores give the complete-data inverse and no MC error", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_stub_augmenter(rep(list(c(1.2, 0.7, 1.5)), 4L)))

  fit <- fit_dynes_toy()

  expect_equal(unname(vcov(fit)), diag(3), tolerance = 1e-10)
  expect_equal(unname(fit$mc_covariance), matrix(0, 3, 3), tolerance = 1e-10)
})

test_that("missing information widens every standard error", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  fit <- fit_dynes_toy()

  louis <- analytic_louis(pool_data(fit), unname(coef(fit)), fit$weights)
  complete_se <- sqrt(diag(solve(louis$complete)))
  expect_true(all(sqrt(diag(vcov(fit))) >= complete_se))
})

# One accepted pass from 0 to 0.3, after which max_iterations ends the run:
# it returns directly after that pass, so the fit's weights are those the
# pass's standard errors were formed with.
fit_one_accepted_pass <- function() {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 0.3)
  local_dynes_augmenter(louis_stub_augmenter())
  fit_dynes_toy(max_iterations = 1L, em_trace_se = TRUE)
}

test_that("the em_trace standard errors are the Louis ones at the proposal", {
  fit <- fit_one_accepted_pass()

  row <- fit$em_trace[fit$em_trace$accepted, ]
  expect_identical(nrow(row), 1L)
  theta <- row$theta[[1]]
  mu <- pool_data(fit)
  w <- compute_pool_weights(
    vapply(mu, analytic_loglik, numeric(1), theta = theta),
    pool_log_proposal(pool_references(fit$pool))
  )
  louis <- analytic_louis(mu, theta, w)
  expect_equal(
    unname(row$se[[1]]),
    sqrt(diag(louis$covariance)),
    tolerance = 1e-10
  )
})

test_that("the last accepted pass's standard errors are the fit's", {
  fit <- fit_one_accepted_pass()

  # The premise: the run returned directly after its one accepted pass.
  expect_false(fit$converged)
  last <- fit$em_trace[nrow(fit$em_trace), ]
  expect_true(last$accepted)
  expect_equal(unname(coef(fit)), last$theta[[1]], ignore_attr = TRUE)
  expect_equal(
    unname(last$se[[1]]),
    unname(sqrt(diag(vcov(fit)))),
    tolerance = 1e-10
  )
})

# A count panel with exogenous effects only: every path has the same
# likelihood, so I_obs = I_c and the standard errors are those of the Poisson
# closed form. The 10% bound absorbs evaluating the information at the SGD's
# estimate rather than at the glm MLE.
test_that("standard errors match the closed-form count-panel oracle", {
  skip_on_cran()
  oracle <- suppressMessages(oracle_count_panel())

  fit <- suppressMessages(suppressWarnings(estimate_dynes(
    oracle$spec,
    set_algorithm_em(
      n_sequences = 20L,
      max_iterations = 30L,
      seed = 1L,
      augmenter = set_augmenter_options(routine = "random")
    ),
    wave_times = oracle$wave_times
  )))

  se <- sqrt(diag(vcov(fit)))[names(oracle$mle$se)]
  expect_equal(se, oracle$mle$se, tolerance = 0.1)
})

# Paths of dynes_multiwave_spec() with an endogenous friendship choice, whose
# friendship events fall at different times, so their scores differ.
louis_hessian_pool <- function(spec) {
  path <- function(times) {
    data <- dynes_multiwave_data()
    data$ties$time[data$ties$layer == "friendship"] <- times
    data$ties <- data$ties[order(data$ties$time), ]
    data
  }
  paths <- list(
    path(c(1, 2, 3, 4)),
    path(c(1, 1.5, 3, 4)),
    path(c(0.5, 1.5, 2.5, 4.5))
  )
  log_proposal <- c(-1, -0.4, -1.7)
  build_sequence_pool(
    decompose_joint_spec(spec),
    Map(
      function(data, q) {
        list(data = data, reference = new_reference_record(c(0, 0, 0), -1, q))
      },
      paths,
      log_proposal
    ),
    theta_length = 3L
  )
}

# With untransformed importance weights over a fixed pool, the negative
# Hessian of log sum_i exp(l_i(theta) - log q_i) is Louis's I_obs exactly,
# given J_i = -Hessian(l_i). An independent check on the sign and centering
# of the missing-information term, which a hand-computed form would share.
test_that("the observed information is the pool log-likelihood Hessian", {
  spec <- dynes_multiwave_spec(~ indeg + offset(recip, coef = -0.5))
  pool <- louis_hessian_pool(spec)
  log_proposal <- pool_log_proposal(pool_references(pool))
  theta <- c(0.3, -1.2, 0.4)
  pool_loglik <- function(theta) {
    l <- evaluate_sequence_pool(pool, theta, what = "loglik")$loglik -
      log_proposal
    max(l) + log(sum(exp(l - max(l))))
  }
  h <- 1e-4
  p <- length(theta)
  hessian <- matrix(0, p, p)
  for (j in seq_len(p)) {
    for (k in seq_len(p)) {
      shifted <- function(a, b) {
        at <- theta
        at[j] <- at[j] + a * h
        at[k] <- at[k] + b * h
        pool_loglik(at)
      }
      hessian[j, k] <- (shifted(1, 1) -
        shifted(1, -1) -
        shifted(-1, 1) +
        shifted(-1, -1)) /
        (4 * h^2)
    }
  }

  res <- evaluate_sequence_pool(pool, theta)
  weights <- compute_pool_weights(
    res$loglik,
    log_proposal,
    weighting = "importance",
    transformation = "identity"
  )
  # The premise: the paths carry different scores.
  expect_gt(max(apply(res$score, 2L, stats::sd)), 0.1)
  louis <- louis_covariances(res$score, res$fisher, res$degenerate, weights)
  expect_equal(louis$observed_information, -hessian, tolerance = 1e-5)
})

# --- Non-positive-definite observed information ---------------------------

# The louis_stub_augmenter() pool with its first coordinate spread by about
# four around a mean near 1: at the estimate the weighted score variance
# exceeds the identity information in that direction, so I_obs is indefinite
# while I_c stays invertible. A wider spread would collapse the importance
# weights onto one sequence and stall the ascent.
not_pd_stub_augmenter <- function() {
  new_stub_augmenter(
    list(
      c(-1.5, 0.7, 1.5),
      c(3.4, 1.6, 1.1),
      c(-0.8, 1.2, 0.4),
      c(3.0, 0.5, 1.3)
    ),
    log_proposal = c(-1.2, -1.5, -1.3, -1.4)
  )
}

# One run over the analytic evaluator with every warning muffled and the two
# covariance warnings kept, by class.
covariance_warning_run <- function(...) {
  kept <- list(not_pd = list(), singular = list())
  fit <- withCallingHandlers(
    estimate_dynes(dynes_pinned_raw_spec(), fit_dynes_control(...)),
    goldfish_dynes_observed_information_not_pd = function(w) {
      kept$not_pd[[length(kept$not_pd) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    goldfish_dynes_singular_information = function(w) {
      kept$singular[[length(kept$singular) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  c(list(fit = fit), kept)
}

expect_not_pd_fit <- function(run) {
  fit <- run$fit
  louis <- analytic_louis(pool_data(fit), unname(coef(fit)), fit$weights)
  # The fixture's premise: I_c is invertible and I_obs is indefinite.
  expect_gt(min(eigen(louis$complete, symmetric = TRUE)$values), 0)
  expect_lt(min(eigen(louis$observed, symmetric = TRUE)$values), 0)
  expect_length(run$not_pd, 1L)
  expect_length(run$singular, 0L)
  expect_false(anyNA(fit$parameters))
  expect_true(all(is.na(vcov(fit))))
  expect_true(all(is.na(fit$mc_covariance)))
  expect_identical(fit$covariance_status, "not_pd")
}

test_that("an observed information that is not positive definite warns once", {
  local_reproducible_output(width = 80)
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(not_pd_stub_augmenter())

  # Per-pass standard errors are on, so a warning per pass would show here.
  run <- covariance_warning_run(em_trace_se = TRUE)

  expect_true(run$fit$converged)
  expect_not_pd_fit(run)
  pass_se <- unlist(run$fit$em_trace$se)
  expect_gt(length(pass_se), 0L)
  expect_true(all(is.na(pass_se)))
  message <- conditionMessage(run$not_pd[[1]])
  expect_no_match(message, "maximum")
  expect_snapshot(cat(message))
})

test_that("a non-converged fit is told the estimate may not be a maximum", {
  local_reproducible_output(width = 80)
  local_analytic_evaluator()
  # The same accepted step as local_accept_then_stop(), after which
  # max_iterations ends the run at the same indefinite estimate.
  local_scripted_optimizer(function(theta) theta + 0.5)
  local_dynes_augmenter(not_pd_stub_augmenter())

  run <- covariance_warning_run(max_iterations = 1L)

  expect_false(run$fit$converged)
  expect_not_pd_fit(run)
  message <- conditionMessage(run$not_pd[[1]])
  expect_match(message, "may not be a maximum")
  expect_snapshot(cat(message))
})

# --- Covariance diagnostics -------------------------------------------------

test_that("an ok fit reports the largest fraction of missing information", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  fit <- fit_dynes_toy()

  louis <- analytic_louis(pool_data(fit), unname(coef(fit)), fit$weights)
  expect_identical(fit$covariance_status, "ok")
  expect_equal(fit$fraction_missing, louis$fraction, tolerance = 1e-10)
  expect_gte(fit$fraction_missing, 0)
  expect_lt(fit$fraction_missing, 1)
})

test_that("a not-PD fit's fraction of missing information is at least one", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(not_pd_stub_augmenter())

  run <- covariance_warning_run()

  fit <- run$fit
  louis <- analytic_louis(pool_data(fit), unname(coef(fit)), fit$weights)
  expect_identical(fit$covariance_status, "not_pd")
  expect_equal(fit$fraction_missing, louis$fraction, tolerance = 1e-10)
  expect_gte(fit$fraction_missing, 1)
  expect_equal(fit$covariance_ess, 1 / sum(fit$weights^2), tolerance = 1e-10)
})

test_that("covariance_ess is the effective size of the covariance weights", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  ok <- fit_dynes_toy()

  expect_equal(ok$covariance_ess, 1 / sum(ok$weights^2), tolerance = 1e-10)
  expect_gt(ok$covariance_ess, 1)
  expect_lt(ok$covariance_ess, length(ok$weights))
})

test_that("covariance_ess is NA under uniform weighting", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  fit <- fit_dynes_toy(
    augmenter = set_augmenter_options(routine = "mcmc"),
    weights = set_weights_options(weighting = "uniform", refresh = TRUE)
  )

  expect_identical(fit$covariance_ess, NA_real_)
})

test_that("glance() carries the covariance diagnostics", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  fit <- fit_dynes_toy()
  glanced <- glance(fit)

  expect_named(
    glanced,
    c(
      "converged",
      "iterations",
      "n_sequences",
      "df",
      "covariance_status",
      "fraction_missing",
      "covariance_ess"
    )
  )
  expect_identical(glanced$covariance_status, "ok")
  expect_identical(glanced$fraction_missing, fit$fraction_missing)
  expect_identical(glanced$covariance_ess, fit$covariance_ess)
})

# A real fit with its diagnostics pinned by hand, as the print test pins its
# numbers: the snapshot records the wording, not floating-point noise.
summary_with_status <- function(status, fraction, ess) {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  fit <- fit_dynes_toy()
  fit$parameters[] <- c(0.6, -1.25, 0.3)
  fit$covariance[] <- diag(c(0.04, 0.09, 0.01))
  fit$mc_covariance[] <- diag(c(0.0004, 0.0009, 0.0001))
  fit$covariance_status <- status
  fit$fraction_missing <- fraction
  fit$covariance_ess <- ess
  summary(fit)
}

test_that("summary() reports the diagnostics for each covariance status", {
  local_abem_cli_context()

  ok <- summary_with_status("ok", 0.4321, 17.5)
  expect_identical(ok$fraction_missing, 0.4321)
  expect_identical(ok$covariance_ess, 17.5)
  expect_snapshot(print(ok))

  not_pd <- summary_with_status("not_pd", 1.37, 2.5)
  expect_identical(not_pd$covariance_status, "not_pd")
  expect_snapshot(print(not_pd))

  singular <- summary_with_status("singular", NA_real_, 9.25)
  expect_identical(singular$covariance_status, "singular")
  expect_snapshot(print(singular))

  dropped <- summary_with_status("pool_dropped", NA_real_, NA_real_)
  expect_identical(dropped$covariance_status, "pool_dropped")
  expect_snapshot(print(dropped))
})

# --- Information decomposition ----------------------------------------------

test_that("the information decomposition is an exact subtraction", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  fit <- fit_dynes_toy()

  louis <- analytic_louis(pool_data(fit), unname(coef(fit)), fit$weights)
  information <- fit$information
  expect_named(information, c("complete", "missing", "observed"))
  expect_equal(unname(information$complete), louis$complete, tolerance = 1e-10)
  expect_equal(unname(information$missing), louis$missing, tolerance = 1e-10)
  expect_equal(
    information$observed,
    information$complete - information$missing,
    tolerance = 1e-10
  )
  labels <- list(names(coef(fit)), names(coef(fit)))
  for (part in information) {
    expect_identical(dimnames(part), labels)
  }
})

test_that("vcov() inverts the requested information", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  fit <- fit_dynes_toy()

  observed <- vcov(fit, information = "observed")
  complete <- vcov(fit, information = "complete_data")
  expect_identical(vcov(fit), observed)
  expect_equal(observed, solve(fit$information$observed), tolerance = 1e-10)
  expect_equal(complete, solve(fit$information$complete), tolerance = 1e-10)
  expect_identical(dimnames(complete), dimnames(observed))
  expect_true(all(diag(observed) >= diag(complete)))
})

test_that("the complete-data inverse survives a not-PD observed information", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(not_pd_stub_augmenter())

  fit <- covariance_warning_run()$fit

  expect_identical(fit$covariance_status, "not_pd")
  expect_no_warning(complete <- vcov(fit, information = "complete_data"))
  expect_false(anyNA(complete))
  expect_equal(complete, solve(fit$information$complete), tolerance = 1e-10)
  expect_true(all(is.na(vcov(fit))))
  expect_named(fit$information, c("complete", "missing", "observed"))
  for (part in fit$information) {
    expect_true(all(is.finite(part)))
  }
})

test_that("an unknown information type is rejected", {
  local_abem_cli_context()
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(louis_stub_augmenter())

  fit <- fit_dynes_toy()

  expect_snapshot(vcov(fit, information = "complete"), error = TRUE)
})

test_that("only vcov() takes an information argument", {
  for (method in list(
    tidy.goldfishDynesFit,
    summary.goldfishDynesFit,
    coef_layout.goldfishDynesFit
  )) {
    expect_identical(names(formals(method))[-1], "...")
  }
})

# --- Singular pooled information ------------------------------------------

no_attribution <- list(coefficients = integer(0), groups = list())

test_that("singular_attribution() names only the non-positive diagonals", {
  collinear <- matrix(c(1, 1, 1, 1), 2L)
  expect_identical(singular_attribution(collinear), no_attribution)

  expect_identical(
    singular_attribution(matrix(0, 3L, 3L)),
    list(coefficients = 1:3, groups = list())
  )
  expect_identical(
    singular_attribution(diag(c(2, 0, 1, -1e-17)))$coefficients,
    c(2L, 4L)
  )
})

test_that("singular_attribution() names nothing for a rounding residue", {
  # A statistic constant at c within the alternatives: the uncentered form
  # sum_j p_j c^2 - (sum_j p_j c)^2 leaves a residue of order eps * c^2.
  utility <- c(0.3, -1.2, 0.8) * 0.05
  p <- exp(utility) / sum(exp(utility))
  c_stat <- 3
  residue <- sum(p * c_stat^2) - sum(p * c_stat)^2
  expect_gt(residue, 0)
  expect_lt(residue, 1e-12)
  information <- diag(c(1, residue))

  expect_identical(singular_attribution(information), no_attribution)
})

singular_labels <- c(
  "calls › rate: Intercept",
  "calls › choice: inertia/calls",
  "friendship › choice: indeg/friendship"
)

test_that("the singular-information warning for a coefficient at its start", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    warn_singular_information(
      list(coefficients = 2L, groups = list()),
      singular_labels,
      start = c(0.1, 0, 0),
      value = c(0.4, 0, 0.2)
    )
  )
})

test_that("the singular-information warning for a coefficient that moved", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    warn_singular_information(
      list(coefficients = c(2L, 3L), groups = list()),
      singular_labels,
      start = c(0.1, 0, 0),
      value = c(0.4, 0, 0.2)
    )
  )
})

test_that("the singular-information warning naming no coefficient", {
  local_reproducible_output(width = 80)
  expect_snapshot(
    warn_singular_information(
      no_attribution,
      singular_labels,
      start = c(0.1, 0, 0),
      value = c(0.4, 0, 0.2)
    )
  )
  expect_warning(
    warn_singular_information(
      no_attribution,
      singular_labels,
      start = c(0.1, 0, 0),
      value = c(0.4, 0, 0.2)
    ),
    class = "goldfish_dynes_singular_information"
  )
})

# One estimate_dynes() run from wave time 0 with every warning muffled: the
# singular- and observed-information warnings kept, theta0, and the
# per-sequence information matrices of the last covariance pass (the fit's
# own, at the estimate).
singular_information_run <- function(spec, control) {
  seen <- new.env(parent = emptyenv())
  seen$warnings <- list()
  seen$not_pd <- list()
  real_initial_theta <- dynes_initial_theta
  real_louis <- louis_covariances
  local_mocked_bindings(
    dynes_initial_theta = function(...) {
      seen$start <- real_initial_theta(...)
      seen$start
    },
    louis_covariances = function(score, fisher, degenerate, weights) {
      seen$fisher <- fisher
      real_louis(score, fisher, degenerate, weights)
    }
  )
  seen$fit <- withCallingHandlers(
    estimate_dynes(
      spec,
      control,
      control_prep = set_preprocessing(start_time = 0)
    ),
    goldfish_dynes_singular_information = function(w) {
      seen$warnings[[length(seen$warnings) + 1L]] <- w
      invokeRestart("muffleWarning")
    },
    goldfish_dynes_observed_information_not_pd = function(w) {
      seen$not_pd[[length(seen$not_pd) + 1L]] <- w
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  seen
}

test_that("an effect with no variation on any path is named at its start", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  # Per-pass standard errors are on, so a warning per pass would show here.
  control <- fit_dynes_control(
    n_sequences = 6L,
    max_iterations = 3L,
    seed = 7L,
    em_trace_se = TRUE
  )

  run <- singular_information_run(recipe_state_spec(), control)

  fit <- run$fit
  expect_length(run$warnings, 1L)
  expect_length(run$not_pd, 0L)
  expect_identical(fit$covariance_status, "singular")
  expect_true(all(is.na(fit$covariance)))
  expect_true(all(is.na(fit$mc_covariance)))
  expect_true(all(is.na(unlist(fit$em_trace$se))))
  expect_false(anyNA(fit$parameters))
  inertia <- match("calls › choice: inertia/calls", names(fit$parameters))
  expect_identical(fit$parameters[[inertia]], run$start[[inertia]])
  expect_snapshot(cat(conditionMessage(run$warnings[[1]])))
})

test_that("a singular fit has no fraction but keeps its covariance ESS", {
  skip_on_cran()
  control <- fit_dynes_control(
    n_sequences = 6L,
    max_iterations = 3L,
    seed = 7L
  )

  fit <- singular_information_run(recipe_state_spec(), control)$fit

  expect_identical(fit$covariance_status, "singular")
  expect_identical(fit$fraction_missing, NA_real_)
  expect_equal(
    fit$covariance_ess,
    1 / sum(fit$weights^2),
    tolerance = 1e-10
  )
})

test_that("a singular fit keeps its information but inverts neither type", {
  skip_on_cran()
  control <- fit_dynes_control(
    n_sequences = 6L,
    max_iterations = 3L,
    seed = 7L
  )

  fit <- singular_information_run(recipe_state_spec(), control)$fit

  expect_identical(fit$covariance_status, "singular")
  expect_named(fit$information, c("complete", "missing", "observed"))
  for (part in fit$information) {
    expect_true(all(is.finite(part)))
  }
  expect_true(all(is.na(vcov(fit))))
  expect_true(all(is.na(vcov(fit, information = "complete_data"))))
})

test_that("singular sequences in an invertible pooled information do not warn", {
  skip_on_cran()
  control <- fit_dynes_control(
    n_sequences = 6L,
    max_iterations = 3L,
    seed = 7L,
    augmenter = set_augmenter_options(routine = "sim")
  )

  run <- singular_information_run(early_calls_spec(), control)

  # The fixture's premise: some final-pool paths carry no calls-choice
  # information, so their own information matrix is singular.
  slot <- match("calls › choice: indeg/friendship", names(run$fit$parameters))
  calls_choice <- vapply(run$fisher, function(j) j[slot, slot], numeric(1))
  expect_contains(calls_choice, 0)
  expect_contains(calls_choice > 0, TRUE)
  expect_length(run$warnings, 0L)
  expect_length(run$not_pd, 0L)
  expect_identical(run$fit$covariance_status, "ok")
  expect_true(all(is.finite(run$fit$covariance)))
  expect_true(all(is.finite(run$fit$mc_covariance)))
})

# --- Completed fids -------------------------------------------------------

test_that("a completed pinned rate is a fixed layout row, not a coefficient", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  layout <- coef_layout(fit)

  pinned_fid <- names(fit$spec$completed_rates)
  expect_length(pinned_fid, 1L)
  row <- layout[layout$fid == as.integer(pinned_fid), ]
  expect_identical(nrow(row), 1L)
  expect_true(row$fixed)
  # One plateau per wave period; the layout row shows the first.
  expect_equal(
    row$value,
    fit$spec$completed_rates[[pinned_fid]]$intercept[[1]]
  )
  expect_true(is.na(row$se))
  expect_true(is.na(row$mc_se))

  expect_false(any(startsWith(names(coef(fit)), row$process)))
  expect_false(any(startsWith(rownames(vcov(fit)), row$process)))

  free <- layout[!layout$fixed, ]
  expect_identical(paste0(free$process, ": ", free$name), names(coef(fit)))
  expect_equal(free$value, unname(coef(fit)))
  expect_equal(free$se, unname(sqrt(diag(vcov(fit)))))
  expect_equal(free$mc_se, unname(sqrt(diag(fit$mc_covariance))))
})

# --- Recorded verdicts ----------------------------------------------------

test_that("logLik, AIC and BIC refuse on a Monte-Carlo fit", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  local_abem_cli_context()

  fit <- fit_dynes_toy()

  expect_error(logLik(fit), class = "goldfish_generic_refused")
  expect_error(stats::AIC(fit), class = "goldfish_generic_refused")
  expect_error(stats::BIC(fit), class = "goldfish_generic_refused")
  expect_snapshot(logLik(fit), error = TRUE)
})

test_that("the estimation-internals generics refuse, naming the class", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  refused <- list(
    model_terms = model_terms,
    augment = generics::augment,
    fitted = stats::fitted,
    predict = stats::predict,
    residuals = stats::residuals,
    evaluate_model = evaluate_model,
    test_gof = test_gof,
    test_parameter = test_parameter,
    test_time = test_time,
    diagnose_outliers = diagnose_outliers,
    diagnose_changepoints = diagnose_changepoints,
    diagnose_onset = diagnose_onset,
    margin_table = margin_table
  )

  for (generic in names(refused)) {
    err <- expect_error(
      refused[[generic]](fit),
      class = "goldfish_generic_refused"
    )
    expect_match(conditionMessage(err), "goldfishDynesFit", info = generic)
  }
})

test_that("times_of answers from the held specification", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()

  expect_identical(times_of(fit), times_of(dynes_pinned_raw_spec()))
})

test_that("simulate() runs the completed specification at the estimates", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  params <- dynes_fit_parameters(fit)

  expect_s3_class(params, "goldfishParams")
  layout <- coef_layout(params)
  free <- layout[!layout$fixed, ]
  expect_equal(
    free$value,
    unname(coef(fit)[paste0(free$process, ": ", free$name)])
  )

  # Re-completing the fit's spec keeps its wave pins and announces them.
  expect_warning(
    sim <- simulate(fit, seed = 1, n_events = 3L),
    class = "goldfish_pinned_rate_warning"
  )
  expected <- suppressWarnings(simulate(
    fit$spec,
    seed = 1,
    coef = params,
    n_events = 3L
  ))
  expect_identical(sim$events, expected$events)
})

test_that("simulate() on a fit pins per wave, as estimation did", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  fit <- fit_dynes_toy()
  simulated <- NULL
  local_mocked_bindings(
    resolve_coef_provider = function(coef, completed, ...) {
      simulated <<- completed
      rlang::abort("stop", class = "provider_seen")
    }
  )

  expect_error(
    suppressWarnings(simulate(fit, seed = 1, n_events = 3L)),
    class = "provider_seen"
  )
  expect_identical(simulated$completed_rates, fit$spec$completed_rates)
  waves <- panel_wave_grid(fit$authored_spec)
  for (pin in simulated$completed_rates) {
    expect_length(pin$intercept, length(waves) - 1L)
  }
})

test_that("compute_lik_seq() resolves an authored spec's grid as the fit did", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())
  fit <- suppressWarnings(estimate_dynes(
    dynes_varying_raw_spec(),
    fit_dynes_control()
  ))
  sequence <- withr::with_seed(
    3L,
    new_wave_grid_augmenter()$draw(1L, 1L, 0)[[1]]$data
  )
  theta <- coef(fit)

  authored <- suppressWarnings(
    compute_lik_seq(fit$authored_spec, sequence, theta)
  )
  completed <- suppressWarnings(compute_lik_seq(fit$spec, sequence, theta))

  expect_false(completed$degenerate)
  expect_equal(authored$loglik, completed$loglik, tolerance = 1e-10)
  expect_equal(authored$score, completed$score, tolerance = 1e-10)
})

# --- Display --------------------------------------------------------------

test_that("tidy() carries the standard and MC error per coefficient", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  tidied <- tidy(fit)

  expect_s3_class(tidied, "tbl_df")
  expect_identical(tidied$term, names(coef(fit)))
  expect_named(
    tidied,
    c(
      "process",
      "term",
      "estimate",
      "std.error",
      "mc.error",
      "statistic",
      "p.value"
    )
  )
  expect_equal(tidied$std.error, unname(sqrt(diag(vcov(fit)))))
  expect_equal(tidied$mc.error, unname(sqrt(diag(fit$mc_covariance))))
})

test_that("glance() reports the run and no information criterion", {
  local_analytic_evaluator()
  local_accept_then_stop()
  local_dynes_augmenter(new_keyed_stub_augmenter())

  fit <- fit_dynes_toy()
  glanced <- glance(fit)

  expect_named(
    glanced,
    c(
      "converged",
      "iterations",
      "n_sequences",
      "df",
      "covariance_status",
      "fraction_missing",
      "covariance_ess"
    )
  )
  expect_true(glanced$converged)
  expect_identical(glanced$df, 3L)
  expect_identical(glanced$n_sequences, length(fit$pool$sequences))
})

test_that("print and summary group per process and show both errors", {
  local_analytic_evaluator()
  local_scripted_optimizer(function(theta) theta + 0.3)
  local_dynes_augmenter(new_keyed_stub_augmenter())
  local_abem_cli_context()

  fit <- fit_dynes_toy(max_iterations = 2L)
  # The analytic fixture's numbers carry no meaning; pin them so the snapshot
  # records the layout rather than floating-point noise.
  fit$parameters[] <- c(0.6, -1.25, 0.3)
  fit$covariance[] <- diag(c(0.04, 0.09, 0.01))
  fit$mc_covariance[] <- diag(c(0.0004, 0.0009, 0.0001))
  fit$fraction_missing <- 0.25
  fit$covariance_ess <- 3.5

  expect_s3_class(summary(fit), "goldfishSummDynesFit")
  expect_snapshot({
    print(fit)
    summary(fit)
  })
})
