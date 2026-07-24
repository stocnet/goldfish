# In-pass diagnostic primitives (ranks, margins, total_rate) computed by the
# default_c engines. For the multinomial engines observed_rank is validated
# against ranks enumerated from the default R engine's per-event probabilities
# at the same MLE, and the parity tests at the end compare default_c ranks and
# margins to the default R engine evaluated at identical parameters; the
# exact-time engines get their own structural and Cox-Snell identity checks.

# Rank of the observed alternative among the risk set, enumerated from a fit
# that stored per-event probabilities. For a multinomial submodel
# exp(intervalLogL) is the observed alternative's probability, so its rank is
# 1 + (# alternatives strictly more likely).
ranks_from_probabilities <- function(fit) {
  p_obs <- exp(fit$intervalLogL)
  vapply(
    seq_along(fit$eventProbabilities),
    function(i) 1L + sum(fit$eventProbabilities[[i]] > p_obs[i] + 1e-9),
    integer(1)
  )
}

estimate_with_ranks <- function(formula, model, sub_model) {
  estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_estimation = set_estimation_opt(diagnostics = c("loglik", "ranks"))
  )
}

estimate_with_probabilities <- function(formula, model, sub_model) {
  estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_estimation = set_estimation_opt(
      engine = "default",
      return_probabilities = TRUE
    )
  )
}

test_that("observed_rank matches enumerated probabilities (DyNAM choice)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  formula <- depNetwork ~ inertia + recip + trans
  fit_ranks <- estimate_with_ranks(formula, "DyNAM", "choice")
  fit_probs <- estimate_with_probabilities(formula, "DyNAM", "choice")

  expect_type(fit_ranks$observed_rank, "integer")
  expect_length(fit_ranks$observed_rank, fit_ranks$nEvents)
  expect_equal(fit_ranks$observed_rank, ranks_from_probabilities(fit_probs))
})

test_that("observed_rank matches enumerated probabilities (DyNAM rate_ordered)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  formula <- depNetwork ~ indeg + outdeg
  fit_ranks <- estimate_with_ranks(formula, "DyNAM", "rate_ordered")
  fit_probs <- estimate_with_probabilities(formula, "DyNAM", "rate_ordered")

  expect_equal(fit_ranks$observed_rank, ranks_from_probabilities(fit_probs))
})

test_that("observed_rank matches enumerated probabilities (REM_ordered)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  formula <- depNetwork ~ inertia + recip
  fit_ranks <- estimate_with_ranks(formula, "REM", "rate_ordered")
  fit_probs <- estimate_with_probabilities(formula, "REM", "rate_ordered")

  expect_equal(fit_ranks$observed_rank, ranks_from_probabilities(fit_probs))
})

test_that("observed_rank is absent unless ranks are requested", {
  fit <- estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest
  )
  expect_null(fit$observed_rank)
})

test_that("observed_rank is a valid rank vector on exact-time REM", {
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- estimate_with_ranks(depNetwork ~ inertia + recip, "REM", "rate")

  ranks <- fit$observed_rank
  expect_type(ranks, "integer")
  expect_length(ranks, fit$nEvents)
  dependent <- !is.na(ranks)
  expect_true(all(ranks[dependent] >= 1L))
})

# In-pass actor margins (per-actor observed vs expected event counts). The
# calibration identity splits by flavor: multinomial expected counts sum
# algebraically to the event count at ANY parameter, so those are checked at a
# non-MLE vector (`max_iterations = 0` pins the evaluation there); exact-time
# expected counts sum to the event count only at the converged MLE via the
# intercept score equation, so those are fit to convergence. REM stores both
# sender and receiver margins whose totals coincide identically.
margins_eval <- function(spec, data_list, params = NULL) {
  opt_args <- c(
    list(diagnostics = c("loglik", "margins")),
    spec$estimation_args
  )
  if (!is.null(params)) {
    opt_args$initial_parameters <- params
    opt_args$max_iterations <- 0
  }
  args <- list(
    x = spec$formula,
    data = data_list[[spec$dataset]],
    control_estimation = do.call(set_estimation_opt, opt_args),
    progress = FALSE,
    verbose = FALSE
  )
  if (spec$model == "DyNAM") {
    args$sub_model <- spec$sub_model
    suppressWarnings(do.call(estimate_dynam, args))
  } else {
    if (!is.null(spec$sub_model)) {
      args$sub_model <- spec$sub_model
    }
    suppressWarnings(do.call(estimate_rem, args))
  }
}

test_that("multinomial margins sum to the event count at a non-MLE vector", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()
  # (spec key, number of free parameters). The expected-count total matches the
  # observed total for every flavor; for choice_coordination both already sum to
  # 2 * n because each event credits both members of the pair.
  cases <- list(
    list(nm = "se_dynam_choice", p = 3),
    list(nm = "se_dynam_rate_ordered", p = 3),
    list(nm = "se_dynam_choice_coord", p = 2),
    list(nm = "se_rem_ordered", p = 3)
  )
  for (case in cases) {
    fit <- margins_eval(grid[[case$nm]], data_list, params = rep(0.3, case$p))
    margins <- fit$margins
    expect_false(is.null(margins), info = case$nm)
    if (!is.null(margins$expected_sender)) {
      # REM carries both sides; each totals the event count.
      n_obs_sender <- sum(margins$observed_sender)
      expect_equal(
        sum(margins$expected_sender),
        n_obs_sender,
        tolerance = 1e-8,
        info = case$nm
      )
      expect_equal(
        sum(margins$expected_receiver),
        sum(margins$observed_receiver),
        tolerance = 1e-8,
        info = case$nm
      )
      expect_equal(
        sum(margins$expected_sender),
        sum(margins$expected_receiver),
        tolerance = 1e-8,
        info = case$nm
      )
      # Observed vectors tabulate whole actors of the dependent events.
      expect_equal(
        margins$observed_sender,
        round(margins$observed_sender),
        info = case$nm
      )
    } else {
      expect_equal(
        sum(margins$expected),
        sum(margins$observed),
        tolerance = 1e-8,
        info = case$nm
      )
      expect_equal(margins$observed, round(margins$observed), info = case$nm)
      expect_true(all(margins$expected >= 0), info = case$nm)
    }
  }
})

test_that("exact-time margins sum to the event count at the MLE", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()

  # DyNAM rate: sender margins, exposure-weighted, right-censored intervals
  # included; equals the dependent-event count at the MLE (intercept present).
  fit_rate <- margins_eval(grid[["se_dynam_rate"]], data_list)
  m_rate <- fit_rate$margins
  expect_false(is.null(m_rate$expected))
  expect_equal(
    sum(m_rate$expected),
    sum(m_rate$observed),
    tolerance = 1e-4
  )

  # REM (exact-time): both sides; each side equals the event count at the MLE
  # and the two totals are identical (the same double sum).
  fit_rem <- margins_eval(grid[["se_rem"]], data_list)
  m_rem <- fit_rem$margins
  expect_false(is.null(m_rem$expected_sender))
  expect_equal(
    sum(m_rem$expected_sender),
    sum(m_rem$observed_sender),
    tolerance = 1e-4
  )
  expect_equal(
    sum(m_rem$expected_receiver),
    sum(m_rem$observed_receiver),
    tolerance = 1e-4
  )
  expect_equal(
    sum(m_rem$expected_sender),
    sum(m_rem$expected_receiver),
    tolerance = 1e-7
  )
})

test_that("margins are absent unless requested", {
  fit <- estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest
  )
  expect_null(fit$margins)
})

# total_rate (per-event summed fitted rate over the realized risk set) rides
# along with the "loglik" primitive on exact-time submodels and is default-on.
# `total_rate * interevent time` is the Cox-Snell/compensator residual, recovered
# from stored primitives with no evaluation pass; at the MLE the compensators sum
# to the dependent-event count via the intercept score equation.
total_rate_fit <- function(spec, data_list, preprocessing_only = FALSE) {
  args <- list(
    x = spec$formula,
    data = data_list[[spec$dataset]],
    control_estimation = do.call(
      set_estimation_opt,
      c(list(), spec$estimation_args)
    ),
    preprocessing_only = preprocessing_only,
    progress = FALSE,
    verbose = FALSE
  )
  if (spec$model == "DyNAM") {
    args$sub_model <- spec$sub_model
    suppressWarnings(do.call(estimate_dynam, args))
  } else {
    if (!is.null(spec$sub_model)) {
      args$sub_model <- spec$sub_model
    }
    suppressWarnings(do.call(estimate_rem, args))
  }
}

test_that("total_rate reproduces the Cox-Snell residuals on exact-time fits", {
  skip_on_cran()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()

  # DyNAM rate: default diagnostics store total_rate; the compensator identity is
  # exact on right-censored intervals (intervalLogL = -Dt * total_rate there) and
  # the compensators sum to the dependent-event count at the MLE.
  fit_rate <- total_rate_fit(grid[["se_dynam_rate"]], data_list)
  prep_rate <- total_rate_fit(grid[["se_dynam_rate"]], data_list, TRUE)
  dt_rate <- prep_rate$intervals
  expect_false(is.null(fit_rate$total_rate))
  expect_length(fit_rate$total_rate, fit_rate$nEvents)
  expect_true(all(fit_rate$total_rate > 0))
  censored <- prep_rate$is_dependent == 0
  expect_equal(
    fit_rate$total_rate[censored] * dt_rate[censored],
    -fit_rate$intervalLogL[censored]
  )
  expect_equal(
    sum(fit_rate$total_rate * dt_rate),
    sum(prep_rate$is_dependent == 1),
    tolerance = 1e-4
  )

  # REM (exact-time): same compensator-sum identity.
  fit_rem <- total_rate_fit(grid[["se_rem"]], data_list)
  prep_rem <- total_rate_fit(grid[["se_rem"]], data_list, TRUE)
  expect_false(is.null(fit_rem$total_rate))
  expect_equal(
    sum(fit_rem$total_rate * prep_rem$intervals),
    sum(prep_rem$is_dependent == 1),
    tolerance = 1e-4
  )
})

test_that("total_rate is stored only for exact-time submodels", {
  fit_choice <- estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest
  )
  expect_null(fit_choice$total_rate)

  fit_ordered <- suppressWarnings(estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "REM",
    sub_model = "rate_ordered",
    data = dataTest
  ))
  expect_null(fit_ordered$total_rate)

  # Dropping "loglik" from diagnostics drops total_rate on an exact-time fit.
  fit_no_loglik <- suppressWarnings(estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "REM",
    sub_model = "rate",
    data = dataTest,
    control_estimation = set_estimation_opt(diagnostics = "scores")
  ))
  expect_null(fit_no_loglik$total_rate)
})

test_that("large per-event storage emits a footprint note above the threshold", {
  # Pure helper: fires above the event threshold, silent below, and scales the
  # reported footprint with the requested primitives.
  expect_message(
    note_diagnostic_storage_footprint(
      n_events = 2e5,
      diagnostics = c("loglik", "scores"),
      n_params = 4,
      is_exact_time = TRUE,
      threshold = 1e5
    ),
    "per-event diagnostics"
  )
  expect_message(
    note_diagnostic_storage_footprint(
      n_events = 2e5,
      diagnostics = c("loglik", "scores"),
      n_params = 4,
      is_exact_time = TRUE,
      threshold = 1e5
    ),
    "diagnostics = FALSE"
  )
  expect_no_message(
    note_diagnostic_storage_footprint(
      n_events = 500,
      diagnostics = c("loglik", "scores"),
      n_params = 4,
      is_exact_time = TRUE,
      threshold = 1e5
    )
  )
  # diagnostics = FALSE (no per-event vectors) stays silent even above threshold.
  expect_no_message(
    note_diagnostic_storage_footprint(
      n_events = 2e5,
      diagnostics = character(0),
      n_params = 4,
      is_exact_time = TRUE,
      threshold = 1e5
    )
  )
})

# Cross-engine parity: the in-pass default_c ranks and margins must equal the
# independent quantities the default R engine computes from its per-event
# probability matrix. Both engines are evaluated at the SAME parameter vector
# (default_c's MLE, pinned on the default engine via max_iterations = 0) so the
# parity is machine-precision rather than the coarser cross-engine tolerance.
parity_fit <- function(spec, data_list, ...) {
  ctrl <- do.call(set_estimation_opt, list(...))
  args <- list(
    x = spec$formula,
    data = data_list[[spec$dataset]],
    control_estimation = ctrl,
    progress = FALSE,
    verbose = FALSE
  )
  if (spec$model == "DyNAM") {
    args$sub_model <- spec$sub_model
    suppressWarnings(do.call(estimate_dynam, args))
  } else {
    if (!is.null(spec$sub_model)) {
      args$sub_model <- spec$sub_model
    }
    suppressWarnings(do.call(estimate_rem, args))
  }
}

test_that("default_c ranks match the default engine at the same parameters", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()
  for (nm in c("se_dynam_choice", "se_dynam_rate_ordered", "se_rem_ordered")) {
    spec <- grid[[nm]]
    fc <- parity_fit(spec, data_list, diagnostics = c("loglik", "ranks"))
    fd <- parity_fit(
      spec,
      data_list,
      engine = "default",
      return_probabilities = TRUE,
      initial_parameters = fc$parameters,
      max_iterations = 0
    )
    expect_equal(fc$observed_rank, ranks_from_probabilities(fd), info = nm)
  }
})

test_that("default_c margins match the default engine at the same parameters", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()
  # Single-sided multinomial margins are the column sums of the default engine's
  # per-event probability vectors (receiver for choice, sender for ordered rate).
  for (nm in c("se_dynam_choice", "se_dynam_rate_ordered")) {
    spec <- grid[[nm]]
    fc <- parity_fit(spec, data_list, diagnostics = c("loglik", "margins"))
    fd <- parity_fit(
      spec,
      data_list,
      engine = "default",
      return_probabilities = TRUE,
      initial_parameters = fc$parameters,
      max_iterations = 0
    )
    expected_from_prob <- Reduce(`+`, fd$eventProbabilities)
    expect_equal(
      fc$margins$expected,
      expected_from_prob,
      tolerance = 1e-9,
      info = nm
    )
  }
})
