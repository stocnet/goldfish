# In-pass diagnostic primitives (ranks, margins, total_rate) computed by the
# cpp backend. For the multinomial sub-models observed_rank is validated
# against ranks enumerated from the default R engine's per-event probabilities
# at the same MLE, and the parity tests at the end compare cpp ranks and
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
    control_algo = set_algorithm_newton(
      diagnostics = c("loglik", "ranks")
    )
  )
}

estimate_with_probabilities <- function(formula, model, sub_model) {
  estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_algo = set_algorithm_newton(
      backend = "r",
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
    control_algo = do.call(set_algorithm_newton, opt_args),
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
    control_algo = do.call(
      set_algorithm_newton,
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
    control_algo = set_algorithm_newton(diagnostics = "scores")
  ))
  expect_null(fit_no_loglik$total_rate)
})

# conditional_logl (the "which" component of the exact-time loglik, the Cox
# partial-likelihood contribution log p_obs) is NA on right-censored intervals
# by design: a censored interval realizes no mover, so there is no observed
# alternative to condition on (D21). The gather Poisson kernel previously stored
# a placeholder (lin_pred[selected] - lse, `selected` being the exogenous
# event's actor) there; it now stores NA at exactly those positions, matching
# the r backend. `indeg(networkExog)` -- an effect on an exogenous network -- is
# what gives this fixture right-censored intervals at all, so the precondition
# is asserted rather than assumed (the vacuous-fixture trap of D21).
test_that("gather conditional_logl is NA on right-censored intervals only", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  formula <- depNetwork ~ 1 + indeg + outdeg(networkExog, weighted = TRUE)

  # A converged gather fit supplies the fixed parameter vector both fits share.
  conv <- suppressWarnings(estimate_wrapper(
    formula,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(
      backend = "gather",
      diagnostics = "loglik"
    )
  ))
  params <- conv$parameters

  fit_gather <- suppressWarnings(estimate_wrapper(
    formula,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(
      backend = "gather",
      diagnostics = "loglik",
      initial_parameters = params,
      max_iterations = 0
    )
  ))

  censored <- fit_gather$right_censored_events
  # Precondition: the fixture must actually carry a right-censored interval, or
  # every NA assertion below would pass vacuously.
  expect_gt(sum(censored), 0)

  expect_false(is.null(fit_gather$conditional_logl))
  # NA at exactly the censored positions, finite log-probabilities elsewhere.
  expect_true(all(is.na(fit_gather$conditional_logl[censored])))
  expect_true(all(!is.na(fit_gather$conditional_logl[!censored])))
  expect_true(all(fit_gather$conditional_logl[!censored] <= 0))

  # Independent r-backend reference: the conditional component log p_obs is the
  # ordinal ("which", timing removed) per-event log-likelihood, so a DyNAM
  # rate_ordered fit at the same non-intercept coefficients reproduces it
  # through an entirely different kernel (the intercept cancels in the softmax).
  ord_r <- suppressWarnings(estimate_wrapper(
    depNetwork ~ indeg + outdeg(networkExog, weighted = TRUE),
    model = "DyNAM",
    sub_model = "rate_ordered",
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(
      backend = "r",
      return_interval_loglik = TRUE,
      initial_parameters = params[-1],
      max_iterations = 0
    )
  ))
  expect_equal(
    fit_gather$conditional_logl[!censored],
    ord_r$intervalLogL,
    tolerance = 1e-10
  )
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

# Cross-backend parity: the in-pass ranks and margins of the cpp and r backends
# are the same reduction over the same per-event weights, so they must agree
# natively -- no reconstruction in between. Both backends are evaluated at the
# SAME parameter vector (cpp's MLE, pinned on r via max_iterations = 0) so the
# parity is machine-precision rather than the coarser cross-backend tolerance.
parity_fit <- function(spec, data_list, ...) {
  ctrl <- do.call(set_algorithm_newton, list(...))
  args <- list(
    x = spec$formula,
    data = data_list[[spec$dataset]],
    control_algo = ctrl,
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

test_that("r ranks match cpp at the same parameters (multinomial)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()
  for (nm in c("se_dynam_choice", "se_dynam_rate_ordered", "se_rem_ordered")) {
    spec <- grid[[nm]]
    fc <- parity_fit(spec, data_list, diagnostics = c("loglik", "ranks"))
    fr <- parity_fit(
      spec,
      data_list,
      backend = "r",
      diagnostics = c("loglik", "ranks"),
      initial_parameters = fc$parameters,
      max_iterations = 0
    )
    # A count of strict inequalities on both sides: exact, not toleranced.
    expect_equal(fr$observed_rank, fc$observed_rank, info = nm)
  }
})

test_that("r margins match cpp at the same parameters (multinomial)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()
  # choice and rate_ordered are single-sided (receiver / sender); REM_ordered is
  # two-sided, a geometry the probability-matrix reconstruction could not reach.
  for (nm in c("se_dynam_choice", "se_dynam_rate_ordered", "se_rem_ordered")) {
    spec <- grid[[nm]]
    fc <- parity_fit(spec, data_list, diagnostics = c("loglik", "margins"))
    fr <- parity_fit(
      spec,
      data_list,
      backend = "r",
      diagnostics = c("loglik", "margins"),
      initial_parameters = fc$parameters,
      max_iterations = 0
    )
    expect_named(fr$margins, names(fc$margins), info = nm)
    for (field in names(fc$margins)) {
      expect_equal(
        fr$margins[[field]],
        fc$margins[[field]],
        tolerance = 1e-10,
        info = paste(nm, field)
      )
    }
  }
})

# Both backends now run the same shared reduction, so agreeing with each other
# cannot rule out a shared mistake inside it. One fixture keeps the original
# reconstruction from the per-event probability matrix as an independent third
# expectation: it recomputes ranks and margins outside the reduction entirely,
# from probabilities the estimator stores rather than from the accumulators.
test_that("probability-matrix reconstruction confirms both backends (choice)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()[["se_dynam_choice"]]

  fc <- parity_fit(
    spec,
    data_list,
    diagnostics = c("loglik", "ranks", "margins")
  )
  fr <- parity_fit(
    spec,
    data_list,
    backend = "r",
    diagnostics = c("loglik", "ranks", "margins"),
    initial_parameters = fc$parameters,
    max_iterations = 0
  )
  fp <- parity_fit(
    spec,
    data_list,
    backend = "r",
    return_probabilities = TRUE,
    initial_parameters = fc$parameters,
    max_iterations = 0
  )

  reconstructed_ranks <- ranks_from_probabilities(fp)
  expect_equal(fc$observed_rank, reconstructed_ranks)
  expect_equal(fr$observed_rank, reconstructed_ranks)

  # Single-sided multinomial expected margins are the column sums of the
  # per-event probability vectors (receiver for choice).
  reconstructed_margins <- Reduce(`+`, fp$eventProbabilities)
  expect_equal(fc$margins$expected, reconstructed_margins, tolerance = 1e-9)
  expect_equal(fr$margins$expected, reconstructed_margins, tolerance = 1e-9)
})

# The r backend accumulates ranks and margins in its contribution loop from the
# per-event probability vector, without materializing the whole probability
# matrix. These directly compare the native r reductions to the cpp kernels at a
# fixed parameter vector, for the families the reconstruction tests above do not
# cover (the exact-time rate / REM, whose margins carry both scale variants).
test_that("r ranks and margins match cpp at the same parameters (exact-time)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()
  for (nm in c("se_dynam_rate", "se_rem")) {
    spec <- grid[[nm]]
    fc <- parity_fit(
      spec,
      data_list,
      diagnostics = c("loglik", "ranks", "margins")
    )
    fr <- parity_fit(
      spec,
      data_list,
      backend = "r",
      diagnostics = c("loglik", "ranks", "margins"),
      initial_parameters = fc$parameters,
      max_iterations = 0
    )
    expect_equal(fr$observed_rank, fc$observed_rank, info = nm)
    # total_rate and the conditional loglik component now exist natively on r.
    expect_equal(fr$total_rate, fc$total_rate, tolerance = 1e-10, info = nm)
    # Compensator-scale margins (the variant cpp also carries) agree; the
    # probability-scale variant is r-only until cpp gains it in task 5.1.
    mc <- fc$margins
    mr <- fr$margins
    for (field in intersect(names(mc), names(mr))) {
      expect_equal(
        mr[[field]],
        mc[[field]],
        tolerance = 1e-9,
        info = paste(nm, field)
      )
    }
  }
})

test_that("requesting margins but not probabilities carries no probability matrix", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # The point of accumulating in the loop rather than reconstructing: margins
  # come back without the O(events x actors) probability matrix ever forming.
  fit <- suppressWarnings(estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(
      backend = "r",
      diagnostics = c("loglik", "margins")
    )
  ))
  expect_false(is.null(fit$margins))
  expect_null(fit$eventProbabilities)
  expect_null(fit$pMatrix)
})

# Per-event probabilities are indexed by actor over the whole node set, not by
# position in the event's reduced risk set, so a position means the same actor
# at every event and on every backend. Requesting margins alongside is the point
# of the contract: the two primitives must agree about what an index is, which
# is why each shape below is asserted against the margin vector's own length.
probability_fit <- function(model, sub_model, formula, data = dataTest) {
  suppressWarnings(suppressMessages(estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = data,
    control_algo = set_algorithm_newton(
      backend = "r",
      diagnostics = c("loglik", "margins", "probabilities"),
      max_iterations = 1
    )
  )))
}

test_that("per-event probabilities are actor-indexed on the sender axis", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  for (sub_model in c("rate", "rate_ordered")) {
    formula <- if (identical(sub_model, "rate")) {
      depNetwork ~ 1 + indeg
    } else {
      depNetwork ~ indeg + outdeg
    }
    fit <- probability_fit("DyNAM", sub_model, formula)
    p <- fit$eventProbabilities[[1]]
    expect_length(p, length(fit$margins$expected))
    expect_equal(sum(p), 1, info = sub_model)
  }
})

test_that("per-event probabilities zero the receivers outside the risk set", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- probability_fit("DyNAM", "choice", depNetwork ~ inertia + recip)
  p <- fit$eventProbabilities[[1]]
  expect_length(p, length(fit$margins$expected))
  expect_equal(sum(p), 1)
  # A DyNAM-choice sender cannot choose itself, so the reflexive position is a
  # zero rather than a dropped entry -- the case that made the old reduced
  # shape ragged even with no composition change at all. Every event therefore
  # carries strictly fewer alternatives at risk than the vector is long.
  n_at_risk <- vapply(
    fit$eventProbabilities,
    function(p) sum(p > 0),
    integer(1)
  )
  expect_true(all(n_at_risk < length(p)))
})

test_that("per-event probabilities span the whole dyad grid", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  fit <- probability_fit("REM", "rate", depNetwork ~ 1 + inertia)
  p <- fit$eventProbabilities[[1]]
  n1 <- length(fit$margins$expected_sender)
  n2 <- length(fit$margins$expected_receiver)
  expect_equal(dim(p), c(n1, n2))
  expect_equal(sum(p), 1)
})

test_that("per-event probability length is constant under composition change", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # The fisheries fixture's actors enter and leave, which under the reduced
  # shape gave 13 distinct per-event lengths across 215 events. Actor indexing
  # makes the length constant and moves the variation into how many entries are
  # nonzero -- the same information, at a stable position per actor.
  data_list <- list(fisheries = baselines_fisheries_data())
  spec <- baselines_model_grid()[["fish_dynam_rate"]]
  fit <- parity_fit(
    spec,
    data_list,
    backend = "r",
    diagnostics = c("loglik", "margins", "probabilities"),
    max_iterations = 1
  )

  lengths <- vapply(fit$eventProbabilities, length, integer(1))
  expect_equal(unique(lengths), length(fit$margins$expected))
  # The precondition: this fixture must actually exercise composition change,
  # or the test passes vacuously on a constant risk set.
  n_at_risk <- vapply(
    fit$eventProbabilities,
    function(p) sum(p > 0),
    integer(1)
  )
  expect_gt(length(unique(n_at_risk)), 1L)
  expect_true(all(n_at_risk < unique(lengths)))
  expect_equal(vapply(fit$eventProbabilities, sum, numeric(1)), rep(1, 215))
})

test_that("cpp per-event probabilities match the r backend", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()
  families <- c(
    "se_dynam_rate",
    "se_dynam_rate_ordered",
    "se_dynam_choice",
    "se_rem",
    "se_rem_ordered"
  )
  for (nm in families) {
    spec <- grid[[nm]]
    fc <- parity_fit(
      spec,
      data_list,
      diagnostics = c("loglik", "probabilities")
    )
    fr <- parity_fit(
      spec,
      data_list,
      backend = "r",
      diagnostics = c("loglik", "probabilities"),
      initial_parameters = fc$parameters,
      max_iterations = 0
    )
    expect_equal(
      lapply(fc$eventProbabilities, as.numeric),
      lapply(fr$eventProbabilities, as.numeric),
      tolerance = 1e-10,
      info = nm
    )
    # The same softmax on the same predictors, so the per-event totals hold to
    # machine precision on every family (D16's next-event probability).
    expect_equal(
      vapply(fc$eventProbabilities, sum, numeric(1)),
      rep(1, fc$nEvents),
      info = nm
    )
  }
})

test_that("the cpp exact-time kernels carry the conditional loglik component", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # The Cox partial-likelihood contribution log p_obs. The cpp kernels compute
  # it from a max-shifted pass added beside the raw one -- the same `x_obs - lse`
  # form the gather kernel uses -- so the three backends agree exactly rather
  # than only where nothing over- or underflows.
  formula <- depNetwork ~ 1 + indeg(networkExog)
  fit_at <- function(backend, initial = NULL, iterations = 1) {
    suppressWarnings(suppressMessages(estimate_wrapper(
      formula,
      model = "DyNAM",
      sub_model = "rate",
      data = dataTest,
      control_algo = set_algorithm_newton(
        backend = backend,
        diagnostics = c("loglik", "margins"),
        initial_parameters = initial,
        max_iterations = iterations
      )
    )))
  }
  fc <- fit_at("cpp")
  fr <- fit_at("r", fc$parameters, 0)
  fg <- fit_at("gather", fc$parameters, 0)

  # Precondition: without a right-censored interval the NA contract below is
  # vacuous, and this fixture is the one that produces them (an effect on an
  # exogenous network turns its events into intervals with no mover).
  censored <- fc$right_censored_events
  expect_gt(sum(censored), 0L)

  expect_false(is.null(fc$conditional_logl))
  expect_equal(is.na(fc$conditional_logl), unname(censored))
  dependent <- !censored
  expect_equal(
    fc$conditional_logl[dependent],
    fr$conditional_logl[dependent],
    tolerance = 1e-10
  )
  expect_equal(
    fc$conditional_logl[dependent],
    fg$conditional_logl[dependent],
    tolerance = 1e-10
  )
  # Exact-time margins now carry BOTH scales on cpp, so its component set
  # matches the r backend's rather than trailing it by one.
  expect_named(fc$margins, names(fr$margins))
  expect_equal(
    fc$margins$expected_probability,
    fr$margins$expected_probability,
    tolerance = 1e-10
  )
  # The probability scale totals the event count at ANY parameter vector,
  # which is what makes it the calibration map; the compensator scale does so
  # only at the MLE.
  expect_equal(sum(fc$margins$expected_probability), sum(dependent))
})

test_that("a right-censored interval's NA rank is not read as a failure", {
  # `observed_rank` is allocated NA-filled and written only for dependent
  # events, so any model with a right-censored interval leaves NAs behind when
  # ranks are requested. The initial-parameters guard scans the kernel result
  # for NA meaning "the likelihood could not be evaluated"; before the guard
  # learned to skip this component, requesting ranks on such a model aborted
  # with "Estimation not possible with initial parameters".
  fit <- suppressWarnings(estimate_wrapper(
    depNetwork ~ 1 + indeg,
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton(
      backend = "cpp",
      diagnostics = c("loglik", "ranks"),
      initial_parameters = c(-3, 0.1),
      max_iterations = 0
    )
  ))
  expect_false(is.null(fit$observed_rank))
  # Right-censored intervals keep their NA; dependent events are ranked.
  expect_true(all(is.na(fit$observed_rank[fit$right_censored_events])))
  expect_true(all(!is.na(fit$observed_rank[!fit$right_censored_events])))
})

# The guard above runs once, at the initial parameters. The Newton loop scans
# the same result again every iteration to decide whether to accept a step, so
# a by-design NA has to be excluded there too. `indeg(networkExog)` is what
# gives this fixture right-censored intervals at all: an effect on an exogenous
# network turns that network's events into likelihood intervals with no mover,
# which is why the plain `~ 1 + indeg` fixture above cannot exercise either
# guard (it preprocesses to zero censored intervals).
test_that("requesting ranks does not perturb a free fit with censored events", {
  formula <- depNetwork ~ 1 + indeg(networkExog)
  fit_free <- function(diagnostics, backend) {
    suppressWarnings(estimate_wrapper(
      formula,
      model = "DyNAM",
      sub_model = "rate",
      data = dataTest,
      control_algo = set_algorithm_newton(
        backend = backend,
        diagnostics = diagnostics
      )
    ))
  }

  for (backend in c("cpp", "gather")) {
    reference <- fit_free("loglik", backend)
    expect_gt(sum(reference$right_censored_events), 0)

    # Before the step-acceptance guard learned to skip `observed_rank`, every
    # step was rejected as a numerical failure and the first reset restored a
    # NULL information matrix, surfacing as "Matrix cannot be inverted".
    with_ranks <- expect_no_error(fit_free(c("loglik", "ranks"), backend))
    expect_equal(
      coef(with_ranks),
      coef(reference),
      info = backend
    )
    expect_true(
      all(is.na(with_ranks$observed_rank[with_ranks$right_censored_events])),
      info = backend
    )
    expect_true(
      all(!is.na(with_ranks$observed_rank[!with_ranks$right_censored_events])),
      info = backend
    )
  }
})
