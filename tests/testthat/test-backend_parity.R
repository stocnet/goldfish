# Three-way parity: cpp, r and gather compute the same per-event primitives
# from the same reductions, so they must agree numerically, not merely produce
# components of the same name.
#
# Every comparison is made at a FIXED parameter vector -- a converged cpp fit,
# pinned on each backend with `max_iterations = 0`. Compared at each backend's
# own optimum instead, the estimates agree only to the ~1e-6 cross-backend
# convergence tolerance, which is far coarser than the reductions themselves and
# would hide a real discrepancy in any of them.

# Fit the same specification on all three backends at one parameter vector.
# Returns the three fits keyed by backend, each carrying every primitive named.
parity_trio <- function(formula, data, model, sub_model, diagnostics) {
  fit_with <- function(backend, control) {
    args <- list(
      formula,
      data = data,
      control_algo = control,
      progress = FALSE,
      verbose = FALSE
    )
    if (!is.null(sub_model)) {
      args$sub_model <- sub_model
    }
    suppressWarnings(suppressMessages(
      do.call(
        if (identical(model, "DyNAM")) estimate_dynam else estimate_rem,
        args
      )
    ))
  }
  converged <- fit_with(
    "cpp",
    set_algorithm_newton(backend = "cpp")
  )
  fits <- lapply(BACKEND_VALUES, function(backend) {
    fit_with(
      backend,
      set_algorithm_newton(
        backend = backend,
        diagnostics = diagnostics,
        initial_parameters = converged$parameters,
        max_iterations = 0
      )
    )
  })
  names(fits) <- BACKEND_VALUES
  fits
}

# Assert the trio agrees on every primitive it was asked for. `tolerance` is
# 1e-10 throughout (D5): the reductions are algebraically identical, so the only
# admissible difference is accumulation order.
expect_parity <- function(fits, label) {
  ref <- fits$cpp
  # A comparison proves nothing unless each fit ran where it was asked to; the
  # probabilities redirect used to make exactly this shape of test vacuous.
  for (backend in names(fits)) {
    expect_equal(fits[[backend]]$backend, backend, info = label)
  }
  for (backend in setdiff(names(fits), "cpp")) {
    other <- fits[[backend]]
    nm <- paste(label, backend)
    # Ranks count strict inequalities on both sides, so they are exact, not
    # toleranced.
    expect_equal(other$observed_rank, ref$observed_rank, info = nm)
    expect_named(other$margins, names(ref$margins), info = nm)
    for (field in names(ref$margins)) {
      expect_equal(
        other$margins[[field]],
        ref$margins[[field]],
        tolerance = 1e-10,
        info = paste(nm, field)
      )
    }
    expect_equal(
      other$event_scores,
      ref$event_scores,
      tolerance = 1e-10,
      info = nm
    )
    expect_equal(
      lapply(other$eventProbabilities, as.numeric),
      lapply(ref$eventProbabilities, as.numeric),
      tolerance = 1e-10,
      info = nm
    )
    expect_equal(
      other$intervalLogL,
      ref$intervalLogL,
      tolerance = 1e-10,
      info = nm
    )
    # Exact-time only; NULL on the multinomial families, where expect_equal
    # compares NULL to NULL.
    expect_equal(other$total_rate, ref$total_rate, tolerance = 1e-10, info = nm)
    expect_equal(
      other$conditional_logl,
      ref$conditional_logl,
      tolerance = 1e-10,
      info = nm
    )
  }
}

ALL_PRIMITIVES <- c("loglik", "scores", "ranks", "margins", "probabilities")

test_that("multinomial primitives agree across backends (DyNAM choice)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  spec <- baselines_model_grid()$se_dynam_choice
  expect_parity(
    parity_trio(
      spec$formula,
      baselines_social_evolution_data(),
      "DyNAM",
      spec$sub_model,
      ALL_PRIMITIVES
    ),
    "dynam_choice"
  )
})

test_that("two-sided multinomial primitives agree across backends", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  spec <- baselines_model_grid()$se_rem_ordered
  expect_parity(
    parity_trio(
      spec$formula,
      baselines_social_evolution_data(),
      "REM",
      spec$sub_model,
      ALL_PRIMITIVES
    ),
    "rem_ordered"
  )
})

test_that("exact-time primitives agree across backends (DyNAM rate)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  spec <- baselines_model_grid()$se_dynam_rate
  fits <- parity_trio(
    spec$formula,
    baselines_social_evolution_data(),
    "DyNAM",
    spec$sub_model,
    ALL_PRIMITIVES
  )
  expect_parity(fits, "dynam_rate")
  # Exact-time margins carry BOTH scales: the compensator, whose per-actor
  # observed-minus-expected is the martingale residual, and the probability
  # scale, which totals the event count at any parameter vector.
  expect_true("expected" %in% names(fits$cpp$margins))
  expect_true("expected_probability" %in% names(fits$cpp$margins))
})

test_that("two-sided exact-time primitives agree across backends (REM)", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  spec <- baselines_model_grid()$se_rem
  expect_parity(
    parity_trio(
      spec$formula,
      baselines_social_evolution_data(),
      "REM",
      NULL,
      ALL_PRIMITIVES
    ),
    "rem"
  )
})

# The largest block of alternatives whose fitted probability sits within a
# relative `tol` of the observed one, over all events. Where that block is big,
# the rank of the observed alternative is decided by rounding rather than by the
# model, and two implementations that both round correctly can still disagree.
worst_near_tie_block <- function(fit, tol = 1e-12) {
  p_obs <- exp(fit$intervalLogL)
  max(vapply(
    seq_along(fit$eventProbabilities),
    function(i) {
      p <- as.numeric(fit$eventProbabilities[[i]])
      p <- p[p > 0]
      sum(abs(p - p_obs[i]) <= tol * p_obs[i])
    },
    numeric(1)
  ))
}

test_that("coordination primitives agree across backends", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # Coordination is the fourth geometry: its realized risk set is the unordered
  # pair list, and a pair's probability is credited to both members, so the
  # per-event grid is symmetric and totals 2.
  fits <- parity_trio(
    depNetwork ~ inertia + trans,
    dataTest,
    "DyNAM",
    "choice_coordination",
    ALL_PRIMITIVES
  )
  # Fixture precondition. The social-evolution coordination fixture carries a
  # ~7000-wide near-tie block at its last event, where r's `stable_softmax` log
  # normalizer and cpp's `log_sum_exp` round ~57 near-degenerate dyads across
  # the observed boundary differently -- an irreducible tie fragility, not a
  # logic error. This fixture is chosen because it has no such block, so "ranks
  # exact" stays exact rather than being weakened to accommodate one. Asserting
  # it keeps that property from eroding unnoticed.
  expect_lt(worst_near_tie_block(fits$cpp), 50)
  expect_parity(fits, "coordination")
  expect_equal(
    vapply(fits$cpp$eventProbabilities, sum, numeric(1)),
    rep(2, fits$cpp$nEvents)
  )
})

test_that("by-design NAs land at exactly the censored positions", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # An effect on an exogenous network turns that network's events into
  # likelihood intervals with no mover. `observed_rank` and the conditional
  # loglik component are undefined there and carry NA by design, identically on
  # every backend.
  fits <- parity_trio(
    depNetwork ~ 1 + indeg(networkExog),
    dataTest,
    "DyNAM",
    "rate",
    ALL_PRIMITIVES
  )
  censored <- fits$cpp$right_censored_events
  # Precondition: with no censored interval the NA contract below is vacuous.
  expect_gt(sum(censored), 0L)
  for (backend in BACKEND_VALUES) {
    fit <- fits[[backend]]
    expect_equal(is.na(fit$observed_rank), unname(censored), info = backend)
    expect_equal(is.na(fit$conditional_logl), unname(censored), info = backend)
  }
  expect_parity(fits, "censored_rate")
})

test_that("the conditional component matches its identity at the MLE", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # log p_obs == intervalLogL - log T + Dt * T. This is a CHECK, never the
  # computation: the identity adds back a term the log-likelihood just
  # subtracted, losing digits in proportion to Dt * T. At the MLE the per-event
  # expected count is order one by the intercept score equation, so the identity
  # still holds to the parity tolerance; away from it, it degrades faster than
  # the tolerance allows, which is why the kernels compute `x_obs - lse`.
  spec <- baselines_model_grid()$se_dynam_rate
  data <- baselines_social_evolution_data()
  fit <- suppressWarnings(suppressMessages(estimate_dynam(
    spec$formula,
    data = data,
    sub_model = spec$sub_model,
    control_algo = set_algorithm_newton(
      backend = "cpp",
      diagnostics = c("loglik", "margins")
    ),
    progress = FALSE,
    verbose = FALSE
  )))
  # The fit stores event times rather than interval lengths, so Dt is the
  # successive difference. The first interval runs from the observation window's
  # start, which no stored component carries, so it is left out.
  interval_length <- c(NA_real_, diff(fit$event_time))
  usable <- seq_len(fit$nEvents)[-1]
  censored <- fit$right_censored_events

  # Validate that reconstruction before relying on it: a right-censored
  # interval's log-likelihood is exactly -Dt * T, so it pins Dt independently.
  pinned <- usable[censored[usable]]
  expect_gt(length(pinned), 0L)
  expect_equal(
    interval_length[pinned] * fit$total_rate[pinned],
    -fit$intervalLogL[pinned],
    tolerance = 1e-9
  )

  dependent <- usable[!censored[usable]]
  identity <- fit$intervalLogL[dependent] -
    log(fit$total_rate[dependent]) +
    interval_length[dependent] * fit$total_rate[dependent]
  expect_equal(
    fit$conditional_logl[dependent],
    identity,
    tolerance = 1e-8
  )
})

test_that("estimation iterates with every primitive on", {
  skip_on_cran()
  withr::local_options(lifecycle_verbosity = "quiet")
  # The parity tests above are fixed-theta on purpose: their job is numerical
  # agreement, not loop mechanics. But `max_iterations = 0` breaks out of the
  # Newton loop BEFORE step acceptance, and step acceptance is where the guard
  # bug lived -- it scanned the kernel result for any NA and read
  # `observed_rank`'s by-design NAs as a numerical failure, rejecting every
  # step. The first rejection restored a still-NULL information matrix, and
  # `NULL[i, j]` being silently NULL turned it into "Matrix cannot be inverted;
  # probably due to collinearity between parameters" -- a message naming the
  # wrong subsystem entirely. Only a free fit on a censored fixture reaches it.
  for (backend in BACKEND_VALUES) {
    fit <- suppressWarnings(suppressMessages(estimate_dynam(
      depNetwork ~ 1 + indeg(networkExog),
      data = dataTest,
      sub_model = "rate",
      control_algo = set_algorithm_newton(
        backend = backend,
        diagnostics = ALL_PRIMITIVES,
        max_iterations = 5
      ),
      progress = FALSE,
      verbose = FALSE
    )))
    # Precondition: without a censored interval there are no by-design NAs and
    # the guard is never exercised.
    expect_gt(sum(fit$right_censored_events), 0L)
    # The loop ran rather than bailing out at the first step.
    expect_gt(fit$nIterations, 1L)
    expect_equal(fit$backend, backend, info = backend)
    for (component in c(
      "intervalLogL",
      "event_scores",
      "observed_rank",
      "margins",
      "eventProbabilities",
      "total_rate",
      "conditional_logl"
    )) {
      expect_false(is.null(fit[[component]]), info = paste(backend, component))
    }
    # The NAs that survive are exactly the by-design ones, on the two
    # components that are undefined without an observed mover.
    censored <- unname(fit$right_censored_events)
    expect_equal(is.na(fit$observed_rank), censored, info = backend)
    expect_equal(is.na(fit$conditional_logl), censored, info = backend)
    expect_false(any(is.na(fit$intervalLogL)), info = backend)
    expect_false(any(is.na(fit$event_scores)), info = backend)
  }
})
