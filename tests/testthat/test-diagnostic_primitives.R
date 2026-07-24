# In-pass diagnostic primitives (ranks, margins, total_rate) computed by the
# default_c engines. For the multinomial engines observed_rank is validated
# against ranks enumerated from the default R engine's per-event probabilities
# at the same MLE; the exact-time engines are cross-checked against the default
# R engine in the engine-parity tests (see test-diagnostic_parity).

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
