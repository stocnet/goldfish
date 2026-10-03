# Golden 1e-10 regression fixtures for the rate/REM per-event contribution
# (event_contribution_rate) scenarios the coefficient baselines do not exercise:
# the REM riskMask zeroing and reflexive-edge exclusion (dim<- reshape +
# crossprod core) and the single-parameter rate path (the removed special-case
# Fisher loop). These fixtures are fully deterministic (set.seed, no fit), so
# the refactored helper's output is pinned as a serialized snapshot and compared
# at 1e-10 on every run.
#
# The transitional old-vs-new equivalence harness (which replayed captured
# events through a frozen reference implementation) is retired now that the
# refactor has shipped: ongoing per-event agreement for all six sub-models on
# both datasets is guarded deterministically by test-process_state_evaluators.R
# (1e-10) and the frozen coefficient baselines (1e-6).

test_that("rate/REM core: riskMask and reflexive zeroing (golden fixture)", {
  skip_on_cran()
  live_rate <- getFromNamespace("event_contribution_rate", "goldfish")
  set.seed(42)
  n1 <- 5L
  n2 <- 5L
  p <- 3L
  statsArray <- array(stats::rnorm(n1 * n2 * p), dim = c(n1, n2, p))
  parameters <- c(0.3, -0.5, 0.8)
  activeDyad <- c(2L, 4L)
  riskMask <- matrix(TRUE, n1, n2)
  riskMask[cbind(c(1L, 4L, 5L), c(3L, 2L, 5L))] <- FALSE

  scenarios <- list(
    list(irc = FALSE, ts = 1.5, refl = FALSE, mask = riskMask),
    list(irc = TRUE, ts = 2.0, refl = TRUE, mask = riskMask),
    list(irc = FALSE, ts = 0.7, refl = FALSE, mask = NULL)
  )
  results <- lapply(scenarios, function(sc) {
    live_rate(
      statsArray,
      activeDyad,
      parameters,
      sc$irc,
      sc$ts,
      sc$refl,
      is_two_mode = FALSE,
      isREM = TRUE,
      active_dyad_mask = sc$mask
    )
  })
  expect_snapshot_value(results, style = "serialize", tolerance = 1e-10)
})

test_that("rate core: single-parameter path (golden fixture)", {
  skip_on_cran()
  live_rate <- getFromNamespace("event_contribution_rate", "goldfish")
  set.seed(7)
  statsArray <- matrix(stats::rnorm(6L), 6L, 1L)
  live <- live_rate(
    statsArray,
    c(3L, NA),
    0.6,
    FALSE,
    1.2,
    TRUE,
    is_two_mode = FALSE,
    isREM = FALSE
  )
  expect_snapshot_value(live, style = "serialize", tolerance = 1e-10)
})

test_that("REM-ordered moments equal the centered score and information", {
  # The ordinal REM forms its score and information from the moments,
  # s_obs - E and sum_d p_d s_d s_d' - E E', without the centered array
  # s_d - E. Both are the probability-weighted covariance; the centered form
  # is the reference here. The moment form loses about (mean / sd)^2 machine
  # epsilons relative to the information, so the statistics are kept at
  # the scale the package's fixtures have (mean over sd below 4).
  set.seed(7)
  n1 <- 6L
  n2 <- 4L
  p <- 3L
  stats_array <- array(stats::rnorm(n1 * n2 * p), dim = c(n1, n2, p))
  stats_array[,, 1] <- stats_array[,, 1] + 3
  parameters <- c(0.4, -0.2, 0.1)
  mask <- matrix(TRUE, n1, n2)
  mask[2L, 3L] <- FALSE
  live <- compute_event_contribution.goldfishLikDyadMultinom(
    NULL,
    stats_array,
    c(1L, 2L),
    parameters,
    active_dyad_mask = mask
  )

  linear_predictor <- matrix(0, n1, n2)
  for (k in seq_len(p)) {
    linear_predictor <- linear_predictor + parameters[k] * stats_array[,, k]
  }
  diag(linear_predictor) <- -Inf
  linear_predictor[!mask] <- -Inf
  probabilities <- exp(linear_predictor) / sum(exp(linear_predictor))
  centered <- array(NA_real_, dim = c(n1, n2, p))
  for (k in seq_len(p)) {
    centered[,, k] <- stats_array[,, k] - sum(stats_array[,, k] * probabilities)
  }
  information <- matrix(NA_real_, p, p)
  for (k in seq_len(p)) {
    for (l in seq_len(p)) {
      information[k, l] <- sum(probabilities * centered[,, k] * centered[,, l])
    }
  }

  expect_equal(live$score, centered[1L, 2L, ], tolerance = 1e-12)
  expect_equal(live$informationMatrix, information, tolerance = 1e-12)
  expect_equal(live$pMatrix, probabilities, tolerance = 1e-12)
})
