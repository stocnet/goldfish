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
