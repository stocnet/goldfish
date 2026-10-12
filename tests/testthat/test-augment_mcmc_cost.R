# What an MCMC move opens and scores, counted on the coupled walk-alignment
# reference fixture (helper-walk_alignment.R), whose support constraint makes
# every move decide legality. Counts, not timings: they are deterministic for
# a seeded chain.
#
# A move reads its forward rates on the current sequence (one walk) and both
# its legality and its reverse rates on the candidate (one walk over the same
# prefix). An illegal candidate is rejected with no reverse rate and no
# log-likelihood.

# Per MCMC step made in `env`: the walks it opened, the candidate scorings it
# made, and its record.
local_mcmc_counts <- function(env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$opens <- 0L
  log$scored <- 0L
  log$steps <- list()
  open <- walk_open
  score <- evaluate_candidate
  step <- mcmc_step
  local_mocked_bindings(
    walk_open = function(...) {
      log$opens <- log$opens + 1L
      open(...)
    },
    evaluate_candidate = function(...) {
      log$scored <- log$scored + 1L
      score(...)
    },
    mcmc_step = function(...) {
      log$opens <- 0L
      log$scored <- 0L
      record <- step(...)
      log$steps[[length(log$steps) + 1L]] <- list(
        opens = log$opens,
        scored = log$scored,
        legal = record$legal,
        reverse = record$reverse
      )
      record
    },
    .env = env
  )
  log
}

test_that("a constrained MCMC move opens at most two walks", {
  fixture <- wa_mcmc_fixture("coupled")
  log <- local_mcmc_counts()
  suppressWarnings(withr::with_seed(
    2L,
    fixture$augmenter$draw(1L, 1L, fixture$theta)
  ))

  legal <- vapply(log$steps, `[[`, logical(1), "legal")
  opens <- vapply(log$steps, `[[`, integer(1), "opens")
  scored <- vapply(log$steps, `[[`, integer(1), "scored")
  # The sweep reaches both kinds of move, or it guards neither.
  expect_gt(sum(legal), 0L)
  expect_gt(sum(!legal), 0L)
  expect_lte(max(opens), 2L)
  expect_identical(unique(scored[legal]), 1L)
  expect_identical(unique(scored[!legal]), 0L)
  for (step in log$steps[!legal]) {
    expect_null(step$reverse)
  }
})
