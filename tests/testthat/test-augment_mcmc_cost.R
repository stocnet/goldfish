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
        accepted = record$accepted,
        log_alpha = record$log_alpha,
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

# The chain's decisions over two seeded sweeps, recorded with the legality
# check and the reverse rate queries on separate walks. No reference fixture
# has a stamp tie, so sharing one walk moves none of them. Flags are written
# one character per step.
# fmt: skip
mcmc_reference_steps <- list(
  coupled = list(
    legal = "TTTFFTTTTTFTTTTTFTTFTT",
    accepted = "FFTFFTTTFFFFTTTFFTTFTF",
    log_alpha = c(
      -1.00524659051772, -2.67913112985473, 0.373701914110254, -Inf, -Inf,
      -0.216331025807926, 0.0681057008002144, -0.0632984286263305,
      -2.37955155461721, -2.6465104893462, -Inf, -5.88201024927037,
      -0.307932154469194, 0.0833923558606005, 0.02476198627516,
      -1.86288144134038, -Inf, 0.37845151316728, 0.0120939599854681, -Inf,
      1.00485498205469, -4.32569868974426
    ),
    loglik = -356.506098363797
  ),
  uncoupled = list(
    legal = strrep("T", 24L),
    accepted = "FTTFTFTTFTTTTTFTTTTTFTFT",
    log_alpha = c(
      -0.099657796047854, 7.105427357601e-15, 0.0220620795982818,
      -2.62599878122384, -0.339871372811578, -0.0866213712856245,
      1.72783596245239, 0.0189017258587532, -2.70333403713056,
      -0.285832257629457, -0.00430831812301413, -0.00584245542785844,
      0.00113904832407918, -0.00591446035125998, -0.354098159179492,
      -0.160519088244659, 0.000939685737813889, -0.00464722818653485,
      0.0496799623928221, 0.495549672568346, -1.63082131639527,
      4.88498130835069e-15, -1.50923912384862, 0.243059652063193
    ),
    loglik = -425.564645023137
  )
)

flags <- function(steps, field) {
  paste(ifelse(vapply(steps, `[[`, logical(1), field), "T", "F"), collapse = "")
}

# Two seeded sweeps of the reference fixture `name`: each step's counts and
# decisions, and the retained draw.
seeded_mcmc_sweeps <- function(name) {
  fixture <- wa_mcmc_fixture(name, thinning = 2L)
  log <- local_mcmc_counts()
  draw <- suppressWarnings(withr::with_seed(
    2L,
    fixture$augmenter$draw(1L, 1L, fixture$theta)
  ))[[1L]]
  list(steps = log$steps, draw = draw)
}

test_that("a seeded chain makes the same decisions on two walks per move", {
  for (name in names(mcmc_reference_steps)) {
    reference <- mcmc_reference_steps[[name]]
    run <- seeded_mcmc_sweeps(name)
    expect_identical(flags(run$steps, "legal"), reference$legal)
    expect_identical(flags(run$steps, "accepted"), reference$accepted)
    expect_equal(
      vapply(run$steps, `[[`, numeric(1), "log_alpha"),
      reference$log_alpha
    )
    expect_equal(run$draw$loglik, reference$loglik)
    expect_lte(max(vapply(run$steps, `[[`, integer(1), "opens")), 2L)
  }
})
