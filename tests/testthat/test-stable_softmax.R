# Numerical-behavior tests for the in-house single-pass stable softmax and the
# four multinomial contributions routed through it. Benign 1e-10
# agreement is covered by the golden fixtures (test-likelihood_equivalence.R)
# and the per-event consistency gate (test-process_state_evaluators.R); here we
# assert the NEW behavior the stabilization adds: finite logL / score /
# information under overflow, and a finite observed log-likelihood when its
# probability underflows (the old code returned -Inf / NaN). The timed rate/REM
# hazard path is a Non-Goal and must stay on plain exp() — verified to still
# overflow to non-finite values on an extreme fixture.

stable_softmax <- getFromNamespace("stable_softmax", "goldfish")
getMultinomialProbabilities <-
  getFromNamespace("getMultinomialProbabilities", "goldfish")

# mathematically-correct references, computed independently of the package
stable_lse <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}
naive_softmax <- function(x) exp(x) / sum(exp(x))

# single-parameter stats whose linear predictor equals `predictor` (parameter 1)
predictor_stats_2d <- function(predictor) matrix(predictor, ncol = 1L)
predictor_stats_3d <- function(predictorMatrix) {
  array(predictorMatrix, dim = c(dim(predictorMatrix), 1L))
}

choice_fn <-
  getFromNamespace(
    "compute_event_contribution.goldfishKindDnChoice",
    "goldfish"
  )
rem_ordered_fn <-
  getFromNamespace(
    "compute_event_contribution.goldfishKindRemCox",
    "goldfish"
  )
rate_ordered_fn <-
  getFromNamespace(
    "compute_event_contribution.goldfishKindDnCox",
    "goldfish"
  )
coord_fn <-
  getFromNamespace(
    "compute_event_contribution.goldfishKindDnCoord",
    "goldfish"
  )

# --- stable_softmax unit behavior -------------------------------------------

test_that("stable_softmax matches naive softmax on benign input", {
  set.seed(1)
  x <- stats::rnorm(8, sd = 2)
  sm <- stable_softmax(x)
  expect_equal(sm$probabilities, naive_softmax(x), tolerance = 1e-12)
  expect_equal(sm$logProbabilities, x - stable_lse(x), tolerance = 1e-12)

  m <- matrix(stats::rnorm(12, sd = 2), 3, 4)
  smRow <- stable_softmax(m, rowwise = TRUE)
  expect_equal(
    smRow$probabilities,
    exp(m) / rowSums(exp(m)),
    tolerance = 1e-12
  )
  expect_equal(rowSums(smRow$probabilities), rep(1, 3), tolerance = 1e-12)
})

test_that("stable_softmax stays finite under overflow", {
  x <- c(1e6, 1e6 + 2, 1e6 + 1)
  sm <- stable_softmax(x)
  expect_true(all(is.finite(sm$probabilities)))
  expect_equal(sum(sm$probabilities), 1, tolerance = 1e-12)
  # shift-invariant reference; the naive form overflows to NaN
  ref <- {
    y <- x - max(x)
    exp(y) / sum(exp(y))
  }
  expect_equal(sm$probabilities, ref, tolerance = 1e-12)
  expect_true(any(is.nan(exp(x) / sum(exp(x)))))
})

test_that("stable_softmax gives a finite log-prob for an underflowing event", {
  # alternative 1 has vanishing probability next to the dominant alternative 2
  x <- c(0, 1000)
  sm <- stable_softmax(x)
  expect_equal(sm$probabilities[1], 0) # underflows to exactly 0
  expect_true(is.finite(sm$logProbabilities[1]))
  expect_equal(sm$logProbabilities[1], x[1] - stable_lse(x), tolerance = 1e-9)
  # what the earlier code computed instead:
  expect_true(is.infinite(log(sm$probabilities[1])))
})

test_that("stable_softmax fully-excluded row maps to 0 / -Inf", {
  m <- rbind(c(2, 1, 0.5), c(-Inf, -Inf, -Inf))
  sm <- stable_softmax(m, rowwise = TRUE)
  expect_equal(sm$probabilities[2, ], c(0, 0, 0))
  expect_true(all(sm$logProbabilities[2, ] == -Inf))
  expect_equal(sum(sm$probabilities[1, ]), 1, tolerance = 1e-12)
})

# --- DyNAM-choice / REM-ordered / rate-ordered contributions -----------------

test_that("DyNAM-choice contribution is finite and correct under overflow", {
  predictor <- c(0, 1e6, 5, 1e6 - 3, 2)
  statsArray <- predictor_stats_2d(predictor)
  observed <- 2L # the dominant alternative
  res <- choice_fn(
    spec = NULL,
    statsArray,
    activeDyad = c(1L, observed),
    parameters = 1,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = TRUE,
    is_two_mode = FALSE
  )
  expect_true(is.finite(res$logLikelihood))
  expect_equal(
    res$logLikelihood,
    predictor[observed] - stable_lse(predictor),
    tolerance = 1e-9
  )
  expect_true(all(is.finite(res$score)))
  expect_true(all(is.finite(res$informationMatrix)))
  expect_equal(sum(res$pMatrix), 1, tolerance = 1e-12)
})

test_that("DyNAM-choice observed logL is finite when it underflows", {
  predictor <- c(0, 900, 5, 3, 2) # observe the vanishing alternative 1
  statsArray <- predictor_stats_2d(predictor)
  res <- choice_fn(
    spec = NULL,
    statsArray,
    activeDyad = c(3L, 1L),
    parameters = 1,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = TRUE,
    is_two_mode = FALSE
  )
  expect_true(is.finite(res$logLikelihood))
  expect_equal(
    res$logLikelihood,
    predictor[1] - stable_lse(predictor),
    tolerance = 1e-9
  )
  expect_equal(res$pMatrix[1], 0) # probability underflowed, logL did not
})

test_that("REM-ordered contribution is finite and correct under overflow", {
  set.seed(2)
  predictorMatrix <- matrix(stats::rnorm(16), 4, 4)
  predictorMatrix[2, 3] <- 1e6 # a dominant dyad
  statsArray <- predictor_stats_3d(predictorMatrix)
  observed <- c(2L, 3L)
  res <- rem_ordered_fn(
    spec = NULL,
    statsArray,
    activeDyad = observed,
    parameters = 1,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = FALSE,
    is_two_mode = FALSE
  )
  expect_true(is.finite(res$logLikelihood))
  expect_equal(
    res$logLikelihood,
    predictorMatrix[2, 3] - stable_lse(as.vector(predictorMatrix)),
    tolerance = 1e-9
  )
  expect_true(all(is.finite(res$score)))
  expect_true(all(is.finite(res$informationMatrix)))
  expect_equal(sum(res$pMatrix), 1, tolerance = 1e-12)
})

test_that("rate-ordered contribution is finite and correct under overflow", {
  predictor <- c(3, 1e6, 0, 2, 1e6 - 4)
  statsArray <- predictor_stats_2d(predictor)
  observed <- 1L # an underflowing sender
  res <- rate_ordered_fn(
    spec = NULL,
    statsArray,
    activeDyad = c(observed, NA),
    parameters = 1,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = TRUE,
    is_two_mode = FALSE
  )
  expect_true(is.finite(res$logLikelihood))
  expect_equal(
    res$logLikelihood,
    predictor[observed] - stable_lse(predictor),
    tolerance = 1e-9
  )
  expect_true(all(is.finite(res$score)))
  expect_true(all(is.finite(res$informationMatrix)))
})

# --- DyNAM-choice-coordination (two-sided) -----------------------------------

test_that("coordination contribution is finite where the reference is not", {
  # predictors in [0, 700]: exp() does not overflow, but the observed dyad's
  # symmetric weight P(i->j)*P(j->i) underflows to 0, so the reference's
  # log(eventLikelihoods[i,j]) is -Inf while the log-space form stays finite.
  set.seed(3)
  n <- 4L
  predictorMatrix <- matrix(0, n, n)
  # dominant dyads (1,2) and (3,4)
  predictorMatrix[cbind(1:n, c(2L, 1L, 4L, 3L))] <- 700
  statsArray <- predictor_stats_3d(predictorMatrix)
  observed <- c(1L, 3L) # a vanishing off-dominant dyad
  args <- list(
    spec = NULL,
    statsArray,
    activeDyad = observed,
    parameters = 1,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = FALSE,
    is_two_mode = FALSE
  )
  live <- do.call(coord_fn, args)

  # Pre-stabilization contribution formed the symmetric dyad weight
  # P(i->j) * P(j->i) in probability space, so an underflowing observed dyad
  # drove log(eventLikelihoods[i, j]) to -Inf. Reconstruct that old value from
  # the per-actor multinomial probabilities to show the log-space form fixes a
  # genuine -Inf, without depending on a frozen reference implementation.
  probabilities <- getMultinomialProbabilities(
    statsArray,
    observed,
    parameters = 1,
    allowReflexive = FALSE
  )$probabilities
  symmetricWeights <- probabilities * t(probabilities)
  diag(symmetricWeights) <- 0
  oldLogLikelihood <- log(
    (symmetricWeights / (sum(symmetricWeights) / 2))[observed[1], observed[2]]
  )

  expect_true(is.finite(live$logLikelihood))
  expect_false(is.finite(oldLogLikelihood)) # old code: -Inf
  expect_true(all(is.finite(live$score)))
  expect_true(all(is.finite(live$informationMatrix)))
  expect_true(all(is.finite(live$pMatrix)))
})

test_that("coordination contribution stays finite under overflow", {
  set.seed(4)
  n <- 4L
  predictorMatrix <- matrix(stats::rnorm(n * n), n, n)
  predictorMatrix[2, 3] <- 1e6
  predictorMatrix[3, 2] <- 1e6
  statsArray <- predictor_stats_3d(predictorMatrix)
  res <- coord_fn(
    spec = NULL,
    statsArray,
    activeDyad = c(2L, 3L),
    parameters = 1,
    isRightCensored = FALSE,
    timespan = 1,
    allowReflexive = FALSE,
    is_two_mode = FALSE
  )
  expect_true(is.finite(res$logLikelihood))
  expect_true(all(is.finite(res$score)))
  expect_true(all(is.finite(res$informationMatrix)))
})

# --- Non-Goal: the timed hazard path keeps plain exp() -----------------------

test_that("timed rate/REM hazard path is unchanged (not stabilized)", {
  live_rate <- getFromNamespace("event_contribution_rate", "goldfish")
  set.seed(5)
  n1 <- 4L
  n2 <- 4L
  p <- 2L
  statsArray <- array(stats::rnorm(n1 * n2 * p), dim = c(n1, n2, p))
  activeDyad <- c(2L, 3L)
  # extreme parameters drive exp() to overflow; the timed hazard path is a
  # Non-Goal for stabilization, so it must still overflow to non-finite
  # likelihood / score / information here (a stabilized path would instead
  # return finite values, which this test deliberately forbids).
  extreme <- c(1e4, -1e4)
  live <- live_rate(
    statsArray,
    activeDyad,
    extreme,
    FALSE,
    1.5,
    TRUE,
    is_two_mode = FALSE,
    isREM = TRUE
  )
  expect_false(is.finite(live$logLikelihood))
  expect_false(all(is.finite(live$score)))
  expect_false(all(is.finite(live$informationMatrix)))
})
