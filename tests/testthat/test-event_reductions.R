# The shared per-event reductions (src/event_reductions.h). Every backend forms
# a weight vector `w` over the event's risk set and a scale `c`, and the
# primitives are reductions of that pair: rank counts strictly-greater weights,
# margins accumulate c * w by actor, and the score is X_obs - c * w'X. These
# tests drive the C++ helpers directly through the test-only probe, so a
# reduction bug shows up here rather than as a coefficient mismatch nine kernels
# away.
#
# Two contract points the fixtures honour, because every kernel does: `obs` is
# 0-based, and `w` is ZERO outside the risk set (stable_softmax_masked returns 0
# for masked entries, REM zeroes explicitly, the rate kernel only writes active
# senders, gather slices are pre-masked). `allowed` is then only a skip hint.

probe <- getFromNamespace("event_reductions_probe", "goldfish")

# A single event: 5 alternatives, 3 effects, position 3 (1-based) outside the
# risk set and therefore carrying weight 0, as a kernel would hand it over.
reductions_fixture <- function() {
  list(
    X = matrix(
      c(
        0.5,
        -1.2,
        0.3,
        1.1,
        0.4,
        -0.7,
        -0.2,
        0.9,
        1.5,
        0.8,
        -0.5,
        0.2,
        -1.0,
        0.6,
        -0.4
      ),
      nrow = 5,
      ncol = 3,
      byrow = TRUE
    ),
    w = c(2, 5, 0, 4, 3),
    allowed = c(1, 1, 0, 1, 1)
  )
}

reduce <- function(fx, c, obs, dependent = TRUE, ...) {
  args <- list(
    X = fx$X,
    w = fx$w,
    c = c,
    obs = obs,
    allowed = fx$allowed,
    dependent = dependent,
    index_a = integer(0),
    index_b = integer(0),
    n_a = length(fx$w),
    n_b = 0L
  )
  do.call(probe, utils::modifyList(args, list(...)))
}

test_that("rank counts strictly greater weights", {
  fx <- reductions_fixture()
  # weights 2 5 0 4 3
  expect_equal(reduce(fx, 1, obs = 1L)$rank, 1L) # w = 5, the largest
  expect_equal(reduce(fx, 1, obs = 3L)$rank, 2L) # w = 4, only 5 beats it
  expect_equal(reduce(fx, 1, obs = 0L)$rank, 4L) # w = 2, beaten by 5, 4, 3
})

test_that("tied weights share the better rank", {
  fx <- reductions_fixture()
  fx$w <- c(4, 4, 0, 4, 3)
  # Strict `>` means none of the three tied alternatives outranks another.
  for (obs in c(0L, 1L, 3L)) {
    expect_equal(reduce(fx, 1, obs = obs)$rank, 1L, info = paste("obs", obs))
  }
})

test_that("the mask is a skip hint, not a semantic switch", {
  # Given the zeroed-w contract, supplying `allowed` or not must give identical
  # results -- it only lets the accumulation loop step over known zeros. This is
  # the property that lets event_score_row() omit the mask entirely.
  fx <- reductions_fixture()
  c_prob <- 1 / sum(fx$w)
  with_mask <- reduce(fx, c = c_prob, obs = 3L)
  without <- reduce(fx, c = c_prob, obs = 3L, allowed = numeric(0))
  expect_equal(with_mask$rank, without$rank)
  expect_equal(with_mask$expected_a, without$expected_a)
  expect_equal(with_mask$observed_a, without$observed_a)
  expect_equal(with_mask$score, without$score)
})

test_that("margins on the probability scale total one per event", {
  fx <- reductions_fixture()
  res <- reduce(fx, c = 1 / sum(fx$w), obs = 3L)
  expect_equal(sum(res$expected_a), 1)
  expect_equal(as.vector(res$expected_a)[3], 0) # outside the risk set
  expect_equal(as.vector(res$observed_a), c(0, 0, 0, 1, 0))
})

test_that("margins on the expected-count scale carry the interval", {
  fx <- reductions_fixture()
  timespan <- 2.5
  res <- reduce(fx, c = timespan, obs = 3L)
  # sum_j c * w_j = Dt * total_rate over the risk set.
  expect_equal(sum(res$expected_a), timespan * sum(fx$w))
})

test_that("a right-censored interval contributes no observed count", {
  fx <- reductions_fixture()
  res <- reduce(fx, c = 2.5, obs = 3L, dependent = FALSE)
  expect_equal(as.vector(res$observed_a), rep(0, 5))
  # ...but its expected contribution still accumulates.
  expect_gt(sum(res$expected_a), 0)
})

test_that("two sides scatter one contribution into both accumulators", {
  # The REM / REM_ordered / MM shape: each risk-set position is a dyad, feeding
  # a sender margin and a receiver margin from the same contribution, in one
  # pass over the risk set.
  fx <- reductions_fixture()
  sender <- c(0L, 0L, 1L, 1L, 1L) # dyad -> sender slot
  receiver <- c(0L, 1L, 0L, 1L, 2L) # dyad -> receiver slot
  res <- reduce(
    fx,
    c = 1 / sum(fx$w),
    obs = 3L,
    index_a = sender,
    index_b = receiver,
    n_a = 2L,
    n_b = 3L
  )
  # Both sides see the same total, since each is a regrouping of one pass.
  expect_equal(sum(res$expected_a), 1)
  expect_equal(sum(res$expected_b), 1)
  # The observed dyad (position 3) is sender 1, receiver 1 in 0-based slots.
  expect_equal(as.vector(res$observed_a), c(0, 1))
  expect_equal(as.vector(res$observed_b), c(0, 1, 0))
})

test_that("the score is the observed statistic minus the weighted mean", {
  fx <- reductions_fixture()
  c_prob <- 1 / sum(fx$w)
  expected_mean <- c_prob * as.vector(fx$w %*% fx$X)

  res <- reduce(fx, c = c_prob, obs = 3L)
  expect_equal(as.vector(res$score), fx$X[4, ] - expected_mean)

  # A right-censored interval contributes the weighted mean only.
  res_rc <- reduce(fx, c = c_prob, obs = 3L, dependent = FALSE)
  expect_equal(as.vector(res_rc$score), -expected_mean)
})
