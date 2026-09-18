# Task 2.2a/2.2b (abmcem): the classed E-step object and the internal
# compute_q()/compute_ase() generics that let the ABMCEM loop stay written once,
# scheme-agnostically. The three weighting schemes (importance / resampling /
# uniform) get scheme-correct estimators (design D8): the self-normalized IS
# delta-method variance, the resampling ratio/delta form with its non-negative
# floor, and the plain sample mean / standard error of the mean. The Lambda_i
# and weights here stand in for what the analytic stub evaluator (design D11)
# yields; Q and ASE are checked against the closed-form estimator formulas.

test_that("the constructor tags the scheme-correct E-step subclass", {
  lambda <- c(-1, 0.5, 2, -0.3)
  w <- c(0.1, 0.2, 0.3, 0.4)

  is_estep <- new_dynes_estep(lambda, w, scheme = "importance")
  expect_s3_class(is_estep, "goldfishEstepIS")
  expect_s3_class(is_estep, "goldfishDynesEstep")

  uniform_estep <- new_dynes_estep(lambda, scheme = "uniform")
  expect_s3_class(uniform_estep, "goldfishEstepUniform")
  expect_s3_class(uniform_estep, "goldfishDynesEstep")

  withr::local_seed(1L)
  rs_estep <- new_dynes_estep(lambda, w, scheme = "resampling")
  expect_s3_class(rs_estep, "goldfishEstepResampling")
  expect_s3_class(rs_estep, "goldfishDynesEstep")
})

test_that("importance Q/ASE are the self-normalized IS closed forms", {
  lambda <- c(-1, 0.5, 2, -0.3)
  w <- c(0.1, 0.2, 0.3, 0.4)
  estep <- new_dynes_estep(lambda, w, scheme = "importance")

  # Q_hat = sum w_i Lambda_i.
  expect_equal(compute_q(estep), 0.48)
  # ASE^2 = sum w_i^2 (Lambda_i - Q_hat)^2 (delta-method SNIS variance).
  q <- 0.48
  expect_equal(
    as.numeric(compute_ase(estep)),
    sqrt(sum(w^2 * (lambda - q)^2))
  )
  # A sum of squares can never floor.
  expect_false(ase_floored(compute_ase(estep)))
})

test_that("uniform Q/ASE are the plain mean and standard error of the mean", {
  lambda <- c(-1, 0.5, 2, -0.3)
  estep <- new_dynes_estep(lambda, scheme = "uniform")

  # Q_hat = mean(Lambda); ASE^2 = s^2(Lambda) / K.
  expect_equal(compute_q(estep), mean(lambda))
  expect_equal(
    as.numeric(compute_ase(estep)),
    sqrt(stats::var(lambda) / length(lambda))
  )
  expect_false(ase_floored(compute_ase(estep)))
})

test_that("resampling Q/ASE follow the ratio/delta form over drawn counts", {
  lambda <- c(-1, 0.5, 2, -0.3, 1.1, -0.7)
  w <- c(0.30, 0.05, 0.25, 0.10, 0.20, 0.10)
  withr::local_seed(11L)
  estep <- new_dynes_estep(lambda, w, scheme = "resampling")

  # The one resample per pass is stored as an unweighted multiset feeding both
  # the E-step quantities and (later) the M-step batches.
  counts <- estep$counts
  k <- length(lambda)
  expect_equal(sum(counts), k)
  expect_length(estep$resample_indices, k)
  expect_type(estep$resample_indices, "integer")

  # Q_hat = K^-1 sum c_i Lambda_i.
  expect_equal(compute_q(estep), sum(counts * lambda) / k)
  # sigma^2 = Q^2 [ sum c L^2 / (sum c L)^2 - 1/K ].
  q <- sum(counts * lambda) / k
  var_est <- q^2 *
    (sum(counts * lambda^2) / sum(counts * lambda)^2 - 1 / k)
  expect_equal(as.numeric(compute_ase(estep)), sqrt(max(var_est, 0)))
})

test_that("resampling ASE floors a negative variance estimate to zero", {
  # Near-constant Lambda across the multiset: the ratio/delta bracket is 0 in
  # exact arithmetic and dips negative under floating-point roundoff (below the
  # resolution floor). rep(0.7, 5) with unit counts yields bracket ~ -2.8e-17
  # deterministically on IEEE-754. The floor must return ASE = 0, never NaN.
  lambda <- rep(0.7, 5)
  estep <- new_dynes_estep(
    lambda,
    weights = rep(0.2, 5),
    scheme = "resampling",
    resample_indices = 1:5
  )
  ase <- compute_ase(estep)
  expect_equal(as.numeric(ase), 0)
  expect_false(is.nan(as.numeric(ase)))
  expect_true(ase_floored(ase))
})

test_that("resampling does not flag a genuinely positive variance as floored", {
  # Spread, non-zero-sum Lambda over the multiset: Q = 2 and the ratio/delta
  # bracket is strictly positive, so the estimate is a real ASE, not floored.
  lambda <- c(0, 1, 2, 3, 4)
  estep <- new_dynes_estep(
    lambda,
    weights = rep(0.2, 5),
    scheme = "resampling",
    resample_indices = c(1L, 2L, 3L, 4L, 5L)
  )
  ase <- compute_ase(estep)
  expect_equal(as.numeric(ase), sqrt(0.4))
  expect_gt(as.numeric(ase), 0)
  expect_false(ase_floored(ase))
})

test_that("importance vs uniform pick different ASE estimators by dispatch", {
  lambda <- c(-1, 0.5, 2, -0.3)
  w <- c(0.1, 0.2, 0.3, 0.4)
  is_ase <- as.numeric(compute_ase(new_dynes_estep(lambda, w, "importance")))
  uni_ase <- as.numeric(compute_ase(new_dynes_estep(
    lambda,
    scheme = "uniform"
  )))
  # Different estimators on the same Lambda => different numbers.
  expect_false(isTRUE(all.equal(is_ase, uni_ase)))
})

test_that("a new E-step subclass extends the generics by method registration", {
  # The EM loop consumes only compute_q()/compute_ase(); adding a scheme means
  # adding methods, not editing the loop.
  obj <- structure(
    list(lambda = c(1, 2, 3)),
    class = c("goldfishEstepTest", "goldfishDynesEstep")
  )
  compute_q.goldfishEstepTest <- function(estep, ...) 42
  compute_ase.goldfishEstepTest <- function(estep, ...) {
    structure(7, ase_floored = FALSE)
  }
  expect_equal(compute_q(obj), 42)
  expect_equal(as.numeric(compute_ase(obj)), 7)
})

test_that("MCMC draws record a lag-1 autocorrelation index for the trace", {
  # A monotone ramp is strongly positively autocorrelated; an alternating series
  # is negatively autocorrelated; the index matches the lag-1 formula.
  ramp <- as.numeric(1:8)
  estep <- new_dynes_estep(
    ramp,
    weights = rep(1 / 8, 8),
    scheme = "importance",
    mcmc = TRUE
  )
  expect_equal(estep$autocorrelation, lag1_autocorrelation(ramp))
  expect_gt(estep$autocorrelation, 0.5)

  alt <- c(1, -1, 1, -1, 1, -1)
  expect_lt(lag1_autocorrelation(alt), 0)

  # A non-MCMC pool records no autocorrelation index.
  non_mcmc <- new_dynes_estep(ramp, weights = rep(1 / 8, 8), "importance")
  expect_true(is.na(non_mcmc$autocorrelation))
})
