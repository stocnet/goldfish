# Task 2.1a/2.1b (abmcem): the E-step weight-state machinery driving the ABMCEM
# loop's importance weighting -- permanent per-sequence reference records, the
# Levine & Casella (2001) likelihood-ratio (own-ratio self-normalized IS)
# reweighting, the named-enum log-weight transformation, effective sample size,
# the three resampling schemes, and the ESS staleness guard with its two
# warnings. Pure functions over hand-computed fixtures; the loop wiring
# (redraw, refresh, growth) lives with the estimation loop, not here.

test_that("new_reference_record stores the log-scale triple", {
  rec <- new_reference_record(
    theta_ref = c(0.2, -0.1),
    loglik_ref = -3.5,
    log_proposal = -3.5
  )
  expect_equal(rec$theta_ref, c(0.2, -0.1))
  expect_equal(rec$loglik_ref, -3.5)
  expect_equal(rec$log_proposal, -3.5)

  # A pool's stored log proposal densities are extracted in sequence order.
  records <- list(
    new_reference_record(c(0), -1, -2),
    new_reference_record(c(0), -1, 0.5),
    new_reference_record(c(0), -1, -0.5)
  )
  expect_equal(pool_log_proposal(records), c(-2, 0.5, -0.5))
})

test_that("new_reference_record rejects non-finite scalar log quantities", {
  expect_error(new_reference_record(c(0), NA_real_, 0))
  expect_error(new_reference_record(c(0), 0, Inf))
  expect_error(new_reference_record(c(0), c(-1, -2), 0))
})

test_that("importance weights are a hand-computed self-normalized ratio", {
  # log w_i = loglik_current_i - log_proposal_i (own-ratio SNIS). With a flat
  # log proposal density the weights are the softmax of the current loglik.
  log_proposal <- c(0, 0, 0)
  loglik <- c(0, log(2), 0)

  w <- compute_pool_weights(loglik, log_proposal, weighting = "importance")
  expect_equal(w, c(0.25, 0.5, 0.25))
  expect_equal(sum(w), 1)
  # ESS = 1 / sum(w^2) = 1 / 0.375 = 8/3.
  expect_equal(effective_sample_size(w), 8 / 3)

  # A non-flat proposal enters as a subtraction on the log scale.
  log_proposal2 <- c(log(2), 0, log(0.5))
  loglik2 <- c(log(4), log(2), log(2))
  # log w = c(log2, log2, log4) -> exp c(2, 2, 4) -> c(0.25, 0.25, 0.5).
  expect_equal(
    compute_pool_weights(loglik2, log_proposal2, weighting = "importance"),
    c(0.25, 0.25, 0.5)
  )
})

test_that("reweighting a persistent pool is a likelihood ratio, not a redraw", {
  # A pool drawn at theta_k, then re-evaluated at theta_{k+1}: the weights are
  # recomputed from the SAME stored reference records and the new loglik, with
  # no new sequences generated: reweighting is a likelihood ratio, not a redraw.
  records <- list(
    new_reference_record(c(0, 0), loglik_ref = 0, log_proposal = 0),
    new_reference_record(c(0, 0), loglik_ref = 0, log_proposal = 0),
    new_reference_record(c(0, 0), loglik_ref = 0, log_proposal = 0)
  )
  before <- records

  loglik_next <- c(log(3), log(1), log(2))
  w <- compute_pool_weights(
    loglik_next,
    pool_log_proposal(records),
    weighting = "importance"
  )
  # exp(c(log3, 0, log2)) = c(3, 1, 2), sum 6.
  expect_equal(w, c(0.5, 1 / 6, 1 / 3))
  # ESS = 1 / (0.25 + 1/36 + 1/9) = 1 / 0.3888889.
  expect_equal(effective_sample_size(w), 1 / (0.25 + 1 / 36 + 1 / 9))
  # The reference records are untouched by reweighting.
  expect_identical(records, before)
})

test_that("uniform weighting ignores the proposal and gives ESS = K", {
  loglik <- c(-1, -5, -2, -9)
  w <- compute_pool_weights(
    loglik,
    log_proposal = c(0, 0, 0, 0),
    weighting = "uniform"
  )
  expect_equal(w, rep(0.25, 4))
  expect_equal(effective_sample_size(w), 4)
})

test_that("smoothing transformation equals normalized w^gamma (gamma = 2)", {
  log_proposal <- c(0, 0, 0)
  loglik <- c(0, log(2), log(3))
  base_w <- compute_pool_weights(loglik, log_proposal, weighting = "importance")

  smoothed <- compute_pool_weights(
    loglik,
    log_proposal,
    weighting = "importance",
    transformation = "smoothing"
  )
  # softmax(2 * log w) == normalize(w^2).
  expect_equal(smoothed, base_w^2 / sum(base_w^2))
})

test_that("clipping transformation caps the dominant weight", {
  log_proposal <- rep(0, 5)
  # One sequence dominates: log-weight far above the rest.
  loglik <- c(0, 0.1, 0.2, 0.3, 8)
  base_w <- compute_pool_weights(loglik, log_proposal, weighting = "importance")
  clipped <- compute_pool_weights(
    loglik,
    log_proposal,
    weighting = "importance",
    transformation = "clipping"
  )
  # Winsorizing the top log-weight lowers the dominant normalized weight.
  expect_lt(max(clipped), max(base_w))
  expect_equal(sum(clipped), 1)

  # Clipping is a no-op when the log-weights are already flat.
  flat <- compute_pool_weights(
    rep(0, 4),
    rep(0, 4),
    weighting = "importance",
    transformation = "clipping"
  )
  expect_equal(flat, rep(0.25, 4))
})

test_that("non-finite per-sequence loglik normalizes its weight to zero", {
  # A degenerate (singular-information) sequence yields a non-finite loglik; its
  # weight must vanish rather than poison the pool.
  log_proposal <- c(0, 0, 0)
  loglik <- c(0, log(2), -Inf)
  w <- compute_pool_weights(loglik, log_proposal, weighting = "importance")
  expect_equal(w, c(1 / 3, 2 / 3, 0))
  expect_equal(sum(w), 1)
})

test_that("resampling schemes return a length-K multiset of pool indices", {
  weights <- c(0.5, 0.3, 0.15, 0.05)
  for (scheme in c("stratified", "residual", "random")) {
    withr::local_seed(1L)
    idx <- resample_pool(weights, scheme = scheme)
    expect_length(idx, length(weights))
    expect_true(all(idx %in% seq_along(weights)))
    expect_type(idx, "integer")
  }
})

test_that("residual resampling forces the deterministic integer copies", {
  # weights * K = c(1.5, 1.5, 0): floor gives one guaranteed copy each of 1, 2
  # and none of 3; the single residual draw comes from {1, 2}.
  withr::local_seed(42L)
  idx <- resample_pool(c(0.5, 0.5, 0), scheme = "residual")
  expect_length(idx, 3L)
  expect_gte(sum(idx == 1L), 1L)
  expect_gte(sum(idx == 2L), 1L)
  expect_equal(sum(idx == 3L), 0L)
})

test_that("resampling is reproducible under a fixed seed", {
  weights <- c(0.4, 0.3, 0.2, 0.1)
  withr::local_seed(7L)
  a <- resample_pool(weights, scheme = "stratified")
  withr::local_seed(7L)
  b <- resample_pool(weights, scheme = "stratified")
  expect_identical(a, b)
})

test_that("effective_sample_size floors a fully degenerate pool at zero", {
  expect_equal(effective_sample_size(c(0, 0, 0)), 0)
  expect_equal(effective_sample_size(c(1, 0, 0)), 1)
})

test_that("ESS guard fires and requests a redraw below the enabled threshold", {
  local_reproducible_output()
  # w = c(0.98, 0.01, 0.01): ESS ~= 1.041 < 0.5 * 3 = 1.5.
  degenerate <- c(0.98, 0.01, 0.01)
  expect_snapshot(
    decision <- ess_guard(degenerate, ess_threshold = 0.5, enabled = TRUE)
  )
  expect_true(decision$redraw)
  expect_true(decision$below_threshold)
})

test_that("ESS guard informs without redrawing when disabled", {
  local_reproducible_output()
  degenerate <- c(0.98, 0.01, 0.01)
  expect_snapshot(
    decision <- ess_guard(degenerate, ess_threshold = 0.5, enabled = FALSE)
  )
  expect_false(decision$redraw)
  expect_true(decision$below_threshold)
})

test_that("ESS guard is silent above the threshold and at startup", {
  # Above the threshold: no warning, no redraw.
  healthy <- c(0.34, 0.33, 0.33)
  expect_no_warning(
    decision <- ess_guard(healthy, ess_threshold = 0.5, enabled = TRUE)
  )
  expect_false(decision$redraw)
  expect_false(decision$below_threshold)

  # Never at startup, even when degenerate.
  degenerate <- c(0.98, 0.01, 0.01)
  expect_no_warning(
    startup <- ess_guard(
      degenerate,
      ess_threshold = 0.5,
      enabled = TRUE,
      at_startup = TRUE
    )
  )
  expect_false(startup$redraw)
})
