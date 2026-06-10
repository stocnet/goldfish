test_that("rejected step with -Inf trial likelihood does not converge", {
  res <- check_convergence(
    score = c(-1852.2, 1136.8, 43539.8),
    log_likelihood = -77735,
    update = c(-0.25, 0.14, 0.78),
    step_accepted = FALSE,
    score_tol = 1e-6,
    step_tol = 1e-8
  )
  expect_false(res$converged)
  expect_identical(res$return_code, 0L)
})

test_that("non-finite likelihood never triggers score convergence", {
  res <- check_convergence(
    score = c(100, 200),
    log_likelihood = -Inf,
    update = c(1, 2),
    step_accepted = FALSE,
    score_tol = 1e-6,
    step_tol = 1e-8
  )
  expect_false(res$converged)
  expect_identical(res$return_code, 0L)

  res_accepted <- check_convergence(
    score = c(100, 200),
    log_likelihood = -Inf,
    update = c(1, 2),
    step_accepted = TRUE,
    score_tol = 1e-6,
    step_tol = 1e-8
  )
  expect_false(res_accepted$converged)
})

test_that("accepted step with small relative score converges with code 1", {
  res <- check_convergence(
    score = c(1e-3, -5e-4),
    log_likelihood = -18203,
    update = c(1e-5, 2e-5),
    step_accepted = TRUE,
    score_tol = 1e-6,
    step_tol = 1e-8
  )
  expect_true(res$converged)
  expect_identical(res$return_code, 1L)
  expect_equal(res$score_rel_norm, 1e-3 / 18203)
})

test_that("small relative score on a rejected step does not converge", {
  res <- check_convergence(
    score = c(1e-3, -5e-4),
    log_likelihood = -18203,
    update = c(1e-2, 2e-2),
    step_accepted = FALSE,
    score_tol = 1e-6,
    step_tol = 1e-8
  )
  expect_false(res$converged)
  expect_identical(res$return_code, 0L)
})

test_that("tiny damped update converges with code 2 even when step rejected", {
  res <- check_convergence(
    score = c(50, -30),
    log_likelihood = -18203,
    update = c(1e-10, -5e-11),
    step_accepted = FALSE,
    score_tol = 1e-6,
    step_tol = 1e-8
  )
  expect_true(res$converged)
  expect_identical(res$return_code, 2L)
})

test_that("score criterion takes precedence over step criterion", {
  res <- check_convergence(
    score = c(1e-4),
    log_likelihood = -18203,
    update = c(1e-10),
    step_accepted = TRUE,
    score_tol = 1e-6,
    step_tol = 1e-8
  )
  expect_true(res$converged)
  expect_identical(res$return_code, 1L)
})

test_that("likelihood scaling denominator is floored at 1", {
  res <- check_convergence(
    score = c(5e-7),
    log_likelihood = -0.2,
    update = c(1),
    step_accepted = TRUE,
    score_tol = 1e-6,
    step_tol = 1e-8
  )
  expect_true(res$converged)
  expect_equal(res$score_rel_norm, 5e-7)
})
