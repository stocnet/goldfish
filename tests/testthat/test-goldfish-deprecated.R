test_that("set_estimation_opt() forwards to set_algorithm_newton()", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_identical(
    set_estimation_opt(max_iterations = 5, engine = "default"),
    set_algorithm_newton(max_iterations = 5, engine = "default")
  )
  expect_s3_class(
    set_estimation_opt(),
    c("algorithm_newton.goldfish", "algorithm.goldfish", "list"),
    exact = TRUE
  )
})

test_that("set_preprocessing_opt() forwards to set_preprocessing()", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_identical(
    set_preprocessing_opt(start_time = 10),
    set_preprocessing(start_time = 10)
  )
  expect_s3_class(
    set_preprocessing_opt(),
    c("preprocessing.goldfish", "list"),
    exact = TRUE
  )
})

test_that("the renamed constructors soft-deprecate onto their new names", {
  expect_snapshot({
    invisible(set_estimation_opt())
    invisible(set_preprocessing_opt())
  })
})

test_that("the renamed estimator arguments still work, with a warning", {
  withr::local_options(lifecycle_verbosity = "quiet")
  formula_test <- depNetwork ~ inertia + recip
  expect_equal(
    coef(estimate_dynam(
      formula_test,
      sub_model = "choice",
      data = dataTest,
      control_estimation = set_algorithm_newton(max_iterations = 3)
    )),
    coef(estimate_dynam(
      formula_test,
      sub_model = "choice",
      data = dataTest,
      control_algo = set_algorithm_newton(max_iterations = 3)
    ))
  )
})

test_that("the new estimator argument wins when both are supplied", {
  withr::local_options(lifecycle_verbosity = "quiet")
  both <- suppressWarnings(estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(max_iterations = 2),
    control_estimation = set_algorithm_newton(max_iterations = 30)
  ))
  only_new <- suppressWarnings(estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(max_iterations = 2)
  ))
  expect_equal(both$nIterations, only_new$nIterations)
  expect_equal(coef(both), coef(only_new))
})

test_that("renamed estimator arguments soft-deprecate onto their new names", {
  expect_snapshot({
    invisible(estimate_dynam(
      depNetwork ~ inertia,
      sub_model = "choice",
      data = dataTest,
      control_estimation = set_algorithm_newton(max_iterations = 1),
      control_preprocessing = set_preprocessing(),
      preprocessing_init = NULL
    ))
  })
})

test_that("a stale preprocessed object is rejected through preprocessed =", {
  withr::local_options(lifecycle_verbosity = "quiet")
  stale <- estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    preprocessing_only = TRUE
  )
  stale$version <- 0L
  expect_snapshot(
    error = TRUE,
    estimate_dynam(
      depNetwork ~ inertia,
      sub_model = "choice",
      data = dataTest,
      preprocessed = stale
    )
  )
})

test_that("an alias reached from inside goldfish stays quiet", {
  # deprecate_soft() warns the direct caller only, so an alias reached from
  # package code stays silent. Under testthat lifecycle warns unconditionally,
  # so the production path is what has to be exercised here: unset TESTTHAT and
  # leave the verbosity option unset.
  withr::local_envvar(TESTTHAT = "")
  withr::local_options(lifecycle_verbosity = NULL)
  caller <- function() set_estimation_opt(max_iterations = 5)
  environment(caller) <- asNamespace("goldfish")
  expect_no_warning(caller())
})
