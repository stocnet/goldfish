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
