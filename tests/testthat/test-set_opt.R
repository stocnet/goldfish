test_that("set_estimation_opt works correctly", {
  expected_est_names <- c(
    "initial_parameters", "fixed_parameters", "max_iterations",
    "convergence_criterion", "initial_damping", "damping_increase_factor",
    "damping_decrease_factor", "return_interval_loglik",
    "return_probabilities", "engine"
  )

  # Test defaults
  default_opts <- set_estimation_opt()
  expect_s3_class(default_opts, c("estimation_opt.goldfish", "list"),
                  exact = TRUE)
  expect_true(is.list(default_opts))
  # Check that all expected names are present
  expect_true(all(expected_est_names %in% names(default_opts)))

  expect_equal(default_opts$engine, "default_c") # Default engine is default_c
  expect_equal(default_opts$max_iterations, 20)
  expect_equal(default_opts$convergence_criterion, 0.001) # Default criterion
  expect_null(default_opts$initial_damping) # Defaults to NULL
  expect_equal(default_opts$damping_increase_factor, 2)
  expect_equal(default_opts$damping_decrease_factor, 3)
  expect_false(default_opts$return_interval_loglik)
  expect_false(default_opts$return_probabilities)

  # Test setting specific parameters
  custom_opts <- set_estimation_opt(
    max_iterations = 50,
    engine = "default", # Change from default_c
    convergence_criterion = 1e-5,
    return_interval_loglik = TRUE,
    return_probabilities = TRUE,
    initial_damping = 15
  )
  expect_s3_class(custom_opts, c("estimation_opt.goldfish", "list"),
                  exact = TRUE)
  expect_true(is.list(custom_opts))
  # Check that all expected names are present
  expect_true(all(expected_est_names %in% names(custom_opts)))
  expect_equal(custom_opts$max_iterations, 50)
  expect_equal(custom_opts$engine, "default")
  expect_equal(custom_opts$convergence_criterion, 1e-5)
  expect_true(custom_opts$return_interval_loglik)
  expect_true(custom_opts$return_probabilities)
  expect_equal(custom_opts$initial_damping, 15)
  # Check a default value that wasn't changed is still there
  expect_equal(custom_opts$damping_increase_factor, 2)
})
test_that("set_estimation_opt throw errors", {
  expect_error(set_estimation_opt(engine = "invalid"))
  expect_error(set_estimation_opt(max_iterations = -1))
  expect_error(set_estimation_opt(convergence_criterion = -1))
  expect_error(set_estimation_opt(initial_damping = -1))
  expect_error(set_estimation_opt(damping_increase_factor = -1))
  expect_error(set_estimation_opt(damping_decrease_factor = -1))
  expect_error(set_estimation_opt(return_interval_loglik = -1))
  expect_error(set_estimation_opt(return_probabilities = -1))
  expect_error(set_estimation_opt(fixed_parameters = character(3)))
  expect_error(set_estimation_opt(initial_parameters = character(3)))
})
test_that("set_preprocessing_opt works correctly", {
  expected_prep_names <- c(
    "start_time", "end_time", "opportunities_list"
  )

  # Test defaults
  default_opts <- set_preprocessing_opt()
  expect_s3_class(default_opts, c("preprocessing_opt.goldfish", "list"),
                  exact = TRUE)
  expect_true(is.list(default_opts))
  # Check that all expected names are present
  expect_true(all(expected_prep_names %in% names(default_opts)))
  expect_null(default_opts$start_time)
  expect_null(default_opts$end_time)
  expect_null(default_opts$opportunities_list)

  # Test setting specific parameters
  dummy_opportunities <- list(c("A", "B"), c("C", "D"))
  custom_opts <- set_preprocessing_opt(
    start_time = 10,
    end_time = 100,
    opportunities_list = dummy_opportunities
  )
  expect_s3_class(custom_opts, c("preprocessing_opt.goldfish", "list"),
                  exact = TRUE)
  expect_true(is.list(custom_opts))
  # Check that all expected names are present
  expect_true(all(expected_prep_names %in% names(custom_opts)))
  expect_equal(custom_opts$start_time, 10)
  expect_equal(custom_opts$end_time, 100)
  expect_equal(custom_opts$opportunities_list, dummy_opportunities)
})
test_that("set_preprocessing_opt throw errors", {
  expect_error(set_preprocessing_opt(start_time = character(3)))
  expect_error(set_preprocessing_opt(end_time = character(3)))
  expect_error(set_preprocessing_opt(opportunities_list = -1))
})
