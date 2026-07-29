test_that("diagnostic methods throw errors when intervalLogLikelihood isn't present", {
  mod00 <- estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0L),
    control_algo = set_algorithm_newton(return_interval_loglik = FALSE),
    progress = FALSE,
    verbose = FALSE
  )
  expect_snapshot(
    error = TRUE,
    diagnose_outliers(mod00, method = "Top", threshold = 2)
  )
  expect_snapshot(
    error = TRUE,
    diagnose_changepoints(mod00, moment = "mean", method = "PELT")
  )
})

test_that("diagnostic methods does not accept non-result objects", {
  expect_snapshot(
    error = TRUE,
    diagnose_outliers(depNetwork, method = "Top", threshold = 2)
  )
  expect_snapshot(
    error = TRUE,
    diagnose_changepoints(depNetwork, moment = "mean", method = "PELT")
  )
})


test_that("diagnostic methods work on \"choice\" models.", {
  mod00 <- estimate_dynam(
    depNetwork ~ inertia + trans + indeg,
    sub_model = "choice",
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0L),
    control_algo = set_algorithm_newton(return_interval_loglik = TRUE),
    progress = FALSE,
    verbose = FALSE
  )

  p1 <- diagnose_outliers(mod00, method = "Top", threshold = 2)
  expect_s3_class(p1, "diagnose_outliers")
  expect_equal(sum(p1$outlier), 2)
  p11 <- diagnose_outliers(mod00, method = "IQR")
  expect_s3_class(p11, "diagnose_outliers")
  p2 <- diagnose_changepoints(mod00, moment = "mean", method = "PELT")
  expect_s3_class(p2, "diagnose_changepoints")
  p21 <- diagnose_changepoints(
    mod00,
    moment = "variance",
    method = "PELT",
    window = 2
  )
  expect_s3_class(p21, "diagnose_changepoints")
})

test_that("diagnostic methods work on \"rate\" models.", {
  data("Social_Evolution")
  call_network <- make_network(nodes = actors, directed = TRUE)
  friendshipNetwork <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  friendshipNetwork <- link_events(
    x = friendshipNetwork,
    change_event = friendship,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  social_evolution_data <- make_data(
    calls_dependent,
    call_network,
    friendshipNetwork,
    calls,
    actors
  )

  # this block is required for diagnose_changepoints otherwise "calls_dependent" throws an error
  assign("calls_dependent", calls_dependent, envir = .GlobalEnv)
  on.exit(rm(calls_dependent, envir = .GlobalEnv))

  mod00 <- estimate_dynam(
    calls_dependent ~ 1 + indeg + outdeg + indeg(friendshipNetwork),
    sub_model = "rate",
    data = social_evolution_data,
    control_prep = set_preprocessing(start_time = 0L),
    control_algo = set_algorithm_newton(return_interval_loglik = TRUE),
    progress = FALSE,
    verbose = FALSE
  )

  p1 <- diagnose_outliers(mod00, method = "Top", threshold = 2)
  expect_s3_class(p1, "diagnose_outliers")
  p2 <- diagnose_changepoints(mod00, moment = "mean", method = "PELT")
  expect_s3_class(p2, "diagnose_changepoints")
})

test_that("diagnostic methods work on \"rem\" models.", {
  data("Social_Evolution")
  call_network <- make_network(nodes = actors, directed = TRUE)
  friendshipNetwork <- make_network(nodes = actors, directed = TRUE)
  call_network <- link_events(
    x = call_network,
    change_event = calls,
    nodes = actors
  )
  friendshipNetwork <- link_events(
    x = friendshipNetwork,
    change_event = friendship,
    nodes = actors
  )
  calls_dependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = call_network
  )
  social_evolution_data <- make_data(
    calls_dependent,
    call_network,
    friendshipNetwork,
    calls,
    actors
  )

  # this block is required for diagnose_changepoints otherwise "calls_dependent" throws an error
  assign("calls_dependent", calls_dependent, envir = .GlobalEnv)
  on.exit(rm(calls_dependent, envir = .GlobalEnv))

  mod00 <- estimate_rem(
    calls_dependent ~ 1 + indeg + outdeg + indeg(friendshipNetwork),
    data = social_evolution_data,
    control_prep = set_preprocessing(start_time = 0L),
    control_algo = set_algorithm_newton(return_interval_loglik = TRUE),
    progress = FALSE,
    verbose = FALSE
  )

  p1 <- diagnose_outliers(mod00, method = "Top", threshold = 2)
  expect_s3_class(p1, "diagnose_outliers")
  p2 <- diagnose_changepoints(mod00, moment = "mean", method = "PELT")
  expect_s3_class(p2, "diagnose_changepoints")
})
