test_that("diagnostic methods throw errors when intervalLogLikelihood isn't present", {
  mod00 <- estimate_dynam(
    depNetwork ~ inertia,
    sub_model = "choice",
    data = dataTest,
    control_preprocessing = set_preprocessing_opt(start_time = 0L),
    control_estimation = set_estimation_opt(return_interval_loglik = FALSE),
    progress = FALSE,
    verbose = FALSE
  )
  expect_error(
    examine_outliers(mod00, method = "Top", parameter = 2),
    "Outlier identification only available when interval log likelihood
      returned in results object."
  )
  expect_error(
    examine_changepoints(mod00, moment = "mean", method = "PELT"),
    "Changepoint identification only available when interval log likelihood
      returned in results object."
  )
})

test_that("diagnostic methods does not accept non-result objects", {
  expect_error(
    examine_outliers(depNetwork, method = "Top", parameter = 2),
    "Not a goldfish results object."
  )
  expect_error(
    examine_changepoints(depNetwork, moment = "mean", method = "PELT"),
    "Not a goldfish results object."
  )
})


test_that("diagnostic methods work on \"choice\" models.", {
  mod00 <- estimate_dynam(
    depNetwork ~ inertia + trans + indeg,
    sub_model = "choice",
    data = dataTest,
    control_preprocessing = set_preprocessing_opt(start_time = 0L),
    control_estimation = set_estimation_opt(return_interval_loglik = TRUE),
    progress = FALSE,
    verbose = FALSE
  )

  p1 <- examine_outliers(mod00, method = "Top", parameter = 2)
  expect_s3_class(p1, "diagnostic.goldfish")
  expect_equal(sum(p1$outlier), 2)
  p11 <- examine_outliers(mod00, method = "IQR")
  expect_s3_class(p11, "diagnostic.goldfish")
  p2 <- examine_changepoints(mod00, moment = "mean", method = "PELT")
  expect_s3_class(p2, "diagnostic.goldfish")
  p21 <- examine_changepoints(
    mod00,
    moment = "variance",
    method = "PELT",
    window = 2
  )
  expect_s3_class(p2, "diagnostic.goldfish")
})

test_that("diagnostic methods work on \"rate\" models.", {
  data("Social_Evolution")
  callNetwork <- make_network(nodes = actors, directed = TRUE)
  friendshipNetwork <- make_network(nodes = actors, directed = TRUE)
  callNetwork <- link_events(
    x = callNetwork,
    change_event = calls,
    nodes = actors
  )
  friendshipNetwork <- link_events(
    x = friendshipNetwork,
    change_event = friendship,
    nodes = actors
  )
  callsDependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = callNetwork
  )
  socialEvolutionData <- make_data(
    callsDependent,
    callNetwork,
    friendshipNetwork,
    calls,
    actors
  )

  # this block is required for examine_changepoints otherwise "callsDependent" throws an error
  assign("callsDependent", callsDependent, envir = .GlobalEnv)
  on.exit(rm(callsDependent, envir = .GlobalEnv))

  mod00 <- estimate_dynam(
    callsDependent ~ 1 + indeg + outdeg + indeg(friendshipNetwork),
    sub_model = "rate",
    data = socialEvolutionData,
    control_preprocessing = set_preprocessing_opt(start_time = 0L),
    control_estimation = set_estimation_opt(return_interval_loglik = TRUE),
    progress = FALSE,
    verbose = FALSE
  )

  p1 <- examine_outliers(mod00, method = "Top", parameter = 2)
  expect_s3_class(p1, "diagnostic.goldfish")
  p2 <- examine_changepoints(mod00, moment = "mean", method = "PELT")
  expect_s3_class(p2, "diagnostic.goldfish")
})

test_that("diagnostic methods work on \"rem\" models.", {
  data("Social_Evolution")
  callNetwork <- make_network(nodes = actors, directed = TRUE)
  friendshipNetwork <- make_network(nodes = actors, directed = TRUE)
  callNetwork <- link_events(
    x = callNetwork,
    change_event = calls,
    nodes = actors
  )
  friendshipNetwork <- link_events(
    x = friendshipNetwork,
    change_event = friendship,
    nodes = actors
  )
  callsDependent <- make_dependent_events(
    events = calls,
    nodes = actors,
    default_network = callNetwork
  )
  socialEvolutionData <- make_data(
    callsDependent,
    callNetwork,
    friendshipNetwork,
    calls,
    actors
  )

  # this block is required for examine_changepoints otherwise "callsDependent" throws an error
  assign("callsDependent", callsDependent, envir = .GlobalEnv)
  on.exit(rm(callsDependent, envir = .GlobalEnv))

  mod00 <- estimate_rem(
    callsDependent ~ 1 + indeg + outdeg + indeg(friendshipNetwork),
    data = socialEvolutionData,
    control_preprocessing = set_preprocessing_opt(start_time = 0L),
    control_estimation = set_estimation_opt(return_interval_loglik = TRUE),
    progress = FALSE,
    verbose = FALSE
  )

  p1 <- examine_outliers(mod00, method = "Top", parameter = 2)
  expect_s3_class(p1, "diagnostic.goldfish")
  p2 <- examine_changepoints(mod00, moment = "mean", method = "PELT")
  expect_s3_class(p2, "diagnostic.goldfish")
})
