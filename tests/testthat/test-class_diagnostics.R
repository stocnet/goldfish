test_that(
  "examine outliers returns correct object",
  {
    skip_on_cran()

    mod00 <- estimate_dynam(
      depNetwork ~ inertia + recip + trans,
      sub_model = "choice",
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0L),
      control_estimation = set_estimation_opt(return_interval_loglik = TRUE),
      progress = FALSE,
      verbose = FALSE
    )

    p1 <- examine_outliers(mod00, method = "Top", parameter = 2)
    expect_s3_class(p1, "outliers.goldfish")
  }
)

test_that(
  "examine changepoints returns correct object",
  {
    skip_on_cran()
    data("Social_Evolution")
    callNetwork <- make_network(nodes = actors, directed = TRUE)
    callNetwork <- link_events(
      x = callNetwork, change_event = calls,
      nodes = actors
    )
    callsDependent <- make_dependent_events(
      events = calls, nodes = actors,
      default_network = callNetwork
    )
    socialEvolutionData <- make_data(callsDependent, callNetwork, calls, actors)

    # this block is required for examine_changepoints otherwise "callsDependent" throws an error
    assign("callsDependent", callsDependent, envir = .GlobalEnv)
    on.exit(rm(callsDependent, envir = .GlobalEnv))

    mod00 <- estimate_dynam(
      callsDependent ~ inertia + recip + trans,
      sub_model = "choice",
      data = socialEvolutionData,
      control_preprocessing = set_preprocessing_opt(start_time = 0L),
      control_estimation = set_estimation_opt(return_interval_loglik = TRUE),
      progress = FALSE,
      verbose = FALSE
    )

    p2 <- examine_changepoints(mod00, moment = "mean", method = "PELT")
    expect_s3_class(p2, "changepoints.goldfish")
  }
)
