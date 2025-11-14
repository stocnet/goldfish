test_that(
  "examine functions return ggplot objects",
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
    
    p2 <- examine_changepoints(mod00, moment = "mean", method = "PELT")  
    expect_s3_class(p2, "changepoints.goldfish")
    expect_match(capture.output(print(p2)), "No regime changes found.")
  }
)
