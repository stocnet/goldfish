test_that(
  "examine functions return ggplot objects",
  {
    skip_on_cran()

    mod00 <- estimate(
      depNetwork ~ inertia + recip + trans,
      model = "DyNAM",
      subModel = "choice",
      control_preprocessing = set_preprocessing_opt(start_time = 0L),
      control_estimation = set_estimation_opt(return_interval_loglik = TRUE),
      progress = FALSE,
      verbose = FALSE
    )

    p1 <- examine_outliers(mod00, method = "Top", parameter = 2)
    expect_s3_class(p1, "ggplot")
    
    out_text <- capture.output(
      p2 <- examine_changepoints(mod00, moment = "mean", method = "PELT")  
    )
    expect_null(p2)
    expect_match(out_text, "No regime changes found.")
  }
)
