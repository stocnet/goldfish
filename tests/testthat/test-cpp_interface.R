test_that(
  "DyNAM-rate",
  {
    skip_on_cran()
    model <- "DyNAM"
    subModel <- "rate"
    # endogenous and right-censored events
    formula <- depNetwork ~ 1 + indeg + outdeg(networkExog, weighted = TRUE)
    modR <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = 
        set_estimation_opt(engine = "default", return_interval_loglik = TRUE),
      progress = FALSE,
      verbose = FALSE
    )
    modCd <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation =
        set_estimation_opt(engine = "default_c", return_interval_loglik = TRUE)
    )
    modCgc <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))

    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "DyNAM-rate ordered",
  {
    skip_on_cran()
    model <- "DyNAM"
    subModel <- "rate"
    # endogenous and right-censored events
    formula <- depNetwork ~ indeg + outdeg(networkExog, weighted = TRUE)
    modR <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default") 
    )
    modCd <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default_c")
    )
    modCgc <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))

    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "DyNAM-choice",
  {
    skip_on_cran()
    model <- "DyNAM"
    subModel <- "choice"
    # endogenous and right-censored events
    formula <- depNetwork ~ inertia + indeg +
      outdeg(networkExog, weighted = TRUE)
    modR <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default")
    )
    modCd <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default_c")
    )
    modCgc <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))

    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "REM",
  {
    skip_on_cran()
    model <- "REM"
    # endogenous and right-censored events
    formula <- depNetwork ~ 1 + inertia + indeg +
      outdeg(networkExog, type = "ego", weighted = TRUE)
    modR <- estimate_wrapper(
      formula,
      model = model,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default")
    )
    modCd <- estimate_wrapper(
      formula,
      model = model,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default_c")
    )
    modCgc <- estimate_wrapper(
      formula,
      model = model,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))

    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "REM ordered",
  {
    skip_on_cran()
    model <- "REM"
    # endogenous and right-censored events
    formula <- depNetwork ~ inertia + indeg +
      outdeg(networkExog, type = "ego", weighted = TRUE)
    modR <- estimate_wrapper(
      formula,
      model = model,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default")
    )
    modCd <- estimate_wrapper(
      formula,
      model = model,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default_c")
    )
    modCgc <- estimate_wrapper(
      formula,
      model = model,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))

    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "DyNAM-choice_coordination",
  {
    skip_on_cran()
    model <- "DyNAM"
    subModel <- "choice_coordination"
    # endogenous and right-censored events
    formula <- depNetwork ~ inertia + indeg +
      indeg(networkExog, type = "ego", weighted = TRUE)
    modR <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default")
    )
    modCd <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "default_c")
    )
    modCgc <- estimate_wrapper(
      formula,
      model = model,
      sub_model = subModel,
      data = dataTest,
      control_preprocessing = set_preprocessing_opt(start_time = 0),
      control_estimation = set_estimation_opt(engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))

    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
