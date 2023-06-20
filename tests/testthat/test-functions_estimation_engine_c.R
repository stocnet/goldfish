test_that(
  "test C and R engine DyNAM-rate",
  {
    skip_on_cran()
    model <- "DyNAM"
    subModel <- "rate"
    # endogenous and right-censored events
    formula <- depNetwork ~ 1 + indeg + outdeg(networkExog, weighted = TRUE)
    modR <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, returnIntervalLogL = TRUE)
    )
    modCd <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, engine = "default_c",
                            returnIntervalLogL = TRUE)
    )
    modCgc <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))
    
    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "test C and R engine DyNAM-rate ordered",
  {
    skip_on_cran()
    model <- "DyNAM"
    subModel <- "rate"
    # endogenous and right-censored events
    formula <- depNetwork ~ indeg + outdeg(networkExog, weighted = TRUE)
    modR <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0)
    )
    modCd <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, engine = "default_c")
    )
    modCgc <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))
    
    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "test C and R engine DyNAM-choice",
  {
    skip_on_cran()
    model <- "DyNAM"
    subModel <- "choice"
    # endogenous and right-censored events
    formula <- depNetwork ~ inertia + indeg +
      outdeg(networkExog, weighted = TRUE)
    modR <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0)
    )
    modCd <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, engine = "default_c")
    )
    modCgc <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))
    
    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "test C and R engine REM",
  {
    skip_on_cran()
    model <- "REM"
    # endogenous and right-censored events
    formula <- depNetwork ~ 1 + inertia + indeg +
      outdeg(networkExog, type = "ego", weighted = TRUE)
    modR <- estimate(
      formula,
      model = model,
      estimationInit = list(startTime = 0)
    )
    modCd <- estimate(
      formula,
      model = model,
      estimationInit = list(startTime = 0, engine = "default_c")
    )
    modCgc <- estimate(
      formula,
      model = model,
      estimationInit = list(startTime = 0, engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))
    
    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "test C and R engine REM ordered",
  {
    skip_on_cran()
    model <- "REM"
    # endogenous and right-censored events
    formula <- depNetwork ~ inertia + indeg +
      outdeg(networkExog, type = "ego", weighted = TRUE)
    modR <- estimate(
      formula,
      model = model,
      estimationInit = list(startTime = 0)
    )
    modCd <- estimate(
      formula,
      model = model,
      estimationInit = list(startTime = 0, engine = "default_c")
    )
    modCgc <- estimate(
      formula,
      model = model,
      estimationInit = list(startTime = 0, engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))
    
    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
test_that(
  "test C and R engine DyNAM-choice_coordination",
  {
    skip_on_cran()
    model <- "DyNAM"
    subModel <- "choice_coordination"
    # endogenous and right-censored events
    formula <- depNetwork ~ inertia + indeg +
      indeg(networkExog, type = "ego", weighted = TRUE)
    modR <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0)
    )
    modCd <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, engine = "default_c")
    )
    modCgc <- estimate(
      formula,
      model = model,
      subModel = subModel,
      estimationInit = list(startTime = 0, engine = "gather_compute")
    )
    expect_equal(coef(modR), coef(modCd))
    expect_equal(coef(modR), coef(modCgc))
    
    expect_equal(vcov(modR), vcov(modCd))
    expect_equal(vcov(modR), vcov(modCgc))
  }
)
