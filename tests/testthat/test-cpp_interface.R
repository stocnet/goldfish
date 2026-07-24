test_that("DyNAM-rate", {
  skip_on_cran()
  model <- "DyNAM"
  sub_model <- "rate"
  # endogenous and right-censored events
  formula <- depNetwork ~ 1 + indeg + outdeg(networkExog, weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(
      engine = "default",
      return_interval_loglik = TRUE
    ),
    progress = FALSE,
    verbose = FALSE
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(
      engine = "default_c",
      return_interval_loglik = TRUE
    )
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "gather_compute")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("DyNAM-rate ordered", {
  skip_on_cran()
  model <- "DyNAM"
  sub_model <- "rate_ordered"
  # endogenous and right-censored events
  formula <- depNetwork ~ indeg + outdeg(networkExog, weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default_c")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "gather_compute")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("DyNAM-choice", {
  skip_on_cran()
  model <- "DyNAM"
  sub_model <- "choice"
  # endogenous and right-censored events
  formula <- depNetwork ~ inertia + indeg + outdeg(networkExog, weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default_c")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "gather_compute")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("REM", {
  skip_on_cran()
  model <- "REM"
  sub_model <- "rate"
  # endogenous and right-censored events
  formula <- depNetwork ~ 1 +
    inertia +
    indeg +
    outdeg(networkExog, type = "ego", weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default_c")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "gather_compute")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("REM ordered", {
  skip_on_cran()
  model <- "REM"
  sub_model <- "rate_ordered"
  # endogenous and right-censored events
  formula <- depNetwork ~ inertia +
    indeg +
    outdeg(networkExog, type = "ego", weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default_c")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "gather_compute")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
test_that("DyNAM-choice_coordination", {
  skip_on_cran()
  model <- "DyNAM"
  sub_model <- "choice_coordination"
  # endogenous and right-censored events
  # NB: choice_coordination rejects an ego-perspective main effect, so this
  # engine-consistency vector uses the default alter
  # perspective — the effect only needs to be valid and non-trivial here.
  formula <- depNetwork ~ inertia +
    indeg +
    indeg(networkExog, weighted = TRUE)
  modR <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default")
  )
  modCd <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "default_c")
  )
  modCgc <- estimate_wrapper(
    formula,
    model = model,
    sub_model = sub_model,
    data = dataTest,
    control_prep = set_preprocessing(start_time = 0),
    control_algo = set_algorithm_newton(engine = "gather_compute")
  )
  expect_equal(coef(modR), coef(modCd))
  expect_equal(coef(modR), coef(modCgc))

  expect_equal(vcov(modR), vcov(modCd))
  expect_equal(vcov(modR), vcov(modCgc))
})
