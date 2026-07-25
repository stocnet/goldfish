# optimizer = c("newton_raphson", "bfgs", "bhhh", "nelder_mead") and the
# maxLik-backed adapter. newton_raphson is the built-in loop; the
# other three run maxLik::maxLik() over the default_c evaluator.

test_that("set_algorithm_newton validates the optimizer against the flat list", {
  expect_identical(set_algorithm_newton()$optimizer, "newton_raphson")
  expect_identical(
    set_algorithm_newton(optimizer = "bfgs")$optimizer,
    "bfgs"
  )
  expect_error(
    set_algorithm_newton(optimizer = "gradient_descent"),
    "should be one of"
  )
})

test_that("the maxLik evaluator runs the C++ pass once per parameter vector", {
  calls <- 0L
  fake_eval <- function(pars, need_scores) {
    calls <<- calls + 1L
    list(logLikelihood = sum(pars), derivative = matrix(pars, nrow = 1))
  }
  ev <- make_memoized_evaluator(fake_eval, FALSE)
  beta <- c(0.1, 0.2)
  # logLik-then-gradient at the same vector share one evaluation.
  invisible(ev(beta))
  invisible(ev(beta))
  invisible(ev(beta))
  expect_equal(calls, 1L)
  # a new parameter vector triggers a fresh evaluation.
  invisible(ev(c(0.3, 0.4)))
  expect_equal(calls, 2L)
})

test_that("maxLik optimizers reject backends other than cpp", {
  skip_on_cran()
  withr::local_options(cli.num_colors = 1L)
  local_reproducible_output()
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_choice
  fit_call <- function(backend) {
    estimate_dynam(
      spec$formula,
      data = data_list$social_evolution,
      sub_model = spec$sub_model,
      control_algo = set_algorithm_newton(
        optimizer = "bfgs",
        backend = backend
      ),
      progress = FALSE
    )
  }
  expect_snapshot(fit_call("gather"), error = TRUE)
  expect_snapshot(fit_call("r"), error = TRUE)
})

test_that("a maxLik optimizer aborts when maxLik is not installed", {
  skip_on_cran()
  withr::local_options(cli.num_colors = 1L)
  local_reproducible_output()
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "maxLik")) FALSE else TRUE
    },
    .package = "base"
  )
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_choice
  expect_snapshot(
    estimate_dynam(
      spec$formula,
      data = data_list$social_evolution,
      sub_model = spec$sub_model,
      control_algo = set_algorithm_newton(optimizer = "bfgs"),
      progress = FALSE
    ),
    error = TRUE
  )
})

test_that("BFGS and BHHH agree with Newton-Raphson on baseline fixtures", {
  skip_on_cran()
  skip_if_not_installed("maxLik")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  grid <- baselines_model_grid()
  fit_opt <- function(spec, optimizer) {
    opt <- set_algorithm_newton(backend = "cpp", optimizer = optimizer)
    args <- list(
      x = spec$formula,
      data = data_list[[spec$dataset]],
      control_algo = opt,
      progress = FALSE
    )
    if (spec$model == "DyNAM") {
      args$sub_model <- spec$sub_model
      suppressWarnings(do.call(estimate_dynam, args))
    } else {
      suppressWarnings(do.call(estimate_rem, args))
    }
  }
  # choice (no intercept) and rate (timed, with intercept) cover both cores.
  for (nm in c("se_dynam_choice", "se_dynam_rate")) {
    spec <- grid[[nm]]
    nr <- fit_opt(spec, "newton_raphson")
    for (optimizer in c("bfgs", "bhhh")) {
      ml <- fit_opt(spec, optimizer)
      label <- paste(nm, optimizer)
      expect_equal(ml$parameters, nr$parameters, tolerance = 1e-2, info = label)
      expect_equal(
        ml$logLikelihood,
        nr$logLikelihood,
        tolerance = 1e-4,
        info = label
      )
    }
  }
})

test_that("maxLik result supports the standard post-estimation methods", {
  skip_on_cran()
  skip_if_not_installed("maxLik")
  data_list <- list(social_evolution = baselines_social_evolution_data())
  spec <- baselines_model_grid()$se_dynam_choice
  nr <- estimate_dynam(
    spec$formula,
    data = data_list$social_evolution,
    sub_model = spec$sub_model,
    control_algo = set_algorithm_newton(backend = "cpp"),
    progress = FALSE
  )
  fit <- estimate_dynam(
    spec$formula,
    data = data_list$social_evolution,
    sub_model = spec$sub_model,
    control_algo = set_algorithm_newton(
      backend = "cpp",
      optimizer = "bfgs"
    ),
    progress = FALSE
  )
  expect_s3_class(fit, "result.goldfish")
  expect_length(fit$parameters, length(nr$parameters))
  expect_true(all(is.finite(fit$standardErrors)))
  expect_true(fit$convergence$isConverged)
  # Fisher-based vcov at the optimum agrees with the NR standard errors.
  expect_equal(fit$standardErrors, nr$standardErrors, tolerance = 1e-2)
  smry <- summary(fit)
  expect_s3_class(smry, "summary.result.goldfish")
  expect_equal(dim(vcov(fit)), c(3L, 3L))
  expect_equal(as.numeric(logLik(fit)), fit$logLikelihood)
})
