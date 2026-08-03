# Frozen two-mode coefficient baselines.
#
# A two-mode model on the frozen `irps_nuclear` subset must reproduce its
# recorded coefficients to 1e-6, so an upstream change to the effect walk is
# caught. These run under `skip_on_cran()`, like the one-mode coefficient
# baselines; a `NOT_CRAN=true` run must report them PASS, not SKIP.

irps_subset_data <- function() {
  as_goldfish(readRDS(test_path("fixtures", "irps_nuclear_subset.rds")))
}

test_that("frozen two-mode DyNAM choice baseline", {
  skip_on_cran()
  fit <- estimate_dynam(
    make_specification(
      choice = list(
        modeled ~ indeg(support) + indeg(contestation) + four(support)
      ),
      model = "DyNAM",
      choice_sub_model = "choice",
      data = irps_subset_data()
    )
  )
  expect_true(fit$convergence$is_converged)
  expect_equal(
    coef(fit),
    c(0.12506808475813694, 0.00805525215385681, 0.11402852287910364),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_equal(as.numeric(logLik(fit)), -409.529564160234, tolerance = 1e-6)
})

test_that("frozen two-mode DyNAM rate baseline", {
  skip_on_cran()
  fit <- estimate_dynam(
    make_specification(
      rate = list(modeled ~ 1 + outdeg(support) + ego(power)),
      model = "DyNAM",
      rate_sub_model = "rate",
      data = irps_subset_data()
    ),
    sub_model = "rate"
  )
  expect_true(fit$convergence$is_converged)
  expect_equal(
    coef(fit),
    c(-15.394980089388412, 0.212964855063395, 1.749560971289161),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_equal(as.numeric(logLik(fit)), -1889.26537129211, tolerance = 1e-6)
})

test_that("frozen two-mode REM baseline", {
  skip_on_cran()
  fit <- estimate_rem(
    make_specification(
      rate = list(modeled ~ 1 + inertia(support) + indeg(support)),
      model = "REM",
      data = irps_subset_data()
    )
  )
  expect_true(fit$convergence$is_converged)
  expect_equal(
    coef(fit),
    c(-18.01054716585719362, 3.39773349777084288, -0.00541016426182189),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  expect_equal(as.numeric(logLik(fit)), -2410.5061450853, tolerance = 1e-6)
})
