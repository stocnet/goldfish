# The missing-data documentation recommends multiple imputation combined under
# Rubin's rules via mitools::MIcombine(), which consumes the coef()/vcov()
# methods a goldfish result provides. This verifies the claim on two real fits
# rather than asserting it from the interface.

test_that("MIcombine combines goldfish fits into estimates and errors", {
  skip_if_not_installed("mitools")

  data("social_evolution", package = "goldfish", envir = environment())
  observed <- stats::na.omit(social_evolution$nodes$gradeType)

  complete_once <- function(seed) {
    withr::local_seed(seed)
    se <- social_evolution
    missing <- is.na(se$nodes$gradeType)
    se$nodes$gradeType[missing] <- sample(
      observed,
      sum(missing),
      replace = TRUE
    )
    estimate_dynam(
      calls ~ inertia + alter(gradeType),
      sub_model = "choice",
      data = se
    )
  }

  fits <- list(complete_once(1), complete_once(2))
  combined <- mitools::MIcombine(fits)

  estimates <- coef(combined)
  std_errors <- sqrt(diag(vcov(combined)))

  # Two coefficients (inertia, alter), combined and finite for both estimate
  # and standard error -- Rubin's rules produced usable output.
  expect_named(estimates, names(coef(fits[[1]])), ignore.order = TRUE)
  expect_length(estimates, 2)
  expect_length(std_errors, 2)
  expect_true(all(is.finite(estimates)))
  expect_true(all(is.finite(std_errors) & std_errors > 0))
})
