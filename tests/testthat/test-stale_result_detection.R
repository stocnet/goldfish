# A fitted object from before the component rename is recognized and reported,
# rather than failing obscurely. What this replaces, on a CRAN 1.6.12 object:
# `summary()` failed with "argument is of length zero", and `logLik()` returned a
# value whose `df` attribute was NULL, so `AIC()` and `BIC()` reported a wrong
# number in silence. The severity split follows from that -- the surfaces that
# would compute something wrong refuse; `print()` still shows what it can.

# The component set of a goldfish 1.6.12 fit, the last CRAN release. Note
# `subModel` and `rightCensored`: the 1.7.0 *function* renames already broke
# these objects, before the component rename existed.
old_cran_fit <- function() {
  structure(
    list(
      parameters = c(5.3751, 1, -0.0816),
      standardErrors = c(0.1554, 0, 0.1975),
      logLikelihood = -699.4532,
      finalScore = c(2.0e-04, 0, 1.5e-05),
      finalInformationMatrix = diag(3),
      convergence = list(isConverged = TRUE, maxAbsScore = 2.0e-04),
      nIterations = 7L,
      nEvents = 439L,
      names = matrix(
        c(rep("call_network", 3), c("FALSE", "TRUE", "FALSE")),
        ncol = 2,
        dimnames = list(c("inertia", "recip", "trans"), c("Object", "fixed"))
      ),
      formula = as.formula(
        "calls_dependent ~ inertia + recip + trans",
        env = new.env(parent = emptyenv())
      ),
      model = "DyNAM",
      subModel = "choice",
      nParams = 3L,
      rightCensored = FALSE,
      call = str2lang("estimate(x = calls_dependent ~ inertia)")
    ),
    class = "result.goldfish"
  )
}

test_that("an old object is recognized, and a current one is not", {
  old <- old_cran_fit()
  # The premise: recognition must rest on something the object actually carries.
  expect_contains(names(old), "logLikelihood")
  expect_false("format_version" %in% names(old))
  expect_identical(result_format_status(old), "outdated")

  data("social_evolution", envir = environment())
  current <- estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )
  expect_identical(result_format_status(current), "current")

  # A fit from a future layout is told apart from an old one, so the advice can
  # be "update goldfish" rather than "re-fit".
  future <- current
  future$format_version <- goldfish_result_format + 1L
  expect_identical(result_format_status(future), "newer")
})

test_that("an unstamped object without retired components is left alone", {
  # The stamp postdates the rename, so absence alone cannot mean "old". Tests
  # and downstream code legitimately build fit-shaped lists by hand; treating
  # them as stale would reject objects that are perfectly current.
  hand_built <- structure(
    list(parameters = c(a = 1), log_likelihood = -10, n_params = 1L),
    class = "result.goldfish"
  )
  expect_identical(result_format_status(hand_built), "current")
  expect_no_condition(inform_if_stale_result(hand_built))
})

test_that("the computing surfaces refuse rather than return a wrong number", {
  old <- old_cran_fit()
  # These are the four that would otherwise compute from components the object
  # does not carry. AIC()/BIC() reach the fit only through logLik(), so guarding
  # logLik() is what stops the silent misreport.
  expect_error(logLik(old), class = "rlang_error")
  expect_error(vcov(old), class = "rlang_error")
  expect_error(summary(old), class = "rlang_error")
  expect_error(augment.result.goldfish(old), class = "rlang_error")
  expect_error(stats::AIC(old), class = "rlang_error")
  expect_error(stats::BIC(old), class = "rlang_error")
})

test_that("printing an old object still shows what it can", {
  old <- old_cran_fit()
  out <- utils::capture.output(
    expect_message(print(old), "not fitted by this version")
  )
  # Informative, not fatal: the coefficients are still shown, because a user
  # holding an unlabelled object needs to be able to see what they have.
  expect_true(any(grepl("Coefficients", out)))
  expect_true(any(grepl("inertia", out)))
})

test_that("a current fit triggers none of it", {
  data("social_evolution", envir = environment())
  fit <- estimate_dynam(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    progress = FALSE,
    verbose = FALSE
  )
  expect_no_condition(inform_if_stale_result(fit))
  expect_no_error(logLik(fit))
  expect_no_error(vcov(fit))
  expect_no_error(summary(fit))
  expect_no_error(stats::AIC(fit))
  # `df` is the attribute whose absence made AIC() wrong on an old object.
  expect_identical(attr(logLik(fit), "df"), fit$n_params)
})

test_that("the messages name the cause and the fix", {
  withr::local_options(cli.num_colors = 1L, cli.width = 72L)
  local_reproducible_output()
  old <- old_cran_fit()
  expect_snapshot(error = TRUE, summary(old))
  expect_snapshot(logLik(old), error = TRUE)
  newer <- old_cran_fit()
  newer$format_version <- goldfish_result_format + 1L
  expect_snapshot(vcov(newer), error = TRUE)
})
