# A fitted object from before the component rename is recognized and reported,
# rather than failing obscurely. What this replaces, on a CRAN 1.6.12 object:
# `summary()` failed with "argument is of length zero", and `logLik()` returned
# a value whose `df` attribute was NULL, so `AIC()` and `BIC()` reported a wrong
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
  # The evidence is the missing record, not the camelCase components. Those are
  # asserted only to document that this fixture really is 1.6.12-shaped.
  expect_false("fit_version" %in% names(old))
  expect_contains(names(old), "logLikelihood")
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
  future$fit_version <- FIT_VERSION + 1L
  expect_identical(result_format_status(future), "newer")
})

test_that("an object carrying no record predates it, whatever else it holds", {
  # One rule for both kinds of object: the missing record is the evidence, and
  # it needs no corroboration. Nothing goldfish builds today lacks the record,
  # so an object without one either predates it or was assembled by hand -- and
  # in both cases computing from it is the thing to refuse.
  no_record <- structure(
    list(parameters = c(a = 1), log_likelihood = -10, n_params = 1L),
    class = "result.goldfish"
  )
  # Deliberately carries none of the retired camelCase components: the verdict
  # must not depend on finding one, which is what the old rule did.
  expect_false(any(c("logLikelihood", "subModel") %in% names(no_record)))
  expect_identical(result_format_status(no_record), "outdated")

  # A fixture that wants the methods to run records the layout it emulates.
  declared <- no_record
  declared$fit_version <- FIT_VERSION
  expect_identical(result_format_status(declared), "current")
  expect_no_condition(inform_if_stale_result(declared))
})

test_that("the two objects are recognized by the same rule", {
  # The asymmetry this replaced had the fit needing a retired component to
  # confirm an absent stamp while the preprocessed object did not. Both now read
  # their own slot against their own epoch and nothing else.
  bare_fit <- structure(list(), class = "result.goldfish")
  bare_prep <- structure(list(), class = "preprocessed.goldfish")
  expect_identical(result_format_status(bare_fit), "outdated")
  expect_identical(preprocessed_format_status(bare_prep), "outdated")

  stamped_fit <- structure(
    list(fit_version = FIT_VERSION),
    class = "result.goldfish"
  )
  stamped_prep <- structure(
    list(prep_version = PREP_VERSION),
    class = "preprocessed.goldfish"
  )
  expect_identical(result_format_status(stamped_fit), "current")
  expect_identical(preprocessed_format_status(stamped_prep), "current")
})

test_that("a preprocessed object is refused on kind, not accepted by default", {
  data("social_evolution", envir = environment())
  prep <- compute_statistics(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    output = "preprocessed"
  )
  # Kind is checked for the message, not the verdict: reading the fit's slot on
  # a preprocessed object finds nothing, so it is refused either way -- but as
  # "outdated", which is false about a freshly built object.
  expect_false("fit_version" %in% names(prep))
  expect_identical(result_format_status(prep), "outdated")

  # The two stamps are readable side by side without either shadowing the other,
  # which is what lets a fit carry the object it was estimated from.
  expect_contains(names(prep), "prep_version")
  expect_identical(prep$prep_version, PREP_VERSION)

  # The class gate is what makes the refusal say the true thing: the object is
  # the wrong kind, not the wrong vintage.
  expect_snapshot(abort_if_stale_result(prep, "a summary"), error = TRUE)
})

test_that("a stale preprocessed object is refused at estimation entry", {
  data("social_evolution", envir = environment())
  prep <- compute_statistics(
    calls ~ inertia + recip,
    sub_model = "choice",
    data = social_evolution,
    output = "preprocessed"
  )
  expect_identical(preprocessed_format_status(prep), "current")

  # Every object built before the rename recorded its epoch under `version`, so
  # the absence of `prep_version` identifies it whatever number it carried --
  # which is what makes resetting the counter to its original value safe.
  pre_rename <- prep
  pre_rename$prep_version <- NULL
  pre_rename$version <- 2L
  expect_identical(preprocessed_format_status(pre_rename), "outdated")

  from_future <- prep
  from_future$prep_version <- PREP_VERSION + 1L
  expect_identical(preprocessed_format_status(from_future), "newer")
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
  newer$fit_version <- FIT_VERSION + 1L
  expect_snapshot(vcov(newer), error = TRUE)
})
