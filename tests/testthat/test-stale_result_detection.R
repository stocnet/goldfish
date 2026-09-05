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

# The same components under the CURRENT class name. No such object exists in
# the wild -- every stored fit predating 2.0.0 carries the retired class -- but
# the stale-epoch path is still reachable on `goldfishFit` the next time
# `FIT_VERSION` moves, so it keeps its own fixture rather than borrowing the
# retired-class one and testing two things at once.
stale_current_class_fit <- function() {
  fit <- old_cran_fit()
  class(fit) <- "goldfishFit"
  fit
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
  bare_prep <- structure(list(), class = "goldfishStat")
  expect_identical(result_format_status(bare_fit), "outdated")
  expect_identical(preprocessed_format_status(bare_prep), "outdated")

  stamped_fit <- structure(
    list(fit_version = FIT_VERSION),
    class = "result.goldfish"
  )
  stamped_prep <- structure(
    list(prep_version = PREP_VERSION),
    class = "goldfishStat"
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
  old <- stale_current_class_fit()
  # These are the four that would otherwise compute from components the object
  # does not carry. AIC()/BIC() reach the fit only through logLik(), so guarding
  # logLik() is what stops the silent misreport.
  expect_error(logLik(old), class = "rlang_error")
  expect_error(vcov(old), class = "rlang_error")
  expect_error(summary(old), class = "rlang_error")
  expect_error(augment(old), class = "rlang_error")
  expect_error(stats::AIC(old), class = "rlang_error")
  expect_error(stats::BIC(old), class = "rlang_error")
})

test_that("printing an old object still shows what it can", {
  old <- stale_current_class_fit()
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
  old <- stale_current_class_fit()
  expect_snapshot(error = TRUE, summary(old))
  expect_snapshot(logLik(old), error = TRUE)
  newer <- stale_current_class_fit()
  newer$fit_version <- FIT_VERSION + 1L
  expect_snapshot(vcov(newer), error = TRUE)
})

# The retired class name ------------------------------------------------------
#
# `result.goldfish` names no class goldfish attaches any more. It survives as a
# discriminator: two stubs explain and stop, and every other generic gives R's
# own dispatch error, which already says the true thing.

test_that("only print() and summary() answer on the retired class", {
  old <- old_cran_fit()
  expect_s3_class(old, "result.goldfish")

  # The two stubs: goldfish's own message, so they are rlang conditions.
  expect_error(print(old), class = "rlang_error")
  expect_error(summary(old), class = "rlang_error")

  # Generics with no applicable method: R's own dispatch error, a base
  # condition rather than an rlang one. Registering stubs for these would
  # replace a correct message with one that has to be maintained.
  for (call in list(
    function() stats::logLik(old),
    function() stats::vcov(old),
    function() stats::predict(old),
    function() generics::augment(old),
    function() generics::tidy(old),
    function() generics::glance(old)
  )) {
    err <- rlang::catch_cnd(call())
    expect_s3_class(err, "error")
    expect_false(inherits(err, "rlang_error"))
  }

  # The diagnostic generics have a goldfish `default` method that names what a
  # diagnosable object is, so they answer with goldfish's message rather than
  # R's. That is better than either stub, and needs none.
  expect_error(test_gof(old), class = "rlang_error")
  expect_error(diagnose_outliers(old), class = "rlang_error")
})

test_that("coef/residuals/fitted return NULL on the retired class", {
  # KNOWN GAP, pinned so it is visible rather than discovered. These three
  # generics have a base `default` method that reads a component off the list
  # and returns NULL when it is absent, so no dispatch error is raised and the
  # retired class produces a silent NULL -- the failure mode the staleness
  # machinery exists to prevent, in the one place the design assumed R would
  # raise for us.
  #
  # Not fixed here: the design fixes the retired name at exactly two stubs, and
  # widening that set is its call, not this test's. If it is widened, this test
  # fails and should be deleted.
  old <- old_cran_fit()
  expect_null(stats::coef(old))
  expect_null(stats::residuals(old))
  expect_null(stats::fitted(old))
})

test_that("the stub tells the two populations apart", {
  # A released goldfish (<= 1.7.0) recorded no epoch AND used the old component
  # names, so it is told both things. A fit saved from the development line
  # records the current epoch and lost only its class name -- telling that user
  # their components were renamed would send them hunting for a change that
  # never happened.
  released <- old_cran_fit()
  dev_line <- old_cran_fit()
  dev_line$fit_version <- FIT_VERSION

  expect_identical(retired_result_class_status(released), "outdated")
  expect_identical(retired_result_class_status(dev_line), "retired_class")

  released_message <- conditionMessage(rlang::catch_cnd(print(released)))
  dev_message <- conditionMessage(rlang::catch_cnd(print(dev_line)))
  expect_false(identical(released_message, dev_message))

  # The load-bearing assertion: the current-epoch object is never told its
  # components were renamed.
  expect_match(released_message, "components of a fitted model were renamed")
  expect_no_match(dev_message, "renamed")
  expect_match(dev_message, "class .* was retired")
})

test_that("the retired-class messages name the cause and the fix", {
  withr::local_options(cli.num_colors = 1L, cli.width = 72L)
  local_reproducible_output()
  released <- old_cran_fit()
  dev_line <- old_cran_fit()
  dev_line$fit_version <- FIT_VERSION
  expect_snapshot(print(released), error = TRUE)
  expect_snapshot(summary(dev_line), error = TRUE)
})
