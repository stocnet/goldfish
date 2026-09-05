# The convention every returned object follows, asserted as a contract rather
# than as a list of names. A reader of one component has to be able to predict
# the spelling of its neighbours; the failure this guards is silent, because a
# component read under the wrong spelling is NULL and `NULL[[i]]` is NULL again,
# so code written against a wrong name runs and reports nothing.
#
# Checking "no name carries an uppercase letter" rather than enumerating the
# expected names is deliberate: an enumeration passes while a newly added
# camelCase component sits beside the ones it lists.

# Acronyms, not camelCase: `summary()` reports the information criteria under
# their standard names, as every other R model summary does.
acronym_components <- c("AIC", "BIC")

expect_snake_case_names <- function(x, label) {
  nms <- setdiff(names(x), acronym_components)
  # The premise: a component set that came back empty would satisfy the
  # assertion below without testing anything.
  expect_gt(length(nms), 0L)
  expect_equal(grep("[A-Z]", nms, value = TRUE), character(0), info = label)
}

test_that("a fitted object and its derived objects are snake_case throughout", {
  data("social_evolution", envir = environment())
  # Every per-event diagnostic requested, so the components that exist only on
  # request are present to be checked -- `event_probabilities` above all, the
  # one this rename exists for. suppressWarnings() covers exactly one expected
  # condition: the storage-footprint guardrail warns by design whenever
  # probabilities are requested.
  fit <- suppressWarnings(estimate_dynam(
    calls ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = social_evolution,
    control_algo = set_algorithm_newton(diagnostics = "all"),
    progress = FALSE,
    verbose = FALSE
  ))

  expect_contains(names(fit), "event_probabilities")
  expect_gt(length(fit$event_probabilities), 0L)
  expect_contains(names(fit), "interval_log_lik")
  expect_gt(length(fit$interval_log_lik), 0L)

  expect_snake_case_names(fit, "goldfishFit")
  expect_snake_case_names(summary(fit), "goldfishSummFit")
  expect_snake_case_names(augment(fit), "augment() tibble")

  # The nested convergence report is where the mixed convention was clearest:
  # four camelCase components beside one snake_case one.
  expect_gt(length(fit$convergence), 0L)
  expect_snake_case_names(fit$convergence, "convergence")
  expect_contains(names(fit$convergence), "score_rel_norm")
})

test_that("the export and preprocessed objects follow the same convention", {
  data("social_evolution", envir = environment())
  gathered <- gather_model_data(
    calls ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = social_evolution
  )
  # `names_effects` and `is_dependent` are the two the gather export renamed;
  # `is_dependent` rides on the intercept, hence the rate model above.
  expect_contains(names(gathered), c("names_effects", "is_dependent"))
  expect_snake_case_names(gathered, "gather export")

  prep <- compute_statistics(
    calls ~ 1 + indeg + outdeg,
    sub_model = "rate",
    data = social_evolution,
    output = "preprocessed"
  )
  expect_contains(names(prep), c("initial_stats", "start_time", "end_time"))
  expect_gt(length(prep$initial_stats), 0L)
  expect_snake_case_names(prep, "goldfishStat")
})
