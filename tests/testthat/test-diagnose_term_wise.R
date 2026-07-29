# Term-wise `diagnose_*`: with `effect =` the two functions stop asking about
# the model and start asking about one coefficient. Changepoints move to the
# term's scaled Schoenfeld series, whose level IS the coefficient, and outliers
# to its dfbeta influence, which is large where an interval moved the estimate
# rather than where the model was surprised.

term_fixture <- function(...) {
  estimate_wrapper(
    depNetwork ~ inertia + recip + trans,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    ...
  )
}

test_that("term-wise outliers rank by that term's influence", {
  fit <- term_fixture()
  terms <- model_terms(fit)

  flagged <- diagnose_outliers(
    fit,
    method = "Top",
    threshold = 2,
    effect = terms$term[[1]]
  )

  # Exactly the two intervals with the largest absolute dfbeta for that term,
  # which is a different question from "least likely" and gives a different
  # answer here.
  influence <- abs(residuals(fit, type = "dfbeta")[, 1])
  expect_equal(
    sort(which(flagged$outlier)),
    sort(order(influence, decreasing = TRUE)[1:2])
  )
  expect_false(identical(
    which(flagged$outlier),
    which(diagnose_outliers(fit, method = "Top", threshold = 2)$outlier)
  ))
  # The selected term rides on the object, so a plot method can label the axis.
  expect_equal(attr(flagged, "params")$effect, terms$term[[1]])
  expect_null(attr(diagnose_outliers(fit), "params")$effect)
})

test_that("term-wise changepoints segment the term's own series", {
  fit <- term_fixture()
  terms <- model_terms(fit)

  segmented <- diagnose_changepoints(fit, effect = terms$term[[2]])

  expect_s3_class(segmented, "diagnose_changepoints")
  expect_equal(attr(segmented, "params")$effect, terms$term[[2]])
  expect_type(segmented$cpt, "logical")
  expect_equal(nrow(segmented), length(fit$interval_log_lik))
  # A different series from the default one, so a different segmentation.
  expect_false(identical(
    which(segmented$cpt),
    which(diagnose_changepoints(fit)$cpt)
  ))
})

test_that("effect accepts every spelling the term answers to", {
  fit <- term_fixture()
  terms <- model_terms(fit)
  selected <- function(name) {
    attr(
      diagnose_outliers(fit, method = "Top", threshold = 1, effect = name),
      "params"
    )$effect
  }

  expect_equal(selected(terms$term[[3]]), terms$term[[3]])
  expect_equal(selected(terms$coefficient[[3]]), terms$term[[3]])
  expect_equal(selected(terms$export[[3]]), terms$term[[3]])
  expect_equal(selected(3L), terms$term[[3]])
})

test_that("effect selects one term at a time", {
  fit <- term_fixture()

  expect_snapshot(
    error = TRUE,
    diagnose_outliers(
      fit,
      effect = c("inertia/networkState", "recip/networkState")
    )
  )
  expect_snapshot(error = TRUE, diagnose_changepoints(fit, effect = "nope"))
})

test_that("an exact-time term-wise series drops its censored rows", {
  fit <- estimate_wrapper(
    depNetwork ~ 1 + indeg + outdeg + indeg(networkExog),
    model = "DyNAM",
    sub_model = "rate",
    data = dataTest,
    control_algo = set_algorithm_newton(
      diagnostics = c("loglik", "scores", "conditional_scores")
    )
  )

  # The scaled Schoenfeld rows are NA on a right-censored interval by
  # construction, so the restriction is automatic there whatever
  # `include_censored` says -- but the table still has one row per interval.
  expect_gt(sum(fit$right_censored_events), 0)
  segmented <- diagnose_changepoints(fit, effect = "indeg/networkExog")
  expect_equal(nrow(segmented), length(fit$interval_log_lik))
  expect_false(any(segmented$cpt[segmented$right_censored_event]))
  expect_equal(
    which(segmented$cpt),
    which(
      diagnose_changepoints(
        fit,
        effect = "indeg/networkExog",
        include_censored = TRUE
      )$cpt
    )
  )
})
