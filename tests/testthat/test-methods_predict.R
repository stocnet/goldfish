# `fitted()` and `predict()`: the model's own quantities at the observed
# decision points. Both read the fit when it stored the primitive they need and
# evaluate only when it did not -- reproducing a stored rank through a pass
# would be a pass nobody needs.

predict_fixture <- function(diagnostics = c("loglik", "ranks"), ...) {
  estimate_wrapper(
    depNetwork ~ inertia + recip,
    model = "DyNAM",
    sub_model = "choice",
    data = dataTest,
    control_algo = set_algorithm_newton(diagnostics = diagnostics),
    ...
  )
}

test_that("fitted outcomes are free", {
  fit <- predict_fixture()

  expect_equal(fitted(fit), exp(fit$interval_log_lik))
  expect_length(fitted(fit), fit$n_events)
  expect_null(fit$preprocessed)
  # Every value is a probability on a multinomial sub-model.
  expect_true(all(fitted(fit) > 0 & fitted(fit) <= 1))
})

test_that("fitted outcomes name the primitive they need", {
  fit <- predict_fixture(diagnostics = character(0))

  expect_null(fit$interval_log_lik)
  expect_snapshot(error = TRUE, fitted(fit))
})

test_that("fitted probabilities are read when stored and evaluated when not", {
  stored <- predict_fixture(diagnostics = c("loglik", "probabilities"))
  plain <- predict_fixture(return_preprocessed = TRUE)

  expect_equal(
    fitted(stored, type = "probabilities"),
    stored$event_probabilities
  )
  expect_null(plain$event_probabilities)
  expect_equal(
    fitted(plain, type = "probabilities"),
    fitted(stored, type = "probabilities")
  )
})

test_that("predicted ranks equal the stored ranks", {
  fit <- predict_fixture()

  expect_equal(predict(fit, type = "ranks"), fit$observed_rank)
  # And a fit that stored none produces them from a pass, agreeing exactly.
  plain <- predict_fixture(
    diagnostics = "loglik",
    return_preprocessed = TRUE
  )
  expect_null(plain$observed_rank)
  expect_equal(predict(plain, type = "ranks"), fit$observed_rank)
})

test_that("predicting at another vector evaluates rather than reads", {
  fit <- predict_fixture(return_preprocessed = TRUE)
  elsewhere <- coef(fit)
  elsewhere[] <- 0

  # At zero every allowed alternative is equally likely, so the tie rule gives
  # every realized alternative rank 1 -- which the stored ranks do not.
  expect_equal(
    unique(predict(fit, type = "ranks", at = elsewhere)),
    1L
  )
  expect_false(identical(
    predict(fit, type = "ranks", at = elsewhere),
    fit$observed_rank
  ))
})

test_that("events selects intervals by position or by mask", {
  fit <- predict_fixture()
  n_intervals <- length(fit$right_censored_events)

  expect_length(predict(fit, type = "ranks", events = 2:4), 3L)
  expect_equal(
    predict(fit, type = "ranks", events = 2:4),
    fit$observed_rank[2:4]
  )
  mask <- rep(FALSE, n_intervals)
  mask[c(1L, n_intervals)] <- TRUE
  expect_equal(
    predict(fit, type = "ranks", events = mask),
    fit$observed_rank[c(1L, n_intervals)]
  )
  expect_snapshot(
    error = TRUE,
    predict(fit, type = "ranks", events = c(1L, n_intervals + 1L))
  )
  expect_snapshot(
    error = TRUE,
    predict(fit, type = "ranks", events = c(TRUE, FALSE))
  )
})

test_that("recall read off stored ranks needs no pass", {
  fit <- predict_fixture()

  ranks <- predict(fit, type = "ranks")
  # The same statistic `evaluate_model(return = "recall")` reports, derived
  # from the ranks themselves so the two cannot disagree about ties.
  expect_equal(
    mean(ranks[!is.na(ranks)] <= 1),
    unname(
      evaluate_model(
        fit,
        return = "recall",
        recall_at = 1L,
        preprocessed = compute_statistics(
          depNetwork ~ inertia + recip,
          model = "DyNAM",
          sub_model = "choice",
          data = dataTest
        )
      )$recall
    )
  )
})
