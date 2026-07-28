# The intercept-only rate representation (Session 1): a rate with no covariate
# columns and a fixed intercept, evaluated through the existing timed-rate
# path (`.pse_eval_rate`), reporting zero free parameters and introducing no
# second evaluator.

test_that("intercept-only rate carries no covariate columns and a fixed intercept", {
  rate <- make_intercept_only_rate(log(0.5))

  expect_s3_class(rate, "intercept_only_rate")
  expect_true(is_intercept_only_rate(rate))
  expect_identical(rate$sub_model, "rate")
  expect_true(rate$has_intercept)
  expect_true(rate$fixed_intercept)
  expect_length(rate$effects, 0)
})

test_that("intercept-only rate reports zero free parameters", {
  rate <- make_intercept_only_rate(log(2))
  expect_identical(rate$n_free_parameters, 0L)
})

test_that("intercept-only rate exposes a constant intensity exp(intercept)", {
  rate <- make_intercept_only_rate(log(0.75))
  expect_equal(intercept_only_rate_intensity(rate), 0.75)
})

test_that("evaluation through the timed-rate path is a constant per-actor hazard", {
  intercept <- log(0.4)
  rate <- make_intercept_only_rate(intercept)
  active <- c(1, 1, 0, 1, 0)

  out <- evaluate_intercept_only_rate(rate, active_sender = active)

  # The shared timed-rate evaluator returns the per-actor hazard; every
  # support-legal (active) actor carries the same exp(intercept), excluded
  # actors carry exact zero.
  expect_identical(out$model_type, "DyNAM-M-Rate")
  expect_equal(out$value[active == 1], rep(exp(intercept), sum(active)))
  expect_true(all(out$value[active == 0] == 0))
})

test_that("evaluation reuses `.pse_eval_rate` and introduces no second evaluator", {
  rate <- make_intercept_only_rate(log(0.9))
  active <- c(1, 0, 1, 1)

  # The primitive must route through the existing evaluator, not a bespoke one:
  # its degenerate state materialized directly and passed to the shared
  # `.pse_eval_rate` reproduces the primitive's own evaluation exactly.
  via_primitive <- evaluate_intercept_only_rate(rate, active_sender = active)
  state <- intercept_only_rate_state(rate, active_sender = active)
  via_shared <- .pse_eval_rate(state, parameters = rate$intercept)
  expect_equal(via_primitive$value, via_shared$value)

  # No intercept-only-specific evaluator was added to the internal set.
  expect_false(exists(".pse_eval_intercept_only", mode = "function"))
})

test_that("a zero-count period pins to a zero per-actor hazard", {
  rate <- make_intercept_only_rate(-Inf)
  expect_equal(intercept_only_rate_intensity(rate), 0)

  out <- evaluate_intercept_only_rate(rate, active_sender = c(1, 1, 1))
  expect_true(all(out$value == 0))
})

test_that("make_intercept_only_rate rejects invalid pins", {
  expect_snapshot(error = TRUE, make_intercept_only_rate(c(1, 2)))
  expect_snapshot(error = TRUE, make_intercept_only_rate(NA_real_))
  expect_snapshot(error = TRUE, make_intercept_only_rate(Inf))
})

test_that("the intercept-only rate primitive is not exported", {
  exported <- getNamespaceExports("goldfish")
  internal <- c(
    "make_intercept_only_rate",
    "is_intercept_only_rate",
    "intercept_only_rate_intensity",
    "intercept_only_rate_state",
    "evaluate_intercept_only_rate"
  )
  expect_length(intersect(internal, exported), 0)
})
