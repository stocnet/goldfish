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
  expect_snapshot(error = TRUE, make_intercept_only_rate(NA_real_))
  expect_snapshot(error = TRUE, make_intercept_only_rate(Inf))
  expect_snapshot(error = TRUE, make_intercept_only_rate(numeric(0)))
})

# The per-period pin (Session 2): the pure function
# (count_w, T_w, |R_w|) -> intercept_w = log(count_w / (T_w * |R_w|)), one
# plateau per inter-wave period. It diffs nothing and infers nothing -- counts,
# durations, and |R_w| are all consumer-supplied.

test_that("pin_intercept_only_rate is log(count / (duration * |R_w|)) per period", {
  count <- c(10, 4, 6)
  duration <- c(2, 1, 3)
  risk_set_size <- c(5, 4, 2)

  expect_equal(
    pin_intercept_only_rate(count, duration, risk_set_size),
    log(count / (duration * risk_set_size))
  )
})

test_that("pin_intercept_only_rate handles a single-period (single-window) pin", {
  expect_equal(pin_intercept_only_rate(12, 4, 3), log(12 / (4 * 3)))
})

test_that("a zero-count period pins to -Inf (the flavor cannot fire)", {
  expect_equal(
    pin_intercept_only_rate(c(0, 3), c(1, 1), c(4, 4)),
    c(-Inf, log(3 / 4))
  )
})

test_that("pin_intercept_only_rate rejects malformed inputs", {
  # length mismatch across the per-period vectors
  expect_snapshot(error = TRUE, pin_intercept_only_rate(c(1, 2), 1, c(3, 4)))
  # a negative count is not a count
  expect_snapshot(error = TRUE, pin_intercept_only_rate(-1, 1, 1))
  # a non-positive exposure denominator
  expect_snapshot(error = TRUE, pin_intercept_only_rate(1, 0, 1))
  # an empty risk set is the consumer's guard, not a pin
  expect_snapshot(error = TRUE, pin_intercept_only_rate(1, 1, 0))
  # missing values
  expect_snapshot(error = TRUE, pin_intercept_only_rate(1, 1, NA_real_))
})

# Compute-once-and-freeze (Session 2.2): the constructor stores the per-period
# pin vector plus its partition as a fixed object, with no recompute hook.

test_that("make_intercept_only_rate freezes the per-period pin with its partition", {
  count <- c(8, 6)
  duration <- c(2, 1)
  risk_set_size <- c(2, 4)
  pinned <- pin_intercept_only_rate(count, duration, risk_set_size)

  rate <- make_intercept_only_rate(pinned, wave_times = c(0, 2, 3))

  expect_equal(rate$intercept, pinned)
  expect_identical(rate$n_periods, 2L)
  expect_identical(rate$wave_times, c(0, 2, 3))
  expect_true(rate$frozen)
  expect_identical(rate$n_free_parameters, 0L)
})

test_that("a single-plateau pin needs no wave grid", {
  rate <- make_intercept_only_rate(log(0.5))
  expect_identical(rate$n_periods, 1L)
  expect_null(rate$wave_times)
  expect_true(rate$frozen)
})

test_that("the frozen pin has no recompute hook", {
  # The freeze is structural: the object stores a fixed vector and there is no
  # function to re-derive it from generated / augmented counts (the latent-count
  # recompute is future development).
  expect_false(exists("intercept_only_rate_recompute", mode = "function"))
  expect_false(exists("update_intercept_only_rate", mode = "function"))
})

test_that("a multi-period pin requires a matching, increasing wave grid", {
  # more than one plateau but no partition to place events in
  expect_snapshot(error = TRUE, make_intercept_only_rate(c(-1, -2)))
  # wrong number of boundaries (K + 1 required)
  expect_snapshot(
    error = TRUE,
    make_intercept_only_rate(c(-1, -2), wave_times = c(0, 5))
  )
  # boundaries not strictly increasing
  expect_snapshot(
    error = TRUE,
    make_intercept_only_rate(c(-1, -2), wave_times = c(0, 5, 3))
  )
})

# Half-open period membership (Session 2.3): interior boundaries left-closed /
# right-open, the final period right-closed --
# findInterval(t, wave_times, rightmost.closed = TRUE) -- so an interior-boundary
# event lands in the next period and a terminal-time event is never dropped.

test_that("membership is left-closed / right-open with a right-closed final period", {
  rate <- make_intercept_only_rate(
    c(log(1), log(2), log(3)),
    wave_times = c(0, 5, 10, 15)
  )

  expect_identical(intercept_only_rate_period(rate, 0), 1L) # left edge -> P1
  expect_identical(intercept_only_rate_period(rate, 4.9), 1L)
  expect_identical(intercept_only_rate_period(rate, 5), 2L) # on w_1 -> P2
  expect_identical(intercept_only_rate_period(rate, 10), 3L) # on w_2 -> P3
  # terminal-time event kept: the final period is right-closed
  expect_identical(intercept_only_rate_period(rate, 15), 3L)
  expect_identical(
    intercept_only_rate_period(rate, c(0, 5, 10, 15)),
    c(1L, 2L, 3L, 3L)
  )
})

test_that("a single-plateau rate places every event in period 1", {
  rate <- make_intercept_only_rate(log(0.5))
  expect_identical(
    intercept_only_rate_period(rate, c(-3, 0, 100)),
    c(1L, 1L, 1L)
  )
})

test_that("intensity by time reads the applicable plateau", {
  rate <- make_intercept_only_rate(c(log(2), log(8)), wave_times = c(0, 4, 9))

  expect_equal(intercept_only_rate_intensity(rate, time = 1), 2)
  expect_equal(intercept_only_rate_intensity(rate, time = 4), 8) # boundary -> P2
  expect_equal(intercept_only_rate_intensity(rate, time = 9), 8) # terminal -> P2
  expect_equal(intercept_only_rate_intensity(rate), c(2, 8)) # all plateaus
})

test_that("evaluation selects the applicable plateau by time", {
  rate <- make_intercept_only_rate(c(log(2), log(8)), wave_times = c(0, 4, 9))
  active <- c(1, 1, 0, 1)

  early <- evaluate_intercept_only_rate(rate, active, time = 1)
  late <- evaluate_intercept_only_rate(rate, active, time = 6)

  expect_equal(early$value[active == 1], rep(2, sum(active)))
  expect_equal(late$value[active == 1], rep(8, sum(active)))
})

test_that("evaluating a multi-period rate without a time is an error", {
  rate <- make_intercept_only_rate(c(log(2), log(8)), wave_times = c(0, 4, 9))
  expect_snapshot(
    error = TRUE,
    evaluate_intercept_only_rate(rate, active_sender = c(1, 1))
  )
})

test_that("an event outside the supplied partition is flagged", {
  rate <- make_intercept_only_rate(c(log(1), log(2)), wave_times = c(0, 5, 10))
  expect_snapshot(error = TRUE, intercept_only_rate_period(rate, -1))
  expect_snapshot(error = TRUE, intercept_only_rate_period(rate, 11))
})

test_that("the intercept-only rate primitive is not exported", {
  exported <- getNamespaceExports("goldfish")
  internal <- c(
    "pin_intercept_only_rate",
    "make_intercept_only_rate",
    "is_intercept_only_rate",
    "intercept_only_rate_period",
    "intercept_only_rate_intensity",
    "intercept_only_rate_state",
    "evaluate_intercept_only_rate"
  )
  expect_length(intersect(internal, exported), 0)
})
