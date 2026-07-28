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

# A multi-wave fixture (Session 2.4): waves at t = 0, 4, 10, 12 -> three
# inter-wave periods of durations 4, 6, 2. The pin is regime-agnostic -- only the
# *source* of |R_w| differs (relational time-weighted avg_active_entity vs panel
# wave-endpoint average), both consumer-supplied.

test_that("per-period pin matches hand computation for a relational |R_w|", {
  wave_times <- c(0, 4, 10, 12)
  duration <- diff(wave_times) # c(4, 6, 2)
  count <- c(8, 3, 5)
  # relational |R_w|: the time-weighted average active entity per period
  # (avg_active_entity restricted to w), supplied by the consumer.
  risk_set_size <- c(5, 4.5, 6)

  intercept <- pin_intercept_only_rate(count, duration, risk_set_size)

  expect_equal(intercept, log(count / (duration * risk_set_size)))
  expect_equal(intercept[1], log(8 / (4 * 5)))
  expect_equal(intercept[2], log(3 / (6 * 4.5)))
  expect_equal(intercept[3], log(5 / (2 * 6)))
})

test_that("per-period pin matches hand computation for a panel wave-endpoint |R_w|", {
  wave_times <- c(0, 4, 10, 12)
  duration <- diff(wave_times)
  # panel count: the net Hamming diff between consecutive observed wave states.
  count <- c(8, 3, 5)
  # panel |R_w|: the wave-endpoint average (|R(w_{k-1})| + |R(w_k)|) / 2 of the
  # flavor's post-constraint entity count at the two observed wave states.
  entity_at_wave <- c(6, 4, 5, 7)
  risk_set_size <-
    (utils::head(entity_at_wave, -1) + utils::tail(entity_at_wave, -1)) / 2

  intercept <- pin_intercept_only_rate(count, duration, risk_set_size)

  expect_equal(risk_set_size, c(5, 4.5, 6))
  expect_equal(intercept, log(count / (duration * risk_set_size)))
  # regime-agnostic: the same wave-endpoint |R_w| feeds the same pin as the
  # relational form above.
  expect_equal(intercept[2], log(3 / (6 * 4.5)))
})

test_that("distinct plateaus per period differ from a single global pin", {
  wave_times <- c(0, 4, 10, 12)
  duration <- diff(wave_times)
  count <- c(8, 3, 5)
  risk_set_size <- c(5, 4, 10) # per-period rates genuinely differ

  per_period <- pin_intercept_only_rate(count, duration, risk_set_size)
  # the rejected aggregate: one global count / T_total / mean|R| plateau, which
  # misplaces events when the per-period rates differ.
  global <- pin_intercept_only_rate(
    sum(count),
    sum(duration),
    mean(risk_set_size)
  )

  expect_length(unique(per_period), 3)
  expect_false(isTRUE(all.equal(per_period, rep(global, 3))))
})

test_that("the frozen pin is unchanged after a round of generated events", {
  wave_times <- c(0, 4, 10, 12)
  rate <- make_intercept_only_rate(
    pin_intercept_only_rate(c(8, 3, 5), diff(wave_times), c(5, 4.5, 6)),
    wave_times = wave_times
  )
  before <- rate$intercept

  # a consumer placing generated events across the three periods must never
  # re-derive the pin -- it is a function of the *supplied* counts, not the
  # generated events, so it stays byte-identical.
  for (t in c(1, 5, 7, 11)) {
    invisible(
      evaluate_intercept_only_rate(rate, active_sender = c(1, 1, 1), time = t)
    )
  }

  expect_identical(rate$intercept, before)
})

# Uniform support-legal sender semantics (Session 3): `exp(intercept_w)` is a
# per-actor constant hazard, so on the shared clock it enters Sum_i exp(.)
# commensurably with a competing per-actor rate, and the sender selection is
# uniform over the support-legal set *as a consequence* of the equal hazards.
# The primitive exposes the semantics -- it performs no draw and no
# empty-support guard (the consuming routine owns both).

test_that("semantics give a uniform sender over the inherited support-legal set", {
  rate <- make_intercept_only_rate(log(0.5))
  # inherited support: the flavor's post-constraint mask (actor 3 illegal).
  sem <- intercept_only_rate_sender_semantics(
    rate,
    support_legal = c(1, 1, 0, 1)
  )

  # uniform over the legal set {1, 2, 4}, exact-zero for the illegal actor --
  # the uniformity following from the equal per-actor hazards.
  expect_equal(sem$probability, c(1 / 3, 1 / 3, 0, 1 / 3))
  expect_equal(sem$hazard, c(0.5, 0.5, 0, 0.5))
  expect_true(sem$uniform)
  expect_identical(sem$support, "inherited")
  expect_identical(sem$self_loops, "disallowed")
})

test_that("self-loops are the only automatic restriction, support is inherited", {
  rate <- make_intercept_only_rate(log(0.5))
  support <- c(1, 1, 1, 1)

  # self-loop mask removes actor 1 even though support marked it legal -- the
  # sole restriction the primitive applies on its own.
  sem <- intercept_only_rate_sender_semantics(
    rate,
    support_legal = support,
    self_loop = c(1, 0, 0, 0)
  )
  expect_equal(sem$probability, c(0, 1 / 3, 1 / 3, 1 / 3))

  # a sibling flavor's different support mask is NEVER borrowed: the distribution
  # depends only on the mask passed for THIS flavor.
  sem_sibling <- intercept_only_rate_sender_semantics(
    rate,
    support_legal = c(0, 1, 1, 1)
  )
  expect_equal(sem_sibling$probability, c(0, 1 / 3, 1 / 3, 1 / 3))
  expect_false(identical(
    intercept_only_rate_sender_semantics(rate, c(1, 1, 0, 1))$probability,
    sem_sibling$probability
  ))
})

test_that("the pinned hazard is commensurable with a competing per-actor rate", {
  n <- 4L
  active <- c(1, 1, 1, 1)

  # the pinned flavor's per-actor hazard: a constant exp(intercept_w) over actors.
  rate <- make_intercept_only_rate(log(0.4))
  pinned <- intercept_only_rate_sender_semantics(
    rate,
    support_legal = active
  )$hazard

  # a competing effect-driven rate scored through the SAME shared evaluator:
  # a per-actor hazard exp(intercept + beta * x_i) -- a genuine spread.
  comp_state <- list(
    model_type = "DyNAM-M-Rate",
    stat_mat = cbind(1, c(0.5, -0.2, 1.0, 0.3)),
    active_sender = active,
    n_actors1 = n,
    n_actors2 = 1L,
    is_rate = TRUE,
    event_sender = NA_integer_,
    is_dependent = FALSE,
    timespan = NA_real_
  )
  comp <- .pse_eval_rate(comp_state, parameters = c(log(0.4), 0.8))

  # both are per-actor vectors in the same hazard space (like compares with
  # like -- no aggregate scalar summed against a per-actor vector).
  expect_length(pinned, n)
  expect_length(comp$value, n)
  expect_equal(pinned, rep(0.4, n)) # pinned: constant across actors
  expect_gt(stats::var(comp$value), 0) # competing: a spread

  # the shared-clock superposition Sum_i exp(.) combines them elementwise per
  # actor, then sums -- the commensurability the per-actor framing buys.
  joint <- pinned + comp$value
  expect_equal(sum(joint), sum(pinned) + sum(comp$value))
})

test_that("a routine driving the semantics reproduces count_w in expectation", {
  # a period of duration T_w = 10 with a risk set that CHANGES but stays
  # non-empty: 3 support-legal actors for the first 4 time units, then 8 for the
  # remaining 6. The time-weighted average risk-set size is
  # (4 * 3 + 6 * 8) / 10 = 6 = |R_w|; the consumer supplies that to the pin.
  count_w <- 5
  t_w <- 10
  risk_set_w <- 6
  rate <- make_intercept_only_rate(pin_intercept_only_rate(
    count_w,
    t_w,
    risk_set_w
  ))

  # the aggregate intensity at an instant is sum(hazard) = |R(t)| * exp(intercept_w),
  # driven straight off the exposed semantics under the live support set.
  segments <- list(
    list(dur = 4, support = c(rep(1, 3), rep(0, 5))),
    list(dur = 6, support = rep(1, 8))
  )
  expected_count <- sum(vapply(
    segments,
    function(seg) {
      sem <- intercept_only_rate_sender_semantics(
        rate,
        support_legal = seg$support
      )
      seg$dur * sum(sem$hazard)
    },
    numeric(1)
  ))

  # integral_w |R(t)| * exp(intercept_w) dt = count_w exactly, because |R_w| is
  # the period's time-weighted average risk-set size.
  expect_equal(expected_count, count_w)
})

test_that("the semantics perform no draw and hold no empty-support guard", {
  rate <- make_intercept_only_rate(log(0.5))

  # no draw: the result is a distribution, deterministic and RNG-independent
  # (a draw would consume the stream and differ across seeds).
  set.seed(1)
  a <- intercept_only_rate_sender_semantics(rate, c(1, 1, 1))$probability
  set.seed(99)
  b <- intercept_only_rate_sender_semantics(rate, c(1, 1, 1))$probability
  expect_identical(a, b)

  # no empty-support guard: an empty support set does not error and is not
  # silently filled with a uniform default -- it surfaces as NaN for the
  # consuming routine to detect and control.
  sem_empty <- intercept_only_rate_sender_semantics(rate, c(0, 0, 0))
  expect_true(all(sem_empty$hazard == 0))
  expect_true(all(is.nan(sem_empty$probability)))

  # the primitive ships no sender-draw function of its own.
  expect_false(exists("intercept_only_rate_draw_sender", mode = "function"))
  expect_false(exists("intercept_only_rate_sample_sender", mode = "function"))
})

test_that("multi-period semantics select the plateau by time (uniform either way)", {
  rate <- make_intercept_only_rate(c(log(2), log(8)), wave_times = c(0, 4, 9))
  support <- c(1, 1, 0, 1)

  # a multi-period rate needs the firing instant to pick the plateau.
  expect_snapshot(
    error = TRUE,
    intercept_only_rate_sender_semantics(rate, support)
  )

  early <- intercept_only_rate_sender_semantics(rate, support, time = 1)
  late <- intercept_only_rate_sender_semantics(rate, support, time = 6)
  expect_equal(early$hazard[c(1, 2, 4)], rep(2, 3))
  expect_equal(late$hazard[c(1, 2, 4)], rep(8, 3))
  # the sender selection is uniform on either plateau -- the plateau magnitude
  # scales every legal actor's hazard equally, leaving the distribution uniform.
  expect_equal(early$probability, late$probability)
})

test_that("a mismatched self_loop mask is rejected", {
  rate <- make_intercept_only_rate(log(0.5))
  expect_snapshot(
    error = TRUE,
    intercept_only_rate_sender_semantics(
      rate,
      support_legal = c(1, 1, 1),
      self_loop = c(1, 0)
    )
  )
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
    "evaluate_intercept_only_rate",
    "intercept_only_rate_sender_semantics"
  )
  expect_length(intersect(internal, exported), 0)
})
