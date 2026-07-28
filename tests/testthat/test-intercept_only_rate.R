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

# Zero-free-parameters contract (Session 4): the pinned intercept never enters
# the fid / θ layout and is excluded from the score & Hessian by θ-INDEPENDENCE
# (not iteration-constancy). It contributes the empty θ block, so adding it to a
# joint fit leaves the θ layout / score / Hessian dimensions unchanged; it MAY be
# a reported constant offset only.

test_that("a pinned rate contributes the empty theta block", {
  rate <- make_intercept_only_rate(log(0.5))
  # It occupies NO theta slot -- distinct from a fixed/offset() term, which keeps
  # its slot and is merely held constant. The empty block is the θ-independence
  # exclusion made concrete.
  expect_identical(intercept_only_rate_theta_block(rate), numeric(0))
})

test_that("theta_block defends the zero-free-parameters contract", {
  rate <- make_intercept_only_rate(log(0.5))
  # a corrupted object that claims a free parameter must be rejected, not laid out
  corrupt <- rate
  corrupt$n_free_parameters <- 1L
  expect_snapshot(error = TRUE, intercept_only_rate_theta_block(corrupt))
  expect_snapshot(error = TRUE, intercept_only_rate_theta_block(list()))
})

test_that("adding a pinned rate leaves the joint theta layout unchanged", {
  # two estimated flavors with 3 and 2 free parameters each
  estimated <- list(flavor_a = c(0.1, -0.2, 0.3), flavor_b = c(1.5, -1.0))
  pinned <- make_intercept_only_rate(log(0.5))

  base <- joint_theta_layout(estimated)
  with_pin <- joint_theta_layout(
    c(estimated, list(flavor_pinned = pinned))
  )

  # the flat theta vector and the estimated flavors' index slices are IDENTICAL
  # -- the pinned flavor took up no dimension.
  expect_identical(with_pin$theta, base$theta)
  expect_identical(with_pin$n_free, base$n_free)
  expect_identical(with_pin$index$flavor_a, base$index$flavor_a)
  expect_identical(with_pin$index$flavor_b, base$index$flavor_b)
  # the pinned flavor owns the empty slice
  expect_identical(with_pin$index$flavor_pinned, integer(0))
  expect_identical(with_pin$n_free, 5L)
})

test_that("a joint layout of only pinned rates has an empty theta", {
  layout <- joint_theta_layout(list(
    make_intercept_only_rate(log(0.5)),
    make_intercept_only_rate(c(log(2), log(3)), wave_times = c(0, 4, 9))
  ))
  expect_identical(layout$theta, numeric(0))
  expect_identical(layout$n_free, 0L)
})

test_that("joint_theta_layout rejects an unknown block", {
  expect_snapshot(
    error = TRUE,
    joint_theta_layout(list(c(0.1, 0.2), "not-a-block"))
  )
})

test_that("the pinned value is iteration-constant and theta-independent", {
  wave_times <- c(0, 4, 10, 12)
  rate <- make_intercept_only_rate(
    pin_intercept_only_rate(c(8, 3, 5), diff(wave_times), c(5, 4.5, 6)),
    wave_times = wave_times
  )
  count <- c(8, 3, 5)
  before_intercept <- rate$intercept
  before_offset <- intercept_only_rate_loglik_offset(rate, count)

  # drive several "iterations": place generated events, re-read the layout and
  # the offset. Nothing recomputes the pin, and the offset function takes no θ,
  # so both are byte-identical every iteration regardless of any parameter state.
  for (iter in seq_len(5)) {
    invisible(joint_theta_layout(list(rate, c(0.1 * iter, -0.2 * iter))))
    for (t in c(1, 5, 7, 11)) {
      invisible(evaluate_intercept_only_rate(
        rate,
        active_sender = c(1, 1, 1),
        time = t
      ))
    }
    expect_identical(rate$intercept, before_intercept)
    expect_identical(
      intercept_only_rate_loglik_offset(rate, count),
      before_offset
    )
  }
})

test_that("the reported offset is the theta-independent constant count*(intercept-1)", {
  rate <- make_intercept_only_rate(
    c(log(2), log(4)),
    wave_times = c(0, 5, 10)
  )
  count <- c(6, 2)
  expect_equal(
    intercept_only_rate_loglik_offset(rate, count),
    sum(count * (c(log(2), log(4)) - 1))
  )
})

test_that("a zero-count period contributes nothing to the offset", {
  rate <- make_intercept_only_rate(
    c(-Inf, log(3)),
    wave_times = c(0, 5, 10)
  )
  # 0 * (-Inf - 1) is NaN in raw arithmetic; the offset must treat it as 0.
  offset <- intercept_only_rate_loglik_offset(rate, c(0, 4))
  expect_false(is.nan(offset))
  expect_equal(offset, 4 * (log(3) - 1))
})

test_that("the loglik offset validates its per-period count", {
  rate <- make_intercept_only_rate(c(log(2), log(4)), wave_times = c(0, 5, 10))
  # wrong length (must be one per period)
  expect_snapshot(error = TRUE, intercept_only_rate_loglik_offset(rate, 6))
  # a negative count is not a count
  expect_snapshot(
    error = TRUE,
    intercept_only_rate_loglik_offset(rate, c(-1, 2))
  )
})

# User surface (Session 5): in the generative context an intercept-only rate
# (`rate = ~ 1`, no other effects, or a completion-supplied rate) is *pinned* --
# a user-written `~ 1` and a completion-supplied rate reduce to the SAME pinned
# object -- while a rate carrying any effect keeps its estimated baseline. The
# reinterpretation is scoped by type to a `joint_specification.goldfish`, so the
# single-process estimation path is untouched.

# cli snapshots are pinned to a reproducible width / no-color context.
local_cli_context <- function(env = parent.frame()) {
  withr::local_options(cli.width = 80, cli.num_colors = 1, .local_envir = env)
}

# Two DyNAM processes over one shared data object with a panel-observed
# friendship layer (read by the calls choice, so the join's panel-reference
# requirement is met). Both processes carry an estimated-baseline `~ 1 + inertia`
# rate; the caller may inject an intercept-only rate into a chosen layer to stand
# in for a completion-supplied rate (make_specification() cannot yet parse a bare
# `~ 1`, so a synthesized bundle models the completion route).
pinned_joint_data <- function() {
  nodes <- data.frame(
    label = paste0("N", 1:6),
    mode = "p",
    stringsAsFactors = FALSE
  )
  ties <- rbind(
    data.frame(
      from = c(1L, 2L, 3L, 4L),
      to = c(2L, 3L, 4L, 5L),
      time = c(1, 2, 3, 4),
      layer = "friendship"
    ),
    data.frame(
      from = c(1L, 2L, 3L, 4L, 5L),
      to = c(2L, 3L, 4L, 5L, 1L),
      time = c(1, 2, 3, 4, 5),
      layer = "calls"
    ),
    data.frame(
      from = c(2L, 3L, 4L),
      to = c(1L, 2L, 3L),
      time = c(1, 2, 3),
      layer = "emails"
    )
  )
  info <- list(
    name = "toy",
    focal = "calls",
    update = c(
      friendship = "increment",
      calls = "increment",
      emails = "increment"
    ),
    directed = c(friendship = TRUE, calls = TRUE, emails = TRUE),
    observation = c(friendship = "panel", calls = "event", emails = "event")
  )
  list(info = info, nodes = nodes, ties = ties)
}

# The synthesized intercept-only rate bundle a completion transform would attach
# to a choice-only flavor: a rate sub_model carrying an intercept and no effects.
# It is the SAME shape a user-written `~ 1` parses to, so the classifier and the
# pinned descriptor cannot tell the two sources apart.
completion_rate_bundle <- function() {
  list(
    input_formula = ~1,
    sub_model = "rate",
    parsed = list(rhs_names = list(), has_intercept = TRUE),
    has_intercept = TRUE
  )
}

test_that("an intercept-only rate bundle is classified by shape, source-agnostic", {
  data <- pinned_joint_data()
  estimated_rate <- make_specification(
    rate = ~ 1 + inertia,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )$submodels$rate

  # a rate carrying any effect is NOT intercept-only (keeps its estimated
  # baseline); a completion-supplied bare-intercept rate IS.
  expect_false(is_intercept_only_rate_bundle(estimated_rate))
  expect_true(is_intercept_only_rate_bundle(completion_rate_bundle()))

  # the classifier reads only the shape, never provenance: a user `~ 1` bundle
  # and a completion bundle carrying different metadata classify identically.
  user_bundle <- c(completion_rate_bundle(), list(source = "user"))
  completion_bundle <- c(
    completion_rate_bundle(),
    list(source = "completion", synthesized = TRUE)
  )
  expect_true(is_intercept_only_rate_bundle(user_bundle))
  expect_true(is_intercept_only_rate_bundle(completion_bundle))

  # a choice family is never intercept-only (has_intercept is FALSE upstream).
  choice_bundle <- make_specification(
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )$submodels$choice
  expect_false(is_intercept_only_rate_bundle(choice_bundle))
})

test_that("a user `~ 1` and a completion-supplied rate pin to the same object", {
  user_bundle <- c(completion_rate_bundle(), list(source = "user"))
  completion_bundle <- c(
    completion_rate_bundle(),
    list(source = "completion", synthesized = TRUE)
  )
  # identical zero-parameter descriptor regardless of source -- one concept.
  expect_identical(
    pinned_rate_descriptor(user_bundle, "DyNAM"),
    pinned_rate_descriptor(completion_bundle, "DyNAM")
  )
  desc <- pinned_rate_descriptor(completion_rate_bundle(), "DyNAM")
  expect_true(desc$fixed_intercept)
  expect_identical(desc$n_free_parameters, 0L)
  expect_true(desc$pinned)
  expect_identical(desc$effects, character(0))
  expect_identical(desc$model_type, "DyNAM-M-Rate")
  # a tie-oriented (REM) flavor routes through the per-dyad evaluator.
  expect_identical(
    pinned_rate_descriptor(completion_rate_bundle(), "REM")$model_type,
    "REM"
  )
})

test_that("a rate carrying any effect is never pinned", {
  data <- pinned_joint_data()
  effect_rate <- make_specification(
    rate = ~ 1 + inertia,
    choice = ~inertia,
    layer = "calls",
    model = "DyNAM",
    data = data
  )$submodels$rate
  # the descriptor refuses to pin an estimated-baseline rate.
  expect_snapshot(error = TRUE, pinned_rate_descriptor(effect_rate, "DyNAM"))
})

test_that("mark_pinned_rates pins an intercept-only rate in a joint spec", {
  data <- pinned_joint_data()
  calls <- make_specification(
    rate = ~ 1 + inertia,
    choice = ~ inertia + tie(friendship),
    layer = "calls",
    model = "DyNAM",
    data = data
  )
  emails <- make_specification(
    rate = ~ 1 + inertia,
    choice = ~inertia,
    layer = "emails",
    model = "DyNAM",
    data = data
  )
  # emails' rate is completion-supplied intercept-only; calls' rate is estimated.
  emails$submodels$rate <- completion_rate_bundle()
  joint <- make_joint_specification(calls, emails, data = data)

  marked <- mark_pinned_rates(joint)
  map <- marked$process_map

  # only the emails rate fid is pinned; the estimated calls rate and both choice
  # fids keep their estimated baseline.
  expect_identical(
    map$pinned,
    map$layer == "emails" & map$family == "rate"
  )
  pinned_fid <- map$fid[map$pinned]
  expect_length(pinned_fid, 1L)
  # the pinned fid carries the zero-parameter descriptor, attached by fid.
  desc <- marked$pinned_rates[[as.character(pinned_fid)]]
  expect_identical(desc$n_free_parameters, 0L)
  expect_true(desc$pinned)
  expect_identical(desc$model_type, "DyNAM-M-Rate")
})

test_that("adding a pinned rate to a joint fit leaves theta/score/Hessian unchanged", {
  # The full joint-fit assertion needs the joint optimizer (estimate_dynes()),
  # which does not exist yet -- deferred behind this guard (see progress.md
  # Session 4). The unit-level θ-exclusion above (joint_theta_layout /
  # theta_block) stands in until the consumer lands.
  skip_if_not(exists("estimate_dynes"), "estimate_dynes() not yet implemented")

  # When estimate_dynes() lands: fit a joint spec WITHOUT a pinned rate, capture
  # theta length / score length / Hessian dim; add a pinned intercept-only rate
  # flavor; refit; assert all three dimensions are unchanged and only a reported
  # log-likelihood offset differs. Left as an explicit failing marker so the
  # deferral is visible if the guard is ever removed prematurely.
  expect_true(FALSE)
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
    "intercept_only_rate_sender_semantics",
    "intercept_only_rate_theta_block",
    "joint_theta_layout",
    "intercept_only_rate_loglik_offset",
    "is_intercept_only_rate_bundle",
    "pinned_rate_model_type",
    "pinned_rate_descriptor",
    "mark_pinned_rates"
  )
  expect_length(intersect(internal, exported), 0)
})
