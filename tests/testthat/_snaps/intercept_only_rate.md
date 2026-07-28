# make_intercept_only_rate rejects invalid pins

    Code
      make_intercept_only_rate(NA_real_)
    Condition
      Error:
      ! Each `intercept` entry must be a finite log-hazard or "-Inf".
      x Got NA.

---

    Code
      make_intercept_only_rate(Inf)
    Condition
      Error:
      ! Each `intercept` entry must be a finite log-hazard or "-Inf".
      x Got Inf.

---

    Code
      make_intercept_only_rate(numeric(0))
    Condition
      Error:
      ! `intercept` must be a non-empty numeric vector of pinned log-hazards (one plateau per period).

# pin_intercept_only_rate rejects malformed inputs

    Code
      pin_intercept_only_rate(c(1, 2), 1, c(3, 4))
    Condition
      Error:
      ! `count`, `duration`, and `risk_set_size` must be non-empty vectors of the same length (one entry per period).
      x Got lengths 2, 1, and 2.

---

    Code
      pin_intercept_only_rate(-1, 1, 1)
    Condition
      Error:
      ! `count` must be a non-negative per-period count.
      x Got -1.

---

    Code
      pin_intercept_only_rate(1, 0, 1)
    Condition
      Error:
      ! `duration` (the exposure denominator T_w) must be positive.
      x Got 0.

---

    Code
      pin_intercept_only_rate(1, 1, 0)
    Condition
      Error:
      ! `risk_set_size` (|R_w|) must be a positive average risk-set size.
      i An empty support set is the consuming routine's guard, not a pin.
      x Got 0.

---

    Code
      pin_intercept_only_rate(1, 1, NA_real_)
    Condition
      Error:
      ! `count`, `duration`, and `risk_set_size` must not contain missing values.

# a multi-period pin requires a matching, increasing wave grid

    Code
      make_intercept_only_rate(c(-1, -2))
    Condition
      Error:
      ! `wave_times` is required for a multi-period pin.
      i Supply the 3 period boundaries for the 2 plateaus.

---

    Code
      make_intercept_only_rate(c(-1, -2), wave_times = c(0, 5))
    Condition
      Error:
      ! `wave_times` must be a numeric vector of 3 period boundaries for a 2-period pin.
      x Got 2 values.

---

    Code
      make_intercept_only_rate(c(-1, -2), wave_times = c(0, 5, 3))
    Condition
      Error:
      ! `wave_times` must be strictly increasing (w_0 < w_1 < ... < w_K).

# evaluating a multi-period rate without a time is an error

    Code
      evaluate_intercept_only_rate(rate, active_sender = c(1, 1))
    Condition
      Error:
      ! `time` is required to evaluate a multi-period pinned rate.
      i Supply the event time so the applicable plateau is selected.

# an event outside the supplied partition is flagged

    Code
      intercept_only_rate_period(rate, -1)
    Condition
      Error:
      ! `time` falls outside the pinned period partition [0, 10].
      i The consuming routine must supply `wave_times` covering every event time.

---

    Code
      intercept_only_rate_period(rate, 11)
    Condition
      Error:
      ! `time` falls outside the pinned period partition [0, 10].
      i The consuming routine must supply `wave_times` covering every event time.

# multi-period semantics select the plateau by time (uniform either way)

    Code
      intercept_only_rate_sender_semantics(rate, support)
    Condition
      Error:
      ! `time` is required to select the plateau of a multi-period pinned rate.
      i Supply the firing instant so the applicable per-period hazard is used.

# a mismatched self_loop mask is rejected

    Code
      intercept_only_rate_sender_semantics(rate, support_legal = c(1, 1, 1),
      self_loop = c(1, 0))
    Condition
      Error:
      ! `self_loop` must be the same length as `support_legal`.
      x Got 2 and 3.

# theta_block defends the zero-free-parameters contract

    Code
      intercept_only_rate_theta_block(corrupt)
    Condition
      Error:
      ! A pinned intercept-only rate must carry zero free parameters.
      x Got fixed_intercept = TRUE, n_free_parameters = 1.

---

    Code
      intercept_only_rate_theta_block(list())
    Condition
      Error:
      ! `rate` must be an <intercept_only_rate>.

# joint_theta_layout rejects an unknown block

    Code
      joint_theta_layout(list(c(0.1, 0.2), "not-a-block"))
    Condition
      Error:
      ! Each element of `blocks` must be a numeric free-parameter block or an <intercept_only_rate>.
      x Got a block of class <character>.

# the loglik offset validates its per-period count

    Code
      intercept_only_rate_loglik_offset(rate, 6)
    Condition
      Error:
      ! `count` must be a numeric vector with one entry per period.
      x Got 1 value for 2 periods.

---

    Code
      intercept_only_rate_loglik_offset(rate, c(-1, 2))
    Condition
      Error:
      ! `count` must be non-negative and free of missing values.

