# make_intercept_only_rate rejects invalid pins

    Code
      make_intercept_only_rate(c(1, 2))
    Condition
      Error:
      ! `intercept` must be a single numeric value (a pinned log-hazard).

---

    Code
      make_intercept_only_rate(NA_real_)
    Condition
      Error:
      ! `intercept` must be a finite log-hazard or "-Inf".
      x Got NA.

---

    Code
      make_intercept_only_rate(Inf)
    Condition
      Error:
      ! `intercept` must be a finite log-hazard or "-Inf".
      x Got Inf.

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

