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

