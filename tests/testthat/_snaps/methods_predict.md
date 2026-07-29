# fitted outcomes name the primitive they need

    Code
      fitted(fit)
    Condition
      Error in `fitted()`:
      ! Fitted values of type "outcome" need the "loglik" primitive, which this fit did not store.
      i Re-estimate with `diagnostics` including "loglik" in `set_algorithm_newton()`.

# events selects intervals by position or by mask

    Code
      predict(fit, type = "ranks", events = c(1L, n_intervals + 1L))
    Condition
      Error in `predict()`:
      ! `events` must index this fit's intervals.
      x It must lie in 1:12.

---

    Code
      predict(fit, type = "ranks", events = c(TRUE, FALSE))
    Condition
      Error in `predict()`:
      ! A logical `events` must have one value per interval.
      x It has 2; the fit has 12.

