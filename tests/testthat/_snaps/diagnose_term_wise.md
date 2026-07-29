# effect selects one term at a time

    Code
      diagnose_outliers(fit, effect = c("inertia/networkState", "recip/networkState"))
    Condition
      Error in `diagnose_outliers()`:
      ! `effect` must select a single term.
      x It selects 2.
      i Call `diagnose_outliers()` once per term.

---

    Code
      diagnose_changepoints(fit, effect = "nope")
    Condition
      Error in `diagnose_changepoints()`:
      ! `effect` names a term this model does not have.
      x Unknown: "nope".
      i Available: "inertia/networkState", "recip/networkState", and "trans/networkState".
      i Search them with `model_terms(fit, pattern = )`.

