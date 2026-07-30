# diagnostic methods throw errors when intervalLogLikelihood isn't present

    Code
      diagnose_outliers(mod00, method = "Top", threshold = 2)
    Condition
      Error in `diagnose_outliers()`:
      ! Outlier identification needs the "loglik" primitive, which this fit did not store.
      i Re-estimate with `diagnostics` including "loglik" in `set_algorithm_newton()`.

---

    Code
      diagnose_changepoints(mod00, moment = "mean", method = "PELT")
    Condition
      Error in `diagnose_changepoints()`:
      ! Changepoint identification needs the "loglik" primitive, which this fit did not store.
      i Re-estimate with `diagnostics` including "loglik" in `set_algorithm_newton()`.

# diagnostic methods does not accept non-result objects

    Code
      diagnose_outliers(depNetwork, method = "Top", threshold = 2)
    Condition
      Error in `diagnose_outliers()`:
      ! `diagnose_outliers()` needs a fitted goldfish model.
      x `x` is a <dependent.goldfish> object.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

---

    Code
      diagnose_changepoints(depNetwork, moment = "mean", method = "PELT")
    Condition
      Error in `diagnose_changepoints()`:
      ! `diagnose_changepoints()` needs a fitted goldfish model.
      x `x` is a <dependent.goldfish> object.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

---

    Code
      diagnose_onset(depNetwork)
    Condition
      Error in `diagnose_onset()`:
      ! `diagnose_onset()` needs a fitted goldfish model.
      x `x` is a <dependent.goldfish> object.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

