# exact-time schoenfeld residuals name both routes to them

    Code
      residuals(fit, type = "schoenfeld")
    Condition
      Error in `residuals()`:
      ! Schoenfeld residuals of an exact-time sub-model need the "conditional_scores" primitive, which this fit did not store, or the statistics to recompute it from, which it does not carry either.
      i Re-estimate with `diagnostics` including "conditional_scores" in `set_algorithm_newton()`, or
      i re-estimate with `return_preprocessed = TRUE`, or pass `preprocessed = compute_statistics(..., output = "preprocessed")`.

# a type names the primitive it needs when the fit lacks it

    Code
      residuals(fit, type = "score")
    Condition
      Error in `residuals()`:
      ! Residuals of type "score" need the "scores" primitive, which this fit did not store.
      i Re-estimate with `diagnostics` including "scores" in `set_algorithm_newton()`.

---

    Code
      residuals(no_loglik, type = "deviance")
    Condition
      Error in `residuals()`:
      ! Residuals of type "deviance" need the "loglik" primitive, which this fit did not store.
      i Re-estimate with `diagnostics` including "loglik" in `set_algorithm_newton()`.

