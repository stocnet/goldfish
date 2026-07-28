# exact-time schoenfeld residuals say why they are unavailable

    Code
      residuals(fit, type = "schoenfeld")
    Condition
      Error in `residuals()`:
      ! Schoenfeld residuals of an exact-time sub-model are not available from stored primitives.
      i Their rows drop the exposure term the stored score rows carry, which needs the observed alternative's statistic row.
      i Use `type = "score"` for the score rows this fit stores.

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

