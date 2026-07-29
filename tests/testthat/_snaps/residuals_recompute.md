# cox_snell says so where there is no compensator

    Code
      residuals(fit_choice(), type = "cox_snell")
    Condition
      Error in `residuals()`:
      ! Cox-Snell residuals are defined for the exact-time sub-models only.
      x They are the compensator of an interval, and a multinomial likelihood has none: it models which alternative was realized, not when.
      i Use `type = "deviance"` for a per-interval goodness-of-fit measure on this sub-model.

# schoenfeld names both routes when neither is available

    Code
      residuals(fit, type = "schoenfeld")
    Condition
      Error in `residuals()`:
      ! Schoenfeld residuals of an exact-time sub-model need the "conditional_scores" primitive, which this fit did not store, or the statistics to recompute it from, which it does not carry either.
      i Re-estimate with `diagnostics` including "conditional_scores" in `set_algorithm_newton()`, or
      i re-estimate with `return_preprocessed = TRUE`, or pass `preprocessed = compute_statistics(..., output = "preprocessed")`.

# the dyad level says so where there is no second axis

    Code
      residuals(fit_rate(), type = "martingale", level = "dyad")
    Condition
      Error in `residuals()`:
      ! `level = "dyad"` is not defined for a sender-axis sub-model.
      x Its alternatives are actors, not dyads: there is no second axis for a per-dyad map to range over.
      i Use `level = "actor"`, which is the per-alternative answer for this sub-model.

