# the evaluator refuses exposure where it is not defined

    Code
      evaluate_model(fit, return = "exposure")
    Condition
      Error in `evaluate_model()`:
      ! "exposure" is not defined for this sub-model.
      x Exposure time is the compensator-scale denominator, which only the exact-time sub-models have.
      i Use `return = "n_opportunities"`, the per-actor availability quantity every family defines.

