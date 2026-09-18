# cold-start diagnostic fires below the pathology floor

    Code
      decision <- cold_start_ess_diagnostic(w, ess_threshold = 0.5)
    Condition
      Warning:
      ! Startup effective sample size 1.06 is pathologically low (below 2 for a pool of 20); the proposal sits far from the target at the starting parameters.
      i This is a proposal/target mismatch, not ordinary staleness: a redraw at the same parameters would be statistically identical.
      i Consider a model-driven `routine` ("sim" or "mcmc") with `initialize` = "sim", or start from `initial_parameters` = `NULL` (theta0 = 0).

