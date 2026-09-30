# cold-start diagnostic fires below the pathology floor

    Code
      decision <- cold_start_ess_diagnostic(w, ess_threshold = 0.5)
    Condition
      Warning:
      ! Startup effective sample size 1.06 is pathologically low (below 2 for a pool of 20); the proposal sits far from the target at the starting parameters.
      i This is a proposal/target mismatch, not ordinary staleness: a redraw at the same parameters would be statistically identical.
      i Consider a model-driven `routine` ("sim" or "mcmc") with `initialize` = "sim", or start from `initial_parameters` = `NULL` (theta0 = 0).

# mid-run ESS collapse recommends augment_seq_mcmc() once per run

    Code
      fired <- recommend_mcmc_on_ess_collapse(state, w, routine = "random")
    Condition
      Warning:
      ! Effective sample size 1.06 has collapsed mid-run under the importance-weighted `routine` = "random" (pool size 20); redrawing cannot recover it.
      i With many changes per interval the importance-weight variance grows with the sequence-space dimension, a cost no redraw at any parameters removes.
      i Switch to `augment_seq_mcmc()`, which does not pay the importance-sampling dimension tax.

# restart-coupling warning fires exactly once per run

    Code
      first <- warn_restart_coupling_once(state, restarted = TRUE, routine = "sim",
        constraint = constraint)
    Condition
      Warning:
      ! A coupled constraint forced the "sim" augmenter to reject and restart at least one interval during this run.
      i Coupled support constraint: "friendship: ~tie(friendship) | recip(friendship)".
      i Restart-conditioning normalizes the proposal density by an estimated completion probability, a plug-in normalizer that biases the importance weights.
      i Consider `augment_seq_mcmc()`, or loosening the coupled constraint, if the coupling is severe.

