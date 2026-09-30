# an initial sequence that misses the waves aborts

    Code
      augment_seq_mcmc(built$spec, built$recipe, mcmc_control(initial_sequence = start))
    Condition
      Error:
      ! The initial sequence does not hit the panel waves.
      x 1 change falls on a wave time.
      x 1 per-dyad chain carries the wrong number of events.
      i An initial sequence must place every change the waves record strictly inside its between-wave interval.

