# offset terms are excluded, and naming one has a destination

    Code
      test_gof(fit, effects = "outdeg/networkState [Fx]")
    Condition
      Error in `test_gof()`:
      ! `effects` names 1 term held fixed through `offset()`: "outdeg/networkState [Fx]".
      x A fixed coefficient's cumulative score process is not a bridge — it is not zero at the optimum, the coefficient never having been moved there.
      i `test_parameter()` tests a coefficient at the value `offset()` imposed.

---

    Code
      test_gof(fit, effects = 3)
    Condition
      Error in `test_gof()`:
      ! `effects` names 1 term held fixed through `offset()`: "outdeg/networkState [Fx]".
      x A fixed coefficient's cumulative score process is not a bridge — it is not zero at the optimum, the coefficient never having been moved there.
      i `test_parameter()` tests a coefficient at the value `offset()` imposed.

# a fit without the score rows says which primitive to store

    Code
      test_gof(fit)
    Condition
      Error in `test_gof()`:
      ! The goodness-of-fit test needs the "scores" primitive, which this fit did not store.
      i Re-estimate with `diagnostics` including "scores" in `set_algorithm_newton()`.

# the replication count is checked before anything is computed

    Code
      test_gof(gof_fixture(), n_sim = 0)
    Condition
      Error in `test_gof()`:
      ! `n_sim` must be a single positive number.

# an effect contributing no score at all is named, not divided by

    Code
      gof_processes(scores, "event", tested = 1:2)
    Condition
      Error:
      ! 1 tested effect contributes no score at all.
      x Its cumulative process is identically zero, so there is nothing to standardize and no statistic to read.
      i Check the model for an effect that is constant across every risk set.

# print names the reference the p-values came from

    Code
      header(test_gof(fit))
    Message
      -- <test_gof> ------------------------------------------------------------------
      Model "DyNAM" · sub-model "rate" · backend "cpp"
      16 intervals, 12 dependent events; 4 effects tested.
      Supremum of the standardized cumulative score process, against the Kolmogorov
      distribution on the event clock.
      Cauchy omnibus over 4 effects: p = 0.922
    Output
      

---

    Code
      header(test_gof(fit, clock = "information", n_sim = 100))
    Message
      -- <test_gof> ------------------------------------------------------------------
      Model "DyNAM" · sub-model "rate" · backend "cpp"
      16 intervals, 12 dependent events; 4 effects tested.
      Supremum of the standardized cumulative score process, against 100 simulated
      bridges on the information clock.
      Cauchy omnibus over 4 effects: p = 0.68
    Output
      

