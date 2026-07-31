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
    Output
      

# a term absent from one process names that process

    Code
      test_gof(container, effects = "trans/calls")
    Condition
      Error in `FUN()`:
      ! `test_gof()` could not test process "calls › creation › rate".
      Caused by error in `test_gof()`:
      ! `effects` names a term this model does not have.
      x Unknown: "trans/calls".
      i Available: "Intercept" and "indeg/calls".
      i An effect name selects all of its terms: "Intercept" and "indeg".
      i Search them with `model_terms(fit, pattern = )`.

# the blocked print groups by process, with no combination

    Code
      print(test_gof(container))
    Message
      -- <test_gof> ------------------------------------------------------------------
      Model "DyNAM" · layer "calls" · 2 flavors over 4 processes
      Supremum of the standardized cumulative score process, against the Kolmogorov
      distribution on the event clock.
      
      creation · rate
    Output
      # A tibble: 2 x 3
        term        statistic p_value
        <chr>           <dbl>   <dbl>
      1 Intercept       0.248   1.000
      2 indeg/calls     0.647   0.797
    Message
      
      creation · choice
    Output
      # A tibble: 1 x 3
        term        statistic p_value
        <chr>           <dbl>   <dbl>
      1 trans/calls     0.519   0.951
    Message
      
      dissolution · rate
    Output
      # A tibble: 2 x 3
        term        statistic p_value
        <chr>           <dbl>   <dbl>
      1 Intercept       0.503   0.962
      2 indeg/calls     0.805   0.536
    Message
      
      dissolution · choice
    Output
      # A tibble: 1 x 3
        term        statistic p_value
        <chr>           <dbl>   <dbl>
      1 trans/calls     0.904   0.387

