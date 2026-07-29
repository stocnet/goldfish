# the unshipped information variant is named, not substituted

    Code
      diagnose_onset(fit, information = "expected")
    Condition
      Error in `diagnose_onset()`:
      ! `information = "expected"` has not shipped yet.
      x It needs the per-interval Fisher contributions, which no stored primitive carries: the fit keeps the total information matrix only.
      i Use `information = "opg"`, the outer-product form, which is what a cumulative share needs.

---

    Code
      diagnose_onset(fit, tolerance = -1)
    Condition
      Error in `diagnose_onset()`:
      ! `tolerance` must be a single positive number.

# a fit without the score rows says which primitive to store

    Code
      diagnose_onset(fit)
    Condition
      Error in `diagnose_onset()`:
      ! The onset diagnostic needs the "scores" primitive, which this fit did not store.
      i Re-estimate with `diagnostics` including "scores" in `set_algorithm_newton()`.

# print reports the excursion, and says it is descriptive

    Code
      header(diagnose_onset(fit))
    Message
      -- <diagnose_onset> ------------------------------------------------------------
      Model "DyNAM" · sub-model "rate" · backend "cpp"
      16 intervals, 12 dependent events; "opg" information accrual.
      Every path is back within 0.1 standard errors of its estimate by the first 5
      events, by when 36% of the information has accrued.
      Descriptive: this is a reading of which events carry the estimate, not a test,
      and no p-value is computed.
    Output
      

