# the print methods report scope, not just counts

    Code
      header(diagnose_outliers(fit, method = "Top", threshold = 2))
    Message
      2 outliers identified by the "Top" method.
      Computed over the dependent intervals: 12 of 16 intervals.
    Output
      # A tibble: 16 x 11
          time sender receiver increment right_censored_event interval_log_lik .fitted

---

    Code
      header(diagnose_changepoints(fit, moment = "mean", method = "PELT"))
    Message
      1 changepoint identified by the "PELT" method.
      Computed over the dependent intervals: 12 of 16 intervals.
    Output
      # A tibble: 16 x 10
          time sender receiver increment right_censored_event interval_log_lik .fitted

---

    Code
      header(diagnose_outliers(fit, method = "Top", threshold = 1, include_censored = TRUE))
    Message
      1 outlier identified by the "Top" method.
      Computed over all intervals, right-censored included: 16 of 16 intervals.
    Output
      # A tibble: 16 x 11
          time sender receiver increment right_censored_event interval_log_lik .fitted

