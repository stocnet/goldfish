# include_censored is deprecated and changes nothing

    Code
      pooled <- diagnose_outliers(fit, method = "Top", threshold = 3,
        include_censored = TRUE)
    Condition
      Warning:
      The `include_censored` argument of `diagnose_outliers()` is deprecated as of goldfish 2.0.0.
      i Each row is now one dependent event, whose span already accumulates the right-censored intervals of its own waiting time, so there is no pooled alternative to select.

---

    Code
      cpt <- diagnose_changepoints(fit, include_censored = TRUE)

# the print methods report scope, not just counts

    Code
      header(diagnose_outliers(fit, method = "Top", threshold = 2))
    Message
      2 outliers identified by the "Top" method.
      Computed over the dependent intervals: 12 of 12 intervals.
    Output
      # A tibble: 2 x 12
         time sender  receiver increment censored n_intervals event_log_lik .fitted

---

    Code
      header(diagnose_changepoints(fit, moment = "mean", method = "PELT"))
    Message
      1 changepoint identified by the "PELT" method.
      Computed over the dependent intervals: 12 of 12 intervals.
    Output
      # A tibble: 1 x 11
         time sender  receiver increment censored n_intervals event_log_lik .fitted

# the print lists the flagged rows, and only those

    Code
      cat(head(capture.output(print(outliers)), 3), sep = "\n")
    Message
      2 outliers identified by the "Top" method.
      Computed over the dependent intervals: 12 of 12 intervals.
    Output
      # A tibble: 2 x 12
         time sender  receiver increment censored n_intervals event_log_lik .fitted
        <dbl> <chr>   <chr>        <dbl> <lgl>          <int>         <dbl>   <dbl>

---

    Code
      print(clean)
    Message
      0 outliers identified by the "IQR" method.
      Computed over the dependent intervals: 12 of 12 intervals.

