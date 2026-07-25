# the gather backend rejects an explicit scores request

    Code
      suppressWarnings(fit_scores(return_event_scores = TRUE))
    Condition
      Error in `estimate_wrapper()`:
      ! The "scores" diagnostic (per-event score matrix) is not supported with `backend = "gather"`.
      i Use `backend = "cpp"` or `backend = "r"` to store the per-event score matrix.

---

    Code
      fit_scores(diagnostics = c("loglik", "scores"))
    Condition
      Error in `estimate_wrapper()`:
      ! The "scores" diagnostic (per-event score matrix) is not supported with `backend = "gather"`.
      i Use `backend = "cpp"` or `backend = "r"` to store the per-event score matrix.

