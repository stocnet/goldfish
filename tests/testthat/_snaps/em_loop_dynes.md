# the retry-exhaustion error explains the stall

    Code
      run_abem_loop(theta0 = 0, decomposition = analytic_decomposition(), augmenter = new_stub_augmenter(
        list(1)), control = analytic_em_control(max_retries = 2L))
    Condition
      Error:
      ! The EM ascent could not be certified at iteration 1 within 2 pool-growth retries.
      x Q stayed below its acceptance bound across 3 passes (Q from -1.5 to -1.5); the pool grew from 4 to 8 sequences, with an effective sample size of 4 at its lowest.
      i The accumulated trace is attached to this error as em_trace: `rlang::last_error()$em_trace`.
      i Consider a larger `n_sequences`, a smaller `step_size`, or a larger `max_retries`.

# the max_pool warning names the near-optimum condition

    Code
      res <- run_abem_loop(theta0 = 0, decomposition = analytic_decomposition(),
      augmenter = new_stub_augmenter(list(-0.2, 1)), control = analytic_em_control(
        max_pool = 6L, max_iterations = 1L, tolerance = 1e-08))
    Condition
      Warning:
      ! The post-acceptance pool size (15) exceeds `max_pool` (6); the next pool is capped.
      i A Q small enough to ask for this many sequences means the loop is near the optimum. Consider relaxing `tolerance` or lowering `stop_count`.
      Warning:
      ! The ABEM loop reached `max_iterations` (1) without meeting the stop rule.
      i Returning the last accepted parameters; the fit is flagged `converged = FALSE`.

