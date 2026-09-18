# ESS guard fires and requests a redraw below the enabled threshold

    Code
      decision <- ess_guard(degenerate, ess_threshold = 0.5, enabled = TRUE)
    Condition
      Warning:
      ! Effective sample size 1.04 fell below `ess_threshold` × pool size (1.5); redrawing the pool at the current parameters.
      i Importance weights have degenerated on the persistent pool.

# ESS guard informs without redrawing when disabled

    Code
      decision <- ess_guard(degenerate, ess_threshold = 0.5, enabled = FALSE)
    Condition
      Warning:
      ! Effective sample size 1.04 fell below `ess_threshold` × pool size (1.5), but the redraw guard is disabled; the degenerate pool is kept.
      i Consider enabling the ESS guard or raising `n_sequences`.

