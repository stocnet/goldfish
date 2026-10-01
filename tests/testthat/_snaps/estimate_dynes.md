# estimate_dynes aborts to estimate_dynam without a modeled panel

    Code
      estimate_dynes(dynes_surface_spec(calls = "event"), surface_control())
    Condition
      Error in `estimate_dynes()`:
      ! No panel layer of `spec` is a modeled dependent process.
      x Nothing is latent, so this specification is a DyNAM, not a DyNES.
      i Its panel layers would only be considered as static exogenous covariates, jumping at their wave times.
      i Estimate its processes with `estimate_dynam()`.

# the warm-start fallback warning names the fid

    Code
      res <- estimate_dynes(rq_layout_spec("panel"), control)
    Condition
      Warning in `estimate_dynes()`:
      ! The warm-start fit for "calls › choice" failed on the drawn sequence; its free parameters start at zero.
      i The other processes keep their warm-start values.

# the explicit-n_cores-without-mirai warning

    Code
      seam <- dynes_map_seam(surface_control(n_cores = 3L))
    Condition
      Warning:
      ! `n_cores` = 3 was requested, but mirai is not installed; running serially.
      i Install mirai to run the per-sequence work in parallel. The result is the same either way.

# the fixed-term degeneracy warning

    Code
      warn_fixed_term_degenerate(tallied_store(12L, 5L, c("friendship (rate)",
        "calls (choice)")))
    Condition
      Warning:
      ! 5 of 12 drawn sequences had a non-finite or unevaluable fixed-only log-likelihood in "friendship (rate)" and "calls (choice)" and were dropped as degenerate.
      i This is likely a bug in goldfish; please report it.

# the fixed-term degeneracy warning after a full pool drop

    Code
      warn_fixed_term_degenerate(tallied_store(4L, 4L, "friendship (rate)",
        pool_dropped = TRUE))
    Condition
      Warning:
      ! 4 of 4 drawn sequences had a non-finite or unevaluable fixed-only log-likelihood in "friendship (rate)" and were dropped as degenerate.
      ! Every sequence in the pool was dropped; estimation stopped at the last accepted parameters and the fit is flagged `converged = FALSE`.
      i This is likely a bug in goldfish; please report it.

# a partially modeled panel layer aborts

    Code
      estimate_dynes(spec, surface_control(), wave_times = c(0, 5, 10))
    Condition
      Error in `estimate_dynes()`:
      ! A modeled panel layer must model all of its flavors.
      x Layer "friendship" carries flavor "dissolution" modeled in neither `rate` nor `choice`.
      i A modeled panel layer's augmented path must place events of every flavor its wave-diff produces; model "dissolution" or drop it from the panel layer.

