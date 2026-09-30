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

