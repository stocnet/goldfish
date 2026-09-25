# the warm-start fallback warning names the fid

    Code
      res <- estimate_dynes(rq_layout_spec(), control)
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

