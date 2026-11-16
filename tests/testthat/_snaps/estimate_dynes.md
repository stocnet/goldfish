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
    Message
      i These processes read no modeled panel layer: "emails › rate" and "emails › choice".
        Their likelihood terms touch no latent path, so estimating them jointly equals estimating them on their own.
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

# the pool degeneracy warning

    Code
      warn_pool_degenerate(tallied_store(8L, 0L, character(0), pool_dropped = TRUE,
      pool_lost = c(walk = 3L, fixed_term = 0L, evaluation = 1L)))
    Condition
      Warning:
      ! No sequence of the pool had a finite likelihood; estimation stopped at the last accepted parameters and the fit is flagged `converged = FALSE`.
      i Of the 4 pooled sequences:
      * 3 failed their preprocessing walk.
      * 1 had a non-finite likelihood at the current parameters.

# both degeneracy warnings after a pool lost to mixed causes

    Code
      warn_fixed_term_degenerate(store)
    Condition
      Warning:
      ! 2 of 4 drawn sequences had a non-finite or unevaluable fixed-only log-likelihood in "friendship (rate)" and were dropped as degenerate.
      i This is likely a bug in goldfish; please report it.
    Code
      warn_pool_degenerate(store)
    Condition
      Warning:
      ! No sequence of the pool had a finite likelihood; estimation stopped at the last accepted parameters and the fit is flagged `converged = FALSE`.
      i Of the 4 pooled sequences:
      * 2 failed their preprocessing walk.
      * 2 had a non-finite fixed-only term, reported in a separate warning.

# a partially modeled panel layer aborts

    Code
      estimate_dynes(spec, surface_control(), wave_times = c(0, 5, 10))
    Condition
      Error in `estimate_dynes()`:
      ! A modeled panel layer must model all of its flavors.
      x Layer "friendship" carries flavor "dissolution" modeled in neither `rate` nor `choice`.
      i A modeled panel layer's augmented path must place events of every flavor its wave-diff produces; model "dissolution" or drop it from the panel layer.

# a mixed specification names its separable fids and proceeds

    Code
      cat(conditionMessage(seen$messages[[1]]))
    Output
      i These processes read no modeled panel layer: "emails › rate" and "emails › choice".
        Their likelihood terms touch no latent path, so estimating them jointly equals estimating them on their own.

# a window over a modeled panel layer is refused at entry

    Code
      estimate_dynes(spec, surface_control(), wave_times = c(0, 5, 10))
    Condition
      Error in `estimate_dynes()`:
      ! A `window` cannot read the modeled panel layer "friendship".
      x Windows over a modeled panel layer, whose events are augmented between waves, are not supported in this version.
      i Windows are supported over observed layers only; drop the `window` argument.

# a Date grid on a POSIXct time column aborts at entry

    Code
      estimate_dynes(spec, surface_control(), wave_times = as_date_days(c(0, 5, 10)))
    Condition
      Error in `estimate_dynes()`:
      ! `wave_times` must have the time class of the rows it marks.
      x `wave_times` is <Date>, but the time column of layer "calls" is <POSIXct>.
      i Give the grid as <POSIXct>: a <Date> counts days and a <POSIXct> seconds, so mixed classes compare different units.

# a POSIXct grid on a Date time column aborts at entry

    Code
      estimate_dynes(spec, surface_control(), wave_times = as_posix_days(c(0, 5, 10)))
    Condition
      Error in `estimate_dynes()`:
      ! `wave_times` must have the time class of the rows it marks.
      x `wave_times` is <POSIXct>, but the time column of layer "calls" is <Date>.
      i Give the grid as <Date>: a <Date> counts days and a <POSIXct> seconds, so mixed classes compare different units.

# every modeled panel layer's time column is checked

    Code
      estimate_dynes(spec, surface_control(), wave_times = c(0, 5, 10))
    Condition
      Error in `estimate_dynes()`:
      ! `wave_times` must have the time class of the rows it marks.
      x `wave_times` is <numeric>, but the time column of layer "texts" is <Date>.
      i Give the grid as <Date>: a <Date> counts days and a <POSIXct> seconds, so mixed classes compare different units.

# a character grid aborts, its hint naming no unit

    Code
      estimate_dynes(spec, surface_control(), wave_times = grid)
    Condition
      Error in `estimate_dynes()`:
      ! `wave_times` must have the time class of the rows it marks.
      x `wave_times` is <character>, but the time column of layer "calls" is <POSIXct>.
      i Convert the grid to <POSIXct> first; a <character> grid is never parsed.

