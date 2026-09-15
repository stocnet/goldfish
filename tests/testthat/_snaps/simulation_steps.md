# a step whose dry call errors is refused, naming the step

    Code
      set_simulation_steps(clock = function(rates, t, handle) stop("boom"))
    Condition
      Error in `set_simulation_steps()`:
      ! The `clock` step failed its dry call.
      x boom
      i It was called as `clock(rates, t, handle)` on a one-actor toy problem.

