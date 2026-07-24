# offset_coef arity and pairing are validated

    Code
      estimate_dynam(calls_dependent ~ inertia + offset(recip), sub_model = "choice",
      data = d, control_algo = set_algorithm_newton(offset_coef = c(1, 2)))
    Condition
      Error in `assemble_fixed_parameters()`:
      ! `offset_coef` must supply one value per `offset()` term.
      x The formula has 1 offset term but `offset_coef` has 2 values.
      i Set it via `set_algorithm_newton(offset_coef = ...)`.

