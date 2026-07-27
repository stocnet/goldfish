# two value sources for one term abort

    Code
      assemble_from_formula(calls_dependent ~ inertia + offset(recip, coef = 2),
      offset_coef = 2)
    Condition
      Error in `assemble_fixed_parameters()`:
      ! A fixed coefficient cannot come from two sources.
      x `recip(call_network)` has a `coef` value in the formula and a value in `offset_coef`.
      i Keep one: the formula's `coef = ` or `set_algorithm_newton(offset_coef = ...)`.

# an offset term with no value from either source aborts

    Code
      assemble_from_formula(calls_dependent ~ inertia + offset(recip))
    Condition
      Error in `assemble_fixed_parameters()`:
      ! Every `offset()` term needs a fixed coefficient value.
      x No value for `recip(call_network)`.
      i Supply it in the formula as `offset(term, coef = value)` or via `set_algorithm_newton(offset_coef = ...)`.

# fixed-coefficient errors name the offending term

    Code
      assemble_from_formula(calls_dependent ~ inertia + offset(recip), offset_coef = c(
        1, 2))
    Condition
      Error in `assemble_fixed_parameters()`:
      ! `offset_coef` must supply one value per `offset()` term.
      x The formula has 1 offset term (`recip(call_network)`) but `offset_coef` has 2 values.
      i Set it via `set_algorithm_newton(offset_coef = ...)`.

---

    Code
      assemble_from_formula(calls_dependent ~ inertia + recip, fixed_parameters = c(
        NA, 2, NA))
    Condition
      Error in `assemble_fixed_parameters()`:
      ! `fixed_parameters` must supply one entry per coefficient.
      x The model has 2 coefficients (`inertia(call_network)` and `recip(call_network)`) but `fixed_parameters` has 3 entries.
      i Wrap a term in `offset()` to fix its coefficient by name instead.

