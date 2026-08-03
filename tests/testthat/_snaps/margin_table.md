# margin_table errors without the stored primitive

    Code
      margin_table(fit)
    Condition
      Error in `margin_table()`:
      ! This fit stored no margins.
      i Refit with `diagnostics` including "margins" in `set_algorithm_newton()`.

---

    Code
      margin_table(1:3)
    Condition
      Error in `margin_table()`:
      ! `margin_table()` needs a fitted goldfish model.
      x `x` is an integer vector.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

