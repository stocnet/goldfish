# the default methods name what they received

    Code
      test_gof(1:3)
    Condition
      Error in `test_gof()`:
      ! `test_gof()` needs a fitted goldfish model.
      x `object` is an integer vector.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

---

    Code
      test_parameter("a")
    Condition
      Error in `test_parameter()`:
      ! `test_parameter()` needs a fitted goldfish model.
      x `x` is a string.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

---

    Code
      test_time(list())
    Condition
      Error in `test_time()`:
      ! `test_time()` needs a fitted goldfish model.
      x `x` is an empty list.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

