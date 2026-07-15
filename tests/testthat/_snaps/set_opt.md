# opportunities_list is deprecated in favour of support_constraint

    Code
      invisible(set_preprocessing_opt(opportunities_list = list(c("A", "B"))))
    Condition
      Warning:
      The `opportunities_list` argument of `set_preprocessing_opt()` is deprecated as of goldfish 1.8.6.
      i Use the `support_constraint` argument of `estimate_dynam()` / `make_specification()` instead.

