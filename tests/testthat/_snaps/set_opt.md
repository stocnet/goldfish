# opportunities_list is deprecated in favour of support_constraint

    Code
      invisible(set_preprocessing_opt(opportunities_list = list(c("A", "B"))))
    Condition
      Warning:
      The `opportunities_list` argument of `set_preprocessing_opt()` is deprecated as of goldfish 1.8.6.
      i Use the `support_constraint` argument of `estimate_dynam()` / `make_specification()` instead.

# an unnamed impute vector aborts

    Code
      set_preprocessing_opt(impute = "as_category")
    Condition
      Error in `set_preprocessing_opt()`:
      ! `impute` must be `NULL` or a named character vector.
      i Key each entry by the attribute name, e.g. `impute = c(party = "as_category")`.

# an unknown impute policy value aborts, listing supported values

    Code
      set_preprocessing_opt(impute = c(x = "bogus"))
    Condition
      Error in `set_preprocessing_opt()`:
      ! Unknown imputation policy "bogus".
      i Supported values are "summary" and "as_category".

# the reserved locf policy aborts as unimplemented

    Code
      set_preprocessing_opt(impute = c(x = "locf"))
    Condition
      Error in `set_preprocessing_opt()`:
      ! Imputation policy "locf" is reserved but not yet implemented.
      i Supported values are "summary" and "as_category".

