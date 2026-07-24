# set_estimation_opt rejects invalid diagnostics

    Code
      set_estimation_opt(diagnostics = c("loglik", "devianc"))
    Condition
      Error in `set_estimation_opt()`:
      ! Unknown `diagnostics` primitive "devianc".
      i Valid primitives are "loglik", "scores", "ranks", "margins", and "probabilities", or one of `TRUE` / `FALSE` / "all".

---

    Code
      set_estimation_opt(diagnostics = NA)
    Condition
      Error in `set_estimation_opt()`:
      ! `diagnostics` must be a single `TRUE` or `FALSE`, or a character vector of primitive names.

---

    Code
      set_estimation_opt(diagnostics = 1L)
    Condition
      Error in `set_estimation_opt()`:
      ! `diagnostics` must be a character vector, `TRUE`, `FALSE`, or "all".
      x You supplied a <integer> vector.

# legacy return_* flags soft-deprecate onto diagnostics

    Code
      invisible(set_estimation_opt(return_interval_loglik = TRUE))
    Condition
      Warning:
      The `return_interval_loglik` argument of `set_estimation_opt()` is deprecated as of goldfish 1.9.11.
      i Please use the `diagnostics` argument instead.
      i Request the "loglik" primitive via diagnostics = "loglik".

---

    Code
      invisible(set_estimation_opt(return_probabilities = TRUE))
    Condition
      Warning:
      The `return_probabilities` argument of `set_estimation_opt()` is deprecated as of goldfish 1.9.11.
      i Please use the `diagnostics` argument instead.
      i Request the "probabilities" primitive via diagnostics = "probabilities".

---

    Code
      invisible(set_estimation_opt(return_event_scores = TRUE))
    Condition
      Warning:
      The `return_event_scores` argument of `set_estimation_opt()` is deprecated as of goldfish 1.9.11.
      i Please use the `diagnostics` argument instead.
      i Request the "scores" primitive via diagnostics = "scores".

# mixing diagnostics with a legacy flag aborts

    Code
      set_estimation_opt(diagnostics = "loglik", return_event_scores = TRUE)
    Condition
      Error in `set_estimation_opt()`:
      ! Cannot supply `diagnostics` together with the deprecated `return_event_scores` flag.
      i Use `diagnostics` alone; it supersedes the `return_*` flags.

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

