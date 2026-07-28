# the engine argument and the legacy values are deprecated

    Code
      invisible(set_algorithm_newton(engine = "cpp"))
    Condition
      Warning:
      The `engine` argument of `set_algorithm_newton()` is deprecated as of goldfish 2.0.0.
      i Please use the `backend` argument instead.

---

    Code
      invisible(set_algorithm_newton(engine = "gather_compute"))
    Condition
      Warning:
      The `engine` argument of `set_algorithm_newton()` is deprecated as of goldfish 2.0.0.
      i Please use the `backend` argument instead.
      i The value "gather_compute" is now "gather".

---

    Code
      invisible(set_algorithm_newton(backend = "default"))
    Condition
      Warning:
      The `set_algorithm_newton()` backend value "default" was deprecated in goldfish 2.0.0.
      i Please use "r" instead.

---

    Code
      invisible(set_algorithm_newton(engine = "default", backend = "gather"))
    Condition
      Warning:
      The `engine` argument of `set_algorithm_newton()` is deprecated as of goldfish 2.0.0.
      i Please use the `backend` argument instead.
      ! Both were supplied; the value of `backend` is used.

# an unknown backend aborts naming the vocabulary

    Code
      set_algorithm_newton(backend = "fortran")
    Condition
      Error in `set_algorithm_newton()`:
      ! `backend` must be one of "cpp", "r", and "gather".
      x You supplied "fortran".

# convergence_criterion is deprecated in favor of score_tol

    Code
      invisible(set_algorithm_newton(convergence_criterion = 1e-04))
    Condition
      Warning:
      The `convergence_criterion` argument of `set_algorithm_newton()` is deprecated as of goldfish 1.7.2.
      i `convergence_criterion` is ignored; please use `score_tol` instead (default: 1e-6).

# deprecation messages name the current constructor

    Code
      invisible(set_algorithm_newton(fixed_parameters = c(NA, 2)))
    Condition
      Warning:
      The `fixed_parameters` argument of `set_algorithm_newton()` is deprecated as of goldfish 1.8.4.
      ! Wrap the term in `offset()` in the model formula and supply its value through `offset_coef` instead.
      i `offset()` aligns fixed values to terms by name rather than by counting coefficient positions.
    Code
      invisible(set_preprocessing(opportunities_list = list(c("A", "B"))))
    Condition
      Warning:
      The `opportunities_list` argument of `set_preprocessing()` is deprecated as of goldfish 1.8.6.
      i Use the `support_constraint` argument of `estimate_dynam()` / `make_specification()` instead.

# set_algorithm_newton rejects invalid diagnostics

    Code
      set_algorithm_newton(diagnostics = c("loglik", "devianc"))
    Condition
      Error in `set_algorithm_newton()`:
      ! Unknown `diagnostics` primitive "devianc".
      i Valid primitives are "loglik", "scores", "ranks", "margins", and "probabilities", or one of `TRUE` / `FALSE` / "all".

---

    Code
      set_algorithm_newton(diagnostics = NA)
    Condition
      Error in `set_algorithm_newton()`:
      ! `diagnostics` must be a single `TRUE` or `FALSE`, or a character vector of primitive names.

---

    Code
      set_algorithm_newton(diagnostics = 1L)
    Condition
      Error in `set_algorithm_newton()`:
      ! `diagnostics` must be a character vector, `TRUE`, `FALSE`, or "all".
      x You supplied a <integer> vector.

# legacy return_* flags soft-deprecate onto diagnostics

    Code
      invisible(set_algorithm_newton(return_interval_loglik = TRUE))
    Condition
      Warning:
      The `return_interval_loglik` argument of `set_algorithm_newton()` is deprecated as of goldfish 1.9.11.
      i Please use the `diagnostics` argument instead.
      i Request the "loglik" primitive via diagnostics = "loglik".

---

    Code
      invisible(set_algorithm_newton(return_probabilities = TRUE))
    Condition
      Warning:
      The `return_probabilities` argument of `set_algorithm_newton()` is deprecated as of goldfish 1.9.11.
      i Please use the `diagnostics` argument instead.
      i Request the "probabilities" primitive via diagnostics = "probabilities".

# the never-public score flag is gone

    Code
      set_algorithm_newton(return_event_scores = TRUE)
    Condition
      Error in `set_algorithm_newton()`:
      ! unused argument (return_event_scores = TRUE)

# mixing diagnostics with a legacy flag aborts

    Code
      set_algorithm_newton(diagnostics = "loglik", return_probabilities = TRUE)
    Condition
      Error in `set_algorithm_newton()`:
      ! Cannot supply `diagnostics` together with the deprecated `return_probabilities` flag.
      i Use `diagnostics` alone; it supersedes the `return_*` flags.

# opportunities_list is deprecated in favour of support_constraint

    Code
      invisible(set_preprocessing(opportunities_list = list(c("A", "B"))))
    Condition
      Warning:
      The `opportunities_list` argument of `set_preprocessing()` is deprecated as of goldfish 1.8.6.
      i Use the `support_constraint` argument of `estimate_dynam()` / `make_specification()` instead.

# an unnamed impute vector aborts

    Code
      set_preprocessing(impute = "as_category")
    Condition
      Error in `set_preprocessing()`:
      ! `impute` must be `NULL` or a named character vector.
      i Key each entry by the attribute name, e.g. `impute = c(party = "as_category")`.

# an unknown impute policy value aborts, listing supported values

    Code
      set_preprocessing(impute = c(x = "bogus"))
    Condition
      Error in `set_preprocessing()`:
      ! Unknown imputation policy "bogus".
      i Supported values are "summary" and "as_category".

# the reserved locf policy aborts as unimplemented

    Code
      set_preprocessing(impute = c(x = "locf"))
    Condition
      Error in `set_preprocessing()`:
      ! Imputation policy "locf" is reserved but not yet implemented.
      i Supported values are "summary" and "as_category".

# an unsupported primitive aborts naming the backends that produce it

    Code
      check_diagnostic_support(c("loglik", "scores"), "gather", support = support)
    Condition
      Error:
      ! The "scores" diagnostic is not available with `backend = "gather"`.
      i Use `backend = "cpp"` or `backend = "r"` to store it.

