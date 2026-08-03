# maxLik optimizers reject backends other than cpp

    Code
      fit_call("gather")
    Condition
      Error in `estimate_wrapper()`:
      ! `optimizer` "bfgs" requires `backend = "cpp"`.
      x It is not available with `backend = "gather"`.
      i maxLik-backed optimizers run only on the "cpp" backend.

---

    Code
      fit_call("r")
    Condition
      Error in `estimate_wrapper()`:
      ! `optimizer` "bfgs" requires `backend = "cpp"`.
      x It is not available with `backend = "r"`.
      i maxLik-backed optimizers run only on the "cpp" backend.

# a maxLik optimizer aborts when maxLik is not installed

    Code
      estimate_dynam(spec$formula, data = data_list$social_evolution, sub_model = spec$
        sub_model, control_algo = set_algorithm_newton(optimizer = "bfgs"), progress = FALSE)
    Condition
      Error in `estimate_wrapper()`:
      ! `optimizer` "bfgs" requires the maxLik package.
      i Install it with `install.packages("maxLik")`.

