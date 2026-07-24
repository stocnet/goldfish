# maxLik optimizers reject engines other than default_c

    Code
      fit_call("gather_compute")
    Condition
      Error in `estimate_wrapper()`:
      ! `optimizer` "bfgs" requires `engine = "default_c"`.
      x It is not available with `engine = "gather_compute"`.
      i maxLik-backed optimizers run only on the default_c evaluator.

---

    Code
      fit_call("default")
    Condition
      Error in `estimate_wrapper()`:
      ! `optimizer` "bfgs" requires `engine = "default_c"`.
      x It is not available with `engine = "default"`.
      i maxLik-backed optimizers run only on the default_c evaluator.

# a maxLik optimizer aborts when maxLik is not installed

    Code
      estimate_dynam(spec$formula, data = data_list$social_evolution, sub_model = spec$
        sub_model, control_algo = set_algorithm_newton(optimizer = "bfgs"), progress = FALSE)
    Condition
      Error in `estimate_wrapper()`:
      ! `optimizer` "bfgs" requires the maxLik package.
      i Install it with `install.packages("maxLik")`.

