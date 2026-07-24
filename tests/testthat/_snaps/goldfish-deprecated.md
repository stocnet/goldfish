# the renamed constructors soft-deprecate onto their new names

    Code
      invisible(set_estimation_opt())
    Condition
      Warning:
      `set_estimation_opt()` was deprecated in goldfish 2.0.0.
      i Please use `set_algorithm_newton()` instead.
    Code
      invisible(set_preprocessing_opt())
    Condition
      Warning:
      `set_preprocessing_opt()` was deprecated in goldfish 2.0.0.
      i Please use `set_preprocessing()` instead.

# the 1.7.0 camelCase shims skip the middle name

    Code
      invisible(examineOutliers(fit, method = "Top", parameter = 2))
    Condition
      Warning:
      `examineOutliers()` was deprecated in goldfish 1.7.0.
      i Please use `diagnose_outliers()` instead.
    Code
      invisible(examineChangepoints(fit, moment = "mean", method = "PELT"))
    Condition
      Warning:
      `examineChangepoints()` was deprecated in goldfish 1.7.0.
      i Please use `diagnose_changepoints()` instead.

# renamed estimator arguments soft-deprecate onto their new names

    Code
      invisible(estimate_dynam(depNetwork ~ inertia, sub_model = "choice", data = dataTest,
      control_estimation = set_algorithm_newton(max_iterations = 1),
      control_preprocessing = set_preprocessing(), preprocessing_init = NULL))
    Condition
      Warning:
      The `control_estimation` argument of `estimate_dynam()` is deprecated as of goldfish 2.0.0.
      i Please use the `control_algo` argument instead.
      Warning:
      The `control_preprocessing` argument of `estimate_dynam()` is deprecated as of goldfish 2.0.0.
      i Please use the `control_prep` argument instead.
      Warning:
      The `preprocessing_init` argument of `estimate_dynam()` is deprecated as of goldfish 2.0.0.
      i Please use the `preprocessed` argument instead.

# a stale preprocessed object is rejected through preprocessed =

    Code
      estimate_dynam(depNetwork ~ inertia, sub_model = "choice", data = dataTest,
      preprocessed = stale)
    Condition
      Error in `estimate_wrapper()`:
      ! The `preprocessed` object uses an outdated preprocessing format.
      x Objects preprocessed with a previous goldfish version cannot be reused for estimation.
      i Recompute the preprocessing object with `compute_statistics()`.

# preprocessing_only soft-deprecates onto compute_statistics()

    Code
      invisible(estimate_rem(depNetwork ~ inertia(networkState), sub_model = "rate_ordered",
      data = dataTest, preprocessing_only = TRUE))
    Condition
      Warning:
      The `preprocessing_only` argument of `estimate_rem()` is deprecated as of goldfish 2.0.0.
      i Please use compute_statistics(output = "preprocessed") instead.

