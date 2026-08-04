# the conditional-scores identity is announced, not left silent

    Code
      invisible(suppressWarnings(estimate_dynam(depNetwork ~ inertia, sub_model = "choice",
      data = dataTest, control_algo = conditional_control())))
    Message
      i "choice" has an already-conditional likelihood, so "conditional_scores" stores nothing: its score rows carry no exposure term and are the conditional rows.
      i Read them from `residuals(type = "score")`, and drop "conditional_scores" from `set_algorithm_newton()`'s `diagnostics`.

# evaluate_model demands a value and so aborts

    Code
      evaluate_model(fit, return = "conditional_scores", preprocessed = prep)
    Condition
      Error in `evaluate_model()`:
      ! "conditional_scores" is not a distinct quantity for this sub-model.
      x "choice" has an already-conditional likelihood, so its score rows carry no exposure term and are the conditional rows.
      i Use `return = "score"`, which returns exactly those rows.

# a flavored container is the wrong object to ask for a risk-set axis

    Code
      risk_set_axis(container)
    Condition
      Error in `risk_set_axis()`:
      ! A flavored fit carries no single risk-set axis.
      x It holds one fit per process, and each has its own.
      i Ask a process: `x$results[[1]]`, or index by the flavor label the fit reports.

