# evaluate_model rejects what it cannot evaluate

    Code
      evaluate_model(fit, return = "residuals")
    Condition
      Error in `evaluate_model()`:
      ! Unknown `return` quantity "residuals".
      i Valid quantities are "loglik", "score", "information", "interval_loglik", "total_rate", "conditional_logl", "event_scores", "ranks", "recall", "margins", and "probabilities".

---

    Code
      evaluate_model(fit, at = c(1, 2))
    Condition
      Error in `evaluate_model()`:
      ! `at` must have one value per coefficient.
      x It has 2; the model has 3.

---

    Code
      evaluate_model(fit, at = c(nonesuch = 1))
    Condition
      Error in `evaluate_model()`:
      ! `at` names coefficient the model does not have: "nonesuch".
      i Its coefficients are "inrt", "rec", and "trans".

---

    Code
      evaluate_model(1:3)
    Condition
      Error in `evaluate_model()`:
      ! `evaluate_model()` needs a fitted goldfish model.
      x `x` is an integer vector.
      i Fit one with `estimate_dynam()` or `estimate_rem()`.

# evaluation needs statistics from the fit or from the caller

    Code
      evaluate_model(fit, return = "loglik")
    Condition
      Error in `evaluate_model()`:
      ! This diagnostic needs the preprocessed statistics of the model, which this fit does not carry.
      i Re-estimate with `return_preprocessed = TRUE`, or
      i supply `preprocessed = compute_statistics(..., output = "preprocessed")`.

