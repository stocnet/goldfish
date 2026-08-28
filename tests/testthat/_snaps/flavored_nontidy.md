# an unknown flavor aborts naming the ones the fit carries

    Code
      residuals(container, flavor = "signing")
    Condition
      Error in `residuals()`:
      ! `flavor` names 1 flavor this fit does not carry.
      ✖ Unknown: "signing".
      ℹ This fit carries "creation" and "dissolution".

---

    Code
      fitted(container, flavor = c("creation", "x"))
    Condition
      Error in `fitted()`:
      ! `flavor` names 1 flavor this fit does not carry.
      ✖ Unknown: "x".
      ℹ This fit carries "creation" and "dissolution".

# a process that refuses is named

    Code
      predict(container)
    Condition
      Error in `predict()`:
      ! `predict()` could not read process "calls › creation › rate".
      Caused by error in `stats::predict()`:
      ! This diagnostic needs the preprocessed statistics of the model, which this fit does not carry.
      ℹ Re-estimate with `return_preprocessed = TRUE`, or
      ℹ supply `preprocessed = compute_statistics(..., output = "preprocessed")`.

