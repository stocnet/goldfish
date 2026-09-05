# a missing replay object names both supply routes

    Code
      resolve_preprocessed(fit = fit)
    Condition
      Error:
      ! This diagnostic needs the preprocessed statistics of the model, which this fit does not carry.
      i Re-estimate with `return_preprocessed = TRUE`, or
      i supply `preprocessed = compute_statistics(..., output = "preprocessed")`.

---

    Code
      resolve_preprocessed(fit$names, fit)
    Condition
      Error:
      ! `preprocessed` must be a <goldfishStat> object.
      x You supplied a <data.frame> object.
      i Build one with `compute_statistics(..., output = "preprocessed")`.

