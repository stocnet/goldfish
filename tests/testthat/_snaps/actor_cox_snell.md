# the level vocabulary is checked per type

    Code
      residuals(fit, type = "cox_snell", level = "dyad")
    Condition
      Error in `residuals()`:
      ! `type = "cox_snell"` has no "dyad" level.
      ℹ Available: "event" and "actor".

---

    Code
      residuals(fit, type = "deviance", level = "actor")
    Condition
      Error in `residuals()`:
      ! `level` does not apply to `type = "deviance"`.
      ✖ That type has one reading, and it is per event.
      ℹ Only "cox_snell" and "martingale" stratify.

