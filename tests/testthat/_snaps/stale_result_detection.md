# a preprocessed object is refused on kind, not accepted by default

    Code
      abort_if_stale_result(prep, "a summary")
    Condition
      Error:
      ! Cannot compute a summary from a <goldfishStat> object.
      x A <goldfishFit> object is required.
      i Pass the fitted model itself, not one of its components.

# the messages name the cause and the fix

    Code
      summary(old)
    Condition
      Error in `summary()`:
      ! Cannot compute a summary from a <goldfishFit> object that was not fitted by this version of goldfish.
      x It was fitted before goldfish 2.0.0, when the components of a fitted model were renamed to snake_case.
      i Re-fit the model to use it with this version. The old names are not translated: an object this old is also missing components the current methods need, so its spelling is not the only thing that would have to be repaired.
      i See `news(package = "goldfish")` for the renamed components.

---

    Code
      logLik(old)
    Condition
      Error in `logLik()`:
      ! Cannot compute a log-likelihood from a <goldfishFit> object that was not fitted by this version of goldfish.
      x It was fitted before goldfish 2.0.0, when the components of a fitted model were renamed to snake_case.
      i Re-fit the model to use it with this version. The old names are not translated: an object this old is also missing components the current methods need, so its spelling is not the only thing that would have to be repaired.
      i See `news(package = "goldfish")` for the renamed components.

---

    Code
      vcov(newer)
    Condition
      Error in `vcov()`:
      ! Cannot compute a variance-covariance matrix from a <goldfishFit> object that was not fitted by this version of goldfish.
      x It was fitted by a newer version of goldfish than the one loaded, so this version does not know its layout.
      i Update goldfish, or re-fit the model with this version.

# the retired-class messages name the cause and the fix

    Code
      print(released)
    Condition
      Error in `print()`:
      ! This object carries the retired class <result.goldfish>.
      x It was fitted before goldfish 2.0.0, when the components of a fitted model were renamed to snake_case.
      i Re-fit the model to use it with this version. The old names are not translated: an object this old is also missing components the current methods need, so its spelling is not the only thing that would have to be repaired.
      i See `news(package = "goldfish")` for the renamed components.

---

    Code
      summary(dev_line)
    Condition
      Error in `summary()`:
      ! This object carries the retired class <result.goldfish>.
      x Its class <result.goldfish> was retired in goldfish 2.0.0: a fitted model is now <goldfishFit>.
      i Its components are current. Only the class name changed, so nothing inside the object needs repairing.
      i goldfish attaches no fallback class, so no method dispatches on the retired name. Re-fit the model. See `news(package = "goldfish")` for the class table.

