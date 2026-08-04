# an intercept-only model aborts on every sub-model

    Code
      estimate_dynam(depNetwork ~ 1, sub_model = "rate", data = dataTest)
    Condition
      Error in `estimate_wrapper()`:
      ! A model needs at least one effect term.
      x This formula carries none.
      i On "rate" an intercept alone is a well-defined baseline rate, but goldfish does not fit a formula with no effect.
      i Add an effect term.

---

    Code
      estimate_dynam(depNetwork ~ 1, sub_model = "choice", data = dataTest)
    Condition
      Warning:
      Model "DyNAM" sub_model "choice" ignores the time intercept.
      Error in `estimate_wrapper()`:
      ! A model needs at least one effect term.
      x This formula carries none.
      i An intercept would not serve on "choice": a statistic constant across the alternatives cancels in the risk-set normalization, so it identifies nothing.
      i Add an effect that varies across the alternatives.

---

    Code
      estimate_dynam(depNetwork ~ 1, sub_model = "choice_coordination", data = dataTest)
    Condition
      Warning:
      Model "DyNAM" sub_model "choice_coordination" ignores the time intercept.
      Error in `estimate_wrapper()`:
      ! A model needs at least one effect term.
      x This formula carries none.
      i An intercept would not serve on "choice_coordination": a statistic constant across the alternatives cancels in the risk-set normalization, so it identifies nothing.
      i Add an effect that varies across the alternatives.

# a covariance needs an estimated coefficient

    Code
      vcov(fit)
    Condition
      Error in `vcov()`:
      ! A variance-covariance matrix needs at least one estimated coefficient.
      x All 1 coefficient of this fit is held at a fixed value.
      i The log-likelihood and the information criteria are defined here; the covariance is not.

