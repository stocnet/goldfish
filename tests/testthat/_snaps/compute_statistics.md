# an unavailable sub_model names the model's allowed set

    Code
      compute_statistics(depNetwork ~ inertia, data = dataTest, model = "REM",
      sub_model = "choice_coordination")
    Condition
      Error in `estimate_wrapper()`:
      ! `sub_model` "choice_coordination" is not available for model "REM".
      i Model "REM" allows "rate" and "rate_ordered".

# REM sub_model = choice points at both successors

    Code
      invisible(compute_statistics(depNetwork ~ inertia(networkState), data = dataTest,
      model = "REM", sub_model = "choice", output = "gather"))
    Condition
      Warning:
      ! `sub_model = "choice"` is deprecated for REM models.
      i Use `sub_model = "rate"` to model the timing of the dyadic events (exact-time), or `sub_model = "rate_ordered"` to model only their order (ordinal).
      i Continuing with `sub_model = "rate"`.
    Message
      i `sub_model = "rate"` models the waiting times between events; a time intercept has been added.
      i Use `sub_model = "rate_ordered"` to model only the order of the events (ordinal likelihood).

# one print method renders every shape by reading the fields

    Code
      print(stat_of("gather"))
    Message
      Gather stack: statistics as expanded rows.
      * 38 rows over 12 events
      * statistic: "inertia_networkState"

