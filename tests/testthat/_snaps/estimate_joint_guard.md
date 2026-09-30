# estimate_dynam rejects a joint specification, pointing to dynes

    Code
      estimate_dynam(js)
    Condition
      Error in `estimate_dynam()`:
      ! A <goldfishJointSpec> cannot be estimated with the event-stream estimators.
      i Multivariate specifications are estimated with `estimate_dynes()`.

# estimate_rem rejects a joint specification, pointing to dynes

    Code
      estimate_rem(js)
    Condition
      Error in `estimate_rem()`:
      ! A <goldfishJointSpec> cannot be estimated with the event-stream estimators.
      i Multivariate specifications are estimated with `estimate_dynes()`.

# estimate_dynam rejects a panel-focal single specification

    Code
      estimate_dynam(spec, sub_model = "rate")
    Condition
      Error in `estimate_dynam()`:
      ! A "panel" layer cannot be the dependent process of an event-stream estimator.
      x Layer "calls" is declared observation = "panel": its event times between waves are not observed.
      i Estimate a panel-observed dependent process with `estimate_dynes()`.

# estimate_rem rejects a panel-focal single specification

    Code
      estimate_rem(spec)
    Condition
      Error in `estimate_rem()`:
      ! A "panel" layer cannot be the dependent process of an event-stream estimator.
      x Layer "calls" is declared observation = "panel": its event times between waves are not observed.
      i Estimate a panel-observed dependent process with `estimate_dynes()`.

# event-stream estimators reject a panel focal on the formula path

    Code
      estimate_dynam(calls ~ inertia, sub_model = "rate", data = x)
    Condition
      Error in `estimate_dynam()`:
      ! A "panel" layer cannot be the dependent process of an event-stream estimator.
      x Layer "calls" is declared observation = "panel": its event times between waves are not observed.
      i Estimate a panel-observed dependent process with `estimate_dynes()`.

