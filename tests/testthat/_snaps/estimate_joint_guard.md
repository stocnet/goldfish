# estimate_dynam rejects a joint specification, pointing to dynes

    Code
      estimate_dynam(js)
    Condition
      Error in `estimate_dynam()`:
      ! A <joint_specification.goldfish> cannot be estimated with the event-stream estimators.
      i Multivariate specifications are estimated with `estimate_dynes()`.

# estimate_rem rejects a joint specification, pointing to dynes

    Code
      estimate_rem(js)
    Condition
      Error in `estimate_rem()`:
      ! A <joint_specification.goldfish> cannot be estimated with the event-stream estimators.
      i Multivariate specifications are estimated with `estimate_dynes()`.

# estimate_dynam rejects a panel-focal single specification

    Code
      estimate_dynam(make_specification(rate = ~1, layer = "calls", model = "DyNAM",
        data = x))
    Condition
      Error in `make_specification()`:
      ! A "panel" layer cannot be the focal (dependent) process.
      x Layer "calls" is declared observation = "panel".
      i goldfish models event-stream dependents; panel-dependent processes are SAOM/RSiena territory.

# estimate_rem rejects a panel-focal single specification

    Code
      estimate_rem(make_specification(rate = ~1, layer = "calls", model = "REM",
        data = x))
    Condition
      Error in `make_specification()`:
      ! A "panel" layer cannot be the focal (dependent) process.
      x Layer "calls" is declared observation = "panel".
      i goldfish models event-stream dependents; panel-dependent processes are SAOM/RSiena territory.

# the panel-focal guard fires on the shared preprocessing path

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! A "panel" layer cannot be the focal (dependent) process.
      x Layer "calls" is declared observation = "panel".
      i goldfish models event-stream dependents; panel-dependent processes are SAOM/RSiena territory.

