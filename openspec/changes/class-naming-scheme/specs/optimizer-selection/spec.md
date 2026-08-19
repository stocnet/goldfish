## MODIFIED Requirements

### Requirement: Optimizer selection via set_algorithm_newton()
`set_algorithm_newton()` (the renamed `set_estimation_opt()`) SHALL accept an
`optimizer` argument as a flat algorithm list — `"newton_raphson"` (default),
`"bfgs"`, `"bhhh"`, `"nelder_mead"` — validated with `match.arg()`.
`"newton_raphson"` SHALL preserve the existing damped Newton-Raphson
estimation path unchanged (interface and, up to floating-point summation
order, results). The returned object SHALL carry the selected optimizer and
SHALL have class `c("algorithm_newton_goldfish", "algorithm_goldfish",
"list")`; the `algorithm_goldfish` superclass is the shared dispatch and
validation hook for all algorithm objects (the DyNES EM constructor joins it
later).

#### Scenario: default is the existing Newton-Raphson
- **WHEN** `set_algorithm_newton()` is called without `optimizer`
- **THEN** estimation runs the existing damped Newton-Raphson loop and
  produces the same results and result object as before this change.

#### Scenario: invalid optimizer value
- **WHEN** `set_algorithm_newton(optimizer = "gradient_descent")` is called
- **THEN** `match.arg()` rejects it, naming the valid choices.

#### Scenario: class hierarchy
- **WHEN** `set_algorithm_newton()` returns
- **THEN** the object inherits both `algorithm_newton_goldfish` and
  `algorithm_goldfish`, and the `print` method renders under the new class.

### Requirement: Estimators accept the algorithm object via control_algo
`estimate_dynam()`, `estimate_dynami()`, and `estimate_rem()` SHALL accept
the algorithm object through a `control_algo` argument (default
`set_algorithm_newton()`), validated with a single
`inherits(x, "algorithm_goldfish")` check so future algorithm objects pass
the same gate. The internal estimation plumbing SHALL carry the object under
the same name end to end.

#### Scenario: algorithm object forwarded
- **WHEN** `estimate_dynam(spec, data = d, control_algo =
  set_algorithm_newton(max_iterations = 5))` is called
- **THEN** estimation honors the option values exactly as
  `control_estimation` did before the rename.

#### Scenario: wrong object rejected
- **WHEN** `control_algo` receives an object that does not inherit
  `algorithm_goldfish`
- **THEN** estimation aborts with a cli error naming the expected
  constructor.
