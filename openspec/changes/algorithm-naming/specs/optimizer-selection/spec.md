# optimizer-selection (delta)

## RENAMED Requirements

- FROM: `### Requirement: Optimizer selection via set_estimation_opt()`
- TO: `### Requirement: Optimizer selection via set_algorithm_newton()`

## MODIFIED Requirements

### Requirement: Optimizer selection via set_algorithm_newton()
`set_algorithm_newton()` (the renamed `set_estimation_opt()`) SHALL accept an
`optimizer` argument as a flat algorithm list — `"newton_raphson"` (default),
`"bfgs"`, `"bhhh"`, `"nelder_mead"` — validated with `match.arg()`.
`"newton_raphson"` SHALL preserve the existing damped Newton-Raphson
estimation path unchanged (interface and, up to floating-point summation
order, results). The returned object SHALL carry the selected optimizer and
SHALL have class `c("algorithm_newton.goldfish", "algorithm.goldfish",
"list")`; the `algorithm.goldfish` superclass is the shared dispatch and
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
- **THEN** the object inherits both `algorithm_newton.goldfish` and
  `algorithm.goldfish`, and the `print` method renders under the new class.

### Requirement: maxLik optimizers run only on the default_c evaluator
Estimation SHALL abort with an informative error when a maxLik-backed
optimizer is combined with the `gather_compute` or `default` engine, stating
that maxLik optimizers require the `default_c` engine. Because the default
engine value is already `default_c`, selecting a maxLik optimizer without an
explicit engine SHALL work without further user action.

#### Scenario: incompatible engine
- **WHEN** `set_algorithm_newton(optimizer = "bfgs", engine = "gather_compute")`
  reaches estimation
- **THEN** it aborts informatively, naming the required engine.

### Requirement: User-facing return_event_scores option
`set_algorithm_newton()` SHALL accept a logical `return_event_scores` argument
(default `FALSE`), parallel to the existing `return_interval_loglik` and
`return_probabilities` flags. When `TRUE`, the fitted result object SHALL
contain the per-event score matrix as `event_scores` (n_events × p, columns
named by effect), evaluated at the returned parameter estimates. The option
SHALL be honored by the `default_c` engine (via the evaluator flag) and the
`default` R engine (captured in its contribution loop); requesting it with
the `gather_compute` engine SHALL abort with an informative error. Both
supported engines SHALL agree on the returned matrix within the cross-engine
tolerance. The documentation SHALL describe the intended uses (robust
sandwich and clustered standard errors, per-effect score-process diagnostics,
event-influence measures) without this change implementing those diagnostics.

#### Scenario: scores returned on request
- **WHEN** a model is estimated with `return_event_scores = TRUE` on the
  `default_c` or `default` engine
- **THEN** the result contains `event_scores` with one row per dependent event
  and one column per effect, and its column sums agree with the (near-zero)
  aggregate score at convergence.

#### Scenario: engines agree on the score matrix
- **WHEN** the same fixture model is estimated on `default` and `default_c`
  with `return_event_scores = TRUE`
- **THEN** the two `event_scores` matrices agree within 1e-10.

#### Scenario: gather_compute rejects the option
- **WHEN** `return_event_scores = TRUE` is combined with
  `engine = "gather_compute"`
- **THEN** estimation aborts with an informative error naming the supported
  engines.

#### Scenario: off by default
- **WHEN** a model is estimated without setting `return_event_scores`
- **THEN** the result contains no `event_scores` component and no per-event
  score matrix is accumulated.

## ADDED Requirements

### Requirement: Estimators accept the algorithm object via control_algo
`estimate_dynam()`, `estimate_dynami()`, and `estimate_rem()` SHALL accept
the algorithm object through a `control_algo` argument (default
`set_algorithm_newton()`), validated with a single
`inherits(x, "algorithm.goldfish")` check so future algorithm objects pass
the same gate. The internal estimation plumbing SHALL carry the object under
the same name end to end.

#### Scenario: algorithm object forwarded
- **WHEN** `estimate_dynam(spec, data = d, control_algo =
  set_algorithm_newton(max_iterations = 5))` is called
- **THEN** estimation honors the option values exactly as
  `control_estimation` did before the rename.

#### Scenario: wrong object rejected
- **WHEN** `control_algo` receives an object that does not inherit
  `algorithm.goldfish`
- **THEN** estimation aborts with a cli error naming the expected
  constructor.
