## ADDED Requirements

### Requirement: Warm-start initial parameters for augmentation-based estimation

`set_algorithm_newton()` SHALL offer a warm-start option for the initial
parameter vector used by augmentation-based estimation: draw one random
endpoint-consistent augmentation, estimate the model on it with the standard
estimator, and use those estimates as the starting vector. The default initial
parameter behavior (zero vector when `initial_parameters` is not supplied)
SHALL remain unchanged, and the option SHALL have no effect on the existing
event-stream estimators.

#### Scenario: warm start seeds the EM loop
- **WHEN** the warm-start option is enabled for an `estimate_dynes()` run
- **THEN** the EM loop's initial parameters are the estimates from one random
  augmentation rather than the zero vector, and the trace records them as
  iteration zero.

#### Scenario: event-stream estimators unaffected
- **WHEN** the warm-start option is set on an `estimate_dynam()` call
- **THEN** behavior is unchanged from `initial_parameters` semantics today
  (the option is inert outside augmentation-based estimation).
