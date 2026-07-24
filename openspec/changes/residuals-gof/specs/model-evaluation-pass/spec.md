# model-evaluation-pass

A single no-iteration evaluation of a fitted model's engine at an arbitrary
parameter vector, returning requested quantities.

## ADDED Requirements

### Requirement: evaluate_model single-pass evaluator
goldfish SHALL provide `evaluate_model(x, at = coef(x), return, preprocessed
= NULL, ...)` performing exactly one evaluation pass (no Newton-Raphson
iterations) of the model's likelihood machinery at the parameter vector
`at`. The `return` argument SHALL accept any subset of `c("loglik",
"score", "information", "interval_loglik", "event_scores", "ranks",
"recall", "margins", "probabilities")`, and the returned list SHALL contain
exactly the requested components. Dispatch SHALL follow the fitted model's
model/submodel routing. Statistics SHALL come from the attached or supplied
`preprocessed.goldfish` per the diagnostic-primitives precedence rules.

#### Scenario: evaluation at the MLE reproduces the fit
- **WHEN** `evaluate_model(fit, at = coef(fit), return = c("loglik",
  "score"))` runs on a converged fixture fit
- **THEN** the log-likelihood equals `logLik(fit)` within 1e-10 and the
  score of the free (non-offset) parameters is near zero (max absolute
  component below the convergence tolerance; offset columns carry the
  fixed value's nonzero score and are reported, not tested).

#### Scenario: evaluation at a constrained vector
- **WHEN** `evaluate_model` is called at a parameter vector with one
  effect fixed to zero on a model whose statistics include that effect
- **THEN** it returns the score and information of the full model evaluated
  at that vector, with dimensions matching the full effect set.

### Requirement: evaluator uses the estimation engine
`evaluate_model()` SHALL default to the engine used for the original
estimation and SHALL record which engine produced its output. Requesting an
engine that does not support a requested quantity SHALL abort with a cli
error naming the supported engines for that quantity.

#### Scenario: engine defaults to the fit's engine
- **WHEN** a fit estimated with `engine = "default_c"` is evaluated without
  an explicit engine
- **THEN** the evaluation runs on `default_c` and per-event quantities agree
  with those stored on the fit within 1e-10.

### Requirement: derived quantities computed in-pass
For `"ranks"`, `"recall"`, and `"margins"`, the evaluator SHALL compute the
quantities inside the engine's event loop and return only the summary
vectors (integer ranks per event; recall proportions at requested
thresholds; named per-actor margins). The full per-event probability matrix
SHALL NOT be materialized unless `"probabilities"` is explicitly requested.

#### Scenario: ranks without probability matrix
- **WHEN** `evaluate_model(fit, return = "ranks")` runs on a REM fixture
- **THEN** an integer vector of observed ranks (one per event) is returned
  and no probability matrix is allocated in the returned object.

#### Scenario: rank correctness on a small fixture
- **WHEN** ranks are computed on a fixture small enough to enumerate
  probabilities in R
- **THEN** each observed rank equals the rank of the observed event's
  probability within its realized risk set.
