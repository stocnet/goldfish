# optimizer-selection Specification

## Purpose
TBD - created by archiving change refactor-likelihood-compute. Update Purpose after archive.
## Requirements
### Requirement: Optimizer selection via set_estimation_opt()
`set_estimation_opt()` SHALL accept an `optimizer` argument as a flat
algorithm list — `"newton_raphson"` (default), `"bfgs"`, `"bhhh"`,
`"nelder_mead"` — validated with `match.arg()`. `"newton_raphson"` SHALL
preserve the existing damped Newton-Raphson estimation path unchanged
(interface and, up to floating-point summation order, results). The returned
`estimation_opt.goldfish` object SHALL carry the selected optimizer.

#### Scenario: default is the existing Newton-Raphson
- **WHEN** `set_estimation_opt()` is called without `optimizer`
- **THEN** estimation runs the existing damped Newton-Raphson loop and
  produces the same results and result object as before this change.

#### Scenario: invalid optimizer value
- **WHEN** `set_estimation_opt(optimizer = "gradient_descent")` is called
- **THEN** `match.arg()` rejects it, naming the valid choices.

### Requirement: maxLik-backed optimizers behind a Suggests dependency
Optimizer values other than `"newton_raphson"` SHALL be driven by
`maxLik::maxLik()` with maxLik declared in `Suggests` (never Imports). When
maxLik is not installed, selecting such a value SHALL abort with a cli error
that names the missing package and how to install it. The adapter SHALL feed
maxLik through closures over the `default_c` evaluator with the preprocessed
data fixed, evaluating the C++ function at most once per parameter vector
(memoized across the logLik/gradient/Hessian closures).

#### Scenario: maxLik missing
- **WHEN** `optimizer = "bfgs"` is used and maxLik is not installed
- **THEN** estimation aborts before any computation with an informative
  message naming maxLik and the install command.

#### Scenario: one evaluation per parameter vector
- **WHEN** maxLik requests the log-likelihood, gradient, and Hessian at the
  same parameter vector
- **THEN** the C++ evaluator runs once and all three closures read from that
  single evaluation.

### Requirement: maxLik optimizers run only on the default_c evaluator
Estimation SHALL abort with an informative error when a maxLik-backed
optimizer is combined with the `gather_compute` or `default` engine, stating
that maxLik optimizers require the `default_c` engine. Because the default
engine value is already `default_c`, selecting a maxLik optimizer without an
explicit engine SHALL work without further user action.

#### Scenario: incompatible engine
- **WHEN** `set_estimation_opt(optimizer = "bfgs", engine = "gather_compute")`
  reaches estimation
- **THEN** it aborts informatively, naming the required engine.

### Requirement: Result-object parity across optimizers
A model estimated with a maxLik-backed optimizer SHALL return the same class
of result object as the Newton-Raphson path — coefficients, vcov derived from
the Fisher information at the optimum, log-likelihood, iteration count, and
convergence details populated — so `summary()`, `vcov()`, `logLik()`, and the
existing post-estimation methods work identically. Offset and fixed-parameter
handling SHALL be resolved before the optimizer, identically on all paths.
On well-conditioned fixtures, maxLik BFGS/BHHH coefficient estimates SHALL
agree with Newton-Raphson within the cross-engine test tolerance.

#### Scenario: summary works after maxLik estimation
- **WHEN** a baseline-fixture model is estimated with `optimizer = "bfgs"`
- **THEN** `summary()` renders with coefficients, standard errors from the
  Fisher-based vcov, and convergence information, and the coefficients agree
  with the Newton-Raphson estimates within the cross-engine tolerance.

### Requirement: Per-event score matrix as an opt-in evaluator output
The `default_c` evaluators SHALL support an opt-in flag returning the
per-event score matrix (n_events × p) alongside the aggregated derivative,
mirroring the existing per-event `intervalLogL`. The flag SHALL default to
off with no memory overhead when unset. The BHHH optimizer SHALL consume this
matrix as its observation-level gradient, enabling the internal flag
regardless of user-facing settings; the per-event scores SHALL sum to
the aggregate derivative within floating-point tolerance.

#### Scenario: per-event scores consistent with aggregate
- **WHEN** the evaluator runs with the per-event score flag on
- **THEN** the column sums of the score matrix equal the aggregate derivative
  within 1e-10.

#### Scenario: BHHH uses observation-level gradients
- **WHEN** `optimizer = "bhhh"` runs
- **THEN** maxLik receives the per-event score matrix and converges on
  well-conditioned fixtures to coefficients agreeing with Newton-Raphson
  within the cross-engine tolerance.

### Requirement: User-facing return_event_scores option
`set_estimation_opt()` SHALL accept a logical `return_event_scores` argument
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

