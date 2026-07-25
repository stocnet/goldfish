# optimizer-selection Specification

## Purpose
TBD - created by archiving change refactor-likelihood-compute. Update Purpose after archive.
## Requirements
### Requirement: maxLik-backed optimizers behind a Suggests dependency
Optimizer values other than `"newton_raphson"` SHALL be driven by
`maxLik::maxLik()` with maxLik declared in `Suggests` (never Imports). When
maxLik is not installed, selecting such a value SHALL abort with a cli error
that names the missing package and how to install it. The adapter SHALL feed
maxLik through closures over the `cpp` backend's evaluator with the
preprocessed data fixed, evaluating the C++ function at most once per parameter
vector (memoized across the logLik/gradient/Hessian closures).

#### Scenario: maxLik missing
- **WHEN** `optimizer = "bfgs"` is used and maxLik is not installed
- **THEN** estimation aborts before any computation with an informative
  message naming maxLik and the install command.

#### Scenario: one evaluation per parameter vector
- **WHEN** maxLik requests the log-likelihood, gradient, and Hessian at the
  same parameter vector
- **THEN** the C++ evaluator runs once and all three closures read from that
  single evaluation.

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
`set_algorithm_newton()` SHALL accept a logical `return_event_scores` argument
(default `FALSE`), parallel to the existing `return_interval_loglik` and
`return_probabilities` flags. When `TRUE`, the fitted result object SHALL
contain the per-event score matrix as `event_scores` (n_events × p, columns
named by effect), evaluated at the returned parameter estimates. The option
SHALL be honored by **both compute backends** — `backend = "cpp"` (via the
evaluator flag) and `backend = "r"` (captured in its contribution loop) —
while requesting it with `backend = "gather"` SHALL abort with an informative
error. The two supporting backends SHALL agree on the returned matrix within
the cross-backend tolerance. The documentation SHALL describe the intended uses
(robust sandwich and clustered standard errors, per-effect score-process
diagnostics, event-influence measures) without this change implementing those
diagnostics.

#### Scenario: scores returned on request
- **WHEN** a model is estimated with `return_event_scores = TRUE` on the
  `cpp` or `r` backend
- **THEN** the result contains `event_scores` with one row per dependent event
  and one column per effect, and its column sums agree with the (near-zero)
  aggregate score at convergence.

#### Scenario: backends agree on the score matrix
- **WHEN** the same fixture model is estimated on `backend = "r"` and
  `backend = "cpp"` with `return_event_scores = TRUE`
- **THEN** the two `event_scores` matrices agree within 1e-10.

#### Scenario: the gather backend rejects the option
- **WHEN** `return_event_scores = TRUE` is combined with
  `backend = "gather"`
- **THEN** estimation aborts with an informative error naming the supported
  backends.

#### Scenario: off by default
- **WHEN** a model is estimated without setting `return_event_scores`
- **THEN** the result contains no `event_scores` component and no per-event
  score matrix is accumulated.

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

### Requirement: maxLik optimizers run only on the cpp backend
Estimation SHALL abort with an informative error when a maxLik-backed
optimizer is combined with the `"gather"` or `"r"` backend, stating that
maxLik optimizers require the `"cpp"` backend. Because the default
backend value is already `"cpp"`, selecting a maxLik optimizer without an
explicit backend SHALL work without further user action.

#### Scenario: incompatible backend
- **WHEN** `set_algorithm_newton(optimizer = "bfgs", backend = "gather")`
  reaches estimation
- **THEN** it aborts informatively, naming the required backend.

### Requirement: backend replaces engine with descriptive values
`set_algorithm_newton()` SHALL accept the computational implementation
through `backend = c("cpp", "r", "gather")` — the C++ event loop
(default), the R reference implementation, and the gather-stack C++
variant — replacing the `engine` argument and its legacy values
(`"default_c"`, `"default"`, `"gather_compute"`). `engine =` SHALL remain
as a `lifecycle::deprecated()` sentinel, and the legacy values SHALL be
accepted wherever supplied (mapped `default_c → cpp`, `default → r`,
`gather_compute → gather`) with a single soft-deprecation warning naming
the final spelling. Estimation behavior per backend SHALL be identical to
the corresponding legacy engine; the internal engine tokens of the
compiled interface are unchanged.

#### Scenario: descriptive value selects the backend
- **WHEN** `set_algorithm_newton(backend = "r")` drives an estimation
- **THEN** the R reference implementation runs, identical to the legacy
  `engine = "default"` path.

#### Scenario: legacy argument and value map with one warning
- **WHEN** `set_algorithm_newton(engine = "gather_compute")` is called
- **THEN** one soft-deprecation warning names `backend = "gather"` and
  the returned options select the gather backend.

#### Scenario: invalid backend value lists the new vocabulary
- **WHEN** `set_algorithm_newton(backend = "fortran")` is called
- **THEN** it aborts with a cli error naming `cpp`, `r`, and `gather`, and the
  value that was supplied.

