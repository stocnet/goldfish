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

### Requirement: Per-event scores primitive
Estimation SHALL store the per-event score matrix as `event_scores`
(n_events × p, columns named by effect), evaluated at the returned parameter
estimates, when the `"scores"` primitive is requested through the
`diagnostics` vector defined by the diagnostic-primitives capability (part of
its default set). The primitive SHALL be honored by **all three backends** —
`backend = "cpp"` (via the evaluator flag), `backend = "r"` (captured in its
contribution loop) and `backend = "gather"` (accumulated in its compute
kernels from the per-event increment they already form) — and the three SHALL
agree on the returned matrix within the cross-backend tolerance. The legacy
`return_event_scores` flag SHALL NOT exist at 2.0.0: it never shipped in a
public release (CRAN 1.6.x or tag v1.7.0), so it is removed without a
deprecation cycle rather than soft-deprecated like the two public
`return_*` flags. The documentation SHALL describe the intended uses (robust
sandwich and clustered standard errors, per-effect score-process diagnostics,
event-influence measures) without this change implementing those diagnostics.

#### Scenario: scores returned on request
- **WHEN** a model is estimated with the `"scores"` primitive requested on any
  backend
- **THEN** the result contains `event_scores` with one row per dependent event
  and one column per effect, and its column sums agree with the aggregate
  score at convergence (an algebraic 1e-10 identity; the aggregate score is
  near zero only for free parameters — offset columns carry the fixed value's
  nonzero score).

#### Scenario: backends agree on the score matrix
- **WHEN** the same fixture model is estimated on `backend = "r"`,
  `backend = "cpp"` and `backend = "gather"` at one fixed parameter vector with
  the `"scores"` primitive requested
- **THEN** the three `event_scores` matrices agree within 1e-10.

#### Scenario: the gather backend stores the matrix rather than aborting
- **WHEN** the `"scores"` primitive is combined with `backend = "gather"`
- **THEN** estimation completes and the result carries `event_scores`, with no
  abort and no substitution of another backend.

#### Scenario: scores stored by default
- **WHEN** a model is estimated with the default `diagnostics` value
- **THEN** the result contains `event_scores` (the `"scores"` primitive is
  part of the default set).

#### Scenario: the removed flag is not an argument
- **WHEN** `set_algorithm_newton(return_event_scores = TRUE)` is called at
  2.0.0
- **THEN** the call fails as an unknown argument, with no lifecycle warning
  path for it.
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
the final spelling. The backend values SHALL be the only runtime vocabulary
downstream of the constructor: the returned control object SHALL carry the
resolved value as its `backend` component and SHALL NOT carry an `engine`
component, and estimation gating, dispatch and their messages SHALL compare
and report backend values directly, with no translation from a legacy token.
Estimation SHALL accept a control object that carries only a legacy `engine`
component (built before 2.0.0), resolving it to the corresponding backend
value on read. Estimation behavior per backend SHALL be identical to the
corresponding legacy engine.

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

#### Scenario: the control object speaks the backend vocabulary
- **WHEN** `set_algorithm_newton(backend = "cpp")` is called
- **THEN** the returned control object's `backend` component is `"cpp"` and
  the object has no `engine` component.

#### Scenario: a pre-2.0.0 control object still estimates
- **WHEN** a control list whose only implementation field is
  `engine = "default_c"` (as built by any pre-2.0.0 constructor —
  `set_estimation_opt()` since v1.7.0 or `set_algorithm_newton()` in 1.9.x) is
  supplied to an estimator
- **THEN** estimation runs on the `cpp` backend.

