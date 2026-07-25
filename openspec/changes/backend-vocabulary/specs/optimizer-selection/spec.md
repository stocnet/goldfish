# optimizer-selection (delta)

## RENAMED Requirements

- FROM: `### Requirement: maxLik optimizers run only on the default_c evaluator`
- TO: `### Requirement: maxLik optimizers run only on the cpp backend`

## MODIFIED Requirements

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


## ADDED Requirements

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
- **THEN** `match.arg()` rejects it naming `cpp`, `r`, and `gather`.
