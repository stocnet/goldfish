# optimizer-selection (delta)

## RENAMED Requirements

- FROM: `### Requirement: User-facing return_event_scores option`
- TO: `### Requirement: Per-event scores primitive`

## MODIFIED Requirements

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
