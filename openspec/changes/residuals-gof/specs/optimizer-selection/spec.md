# optimizer-selection (delta)

## MODIFIED Requirements

### Requirement: User-facing return_event_scores option
`set_algorithm_newton()` SHALL accept a logical `return_event_scores` argument
(default `FALSE`), parallel to the existing `return_interval_loglik` and
`return_probabilities` flags. All three flags are soft-deprecated
(lifecycle) in favor of the `diagnostics` primitives vector defined in the
diagnostic-primitives capability: supplying a legacy flag SHALL emit a
lifecycle deprecation warning naming the corresponding primitive
(`return_event_scores` → `"scores"`, `return_interval_loglik` →
`"loglik"`, `return_probabilities` → `"probabilities"`) and SHALL behave
identically to requesting that primitive. When `TRUE` (or when
`diagnostics` includes `"scores"`), the fitted result object SHALL contain
the per-event score matrix as `event_scores` (n_events × p, columns named
by effect), evaluated at the returned parameter estimates. The option
SHALL be honored by **both compute backends** — `backend = "cpp"` (via the
evaluator flag) and `backend = "r"` (captured in its contribution loop) —
while requesting it with `backend = "gather"` SHALL abort with an informative
error. The two supporting backends SHALL agree on the returned matrix within
the cross-backend tolerance. The documentation SHALL describe the intended uses (robust
sandwich and clustered standard errors, per-effect score-process diagnostics,
event-influence measures) and SHALL point to the `diagnostics` vector as
the non-deprecated surface.

#### Scenario: scores returned on request
- **WHEN** a model is estimated with the scores primitive requested (via
  `diagnostics` or the deprecated flag) on the `cpp` or `r` backend
- **THEN** the result contains `event_scores` with one row per dependent event
  and one column per effect, and its column sums agree with the aggregate
  score at convergence (an algebraic 1e-10 identity; the aggregate score is
  near zero only for free parameters — offset columns carry the fixed
  value's nonzero score).

#### Scenario: backends agree on the score matrix
- **WHEN** the same fixture model is estimated on `backend = "r"` and
  `backend = "cpp"` with the scores primitive requested
- **THEN** the two `event_scores` matrices agree within 1e-10.

#### Scenario: the gather backend rejects the option
- **WHEN** the scores primitive is requested with
  `backend = "gather"`
- **THEN** estimation aborts with an informative error naming the supported
  backends.

#### Scenario: legacy flag deprecation
- **WHEN** `set_algorithm_newton(return_event_scores = TRUE)` is called
- **THEN** a lifecycle soft-deprecation warning names
  `diagnostics = "scores"` and the stored result is identical to requesting
  that primitive.

#### Scenario: scores stored by default
- **WHEN** a model is estimated with the default `diagnostics` value
- **THEN** the result contains `event_scores` (the `"scores"` primitive is
  part of the default set).
