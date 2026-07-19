# optimizer-selection (delta)

## MODIFIED Requirements

### Requirement: User-facing return_event_scores option
`set_estimation_opt()` SHALL accept a logical `return_event_scores` argument
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
SHALL be honored by the `default_c` engine (via the evaluator flag) and the
`default` R engine (captured in its contribution loop); requesting it with
the `gather_compute` engine SHALL abort with an informative error. Both
supported engines SHALL agree on the returned matrix within the cross-engine
tolerance. The documentation SHALL describe the intended uses (robust
sandwich and clustered standard errors, per-effect score-process diagnostics,
event-influence measures) and SHALL point to the `diagnostics` vector as
the non-deprecated surface.

#### Scenario: scores returned on request
- **WHEN** a model is estimated with the scores primitive requested (via
  `diagnostics` or the deprecated flag) on the `default_c` or `default`
  engine
- **THEN** the result contains `event_scores` with one row per dependent event
  and one column per effect, and its column sums agree with the (near-zero)
  aggregate score at convergence.

#### Scenario: engines agree on the score matrix
- **WHEN** the same fixture model is estimated on `default` and `default_c`
  with the scores primitive requested
- **THEN** the two `event_scores` matrices agree within 1e-10.

#### Scenario: gather_compute rejects the option
- **WHEN** the scores primitive is requested with
  `engine = "gather_compute"`
- **THEN** estimation aborts with an informative error naming the supported
  engines.

#### Scenario: legacy flag deprecation
- **WHEN** `set_estimation_opt(return_event_scores = TRUE)` is called
- **THEN** a lifecycle soft-deprecation warning names
  `diagnostics = "scores"` and the stored result is identical to requesting
  that primitive.

#### Scenario: scores stored by default
- **WHEN** a model is estimated with the default `diagnostics` value
- **THEN** the result contains `event_scores` (the `"scores"` primitive is
  part of the default set).
