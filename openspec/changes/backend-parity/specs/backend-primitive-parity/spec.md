# backend-primitive-parity (delta)

## ADDED Requirements

### Requirement: Every backend computes the same per-event reduction inputs
Every estimation backend SHALL form, at each event, the probability vector
`p_e` over that event's risk set together with a scalar scale `c_e`, such that
`m_ej = c_e * p_ej` is the contribution of alternative `j`. `p_e` SHALL be
derived from a numerically stable log-sum-exp of the linear predictors —
`p_ej = exp(x_ej - lse_e)` where `lse_e = log sum_j exp(x_ej)` — so it is exact
whether or not the corresponding rates overflow or underflow, and SHALL sum to
1 per event in every family: the choice probability on multinomial sub-models
(DyNAM-choice, DyNAM-rate_ordered, REM_ordered, choice_coordination) and the
competing-risks probability that alternative `j` produces the next event on
exact-time sub-models (DyNAM-rate, REM). The scale SHALL carry the family
difference: `c_e = 1` for the probability scale in every family, and
`c_e = Δt_e * T_e` for the exact-time compensator scale, where `T_e` is the
per-event total rate — so exact-time compensator contributions total the
expected number of events over the interval, and `m_ej = Δt_e * λ_ej` exactly.
The four per-event diagnostic primitives SHALL be defined as reductions of
`(p_e, c_e)` and SHALL NOT be defined per backend.

#### Scenario: multinomial contributions form a probability vector
- **WHEN** any backend evaluates a multinomial sub-model at a fixed parameter
  vector
- **THEN** the per-event contributions `m_e` sum to 1 for every event.

#### Scenario: timed contributions carry the interval
- **WHEN** any backend evaluates a timed sub-model at a fixed parameter vector
- **THEN** the per-event contributions `m_e` sum to the interval length times
  the event's total rate, so the accumulated expected counts are comparable to
  observed event counts.

#### Scenario: exact-time probability contributions sum to one
- **WHEN** any backend evaluates an exact-time sub-model at a fixed parameter
  vector
- **THEN** the per-event probability-scale vector `p_e` sums to 1 — the
  probability that each sender (rate) or dyad (REM) is the next to create an
  event.

#### Scenario: the probability scale survives an overflowing total rate
- **WHEN** an exact-time sub-model is evaluated at a parameter vector where the
  per-event total rate exceeds the double range, so the per-event
  log-likelihood is not finite
- **THEN** `p_e` is still exact and sums to 1, and the per-event ranks derived
  from it are unaffected — the stable log-sum-exp, not the raw rate sum, is
  what the probability scale is built on.

#### Scenario: the probability scale survives underflowing rates
- **WHEN** every rate in an event's risk set underflows to zero on the linear
  scale
- **THEN** `p_e` is still exact, rather than the `NaN` or the spurious `1` a
  ratio of underflowed rates produces.

### Requirement: The per-event reductions live in one shared implementation
The rank, margin and event-score reductions SHALL be implemented once as shared
C++ helpers consuming only `(w_e, c_e, X_e, observed index, actor index)` — no
loop state, presence buffers, or broadcast decoding — and SHALL be consumed by
every compiled kernel of both the `cpp` and `gather` backends. The `r` backend
SHALL mirror the same reductions in R, as the R `stable_softmax()` helper
already mirrors its C++ counterpart. Adding a per-event primitive SHALL require
one new reduction plus its R mirror, not one edit per kernel.

#### Scenario: a kernel does not carry its own reduction
- **WHEN** a compiled kernel produces `ranks`, `margins` or `event_scores`
- **THEN** it does so by calling the shared helper, and contains no private
  copy of the reduction arithmetic.

#### Scenario: the R mirror agrees with the C++ helper
- **WHEN** the same `(w_e, c_e, X_e, observed)` inputs are reduced by the C++
  helper and by the R mirror
- **THEN** the results are identical for ranks and agree within 1e-10 for
  margins and event scores.

### Requirement: The backend and primitive support contract is declared once
The supported `(backend, primitive)` combinations SHALL be declared in a single
table consulted once per estimation, before any preprocessing runs. An
unsupported combination SHALL abort with one cli error naming the primitive, the
requested backend, and the backends that do support that primitive. A requested
primitive SHALL NOT be silently dropped from the result, and the requested
backend SHALL NOT be silently changed in order to serve a primitive.

#### Scenario: unsupported combination aborts once, naming alternatives
- **WHEN** a primitive is requested on a backend the table does not support
- **THEN** estimation aborts before preprocessing with a cli error naming the
  primitive, the requested backend, and the supporting backends.

#### Scenario: no silent omission
- **WHEN** estimation completes with a set of requested primitives
- **THEN** every requested primitive is present on the result object.

#### Scenario: no silent backend substitution
- **WHEN** a fit is requested on a given backend with any set of primitives
- **THEN** the fit is produced by that backend, or estimation aborts.

#### Scenario: an unrelated primitive does not change the verdict
- **WHEN** the same primitive is requested on the same backend, once alone and
  once alongside further primitives
- **THEN** both requests reach the same verdict — both succeed, or both abort
  with the same error.

### Requirement: All five primitives are available on all three backends
The `loglik`, `scores`, `ranks`, `margins` and `probabilities` primitives SHALL
each be produced by the `cpp`, `r` and `gather` backends. The `gather` backend
SHALL accumulate `scores`, `ranks` and `margins` inside its compute kernels,
reading the per-row actor index the gather stack already carries. The `r`
backend SHALL accumulate `ranks` and `margins` in its contribution loop without
materializing the full per-event probability matrix. The `cpp` backend SHALL
return per-event probabilities without redirecting to another backend.
`probabilities` SHALL be the probability-scale vector on every family —
next-event probabilities, summing to 1 per event, on exact-time sub-models.
On exact-time sub-models the `loglik` primitive SHALL include, on every
backend, the per-event `total_rate` and the conditional component (the
per-event log next-event probability), and the `margins` primitive SHALL
carry both labeled scale variants (probability and expected-count) as
defined by the diagnostic-primitives capability. The per-event storage
guardrail SHALL continue to govern the cost of `probabilities` on every
backend.

#### Scenario: every cell of the support matrix is populated
- **WHEN** a fit is requested on each of `cpp`, `r` and `gather` with
  `diagnostics = "all"`
- **THEN** each fit carries all five primitives, and none of the three emits a
  backend-substitution warning.

#### Scenario: gather stores per-event scores
- **WHEN** a model is estimated with `backend = "gather"` and the `scores`
  primitive requested
- **THEN** the result contains `event_scores` whose column sums equal the
  aggregate score, and no error is raised.

#### Scenario: the R backend accumulates margins without the probability matrix
- **WHEN** a model is estimated with `backend = "r"` and the `margins`
  primitive requested but not `probabilities`
- **THEN** the result carries margins and carries no per-event probability
  matrix.

#### Scenario: total_rate rides loglik on every backend
- **WHEN** an exact-time sub-model is estimated on each of `cpp`, `r` and
  `gather` with the `loglik` primitive requested
- **THEN** each fit carries the per-event `total_rate` and the conditional
  log-probability component, and the three backends' vectors agree within
  1e-10 at a fixed parameter vector.

#### Scenario: the conditional component is computed from the log-normalizer
- **WHEN** the conditional log-probability is stored for an exact-time fit
- **THEN** it equals the observed alternative's linear predictor minus the
  per-event log-sum-exp, and it is finite whenever `p_obs` is non-zero — including
  where the total rate has overflowed and the per-event log-likelihood has not.

#### Scenario: the algebraic identity is a check near the MLE, not the route
- **WHEN** an exact-time model is evaluated at its MLE, where the per-event
  expected count is of order one
- **THEN** the stored conditional component agrees with
  `intervalLogL − log(total_rate) + Δt · total_rate` — an independent check that
  is only accurate in that regime, since the identity loses digits in proportion
  to the expected count and cannot itself meet the cross-backend tolerance away
  from the MLE.

#### Scenario: both margins variants agree across backends
- **WHEN** an exact-time sub-model is estimated on each backend with
  `margins` requested at a fixed parameter vector
- **THEN** each fit carries the probability-scale and expected-count margin
  vectors, and each variant agrees across the three backends within 1e-10.

#### Scenario: guardrail still fires for probabilities
- **WHEN** `probabilities` is requested on the `cpp` backend for a fit above
  the storage threshold
- **THEN** the storage guardrail message is emitted before the fit runs.

### Requirement: The fitted result records the backend that produced it
Every fitted `result.goldfish` object SHALL carry the resolved backend value
(`"cpp"`, `"r"` or `"gather"`) as its `backend` component, written once in the
shared results assembly so both compiled and R estimation paths populate it
identically. Consumers of fitted objects SHALL treat a missing `backend`
component (a fit produced before 2.0.0) as an unknown backend — falling back
to the presence or absence of the components they need — and SHALL NOT error
on the absence itself.

#### Scenario: the fit names its backend
- **WHEN** the same model is estimated once on each of `cpp`, `r` and `gather`
- **THEN** each fitted object's `backend` component equals the backend that
  ran it.

#### Scenario: a diagnostic names the backend to refit with
- **WHEN** a post-estimation consumer requires a primitive the fit does not
  carry
- **THEN** its error can name the fit's recorded backend and the backends that
  support the primitive, rather than reporting a generic missing component.

#### Scenario: pre-2.0.0 fits are tolerated
- **WHEN** a fitted object with no `backend` component is passed to a consumer
  that gates on the backend
- **THEN** the consumer proceeds from the components the fit actually carries
  and does not error on the missing field.

### Requirement: Backends agree numerically on every primitive
Any two backends SHALL agree on every primitive computed for the same model,
fixture and **fixed parameter vector**: `ranks` exactly, and `margins`,
`event_scores` and `probabilities` within 1e-10. Parity SHALL be asserted at a
fixed parameter vector rather than at each backend's own converged estimate,
because converged estimates agree only within the coarser cross-backend
coefficient tolerance, which would mask a reduction discrepancy. At least one
primitive SHALL additionally be checked against an independent reconstruction
that does not use the shared reduction, so a shared implementation error is not
self-confirming.

#### Scenario: ranks agree exactly across backends
- **WHEN** the same fixture is evaluated on `cpp`, `r` and `gather` at one
  fixed parameter vector with `ranks` requested
- **THEN** the three integer rank vectors are identical.

#### Scenario: margins and scores agree within tolerance
- **WHEN** the same fixture is evaluated on `cpp`, `r` and `gather` at one
  fixed parameter vector with `margins` and `scores` requested
- **THEN** the margin vectors and score matrices agree within 1e-10 across all
  three backends.

#### Scenario: an independent reconstruction confirms the reduction
- **WHEN** ranks and margins are reconstructed from a stored per-event
  probability matrix rather than from the shared reduction
- **THEN** the reconstruction matches the reduced values.

#### Scenario: parity holds for both sub-model families
- **WHEN** parity is asserted for a multinomial sub-model and for a timed
  sub-model
- **THEN** both families meet the tolerances above.

#### Scenario: coordination's ragged risk set uses the same rank rule
- **WHEN** a choice_coordination fixture (ragged per-sender risk-set groups)
  is evaluated with `ranks` requested
- **THEN** the rank of the observed dyad counts strictly-greater weights over
  the event's whole realized risk set — not within a group — and agrees
  exactly across backends.
