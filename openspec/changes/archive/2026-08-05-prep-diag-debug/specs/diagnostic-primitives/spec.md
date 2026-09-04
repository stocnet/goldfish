## MODIFIED Requirements

### Requirement: conditional scores primitive
The `"conditional_scores"` primitive SHALL store, on exact-time sub-models
only, the per-event score rows of the model's **conditional** (partial)
likelihood: `X_obs − sum_j p_j X_j` over the realized risk set, with
`p_j = lambda_j / sum lambda`, one row per interval and one column per
coefficient, `NA` on right-censored intervals, which realize no observed
alternative. It SHALL be the score counterpart of the `"loglik"`
primitive's conditional component, and SHALL be computed in the estimation
pass by the shared per-event score reduction at unit scale — the same
reduction, and the same call, the multinomial kernels already make — never
reassembled afterwards from the stored score rows, which carry the exposure
term and cannot be un-scaled without the observed alternative's own
statistic row. It SHALL be produced identically by the `cpp`, `r` and
`gather` backends.

Requesting `"conditional_scores"` on a multinomial family (choice, the
ordinal rate and REM sub-models, choice_coordination) SHALL store nothing,
because those likelihoods are already conditional, so their `event_scores`
rows *are* the conditional score rows. The estimation call SHALL emit one
informational message naming that identity and the `set_algorithm_newton()`
adjustment that drops the redundant request, so the absence is read as an
identity rather than passing unnoticed. It SHALL be a message and not a
warning: the condition is informational, it arises once per estimation call
and so would be noisy under repeated fitting, and a warning would be
promoted to an error under `options(warn = 2)`. The documentation SHALL
state the identity. The per-event storage footprint note SHALL size this
primitive as it sizes `"scores"`.

The same rows SHALL also be obtainable on demand from
`evaluate_model(return = "conditional_scores")` over the preprocessed
statistics, so a fit that did not store the primitive can still produce
them without re-estimating. Storage and evaluation SHALL be the same
computation reached through the same engine flag — never two
implementations — and SHALL agree at the fitted estimate within the
documented cross-backend tolerance. Requesting them from the evaluator on a
family that does not define them SHALL abort naming the family, as
`"exposure"` already does: asking the evaluator for a value is a demand for
that value, where requesting a stored primitive is a preference about what
to keep.

#### Scenario: the conditional rows drop the exposure term
- **WHEN** an exact-time rate fit stores both `"scores"` and
  `"conditional_scores"`
- **THEN** each dependent interval's conditional row equals its score row
  with the exposure-weighted mean statistic replaced by the
  probability-weighted one, and the two coincide only where the interval's
  expected count is one.

#### Scenario: the conditional rows are the multinomial score rows
- **WHEN** the conditional score rows of an exact-time fit are compared, at
  the same non-intercept coefficients, with the stored `event_scores` of
  the corresponding ordinal sub-model over the same statistics
- **THEN** they agree within tolerance on the dependent intervals, and the
  time-intercept column of the conditional rows is zero, because a
  constant statistic cancels in the risk-set mean.

#### Scenario: a fit that stored nothing can still be evaluated
- **WHEN** `evaluate_model(return = "conditional_scores")` is called on an
  exact-time fit estimated without the primitive, with the preprocessed
  statistics attached or supplied
- **THEN** the rows are returned, and they equal the rows the same fit
  would have stored had the primitive been requested at estimation

#### Scenario: requesting it off exact-time informs and stores nothing
- **WHEN** a choice sub-model is estimated with `"conditional_scores"`
  requested
- **THEN** the fit carries no such component, the stored `event_scores` are
  unchanged, and one message names the identity and how to drop the request

#### Scenario: the informational message is not a warning
- **WHEN** a choice sub-model is estimated with `"conditional_scores"`
  requested under `options(warn = 2)`
- **THEN** estimation completes and no condition is promoted to an error

#### Scenario: the evaluator refuses what it cannot define
- **WHEN** `evaluate_model(return = "conditional_scores")` is called on a
  multinomial fit
- **THEN** it aborts naming the family, rather than returning nothing under
  a name that was asked for

#### Scenario: every backend produces the same rows
- **WHEN** the same exact-time model is estimated on `cpp`, `r` and
  `gather` with `"conditional_scores"` requested, at a shared parameter
  vector
- **THEN** the stored rows agree within the documented cross-backend
  tolerance.

### Requirement: margin_table accessor
goldfish SHALL export `margin_table()`, returning the stored margins of a
fit as one tibble with columns `actor`, `role`, `observed`,
`expected_probability`, `expected_count` and `dispersion`, identically
shaped on every
family. The `role` column SHALL take values `sender`, `receiver`, and
`endpoint`: REM fits contribute one `sender` and one `receiver` row per
actor, rate fits `sender` rows, choice fits `receiver` rows, and
choice_coordination fits `endpoint` rows (whose observed column totals
twice the event count — each event credits both endpoints). Both expected
columns SHALL always be present; `expected_count` SHALL be `NA` on
multinomial families, and the documentation SHALL state that this `NA`
means the compensator scale is not defined for the model class, never that
it was not computed.

The `dispersion` column SHALL carry the actor's **shape** statistic — the
sample variance of that actor's stratified waiting-time residuals, which is
one under a correctly specified model — and SHALL follow the same
not-defined convention as `expected_count`: present always, `NA` on the
families that define no waiting time. It answers the question the other
columns structurally cannot. `observed` and `expected_count` are the count
and the sum of one actor's stratified residuals, so together they report
**level** only; an actor whose events are correctly counted but clustered in
time is calibrated on every other column and detected on this one. The
documentation SHALL state that relationship, and SHALL state that the
statistic is uninformative for an actor with few events, the column being
read alongside `observed` rather than alone.

The returned object SHALL carry the class
`margin_table` prepended to the tibble classes and the diagnostic metadata
attributes (`diagnostic`, `context`, `params`, `version`), where `context`
records which expected columns are defined for the family, the event
totals per side, and — on two-mode fits — the node-set/mode labels. A
`flavored_result.goldfish` method SHALL return the row-bound per-fid
tables with `flavor` and `family` columns from the fit's `process_map`,
and its print method SHALL reflect the multiple flavors estimated.

#### Scenario: one schema across families
- **WHEN** `margin_table()` is called on a rate, choice, coordination, and
  REM fit each storing `"margins"`
- **THEN** every result has the same columns with the family's roles
  as specified, and the REM table carries two rows per actor.

#### Scenario: not-defined scale is NA, not absent
- **WHEN** `margin_table()` is called on a multinomial-family fit
- **THEN** the `expected_count` and `dispersion` columns are present and
  entirely `NA`, and the `context` attribute names `expected_probability`
  as the only defined scale.

#### Scenario: level and shape are separate readings
- **WHEN** an exact-time fit contains an actor whose event count matches its
  expectation but whose events are clustered in time
- **THEN** that actor's `observed` and `expected_count` agree while its
  `dispersion` departs from one

#### Scenario: flavored fits gain flavor columns
- **WHEN** `margin_table()` is called on a `flavored_result.goldfish`
- **THEN** the result contains each fid's rows tagged with `flavor` and
  `family` columns consistent with the fit's `process_map`, and printing
  the object reports the flavors estimated.
