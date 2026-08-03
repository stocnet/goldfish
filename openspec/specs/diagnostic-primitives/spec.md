# diagnostic-primitives Specification

## Purpose
TBD - created by archiving change residuals-gof. Update Purpose after archive.
## Requirements
### Requirement: diagnostics option names stored primitives
`set_algorithm_newton()` SHALL accept a `diagnostics` argument taking a
character vector of primitive names from `c("loglik", "scores", "ranks",
"margins", "probabilities", "conditional_scores", "availability")`, or
`TRUE` (equivalent to `c("loglik", "scores")`), `"all"` (every primitive),
or `FALSE`/`character(0)` (none). The default SHALL be
`c("loglik", "scores")`. The vocabulary names **what to compute**, and a
single name MAY map to several stored components — as `"loglik"` already
does; the finer per-component names belong to
`evaluate_model(return = )`, which names what to hand back. Each primitive maps
to a stored component on the fitted result: `"loglik"` → `intervalLogL`
(numeric, one per event) and, for exact-time submodels, `total_rate` (the
per-event sum of fitted rates over the realized risk set, numeric, one per
event — the quantity the waiting-time/Cox–Snell diagnostics and the
autograph Q-Q panel consume) and the conditional component (the per-event
log next-event probability — the Cox-partial-likelihood contribution —
computed in the estimation pass as the observed linear predictor minus the
per-event log-normalizer; defined on dependent events, `NA` on
right-censored intervals),
`"scores"` → `event_scores` (n_events × p,
columns named by effect), `"ranks"` → `observed_rank` (integer, one per
event, rank of the observed alternative among the risk-set weights),
`"margins"` → per-actor observed and expected vectors (both sender
and receiver margins on REM fits; see the margins requirement),
`"probabilities"` → per-event probability vectors aligned to the realized
risk set using the sanitized index/label vocabulary of the
process-state-evaluators capability — on exact-time submodels the
probability that each sender (rate) or dyad (REM) creates the next event,
summing to 1 per event, `"conditional_scores"` → the exact-time
conditional score rows defined in the requirement below, and
`"availability"` → the per-actor `n_opportunities` and (on exact-time
sub-models) `exposure` vectors defined in the requirement below. Unknown
names SHALL abort with a cli error listing the valid primitives.

#### Scenario: default stores loglik and scores
- **WHEN** a model is estimated without setting `diagnostics`
- **THEN** the result contains `intervalLogL` and `event_scores` and none of
  the other primitives.

#### Scenario: total_rate stored with loglik on exact-time models
- **WHEN** an exact-time rate or REM submodel is estimated with the default
  `diagnostics`
- **THEN** the result contains `total_rate` with one value per event, and
  `total_rate * interevent time` reproduces the Cox–Snell residuals without
  an evaluation pass.

#### Scenario: conditional loglik component stored on exact-time models
- **WHEN** an exact-time rate or REM submodel stores `"loglik"`
- **THEN** the result carries the per-event conditional component — `NA` at
  right-censored positions, and near the MLE (where the per-event expected
  count is order one) agreeing with the algebraic check
  `intervalLogL − log(total_rate) + interevent time × total_rate` — so
  partial-likelihood diagnostics read it without an evaluation pass.

#### Scenario: large-dataset storage note
- **WHEN** a model with a large event count (threshold documented on the
  option's help page) is estimated with the default `diagnostics`
- **THEN** a cli message notes that per-event diagnostic vectors are being
  stored, states their approximate memory footprint, and names
  `diagnostics = FALSE` as the opt-out.

#### Scenario: all shorthand
- **WHEN** `diagnostics = "all"` is set on a small exact-time fixture
- **THEN** the result contains every primitive component, including the
  conditional score rows.

#### Scenario: invalid name rejected
- **WHEN** `diagnostics = c("loglik", "devianc")` is passed
- **THEN** `set_algorithm_newton()` aborts with a cli error naming the valid
  primitive names.

### Requirement: probabilities guardrail
When `"probabilities"` is requested, estimation SHALL emit a cli warning
before the estimation run stating the estimated memory footprint computed
as `n_events × |riskset| × 8` bytes in human-readable units, and pointing
to `"ranks"` and `"margins"` as scalable alternatives. The warning SHALL
appear exactly once per estimation call. There SHALL be no silent size
threshold that drops the request.

#### Scenario: guardrail fires with estimated size
- **WHEN** a REM is estimated with `diagnostics` including `"probabilities"`
- **THEN** a cli warning is emitted before estimation reporting the
  estimated size in bytes/MB/GB for that model's event count and risk-set
  size, and the probabilities are still returned.

### Requirement: margins primitive content
The `"margins"` primitive SHALL store per-actor `observed` and `expected`
event-count vectors, named by actor label, defined per submodel:

- **choice (DyNAM, DyNAMi):** receiver margins — observed = tabulated
  receivers of the dependent events; `expected[r]` = sum over events of the
  fitted conditional-multinomial probability `p(r | s_k, t_k)` (zero for
  actors never at risk).
- **rate, exact-time:** sender margins — `expected[s]` = sum of interevent
  time multiplied by the sender's fitted rate, accumulated over every
  likelihood interval where `s` is active, **including right-censored
  intervals**.
- **rate, ordinal:** sender margins — `expected[s]` = sum of the sender's
  fitted multinomial probability.
- **REM (tie-oriented): both sender and receiver margins** from the same
  fit. Exact-time: the in-/out-degree compensators
  `expected_receiver[r]` = sum over events of interevent time multiplied by
  the summed fitted intensities of dyads incident to `r` (mirror over
  receivers for `expected_sender[s]`), right-censored intervals included;
  ordinal: the corresponding sums of fitted multinomial dyad probabilities
  (the coarsened multinomial).
- **choice_coordination:** per-actor margins over pairs — `expected[a]` =
  sum over events of the fitted probabilities of all pairs containing `a`;
  each event credits both members.

All expected counts SHALL accumulate over the same realized risk set as
estimation (support constraints, presence/composition changes, and the
unified `twomode_or_reflexive` broadcast flag — never `is_two_mode` alone).
On a two-mode fit (multimode mode-map models) the margins are per side —
sender margins over the sender-mode node slice, receiver margins over the
receiver-mode slice — with labels joined per side via the model's
`node_lookup`. The documentation SHALL present margins as calibration
descriptives (observed-vs-expected maps screening for unmodeled actor
heterogeneity), not per-actor tests: per-actor differences are plug-in
quantities, negatively correlated across actors; the formal test of actor
heterogeneity is the `test_parameter()` score test with an
activity/popularity candidate effect. Margins SHALL be stored on the
probability scale on every family — per-event next-event-probability sums
accumulated over **dependent events only**, the direct parallel of the
choice margins, totalling the event count at any parameter vector (a
right-censored interval realizes no mover, so it contributes nothing to
this variant) — and exact-time fits SHALL additionally store the
expected-count variant (compensator sums over **all intervals, dependent
and right-censored** — the compensator integrates over all exposure time),
whose per-actor observed-minus-expected is the
martingale residual. Each stored vector SHALL be labeled with its scale
(`"probability"` / `"expected_count"`) so a consumer can tell them apart
programmatically, and labels and shapes SHALL be identical whichever backend
produced the fit.

#### Scenario: multinomial margins sum exactly
- **WHEN** a choice, ordinal-rate, ordinal-REM, or choice_coordination
  model is estimated with `diagnostics` including `"margins"`
- **THEN** each expected-count vector sums, within floating-point
  tolerance and at any parameter vector, to the number of events (`2 *`
  the number of events for choice_coordination), and the observed-count
  vector reproduces the tabulated actors of the dependent events.

#### Scenario: exact-time expected-count margins sum at the MLE
- **WHEN** a converged exact-time rate or REM fit (time intercept present,
  as goldfish enforces for exact-time models) stores `"margins"`
- **THEN** the `"expected_count"` vector sums to the number of events within
  the convergence tolerance (the intercept score-equation identity — not a
  floating-point identity), and for REM the sender and receiver expected
  totals are identical.

#### Scenario: exact-time probability margins sum at any parameter vector
- **WHEN** an exact-time rate or REM fit stores `"margins"` at any parameter
  vector (converged or not)
- **THEN** the `"probability"` vector sums, within floating-point tolerance,
  to the number of dependent events — the same identity the multinomial
  families satisfy, since each event contributes a probability vector
  summing to 1.

#### Scenario: REM margins carry both sides
- **WHEN** a REM is estimated with `diagnostics` including `"margins"`
- **THEN** the result contains both sender and receiver margin vectors
  under the single `"margins"` primitive.

#### Scenario: the margins scales are labeled
- **WHEN** a multinomial fit and an exact-time fit each store `"margins"`
- **THEN** the multinomial fit carries the `"probability"` vector only, the
  exact-time fit carries both the `"probability"` and `"expected_count"`
  vectors, each labeled, and neither the labels nor the shapes vary with the
  backend that produced the fit.

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
ordinal rate and REM sub-models, choice_coordination) SHALL store nothing
and SHALL NOT warn: those likelihoods are already conditional, so their
`event_scores` rows *are* the conditional score rows. The documentation
SHALL state this, so the absence reads as an identity rather than as a
gap. The per-event storage footprint note SHALL size this primitive as it
sizes `"scores"`.

The same rows SHALL also be obtainable on demand from
`evaluate_model(return = "conditional_scores")` over the preprocessed
statistics, so a fit that did not store the primitive can still produce
them without re-estimating. Storage and evaluation SHALL be the same
computation reached through the same engine flag — never two
implementations — and SHALL agree at the fitted estimate within the
documented cross-backend tolerance.

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

#### Scenario: requesting it off exact-time is a silent no-op
- **WHEN** a choice sub-model is estimated with `"conditional_scores"`
  requested
- **THEN** the fit carries no such component, no warning is emitted, and
  the stored `event_scores` are unchanged.

#### Scenario: every backend produces the same rows
- **WHEN** the same exact-time model is estimated on `cpp`, `r` and
  `gather` with `"conditional_scores"` requested, at a shared parameter
  vector
- **THEN** the stored rows agree within the documented cross-backend
  tolerance.

### Requirement: availability primitive
The `"availability"` primitive SHALL store the per-actor availability
vectors defined by the model-evaluation-pass capability — `n_opportunities`
on every family, and `exposure` additionally on exact-time sub-models — as
a single `availability` component of the fitted result, shaped exactly as
the `margins` component is: the same container, the same per-side suffix
rule (unsuffixed on the sender, receiver and endpoint families;
`_sender` / `_receiver` on the two-sided REM families, since a dyad at risk
makes its sender available on one side and its receiver on the other), and
the same actor labeling. On a multinomial family `exposure` SHALL simply be
absent, and requesting the primitive there SHALL NOT warn: the compensator
scale it measures is not defined for those models, the same reason
`total_rate` is absent from them.

`margin_table()` SHALL NOT be extended to carry availability columns: it
stays the margins accessor, and a per-actor join surface is a separate
future capability.

The stored vectors SHALL be the same quantities
`evaluate_model(return = c("exposure", "n_opportunities"))` returns,
computed by the same engine flag in the same masked walk — never a second
implementation, and never an R-side replay of the availability masks. A fit
that stored them SHALL NOT require an evaluation pass to read them; a fit
that did not SHALL still be able to obtain them on demand.

Because these are per-actor vectors of the node set's length, and because
they do not depend on the parameter vector, they SHALL NOT be subject to
the per-event storage footprint note that governs `"scores"`,
`"conditional_scores"` and `"probabilities"`.

#### Scenario: availability rides along with the fit
- **WHEN** a model is estimated with `"availability"` among the
  `diagnostics` primitives
- **THEN** the result carries an `availability` component holding the
  opportunity counts named by actor label, and an exact-time fit
  additionally the exposure times, without any evaluation pass being run
  afterwards to read them

#### Scenario: a two-sided family reports each side separately
- **WHEN** availability is stored on a REM fit
- **THEN** its component names carry the same `_sender` / `_receiver`
  suffixes the fit's margins carry, so an actor available only as a
  receiver shows zero sender-side exposure and non-zero receiver-side
  exposure

#### Scenario: the compensator-scale vector is absent, silently, off exact-time
- **WHEN** a choice sub-model is estimated with `"availability"` requested
- **THEN** the fit carries `n_opportunities` and no `exposure`, and no
  warning is emitted

#### Scenario: an actor at risk in many dyads is counted once
- **WHEN** availability is stored on a one-mode REM fit
- **THEN** each actor's exposure is at most the observation window and each
  actor's opportunity count is at most the number of dependent events,
  because membership is counted per interval and per event rather than per
  dyad of the risk set

### Requirement: margin_table accessor
goldfish SHALL export `margin_table()`, returning the stored margins of a
fit as one tibble with columns `actor`, `role`, `observed`,
`expected_probability`, and `expected_count`, identically shaped on every
family. The `role` column SHALL take values `sender`, `receiver`, and
`endpoint`: REM fits contribute one `sender` and one `receiver` row per
actor, rate fits `sender` rows, choice fits `receiver` rows, and
choice_coordination fits `endpoint` rows (whose observed column totals
twice the event count — each event credits both endpoints). Both expected
columns SHALL always be present; `expected_count` SHALL be `NA` on
multinomial families, and the documentation SHALL state that this `NA`
means the compensator scale is not defined for the model class, never that
it was not computed. The returned object SHALL carry the class
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
- **THEN** every result has the same five columns with the family's roles
  as specified, and the REM table carries two rows per actor.

#### Scenario: not-defined scale is NA, not absent
- **WHEN** `margin_table()` is called on a multinomial-family fit
- **THEN** the `expected_count` column is present and entirely `NA`, and
  the `context` attribute names `expected_probability` as the only
  defined scale.

#### Scenario: flavored fits gain flavor columns
- **WHEN** `margin_table()` is called on a `flavored_result.goldfish`
- **THEN** the result contains each fid's rows tagged with `flavor` and
  `family` columns consistent with the fit's `process_map`, and printing
  the object reports the flavors estimated.

### Requirement: legacy flags are soft-deprecated onto diagnostics
The two publicly-shipped legacy flags SHALL remain accepted:
`return_interval_loglik` and `return_probabilities` (public since CRAN 1.6.x
and v1.7.0) keep working in `set_algorithm_newton()` with their current
semantics, each emitting a lifecycle soft-deprecation warning that names
the corresponding `diagnostics` primitive (`"loglik"`, `"probabilities"`).
Supplying both a legacy flag and a conflicting `diagnostics` value SHALL
abort with a cli error. `return_event_scores` SHALL NOT be an argument of
`set_algorithm_newton()`: it never shipped in a public release, so it is
removed at 2.0.0 without a deprecation cycle (the deprecation-scope audit is
`backend-parity` design D11; the scores primitive itself is specced there as
"Per-event scores primitive").

#### Scenario: legacy flag maps with deprecation warning
- **WHEN** `set_algorithm_newton(return_interval_loglik = TRUE)` is called
- **THEN** a lifecycle deprecation warning points to
  `diagnostics = "loglik"` and the resulting options store the loglik
  primitive.

#### Scenario: the never-public flag is gone
- **WHEN** `set_algorithm_newton(return_event_scores = TRUE)` is called at
  2.0.0
- **THEN** the call fails as an unknown argument, with no lifecycle warning
  path for it.

### Requirement: return_preprocessed attaches the replay object
`estimate_dynam()`, `estimate_rem()`, and `estimate_dynami()` SHALL accept
`return_preprocessed = FALSE`; when `TRUE`, the returned fit SHALL carry the
`preprocessed.goldfish` object used for estimation, and a cli message SHALL
report its approximate size. Diagnostic consumers requiring a statistics
replay SHALL accept a `preprocessed =` argument and SHALL use, in order of
precedence: the supplied `preprocessed`, then the object attached to the
fit. When neither is available, they SHALL abort with a cli error naming
both routes: `return_preprocessed = TRUE` at estimation, or
`preprocessed = compute_statistics(..., output = "preprocessed")` — the
consolidated producer from the revise-gather-output change, whose
`"preprocessed"` output IS the replay object this argument takes. The
error SHALL NOT name `estimate_*(..., preprocessing_only = TRUE)`, which
that change soft-deprecated onto exactly this route.

#### Scenario: replay unavailable produces guiding error
- **WHEN** a diagnostic requiring a replay is called on a fit estimated
  without `return_preprocessed` and without a `preprocessed` argument
- **THEN** it aborts with a cli error that names both supported routes.

#### Scenario: attached object is used
- **WHEN** a model is estimated with `return_preprocessed = TRUE` and a
  replay-requiring diagnostic is called without `preprocessed`
- **THEN** the diagnostic runs using the attached object without
  re-preprocessing.

### Requirement: Ranks resolve tied alternatives by a documented, reproducible rule

The stored `observed_rank` SHALL resolve alternatives of equal probability by a
documented rule that treats values within a stated relative tolerance as tied,
so that the rank a user reads does not depend on which backend produced the fit.
Exact ties are not an edge case in these models: on a nearly empty network most
alternatives have identical statistics, so a strict comparison resolves large
blocks by whichever way the last bit happens to fall. The rule and its tolerance
SHALL be documented where a user reading a rank will find them, and the same
rule SHALL govern every rank-sensitive primitive, so that a rank and a recall
statistic over the same event agree about which alternatives are tied.

#### Scenario: tied alternatives get the same rank on every backend
- **WHEN** a model whose risk set contains blocks of equal-probability
  alternatives is estimated on each supported backend with ranks requested
- **THEN** the stored `observed_rank` vectors are identical

#### Scenario: the tie rule is applied consistently across rank-sensitive primitives
- **WHEN** a rank and a top-k recall statistic are computed for the same event
  over a risk set containing a tied block spanning the k-th position
- **THEN** both treat the same alternatives as tied, so the two do not disagree
  about whether the observed alternative is within the top k

#### Scenario: genuinely distinct alternatives are not merged
- **WHEN** two alternatives differ in probability by more than the documented
  tolerance
- **THEN** they receive distinct ranks, so the tolerance does not flatten real
  differences

