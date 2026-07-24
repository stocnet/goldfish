# diagnostic-primitives

Storage of per-event diagnostic quantities at estimation time and the
preprocessed-object plumbing that on-demand diagnostics consume.

## ADDED Requirements

### Requirement: diagnostics option names stored primitives
`set_algorithm_newton()` SHALL accept a `diagnostics` argument taking a
character vector of primitive names from `c("loglik", "scores", "ranks",
"margins", "probabilities")`, or `TRUE` (equivalent to
`c("loglik", "scores")`), `"all"` (all five), or `FALSE`/`character(0)`
(none). The default SHALL be `c("loglik", "scores")`. Each primitive maps
to a stored component on the fitted result: `"loglik"` → `intervalLogL`
(numeric, one per event) and, for exact-time submodels, `total_rate` (the
per-event sum of fitted rates over the realized risk set, numeric, one per
event — the quantity the waiting-time/Cox–Snell diagnostics and the
autograph Q-Q panel consume), `"scores"` → `event_scores` (n_events × p,
columns named by effect), `"ranks"` → `observed_rank` (integer, one per
event, rank of the observed alternative among the risk-set weights),
`"margins"` → per-actor observed and expected count vectors (both sender
and receiver margins on REM fits; see the margins requirement),
`"probabilities"` → per-event probability vectors aligned to the realized
risk set using the sanitized index/label vocabulary of the
process-state-evaluators capability. Unknown names SHALL abort with a cli
error listing the valid primitives.

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

#### Scenario: large-dataset storage note
- **WHEN** a model with a large event count (threshold documented on the
  option's help page) is estimated with the default `diagnostics`
- **THEN** a cli message notes that per-event diagnostic vectors are being
  stored, states their approximate memory footprint, and names
  `diagnostics = FALSE` as the opt-out.

#### Scenario: all shorthand
- **WHEN** `diagnostics = "all"` is set on a small fixture
- **THEN** the result contains all five primitive components.

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
activity/popularity candidate effect.

#### Scenario: multinomial margins sum exactly
- **WHEN** a choice, ordinal-rate, ordinal-REM, or choice_coordination
  model is estimated with `diagnostics` including `"margins"`
- **THEN** each expected-count vector sums, within floating-point
  tolerance and at any parameter vector, to the number of events (`2 *`
  the number of events for choice_coordination), and the observed-count
  vector reproduces the tabulated actors of the dependent events.

#### Scenario: exact-time margins sum at the MLE
- **WHEN** a converged exact-time rate or REM fit (time intercept present,
  as goldfish enforces for exact-time models) stores `"margins"`
- **THEN** each expected-count vector sums to the number of events within
  the convergence tolerance (the intercept score-equation identity — not a
  floating-point identity), and for REM the sender and receiver expected
  totals are identical.

#### Scenario: REM margins carry both sides
- **WHEN** a REM is estimated with `diagnostics` including `"margins"`
- **THEN** the result contains both sender and receiver margin vectors
  under the single `"margins"` primitive.

### Requirement: legacy flags are soft-deprecated onto diagnostics
The legacy flags SHALL remain accepted: `return_interval_loglik`,
`return_probabilities`, and
`return_event_scores` keep working in `set_algorithm_newton()` with
their current
semantics, each emitting a lifecycle soft-deprecation warning that names
the corresponding `diagnostics` primitive (`"loglik"`, `"probabilities"`,
`"scores"`). Supplying both a legacy flag and a conflicting `diagnostics`
value SHALL abort with a cli error.

#### Scenario: legacy flag maps with deprecation warning
- **WHEN** `set_algorithm_newton(return_interval_loglik = TRUE)` is called
- **THEN** a lifecycle deprecation warning points to
  `diagnostics = "loglik"` and the resulting options store the loglik
  primitive.

### Requirement: return_preprocessed attaches the replay object
`estimate_dynam()`, `estimate_rem()`, and `estimate_dynami()` SHALL accept
`return_preprocessed = FALSE`; when `TRUE`, the returned fit SHALL carry the
`preprocessed.goldfish` object used for estimation, and a cli message SHALL
report its approximate size. Diagnostic consumers requiring a statistics
replay SHALL accept a `preprocessed =` argument and SHALL use, in order of
precedence: the supplied `preprocessed`, then the object attached to the
fit. When neither is available, they SHALL abort with a cli error naming
both routes (`return_preprocessed = TRUE` at estimation, or
`preprocessed = compute_statistics(...)` — the consolidated producer from
the revise-gather-output change; `estimate_*(..., preprocessing_only =
TRUE)` remains its equivalent until superseded).

#### Scenario: replay unavailable produces guiding error
- **WHEN** a diagnostic requiring a replay is called on a fit estimated
  without `return_preprocessed` and without a `preprocessed` argument
- **THEN** it aborts with a cli error that names both supported routes.

#### Scenario: attached object is used
- **WHEN** a model is estimated with `return_preprocessed = TRUE` and a
  replay-requiring diagnostic is called without `preprocessed`
- **THEN** the diagnostic runs using the attached object without
  re-preprocessing.
