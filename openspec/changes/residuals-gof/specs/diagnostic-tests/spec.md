# diagnostic-tests

Goodness-of-fit and specification tests: `test_gof()`, `test_parameter()`,
`test_time()`.

## ADDED Requirements

### Requirement: test_gof cumulative-score bridge test
`test_gof()` SHALL implement the Boschi-Wit martingale-residual test for
fixed-linear-effect models from stored `event_scores`: per effect, the
standardized cumulative score process
`W_d(u) = J_d^{-1/2} n^{-1/2} cumsum(s_kd)` with `J_d` the **empirical
per-event variance of the centered score contributions** (the per-effect
OPG scale both reference implementations use — Boschi's own code and
amorem 1.0.0; the observed-information diagonal `I_dd / n` estimates the
same per-event quantity and SHALL be documented as the asymptotically
equivalent alternative, not used: the two diverge by up to 37% on the
measured fixture and the choice moves p-values across conventional
levels — D32), the statistic `T_d = sup_u |W_d(u)|`, and — on the
default clock — the analytic Kolmogorov p-value
`p(t) = 2 * sum_{j>=1} (-1)^{j-1} exp(-2 j^2 t^2)`.
Effect-level p-values SHALL be combined per submodel block and jointly via
the Cauchy combination `T_o = mean(tan(pi * (0.5 - P_l)))` with
`p = 1/2 - atan(T_o)/pi`. The combination SHALL be **computed and carried on
the returned object** — per block, and jointly in the metadata — and SHALL
NOT be rendered by the print methods until a null-calibration and power study
has been run for it: the per-effect test is validated by simulation and the
combination is not, and `tan(pi * (0.5 - P))` diverges at both ends, so a
p-value near 1 dominates the combination exactly as a tiny one does and can
mask a significant effect elsewhere. Suppression at the print rather than
removal from the object is deliberate — the plot-data contract does not move,
and the study can run against shipped objects. Methods SHALL exist for
`result.goldfish` (single submodel) and for the specification-based fit
(per-block tests plus joint omnibus). On a flavored specification fit the
blocks are per process (fid) × submodel: each process is tested exactly as a
single-model fit, the printed report is the **per-block individual tests**,
and the joint omnibus combines across processes on the object. Offset
(fixed-coefficient) terms SHALL be excluded from the tested effects (their
score processes are not bridges — a fixed coefficient's score component is
not zero at the optimum); testing an offset term SHALL abort with a cli
error pointing to `test_parameter()`. No `gof()` S3 generic SHALL be
defined.

`test_gof()` SHALL accept `clock = c("event", "information")`. The
statistic `T_d` SHALL be identical under both clocks — a supremum does
not read the axis — and the documentation SHALL say so; the clock SHALL
select (a) the `u`-axis stored with the process path and (b) the
**reference distribution** from which the p-value is computed. The
default `"event"` places increment `k` at `u_k = k/n` and uses the
analytic Kolmogorov p-value (the Boschi-Wit normalization), whose
accuracy requires approximately proportional information accrual over
the event sequence — an assumption the documentation SHALL state, with
cold-start endogenous statistics named as the typical violation.
`"information"` places increment `k` at `u_k = OPG_d(k) / OPG_d(n)`
computed from cumulative outer-product (OPG) sums of the stored score
rows, and computes the p-value from a simulated reference on that
observed grid: replicated Gaussian increments with variances
`Delta u_k = s_kd^2 / OPG_d(n)`, centered to end at zero, supremum
recorded per replication — the Lin-Wei-Ying multiplier reference whose
proportional-accrual special case is the analytic Kolmogorov formula.
The simulation SHALL run from the stored scores with zero evaluation
passes, SHALL expose its replication count as an argument, and SHALL
draw through the session RNG so `set.seed()` reproduces it. The
documentation SHALL cross-reference `diagnose_onset()`'s
information-accrual curve as the diagnostic for choosing the clock — the
curve predicts in advance whether the two clocks' p-values will
separate.

#### Scenario: bridge property holds at the MLE
- **WHEN** `test_gof(fit)` runs on a converged fixture with no offset
  terms
- **THEN** each free effect's cumulative score process ends at zero within
  the convergence tolerance, and the returned object stores the full
  process paths.

#### Scenario: information clock from stored scores
- **WHEN** `test_gof(fit, clock = "information")` runs on a fit with
  stored `event_scores`
- **THEN** it completes without an evaluation pass, the process increments
  are placed at the normalized cumulative per-effect OPG information, the
  statistic equals the event-clock statistic exactly while the p-value
  comes from the simulated on-grid reference, and the bridge property at
  `u = 1` still holds.

#### Scenario: cold-start coverage on the information clock
- **WHEN** null-coverage replications run on a cold-start fixture (all
  endogenous statistics empty at onset, with the fixture's own accrual
  curve verified concentrated — near-uniform accrual would make the two
  references coincide) under both clocks
- **THEN** each replication's statistic is identical under the two
  clocks, the information-clock p-values are approximately uniform, and
  the event-clock p-values deviate conservatively (toward 1), documented
  with the fixture, as a NOT_CRAN test.

#### Scenario: null coverage
- **WHEN** the test is applied across replicated fits of correctly
  specified simulated data (fixture seeds, n in {1000, 5000})
- **THEN** effect-level p-values are approximately uniform (rejection
  proportion within simulation error of the nominal 5% level), as a
  NOT_CRAN test.

#### Scenario: power against misspecification
- **WHEN** the test is applied to fits that model a non-linear reciprocity
  data-generating process with a linear effect (fixture from the reference
  design)
- **THEN** the rejection proportion at the 5% level is materially above
  nominal, as a NOT_CRAN test.

#### Scenario: specification fit combines blocks
- **WHEN** `test_gof()` runs on a specification-based fit with rate and
  choice blocks
- **THEN** the returned object carries per-effect tests within each block, a
  per-block Cauchy omnibus, and a joint omnibus over all blocks.

#### Scenario: the printed report shows the individual tests only
- **WHEN** a `test_gof()` result is printed, on a single fit or on a
  specification fit
- **THEN** the output shows the per-effect statistics and p-values, grouped by
  block where there is more than one, and shows no omnibus value at any level,
  while `x$omnibus` and `attr(x, "context")$joint` still carry them

### Requirement: test_gof is experimental
`test_gof()` SHALL carry the `lifecycle::badge("experimental")` marker on its
documentation, in the form the package already uses on `as_goldfish()`,
`add_flavor()`, `make_specification()` and `state_at()`. The badge SHALL be
accompanied by prose naming what is provisional: the combination surface,
which is computed but unvalidated and unprinted, and the intercept row, which
extends the cited test rather than reproducing it.

#### Scenario: the badge names what is provisional
- **WHEN** the `test_gof()` documentation is read
- **THEN** it shows the experimental badge and states that the per-effect test
  is validated by simulation while the omnibus combination is not

### Requirement: the intercept is tested and documented as the baseline check
On the exact-time (Poisson) families the time intercept SHALL be tested as any
other free coefficient, and the documentation SHALL state what its row means:
its per-interval score is `dN_k - Dt_k * total_rate_k`, so its cumulative
process is the counting-process martingale `N(t) - Lambda(t)` and its test is a
test of **baseline constancy** — whether the fitted intensity reproduces the
observed event flow over time — rather than a test of an effect's functional
form. The documentation SHALL name the misspecifications it signals
(non-constant baseline, a missing global time-varying covariate, an incorrect
presence/exposure schedule, temporal clustering beyond the fitted intensity,
degenerate or tied timestamps), SHALL state that the remedies differ from those
of a covariate row, and SHALL record that the ordinal (`rate_ordered`) families
carry no intercept — it cancels in the softmax, a constant column there having
identically zero score — so no such row exists on them.

#### Scenario: the intercept row reads as the martingale residual
- **WHEN** `test_gof()` runs on an exact-time rate or REM fit with a time
  intercept
- **THEN** the intercept's stored score column equals `dN_k` minus the
  Cox-Snell compensator of the same interval, and its cumulative process is the
  counting-process martingale, zero at the maximum by the intercept score
  equation

#### Scenario: an ordinal fit has no intercept row
- **WHEN** `test_gof()` runs on a `rate_ordered` fit
- **THEN** no intercept appears among the tested effects, that family carrying
  none

### Requirement: test_parameter score test
`test_parameter()` SHALL implement the score (LM) test of the fit's
**offset (fixed-coefficient) terms** at the values their formula imposed:
it SHALL evaluate the full model's score `U` and information `I` at the
fitted estimate via `evaluate_model()` and report
`LM = t(U) %*% solve(I) %*% U` with its chi-square p-value on the tested
block's degrees of freedom (efficient-score form). The evaluation SHALL be
unconditional on fixedness, since the fitted object's own `final_score` has
the fixed components zeroed before the Newton step and therefore does not
carry the score at the offset coefficients. The evaluation SHALL therefore
require the preprocessed statistics — attached by
`estimate_*(return_preprocessed = TRUE)` or supplied through `preprocessed =`
— and SHALL abort naming both routes when it has neither, through the same
guiding error the other replay-needing diagnostics raise. The score at a fixed
coefficient SHALL NOT be stored on the fitted object to avoid that pass: one
rule for which diagnostics need the statistics is worth more than saving a
pass on one of them, and the masked score is produced inside the estimation
loop's accept/reject reset, which is not edited for a diagnostic's
convenience. Testing a candidate effect
**absent** from the formula is deferred: it requires preprocessing an
augmented model over the whole event sequence, and the documentation SHALL
name `offset(term, coef = 0)` as the way to test a candidate today — the
term enters the model held at zero, its statistics are preprocessed in the
same pass, and the resulting fit is the constrained one the test needs.
The cost difference SHALL be stated where the restriction is documented, so
the deferral reads as a route rather than a gap.
The Wald form for linear parameter combinations (restriction matrix on an
unconstrained fit) is deferred to a post-release change (2026-07-19
decision). The documentation SHALL point users to `lmtest::lrtest()` and
`lmtest::waldtest()` for nested fitted-model comparisons rather than
reimplementing them.

#### Scenario: an offset term is tested at its imposed value
- **WHEN** a model is fitted with `offset(term, coef = 0)` and
  `return_preprocessed = TRUE`, and `test_parameter()` is called on it
- **THEN** the test reports the score statistic and p-value for that term,
  computed from one evaluation pass, with no preprocessing pass run

#### Scenario: without the statistics the test says how to supply them
- **WHEN** `test_parameter()` is called on a fit carrying no preprocessed
  statistics and none is supplied
- **THEN** it aborts naming both routes — re-estimating with
  `return_preprocessed = TRUE`, or passing `preprocessed =` — rather than
  reporting a statistic from the masked score the fit does carry

#### Scenario: a candidate absent from the formula is refused with the idiom
- **WHEN** `test_parameter()` is asked to test an effect the formula does not
  contain
- **THEN** it aborts naming the `offset(term, coef = 0)` idiom and the reason
  the absent-effect form is not available

#### Scenario: score test detects an omitted effect
- **WHEN** data simulated with a nonzero reciprocity effect are fitted
  without it and `test_parameter()` tests reciprocity at the constrained
  estimate
- **THEN** the LM statistic is significant at the 5% level on the fixture
  seed, without any Newton-Raphson iterations on the full model.

#### Scenario: score test equals the quadratic form of one scoring step
- **WHEN** the LM statistic is computed on a fixture
- **THEN** it equals `t(Delta) %*% I %*% Delta` for the one-step update
  `Delta = solve(I) %*% U` within floating-point tolerance.

### Requirement: test_time trend and periods methods
`test_time()` SHALL test time heterogeneity of effects with
`method = c("trend", "periods")`. `"trend"` (default) SHALL compute scaled
Schoenfeld residuals against a time transform
(`transform = c("identity", "rank", "km")`) and report per-effect
zero-slope score tests plus a global test, from stored primitives only.
`"periods"` SHALL implement the RSiena sienaTimeTest analog as a score
test of period-dummy-by-effect interactions computed by **masking the
stored `event_scores` rows by period membership** — no preprocessing pass
and no new statistics (the period indicator is constant across the risk
set at each event, so the interaction's score contribution is the stored
row times the indicator). The first period SHALL be the reference
(interactions tested for periods 2..J). The `periods` argument SHALL
accept an integer J (split into J periods of approximately equal event
counts — the default form), a numeric vector of cut times (right-open
intervals on the event-time axis), or a length-n grouping vector/factor
(exogenous regimes). The `information` argument SHALL offer
`c("expected", "opg")`: `"expected"` (default) accumulates exact
per-event Fisher contributions period-wise inside one evaluator pass
without storing per-event matrices; `"opg"` uses period-wise
outer-product sums of the stored score rows with zero evaluation passes,
documented as a screening mode (OPG-based LM tests over-reject in finite
samples). The result SHALL include the one-step per-period coefficient
deltas as the interpretable readout. Both methods SHALL return one object
class carrying the per-effect table and the plot-ready residual/process
data. The documentation SHALL distinguish this test from testing a
windowed statistic (e.g. a `window =` effect variant), which is a memory
hypothesis routed through `test_parameter()` with preprocessing.

#### Scenario: trend test flat under time-constant effects
- **WHEN** `test_time(fit)` runs on data simulated with time-constant
  effects
- **THEN** per-effect p-values are approximately uniform across fixture
  replications (NOT_CRAN).

#### Scenario: periods method needs no preprocessing
- **WHEN** `test_time(fit, method = "periods", periods = 3,
  information = "opg")` runs on a fit with stored `event_scores` and no
  preprocessed object available
- **THEN** the test completes without an evaluation pass or replay error,
  and its block scores equal the period-wise partial sums of the stored
  score rows.

#### Scenario: periods method detects a time-varying effect
- **WHEN** data are simulated with an effect that changes between two time
  regimes and `test_time(fit, method = "periods")` is run with matching
  cut times
- **THEN** the score test for that effect's period interaction rejects at
  the 5% level on the fixture seed, and the per-period deltas have the
  simulated signs.

#### Scenario: expected and OPG information agree asymptotically
- **WHEN** both `information` options run on a large well-specified
  fixture
- **THEN** the two LM statistics agree within the documented tolerance,
  and the expected-information variant is the one reported by default.

### Requirement: effect selection by compact term strings
Effect-selecting arguments of the diagnostic functions SHALL match against
the compact term strings produced by the shared builder (the same strings
shown by the console summary and `tidy()`), with integer positions
accepted as a fallback. An exact compact-string match SHALL select that
term. A bare effect-family name matching several terms SHALL select all
matching terms in the vectorized `test_*` functions and SHALL abort in
single-series contexts with a cli error listing the matching compact
strings.

#### Scenario: family name selects all variants in test_time
- **WHEN** a model contains `inertia/net`, `inertia/net [W]`, and
  `inertia/net [W,300s]` and `test_time(fit, effects = "inertia")` is
  called
- **THEN** all three terms are tested and reported under their compact
  strings.

#### Scenario: ambiguous family name errors in a single-series context
- **WHEN** the same model is passed to a single-series diagnostic with
  `effect = "inertia"`
- **THEN** it aborts with a cli error listing the three matching compact
  term strings.

### Requirement: cli print methods for test objects
Each test function SHALL return a classed object whose `print()` method
renders via cli semantic elements (headers, bullet lists, pluralization,
data interpolation — no literal markup), reporting statistics, degrees of
freedom or process dimension, and p-values. The `test_gof()` print SHALL NOT
render the omnibus combination at any level while it is unvalidated (see the
bridge-test requirement); the other test functions render whatever
combination they define. Print output SHALL be covered by snapshot tests with a pinned
reproducible cli context.

#### Scenario: printed block summary
- **WHEN** a `test_gof()` result for a specification fit is printed
- **THEN** the output shows per-effect statistics and p-values grouped by
  block, rendered through cli, and no omnibus value.

### Requirement: diagnostics of a flavored fit map over its processes
Every `test_*` and `diagnose_*` function SHALL apply to each process of a
flavored (multi-process) specification exactly as to a single-model fit, since
the competing-flavor likelihood factorizes into independent fits. A function
returning a table SHALL provide a flavored method that row-binds the per-process
results and **appends** `flavor` and `family` columns, in the shape
`margin_table()` established, so a plot method facets on those columns rather
than needing a separate flavored plot method. A function returning a test SHALL
report per process, and SHALL combine across processes only through a declared
omnibus, never by pooling residuals, scores or scaling constants: the processes
have different effect sets and different event counts, so a shared constant
would assert a joint model that was never estimated. `test_parameter()` on a
flavored fit SHALL require no per-process candidate argument, its candidates
being the `offset()` terms each process formula already declares.

#### Scenario: a flavored diagnostic labels its rows by process
- **WHEN** `diagnose_outliers()` runs on a flavored fit
- **THEN** the result row-binds the per-process tables with `flavor` and
  `family` columns appended, and each process's statistics are computed from
  that process alone

#### Scenario: a flavored test reports per process
- **WHEN** `test_time()` runs on a flavored fit
- **THEN** it reports one result per process, and any omnibus across them is
  the declared combination rather than a pooled series

#### Scenario: flavored score test reads each process's own offsets
- **WHEN** `test_parameter()` runs on a flavored fit whose processes declare
  different `offset()` terms
- **THEN** each process is tested against its own offsets, with no candidate
  argument supplied
