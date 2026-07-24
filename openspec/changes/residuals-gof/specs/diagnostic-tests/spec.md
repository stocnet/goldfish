# diagnostic-tests

Goodness-of-fit and specification tests: `test_gof()`, `test_parameter()`,
`test_time()`.

## ADDED Requirements

### Requirement: test_gof cumulative-score bridge test
`test_gof()` SHALL implement the Boschi-Wit martingale-residual test for
fixed-linear-effect models from stored `event_scores`: per effect, the
standardized cumulative score process
`W_d(u) = J_d^{-1/2} n^{-1/2} cumsum(s_kd)` with `J_d = I_dd / n` — the
**average per-event** observed information at the estimate, not the total
(the `n^{-1/2}` normalization requires the per-event scale; the empirical
variance of centered contributions is the fallback estimate of the same
per-event quantity), the statistic `T_d = sup_u |W_d(u)|`, and the
analytic Kolmogorov p-value
`p(t) = 2 * sum_{j>=1} (-1)^{j-1} exp(-2 j^2 t^2)`.
Effect-level p-values SHALL be combined per submodel block and jointly via
the Cauchy combination `T_o = mean(tan(pi * (0.5 - P_l)))` with
`p = 1/2 - atan(T_o)/pi`. Methods SHALL exist for `result.goldfish`
(single submodel) and for the specification-based fit (per-block tests
plus joint omnibus). On a flavored specification fit the blocks are per
process (fid) × submodel: each process is tested exactly as a single-model
fit and the joint omnibus combines across processes. Offset
(fixed-coefficient) terms SHALL be excluded from the tested effects (their
score processes are not bridges — a fixed coefficient's score component is
not zero at the optimum); testing an offset term SHALL abort with a cli
error pointing to `test_parameter()`. No `gof()` S3 generic SHALL be
defined.

`test_gof()` SHALL accept `clock = c("event", "information")`. The default
`"event"` places increment `k` at `u_k = k/n` (the Boschi-Wit
normalization), whose Brownian-bridge null requires approximately
proportional information accrual over the event sequence — an assumption
the documentation SHALL state, with cold-start endogenous statistics named
as the typical violation. `"information"` places increment `k` at
`u_k = I_d(k) / I_d(n)` computed from cumulative outer-product (OPG) sums
of the stored score rows — the martingale time change that restores the
bridge limit under non-uniform accrual — with zero evaluation passes. The
documentation SHALL cross-reference `diagnose_onset()`'s
information-accrual curve as the diagnostic for choosing the clock.

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
  are placed at the normalized cumulative per-effect OPG information, and
  the bridge property at `u = 1` still holds.

#### Scenario: cold-start coverage on the information clock
- **WHEN** null-coverage replications run on a cold-start fixture (all
  endogenous statistics empty at onset) under both clocks
- **THEN** the information-clock p-values are approximately uniform, and
  any event-clock deviation from uniformity is documented with the
  fixture, as a NOT_CRAN test.

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
- **THEN** the result reports per-effect tests within each block, a
  per-block Cauchy omnibus, and a joint omnibus over all blocks.

### Requirement: test_parameter score test
`test_parameter()` SHALL implement the score (LM) test of candidate effect
blocks: given a constrained fit and the candidate effects, it SHALL
evaluate the full model's score `U` and information `I` at the constrained
estimate via `evaluate_model()` (statistics via the diagnostic-primitives
replay rules) and report `LM = t(U) %*% solve(I) %*% U` with its chi-square
p-value on the tested block's degrees of freedom (efficient-score form).
The Wald form for linear parameter combinations (restriction matrix on an
unconstrained fit) is deferred to a post-release change (2026-07-19
decision). The documentation SHALL point users to `lmtest::lrtest()` and
`lmtest::waldtest()` for nested fitted-model comparisons rather than
reimplementing them.

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
freedom or process dimension, p-values, and the omnibus combination where
applicable. Print output SHALL be covered by snapshot tests with a pinned
reproducible cli context.

#### Scenario: printed omnibus summary
- **WHEN** a `test_gof()` result for a specification fit is printed
- **THEN** the output shows per-effect statistics and p-values grouped by
  block, and the joint Cauchy omnibus p-value, rendered through cli.
