## ADDED Requirements

### Requirement: estimate_dynes estimates flavored models from panel waves

The package SHALL provide `estimate_dynes()` estimating one joint multi-layer
likelihood: any number of relational-event and panel-semantics layers, each modeled
layer × flavor carrying rate and choice formulas (per the `flavored-processes`
capability), the parameter vector concatenating all modeled sub-models — with an
`algorithm` control object from `set_alg_em()`. (How the multi-layer specification
is passed is an open design question, resolved before the estimator-surface phase.)
It SHALL estimate the concatenated parameters by ascent-based
Monte Carlo EM: per iteration, augment/refresh a pool of endpoint-hitting sequences
spanning the wave span, evaluate E-step quantities on the pool at the current
parameters, and take an ascent step, until the stop rule is met. Exhausting
`max_retries` (within-iteration pool growth after rejected updates) or
`max_iterations` without convergence SHALL abort with a cli error explaining the
non-convergence from the trace-derived diagnostics. `estimate_dynes()` SHALL be
marked experimental (`lifecycle::badge("experimental")`).

#### Scenario: two-wave friendship model estimates
- **WHEN** `estimate_dynes(spec, algorithm = set_alg_em(n_sequences = 100,
  augmenter = set_alg_augment(routine = "random")))` runs on a two-wave panel
  friendship layer with creation/dissolution formulas
- **THEN** estimation returns per-flavor parameter estimates with convergence
  diagnostics, without requiring observed event times.

#### Scenario: non-convergence aborts with diagnosis
- **WHEN** the ascent lower bound stays negative through `max_retries` pool-growth
  retries within one EM iteration
- **THEN** estimation aborts with a cli error explaining the non-convergence.

#### Scenario: event-stream specification rejected
- **WHEN** `estimate_dynes()` receives a specification whose focal layer is an
  event-stream layer
- **THEN** it aborts pointing to `estimate_dynam()`/`estimate_rem()` for fully observed
  sequences.

### Requirement: Specification validation guards the DyNES estimand

`estimate_dynes()` SHALL validate the multi-layer specification before estimation:
relational-event layers and flavors may be modeled or exogenous-only (exogenous
events update state, contribute no likelihood terms, and never compete in
augmentation draws); panel-semantics layers SHALL be modeled for all their flavors
or not at all — partially modeled panel layers abort, fully unmodeled ones keep the
exogenous change-list semantics. It SHALL detect, from the formulas and the objects
they create, sub-models whose statistics read no modeled panel state: a fully
panel-independent specification SHALL abort explaining that nothing is latent and
`estimate_dynam()` covers the model; a mixed specification SHALL abort naming both
remedies — separate `estimate_dynam()` runs for the panel-independent sub-models,
or folding them as flavors of one single layer in a flavored specification.
Windowed/memory effects SHALL be accepted (at the cost of one global MCMC chain and
no wave-level parallelism). Modeled relational events before the first wave SHALL
enter as history only (informing the state at the first wave, contributing no
likelihood terms); modeled relational events after the last wave SHALL be
discarded.

#### Scenario: fully panel-independent specification redirected
- **WHEN** no modeled sub-model's statistics read any modeled panel layer
- **THEN** `estimate_dynes()` aborts explaining that `estimate_dynam()` covers this
  model.

#### Scenario: mixed specification aborts with remedies
- **WHEN** some modeled sub-models read modeled panel state and others read none
- **THEN** `estimate_dynes()` aborts naming both remedies: separate
  `estimate_dynam()` runs for the independent sub-models, or one flavored layer
  folding them.

#### Scenario: partially modeled panel layer rejected
- **WHEN** a panel layer has two flavors and the specification models only one
- **THEN** validation aborts: panel layers are modeled for all flavors or not at
  all.

### Requirement: Nested set_alg_*() constructors control the ABEM algorithm

The package SHALL provide four control constructors, one per algorithm concern:
`set_alg_em()` (the EM loop: `n_sequences`, `max_iterations`, the
`accept_quantile`/`growth_quantile`/`stop_quantile` stop-rule quantiles,
`tolerance`, `max_retries`, the single `seed` governing all draws, `em_trace_se`,
`n_cores`), nesting `set_alg_augment()` (routine `mcmc`/`random`/`sim` matching the
`augment_seq_*()` constructor names; `burn_in` and `thinning` counted in sweeps;
`initialize` random/sim for the first chain's start; `initial_sequence`; the
MCMC-only `move_probs` move-type mix), `set_alg_weights()` (`weighting`
importance/uniform, `use` importance/resampling, `resampling_scheme`
stratified/residual/random, `transformation`, `refresh`, `ess_threshold`), and
`set_alg_sgd()` (`variant`, `batch_size`, `batch_scheme` weighted/cyclic,
`step_size`, `step_schedule` constant/adagrad/adam/momentum, `max_iterations`,
`tolerance`). Argument names SHALL be descriptive, never Greek. Each child
constructor SHALL validate its own arguments with cli errors naming the valid
options; `set_alg_em()` SHALL enforce the cross-object rules — the
augmenter × weighting validity matrix (uniform weighting only with the mcmc
routine and pool refresh) and the precedence table — warning and ignoring
inconsistent-but-ignorable combinations, aborting on impossible ones. The
assembled control SHALL select the augmenter, evaluator, and optimizer step
implementations through their contracts, so new variants extend the options
without modifying `estimate_dynes()`.

#### Scenario: control object selects the steps
- **WHEN** `set_alg_em(augmenter = set_alg_augment(routine = "mcmc"),
  weights = set_alg_weights(weighting = "importance"))` is passed to
  `estimate_dynes()`
- **THEN** the ABEM loop runs the mutation augmenter with importance-weighted E-steps,
  with no branching inside `estimate_dynes()` beyond contract dispatch.

#### Scenario: invalid option aborts
- **WHEN** `set_alg_augment(routine = "exact")` is called
- **THEN** a cli error lists the valid routines.

#### Scenario: invalid cross-object combination warns and is corrected
- **WHEN** `set_alg_em(augmenter = set_alg_augment(routine = "random"),
  weights = set_alg_weights(weighting = "uniform"))` is constructed
- **THEN** a cli warning explains that uniform weighting requires the mcmc routine
  with pool refresh, and importance weighting is used instead.

### Requirement: Weight staleness guards warn and refresh

With a persistent pool (`refresh = FALSE`), the effective sample size
(ESS = 1/Σ normalized-weights²) SHALL be monitored every EM iteration. When the
guard is enabled and ESS falls below `ess_threshold` × pool size (default 0.5),
the pool SHALL be redrawn at the current parameters with a cli warning informing
that new draws are generated; when the guard is disabled but the condition is
met, a cli warning SHALL still be emitted. A non-identity weight `transformation`
combined with resampling SHALL warn that the target distribution changes. No
guard warning SHALL be emitted at startup.

#### Scenario: ESS guard fires
- **WHEN** importance weights degenerate below the enabled threshold on a
  persistent pool
- **THEN** the pool is redrawn at the current parameters and a cli warning
  reports the refresh.

#### Scenario: disabled guard still informs
- **WHEN** the guard is disabled and the ESS condition is met during a run
- **THEN** a cli warning reports the degeneracy without redrawing.

### Requirement: ABEM results carry Fisher-based vcov and Monte-Carlo error

The `estimate_dynes()` result SHALL contain the parameter estimates, the
Fisher-information approximation accumulated at convergence (inverted by `vcov()`,
with NA rows/columns for fixed parameters), Monte-Carlo standard errors, and the
`em_trace` per-iteration diagnostics: parameters, Q and its standard error, the
accept/grow/stop decision, pool size and new draws, effective sample size, the
weighting scheme in force, MCMC chain diagnostics (acceptance rate, autocorrelation
index), and — opt-in via `em_trace_se` — per-iteration parameter standard errors.
`summary()` SHALL display asymptotic and Monte-Carlo
error side by side, and `print()`/`summary()` SHALL render via cli semantic elements,
stable under a pinned cli context for snapshot tests.

#### Scenario: em_trace records the ascent
- **WHEN** a converged result is inspected
- **THEN** its `em_trace` holds one row per EM iteration with the ascent decision,
  Q and its standard error, pool bookkeeping, and effective sample size.

#### Scenario: vcov from the Fisher approximation
- **WHEN** `vcov()` is called on a converged `estimate_dynes()` result
- **THEN** it returns the inverse of the accumulated Fisher approximation with
  parameter names, NA-padded for fixed parameters.

#### Scenario: MC error is visible
- **WHEN** `summary()` renders a DyNES result estimated with a small pool
- **THEN** the Monte-Carlo standard errors appear alongside the asymptotic standard
  errors, making pool-limited precision visible.

### Requirement: Parameter recovery on simulated panels

The change SHALL include a simulation-based validation: data simulated from known
creation/dissolution parameters, observed only at wave snapshots, re-estimated by
`estimate_dynes()`, with recovery within pre-registered tolerance bounds. A reduced
deterministic-seed variant SHALL run in the test suite (skip_on_cran), and the full
study — including the identifiability assessment of separate creation/dissolution
parameters from waves — SHALL be recorded with the change.

#### Scenario: known parameters recovered
- **WHEN** the seeded recovery test runs on a small simulated two-wave panel
- **THEN** estimates fall within the pre-registered bounds of the generating
  parameters.
