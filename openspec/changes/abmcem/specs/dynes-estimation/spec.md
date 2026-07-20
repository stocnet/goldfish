## ADDED Requirements

### Requirement: estimate_dynes runs the ascent-based Monte Carlo EM loop

The package SHALL provide `estimate_dynes(spec, algorithm = set_alg_em(...))`
estimating the specification's concatenated parameter vector by ascent-based
Monte Carlo EM: per iteration, augment or refresh a pool of endpoint-hitting
sequences via the configured augmenter, evaluate E-step quantities on the pool
at the current parameters via the evaluator contract, take an M-step via the
optimizer contract, and decide accept/grow/stop from Q and its standard error
— with no branching on algorithm variants inside the loop beyond contract
dispatch. The initial parameter vector SHALL come from
`set_estimation_opt()`'s `initial_parameters` (default zero vector, warm-start
option available). Exhausting `max_retries` or `max_iterations` without
convergence SHALL abort with a cli error explaining the non-convergence from
the trace-derived diagnostics. `estimate_dynes()` SHALL be marked experimental
(`lifecycle::badge("experimental")`).

#### Scenario: toy ascent converges through the contracts
- **WHEN** `estimate_dynes()` runs on a toy fixture with a
  contract-conformant augmenter and evaluator from a perturbed starting vector
- **THEN** the loop accepts ascent steps, stops by the stop rule, and returns
  estimates within the fixture's recovery bounds.

#### Scenario: non-convergence aborts with diagnosis
- **WHEN** the ascent lower bound stays negative through `max_retries`
  pool-growth retries within one EM iteration
- **THEN** estimation aborts with a cli error explaining the non-convergence.

### Requirement: Nested set_alg_*() constructors control the ABEM algorithm

The package SHALL provide four control constructors, one per algorithm
concern: `set_alg_em()` (the EM loop: `n_sequences`, `max_iterations`, the
`accept_quantile`/`growth_quantile`/`stop_quantile` stop-rule quantiles,
`tolerance`, `max_retries`, the single `seed` governing all draws,
`em_trace_se`, `n_cores`), nesting `set_alg_augment()` (routine
`mcmc`/`random`/`sim` matching the `augment_seq_*()` constructor names;
`burn_in` and `thinning` counted in sweeps; `initialize` random/sim for the
first chain's start; `initial_sequence`; the MCMC-only `move_probs` move-type
mix), `set_alg_weights()` (`weighting` importance/uniform, `use`
importance/resampling, `resampling_scheme` stratified/residual/random,
`transformation`, `refresh`, `ess_threshold`), and `set_alg_sgd()` (`variant`,
`batch_size`, `batch_scheme` weighted/cyclic, `step_size`, `step_schedule`
constant/adagrad/adam/momentum, `max_iterations`, `tolerance`). Argument
names SHALL be descriptive, never Greek. Each child constructor SHALL
validate its own arguments with cli errors naming the valid options;
`set_alg_em()` SHALL enforce the cross-object rules — the
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
- **THEN** the ABEM loop runs the mutation augmenter with importance-weighted
  E-steps, with no branching inside `estimate_dynes()` beyond contract
  dispatch.

#### Scenario: invalid option aborts
- **WHEN** `set_alg_augment(routine = "exact")` is called
- **THEN** a cli error lists the valid routines.

#### Scenario: invalid cross-object combination warns and is corrected
- **WHEN** `set_alg_em(augmenter = set_alg_augment(routine = "random"),
  weights = set_alg_weights(weighting = "uniform"))` is constructed
- **THEN** a cli warning explains that uniform weighting requires the mcmc
  routine with pool refresh, and importance weighting is used instead.

### Requirement: ABEM results carry Fisher-based vcov and Monte-Carlo error

The `estimate_dynes()` result SHALL contain the parameter estimates, the
Fisher-information approximation accumulated at convergence (inverted by
`vcov()`, with NA rows/columns for fixed parameters), Monte-Carlo standard
errors, and the `em_trace` per-iteration diagnostics: parameters, Q and its
standard error, the accept/grow/stop decision, pool size and new draws,
effective sample size, the weighting scheme in force, MCMC chain diagnostics
(acceptance rate, autocorrelation index), and — opt-in via `em_trace_se` —
per-iteration parameter standard errors. `summary()` SHALL display asymptotic
and Monte-Carlo error side by side, and `print()`/`summary()` SHALL render via
cli semantic elements, stable under a pinned cli context for snapshot tests.

#### Scenario: em_trace records the ascent
- **WHEN** a converged result is inspected
- **THEN** its `em_trace` holds one row per EM iteration with the ascent
  decision, Q and its standard error, pool bookkeeping, and effective sample
  size.

#### Scenario: vcov from the Fisher approximation
- **WHEN** `vcov()` is called on a converged `estimate_dynes()` result
- **THEN** it returns the inverse of the accumulated Fisher approximation with
  parameter names, NA-padded for fixed parameters.

#### Scenario: MC error is visible
- **WHEN** `summary()` renders a DyNES result estimated with a small pool
- **THEN** the Monte-Carlo standard errors appear alongside the asymptotic
  standard errors, making pool-limited precision visible.
