## ADDED Requirements

### Requirement: estimate_dynes runs the ascent-based Monte Carlo EM loop

The package SHALL provide `estimate_dynes(spec, control_algo = set_algorithm_em(...))`,
where `spec` is a `make_joint_specification()` object (`make-multivariate-spec`),
estimating the specification's concatenated parameter vector by ascent-based
Monte Carlo EM: per iteration, augment or refresh a pool of endpoint-hitting
sequences via the configured augmenter, evaluate E-step quantities on the pool
at the current parameters via the evaluator contract, take an M-step via the
optimizer contract, and decide accept/grow/stop from Q and its standard error
— with no branching on algorithm variants inside the loop beyond contract
dispatch. The initial parameter vector SHALL come from `set_algorithm_em()`'s
`initial_parameters` — a `goldfishParams` (`joint-parameters`) whose free
(non-offset `NA`) slots are the estimand, default `NULL` → zero over the free
set; fixed effects are the specification's formula offsets and are not
estimated. When `set_augmenter_options(warm_start = TRUE)` is set and
`initial_parameters` is not supplied, θ₀ SHALL instead be the concatenation of
per-fid independent internal `set_algorithm_newton()` fits (their default
arguments) on a single augmentation, each fit reading its sub-specification's
offsets; when `initial_parameters` is also supplied the warm start SHALL be
warned and ignored. A per-fid warm-start fit that fails (e.g. a singular
information matrix on the single drawn sequence) SHALL fall back to zero over
that fid's free set with a cli warning naming the fid, without aborting the run
or affecting the other fids' fitted starts. Exhausting `max_retries` within one iteration SHALL abort
with a cli error explaining the non-convergence from the trace-derived
diagnostics; exhausting the outer `max_iterations` SHALL instead return the last
accepted parameters with a non-convergence **warning** and a `converged = FALSE`
flag on the result (never silent, never discarded). `estimate_dynes()` SHALL be
marked experimental (`lifecycle::badge("experimental")`). `estimate_dynes()`
SHALL be implemented and tested in this change through a contract-conformant
stub augmenter, but SHALL NOT be exported here (no `@export`, absent from
`NAMESPACE`); the export is added by `dynes-augmentation` once a real augmenter
exists, so no exported surface can be called with real data that only aborts for
lack of an augmenter. The control constructors
(`set_algorithm_em()`/`set_augmenter_options()`/`set_weights_options()`/
`set_sgd_options()`) SHALL export here.

#### Scenario: toy ascent converges through the contracts
- **WHEN** `estimate_dynes()` runs on a toy fixture with a
  contract-conformant augmenter and evaluator from a perturbed starting vector
- **THEN** the loop accepts ascent steps, stops by the stop rule, and returns
  estimates within the fixture's recovery bounds.

#### Scenario: within-iteration retry exhaustion aborts with diagnosis
- **WHEN** the ascent lower bound stays negative through `max_retries`
  pool-growth retries within one EM iteration
- **THEN** estimation aborts with a cli error explaining the non-convergence.

#### Scenario: outer iteration-budget exhaustion returns the last accepted fit
- **WHEN** the loop is still accepting ascent steps when it reaches
  `max_iterations`
- **THEN** it returns the last accepted parameters with a non-convergence
  warning and `converged = FALSE` on the result, rather than aborting.

### Requirement: set_algorithm_em() and its nested constructors control the ABEM algorithm

The package SHALL provide four control constructors, one per algorithm
concern: `set_algorithm_em()` (the EM loop: `n_sequences` — the initial pool
size, default 100, `max_iterations`, the
`accept_quantile`/`growth_quantile`/`stop_quantile` stop-rule quantiles,
`tolerance`, `stop_count`, `max_retries`, `max_pool` (default 5000, the ceiling
clamping the post-acceptance pool-sizing rule), the single `seed` governing all draws,
`initial_parameters` — θ₀ as a `goldfishParams` (`joint-parameters`) whose
free (non-offset `NA`) slots are the estimand, default `NULL` → zero over the
free set, flowing to both the evaluator and the warm-start; there SHALL be no
`fixed_parameters` argument, the fixed set being the specification's formula
offsets — `em_trace_se`, `n_cores`), nesting `set_augmenter_options()` (routine
`mcmc`/`random`/`sim` matching the `augment_seq_*()` constructor names;
`burn_in` and `thinning` counted in sweeps; `initialize` random/sim for the
first chain's start; `initial_sequence`; `warm_start` (draw one augmentation and
seed θ₀ from an internal default-`set_algorithm_newton()` fit); the MCMC-only
`move_probs` move-type
mix), `set_weights_options()` (`weighting` importance/uniform, `use`
importance/resampling, `resampling_scheme` stratified/residual/random,
`transformation`, `refresh`, `ess_threshold`), and `set_sgd_options()` (`variant`,
`batch_size`, `batch_scheme` weighted/cyclic, `step_size`, `step_schedule`
constant/decay/adagrad/adam/momentum, `max_iterations`, `sgd_tolerance` — named
distinctly from `set_algorithm_em()`'s EM-stop `tolerance`, a different scale,
`convergence` gradient/iterations). The `set_weights_options()` `transformation`
SHALL be a named enum (`identity`/`clipping`/`smoothing`), not a user-supplied
function. Argument
names SHALL be descriptive, never Greek. Each child constructor SHALL
validate its own arguments with cli errors naming the valid options;
`set_algorithm_em()` SHALL enforce the cross-object rules — the
augmenter × weighting validity matrix (uniform weighting only with the mcmc
routine and pool refresh) and the precedence table — warning and ignoring
inconsistent-but-ignorable combinations, aborting on impossible ones. As part of
these cross-object rules, `set_algorithm_em()` SHALL emit a **cold-start warning**
when `routine = "random"` is combined with a non-zero initial parameter vector
(uniform draws sit far from the target at iteration 1, risking E-step weight
collapse), recommending a model-driven routine or θ₀ = 0; the combination remains
valid (no abort) and no warning is emitted for `routine = "random"` at θ₀ = 0. The
assembled control SHALL select the augmenter, evaluator, and optimizer step
implementations through their contracts, so new variants extend the options
without modifying `estimate_dynes()`.

#### Scenario: control object selects the steps
- **WHEN** `set_algorithm_em(augmenter = set_augmenter_options(routine = "mcmc"),
  weights = set_weights_options(weighting = "importance"))` is passed to
  `estimate_dynes()`
- **THEN** the ABEM loop runs the mutation augmenter with importance-weighted
  E-steps, with no branching inside `estimate_dynes()` beyond contract
  dispatch.

#### Scenario: invalid option aborts
- **WHEN** `set_augmenter_options(routine = "exact")` is called
- **THEN** a cli error lists the valid routines.

#### Scenario: invalid cross-object combination warns and is corrected
- **WHEN** `set_algorithm_em(augmenter = set_augmenter_options(routine = "random"),
  weights = set_weights_options(weighting = "uniform"))` is constructed
- **THEN** a cli warning explains that uniform weighting requires the mcmc
  routine with pool refresh, and importance weighting is used instead.

#### Scenario: cold-start warning on random draws from a non-zero θ₀
- **WHEN** `set_algorithm_em(augmenter = set_augmenter_options(routine = "random"))`
  is constructed with a non-zero `initial_parameters` (or
  `set_augmenter_options(warm_start = TRUE)`)
- **THEN** a cli warning recommends a model-driven routine or θ₀ = 0, and
  construction still succeeds; the same construction at θ₀ = 0 emits no warning.

### Requirement: ABEM results carry Fisher-based vcov and Monte-Carlo error

The `estimate_dynes()` result SHALL contain the parameter estimates, a
`converged` flag, the Fisher-information approximation, Monte-Carlo standard
errors, and the `em_trace` per-iteration
diagnostics: parameters, Q and its standard error, the accept/grow/stop
decision, pool size and new draws, effective sample size, the weighting scheme
in force, MCMC chain diagnostics (acceptance rate, autocorrelation index), and
— opt-in via `em_trace_se` — per-iteration parameter standard errors. `coef()`
and `vcov()` SHALL be **flat** (a named vector and matrix over the free
parameters, named by the `joint-parameters` composite labels); fixed effects
are the specification's formula offsets and do not enter the free parameter
vector, their display padding reusing the existing `GetFixed()` /
`stats::.vcov.aliased()` machinery. `summary()` SHALL display asymptotic and
Monte-Carlo error side by side, grouping the flat coefficients into per-process
blocks via `coef_layout()` and surfacing the `converged` flag; `print()` /
`summary()` SHALL render via cli semantic elements, stable under a pinned cli
context for snapshot tests.

The Fisher approximation and both error estimates SHALL come from a single
dedicated full-pool evaluation at the returned parameters, run on **every
returning termination regardless of the `converged` flag** — including the
`max_iterations`-exhaustion path that returns `converged = FALSE`. A returned
fit SHALL therefore always carry a populated `vcov()` and Monte-Carlo error; a
non-converged result SHALL have standard errors (which `summary()` caveats
beside the `converged = FALSE` status), never a `NULL` covariance. Only the
`max_retries`-abort path produces no return and thus no such pass.

#### Scenario: em_trace records the ascent
- **WHEN** a converged result is inspected
- **THEN** its `em_trace` holds one row per EM iteration with the ascent
  decision, Q and its standard error, pool bookkeeping, and effective sample
  size.

#### Scenario: vcov from the Fisher approximation
- **WHEN** `vcov()` is called on a converged `estimate_dynes()` result
- **THEN** it returns the inverse of the accumulated Fisher approximation over
  the free parameters, named by the composite labels, with offset (fixed)
  effects padded via the existing aliased-vcov machinery.

#### Scenario: MC error is visible
- **WHEN** `summary()` renders a DyNES result estimated with a small pool
- **THEN** the Monte-Carlo standard errors appear alongside the asymptotic
  standard errors, making pool-limited precision visible.

#### Scenario: non-converged fit still carries standard errors
- **WHEN** `vcov()` is called on a result returned after `max_iterations`
  exhaustion (`converged = FALSE`)
- **THEN** it returns the covariance from the final full-pool pass at the
  last-accepted parameters, not `NULL`, and `summary()` shows the standard
  errors beside the non-convergence status.

#### Scenario: warm-start fit failure falls back to zero for that fid
- **WHEN** `warm_start = TRUE` and one fid's internal `set_algorithm_newton()`
  fit fails on the single drawn sequence
- **THEN** that fid's θ₀ block falls back to zero with a cli warning naming the
  fid, the other fids keep their fitted starts, and estimation proceeds.
