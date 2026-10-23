## MODIFIED Requirements

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
`batch_size`, `batch_scheme` weighted/cyclic, `step_size` — unset by default and
resolved per schedule; `variant` — `"full"` by default under every schedule,
`"minibatch"` selectable under every schedule, `step_schedule`
fisher_scoring/constant/decay/adagrad/adam/momentum with `"fisher_scoring"` the
default, `max_iterations` — unset by default and resolved per schedule, `sgd_tolerance` — in standard-error units, default 0.05, named
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

#### Scenario: the SGD defaults are scale-aware
- **WHEN** `set_sgd_options()` is called with no arguments
- **THEN** the control carries `step_schedule = "fisher_scoring"`,
  `step_size = 1`, `variant = "full"`, `max_iterations = 20` and
  `sgd_tolerance = 0.05`, and its
  documentation states that `sgd_tolerance` is in standard-error units and the
  trade of `variant = "minibatch"`.

## ADDED Requirements

### Requirement: A DyNES fit reports whether its estimate is at the final pool's optimum

The fit returned by `estimate_dynes()` SHALL carry `newton_se`, the largest
absolute coordinate of the exact Newton step from the returned estimate to
the optimum of its final pool's importance-weighted observed-data
log-likelihood, in standard-error units:
`max_j |(I_obs⁻¹ S)_j| / SE_j` over the free coefficients, where `S` is the
weighted mean score and `I_obs⁻¹` the covariance of the fit's final
covariance pass and `SE_j = sqrt((I_obs⁻¹)_jj)`. It SHALL be computed from
that covariance pass and SHALL NOT trigger an additional evaluation of the
pool. The fit SHALL carry `at_optimum`, `TRUE` exactly when `newton_se` is
below 0.1, a fixed threshold independent of `sgd_tolerance`, whatever the
M-step's `convergence` mode. The gate is against the importance-weighted objective of the final
pool, also when the M-step optimized a resampled multiset
(`use = "resampling"`). Both SHALL be `NA` when
`covariance_status` is not `"ok"`. `converged` SHALL keep its meaning, that
the EM stop rule was met, and SHALL NOT be set from `at_optimum`.

`glance()` SHALL return `newton_se` and `at_optimum` as columns. `print()` and
`summary()` SHALL state, via cli semantic elements and beside the `converged`
status, whether the estimate is at the final pool's optimum, as a warning
alert naming `newton_se` and the 0.1 threshold when `at_optimum` is `FALSE`.
Under `use = "resampling"` that alert SHALL add an informational bullet stating
that the gate reads the importance-weighted pool, not the resampled multiset
the M-step optimized.

#### Scenario: the gate is the exact Newton step at the estimate
- **WHEN** a fit's final covariance pass has weighted mean score `S` and
  covariance `V` with an `"ok"` status
- **THEN** `newton_se` equals `max(abs(V %*% S) / sqrt(diag(V)))` within
  1e-10, and `at_optimum` is `newton_se < 0.1`, whatever `sgd_tolerance`.

#### Scenario: no extra pool evaluation
- **WHEN** a fit is returned
- **THEN** the pool was evaluated at the returned estimate exactly as often as
  before the fit carried `newton_se`.

#### Scenario: a fit away from its optimum says so
- **WHEN** a fit's `newton_se` is at least 0.1
- **THEN** `glance()` reports `at_optimum = FALSE`, and `print()` shows a
  warning alert naming the Newton step and the 0.1 threshold, while `converged`
  still reports the EM stop rule.

#### Scenario: a resampling fit's alert names the objective it gates
- **WHEN** a fit with `use = "resampling"` reads `at_optimum = FALSE`
- **THEN** its warning alert carries an informational bullet stating that the
  gate reads the importance-weighted pool.

#### Scenario: no gate without a covariance
- **WHEN** a fit's `covariance_status` is `"singular"`, `"not_pd"` or
  `"pool_dropped"`
- **THEN** `newton_se` and `at_optimum` are `NA`, and no warning is raised for
  them.
