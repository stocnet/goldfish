## MODIFIED Requirements

### Requirement: SGD M-step over the concatenated parameter vector

The M-step SHALL optimize the concatenated parameter vector across all
sub-models through an optimizer contract with one SGD implementation that
**owns its inner loop**: the EM loop SHALL hand the optimizer an injected
batch-evaluator closure (current pool and score-only access baked in), an
injected information closure over the same pool, and the normalized
selection weights, and the optimizer SHALL draw batches, request scores, and
update until its stopping rule fires. Batch schemes:
`"weighted"` (default) SHALL draw batch members with replacement proportional
to the normalized weights and apply the unweighted batch mean; `"cyclic"`
SHALL rotate fixed batches deterministically and SHALL importance-weight each
within-batch gradient contribution (weight × pool-size/batch-size), so the
weighted objective is maximized under both schemes (they coincide under
uniform weights); under resampling, batches SHALL be drawn uniformly from
the resampled multiset with unweighted gradients. The batch size SHALL be a
fixed absolute count exposed on `set_sgd_options()`, held constant across EM
iterations even as the pool grows (the batch never tracks the pool size); its
unset default SHALL be resolved once at construction to `min(n_sequences, 10)`
and then frozen (floored at `n_sequences` so the batch never exceeds a tiny pool).

Step-size schedules SHALL offer `"preconditioned"` (the default), constant,
the Bottou decay schedule, and AdaGrad, Adam, and momentum at fixed
literature defaults with hyperparameters not exposed; every schedule SHALL
remain selectable through `set_sgd_options(step_schedule = )`. The
preconditioned schedule SHALL step by `step_size · P⁻¹ g`, where `g` is the
tested gradient and `P` is the weighted complete-data information of the
M-step's pool at the M-step's starting parameters. An unset `step_size`
SHALL be resolved by schedule: 1 under `"preconditioned"`, 0.01 under
`"adagrad"` and `"adam"`, 0.001 under `"constant"`, `"decay"` and
`"momentum"`; an explicit `step_size` SHALL be used verbatim.

The optimizer SHALL request the information exactly once per M-step, at its
starting parameters, and SHALL hold `P` for every iteration of that M-step.
Where `P` is not positive definite, the optimizer SHALL use its
pseudo-inverse, SHALL take no preconditioned step and run no convergence test
along its null space, and SHALL report the M-step as rank-deficient.

Convergence SHALL default to a Newton-step test in standard-error units for
every schedule and both variants: with `g` the exact gradient under the full
variant, or an exponential moving average of batch gradients (smoothing
constant fixed internally) under minibatch, the statistic
`max_j |(P⁻¹ g)_j| / sqrt((P⁻¹)_jj)` SHALL be compared with `sgd_tolerance`,
whose default SHALL be 0.1 and which SHALL be expressed in standard-error
units. The test SHALL NOT be a norm of the raw summed score. A fixed-iteration
budget SHALL remain the opt-out mode, in which the M-step does not test
`sgd_tolerance` (the value still sets the threshold of a fit's `at_optimum`,
see `dynes-estimation`) and supplying it does not warn. The M-step tolerance SHALL be named `sgd_tolerance`,
distinct from `set_algorithm_em()`'s EM-stop `tolerance` (a different scale —
standard-error units vs Q-scale).

The optimizer SHALL return its iteration count, its convergence flag, the
final Newton-step statistic, and its rank-deficiency flag, and the EM loop
SHALL record all four in that pass's `em_trace` row as `mstep_iterations`,
`mstep_converged`, `mstep_newton_se` and `mstep_rank_deficient`. The
`em_trace` SHALL hold one row per decision pass and SHALL NOT hold a
per-iteration history of the M-step. An M-step exiting on its iteration cap
with the Newton-step criterion in force but unmet SHALL NOT warn per pass;
the EM loop SHALL instead warn once per run, at the first such pass, with
condition class `goldfish_abem_mstep_unconverged`, and estimation SHALL
continue.

Optimizer accumulators SHALL reset at every M-step. The SGD loop SHALL
request score-only evaluation for its batches; Fisher SHALL be computed only
where consumed, which includes the one information evaluation per M-step.
The optimized vector SHALL contain only free parameters — fixed effects are the
specification's formula offsets, excluded from θ before the M-step, so there are
no in-vector fixed positions to mask.

#### Scenario: cyclic batches stay unbiased
- **WHEN** a pool with non-uniform weights is optimized under
  `batch_scheme = "cyclic"`
- **THEN** each batch gradient is importance-weighted and the ascent direction
  matches the weighted full-pool gradient in expectation.

#### Scenario: fixed effects are absent from the optimized vector
- **WHEN** an effect is fixed via an `offset()` term in its formula
- **THEN** it never appears in the optimized parameter vector, and its fixed
  value enters only through the sub-specification's preprocessed statistics.

#### Scenario: fresh optimizer state per M-step
- **WHEN** a second EM iteration starts under an adaptive schedule
- **THEN** the schedule's accumulators start from their initial values.

#### Scenario: batch size is fixed and pool-independent
- **WHEN** an unset `batch_size` is defaulted from an initial pool of
  `n_sequences` and the pool later grows
- **THEN** the batch size stays `min(n_sequences, 10)` for every iteration, not
  a fraction recomputed from the grown pool.

#### Scenario: the stop rule is invariant to the score's scale
- **WHEN** the same analytic objective is optimized twice under the default
  options, once with its score and information multiplied by a constant
  `c = 1000` (as if the pool carried 1000 times the events, standard errors
  scaled by `1 / sqrt(c)`)
- **THEN** both runs report the same convergence flag, and their final
  `mstep_newton_se` values agree within the minibatch noise of the stub.

#### Scenario: the default does not overshoot a large summed score
- **WHEN** the default M-step starts one standard error from the optimum of
  an analytic objective whose information is that of 10,000 events
- **THEN** it ends converged, within `sgd_tolerance` standard errors of the
  optimum, without passing it by more than one standard error at any
  iteration.

#### Scenario: a start at the optimum stays at the optimum
- **WHEN** the default M-step starts at the exact optimum of the pool
  objective
- **THEN** it returns converged, with parameters within `sgd_tolerance`
  standard errors of the start.

#### Scenario: a direction without information is not stepped along
- **WHEN** the pool's information is singular along one direction
- **THEN** the M-step leaves that direction's coordinate combination
  unchanged, tests convergence on the remaining directions, and reports
  `rank_deficient = TRUE`.

#### Scenario: one information request per M-step
- **WHEN** an M-step runs under any schedule with `convergence = "gradient"`
- **THEN** the injected information closure is called exactly once, at the
  starting parameters, and every batch evaluation requests scores only.

#### Scenario: every schedule remains selectable
- **WHEN** `set_sgd_options(step_schedule = s)` is called for `s` in
  `"constant"`, `"adagrad"` and `"adam"` with `step_size` unset
- **THEN** the control carries schedule `s` with `step_size` 0.001, 0.01 and
  0.01 respectively, and the M-step ascends the analytic stub under each.

#### Scenario: the M-step verdict is in the trace
- **WHEN** a DyNES fit completes at least one decision pass
- **THEN** every `em_trace` row carries `mstep_iterations`,
  `mstep_converged`, `mstep_newton_se` and `mstep_rank_deficient` for that
  pass's M-step.

#### Scenario: an unmet stop rule warns once per run
- **WHEN** the M-step exits on its iteration cap with the Newton-step
  criterion in force and unmet on more than one pass of a run
- **THEN** exactly one warning of class `goldfish_abem_mstep_unconverged` is
  raised for the run, the passes are flagged `mstep_converged = FALSE` in the
  `em_trace`, and estimation continues (the EM accept test governs
  correctness).
