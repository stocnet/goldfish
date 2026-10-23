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

Step-size schedules SHALL offer `"fisher_scoring"` (the default), constant,
the Bottou decay schedule, and AdaGrad, Adam, and momentum at fixed
literature defaults with hyperparameters not exposed; every schedule SHALL
remain selectable through `set_sgd_options(step_schedule = )`. The optimizer
`variant` SHALL default to `"full"` for every schedule, and both `"full"` and
`"minibatch"` SHALL remain selectable under every schedule, `"fisher_scoring"`
included. The Fisher-scoring schedule SHALL step by `step_size · P⁻¹ g`, where `g` is the
tested gradient and `P` is the weighted complete-data information of the
M-step's pool at the M-step's starting parameters. An unset `step_size`
SHALL be resolved by schedule: 1 under `"fisher_scoring"`, 0.01 under
`"adagrad"` and `"adam"`, 0.001 under `"constant"`, `"decay"` and
`"momentum"`; an explicit `step_size` SHALL be used verbatim. An unset
`max_iterations` SHALL likewise be resolved by schedule: 20 under
`"fisher_scoring"`, 100 under every other schedule; an explicit
`max_iterations` SHALL be used verbatim.

Under `"fisher_scoring"` with the full variant, the M-step SHALL damp its
step: whenever an iteration's Newton-step statistic exceeds the previous
iteration's, `step_size` SHALL be halved for the rest of that M-step, and the
iteration SHALL continue from its current parameters (no backtracking), with
no lower bound on the step beyond the iteration cap. No other schedule, and
not the minibatch variant, SHALL damp.

The optimizer SHALL request the information exactly once per M-step, at its
starting parameters, and SHALL hold `P` for every iteration of that M-step.
The same request SHALL return the per-sequence scores of the pool at those
parameters, and under the full variant the first iteration's gradient SHALL be
built from them, weighted as a score evaluation would be (importance weights,
or the resampled multiset's rows), without a further score evaluation.
The EM loop SHALL serve that request from the per-sequence harvests it
already holds at the starting parameters (each sequence's pool-entry
evaluation, and on an accepted pass the evaluation of the whole pool at the
accepted proposal), and SHALL NOT evaluate the pool to answer it; the
returned information and scores SHALL be bit-identical to those of a fresh
evaluation at the starting parameters. A reused harvest SHALL be the
evaluator's whole return, its per-sequence `degenerate` flags included.

Every iteration SHALL test its statistic at the iterate its gradient was
taken at before stepping, and SHALL step only when the test fails, so a
converged M-step returns the iterate its last gradient was evaluated at.
Under the full variant each iteration after the first SHALL evaluate every
pool member once (under resampling the multiset's gradient is read from those
rows, never one evaluation per duplicate). When the M-step returns parameters
identical to those of its last whole-pool evaluation, or to its starting
parameters, the EM loop SHALL take the pass's evaluation at the proposal
from the harvest it already holds there, and SHALL NOT evaluate the pool
again; otherwise it SHALL evaluate the pool at the proposal.
Where `P` is not positive definite, the optimizer SHALL use its
pseudo-inverse, SHALL take no Fisher-scoring step and run no convergence test
along its null space, and SHALL report the M-step as rank-deficient.

Convergence SHALL default to a Newton-step test in standard-error units for
every schedule and both variants: with `g` the exact gradient under the full
variant, or an exponential moving average of batch gradients (smoothing
constant fixed internally) under minibatch, the statistic
`max_j |(P⁻¹ g)_j| / sqrt((P⁻¹)_jj)` SHALL be compared with `sgd_tolerance`,
whose default SHALL be 0.05 and which SHALL be expressed in standard-error
units. The test SHALL NOT be a norm of the raw summed score. A fixed-iteration
budget SHALL remain the opt-out mode, in which the M-step does not test
`sgd_tolerance` (the value is ignored) and supplying it does not warn. The M-step tolerance SHALL be named `sgd_tolerance`,
distinct from `set_algorithm_em()`'s EM-stop `tolerance` (a different scale —
standard-error units vs Q-scale).

The optimizer SHALL return its iteration count, its convergence flag, the
Newton-step statistic at its starting parameters, the final Newton-step
statistic, its rank-deficiency flag, and its count of step halvings, and the
EM loop SHALL record all six in that pass's `em_trace` row as
`mstep_iterations`, `mstep_converged`, `mstep_start_newton_se`,
`mstep_newton_se`, `mstep_rank_deficient` and `mstep_damped`. The
`em_trace` SHALL hold one row per decision pass and SHALL NOT hold a
per-iteration history of the M-step. An M-step exiting on its iteration cap
with the Newton-step criterion in force but unmet SHALL NOT warn per pass;
the EM loop SHALL instead warn once per run, at the first such pass, with
condition class `goldfish_abem_mstep_unconverged`, naming that pass's count of
step halvings when it is non-zero, and estimation SHALL continue.

Optimizer accumulators SHALL reset at every M-step. The SGD loop SHALL
request score-only evaluation for its minibatch batches; Fisher SHALL be
retained only where consumed: the pool-entry and accepted-proposal harvests
that serve the information request, and the full variant's whole-pool
evaluations that may serve the evaluation at the proposal.
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
  starting parameters, and every minibatch batch evaluation requests scores
  only.

#### Scenario: the information request evaluates nothing
- **WHEN** a DyNES fit runs several passes, including a rejected pass whose
  retry grows the pool, an accepted pass, and an ESS-guard redraw
- **THEN** no pool evaluation is made to answer any information request, and
  every returned information matrix and score matrix is bit-identical to a
  fresh evaluation of the whole pool at that M-step's starting parameters.

#### Scenario: the proposal evaluation reuses the M-step's last
- **WHEN** a full-variant M-step converges after stepping, or passes its test
  at its starting parameters
- **THEN** the pass makes no pool evaluation at the proposal, and its
  whole evaluation at the proposal (log-likelihoods, scores, information and
  `degenerate` flags) is bit-identical to a fresh evaluation there; when the M-step exits on its iteration cap,
  the pool is evaluated at the proposal once.

#### Scenario: the test comes before the step
- **WHEN** an M-step starts at parameters whose statistic is already below
  `sgd_tolerance`
- **THEN** it returns those parameters unchanged, converged, after one
  iteration.

#### Scenario: the first full-variant gradient reuses the information request's scores
- **WHEN** an M-step runs under `variant = "full"`, with importance weights or
  with a resampled multiset
- **THEN** its first gradient equals the gradient a fresh score evaluation at
  the starting parameters would give, and no score evaluation is made at the
  starting parameters beyond the information request. Under
  `variant = "minibatch"` the returned scores are not used.

#### Scenario: every schedule remains selectable
- **WHEN** `set_sgd_options(step_schedule = s)` is called for `s` in
  `"constant"`, `"adagrad"` and `"adam"` with `step_size` and
  `max_iterations` unset
- **THEN** the control carries schedule `s` with `step_size` 0.001, 0.01 and
  0.01 respectively and `max_iterations` 100, and the M-step ascends the
  analytic stub under each.

#### Scenario: the Fisher-scoring cap is tighter
- **WHEN** `set_sgd_options()` is called with `max_iterations` unset under
  `"fisher_scoring"`, and again with `max_iterations = 50L`
- **THEN** the first control carries `max_iterations` 20 and the second 50.

#### Scenario: a diverging Fisher-scoring step is damped
- **WHEN** a full-variant `"fisher_scoring"` M-step runs on an analytic
  objective whose information grows like `exp(theta)`, from a start where the
  undamped step overshoots so that the statistic rises
- **THEN** `step_size` is halved at each rise, the M-step continues from its
  current parameters, it ends converged within the iteration cap, and its
  result reports the number of halvings; the same objective under
  `variant = "minibatch"` or any other schedule never halves.

#### Scenario: both variants are selectable under every schedule
- **WHEN** `set_sgd_options(step_schedule = s, variant = v)` is called for
  every schedule `s` and for `v` in `"full"` and `"minibatch"`
- **THEN** the control carries both, and the M-step ascends the analytic stub
  under each pair; an unset `variant` resolves to `"full"` under every
  schedule.

#### Scenario: the M-step verdict is in the trace
- **WHEN** a DyNES fit completes at least one decision pass
- **THEN** every `em_trace` row carries `mstep_iterations`,
  `mstep_converged`, `mstep_start_newton_se`, `mstep_newton_se`,
  `mstep_rank_deficient` and `mstep_damped` for that pass's M-step.

#### Scenario: an unmet stop rule warns once per run
- **WHEN** the M-step exits on its iteration cap with the Newton-step
  criterion in force and unmet on more than one pass of a run
- **THEN** exactly one warning of class `goldfish_abem_mstep_unconverged` is
  raised for the run, the passes are flagged `mstep_converged = FALSE` in the
  `em_trace`, and estimation continues (the EM accept test governs
  correctness).
