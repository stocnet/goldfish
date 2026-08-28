## ADDED Requirements

### Requirement: E-step weighting follows the augmenter × weighting validity matrix

The E-step SHALL weight pooled sequences by importance weights (the default):
the model density at the current parameters over the sequence's stored log
proposal density, computed on the log scale as a likelihood ratio against each
sequence's own reference record (the parameters it was drawn under and its
log-likelihood there) — mixed-reference pools SHALL be combined as multiple
importance sampling. Uniform weighting SHALL be accepted only with the MCMC
augmentation routine and pool refresh enabled; every other combination SHALL
warn and switch to importance weighting. The weight `transformation` SHALL
apply to raw log-weights before normalization and SHALL feed both importance
estimates and resampling probabilities; a non-identity transformation combined
with resampling SHALL warn that the target distribution changes. Resampling
SHALL support stratified, residual, and random schemes. Weights SHALL be fixed
for the duration of each M-step; the pool's canonical weights change only
between EM iterations.

#### Scenario: reweighting is a likelihood ratio, not a redraw
- **WHEN** a persistent pool drawn at θ_k is evaluated at θ_{k+1}
- **THEN** each sequence's weight is recomputed from its stored reference
  record and its log-likelihood at θ_{k+1}, without generating new sequences.

#### Scenario: transformation with resampling warns
- **WHEN** a clipping transformation is configured together with
  `use = "resampling"`
- **THEN** a cli warning explains that transformed weights change the
  resampling target distribution, and the run proceeds.

### Requirement: Weight staleness guards warn and refresh

The E-step SHALL monitor the effective sample size
(ESS = 1/Σ normalized-weights²) every EM iteration with a persistent pool
(`refresh = FALSE`). When
the guard is enabled and ESS falls below `ess_threshold` × pool size (default
0.5), the pool SHALL be redrawn at the current parameters with a cli warning
informing that new draws are generated; when the guard is disabled but the
condition is met, a cli warning SHALL still be emitted. With `refresh = TRUE`
the whole pool SHALL be redrawn every EM iteration at its grown size, and
cross-iteration reweighting, the ESS guard, and staleness transformations
become inapplicable — non-default settings of those arguments SHALL warn and
be ignored. No guard warning SHALL be emitted at startup.

Distinct from the recurring guard warning, a **one-shot cold-start diagnostic**
SHALL be emitted when the startup (first-iteration) ESS falls below a hard
pathology floor strictly lower than `ess_threshold` × pool size. The diagnostic
SHALL name the proposal/target mismatch and the mitigations (a model-driven
`routine`, or θ₀ = 0) and SHALL NOT redraw or grow the pool. As a
settle-at-construction complement, `set_algorithm_em()` SHALL warn when
`routine = "random"` is combined with a non-zero initial parameter vector; the
combination remains valid (no abort) and no such warning is emitted for
`routine = "random"` at θ₀ = 0.

#### Scenario: ESS guard fires
- **WHEN** importance weights degenerate below the enabled threshold on a
  persistent pool
- **THEN** the pool is redrawn at the current parameters and a cli warning
  reports the refresh.

#### Scenario: disabled guard still informs
- **WHEN** the guard is disabled and the ESS condition is met during a run
- **THEN** a cli warning reports the degeneracy without redrawing.

#### Scenario: pathological cold start is diagnosed, not silently absorbed
- **WHEN** the first-iteration ESS falls below the cold-start pathology floor
  (e.g. `routine = "random"` under a non-zero θ₀)
- **THEN** a one-shot cli diagnostic reports the proposal/target mismatch and its
  mitigations, without redrawing or growing the pool, and distinct from the ESS
  guard warning.

#### Scenario: cold-start construction guard on random + non-zero θ₀
- **WHEN** `set_algorithm_em()` is constructed with `routine = "random"` and a
  non-zero initial parameter vector
- **THEN** a cli warning recommends a model-driven routine or θ₀ = 0, and
  construction still succeeds (no warning at θ₀ = 0).

### Requirement: Q and its standard error dispatch on the weighting scheme

Pool evaluation SHALL return a classed E-step object identifying the weighting
scheme in force, and internal generics `compute_q()` and `compute_ase()` SHALL
dispatch on it with the scheme-correct estimators: weighted-mean variance
under importance sampling, resampling noise included under resampling, plain
mean under uniform weights. Under resampling, one resample per decision pass
SHALL convert the pool into an unweighted multiset feeding both the E-step
quantities and the M-step batches; under importance weighting no resample
occurs. The EM loop SHALL consume only these generics —
adding a weighting scheme means adding methods, not editing the loop. Under
MCMC draws the ASE SHALL treat retained sequences as independent given the
thinning; an autocorrelation index SHALL be recorded in the trace with
user-facing guidance to increase thinning when it is high.

#### Scenario: scheme-specific ASE
- **WHEN** the same pool is evaluated once under importance weighting and once
  under uniform weighting (MCMC + refresh)
- **THEN** `compute_ase()` applies the weighted-variance and plain-mean
  estimators respectively, selected by dispatch alone.

#### Scenario: new scheme extends by method
- **WHEN** a new E-step class with `compute_q()`/`compute_ase()` methods is
  registered in a test
- **THEN** the EM loop runs with it unmodified.

### Requirement: SGD M-step over the concatenated parameter vector

The M-step SHALL optimize the concatenated parameter vector across all
sub-models through an optimizer contract with one SGD implementation that
**owns its inner loop**: the EM loop SHALL hand the optimizer an injected
batch-evaluator closure (current pool and score-only access baked in) plus
the normalized selection weights, and the optimizer SHALL draw batches,
request scores, and update until its stopping rule fires. Batch schemes:
`"weighted"` (default) SHALL draw batch members with replacement proportional
to the normalized weights and apply the unweighted batch mean; `"cyclic"`
SHALL rotate fixed batches deterministically and SHALL importance-weight each
within-batch gradient contribution (weight × pool-size/batch-size), so the
weighted objective is maximized under both schemes (they coincide under
uniform weights); under resampling, batches SHALL be drawn uniformly from
the resampled multiset with unweighted gradients. Step-size schedules SHALL
offer constant (the default), the Bottou decay schedule, and AdaGrad, Adam,
and momentum at fixed literature defaults with hyperparameters not exposed.
Convergence SHALL default to a gradient-norm test against the tolerance for
both variants — the exact gradient under the full variant, an exponential
moving average of batch gradients (smoothing constant fixed internally)
under minibatch — with a fixed-iteration budget as the opt-out mode, in
which the tolerance is unused and supplying it warns. Exiting on the
iteration cap with the gradient criterion in force but unmet SHALL warn.
Optimizer accumulators SHALL reset at every M-step. The SGD loop SHALL
request score-only evaluation; Fisher SHALL be computed only where consumed.
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

#### Scenario: unmet gradient criterion warns
- **WHEN** the M-step exits on its iteration cap with the gradient-norm
  criterion in force and unmet
- **THEN** a cli warning reports the unconverged M-step and estimation
  continues (the EM accept test governs correctness).

### Requirement: EM ascent control flow with bounded growth and hard failure

The EM loop SHALL run in decision passes (an iteration is its initial pass
plus any growth retries). Each pass SHALL run the M-step from the last
accepted parameters on the current pool, evaluate the full pool at the
proposal, and decide in order. FIRST the stopping rule: when
|Q + z(`stop_quantile`)·ASE| < `tolerance`, a consecutive-hit streak
increments — any miss, including on a grow pass, resets it — and when the
streak reaches `stop_count` (default 1) estimation SHALL terminate,
returning the **last accepted** parameters. THEN acceptance: the proposal is
accepted when the ascent lower bound Q − z(`accept_quantile`)·ASE is
positive, and the next pool size SHALL be
max(current, ⌈σ̂²·(z(`accept_quantile`) + z(`growth_quantile`))²/Q²⌉);
otherwise the proposal is discarded, ⌈m/k⌉ sequences are appended (m the
pool size at iteration start; k = 2, 3, … per consecutive rejection, reset
each iteration) and the pass retries, at most `max_retries` times (default
20). Exhausting `max_retries` within one iteration SHALL abort with a cli
error carrying the trace-derived diagnosis (no meaningful partial fit at a
stalled iteration). Exhausting the outer `max_iterations` while still
accepting SHALL instead **return the last accepted parameters** with a
non-convergence warning and a `converged = FALSE` flag — never a silent
result, never discarded work. A trace SHALL record one row per pass, indexed by
iteration and retry: the parameters, Q and its standard error, the decision
(accept/grow/stop-hit/stop), the streak, pool size and new draws, effective
sample size, the weighting scheme in force, and MCMC chain diagnostics when
supplied; per-iteration parameter standard errors SHALL be computed only
when opted in.

#### Scenario: rejected update grows the pool boundedly
- **WHEN** the ascent lower bound is negative for one pass
- **THEN** the pool grows by the harmonic increment, the discarded proposal
  is replaced by a fresh M-step from the last accepted parameters, and the
  trace records a "grow" decision with the new pool size.

#### Scenario: stop rule reachable from the rejection path
- **WHEN** near convergence a pass meets the stopping bound while failing
  the acceptance bound, `stop_count` times consecutively
- **THEN** estimation terminates returning the last accepted parameters
  rather than growing until retry exhaustion aborts.

#### Scenario: retry exhaustion aborts with diagnosis
- **WHEN** `max_retries` growth retries all fail within one iteration
- **THEN** estimation aborts with a cli error summarizing the trace evidence
  (Q trajectory, ESS, pool growth).

#### Scenario: iteration-budget exhaustion returns the last accepted fit
- **WHEN** the loop reaches `max_iterations` while still accepting ascent steps
- **THEN** it returns the last accepted parameters with a non-convergence
  warning and `converged = FALSE`, rather than aborting.

### Requirement: Pool evaluation through the zero-iteration engine path

The package SHALL provide an evaluator-contract implementation that computes
per-sequence log-likelihood, score, and Fisher information at any parameter
vector through the existing estimation engine at zero optimizer iterations:
each pooled sequence is preprocessed once on pool entry through the existing
pipeline, and re-evaluation at a new parameter vector reuses the stored
preprocessed statistics without re-preprocessing. Callers SHALL request only
the quantities they consume via a `what` flag. Each pooled sequence SHALL
permanently carry its reference record — the parameters it was drawn under,
its log-likelihood there, and its log proposal density — on the log scale.
A pooled sequence whose information matrix is singular at the requested
parameters SHALL yield a non-finite per-sequence result rather than aborting
the whole pool evaluation (the engine path inverts the information matrix
unconditionally); its normalized weight then vanishes and the ESS guard
absorbs the loss.
The ABMCEM loop SHALL depend only on the evaluator contract, so a batched
evaluator implementation can replace this path with no loop changes.

#### Scenario: adapter matches direct engine evaluation
- **WHEN** a pooled sequence is evaluated at θ through the adapter
- **THEN** its log-likelihood, score, and Fisher information equal a direct
  zero-iteration engine evaluation of the same sequence's model at θ.

#### Scenario: evaluation at new parameters skips preprocessing
- **WHEN** the same pool is evaluated at two parameter vectors in succession
- **THEN** preprocessing runs once per sequence (on entry), not per
  evaluation.

#### Scenario: degenerate sequence does not abort the pool
- **WHEN** a pooled sequence has a singular information matrix at the requested
  parameters (e.g. a collinear augmentation)
- **THEN** that sequence's result is non-finite and the pool evaluation
  completes for the remaining sequences, without raising the engine's
  matrix-inversion error.

### Requirement: Per-sequence work runs through one map seam

Every per-sequence loop (pool entry preprocessing, pool evaluation) SHALL run
through one internal map seam with a serial default requiring no optional
dependencies. When mirai is installed and more than one core is requested,
the seam SHALL use persistent daemons that receive the data once and only
parameters and draws per iteration, with reproducible parallel RNG streams
and a non-nested thread budget (workers × BLAS threads bounded by the core
count). The core count default SHALL respect CRAN's two-core check limit.
Serial and parallel execution SHALL produce identical results under the same
seed.

#### Scenario: serial works without mirai
- **WHEN** mirai is not installed
- **THEN** estimation runs serially with no error and no reference to the
  missing package.

#### Scenario: seed-identical across backends
- **WHEN** the same estimation runs serially and with two workers under the
  same seed
- **THEN** the resulting estimates and trace are identical.
