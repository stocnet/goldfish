## MODIFIED Requirements

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
parameters, with finite entries, SHALL NOT be degenerate: it SHALL keep its
finite log-likelihood, score, and Fisher information and its weight, because
neither its importance weight nor the pooled information it contributes to
requires its own information to be invertible. A pooled sequence whose
engine evaluation returns a non-finite log-likelihood, or a missing score or
Fisher information entry, at the requested parameters SHALL be degenerate,
with a `-Inf` log-likelihood, without aborting the whole pool evaluation.
A pooled sequence whose fixed-only sub-models contribute a non-finite
log-likelihood term (`-Inf`, `+Inf`, `NaN` or `NA`) SHALL be degenerate in
the same way, whether the engine errored or returned the non-finite value:
its log-likelihood SHALL be `-Inf`, its score and Fisher information `NA`,
and its `degenerate` flag `TRUE`. The same rules SHALL hold for a single
candidate scored outside any pool by the proposal evaluator.
Pool-entry preprocessing SHALL score each sequence under the joint
single-clock likelihood: each process's own events are dependent events for
its fids, and every other modeled process's events, on another layer or
another flavor, are state changes and right-censoring boundaries for the
timed rate fids, whose exposure runs to the end of the observation window.
Fixed-only fids SHALL be included in that likelihood.
The ABMCEM loop SHALL depend only on the evaluator contract, so a batched
evaluator implementation can replace this path with no loop changes.

#### Scenario: adapter matches direct engine evaluation
- **WHEN** a pooled sequence is evaluated at θ through the adapter
- **THEN** its log-likelihood, score, and Fisher information equal a direct
  zero-iteration engine evaluation of each fid's statistics from the
  sequence's joint preprocessing at θ.

#### Scenario: evaluation at new parameters skips preprocessing
- **WHEN** the same pool is evaluated at two parameter vectors in succession
- **THEN** preprocessing runs once per sequence (on entry), not per
  evaluation.

#### Scenario: a singular sequence keeps its weight
- **WHEN** a pooled sequence's information matrix is singular at the
  requested parameters with finite entries (e.g. an effect whose statistic is
  zero on that path)
- **THEN** the sequence is not degenerate, its log-likelihood, score and
  Fisher information are the engine's finite values, and its normalized
  weight is the one its log-likelihood and proposal density give.

#### Scenario: a non-finite evaluation does not abort the pool
- **WHEN** the engine returns a non-finite log-likelihood for one pooled
  sequence at the requested parameters
- **THEN** that sequence is degenerate with a `-Inf` log-likelihood and the
  pool evaluation completes for the remaining sequences.

#### Scenario: a multi-process likelihood carries cross-process censoring
- **WHEN** a pooled sequence of a two-layer joint specification is evaluated
  at θ
- **THEN** each timed rate fid's statistics include a right-censoring row at
  every other process's events, and its exposure ends at the end of the
  observation window rather than at its own last event.

#### Scenario: flavored processes are scored on their own events
- **WHEN** a pooled sequence with competing flavors on one layer is evaluated
  at θ
- **THEN** each flavor's log-likelihood, score, and Fisher information equal
  those of the ordinary flavored estimation path on the same sequence at the
  same θ.

#### Scenario: a non-finite fixed-only term makes the sequence degenerate
- **WHEN** a pooled sequence's fixed-only term is non-finite without the
  engine erroring (e.g. a pinned rate whose plateau is `-Inf` in a period
  that holds events)
- **THEN** the sequence is flagged `degenerate = TRUE` with a `-Inf`
  log-likelihood and `NA` score and Fisher information, its free sub-models
  are not evaluated, and the pool evaluation completes for the remaining
  sequences.

#### Scenario: a single candidate with a non-finite fixed-only term is degenerate
- **WHEN** the proposal evaluator scores a single candidate outside any pool
  and the candidate's fixed-only term is non-finite (`-Inf`, `+Inf`, `NaN` or
  `NA`)
- **THEN** the result is `degenerate = TRUE` with a `-Inf` log-likelihood and
  `NA` score and Fisher information, and no error is raised.

#### Scenario: a single singular candidate keeps a finite density
- **WHEN** the proposal evaluator scores a single candidate whose information
  matrix is singular with finite entries
- **THEN** the result is not degenerate and carries the candidate's finite
  log-likelihood, so an MCMC acceptance ratio over it is defined.

### Requirement: Fixed-term degeneracy is reported

Each pooled sequence SHALL record why it is degenerate: its preprocessing
walk failed, or a fixed-only log-likelihood term was non-finite or could not
be evaluated; in the latter case it SHALL also record the label of the
sub-model whose term failed. DyNES estimation SHALL count, across every pool
built or grown during the run, the drawn sequences and those among them
degenerate for the fixed-only cause, and when the latter count is positive
SHALL emit exactly one warning of class
`goldfish_dynes_fixed_term_degenerate` after the optimization loop, stating
both counts, naming the failing sub-models, and asking the user to report it
as a goldfish bug. Sequences degenerate only because their walk failed SHALL
NOT count toward it. The fit's `em_trace` SHALL carry an integer column
`fixed_degenerate` holding, for each pass, the number of sequences in the
current pool degenerate for the fixed-only cause.
When no sequence in the current pool has both a finite log-likelihood and a
positive weight at the current parameters, whatever made each one degenerate
and under either weighting scheme, the optimization loop SHALL
stop before evaluating the pool's ascent, the ESS guard or the M-step, under
either weighting scheme, returning its last accepted parameters with
`converged = FALSE`, and SHALL NOT raise an error. When every sequence was
lost to the fixed-only cause, the fixed-term warning SHALL state that
estimation stopped because every sequence was dropped. Otherwise DyNES
estimation SHALL emit exactly one warning of class
`goldfish_dynes_pool_degenerate` stating that estimation stopped because no
sequence of the pool had a finite likelihood; when some of the lost sequences
were lost to the fixed-only cause, the fixed-term warning SHALL still be
emitted with its count, without stating that estimation stopped.

#### Scenario: a bad pin warns once with its count
- **WHEN** a DyNES estimation runs with a pinned rate that is `-Inf` in a
  period holding events of the pooled sequences
- **THEN** exactly one warning of class
  `goldfish_dynes_fixed_term_degenerate` is emitted, and it states how many
  of the drawn sequences were dropped for a non-finite fixed-only term and
  names the pinned rate's sub-model.

#### Scenario: a fully fixed-term-degenerate pool stops unconverged
- **WHEN** every sequence in the current pool is degenerate for the
  fixed-only cause, under importance weighting or under uniform weighting
- **THEN** the loop stops without computing Q, no error is raised, the fit
  carries the last accepted parameters with `converged = FALSE`, and the
  warning states that estimation stopped because every sequence was dropped.

#### Scenario: a pool degenerate for another cause stops unconverged
- **WHEN** no sequence of the current pool has a finite log-likelihood and
  none was lost to the fixed-only cause (e.g. every walk failed, or every
  evaluation at the current parameters was non-finite), under any augmenter
  and either weighting scheme
- **THEN** the loop stops without computing Q or selecting a minibatch, no
  error is raised, the fit carries the last accepted parameters with
  `converged = FALSE`, exactly one `goldfish_dynes_pool_degenerate` warning is
  emitted, and no `goldfish_dynes_fixed_term_degenerate` warning is emitted.

#### Scenario: a pool lost to mixed causes reports each cause once
- **WHEN** no sequence of the current pool has a finite log-likelihood, some
  were lost to the fixed-only cause and the others to a failed walk
- **THEN** the loop stops unconverged, exactly one
  `goldfish_dynes_fixed_term_degenerate` warning states the fixed-only count
  without stating that estimation stopped, and exactly one
  `goldfish_dynes_pool_degenerate` warning states the stop.

#### Scenario: a walk failure does not count as fixed-term degeneracy
- **WHEN** a pooled sequence is degenerate because its preprocessing walk
  failed, and no fixed-only term is non-finite
- **THEN** its recorded cause is the walk, and it contributes neither to the
  warning's count nor to the `fixed_degenerate` trace column.

#### Scenario: the trace records the per-pass count
- **WHEN** a DyNES estimation completes
- **THEN** its `em_trace` has an integer column `fixed_degenerate`, zero on
  every pass of a run with no non-finite fixed-only term.

### Requirement: Q and its standard error dispatch on the weighting scheme

Pool evaluation SHALL return a classed E-step object identifying the weighting
scheme in force, and internal generics `compute_q()` and `compute_ase()` SHALL
dispatch on it with the scheme-correct estimators: weighted-mean variance
under importance sampling, resampling noise included under resampling, plain
mean under uniform weights. Uniform weights SHALL be equal over the pooled
sequences whose log-likelihood at the current parameters is finite and zero
on the others, and the uniform estimators SHALL be the plain mean and its
sample standard error over the positive-weight sequences only, so a
degenerate sequence cannot turn Q into `NaN`; a sequence with a finite
current log-likelihood and a non-finite log-likelihood at the proposal SHALL
still enter the mean. Under resampling, one resample per decision pass
SHALL convert the pool into an unweighted multiset feeding both the E-step
quantities and the M-step batches; under importance weighting no resample
occurs. `compute_ase()` SHALL floor a negative variance estimate at 0 (returning
ASE = 0) rather than propagating `NaN` from the square root — a negative estimate
under the resampling ratio/delta form marks the low-variance near-convergence
regime, where ASE = 0 correctly degrades the decision bounds to `|Q| < tolerance`
and `Q > 0`; the flooring SHALL be recorded per pass in the trace. The EM loop
SHALL consume only these generics —
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

#### Scenario: negative variance estimate floors to zero
- **WHEN** the resampling ASE estimator's variance term is negative (near-constant
  per-sequence log-likelihood differences across the multiset)
- **THEN** `compute_ase()` returns ASE = 0 rather than `NaN`, the pass records the
  flooring in the trace, and the decision bounds reduce to `|Q| < tolerance` and
  `Q > 0`.

#### Scenario: a degenerate sequence is left out of the uniform mean
- **WHEN** a pool under uniform weighting holds a sequence whose current
  log-likelihood is `-Inf` beside sequences with finite ones
- **THEN** that sequence has weight zero, the others share the weight equally,
  and `compute_q()` and `compute_ase()` are the plain mean and its sample
  standard error over the finite sequences, finite rather than `NaN`.

#### Scenario: a proposal with no likelihood still rejects under uniform weights
- **WHEN** under uniform weighting a sequence with a finite current
  log-likelihood has a `-Inf` log-likelihood at the proposal
- **THEN** `compute_q()` is `-Inf`.

## ADDED Requirements

### Requirement: A singular pooled information at the estimate is reported

When the weighted pooled information `I_c = sum_i w_i J_i` at the returned
estimate cannot be inverted, DyNES estimation SHALL still return the fit,
with `NA` covariance and Monte-Carlo covariance matrices, and SHALL emit
exactly one warning of class `goldfish_dynes_singular_information`. The
warning SHALL name, by their composite labels, every free coefficient whose
pooled information diagonal is not positive (`I_jj <= 0`), with no tolerance
relative to other coefficients. For a named coefficient whose returned value
equals its starting value, the warning SHALL state that it was not estimated
and stays at its starting value; for one whose returned value differs, it
SHALL state that the coefficient has no information in the final pool and
that its returned value, which it SHALL state, is not an estimate. When no
such coefficient exists the warning SHALL state that the pooled information
is singular without naming a coefficient. The fit's
parameters SHALL be left unchanged. When the optimization loop stopped
because no sequence of the pool had a finite likelihood, this warning SHALL
NOT be emitted. The per-pass standard errors of the `em_trace` SHALL NOT
warn.

#### Scenario: an effect with no variation on any path is named
- **WHEN** a DyNES estimation includes an effect whose statistic is zero on
  every pooled path
- **THEN** the run completes, the fit's covariances are `NA`, and exactly one
  `goldfish_dynes_singular_information` warning names that effect's
  coefficient and states that it stays at its starting value.

#### Scenario: a coefficient that moved is not claimed to be at its start
- **WHEN** the pooled information at the estimate has a zero diagonal for a
  free coefficient whose returned value differs from its starting value
- **THEN** the warning names that coefficient, states its returned value and
  that the value is not an estimate, and does not state that it stays at its
  starting value.

#### Scenario: an invertible pooled information does not warn
- **WHEN** some pooled sequences have singular information matrices but the
  weighted pooled information at the estimate is invertible
- **THEN** no `goldfish_dynes_singular_information` warning is emitted and
  the covariances are finite.

#### Scenario: a collinear pooled information warns without a name
- **WHEN** the pooled information at the estimate is singular and every free
  coefficient's diagonal is positive, including a diagonal that is only a
  rounding residue
- **THEN** exactly one `goldfish_dynes_singular_information` warning states
  that the pooled information is singular and names no coefficient.

#### Scenario: a stopped pool does not also warn of singular information
- **WHEN** the optimization loop stopped because no sequence of the pool had
  a finite likelihood
- **THEN** no `goldfish_dynes_singular_information` warning is emitted; the
  stop's own warning is the only report.
