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
parameters SHALL yield a non-finite per-sequence result rather than aborting
the whole pool evaluation (the engine path inverts the information matrix
unconditionally); its normalized weight then vanishes and the ESS guard
absorbs the loss.
A pooled sequence whose fixed-only sub-models contribute a non-finite
log-likelihood term (`-Inf`, `+Inf`, `NaN` or `NA`) SHALL be degenerate in
the same way, whether the engine errored or returned the non-finite value:
its log-likelihood SHALL be `-Inf`, its score and Fisher information `NA`,
and its `degenerate` flag `TRUE`. The same rule SHALL hold for a single
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

#### Scenario: degenerate sequence does not abort the pool
- **WHEN** a pooled sequence has a singular information matrix at the requested
  parameters (e.g. a collinear augmentation)
- **THEN** that sequence's result is non-finite and the pool evaluation
  completes for the remaining sequences, without raising the engine's
  matrix-inversion error.

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

## ADDED Requirements

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
When every sequence in the current pool is degenerate for the fixed-only
cause, the optimization loop SHALL stop before evaluating the pool's
weights or ascent, under either weighting scheme, returning its last
accepted parameters with `converged = FALSE`; the warning SHALL then also
state that estimation stopped because every sequence was dropped.

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

#### Scenario: a walk failure does not count as fixed-term degeneracy
- **WHEN** a pooled sequence is degenerate because its preprocessing walk
  failed, and no fixed-only term is non-finite
- **THEN** its recorded cause is the walk, and it contributes neither to the
  warning's count nor to the `fixed_degenerate` trace column.

#### Scenario: the trace records the per-pass count
- **WHEN** a DyNES estimation completes
- **THEN** its `em_trace` has an integer column `fixed_degenerate`, zero on
  every pass of a run with no non-finite fixed-only term.
