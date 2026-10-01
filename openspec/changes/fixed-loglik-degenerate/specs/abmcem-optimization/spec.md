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
