## ADDED Requirements

### Requirement: One merged single-clock walk serves all fids

Preprocessing of a multivariate specification SHALL walk the event sequence
once on a single clock, hosting both statistic blocks (sender-indexed and
dyad-indexed) over one shared process state. Per event, each block's effect
statistics SHALL be computed once; consumer routing SHALL go through a
`(layer, flavor) → fid` lookup: the event is dependent for its own
`(layer, flavor)` fids, a right-censoring boundary for every other timed rate
fid regardless of layer, and state-only for choice/ordered fids. Effect
deduplication SHALL extend across processes within a statistic block
(`stat_block` = model, sub-model family, statistic dims) and SHALL NOT cross
effect-dispatch families. The walk SHALL return one `preprocessed.goldfish`
object per fid, indexed by fid with the process_map attached, each passing the
engine-readiness checks; single-process and flavored specifications SHALL
preprocess byte-identically to their pre-merge outputs (frozen-baseline gate).

#### Scenario: cross-process right-censoring
- **WHEN** a phone-calls event occurs at time t in a friendship + calls
  multivariate walk with timed rates
- **THEN** the calls rate fid records a dependent event and every friendship
  timed rate fid records a right-censored boundary at t.

#### Scenario: cross-process effect computed once
- **WHEN** `tie(friendship)` appears in both processes' choice formulas
- **THEN** it occupies one dyad-block column whose updates are computed once
  and referenced by both processes' effect maps.

#### Scenario: no cross-family deduplication
- **WHEN** `inertia(calls)` appears in a DyNAM-choice formula and a REM rate
  formula
- **THEN** each dispatch family computes its own statistic column.

### Requirement: A stepping walk handle supports evaluation and injection

The package SHALL provide a stateful walk handle over the merged walk:
`walk_open(spec, data)` constructs it; `walk_advance(handle, t)` moves the
clock applying exogenous events up to t; `walk_evaluate(handle, fid, theta)`
returns that fid's evaluation at the current state — a rate vector for
sender-block fids, a choice matrix for dyad-block fids, with the fid's
compiled support mask applied; `walk_inject(handle, event)` applies an event
(observed or sampled) to the shared process state. Replaying an observed
sequence through advance/evaluate/inject SHALL reproduce the batch
preprocessing quantities for every fid on a fixture (batch-vs-replay
equality).

#### Scenario: evaluate between observed events
- **WHEN** a driver advances the handle to a time between two observed events
  and evaluates a coupled rate fid at parameters theta
- **THEN** it receives that fid's masked rate vector at the current process
  state without any event being written.

#### Scenario: injected sampled event updates all consumers' state
- **WHEN** the driver injects a sampled friendship creation event
- **THEN** subsequent evaluations of every fid reflect the updated friendship
  state, exactly as an observed event would.
