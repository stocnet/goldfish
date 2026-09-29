## MODIFIED Requirements

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

`walk_open()` SHALL **assert** that its specification is generatively complete
for the variant its consumer runs (every modeled DyNAM flavor carrying its
choice, and a rate as well unless the run is time-anchored; the definition is
`multivariate-specification`'s) and SHALL abort
with a `cli` error pointing to `simulate()` / `estimate_dynes()` when it is not.
`walk_open()` SHALL NOT perform completion itself — the generative-readiness
completion transform (`multivariate-specification`) is run **once** at the
consumer entry, so the handle only ever opens an already-complete spec and a
skipped transform fails loudly rather than opening a mismatched fid set. The
handle remains an internal developer substrate (not user-exported) in this
change.

#### Scenario: evaluate between observed events
- **WHEN** a driver advances the handle to a time between two observed events
  and evaluates a coupled rate fid at parameters theta
- **THEN** it receives that fid's masked rate vector at the current process
  state without any event being written.

#### Scenario: injected sampled event updates all consumers' state
- **WHEN** the driver injects a sampled friendship creation event
- **THEN** subsequent evaluations of every fid reflect the updated friendship
  state, exactly as an observed event would.

#### Scenario: opening an incomplete spec aborts
- **WHEN** `walk_open()` is handed a half-specified flavored specification that
  skipped the completion transform (a flavor with a rate but no choice)
- **THEN** it aborts naming the incomplete flavor and pointing to `simulate()` /
  `estimate_dynes()`, rather than opening a walk with a mismatched fid set.

## ADDED Requirements

### Requirement: Every non-dependent event closes the open rate interval

The merged walk SHALL close the open interval of every exact-time rate fid
(DyNAM rate, REM) with a right-censored row at every event in the window
that is not one of that fid's dependent events: another process's
dependent event, a covariate or network event of an object a statistic
reads, a presence (`active`) change, and an event of an object only a
support constraint reads. The interval that follows SHALL be evaluated
over the state after that event, so a rate's exposure integrates presence
and support over the time each was in force, never over the state at the
interval's end. A presence change or a constraint-only object's event
SHALL NOT fold a statistic: it changes the risk set, not the statistics.
The ordered rate and the choice families, which condition on the event
alone, are unaffected.

#### Scenario: a presence change is a boundary of its own
- **WHEN** an exact-time DyNAM rate is preprocessed on nodes one of which
  leaves between two dependent events
- **THEN** the interval is split at the change by a right-censored row, the
  departing node is at risk before it and absent after it, and the fit
  equals a Poisson regression with a log-exposure offset on the frame
  built by the same rule

#### Scenario: a constraint-only object's event is a boundary
- **WHEN** an exact-time rate carries a `support_constraint` reading a
  changing layer no formula term reads, and an event of that layer gates a
  sender between two dependent events
- **THEN** the interval is split at that event by a right-censored row, the
  sender is at risk before it and gated after it, and no statistic column
  changes at the boundary
