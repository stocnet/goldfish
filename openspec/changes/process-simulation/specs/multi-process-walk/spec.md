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
