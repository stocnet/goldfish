## ADDED Requirements

### Requirement: A candidate sequence replays through the walk-handle substrate by schedule position

The package SHALL provide an internal driver that, given a walk handle opened
over a candidate sequence (`walk_open()`, `multi-process-walk`), applies the
handle's schedule rows in schedule order up to a requested row: an exogenous
row through `walk_advance()`, a focal-layer realization row through
`walk_inject()`, and a dependent row by bringing in the node composition in
force at its stamp without applying any value. The driver SHALL resume from
the handle's current position when called again with a later row. It SHALL NOT
modify `walk_open()`, `walk_advance()`, `walk_evaluate()`, or `walk_inject()`'s
contracts. The batch-versus-replay test oracle SHALL call this driver rather
than carry its own copy of the loop.

#### Scenario: replay stops at the requested schedule row

- **WHEN** a candidate sequence is replayed to the schedule row of its second
  candidate event's realization
- **THEN** the first candidate event has been applied, the second and every
  later one have not, and no schedule row after the requested one has been
  applied.

#### Scenario: a candidate event tied with an exogenous row keeps batch order

- **WHEN** a candidate event and an exogenous covariate row share a timestamp,
  and the candidate is replayed to that event's dependent row
- **THEN** the state read there excludes the exogenous row sharing the stamp,
  matching the batch path, which scores a dependent event before the covariate
  rows sharing its stamp.

#### Scenario: a replayed candidate sequence matches the batch path

- **WHEN** a candidate sequence identical to an observed fixture is replayed to
  each of its dependent rows in turn and every walked fid is evaluated there
- **THEN** each evaluation agrees with the batch-preprocessed path's
  materialized state at the same event within 1e-10, mirroring
  `multi-process-walk`'s own batch-vs-replay equality scenario.

#### Scenario: replay resumes forward

- **WHEN** a handle already replayed to one row is replayed to a later row
- **THEN** only the rows between the two positions are applied, and the
  resulting state equals a fresh replay to the later row.

### Requirement: A pair query returns the process's frozen pair intensity

The package SHALL provide an internal function that, at a handle's current
state, returns the intensity of one named `(process, sender, receiver)` pair,
evaluating every fid through the same evaluation steps the simulation driver
draws with, so that completed and effect-free fids are evaluated rather than
refused. For a DyNAM process the intensity SHALL be the sender's rate times the
paired choice fid's probability for the receiver given that sender, with the
choice evaluated for that sender alone. For a REM process it SHALL be the rate
fid's value for the dyad. An alternative excluded from the risk set SHALL
return its computed value, an exact zero, not an error. A query that does not
resolve — an unknown process, an unknown actor label, a query against an
ordered or coordination regime — SHALL abort with a classed error naming the
query and why it cannot be answered, rather than return `NA` or a default.

#### Scenario: a DyNAM pair returns rate times choice probability

- **WHEN** a pair query names an active, available `(sender, receiver)` pair of
  a DyNAM process
- **THEN** it returns the rate fid's value for the sender multiplied by the
  choice fid's probability for the receiver in that sender's row, each equal to
  what `walk_evaluate()` reports for the same fid at the same state.

#### Scenario: a REM pair returns the dyad's rate

- **WHEN** a pair query names an active dyad of a REM process
- **THEN** it returns that dyad's rate as the rate fid's full vector reports it
  at the same state.

#### Scenario: a completed sub-model is evaluated, not refused

- **WHEN** a pair query names a DyNAM process whose choice is a completed
  uniform default, or whose rate is a completed intercept-only rate
- **THEN** it returns the intensity computed from the completed sub-model's
  uniform or pinned inputs over the sibling fid's live support, and does not
  abort.

#### Scenario: an excluded pair returns zero, not an error

- **WHEN** a pair query names a pair excluded from the risk set at the
  evaluated state (for example, the receiver is currently unavailable)
- **THEN** it returns `0` and does not abort.

#### Scenario: an unresolvable query aborts naming what was asked

- **WHEN** a pair query names an actor label absent from the process's node
  set, a process the specification does not carry, or a process in an ordered
  or coordination regime
- **THEN** it aborts with a classed cli error naming the query and why it
  cannot be answered.

### Requirement: Rate queries against one candidate are answered in one forward pass

The package SHALL provide a query entry point taking a candidate-bound
completed specification, per-fid parameters, and a list of queries, each naming
a sequence position `after = k` (the state immediately after candidate event
`k` is applied, `k = 0` being the state at open) and a `(process, sender,
receiver)` pair. The state at position `k` SHALL include every schedule row up
to and including candidate event `k`'s realization and no row after it, even
one sharing its timestamp. The entry point SHALL answer every query from a
single opened handle, replaying forward through the positions in order, and
SHALL return one numeric intensity per query in the caller's original order.

#### Scenario: all queries share one opened handle

- **WHEN** queries at several different positions are submitted together
- **THEN** the walk handle is opened exactly once and each schedule row is
  applied at most once across the whole batch.

#### Scenario: results come back in the caller's original query order

- **WHEN** queries are submitted in an order that does not match position order
- **THEN** the returned numeric vector matches the queries' original order, one
  value per query.

#### Scenario: a position excludes later rows at the same timestamp

- **WHEN** a query asks for position `after = k` and an exogenous row shares
  candidate event `k`'s timestamp but follows it in the schedule
- **THEN** the returned intensity is computed without that exogenous row
  applied.

#### Scenario: tied candidate events are addressable one at a time

- **WHEN** two candidate events share a timestamp and queries ask for the
  positions after the first and after the second
- **THEN** the two positions are distinct states, the first including only the
  first tied event.

### Requirement: A completed specification is rebound to a candidate without recompletion

The package SHALL provide an internal helper that binds a completed joint
specification to a candidate sequence's data by replacing its data, and the
data of each of its process specifications, with the candidate, without
re-running specification construction or generative completion. The helper
SHALL abort, naming which component differs, when the candidate's `info` or
`nodes` are not identical to the specification's data.

#### Scenario: a candidate differing only in its ties is bound

- **WHEN** a completed joint specification is bound to a candidate whose `info`
  and `nodes` are identical to its data and whose ties differ
- **THEN** the bound specification carries the candidate's data at the joint
  level and on every process specification, and its process map and completed
  rates are unchanged.

#### Scenario: completed rates do not move with the candidate

- **WHEN** a pair query against a completed intercept-only rate is answered
  under two candidates that differ only in their augmented event times
- **THEN** the pinned intercept used is identical under both.

#### Scenario: a candidate with different info or nodes aborts

- **WHEN** a completed joint specification is bound to a candidate whose `info`
  or `nodes` differ from its data
- **THEN** the helper aborts with a classed cli error naming the differing
  component.

### Requirement: `make_proposal_evaluator()` honors a supplied `rate_queries` argument

`make_proposal_evaluator()` (`abmcem`) SHALL accept an optional completed joint
specification. Its closure SHALL answer a non-`NULL` `rate_queries` argument by
binding the candidate to that specification and returning the queried
intensities through the query entry point above, populating the `rates` element
of its `list(loglik, rates)` return, with parameters mapped from the flat
parameter vector to per-fid vectors through the specification's coefficient
layout. At construction it SHALL abort when the sub-models' parameter positions
do not match the specification's free coefficient layout. A non-`NULL`
`rate_queries` with no specification SHALL abort naming the missing argument.
A `NULL` or omitted `rate_queries` SHALL leave the closure's behavior
unchanged, with `rates` equal to `NULL` and no walk opened.

#### Scenario: a supplied rate_queries argument returns real intensities

- **WHEN** the closure, built with a completed joint specification, is called
  with a non-`NULL` `rate_queries` naming valid pairs
- **THEN** the returned list's `rates` element carries one numeric value per
  query, equal to the query entry point's answer at the same parameters, and
  the call does not abort.

#### Scenario: rate_queries without a specification aborts

- **WHEN** the closure, built without a specification, is called with a
  non-`NULL` `rate_queries`
- **THEN** it aborts with a message naming the missing specification argument.

#### Scenario: a mismatched parameter layout aborts at construction

- **WHEN** `make_proposal_evaluator()` is built with a specification whose free
  coefficient positions differ from the sub-models' parameter positions
- **THEN** it aborts at construction, naming the mismatch.

#### Scenario: an absent rate_queries argument is unaffected

- **WHEN** the closure is called with no `rate_queries` argument (or `NULL`)
- **THEN** the returned list's `rates` element is `NULL`, exactly as before
  this change.
