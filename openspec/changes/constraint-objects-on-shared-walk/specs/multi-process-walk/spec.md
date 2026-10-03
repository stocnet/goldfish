## MODIFIED Requirements

### Requirement: One merged single-clock walk serves all fids

Preprocessing of a multivariate specification SHALL walk the event sequence
once on a single clock, hosting the statistic blocks over one shared process
state. A statistic block is keyed by `stat_block = (model, sub-model family,
mode-pair)` — the sender mode for rate blocks, the `(sender mode, receiver
mode)` pair for dyad blocks — so a single-mode-pair join has exactly the two
blocks (sender-indexed and dyad-indexed) and a multi-mode-pair join has one
sender block per distinct sender mode and one dyad block per distinct mode-pair.
At a stamp shared by several rows, every dependent row SHALL be evaluated
before any row of that stamp is applied, whichever process the rows belong
to, so a statistic and a support mask at a dependent event both read the
state strictly before its stamp; the schedule SHALL be ordered by time, then
dependent rows before the others, then stream order, resolved once when the
schedule is built. Per event, each block's effect statistics SHALL be
computed once; consumer
routing SHALL go through a `(layer, flavor) → fid` lookup: the event is
dependent for its own `(layer, flavor)` fids, a right-censoring boundary for
every other timed rate fid regardless of layer or mode-pair, and state-only for
choice/ordered fids. Effect deduplication SHALL extend across processes within a
statistic block and SHALL NOT cross effect-dispatch families. The shared
object registry SHALL carry every object any formula term or any support
constraint atom reads: objects read only by constraint atoms are appended
after the units' objects, their event streams are appended to the joint
schedule after the unit streams and deduplicated by stream key, an engine
with no local id for such an object folds no statistic from its rows, and
each such row is a right-censoring boundary for every timed rate fid whose
support mask reads the object; whether such a row extends the walk extent is
governed by the observation-window capability. The walk SHALL return one `goldfishStat`
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

#### Scenario: distinct mode-pairs get distinct blocks with cross-mode-pair censoring
- **WHEN** an advice process `{staff}×{director}` and a nominations process
  `{director}×{project}` are walked together with timed rates, and a nominations
  event occurs at time t
- **THEN** the walk hosts a dyad block per mode-pair (`staff×director` and
  `director×project`), the nominations rate fid records a dependent event, and
  every advice timed rate fid records a right-censored boundary at t despite the
  differing mode-pair.

#### Scenario: no cross-family deduplication
- **WHEN** `inertia(calls)` appears in a DyNAM-choice formula and a REM rate
  formula
- **THEN** each dispatch family computes its own statistic column.

#### Scenario: a tied event of another process is not read at its stamp
- **WHEN** a calls event and an emails event share a stamp, the calls
  process is planned first, and the emails choice reads `tie(calls)`
- **THEN** the emails event's statistic reads the calls layer strictly
  before the stamp, as its support mask would, and a replay that evaluates
  every dependent event of the stamp before injecting any equals the batch

#### Scenario: a constraint-only object is on the shared schedule
- **WHEN** a specification's `support_constraint` reads a changing layer
  that no effect term reads
- **THEN** that layer's events are stepped by the shared walk in schedule
  order, its registry id follows every unit object's, no statistic block
  reads it, and each of its events is a right-censored boundary for every
  timed rate fid whose support mask reads the layer, with no statistic
  column changing at that boundary

### Requirement: A stepping walk handle supports evaluation and injection

The package SHALL provide a stateful walk handle over the merged walk:
`walk_open(spec, data)` constructs it; `walk_advance(handle, t)` moves the
clock applying exogenous events up to t; `walk_evaluate(handle, fid, theta)`
returns that fid's evaluation at the current state — a rate vector for
sender-block fids, a choice matrix for dyad-block fids, with the fid's
compiled support mask applied; `walk_inject(handle, event)` applies an event
(observed or sampled) to the shared process state. The handle SHALL maintain
every support constraint the specification carries — over formula-read
objects, static allowed networks, derived flavor masks and objects read only
by constraint atoms — by advancing the shared walk's own atom stores and
recomputing each fid's mask, never by refusing a constraint or serving a mask
frozen at its seed. The handle's state at its clock SHALL include every row
stamped up to and including that clock, for every driver. A driver that
evaluates an observed event at its own stamp SHALL step strictly before the
stamp with `walk_advance(handle, t, inclusive = FALSE)`, which applies the
rows stamped before `t` and the composition up to and including `t`, SHALL
evaluate every dependent event sharing that stamp, of the same process or of
another, before injecting any of them, and SHALL apply the rows sharing the
stamp only after those events are injected; an evaluation at an observed
stamp then reads the mask over atoms strictly before its time as the batch
does, and no event stamped at `t` informs another evaluation at `t`. The
handle SHALL NOT hold atom moves in a pending set of its own: a free-running
driver evaluating for the next event after the clock reads the moves at the
clock. Replaying an observed
sequence through advance/evaluate/inject SHALL reproduce the batch
preprocessing quantities for every fid on a fixture (batch-vs-replay
equality), including on constrained fixtures whose risk sets exclude at
least one alternative.

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

#### Scenario: tied dependent events are evaluated before any is injected
- **WHEN** two observed dependent events share a stamp, of one process or of
  two, and the later one's support constraint reads the earlier one's layer
- **THEN** a driver evaluating observed stamps evaluates both over the state
  strictly before the stamp before injecting either, so the later event's
  mask does not exclude the alternative the earlier one creates at that
  stamp, and no statistic reads it there, as in the batch

#### Scenario: a constraint on an object no formula reads replays exactly
- **WHEN** a specification with `choice ~ recip` and `support_constraint = ~
  !tie(emails)`, where no term reads `emails`, is opened and replayed against
  its batch preprocessing
- **THEN** `walk_open()` succeeds, every dependent event's evaluation equals
  the batch quantities, and the compared risk sets excluded at least one
  alternative
