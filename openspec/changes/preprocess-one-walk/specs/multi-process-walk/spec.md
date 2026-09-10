## MODIFIED Requirements

### Requirement: One merged single-clock walk serves all fids

Preprocessing of a multivariate specification SHALL walk the event sequence
once on a single clock, hosting the statistic blocks over one shared process
state. A statistic block is keyed by `stat_block = (model, sub-model family,
mode-pair)` — the sender mode for rate blocks, the `(sender mode, receiver
mode)` pair for dyad blocks — so a single-mode-pair join has exactly the two
blocks (sender-indexed and dyad-indexed) and a multi-mode-pair join has one
sender block per distinct sender mode and one dyad block per distinct mode-pair.
Per event, each block's effect statistics SHALL be computed once; consumer
routing SHALL go through a `(layer, flavor) → fid` lookup: the event is
dependent for its own `(layer, flavor)` fids, a right-censoring boundary for
every other timed rate fid regardless of layer or mode-pair, and state-only for
choice/ordered fids. Effect deduplication SHALL extend across processes within a
statistic block and SHALL NOT cross effect-dispatch families. The walk SHALL return one `goldfishStat`
object per fid, indexed by fid with the process_map attached, each passing the
engine-readiness checks.

The merged walk SHALL be the only batch preprocessing loop for every
specification whose descriptor `input_shape` is `standard`: a single-process
specification and a flavored specification SHALL preprocess through it as a
one-process join, and the package SHALL carry no separate per-axis recipe
loop. To serve those callers the walk SHALL support windowed effects (each
unit's derived windowed networks realized into the shared object registry and
their expiry streams merged into the schedule as ordinary covariate rows,
deduplicated by derived-object identity across units), an explicit
observation window (`start_time` / `end_time` applied to the shared schedule,
with every timed engine writing the closing right-censored row at the end
time), restricted opportunity sets, and every writer the preprocessing
controls select (`default`, `gather`, `db`). Single-process and flavored
specifications SHALL preprocess byte-identically to the outputs of the deleted
recipe loops, with the frozen 1e-6 coefficient baselines and the C++ goldens
as the gate.

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

#### Scenario: a windowed single-process model preprocesses on the merged walk
- **WHEN** a DyNAM-choice formula with `inertia(calls, window = 30)` is
  preprocessed through `compute_statistics()`
- **THEN** the merged walk realizes the derived windowed network and its
  expiry rows into the shared schedule, and the statistics equal the frozen
  windowed baseline to the byte.

#### Scenario: the observation window closes on the shared clock
- **WHEN** a timed rate model is preprocessed with an `end_time` later than
  the last event
- **THEN** the walk drops nothing, writes one closing right-censored row at
  the end time for every timed engine, and the intercept scalars equal the
  pre-change values.

#### Scenario: the gather and db writers run on the merged walk
- **WHEN** `compute_statistics(output = "gather")` and
  `compute_statistics(output = "db")` are called on a single-process model
- **THEN** the merged walk emits through the selected writer and the rendered
  output equals the pre-change output.
