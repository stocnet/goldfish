## MODIFIED Requirements

### Requirement: One merged single-clock walk serves all fids

Preprocessing of a multivariate specification SHALL walk the event sequence
once on a single clock, hosting the statistic blocks over one shared process
state. A statistic block is keyed by `stat_block = (model, sub-model family,
mode-pair)` — the sender mode for rate blocks, the `(sender mode, receiver
mode)` pair for dyad blocks, **and the sender mode alone for a behavior
process's choice block** (a behavior choice submodel's risk set is the actor's
own reachable states, so it has no receiver side) — so a single-mode-pair join
has exactly the two blocks (sender-indexed and dyad-indexed) and a multi-mode-pair
join has one sender block per distinct sender mode and one dyad block per distinct
mode-pair. Per event, each block's effect statistics SHALL be computed once; consumer
routing SHALL go through a `(layer, flavor) → fid` lookup: the event is
dependent for its own `(layer, flavor)` fids, a right-censoring boundary for
every other timed rate fid regardless of layer or mode-pair, and state-only for
choice/ordered fids. Effect deduplication SHALL extend across processes within a
statistic block and SHALL NOT cross effect-dispatch families. The walk SHALL return one `preprocessed.goldfish`
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

#### Scenario: a behavior process's choice block is sender-indexed only
- **WHEN** a friendship network process (choice, dyad-indexed) is walked
  together with a smoking behavior process (choice, sender-indexed only, per
  `behavior-coevolution`) over the same `{staff}` mode
- **THEN** the walk hosts a dyad block for the friendship choice statistics and
  a separate sender-only block for the smoking choice statistics — the two are
  not merged into one dyad block, and the smoking block has no receiver-mode
  dimension.
