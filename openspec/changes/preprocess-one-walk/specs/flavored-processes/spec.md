## MODIFIED Requirements

### Requirement: Single-pass preprocessing emits per-flavor outputs

Preprocessing of a multi-flavor specification SHALL walk the event sequence once
for all of its sub-model families together, on the merged single-clock walk:
the union of effects across all flavors' formulas is computed once per
statistic block (an effect shared by several formulas contributes one
statistics computation, referenced by each flavor's effect map), the rate and
choice families of the specification run as engines of the same walk rather
than as one walk per family, and the output is one `goldfishStat` object per
flavor and family. Event routing SHALL follow the sub-model family: on timed
rate sub-models (DyNAM-rate, REM) a dependent event of flavor g is dependent in
flavor g's output and right-censored in every other flavor's output; on ordered
and choice sub-models other-flavor events carry no right-censoring — they enter
only as process-state updates. Each flavor's derived mask flips segment that
flavor's right-censored timeline.

The driver SHALL return, from a single call, a list of `goldfishStat`
objects indexed by an integer formula id (fid), carrying a `process_map` table
attribute — columns `fid`, `layer`, `flavor`, `family` (rate/choice), `stat_block`,
`has_intercept`, `constraint_id` — as the identity authority. Support constraints
SHALL be identified by one `constraint_id` per `(layer, flavor)`, carried by that
flavor's rate and choice outputs, which SHALL resolve to the same constraint. Each
output's mask SHALL be realized against its own stored event timeline, so outputs
sharing a `constraint_id` may hold different mask sequences whenever their stored
events differ (a timed rate output carries cross-flavor right-censored rows that a
choice output does not). Human-readable labels in messages and results SHALL
be rendered from the process_map, never parsed back from list keys.

#### Scenario: fid-indexed return with process_map
- **WHEN** a two-flavor DyNAM specification with rate and choice formulas is
  preprocessed by a single driver call
- **THEN** the result is four `goldfishStat` objects indexed by fid whose
  process_map rows identify (layer, flavor, family), with creation's rate and choice
  rows sharing one `constraint_id`.

#### Scenario: one walk for both families
- **WHEN** a two-flavor DyNAM specification with rate and choice formulas is
  preprocessed
- **THEN** the event sequence is walked once, with a sender-block engine
  serving the two rate fids and a dyad-block engine serving the two choice
  fids, and the four outputs equal those of the former per-family walks to
  the byte.

#### Scenario: one constraint, per-output mask timelines
- **WHEN** creation's rate output stores its own events plus the dissolutions as
  right-censored rows, while its choice output stores only its own events
- **THEN** both outputs carry the same `constraint_id` and resolve to the same
  constraint, and each holds a mask sequence aligned with its own stored events —
  the rate output's being the longer of the two.

#### Scenario: shared effect computed once
- **WHEN** `indeg(friendship)` appears in both the creation and dissolution rate
  formulas
- **THEN** its statistic updates are computed once during the single pass and both
  flavors' outputs reference them.

#### Scenario: cross-flavor right-censoring on timed models
- **WHEN** a dissolution event occurs at time t in a two-flavor DyNAM-rate model
- **THEN** the creation output records a right-censored event at t (rate-integral
  boundary) while the dissolution output records a dependent event.

#### Scenario: choice sub-models skip cross-flavor censoring
- **WHEN** the same sequence preprocesses the DyNAM-choice sub-model
- **THEN** other-flavor events update process state only and add no right-censored
  entries to a flavor's choice output.
