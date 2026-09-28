## MODIFIED Requirements

### Requirement: Constraint atoms are non-estimated operands
Each effect atom in a `support_constraint` SHALL be parsed as an operand and seeded/updated
through the existing operand / `stat_state` registries (its existing `init_*` / `update_*`
functions), carrying `role = "constraint"` in `plan$effects`. Constraint-role atoms SHALL be
excluded from `initialStats` and from the output statistic columns while their `stat_state`
stays live across the event loop. An atom whose inputs read the availability mask / risk set
itself SHALL be rejected at parse time with a `cli` error (the mask may not depend on
itself), keeping the data flow a two-layer DAG: atoms → mask.

#### Scenario: constraint atom produces no estimated column
- **WHEN** a model with effects `~ inertia(net)` and `support_constraint = ~ tie(other)` is
  preprocessed
- **THEN** the output statistics contain exactly the `inertia(net)` column; `tie(other)`
  appears in `plan$effects` with `role = "constraint"` and contributes no column and no
  coefficient.

#### Scenario: atom state updated by the existing effect machinery
- **WHEN** an event updates the network read by a constraint atom
- **THEN** the atom's `stat_state` is updated by the same `update_*` function an estimated
  effect would use, and the mask re-evaluates at the touched cells.

#### Scenario: availability-dependent atom rejected
- **WHEN** a constraint atom's definition reads the active risk set (an availability-derived
  effect)
- **THEN** parsing aborts with a `cli` error explaining that a constraint may not depend on
  the risk set it defines.


#### Scenario: the constraint role is read, not only written
- **WHEN** a constrained model is compiled
- **THEN** its atoms appear in `plan$effects` with `role = "constraint"`, and
  the walk excludes exactly those columns from `initialStats` and from the
  output statistics by reading that role

#### Scenario: a covered constrained model is walked once
- **WHEN** a specification carrying a `support_constraint` whose atoms the
  merged walk maintains (a covered constraint — every DyNAM rate / choice /
  coordination and REM family over a constraint the shared schedule already
  walks) is preprocessed
- **THEN** the event sequence is advanced by one walk, with no second state
  container, schedule or event loop maintained for the constraint

#### Scenario: an uncovered constraint falls back to the private walk
- **WHEN** a `support_constraint` reads an object the shared substrate does not
  yet carry (an uncovered constraint — e.g. a flavored derived mask or a
  dynamic constraint-only network the units never compiled into the shared
  schedule)
- **THEN** its atoms are maintained by the private atom walk as before, without
  error, until that substrate is extended to carry them (a follow-up change)

#### Scenario: an unconstrained model's plan is unchanged
- **WHEN** a specification carrying no `support_constraint` is compiled
- **THEN** `plan$effects` holds exactly the formula's effects, with no
  constraint-role column
