## MODIFIED Requirements

### Requirement: Constraint atoms are non-estimated operands
Each effect atom in a `support_constraint` SHALL be parsed as an operand and seeded/updated
through the existing operand / `stat_state` registries (its existing `init_*` / `update_*`
functions), carrying `role = "constraint"` in `plan$effects`. Constraint-role atoms SHALL be
excluded from `initialStats` and from the output statistic columns while their `stat_state`
stays live across the event loop. An atom whose inputs read the availability mask / risk set
itself SHALL be rejected at parse time with a `cli` error (the mask may not depend on
itself), keeping the data flow a two-layer DAG: atoms → mask.

#### Scenario: the constraint role is read, not only written
- **WHEN** a constrained model is compiled
- **THEN** its atoms appear in `plan$effects` with `role = "constraint"`, and
  the walk excludes exactly those columns from `initialStats` and from the
  output statistics by reading that role

#### Scenario: a constrained model is walked once
- **WHEN** a specification carrying a `support_constraint` is preprocessed
- **THEN** the event sequence is advanced by one walk, with no second state
  container, schedule or event loop maintained for the constraint

#### Scenario: an unconstrained model's plan is unchanged
- **WHEN** a specification carrying no `support_constraint` is compiled
- **THEN** `plan$effects` holds exactly the formula's effects, with no
  constraint-role column
