## MODIFIED Requirements

### Requirement: Constraint atoms are non-estimated operands
Each effect atom in a `support_constraint` SHALL be parsed as an operand and seeded/updated
through the existing operand / `stat_state` registries (its existing `init_*` / `update_*`
functions), carrying `role = "constraint"` in `plan$effects`. Constraint-role atoms SHALL be
excluded from `initialStats` and from the output statistic columns while their `stat_state`
stays live across the event loop. An atom whose inputs read the availability mask / risk set
itself SHALL be rejected at parse time with a `cli` error (the mask may not depend on
itself), keeping the data flow a two-layer DAG: atoms → mask. Every object a
constraint atom reads SHALL be carried by the shared walk: an object no
formula term reads joins the shared registry after the units' objects and
its event stream joins the joint schedule after the unit streams, so that
no constraint is maintained by a private state container, schedule or event
loop, on the batch walk or on the stepping handle. A support mask SHALL read
its atoms strictly before the event's stamp on every path: an atom-object
event at a dependent event's own stamp is not in force for that event's
mask, on the batch walk and on the handle alike.

#### Scenario: the constraint role is read, not only written
- **WHEN** a constrained model is compiled
- **THEN** its atoms appear in `plan$effects` with `role = "constraint"`, and
  the walk excludes exactly those columns from `initialStats` and from the
  output statistics by reading that role

#### Scenario: every constrained model is walked once
- **WHEN** a specification carrying a `support_constraint` is preprocessed —
  over an object the formulas read, a static allowed network, a derived
  flavor mask, or a changing object no formula term reads
- **THEN** the event sequence is advanced by one walk, with no second state
  container, schedule or event loop maintained for the constraint

#### Scenario: a constraint-only object is threaded without moving anything else
- **WHEN** a specification whose `support_constraint` reads a changing
  object no formula term reads is preprocessed
- **THEN** that object's registry id follows every unit object's, its
  stream follows every unit stream, no engine folds a statistic from it and
  no right-censoring row is emitted for its events, and an unconstrained or
  already-covered specification preprocesses byte-identically to before

#### Scenario: the mask lags a tied atom event on both paths
- **WHEN** an atom-object event shares a dependent event's stamp and
  precedes it in schedule order
- **THEN** the batch mask and the handle's live mask for that dependent
  event both exclude the tied atom event, and batch-vs-replay equality holds

#### Scenario: an unconstrained model's plan is unchanged
- **WHEN** a specification carrying no `support_constraint` is compiled
- **THEN** `plan$effects` holds exactly the formula's effects, with no
  constraint-role column
