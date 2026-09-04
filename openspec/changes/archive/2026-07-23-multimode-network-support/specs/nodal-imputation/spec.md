## ADDED Requirements

### Requirement: Imputation resolves against the state at the time of the event

A missing nodal value SHALL be replaced by a summary of that variable's **state
at the moment the value is needed**, never by a separately-derived rule per call
site. Imputation of the initial nodes table SHALL be the same rule evaluated at
the start of the observation window, not a distinct procedure. The node whose
value is being imputed SHALL be excluded from its own pool; at the initial
evaluation that exclusion is vacuous, since the node's value is missing.

An imputed value becomes part of the state and SHALL inform later imputations of
the same variable, because it is the state at that later moment.

#### Scenario: The same variable is imputed at the start and mid-walk

- **WHEN** a nodal attribute has a missing value in the `nodes` table and a later
  `replace` event carries a missing value for a different node
- **THEN** the first is imputed from the variable's state at the start of the
  window and the second from its state immediately before that event, both
  through one resolver, and the imputed first value is part of the pool the
  second is drawn from.

#### Scenario: A node does not impute from its own stale value

- **WHEN** a `replace` event carries a missing value for a node that currently
  holds an observed value
- **THEN** that node's current value is excluded from the pool the replacement is
  summarized from.

### Requirement: Imputation pools within the node's mode category

The pool a missing value is summarized from SHALL be the nodes sharing the
imputed node's **mode category** — a single value of `nodes$mode` — and SHALL NOT
pool across the mode categories a view spans. Where the nodes carry no `mode`
column, every node SHALL belong to one implicit category, reproducing the
one-mode behavior exactly.

Because a side is every node whose mode is in the declared set, a mode category
lies entirely inside a view or entirely outside it; imputation SHALL therefore
resolve identically whichever view a node is read through.

#### Scenario: A view spanning two modes imputes each separately

- **WHEN** a receiver side is declared `c("employee", "supervisor")` and a nodal
  attribute is missing for one employee and one supervisor
- **THEN** the employee's value is summarized from employees only and the
  supervisor's from supervisors only, and neither pool contains the other mode.

#### Scenario: A node read through two views imputes to one value

- **WHEN** an attribute is read on a view of `employee` alone and on a view of
  `c("employee", "supervisor")`
- **THEN** an employee's missing value resolves to the same imputed value in
  both, because the pool is its mode category in either case.

#### Scenario: One-mode data is unchanged

- **WHEN** an object carries a single mode value, or no `mode` column
- **THEN** the pool is every node of the read side, identical to the behavior
  before mode categories existed.

### Requirement: The imputation summary follows the object's recorded value type

The summary applied SHALL be selected from the object's **recorded value type**:
the arithmetic mean for a numeric variable, and the most common observed value
for a factor or character variable. The same type rule SHALL apply at every
evaluation time; a categorical variable SHALL NOT be summarized by a rule that
requires a numeric variable at any call site.

User-facing messages SHALL name this the *most common value*, reserving *mode*
for the node category.

#### Scenario: A categorical attribute changes mid-walk

- **WHEN** a factor or character nodal attribute carries a `replace` event with a
  missing value
- **THEN** the replacement is the most common observed value within the node's
  mode category, and no missing value is written into state.

#### Scenario: A missing value never enters state through imputation

- **WHEN** any imputation is applied, of either type
- **THEN** the resulting state holds no missing value at that position, so later
  comparisons against it cannot fail on an undefined condition.

### Requirement: Object metadata records value type and missingness at mapping time

The metadata pass that resolves an object's routing SHALL also record its
**value type** and whether it **carries missing values** — in the initial table
and in its event streams — without materializing state. The walk SHALL consume
those recorded facts and SHALL NOT rediscover them by scanning state or by
dispatching on the runtime class of a value.

#### Scenario: The walk does not scan for missingness

- **WHEN** preprocessing runs over an attribute recorded as carrying no missing
  values in either its initial table or its event streams
- **THEN** no imputation machinery is entered for that object, and no scan of its
  state is performed to establish that.

#### Scenario: The type decision is made once

- **WHEN** an object's value type is recorded at mapping time
- **THEN** every imputation of that object uses the summary that type selects,
  with no per-event type inspection.

### Requirement: An unimputable node aborts at schedule construction

The package SHALL abort at **schedule construction**, before the walk begins,
where a missing value would be summarized from an **empty pool** — that is, the
node's mode category has no other member, or no observed value. The abort SHALL
name the attribute, the node, and the mode category. The package SHALL NOT emit
a not-a-number value, and SHALL NOT widen the pool to another mode category as a
fallback, which would reintroduce the pooling this contract removes.

An attribute read on a mode category where it is wholly missing SHALL abort on
the same grounds: there is nothing to impute from.

Discarding missing values from a **non-empty** pool is ordinary handling and
SHALL NOT abort.

#### Scenario: A singleton mode category cannot impute

- **WHEN** a mode category contains exactly one node and an event carries a
  missing `replace` value for it
- **THEN** the package aborts at schedule construction naming the attribute, that
  node, and its mode category, rather than producing a not-a-number value part
  way through the walk.

#### Scenario: An attribute undefined on a mode category aborts

- **WHEN** an attribute is read on a mode category for which every node's value
  is missing
- **THEN** the package aborts naming the attribute and that mode category, on the
  same grounds as a wholly missing one-mode attribute.

#### Scenario: Sparse missingness is imputed, not rejected

- **WHEN** a mode category has several missing values but at least one observed
  value remains after excluding the imputed node
- **THEN** the missing values are discarded from the pool, the summary is taken
  over what remains, and no abort occurs.
