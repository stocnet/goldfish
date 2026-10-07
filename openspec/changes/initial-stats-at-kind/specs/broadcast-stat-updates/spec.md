## ADDED Requirements

### Requirement: A broadcast effect seeds and emits at its kind
Every broadcast-eligible effect (`alter`, `ego`, `global`, the degree
effects in their alter or ego projection, and any effect the eligibility
requirement later admits) SHALL return its initial statistic from its init
at its broadcast kind, a scalar for kind 3, a length-`n1` vector for kind 2
and a length-`n2` vector for kind 1, and SHALL return each per-event change
as entries at that kind, `(node, replace)` for kinds 1 and 2 and `(replace)`
for kind 3, never as a materialized fan-out of one row per other actor. The
walk SHALL map such an entry to one `stat_mat_broadcast` column directly.
Neither the init nor the update SHALL write the one-mode diagonal; the
diagonal rule SHALL be applied wherever a kind-shaped value is read or
expanded to the grid. The eager expansion of a kind-shaped initial value
and of a kind-shaped update stream SHALL equal, cell for cell, the dense
initial value and the expanded fan-out the effect produced before this
requirement, and that equality SHALL be a test, so the fan-out form is the
oracle and not a code path.

#### Scenario: an alter init is a vector
- **WHEN** `alter(attr)` is initialized on a one-mode DyNAM-choice model with
  `n` actors
- **THEN** the init returns a length-`n` numeric vector equal to `attr`, no
  `n × n` object is allocated by the init, and the accessor's dense slice
  equals `matrix(attr, n, n, byrow = TRUE)` with a zero diagonal

#### Scenario: an attribute change emits one entry
- **WHEN** an actor's attribute read by `alter()` changes during a stored
  event
- **THEN** the update returns one `(node, replace)` entry, the walk writes one
  `stat_mat_broadcast` column with `kind = 1`, and the memory allocated by the
  update does not grow with the number of actors

#### Scenario: the kind emission equals the fan-out
- **WHEN** the kind-shaped update stream of a dyad model with `alter()`,
  `ego()` and `indeg()` is expanded eagerly over a full event sequence
- **THEN** the expanded statistics equal, at every stored event, the
  statistics the former fan-out (`to_ego()` / `to_alter()` collapsed by the
  broadcast encoder) produced, and the estimates on every backend are
  unchanged within 1e-10

#### Scenario: every init returns the shape its kind implies
- **WHEN** the init of every effect is called for every family it supports on
  a small fixture
- **THEN** each returned `stat` has exactly the shape of the effect's
  classified broadcast kind for that family, and a mismatch fails the
  conformance test naming the effect and the family
