## ADDED Requirements

### Requirement: A nodal attribute can be declared as a modeled behavior layer

`make_specification()` SHALL accept a `layer` naming a `nodes.goldfish` attribute
(rather than a network) as its focal process, provided the attribute's change events
are increments of exactly ±1 (matching the one-step-at-a-time restriction of a
behavior/state variable). The returned `specification.goldfish` object SHALL be
tagged as a **behavior** process (as opposed to a network/dyadic process), and SHALL
reuse the DyNAM `rate`/`choice` engine unchanged in kind: the `rate` submodel governs
when the actor's behavior becomes eligible to change, the `choice` submodel governs
which direction it moves.

#### Scenario: behavior layer specification builds
- **WHEN** a user calls `make_specification(rate = ~ 1 + shape_linear, choice = ~
  similarity_alters, model = "DyNAM", layer = "smoking", data = d)` where `smoking` is
  a `nodes.goldfish` attribute whose recorded change events are all ±1
  - **THEN** a `specification.goldfish` object is returned, tagged as a behavior
    process, with the parsed rate and choice formula components.

#### Scenario: non-±1 change events rejected
- **WHEN** `layer` names an attribute whose recorded change events include an
  increment other than +1 or −1 (e.g. a jump of 2, or an absolute `replace` to an
  arbitrary value)
- **THEN** `make_specification()` aborts with a cli error stating that a behavior
  layer's change events must be single ±1 steps, naming the offending event(s).

### Requirement: A behavior layer's choice set is its own reachable states

Preprocessing a behavior specification's `choice` submodel SHALL construct, for each
eligible actor and event, a risk set of exactly the actor's two adjacent states
(`current − 1` and `current + 1`), clamped to the attribute's declared valid range when
one bound is reached (a clamped actor's risk set has one element, not zero — reaching a
bound restricts, but does not remove, the actor's eligibility to move away from it).
This risk set SHALL NOT include any other actor (there is no "receiver" side for a
behavior choice submodel).

#### Scenario: interior actor has a two-element risk set
- **WHEN** an actor's current behavior value is strictly between the attribute's
  declared minimum and maximum at the time it becomes eligible to change
- **THEN** its choice risk set contains exactly `current − 1` and `current + 1`.

#### Scenario: boundary actor has a one-element risk set
- **WHEN** an actor's current behavior value equals the attribute's declared maximum
- **THEN** its choice risk set contains exactly `current − 1` (moving off the
  boundary); `current + 1` is excluded as out of declared range.

### Requirement: Behavior evaluation-function effects

The package SHALL provide a behavior-specific effect family for use in a behavior
layer's `rate`/`choice` formulas, dispatched through the existing
`init_DyNAM_choice.*`/`update_DyNAM_choice.*` effect contract:
- `shape_linear`: the actor's own current behavior value.
- `shape_quadratic`: the square of the actor's own current behavior value.
- `similarity_alters`: an aggregate (e.g. average) similarity between the actor's
  current behavior value and the current behavior values of its alters in a named,
  composed network layer.
- `avg_alter`: the average current behavior value among the actor's alters in a
  named, composed network layer.

`similarity_alters` and `avg_alter` SHALL read the named network layer's current tie
state at each event (the same live-state read pattern existing dyadic effects use for
attributes), not a static snapshot taken at construction.

#### Scenario: shape effects use only own state
- **WHEN** a behavior choice formula includes `shape_linear`
- **THEN** its statistic for each candidate state in the actor's risk set is that
  candidate state's numeric value, independent of any other actor.

#### Scenario: network-dependent effects track live ties
- **WHEN** `avg_alter(friendship)` is in a behavior formula and a `friendship` tie
  involving the focal actor changes before the actor's next behavior-change
  opportunity
- **THEN** the statistic computed at that next opportunity reflects the updated
  `friendship` neighborhood, not the neighborhood at specification construction.

### Requirement: A behavior process composes with a network process for coevolution

`make_joint_specification()` SHALL accept one behavior-process specification and one
or more network-process specifications over the same shared node set, per the
mode-set-identity conformance rule (see `multivariate-specification`). A composed
network process's effects (e.g. `ego()`, `alter()`, `similarity()`) MAY read the
behavior layer's current state (a selection effect); a composed behavior process's
effects (`similarity_alters`, `avg_alter`) MAY read a composed network layer's current
ties (an influence effect). Both processes in this change's scope SHALL be
event-observed (`observation = "event"`); a panel-observed behavior or network layer
in the same join is out of scope and SHALL abort naming panel-observed coevolution as
future development.

#### Scenario: selection and influence compose
- **WHEN** `make_joint_specification(friendship_spec, smoking_spec, data = x)` runs
  with `friendship_spec`'s choice formula containing `similarity(smoking)` and
  `smoking_spec`'s choice formula containing `avg_alter(friendship)`
- **THEN** a multivariate specification is returned covering both processes, and each
  process's cross-reading effect resolves against the other process's live state.

#### Scenario: panel-observed behavior layer rejected
- **WHEN** a behavior specification's focal layer has `observation = "panel"` is
  composed via `make_joint_specification()`
- **THEN** construction aborts stating panel-observed behavior coevolution is not yet
  supported, naming event-observed behavior layers as the current scope.

### Requirement: Event-observed behavior–network coevolution estimates per-process

A multivariate specification composing only event-observed processes (network and
behavior alike) SHALL be estimable per-process via `estimate_dynam()`, per the
existing separability rule (`multivariate-specification`): since neither process
references a modeled panel layer, both fids are marked separable and no new estimator
is required.

#### Scenario: separable coevolution estimates with estimate_dynam
- **WHEN** a multivariate specification composes an event-observed friendship process
  and an event-observed smoking behavior process, neither referencing a modeled panel
  layer
- **THEN** `estimate_dynam()` estimates each process's parameters from its own fid's
  preprocessed output, and the specification print marks both fids separable.
