# diagnostic-object-contract (delta)

## ADDED Requirements

### Requirement: Actor margins SHALL use one role-keyed shape on every risk-set geometry
Stored actor margins SHALL present the same structure regardless of the model
family: a list carrying `axis` (the risk-set axis verbatim), `roles` (the
character vector of populated role slots), and one named element per role
holding that role's `observed` and `expected` vectors, plus
`expected_probability` on exact-time sub-models. A consumer SHALL be able to
iterate `roles` and read each role's vectors without branching on model or
sub-model. Role names SHALL be `"sender"` and `"receiver"` for the directed
geometries and `"participant"` for coordination, whose realized risk set is the
unordered pair list and whose single actor set is neither a sender set nor a
receiver set.

#### Scenario: a single-sided family declares one role
- **WHEN** a DyNAM-choice fit is estimated with `diagnostics` naming `"margins"`
- **THEN** `fit$margins$roles` is `"receiver"`, `fit$margins$receiver` holds
  `observed` and `expected`, and no `sender` element is present

#### Scenario: a two-sided family declares both roles under the same schema
- **WHEN** a REM fit is estimated with `diagnostics` naming `"margins"`
- **THEN** `fit$margins$roles` is `c("sender", "receiver")` and each of
  `fit$margins$sender` and `fit$margins$receiver` holds `observed`, `expected`
  and `expected_probability` under those exact names

#### Scenario: the same consumer code reads every family
- **WHEN** a routine iterates `fit$margins$roles` and reads
  `fit$margins[[role]]$expected` for a fit of each supported sub-model
- **THEN** it succeeds for every one of them without inspecting `fit$model` or
  `fit$sub_model`

#### Scenario: coordination declares a participant role, not a sender role
- **WHEN** a DyNAM choice_coordination fit is estimated with `"margins"`
- **THEN** `fit$margins$roles` is `"participant"` and no element named `sender`
  or `receiver` is present

### Requirement: Coordination margins SHALL declare their doubled accumulation
Coordination margins SHALL record that each observation credits two actors, so a
consumer normalizing per-actor counts does not have to know that coordination
totals twice the event count while every other family totals it once. The
declaration SHALL be a machine-readable attribute on the margins object rather
than prose, and SHALL be present on every family with the value that family
uses.

#### Scenario: coordination totals twice the event count and says so
- **WHEN** a DyNAM choice_coordination fit stores margins
- **THEN** the sum of `fit$margins$participant$expected` equals twice the number
  of dependent events, and the margins object declares two credits per
  observation

#### Scenario: a single-credit family declares one
- **WHEN** a DyNAM-choice fit stores margins
- **THEN** the sum of `fit$margins$receiver$expected` equals the number of
  dependent events, and the margins object declares one credit per observation

### Requirement: A fitted model SHALL declare the axis its per-event indices refer to
A fitted model SHALL expose, through documented and exported surface, the
risk-set axis that gives meaning to a position in any per-event diagnostic
component. Reading it SHALL NOT require the `:::` operator or reaching into the
stored model specification. The axis SHALL distinguish the sender axis, the
receiver-given-sender axis, the ordered dyad grid, and the unordered pair list,
so that two fits returning vectors of identical length are still
distinguishable.

#### Scenario: same-length vectors from different axes are distinguishable
- **WHEN** a DyNAM-rate fit and a DyNAM-choice fit over the same node set both
  return per-event probability vectors of the same length
- **THEN** the declared axis is the sender axis for the first and the
  receiver-given-sender axis for the second

#### Scenario: the axis is readable without reaching into internals
- **WHEN** a consumer reads the axis from a fitted model using exported surface
  only
- **THEN** it succeeds without calling an unexported function and without
  indexing into the stored model specification

### Requirement: Per-event indices SHALL resolve to actors through the fit's node lookup
A fitted model SHALL carry a node lookup sufficient to resolve any per-event
diagnostic index to the original node row and its label, on every risk-set
geometry and for both sides of a two-mode or node-subset model. Per-event
components SHALL NOT carry their own labels: the lookup is one table per fit,
whereas per-event labelling would repeat it once per event and defeat the
storage guardrail the per-event primitives already sit behind.

#### Scenario: an index resolves to its original node row and label
- **WHEN** a position in a per-event probability vector is joined to the fit's
  node lookup on the side the declared axis names
- **THEN** it resolves to exactly one node row, carrying that node's original
  identifier and label

#### Scenario: both sides resolve on a two-mode model
- **WHEN** a two-mode fit's node lookup is inspected
- **THEN** it carries entries for both node sets, distinguished by side, so a
  dyad index resolves on each axis independently

#### Scenario: per-event components stay unlabelled
- **WHEN** a fit stores per-event probabilities for many events
- **THEN** the stored components carry no per-event copy of the node labels
