# diagnostic-object-contract (delta)

## ADDED Requirements

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

#### Scenario: the axis names the geometry, not the family
- **WHEN** the declared axis is read from a REM fit and from a DyNAM
  choice_coordination fit
- **THEN** the first reports the ordered dyad grid and the second the unordered
  pair list, without the consumer inspecting `model` or `sub_model`

### Requirement: Per-event indices SHALL resolve to actors through the fit's node lookup
A fitted model SHALL carry a node lookup sufficient to resolve any per-event
diagnostic index to the original node row and its label, on every risk-set
geometry and for both sides of a two-mode or node-subset model. Per-event
components SHALL NOT carry their own labels: the lookup is one table per fit,
whereas per-event labelling would repeat it once per event and defeat the
storage guardrail the per-event primitives already sit behind. This constrains
per-event components only; per-actor components, of which there is one vector
per fit rather than one per event, are labelled by the capability that owns
them.

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
