# diagnostic-object-contract Specification

## Purpose
TBD - created by archiving change parity-followups. Update Purpose after archive.
## Requirements
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

### Requirement: A fitted model SHALL carry the interval clock of its likelihood
A fitted model SHALL carry the per-interval elapsed time its likelihood was
computed over (`intervals`), and the observation window's `start_time` and
`end_time`, alongside the event times and the right-censoring indicator it
already carries. These are per-interval and scalar quantities — one vector
of the same length as the other per-event components, and two numbers — not
a replay object, so they SHALL be present on every fit regardless of the
`diagnostics` request or of whether the preprocessed statistics were
attached.

Their purpose is that a diagnostic which needs only the clock SHALL NOT
require the replay object: with `intervals` and the `"loglik"` primitive,
the Cox–Snell residual of an exact-time fit is the product
`intervals * total_rate`, and the count of likelihood events versus
dependent events — the constant that scales Schoenfeld residuals — is
readable from the fit alone. A diagnostic or plot placing per-event
quantities on the observed time axis SHALL read them from these components
rather than reconstructing a clock from event times.

The fit SHALL NOT carry a second spelling of the dependence indicator: the
existing `right_censored_events` is that fact, and the documentation SHALL
cross-reference it against the preprocessed object's `is_dependent`, which
is its negation.

Adding these components SHALL NOT move the fitted object's format epoch:
that epoch names a **released** layout, and it moves once per release whose
layout differs from the previous release's, never once per component added
during a development line. A consequence SHALL be handled by the consumers
rather than by the stamp: within a development line an object can carry the
current epoch and still lack a component added after it was fitted, so a
diagnostic that needs such a component SHALL check for it and abort naming
what it needs and how to obtain it, exactly as a diagnostic naming a
missing stored primitive does.

#### Scenario: adding a component does not invalidate stored objects
- **WHEN** the interval clock is added to the fitted object's layout during
  a development line
- **THEN** the format epoch is unchanged, and an object fitted before the
  addition is still accepted by the surfaces that do not read the clock

#### Scenario: a consumer of a missing component says what it needs
- **WHEN** a diagnostic requiring the interval clock is called on a fit
  made before the clock existed
- **THEN** it aborts naming the missing component and the remedy, rather
  than failing on a `NULL` or reporting a wrong number

#### Scenario: the clock is present without the replay object
- **WHEN** a model is fitted without `return_preprocessed = TRUE`
- **THEN** the result carries `intervals`, `start_time` and `end_time`, and
  `intervals` has one value per interval, matching the length of the
  per-event log-likelihood

#### Scenario: Cox-Snell residuals need no replay object
- **WHEN** an exact-time fit stores the `"loglik"` primitive and carries no
  preprocessed statistics
- **THEN** the compensator of each interval is the product of that
  interval's elapsed time and its total rate, and at the maximum the
  compensators sum to the number of dependent events

#### Scenario: the clock spans the observation window
- **WHEN** the elapsed times of a fit are accumulated from `start_time`
- **THEN** they reach `end_time`, and the fit's event times fall inside the
  window, so a per-event quantity can be placed on the observed time axis
  without reconstructing the window

#### Scenario: the dependence indicator has one spelling
- **WHEN** the components of a fitted object are inspected
- **THEN** the fit carries `right_censored_events` and no second
  dependence indicator, and its documentation names the preprocessed
  object's `is_dependent` as the negated counterpart

