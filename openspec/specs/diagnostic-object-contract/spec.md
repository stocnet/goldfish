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

How far the accumulated intervals reach SHALL depend on whether the
sub-model's likelihood defines a compensator. On the exact-time rate and REM
families the intervals SHALL accumulate from `start_time` to `end_time`,
the final one being exposure during which no event occurred. On the
multinomial families, which store no right-censored intervals because such
an interval would contribute zero to their likelihood, the intervals SHALL
accumulate to the last dependent event, and the documentation SHALL state
the difference and its reason.

The fit SHALL carry both of its counts as named scalars — `n_events`, the
number of **dependent events**, and `n_intervals`, the number of likelihood
intervals — and SHALL NOT report either under a name that means the other. The
two coincide on the multinomial families and differ on any fit carrying
right-censored intervals, where a windowed effect opens one closure per event.

Every consumer that means the number of events SHALL read `n_events`, and no
consumer SHALL recompute either count for itself. This covers the information
criteria that carry a sample size (BIC and AICc), the per-event average
log-likelihood, the Grambsch-Therneau scaling constant, and every rendered count
described as events. AIC SHALL be unaffected, carrying no sample size.

The event count is the right constant for the information criteria because
information accrues with events rather than with exposure records, and because
only that choice leaves a windowed and an unwindowed model on one scale. It is
the right constant for the Grambsch-Therneau scaling because the residual being
scaled is the conditional score, which carries no exposure term and is undefined
on an interval that realizes no alternative — the Cox partial-likelihood
residual, whose scaling constant is the event count.

`n_intervals` SHALL remain available for the diagnostics that legitimately
report both, and SHALL equal the length of the fit's per-interval components.

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
missing stored primitive does. A diagnostic reaching for a component that a
**flavored container** does not carry SHALL abort on the same rule, naming
the component and the per-process route, rather than returning `NULL`.

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

#### Scenario: a container without a component aborts too
- **WHEN** an accessor reading the risk-set axis is called on a flavored
  container, which carries no single model specification
- **THEN** it aborts naming the component and the per-process route, rather
  than returning `NULL`

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
- **WHEN** the elapsed times of an exact-time fit are accumulated from
  `start_time`, including one whose `end_time` falls after its last event
- **THEN** they reach `end_time`, and the fit's event times fall inside the
  window, so a per-event quantity can be placed on the observed time axis
  without reconstructing the window

#### Scenario: a multinomial clock ends at the last event
- **WHEN** the elapsed times of a choice fit with an `end_time` after its
  last event are accumulated from `start_time`
- **THEN** they reach that last event rather than `end_time`, because the
  sub-model stores no right-censored interval

#### Scenario: a window does not change the information-criterion sample size
- **WHEN** a rate model is fitted twice on one event stream, once with a
  windowed effect that opens a right-censored interval per event and once
  without it
- **THEN** both report the same sample size to BIC and AICc, equal to the
  number of dependent events, so the two fits are compared on one scale

#### Scenario: the two counts are stored under honest names
- **WHEN** a fit carrying right-censored intervals is inspected
- **THEN** `n_events` equals the number of dependent events, `n_intervals`
  equals the length of the per-interval components, and neither is reported
  under the other's name

#### Scenario: a per-event average divides by events
- **WHEN** the per-event average log-likelihood is requested on a fit whose
  intervals outnumber its events
- **THEN** it equals the total log-likelihood divided by the event count

#### Scenario: every rendered event count is the event count
- **WHEN** a diagnostic renders a count it describes as events
- **THEN** the number shown is the dependent event count, and a diagnostic
  reporting both names each of them

#### Scenario: the dependence indicator has one spelling
- **WHEN** the components of a fitted object are inspected
- **THEN** the fit carries `right_censored_events` and no second
  dependence indicator, and its documentation names the preprocessed
  object's `is_dependent` as the negated counterpart

### Requirement: Diagnostic generics carry a verdict for every fit class

Each diagnostic generic dispatching on a fitted-model class SHALL have a
recorded verdict, for every concrete fit class, in the `fit-class-hierarchy`
capability, so that what a flavored or Monte-Carlo fit returns from a
diagnostic is stated rather than implied by which method happens to have been
written. A diagnostic
that cannot be computed for a fit class SHALL refuse with a reason rather than
returning an empty or partial result.

#### Scenario: a diagnostic states its behavior per fit class

- **WHEN** a diagnostic generic is called on each concrete fit class
- **THEN** each call either returns the diagnostic, or aborts with the
  recorded reason, and no combination is left to whichever method happens to
  exist

#### Scenario: a diagnostic that cannot be computed refuses
- **WHEN** a diagnostic is not computable for a fit class
- **THEN** the call aborts naming the class and the reason, rather than
  returning an empty or partial table

