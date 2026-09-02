# diagnostic-object-contract

What a fitted model carries so a diagnostic can be computed from it.

## ADDED Requirements

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
