## MODIFIED Requirements

### Requirement: The observation window is closed when the schedule ends first
Preprocessing SHALL close the observation window at the last **real** event when
no explicit `end_time` is given, and at `end_time` when one is given, on every
preprocessing substrate — the recipe loops, the merged single-clock walk, and the
walk handle — and identically across them. A **real** event is a modeled
dependent event or a genuinely exogenous covariate or composition change; a
**window-derived** row (a dissolve / expiry pseudo-event a window effect
schedules one window length after the event it expires) is not a real event and
SHALL NOT set the resolved end. The resolved end therefore spans the non-window
streams, never the window-derived rows.

A window-derived row or a composition-change row whose time is past the resolved
end SHALL update the shared state as needed but SHALL NOT emit a right-censored
observation: no stored row on any substrate SHALL have an event time after the
resolved end unless an explicit `end_time` licenses it, and the three substrates
SHALL NOT disagree about where the resolved end falls.

For the sub-models whose likelihood defines a compensator — the exact-time rate
and REM families — the elapsed time between the last event and an explicit
`end_time` is exposure during which no event occurred, a likelihood contribution,
and dropping it biases the baseline rate upward; that exposure SHALL be counted.
A sub-model whose likelihood has no compensator — the multinomial families —
SHALL store no such trailing row, because it would contribute zero while changing
the interval count. A right-censored row written at the window boundary SHALL NOT
carry the sender and receiver of an out-of-window event.

#### Scenario: exposure past the last event is counted
- **WHEN** an exact-time rate model is fitted with an `end_time` some days after
  its last event
- **THEN** the accumulated intervals reach `end_time`, and the fitted baseline
  rate is lower than the same model fitted with no `end_time`

#### Scenario: a later end_time counts more exposure
- **WHEN** the same exact-time rate model is fitted at two `end_time` values,
  both past the last event
- **THEN** the two fits differ, the later `end_time` giving the lower baseline
  rate

#### Scenario: a multinomial sub-model stores no trailing row
- **WHEN** a choice model is fitted with an `end_time` past its last event
- **THEN** its stored intervals end at the last dependent event, and its
  log-likelihood is unchanged from the same fit with no `end_time`

#### Scenario: the boundary row carries no borrowed identity
- **WHEN** a right-censored row is written at the window boundary
- **THEN** it does not report the sender or receiver of an event outside the
  window

#### Scenario: a window-effect expiry row does not extend the exact-time tail
- **WHEN** an exact-time rate or REM model with a windowed term is preprocessed
  with no explicit `end_time`, and the term's dissolve pseudo-events fall past
  the last real event
- **THEN** the resolved end is the last real event, no stored row has an event
  time past it, and the fit is identical whether taken through the recipe loop,
  the merged walk, or the walk handle

#### Scenario: the same holds under a support constraint
- **WHEN** the windowed model above also carries a `support_constraint` (with or
  without a windowed atom of its own)
- **THEN** the constraint's own expiry rows likewise do not extend the resolved
  end, and no stored row is right-censored past the last real event

#### Scenario: a composition change past the last dependent event
- **WHEN** an exact-time rate model is preprocessed with a composition change
  (`active_1` or `active_2`) whose time is after the last dependent event and no
  explicit `end_time` is given
- **THEN** the stored tail is governed by the resolved-end rule the same way on
  every substrate, and the merged walk and walk handle agree with the recipe
  loop on which rows, if any, are right-censored
