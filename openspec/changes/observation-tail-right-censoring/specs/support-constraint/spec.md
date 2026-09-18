## ADDED Requirements

### Requirement: The support mask reads node presence at the observation tail
The effective support mask SHALL read node presence (`active_1` and `active_2`)
at the observation tail the same way it reads it mid-sequence, so that a support
constraint never induces a right-censored row past the last real event and never
hides one that a real event requires. A frozen presence axis SHALL NOT be read at
the tail: a sender that joins after the first event is a candidate from the event
it joins, and a sender or receiver whose only activity is past the resolved end
contributes no risk-set membership and no censored exposure there.

The risk-set-size validations (observed-dyad-excluded, empty-risk-set,
never-a-candidate, forced-choice) are defined over present-and-allowed
candidates, so they SHALL take the tail's presence from the composition-change
stream, not from a time-zero vector, for the rate family and the choice family
alike. A constraint whose windowed atom schedules its own expiry rows SHALL NOT
let those rows extend the resolved end or emit a censored observation past the
last real event.

#### Scenario: a windowed constraint atom does not extend the tail
- **WHEN** a `support_constraint` with a windowed atom is preprocessed on an
  exact-time model whose atom expiry rows fall past the last real event, with no
  explicit `end_time`
- **THEN** the resolved end is the last real event and no stored row is
  right-censored past it, identically on every substrate

#### Scenario: a sender that joins at the tail is not a spurious censored row
- **WHEN** a sender joins the node set after the last dependent event under a
  rate-family support constraint
- **THEN** it is not read as at-risk-then-censored across an interval it was
  never present for, and the sender-gate availability at the tail matches a
  from-scratch per-event reduction
