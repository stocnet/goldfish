## MODIFIED Requirements

### Requirement: Interaction operands maintained at their broadcast kind
An interaction operand's live value SHALL be stored and updated at the operand's own
broadcast kind on every kernel shape, so that a kind-shaped delta is written at kind size
rather than expanded into dense cells. A sender-indexed kernel already stores an operand
as a length-n1 vector; a dyad-indexed kernel SHALL likewise store a `global` operand as a
scalar, an `ego` operand as a length-n1 vector, an `alter` operand as a length-n2 vector,
and a dense n1 x n2 value only for a `point` operand. The operand write SHALL go through
the shared maintain-at-kind routines rather than through an operand-specific expansion.

#### Scenario: a sender-indexed operand write goes through the shared routine
- **WHEN** an interaction in a sender-indexed model has an operand and an event
  changes it
- **THEN** the operand's stored value is written through the shared
  maintain-at-kind write, in place, and the buffer is not duplicated

#### Scenario: a kind-shaped delta is written once per entry
- **WHEN** a broadcast effect reports an operand delta on the dyad grid's terms,
  so that one changed entry arrives as many rows naming it
- **THEN** the operand's stored value receives exactly one write per distinct
  entry
