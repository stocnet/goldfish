## ADDED Requirements

### Requirement: Interaction operands maintained at their broadcast kind
An interaction operand's live value SHALL be stored and updated at the operand's own
broadcast kind on every kernel shape, so that a kind-shaped delta is written at kind size
rather than expanded into dense cells. A sender-indexed kernel already stores an operand
as a length-n1 vector; a dyad-indexed kernel SHALL likewise store a `global` operand as a
scalar, an `ego` operand as a length-n1 vector, an `alter` operand as a length-n2 vector,
and a dense n1 x n2 value only for a `point` operand. The operand write SHALL go through
the shared maintain-at-kind routines rather than through an operand-specific expansion.

#### Scenario: an alter operand is not expanded to dense cells
- **WHEN** an interaction in a dyad-indexed model has an `alter`-kind operand and an event
  changes that operand for one receiver
- **THEN** the operand's stored value is a length-n2 vector, one entry of it is written,
  and no n1 x n2 cell matrix is constructed for the update

#### Scenario: the interaction product is unchanged
- **WHEN** any interaction model is preprocessed before and after operands move to
  kind-shaped storage
- **THEN** the product statistic and its update stream are byte-identical, and the frozen
  coefficient baselines reproduce exactly

#### Scenario: a point operand still stores dense
- **WHEN** an interaction operand is genuinely dyadic (`point` kind)
- **THEN** its live value is a dense n1 x n2 matrix and its updates are point cells
