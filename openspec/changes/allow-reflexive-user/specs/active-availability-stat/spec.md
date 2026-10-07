## MODIFIED Requirements

### Requirement: Structural self-tie exclusion is not folded into active_dyad
The likelihood loop SHALL keep the structural exclusion of self-ties (the
diagonal) in choice/REM models whose risk set excludes them — one node set and
`allow_reflexive = FALSE` — and SHALL keep the diagonal in the risk set
otherwise; either way the rule SHALL NOT be folded into `active_dyad` nor
contribute to its encoding decision.

#### Scenario: diagonal does not force a dyadic encoding
- **WHEN** an unconstrained one-mode choice model with receiver composition
  changes is preprocessed
- **THEN** `active_dyad` is at the alter encoding (not outer/point), carries no
  diagonal information, and self-ties are still excluded from every event's
  candidate set by the likelihood.

#### Scenario: a reflexive model keeps its encoding too
- **WHEN** the same model is preprocessed with `allow_reflexive = TRUE`
- **THEN** `active_dyad` is at the same encoding and still carries no diagonal
  information, and the likelihood includes the self-dyad in each event's
  candidate set
