## MODIFIED Requirements

### Requirement: Symmetric mask for coordination and undirected REM
For DyNAM `choice_coordination` and REM on an undirected network, the mask SHALL be
symmetric: the evaluator symmetrizes a point-kind mask as `mask & t(mask)` so a mutual
dyad is active only when both directions are allowed. A mask of alter, ego or
global kind SHALL be symmetrized without a grid: the symmetric cell `(i, j)` of
a vector mask `m` is `m[i] & m[j]`, so the availability is stored at the outer
encoding with the mask vector folded into both factors, and a point value
SHALL NOT be allocated for it. (Directed-REM asymmetry is out of
scope — deferred to the `review-rem-directed` change.)

#### Scenario: one-directional allowance excludes the coordination dyad
- **WHEN** a `choice_coordination` model has `support[i, j] = TRUE` but
  `support[j, i] = FALSE`
- **THEN** the dyad `(i, j)` is not in the coordination risk set.

#### Scenario: a vector mask symmetrizes as two equal factors
- **WHEN** a `choice_coordination` model has `support_constraint = ~
  alter(attr) > 0` and actor `k` has `attr == 0`
- **THEN** every dyad touching `k` is outside the risk set, the stored
  availability is outer-encoded with both factors equal to presence AND the
  mask vector, and the estimate equals the point-encoded symmetric fold within
  1e-10.
