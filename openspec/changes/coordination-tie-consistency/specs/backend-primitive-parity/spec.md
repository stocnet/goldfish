# backend-primitive-parity (delta)

## ADDED Requirements

### Requirement: Alternatives the model makes equal SHALL be equal on every backend

Every backend SHALL produce identical values for alternatives whose model
statistics are identical, exactly rather than within a tolerance. A tolerance is
the right standard for values the model distinguishes, and the wrong one for
values it does not: a primitive that compares alternatives exactly, such as a
rank, turns a last-bit difference between mathematically equal alternatives into
a different user-visible answer, while every tolerance-based parity check
continues to pass. This applies within a backend as well as across them — a
computation that splits an equal block into several distinct values has already
lost the property, whichever backend it runs on.

#### Scenario: an equal block stays equal within a backend
- **WHEN** a model's risk set contains alternatives whose statistics are
  identical, so that their linear predictors are equal
- **THEN** the stored per-event probabilities for those alternatives are
  bit-identical to one another

#### Scenario: the backends agree on which alternatives are equal
- **WHEN** the same model is estimated on each supported backend at one fixed
  parameter vector with per-event probabilities requested
- **THEN** each backend partitions the alternatives into the same blocks of
  equal value, so the number of distinct values agrees across backends

#### Scenario: ranks do not depend on the backend
- **WHEN** a coordination model is estimated on each supported backend with
  ranks requested
- **THEN** the stored `observed_rank` vectors are identical
