## ADDED Requirements

### Requirement: Specification-derived flavor constraints ride the user-constraint machinery

Derived flavor masks SHALL ride the user-constraint machinery: support-constraint
masks derived by `make_specification()` from a mutually exclusive flavored layer (per
the `flavored-processes` capability) are compiled, stored, and maintained exactly as
user-supplied `support_constraint` formulas are: each
flavor's derived formula is expressed in the restricted boolean-tree grammar over
`tie(L)` atoms reading the modeled layer's evolving state (mask-reading atoms remain
forbidden), AND-composed with any user constraint into one compiled mask per flavor,
stored as multiple derived objects in the plan's derivations, maintained incrementally
during preprocessing, and segmenting that flavor's right-censored timeline at every
flip. The preprocessing-time set-size validation SHALL apply per flavor — a flavor
whose combined mask empties its risk set at some event aborts with the existing
empty-set diagnostics naming the flavor.

#### Scenario: derived masks flip with the modeled layer's own events
- **WHEN** a creation event adds tie (i, j) on a mutually exclusive layer
- **THEN** the creation mask closes (i, j) and the dissolution mask opens it, each flip
  segmenting the respective flavor's right-censored timeline.

#### Scenario: contradiction with a user constraint is caught per flavor
- **WHEN** a user `support_constraint` combined with a flavor's derived mask leaves an
  event with zero allowed candidates for that flavor
- **THEN** preprocessing aborts with the existing empty-risk-set diagnostics, naming the
  flavor whose mask emptied.
