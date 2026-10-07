## MODIFIED Requirements

### Requirement: make_specification constructs a goldfishSpec object

The package SHALL provide `make_specification()` that accepts `rate` and/or `choice`
formulas with an **empty left-hand side**, a `model`, the relevant sub-model(s), a `layer`
naming the dependent process, an optional `support_constraint` formula, an
`allow_reflexive` flag, and a `data` object,
and returns an S3 object of class `goldfishSpec`. The object MUST hold the parsed
rate/choice formula objects that preprocessing consumes (effect descriptors,
perspective/`type` parameters, interaction structure, intercept flag, and per-effect
arguments) together with model metadata and validation results. v1 MUST cover DyNAM and REM
(DyNAMi is deferred to its separate preprocessing-path change) for the simple rate and choice
formula case.
`make_specification()` SHALL be marked **experimental** (`lifecycle::badge("experimental")`)
because its v1 surface is expected to evolve.

`allow_reflexive` SHALL default to `FALSE`, so a self-dyad is excluded from every
risk set unless the user asks for it, and SHALL be stored on the specification and on
its model spec so that preprocessing, estimation and simulation read one value. It
SHALL be refused at construction, with a `cli` error naming the reason, when the
dependent layer spans two node sets (no self-dyad exists) or when the sub-model is
DyNAM `choice_coordination` (a self-pair is not a coordination pair).

#### Scenario: build a DyNAM specification
- **WHEN** a user calls `make_specification(rate = ~ 1 + indeg, choice = ~ inertia + recip,
  model = "DyNAM", layer = "callsDep", data = d)`
- **THEN** a `goldfishSpec` object is returned containing the parsed rate and
  choice components and passing validation for the DyNAM model.

#### Scenario: support_constraint is parsed, validated, and active
- **WHEN** a `support_constraint` formula is supplied to `make_specification()`
- **THEN** it is parsed against the restricted boolean-tree grammar, validated, and stored
  on the object
- **AND** estimating from the specification restricts the risk set per the
  `support-constraint` capability (the constraint is no longer stored-only).

#### Scenario: invalid model/sub_model combination
- **WHEN** a specification requests a `model`/`sub_model` combination that is not allowed
- **THEN** `make_specification()` aborts with a single consistent `cli` error.

#### Scenario: self-ties are excluded unless asked for
- **WHEN** a one-mode DyNAM specification is built without `allow_reflexive`
- **THEN** the specification carries `allow_reflexive = FALSE`, and every risk set
  built from it — the rate's sender gate, the choice row, the REM matrix and the
  simulated draws — excludes each sender's own cell, as before this change.

#### Scenario: a reflexive model keeps the diagonal
- **WHEN** a one-mode DyNAM specification is built with `allow_reflexive = TRUE`
- **THEN** the self-dyad is a candidate wherever the support constraint and presence
  allow it: it counts toward the rate's sender gate and the event's candidate count,
  the likelihood does not exclude it, and a simulated event may name the same actor
  as sender and receiver.

#### Scenario: reflexivity is refused where no self-dyad exists
- **WHEN** `allow_reflexive = TRUE` is supplied for a dependent layer spanning two
  node sets, or for a DyNAM `choice_coordination` sub-model
- **THEN** `make_specification()` aborts with a `cli` error naming the layer or the
  sub-model and why a self-dyad cannot exist there.
