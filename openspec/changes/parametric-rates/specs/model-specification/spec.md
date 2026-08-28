# model-specification Delta Specification

## MODIFIED Requirements

### Requirement: make_specification constructs a specification.goldfish object

The package SHALL provide `make_specification()` that accepts `rate` and/or `choice`
formulas with an **empty left-hand side**, a `model`, the relevant sub-model(s), a
`distribution` selecting the rate waiting-time model (`"exponential"` default,
`"weibull"`, `"gompertz"`, `"cox"`; stored on the object and consumed at estimation
without any estimate-time argument), a `layer` naming the dependent process, an
optional `support_constraint` formula, and a `data` object,
and returns an S3 object of class `specification.goldfish`. The object MUST hold the parsed
rate/choice formula objects that preprocessing consumes (effect descriptors,
perspective/`type` parameters, interaction structure, intercept flag, and per-effect
arguments) together with model metadata and validation results. The `rate_sub_model`
vocabulary SHALL NOT include `"rate_ordered"` (the Cox model is
`distribution = "cox"`). v1 MUST cover DyNAM and REM
(DyNAMi is deferred to its separate preprocessing-path change) for the simple rate and choice
formula case.
`make_specification()` SHALL be marked **experimental** (`lifecycle::badge("experimental")`)
because its v1 surface is expected to evolve.

#### Scenario: build a DyNAM specification
- **WHEN** a user calls `make_specification(rate = ~ 1 + indeg, choice = ~ inertia + recip,
  model = "DyNAM", layer = "callsDep", data = d)`
- **THEN** a `specification.goldfish` object is returned containing the parsed rate and
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

#### Scenario: distribution stored and honored
- **WHEN** `make_specification(rate = ~ 1 + indeg, model = "DyNAM",
  distribution = "weibull", layer = "callsDep", data = d)` is created and estimated
- **THEN** the object records the distribution, the print method shows it, and the rate
  block is fitted under the Weibull baseline.

#### Scenario: distribution without a rate formula aborts
- **WHEN** `make_specification(choice = ~ inertia, model = "DyNAM",
  distribution = "weibull", layer = "callsDep", data = d)` is called
- **THEN** a `cli` error explains `distribution` applies to the rate part only.
