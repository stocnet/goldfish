## ADDED Requirements

### Requirement: make_specification constructs a specification.goldfish object

The package SHALL provide `make_specification()` that accepts `rate` and/or `choice`
formulas with an **empty left-hand side**, a `model`, the relevant sub-model(s), a `layer`
naming the dependent process, an optional `support_constraint` formula, and a `data` object,
and returns an S3 object of class `specification.goldfish`. The object MUST hold the parsed
rate/choice formula objects that preprocessing consumes (effect descriptors,
perspective/`type` parameters, interaction structure, intercept flag, and per-effect
arguments) together with model metadata and validation results. v1 MUST cover DyNAM and REM
(DyNAMi is deferred to its separate preprocessing-path change) for the simple rate and choice
formula case.
`make_specification()` SHALL be marked **experimental** (`lifecycle::badge("experimental")`)
because its v1 surface is expected to evolve.

#### Scenario: build a DyNAM specification
- **WHEN** a user calls `make_specification(rate = ~ 1 + indeg, choice = ~ inertia + recip,
  model = "DyNAM", layer = "callsDep", data = d)`
- **THEN** a `specification.goldfish` object is returned containing the parsed rate and
  choice components and passing validation for the DyNAM model.

#### Scenario: support_constraint stored but not yet active
- **WHEN** a `support_constraint` formula is supplied to `make_specification()`
- **THEN** it is parsed and validated and stored on the object
- **AND** v1 does not yet alter the risk set (that engine is delivered by the
  `support-constraint-risk-set` change).

#### Scenario: invalid model/sub_model combination
- **WHEN** a specification requests a `model`/`sub_model` combination that is not allowed
- **THEN** `make_specification()` aborts with a single consistent `cli` error.

### Requirement: layer identifies the dependent process; the LHS does not

The rate and choice formulas SHALL leave the left-hand side empty; the dependent process is
named by the `layer` argument. `layer` SHALL be resolved against `data`: in the bridge era
it MUST resolve via the existing parser lookup to a `dependent.goldfish` object (the
dependent-events object name), and once the single data object exists the same `layer`
string SHALL resolve to a network/layer name without any change to the call surface. Placing
a dependent-events object on the LHS SHALL be a clear error that directs the user to `layer`.
When rate/choice are supplied as a flavour-keyed `list`, the formula LHS SHALL carry only the
flavour symbol (e.g. `creation ~ ...`), never the dependent-events object.

#### Scenario: layer resolves to a dependent process in the bridge era
- **WHEN** `layer = "callsDep"` is given and `data` contains a `dependent.goldfish` named
  `callsDep`
- **THEN** the specification resolves the dependent process from that object and records its
  network, nodesets, event count, and time span.

#### Scenario: dependent object on the LHS is rejected
- **WHEN** a rate or choice formula is written with a dependent-events object on the LHS
  (e.g. `callsDep ~ ...`)
- **THEN** `make_specification()` aborts with an error directing the user to use the `layer`
  argument with an empty LHS.

#### Scenario: rate and choice share the dependent without an equality check
- **WHEN** both `rate` and `choice` are supplied with empty LHS and a single `layer`
- **THEN** both submodels reference the same dependent process by construction, with no
  rate-vs-choice LHS comparison required.

### Requirement: specification print is a single-glance overview

`print()` for a `specification.goldfish` SHALL render an overview and there SHALL be no
`summary` method. The output MUST state the model and which sub-models are present, and a
Dependent block that names the `layer` and — from the resolved dependent object — the number
of events, the time span, the sender→receiver nodesets, and the network. The rate and choice
formula(s) SHALL be shown, the support-constraint formula SHALL be shown only when supplied,
and the validation result SHALL be indicated. Right-censored events SHALL NOT be shown (they
are unknown until preprocessing). When flavours are present, the flavours SHALL be nested
under the single dependent layer.

The output SHALL be rendered with `cli` semantic elements (e.g. `cli_rule`, `cli_text`,
`cli_bullets`, `cli_dl`, `cli_alert_success`/`cli_alert_danger`) and inline markup (`{.val}`,
`{.field}`, `{.code}`, `{.cls}`), consistent with the package's existing `cli` usage for
conditions. Formula content SHALL be interpolated as pre-deparsed strings (data), never as
literal markup.

#### Scenario: simple specification overview
- **WHEN** a single-layer DyNAM rate+choice specification is printed
- **THEN** the output shows `Model DyNAM` with the present sub-models, a Dependent block with
  the layer, event count, time span, nodesets and network, the Rate and Choice formulas, and
  the validation result.

#### Scenario: omit absent elements
- **WHEN** a specification has no `support_constraint` and only one sub-model
- **THEN** the Support line is omitted and only the present sub-model's formula is shown.

#### Scenario: flavours nested under the dependent layer
- **WHEN** a flavour-keyed specification is printed
- **THEN** the flavours are listed beneath the single Dependent layer and each flavour's
  rate/choice formulas are shown under its label.

#### Scenario: cli-rendered output is deterministic for snapshots
- **WHEN** the print is captured under a reproducible `cli` context (fixed width, colour off)
- **THEN** the rendered output uses `cli` semantic elements and is stable across terminals so
  it can be snapshot-tested.

### Requirement: estimate_* accepts a specification object or a formula

`estimate_dynam()` and `estimate_rem()` SHALL accept a
`specification.goldfish` object in place of a formula (`estimate_dynami()` is deferred to the
DyNAMi preprocessing-path change). When given the object they MUST reuse
its parsed contents rather than re-parsing, and produce results identical to passing the
equivalent formula. The existing formula interface MUST continue to work unchanged.

#### Scenario: estimate from a specification equals estimate from a formula
- **WHEN** the same model is estimated once via `estimate_dynam(spec)` and once via the
  equivalent `estimate_dynam(formula, data)`
- **THEN** the estimated coefficients are identical to within 1e-6.

#### Scenario: legacy formula path preserved
- **WHEN** an existing call passes a formula (not a specification) to any `estimate_*()`
- **THEN** behaviour and results are unchanged from before this change.
