# model-specification Specification

## Purpose
TBD - created by archiving change refactor-formula-parsing. Update Purpose after archive.
## Requirements
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

#### Scenario: support_constraint is parsed, validated, and active
- **WHEN** a `support_constraint` formula is supplied to `make_specification()`
- **THEN** it is parsed against the restricted boolean-tree grammar, validated, and stored
  on the object
- **AND** estimating from the specification restricts the risk set per the
  `support-constraint` capability (the constraint is no longer stored-only).

#### Scenario: invalid model/sub_model combination
- **WHEN** a specification requests a `model`/`sub_model` combination that is not allowed
- **THEN** `make_specification()` aborts with a single consistent `cli` error.

### Requirement: layer identifies the dependent process; the LHS does not

The rate and choice formulas SHALL leave the left-hand side empty; the dependent process is
named by the `layer` argument or by `info$focal` (with `layer` overriding it when both are
given). `layer` SHALL resolve against `data` to one of the stocnet object's layer names —
there is no bridge-era `dependent.goldfish` lookup: the legacy constructors assemble
stocnet, so layer names are the only vocabulary. Placing a
dependent-events object on the LHS SHALL be a clear error that directs the user to `layer`.
`rate`/`choice` MAY be supplied as a **flavor-keyed `list`** (American spelling) whose
formula LHS carries only the flavor symbol (e.g. `creation ~ ...`), never the
dependent-events object: the named flavor selects which focal-layer rows are modeled
(rows with any other or `NA` `flavor` update state only). In this change exactly ONE
flavor key is supported — multiple keys SHALL abort pointing to the future
multi-process change — and when both `rate` and `choice` are lists they MUST key the
same flavor. A plain (non-list) formula on a focal layer that carries a `flavor` column
SHALL model all rows and emit a `cli_inform` making that visible. Legacy
`make_dependent_events()`-wrapper data resolves the dependent-object name to (focal
layer, its stamped flavor key) internally, without the inform.

#### Scenario: One modeled flavor selects the dependent rows
- **WHEN** `make_specification(rate = list(creation ~ inertia()), layer = "treaties",
  data = fisheries_treaties)` runs on a layer whose ties carry
  `flavor ∈ {creation, dissolution}`
- **THEN** only `creation` rows are modeled as dependent events while `dissolution` rows
  update the network state, reproducing the legacy filtered-dependent-events model.

#### Scenario: Multiple flavor keys rejected for now
- **WHEN** `rate = list(creation ~ ..., dissolution ~ ...)` is supplied
- **THEN** the call aborts explaining one modeled flavor is supported and multi-process
  estimation arrives with a later change.

#### Scenario: Plain formula on a flavored layer informs
- **WHEN** a plain `rate = ~ 1 + indeg()` is supplied and the focal layer has a `flavor`
  column
- **THEN** all focal rows are modeled and a `cli_inform` states the flavors present and
  that all are modeled.

#### Scenario: layer resolves to a stocnet layer
- **WHEN** `make_specification(..., layer = "calls", data = x)` is given a stocnet `x`
  with a `calls` layer
- **THEN** the specification resolves the dependent process to the calls layer and records
  its event count, time span, nodesets (mode pair), and network.

#### Scenario: focal used when layer omitted
- **WHEN** `layer` is not supplied and the stocnet declares `info$focal = "calls"`
- **THEN** the calls layer is the dependent process.

#### Scenario: layer set by a legacy wrapper resolves as a stocnet layer
- **WHEN** data was assembled through the deprecated `make_dependent_events()` wrapper
  and `layer` names the layer that wrapper created
- **THEN** the specification resolves it through the stocnet layer vocabulary — the same
  path as directly-constructed stocnet input, with no separate lookup.

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

