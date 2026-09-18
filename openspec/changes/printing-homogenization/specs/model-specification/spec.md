## MODIFIED Requirements

### Requirement: specification print is a single-glance overview

`print()` for a `goldfishSpec` SHALL render an overview and there SHALL be no
`summary` method. The output MUST state the model and which sub-models are present, and a
Dependent block that names the `layer` and — from the resolved dependent object — the number
of events, the time span, the sender→receiver nodesets, and the network. The rate and choice
formula(s) SHALL be shown, the support-constraint formula SHALL be shown only when supplied,
and the validation result SHALL be indicated. Right-censored events SHALL NOT be shown (they
are unknown until preprocessing). When flavors are present, the flavors SHALL be nested
under the single dependent layer and rendered through the shared multi-process
renderer, each process labeled with the label every data method uses and its
fid, in flavor-major fid order, with the formulas as that process's content.

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

#### Scenario: flavors nested under the dependent layer
- **WHEN** a flavor-keyed specification is printed
- **THEN** the flavors are listed beneath the single Dependent layer and each flavor's
  rate/choice formulas are shown under its process label and fid, in the
  same order and layout the fitted container prints.

#### Scenario: cli-rendered output is deterministic for snapshots
- **WHEN** the print is captured under a reproducible `cli` context (fixed width, colour off)
- **THEN** the rendered output uses `cli` semantic elements and is stable across terminals so
  it can be snapshot-tested.
