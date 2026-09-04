## MODIFIED Requirements

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
