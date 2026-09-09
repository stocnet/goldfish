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
dependent-events object: each named flavor selects the focal-layer rows modeled by its
formula (rows whose flavor matches no key, including `NA`, update state only). One or
MORE flavor keys SHALL be accepted — each key defines a parallel process on the same
focal layer per the `flavored-processes` capability. Keys MUST be distinct and MUST
resolve against the layer's flavor values (or the inferred default mapping on an
unflavored layer). When both `rate` and `choice` are flavor-keyed lists they MAY
key different flavor sets: a flavor keyed in one list only is modeled in that
family and unmodeled in the other, the difference is recorded on the
specification as a completion gap for the generative consumers, and
construction SHALL NOT abort on it. A plain (non-list) formula on a focal
layer that carries a `flavor` column SHALL model all rows and emit a
`cli_inform` making that visible. Legacy `make_dependent_events()`-wrapper
data resolves the dependent-object name to (focal layer, its stamped flavor
key) internally, without the inform.

#### Scenario: One modeled flavor selects the dependent rows
- **WHEN** `make_specification(rate = list(creation ~ inertia()), layer = "treaties",
  data = fisheries_treaties)` runs on a layer whose ties carry
  `flavor ∈ {creation, dissolution}`
- **THEN** only `creation` rows are modeled as dependent events while `dissolution` rows
  update the network state, reproducing the legacy filtered-dependent-events model.

#### Scenario: Multiple flavor keys build parallel processes
- **WHEN** `rate = list(creation ~ 1 + indeg(), dissolution ~ 1 + inertia())` is
  supplied on a mutually exclusive flavored layer
- **THEN** the specification carries both processes, each with its formulas and derived
  support constraint, validated as one multi-process specification.

#### Scenario: Mismatched rate/choice key sets build and record the gap
- **WHEN** `rate` keys {creation, dissolution} but `choice` keys only {creation}
- **THEN** the call succeeds, the specification records `dissolution`'s missing
  choice as a completion gap, the estimators model `dissolution` in the rate
  family only, and the generative consumers complete its choice at their entry.

#### Scenario: Unknown flavor key rejected
- **WHEN** a formula list keys `deletion` but the layer's flavors are
  {creation, dissolution}
- **THEN** the call aborts listing the available flavor values.

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
