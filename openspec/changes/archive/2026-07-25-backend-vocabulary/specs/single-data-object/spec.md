# single-data-object (delta)

Wording-only: the equivalence scenario names the compute backends a user
selects. The recipe-input contract itself is unchanged.

## MODIFIED Requirements

### Requirement: Component mapping onto the recipe input contract
The stocnet components SHALL map onto the recipe input contract (state container, event
schedule, update plan) exclusively through the builder layer; recipe loops, writers,
estimation, and the C++ core SHALL NOT change. The mapping: `info` → object registry
(layers, directed, update, focal); `nodes` → labels, attribute columns, `active` initial
composition; `ties` → history (time = NA) into initial matrices, focal-layer rows into
dependent events — **filtered by the modeled flavor when the specification keys one**
(non-matching and `NA`-flavor focal rows enter as state updates only, while every focal
row still updates state per `info$update`) — other layers into exogenous network events;
`changes`
(`time`,`node`,`var`,`value`) → nodal attribute events, `var == "active"` routing to
composition; `global` → global attribute events. The `get(name, envir = prepEnvir)`
object-resolution SHALL be absent from the stocnet path. Name resolution for effect AND
`support_constraint` formulas SHALL go through one resolver against the stocnet's layer
and variable names, erroring with the available candidates on an unknown name.

#### Scenario: Untouched downstream pipeline
- **WHEN** a model expressible in both input formats is estimated via the stocnet path and
  the legacy path
- **THEN** preprocessing output and coefficients agree to within 1e-6 for both compute
  backends (`r`, `cpp`), and the frozen baselines PASS (not SKIP) under `NOT_CRAN=true`.

#### Scenario: active changes route to composition and the mask factors
- **WHEN** `changes` contains rows with `var == "active"` and logical values
- **THEN** they produce composition changes feeding `active_mode1`/`active_mode2` (the
  support-constraint `active_1`/`active_2` mask factors), not nodal attribute statistics.

#### Scenario: Unknown formula name errors with candidates
- **WHEN** a formula references `inertia(callz)` against an object with a `calls` layer
- **THEN** the error lists the available layer names; the same resolver serves
  `support_constraint` atom references.
