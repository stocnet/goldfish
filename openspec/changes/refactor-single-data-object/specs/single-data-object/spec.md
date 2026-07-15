## ADDED Requirements

### Requirement: Dual input path with one validate-and-stamp boundary
The package SHALL accept a `stocnet` object (list of `info`, `nodes`, `ties`, `changes`,
`global` components per `manynet::make_stocnet()`) as the data input for DyNAM and REM
estimation through two entries sharing one internal validator: directly via the `data`
argument of `estimate_dynam()` / `estimate_rem()` / `make_specification()` (validated at
specification time), and via an exported `as_goldfish(x, ...)` that validates early and
stamps the object (subclass marker) without restructuring it. Validation SHALL run
unconditionally at specification/estimation time — the stamp SHALL NOT bypass it (manynet
verbs and plain list assignment mutate the object while preserving the class vector, so a
stamp is not evidence of validity); the stamp serves provenance and print dispatch only.
goldfish SHALL read stocnet components structurally (accepting plain data.frames — tibble
not required) while manynet SHALL be listed in Imports pinned `>= 2.1.0`; manynet
functions are called only where delegation is DRY (wrapper assembly, legacy conversion),
via `@importFrom` or `manynet::` — never `manynet:::`.

#### Scenario: Raw stocnet accepted directly
- **WHEN** `estimate_dynam(formula, data = social_evolution, ...)` is called with a valid
  unstamped stocnet object
- **THEN** the object is validated on the fly and estimation completes.

#### Scenario: as_goldfish validates early and is reused
- **WHEN** `d <- as_goldfish(social_evolution)` succeeds and `d` is passed to two
  `estimate_*()` calls
- **THEN** validation fails early at `as_goldfish()` if the data is invalid; both
  estimations re-validate the stamped object and produce results identical to the
  raw-stocnet path.

#### Scenario: Post-stamp mutation is caught at estimation
- **WHEN** a stamped object is modified after `as_goldfish()` (e.g. piped through
  `bind_ties()` adding rows with character `time`, or edited by list assignment) and then
  passed to `estimate_*()`
- **THEN** the unconditional validation at specification time catches the invalid data
  and aborts — the surviving stamp does not bypass any check.

#### Scenario: Hand-built structural input accepted
- **WHEN** a hand-built list with the stocnet structure (plain data.frames, no manynet
  constructor involved) is passed to `as_goldfish()`
- **THEN** validation and estimation work — the boundary reads structure, not class
  provenance, keeping the test fixtures independent of manynet's release cadence.

### Requirement: Validator narrows the stocnet contract
The goldfish validator SHALL treat manynet input as untrusted (manynet validates
reserved entries only if present — it enforces no per-layer coverage, no `directed`
check, no label uniqueness, and no value classes) and require, beyond
`validate_stocnet()`'s shape checks: **named** per-layer `info$update`
(`"increment"`/`"replace"`), per-layer `info$directed` (one-mode layers), and per-layer
`info$observation` limited to `"event"` or `"panel"` — each covering exactly the distinct
layers present in `ties$layer` (other stocnet observation values abort as unmodeled);
`info$focal` naming a layer (or an explicit argument override); unique `nodes$label`;
list-column `changes$value`/`global$value` unwrapped with per-`var` type consistency and
`var == "active"` values logical; and layer and attribute-variable names syntactically
valid R names (`make.names(x) == x`), so every validated object is expressible in
effect/`support_constraint` formulas without backticks. An optional **`ties$flavor`
column** (American spelling) SHALL be validated as character with syntactically valid
values (they appear as formula-list keys); `NA` flavor entries are allowed (state-only
rows under a keyed specification). Missing or inconsistent entries
SHALL abort with a `cli` error naming the layer and the missing/offending entry.

#### Scenario: Missing update semantics error
- **WHEN** a stocnet lacks `info$update` for a layer referenced in ties (including an
  unnamed or shorter-than-layers `update` vector that manynet accepts)
- **THEN** an informative error names the layer and the missing `update` entry.

#### Scenario: Unmodeled observation type rejected
- **WHEN** a layer declares `observation = "cross-sectional"`
- **THEN** validation aborts stating goldfish models `event` and `panel` layers only.

#### Scenario: Non-syntactic layer name rejected
- **WHEN** a stocnet carries a layer named `"phone calls"`
- **THEN** validation aborts explaining formula referencing requires a syntactic name and
  suggesting a rename (e.g. `phone_calls`).

#### Scenario: Duplicate node labels rejected
- **WHEN** two rows of `nodes` share the same `label`
- **THEN** validation aborts naming the duplicated labels.

### Requirement: Strict time contract with NA-as-history
`time` SHALL be required on every event layer's ties and on `changes`/`global` rows, with
class numeric, POSIXct, or Date (converted to numeric internally); `character` and `mdate`
times SHALL abort with conversion guidance. All layers' time columns MUST be mutually
comparable — integer wave times are allowed only when every layer lives on that axis.
Tie rows with `time = NA` SHALL be treated as pre-observation history initializing the
network state matrices; rows timestamped before `start_time` SHALL fold into the initial
state.

#### Scenario: Character time rejected
- **WHEN** an event layer's ties carry `time` as character
- **THEN** validation aborts telling the user to convert to numeric/POSIXct/Date.

#### Scenario: Mixed time axes rejected
- **WHEN** the focal layer uses POSIXct times and a panel layer uses integer waves
- **THEN** validation aborts explaining the axes are not comparable.

#### Scenario: History ties initialize state
- **WHEN** a layer has ties with `time = NA` and later timed tie events
- **THEN** the initial network matrix contains the NA-time ties and the event schedule
  contains only the timed events.

### Requirement: Deterministic event ordering with reserved order column
Conversion SHALL produce a deterministic event schedule independent of the incoming row
order (stocnet coercions arrange ties by `from`/`to`): sorted by `time`, then dependent
events before exogenous, then component order (ties, changes, global), then layer, then
`from`/`to`. When `ties` or `changes` carry an integer `order` column it SHALL be the
final tie-break in place of `from`/`to`. Multiple `replace` events targeting the same cell
(or the same node-variable) at the same time without an `order` column SHALL abort with a
`cli` error naming the colliding rows; same-time increments commute and pass silently.

#### Scenario: Ordering independent of input arrangement
- **WHEN** the same stocnet is converted before and after `dplyr::arrange(ties, from, to)`
- **THEN** the resulting event schedules are identical.

#### Scenario: order column breaks timestamp ties
- **WHEN** two same-time `replace` events on the same dyad carry `order = 1, 2`
- **THEN** the schedule applies them in `order`, and removing the column makes the same
  input abort with an error naming the ambiguous rows.

### Requirement: Observation window from arguments defaulting to focal span
`start_time` / `end_time` arguments on the specification/estimation surface SHALL define
the observation window; when absent, the window SHALL default to the focal layer's
dependent-event span. No info-metadata window convention SHALL be introduced.

#### Scenario: Explicit window honored
- **WHEN** `estimate_dynam(..., start_time = t0, end_time = t1)` is supplied
- **THEN** preprocessing windows the sequence to `[t0, t1]` with earlier events folded
  into initial state, matching the legacy startTime behaviour.

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
- **THEN** preprocessing output and coefficients agree to within 1e-6 for both engines
  (`default`, `default_c`), and the frozen baselines PASS (not SKIP) under `NOT_CRAN=true`.

#### Scenario: active changes route to composition and the mask factors
- **WHEN** `changes` contains rows with `var == "active"` and logical values
- **THEN** they produce composition changes feeding `active_mode1`/`active_mode2` (the
  support-constraint `active_1`/`active_2` mask factors), not nodal attribute statistics.

#### Scenario: Unknown formula name errors with candidates
- **WHEN** a formula references `inertia(callz)` against an object with a `calls` layer
- **THEN** the error lists the available layer names; the same resolver serves
  `support_constraint` atom references.

### Requirement: Focal layer designates the dependent events
The dependent event stream SHALL be the layer named by `info$focal`, overridable by an
explicit argument; a panel layer SHALL NOT be focal (the error notes that panel-dependent
processes are SAOM territory). Multi-process designation stays reserved for the
multivariate change.

#### Scenario: Focal layer drives dependent events
- **WHEN** `info$focal = "calls"`
- **THEN** calls-layer events are dependent and all other layers are exogenous.

#### Scenario: Panel focal rejected
- **WHEN** `info$focal` names a layer with `observation = "panel"`
- **THEN** validation aborts explaining goldfish models event-stream dependents.

### Requirement: Panel layers are change-list exogenous covariates
A layer with `observation = "panel"` SHALL enter as an exogenous dyadic covariate whose tie
rows are updates applied at their wave times per `info$update`; dissolutions MUST be
explicit value-0 rows (documented — not statically detectable). Wave updates SHALL emit
right-censored statistic updates like other exogenous events. A `window` parameter on an
effect reading a panel layer SHALL abort before preprocessing. A per-layer panel-semantics
flag SHALL be reserved (documented, not implemented) for the future DyNES change that owns
the snapshot interpretation with between-wave augmentation.

#### Scenario: Panel layer updates at waves
- **WHEN** a friendship panel layer has tie rows at wave timestamps
- **THEN** effects reading friendship see the state of the most recent wave at each event
  time.

#### Scenario: Window on panel layer errors
- **WHEN** `estimate_dynam(dep ~ inertia(friendship, window = 30), ...)` runs with
  friendship a panel layer
- **THEN** an informative error is raised before preprocessing.

### Requirement: Layer sides via declared sender/receiver mode sets only
A layer's sides SHALL be defined by `info$sender`/`info$receiver` as **per-layer sets of
`nodes$mode` values** (each node carries exactly one mode; a layer's declaration may name
several, and different layers MAY declare different pairs): identical sets make the layer
one-mode over that subset of nodes; disjoint sets make it two-mode with `from`/`to`
remapped through a mode map (global id + label ⇄ (side, local id)) onto the engine's local
index spaces; partially overlapping sets SHALL abort. The declaration SHALL be a
**character vector whose names repeat once per (layer, mode)**
(`c(survey = "employees", survey = "supervisor", report = "employees")`) — the only
per-layer set encoding `manynet` admits, since it type-checks these entries as character.
An **unnamed** character vector SHALL apply to every layer. A **list** of per-layer sets
SHALL abort with a message showing the vector form: `add_info()` does not validate, so a
list would pass where it is written and abort later inside `bind_changes()`, which does.
`is_two_mode` SHALL be resolved once per layer during mapping and carried on the map,
rather than re-derived downstream from node-set names.
Side purity SHALL be validated (error naming the layer and offending nodes). The focal
layer's side pair defines the model's node sets. A layer without the declaration SHALL be
one-mode over ALL nodes — ties are never inspected to infer modes and `nodes$mode` alone
is an ordinary attribute (documented callout for multimodal objects). Attributes SHALL be
served per side from the single nodes tibble (`ego()` reads the sender-side slice,
`alter()` the receiver-side slice); NAs flow into the existing imputation. Composition
SHALL split by the node's side into `active_mode1`/`active_mode2`. `directed` SHALL be
ignored (with a validation note) on two-mode layers.

#### Scenario: Declared two-mode layer remaps to local indices
- **WHEN** a membership layer declares `sender = "person"`, `receiver = "org"` over a
  nodes tibble with a `mode` column
- **THEN** its events land in an n_person × n_org matrix under per-side local indices and
  effects get `is_two_mode` set as with legacy two-node-set input.

#### Scenario: One object mixes a one-mode and a two-mode layer
- **WHEN** `info$sender` declares `survey = c("employees", "supervisor")` and
  `report = "employees"`, with `info$receiver` declaring
  `survey = c("employees", "supervisor")` and `report = "supervisor"`
- **THEN** `survey` is one-mode over the employee+supervisor nodes while `report` is
  two-mode employees→supervisor, each layer mapping to its own local index spaces.

#### Scenario: The declaration survives the manynet workflow
- **WHEN** per-layer sets are declared as a repeated-name character vector on an object
  assembled with `make_stocnet()` / `add_info()` and then piped through `bind_changes()`
- **THEN** every step validates, and `split()` on the declaration recovers the per-layer
  sets goldfish maps.

#### Scenario: A list declaration is rejected early
- **WHEN** `info$sender` is written as a list of per-layer sets
- **THEN** goldfish aborts naming the field and showing the repeated-name vector form,
  rather than letting the object pass here and abort later inside `bind_changes()`.

#### Scenario: Identical mode sets restrict a one-mode layer
- **WHEN** a friendship layer declares `sender = receiver = c("employees", "supervisors")`
  in a nodes tibble that also contains other modes
- **THEN** the layer is one-mode over exactly the employee+supervisor nodes; ties touching
  any other mode abort as side-impure.

#### Scenario: Partially overlapping mode sets rejected
- **WHEN** a layer declares `sender = "employees"`,
  `receiver = c("employees", "supervisors")`
- **THEN** validation aborts explaining sides must be identical (one-mode subset) or
  disjoint (two-mode), suggesting identical sets plus a `support_constraint`.

#### Scenario: Side impurity rejected
- **WHEN** a tie in a two-mode layer has a `from` node whose mode is not in the sender set
- **THEN** validation aborts naming the layer and the offending node.

#### Scenario: Undeclared layer spans all nodes
- **WHEN** a multimodal object has a calls layer with no `sender`/`receiver` declaration
- **THEN** calls is one-mode over the full node set, with no inference from its ties.

### Requirement: Node identity survives to postestimation surfaces
The mode map SHALL be carried onto preprocessing/estimation results and the gather/db
exports so local indices are always resolvable to the original `nodes` row and `label`:
exports that surface `index_i`/`index_j` SHALL also provide (directly or via an attached
lookup table) the corresponding global node ids/labels, serving residuals, event scores,
and external analyses of exported tables.

#### Scenario: Gather export resolves to labels
- **WHEN** a two-mode model's gather/db export is produced
- **THEN** its `index_i`/`index_j` local indices are joinable to a provided node lookup
  (side, local index, global id, label) without re-deriving the mode map.

### Requirement: State-at-time helpers on the new object
The package SHALL export helpers evaluating a layer's network state and the nodes'
attribute values at a time point `t` from a stocnet/stamped object — honoring `time = NA`
history rows, per-layer `info$update` semantics (last replace wins, increments aggregate),
and an optional `start_time` — implemented on the same vectorized update engine that
preprocessing uses as its initial-state materializer (no per-event loop). Upstream
contribution of a `to_time.stocnet` method to manynet SHALL be explored as a late task
without blocking this change.

#### Scenario: Network state at a date
- **WHEN** the user asks for the treaties layer state at `as.POSIXct("1965-12-31")`
- **THEN** the returned matrix reflects history rows plus all timed updates strictly
  before that time under the layer's update semantics, matching what preprocessing uses
  as initial state for `start_time` at that date.

### Requirement: Attribute references resolve against components without a data-frame prefix
Effect formulas SHALL reference nodal attributes by bare column name (`ego(floor)`)
resolved against `nodes`, and global attributes by their `global$var` name, through the
shared resolver (unknown names error listing candidates; a name matching both a `nodes`
column and a `global` variable SHALL abort as ambiguous). The legacy `df$var` prefix SHALL
be accepted for one deprecation cycle — the prefix is dropped, the bare name resolved, and
a deprecation warning issued once pointing to the bare syntax — then abort.

#### Scenario: Bare attribute name resolves
- **WHEN** a formula uses `ego(floor)` against data whose `nodes` has a `floor` column
- **THEN** the effect reads the nodes column; no data-frame prefix is needed.

#### Scenario: Legacy prefix translated with a warning
- **WHEN** a formula uses `ego(actors$floor)` during the deprecation cycle
- **THEN** the term behaves exactly as `ego(floor)` and a deprecation warning (once per
  session) shows the bare-name replacement.

### Requirement: Legacy environment input rejected with a conversion path
A legacy `data.goldfish` environment passed as `data` SHALL abort (obtainable only from
objects saved before this change): `make_specification()` / `estimate_*()` raise a
`cli` error naming `as_goldfish()` as the migration. `as_goldfish()` SHALL accept such an
environment and convert it: the contained `nodes.goldfish` / `network.goldfish` /
`dependent.goldfish` / global objects and their `attr(x, "events")` streams assemble into
the equivalent stocnet, which is then validated and stamped. Legacy detection SHALL be
`is.environment(data)`; the stamped list SHALL carry class `data.goldfish` ahead of the
stocnet classes, and `print.data.goldfish()` SHALL render the list shape.

#### Scenario: Saved environment aborts at estimation
- **WHEN** a `data.goldfish` environment restored from an `.rds` is passed to
  `estimate_dynam(..., data = old_env)`
- **THEN** the call aborts with an error telling the user to run
  `as_goldfish(old_env)` once and pass the result.

#### Scenario: as_goldfish converts a saved environment
- **WHEN** `as_goldfish(old_env)` is called on that environment
- **THEN** it returns a stamped stocnet whose estimation reproduces the legacy
  coefficients to within 1e-6.

### Requirement: Prebuilt stocnet datasets with human-readable times
The package SHALL ship prebuilt stocnet data objects — `social_evolution` and
`fisheries_treaties` — assembled in `data-raw/` scripts (the shipped objects are plain
lists of tibbles). Rd examples SHALL load the prebuilt objects rather than construct data
(no deprecation warnings at example runtime); construction workflows SHALL appear in the
dataset help pages and as a dedicated vignette section. The raw
`Social_Evolution` data frames SHALL keep their names and columns with `time` converted
to POSIXct `tz = "GMT"`; the conversion SHALL be coefficient-neutral so the frozen 1e-6
baselines PASS without regeneration.

#### Scenario: Example loads prebuilt object cleanly
- **WHEN** an Rd example runs `data("social_evolution")`
- **THEN** the object loads and estimates with no deprecation warning emitted and no
  constructor call in the example body.

#### Scenario: Raw times are human-readable and baseline-neutral
- **WHEN** `data("Social_Evolution")` is loaded after the change
- **THEN** `calls$time` is POSIXct in `tz = "GMT"` dated 2008, `as.numeric(calls$time)`
  equals the previous numeric values exactly, and the frozen coefficient baselines PASS
  under `NOT_CRAN=true`.

### Requirement: Legacy constructors deprecated as stocnet-assembling wrappers
The legacy constructors SHALL keep working for one deprecation cycle as wrappers assembling
the same stocnet representation the direct path consumes — `make_nodes()`, `make_network()`,
`make_dependent_events()`, `make_global_attributes()`, `link_events()`, `make_data()`, and
the `make_data_goldfish()` alias (which follows `make_data()` and returns the new
structure) — delegating assembly to `manynet::make_stocnet()`/`from_ties()` rather than
re-implementing it, so both paths share the downstream pipeline and there is no bridge
code: builders resolve only the stocnet shape at every commit.
`make_dependent_events()` SHALL express the legacy subset-dependent pattern through
flavor: its events join to the default network's layer rows (time, sender, receiver,
update value) and matches are stamped `flavor = <dependent object name>`, which the
specification surface resolves internally; unmatched dependent rows are added as
flavored increment-0 rows on increment layers and abort with guidance on replace layers.

#### Scenario: Filtered dependent events reproduce via flavor
- **WHEN** legacy code runs
  `make_dependent_events(bilatchanges[bilatchanges$increment == 1, ], nodes = states,
  default_network = bilatnet)` and estimates the Fisheries model
- **THEN** the wrapper stamps the matching treaty-layer rows with the dependent object's
  flavor, only those rows are modeled, dissolutions still update state, and coefficients
  match the frozen baselines to 1e-6. Each SHALL emit a lifecycle deprecation whose message shows the
equivalent replacement code as a cli code block interpolating the user's object names.
Exceptions: `make_groups_interaction()` (unchanged until the DyNAMi engine change) and the
1.7.0 camelCase aliases (chain to the new warnings without a second wrapper layer).

#### Scenario: Legacy path still estimates identically
- **WHEN** a model is specified with the legacy constructors after this change
- **THEN** estimation completes with deprecation warnings and coefficients identical to
  the stocnet path to within 1e-6.

#### Scenario: Deprecation shows replacement code
- **WHEN** `make_network(m, nodes = actors)` is called
- **THEN** the warning renders a code block suggesting the stocnet equivalent using the
  caller's names (e.g. `as_stocnet(m) |> join_nodes(actors)`).

#### Scenario: DyNAMi constructor untouched
- **WHEN** `make_groups_interaction()` is called
- **THEN** no new deprecation is emitted and the DyNAMi path behaves as before this change.
