## ADDED Requirements

### Requirement: Flavor metadata lives in layer info; add_flavor stamps it

The package SHALL support flavor semantics on an event layer through **layer-info
metadata**: `flavor_style` (one of `mutually_exclusive`, `redundant`) and
`values_equivalence` (a named vector mapping flavor names to update values, e.g.
`c(creation = 1, dissolution = 0)`). Flavor names MUST be syntactically valid R names
(they appear as formula-list keys). The package SHALL export `add_flavor()`, a thin
goldfish verb that, given a layer and a `values_equivalence` mapping (plus a
`flavor_style`), populates `ties$flavor` from the update values and records the mapping
and style in the layer info. `add_flavor()` SHALL NOT derive or precompute support
constraints. Supported mappings are dichotomous states only: increment layers with ±1
updates and replace layers with 1/0 values; any other encoding SHALL abort with a cli
error explaining that state updates will accumulate (increment) or replace raw values
(replace), advising the user to verify that intent and to consider `weighted = FALSE`
in effect terms.

#### Scenario: add_flavor stamps ties and records metadata
- **WHEN** `add_flavor(x, layer = "friendship", values_equivalence = c(creation = 1,
  dissolution = -1), flavor_style = "mutually_exclusive")` runs on an increment layer
  with ±1 updates
- **THEN** every tie row gains `flavor` per its update value and the layer info records
  the mapping and style; no constraint object is created.

#### Scenario: non-dichotomous mapping rejected
- **WHEN** `add_flavor()` is called with a mapping of three or more values, or on a
  layer whose updates are weighted (non-±1 increments)
- **THEN** the call aborts with a cli error stating only dichotomous state mappings are
  supported and explaining the accumulate/replace consequence of raw values.

#### Scenario: non-syntactic flavor names rejected
- **WHEN** the `values_equivalence` names are not valid R names (e.g. `"tie created"`)
- **THEN** `add_flavor()` aborts suggesting syntactic names, since flavors key formula
  lists.

### Requirement: Multi-flavor specifications derive per-flavor support constraints

When a specification models K flavors on a `mutually_exclusive` layer,
`make_specification()` SHALL derive one support-constraint formula per flavor from the
layer state: the flavor mapped to state value v is supportable only where the current
state differs from v (for a creation/dissolution pair: `creation → ~ !tie(L)`,
`dissolution → ~ tie(L)`). Each derived formula SHALL be AND-combined with any
user-supplied `support_constraint`, producing K compiled masks in the specification
plan's derivations. Layers with `flavor_style = "redundant"` SHALL derive no
constraint. The specification print SHALL show, under each flavor, its formulas and its
derived (and combined) support constraint.

#### Scenario: mutually exclusive flavors derive complementary masks
- **WHEN** `make_specification(rate = list(creation ~ 1 + indeg(), dissolution ~ 1),
  layer = "friendship", data = x)` runs on a mutually exclusive flavored layer
- **THEN** the plan carries two derived masks — creation allowed only on non-ties,
  dissolution only on existing ties — each maintained from the layer's evolving state.

#### Scenario: user constraint composes with the derived one
- **WHEN** the same specification also supplies `support_constraint = ~ !same(dept)`
- **THEN** each flavor's effective mask is the AND of its derived constraint and the
  user constraint, and a dyad excluded by either is out of that flavor's risk set.

#### Scenario: redundant style derives nothing
- **WHEN** the layer's `flavor_style` is `redundant`
- **THEN** no constraint is derived and only user-supplied constraints apply.

### Requirement: Unflavored layers under a flavored model infer a default mapping

When a flavor-keyed formula list targets a layer with no `flavor` column,
`make_specification()` SHALL infer the mapping — increment layers with only ±1 updates
map `+1 = creation` / `-1 = dissolution`; replace layers with only 1/0 values map
`1 = creation` / `0 = dissolution` — emitting a `cli_inform` that states the assumed
mapping. The formula-list keys MUST match the inferred names. Ambiguous encodings
(non-±1 increments, values other than 1/0 on replace layers) or non-matching keys SHALL
abort with guidance to `add_flavor()`. A layer that carries `flavor` values SHALL never
trigger inference — keys resolve against the values present.

#### Scenario: increment layer infers creation/dissolution
- **WHEN** `rate = list(creation ~ 1, dissolution ~ 1)` targets an unflavored increment
  layer whose updates are all ±1
- **THEN** the specification builds with `+1` rows as creation and `-1` rows as
  dissolution, and a cli message states the assumption.

#### Scenario: weighted layer aborts inference
- **WHEN** the same list targets an increment layer with updates in {−3, 2, 5}
- **THEN** the call aborts explaining the mapping is ambiguous and pointing to
  `add_flavor()`.

### Requirement: Single-pass preprocessing emits per-flavor outputs

Preprocessing of a multi-flavor specification SHALL walk the event sequence once:
the union of effects across all flavors' formulas is computed once (an effect shared by
several formulas contributes one statistics computation, referenced by each flavor's
effect map), and the output is one `preprocessed.goldfish` object per flavor. Event
routing SHALL follow the sub-model family: on timed rate sub-models (DyNAM-rate, REM) a
dependent event of flavor g is dependent in flavor g's output and right-censored in
every other flavor's output; on ordered and choice sub-models other-flavor events carry
no right-censoring — they enter only as process-state updates. Each flavor's derived
mask flips segment that flavor's right-censored timeline.

#### Scenario: shared effect computed once
- **WHEN** `indeg(friendship)` appears in both the creation and dissolution rate
  formulas
- **THEN** its statistic updates are computed once during the single pass and both
  flavors' outputs reference them.

#### Scenario: cross-flavor right-censoring on timed models
- **WHEN** a dissolution event occurs at time t in a two-flavor DyNAM-rate model
- **THEN** the creation output records a right-censored event at t (rate-integral
  boundary) while the dissolution output records a dependent event.

#### Scenario: choice sub-models skip cross-flavor censoring
- **WHEN** the same sequence preprocesses the DyNAM-choice sub-model
- **THEN** other-flavor events update process state only and add no right-censored
  entries to a flavor's choice output.

### Requirement: Per-flavor intercept bookkeeping over per-flavor risk sets

Each flavor's `preprocessed.goldfish` object SHALL carry its own intercept scalars:
`n_dep_events` counts that flavor's dependent events, and `avg_active_actors` is the
time-weighted post-constraint count over that flavor's combined (derived + user) mask,
with sub-intervals at every event and mask flip, such that
`log(n_dep_events / total_time / avg_active_actors)` recovers that flavor's baseline
rate over its own risk set. The gating math note (design D2) SHALL verify this
bookkeeping — including whether right-censored cross-flavor events make the average
coincide across flavors — before preprocessing implementation begins, and this
requirement SHALL be amended if the derivation concludes otherwise.

#### Scenario: complementary masks yield complementary averages
- **WHEN** a sparse friendship layer is modeled with mutually exclusive
  creation/dissolution flavors
- **THEN** creation's `avg_active_actors` reflects the (large) non-tie risk set and
  dissolution's the (small) existing-tie risk set, each matching a hand-computed
  fixture value.

### Requirement: Per-flavor estimation returns a sectioned multi-process result

Estimation of a multi-flavor specification SHALL fit each flavor's model separately on
its preprocessed object using the existing engines, and return a container object
holding one result per flavor (and per sub-model for DyNAM). The container's `print()`
SHALL render cli sections per flavor (rate and choice nested within a flavor for
DyNAM); `coef()`, `vcov()`, and `logLik()` SHALL return flavor-named components, with
the container's total log-likelihood the sum over flavors. Estimating flavor g through
the container SHALL produce coefficients identical (within 1e-6) to a standalone
single-flavor specification of flavor g with the equivalent derived constraint supplied
as a user `support_constraint`.

#### Scenario: container equals standalone per-flavor fits
- **WHEN** a two-flavor DyNAM is estimated via the container and each flavor is also
  estimated standalone with the equivalent constraint
- **THEN** all coefficients agree within 1e-6.

#### Scenario: sectioned print
- **WHEN** the container result of a two-flavor DyNAM is printed
- **THEN** the output shows a section per flavor, each with its rate and choice
  estimates, rendered with cli semantic elements and stable under a pinned cli context.

### Requirement: Unmodeled flavors update state only

A flavor present in the layer's data but absent from the formula list SHALL contribute
no dependent events and no likelihood term; its events SHALL update process state and,
on timed rate sub-models, right-censor every modeled flavor's output. `NA`-flavor rows
follow the same state-only convention.

#### Scenario: three flavors, two modeled
- **WHEN** a layer carries flavors {creation, dissolution, renewal} and the formula list
  keys only creation and dissolution
- **THEN** renewal events update the network state and right-censor both modeled
  flavors' timed outputs, and no renewal parameters are estimated.
