## Why

DyNAMi is the last model family whose data flows through the legacy
`data.goldfish` **environment** at the public surface: `make_groups_interaction()`
plus the constructors still hand `estimate_dynami()` an environment, which is the
sole remaining reason `make_specification()` / `estimate_*()` cannot reject
environments outright (the single-object change's deferred legacy-environment
abort — see its task 8.3 conformance note). The full DyNAMi engine conversion
(`refactor-dynami-engine`) is a post-release project; the 2.0.0 release needs
only the **boundary**: DyNAMi accepting the single stocnet data object publicly,
so the abort can land and the release can honestly say "legacy environments are
rejected everywhere".

## What Changes

- **DyNAMi accepts the stocnet data object at the public surface**:
  `estimate_dynami()` **and** `make_specification()` (both surfaces, decided
  2026-07-19) take a stocnet whose actors × groups structure is a two-mode
  layer under the mode map (the representation `multimode-network-support`
  hardens), with the interaction/composition event streams as object
  components.
- **DyNAMi's two rate models use the flavor-keyed grammar on the new
  surface** (decided 2026-07-21): `make_specification(model = "DyNAMi")`
  takes `rate = list(join ~ ..., leave ~ ...)` — the existing flavor-keyed
  list, keys matching the layer's `flavor` stamps — expressing the paper's
  joining and leaving Poisson rates. The boundary desugars the keyed list
  into the legacy per-effect `joining = 1/-1` single-formula encoding the
  untouched monolith consumes, so the flags (and their inconsistent
  defaults) disappear from the public surface; coefficient names render
  flavor labels. `choice` is a plain formula meaning the joining choice
  (leaving is deterministic; a flavor-keyed `choice` is rejected with an
  error saying so). Equivalence-tested against hand-written flag formulas
  on the DyNAMi baselines.
- **The `opportunities` list is replaced by a derived occupancy constraint**
  (grounded 2026-07-19): the stored list is exactly the occupied second-mode
  nodes at each dependent join in event order, so the DyNAMi choice
  specification **auto-derives** `~ indeg(<focal layer>) >= 1` — the existing
  support-constraint grammar, no extension — reproducing today's choice set
  exactly (the joiner's own intermediary singleton included, matching the
  paper's denominator); user `support_constraint`s AND-combine. The internal
  bridge feeds the derived per-event availability to the estimation engine
  through the existing `opportunitiesList` channel (equivalence tested
  against constructor-supplied lists). The dead
  `setopportunities_interaction()` is deleted.
- **`make_groups_interaction()` returns the stocnet directly** (**BREAKING**,
  decided 2026-07-19): the records→events transformation is untouched, but
  the 5-component return (incl. `opportunities`) becomes the assembled
  multipartite object — actors+groups `nodes` with `mode`, the focal
  two-mode `interactions` layer (dependent joins `flavor = "join"`, leaves
  `"leave"`, exogenous rows `NA` = state-only; `order` attributes become the
  reserved `order` column), and the one-mode `past` covariate layer.
- **Internal environment bridge (temporary, explicit)**: at the boundary the
  stocnet is converted down to the environment the untouched
  `preprocessInteraction` monolith consumes. The bridge is an implementation
  detail — created internally, never accepted from the user — and is retired by
  `refactor-dynami-engine` together with the monolith.
- **DyNAMi data assembly**: the DyNAMi env fallback in the legacy wrappers
  (`is_stocnet_assemblable()` returning `FALSE` for interaction data) is lifted —
  `make_data()` with DyNAMi components returns a stocnet, never an environment,
  completing what `multimode-network-support` did for two-mode.
- **The legacy-environment abort lands** (**BREAKING** for objects saved before
  1.9.0): `make_specification()` / `estimate_*()` abort on
  `is.environment(data)` with a `cli` error naming `as_goldfish()` as the
  migration — the deferred single-object requirement, owned here because this
  change removes the last public env producer. The `as_goldfish()` conversion
  path for saved environments already exists.
- **Load-time fixtures cleanup**: `R/zzz_testthat_helpers.R` stops building
  legacy-constructor fixtures under a quieted lifecycle context where the
  stocnet path now serves; DyNAMi test fixtures route through the stocnet
  boundary.
- **Out of scope**: the recipe-loop conversion, the shared `spec_map`/realizer
  unification, and the internal `data_source_envir`/`is_legacy` seam deletion —
  all stay with the post-release `refactor-dynami-engine`.

## Capabilities

### New Capabilities

- `dynami-data-boundary`: the DyNAMi public data surface — stocnet acceptance
  on `estimate_dynami()`/`make_specification()`, the actors×groups two-mode
  representation, DyNAMi `make_data()` assembly to stocnet, and the internal
  (never user-facing) environment bridge to the monolith.

### Modified Capabilities

- `single-data-object`: the legacy-environment requirement tightens to the full
  abort — a `data.goldfish` environment passed as `data` is rejected on every
  estimation surface (the deferral exception recorded at the
  `refactor-single-data-object` archive is removed).

## Impact

- `R/make_data_group.R` (`make_groups_interaction()` stocnet return;
  `setopportunities_interaction()` dead code deleted; transformation logic
  untouched), `R/legacy_wrappers.R` (DyNAMi assembly branch; env fallback
  lifted), `R/model_estimate.R` + `R/make_specification.R` (stocnet
  acceptance for DyNAMi; the flavor-keyed rate desugarer; the auto-derived
  occupancy constraint; the `is.environment(data)` abort), a new internal
  stocnet→env bridge consumed
  by the DyNAMi front-end (`R/model_preprocess_group.R` untouched),
  `R/zzz_testthat_helpers.R` (fixtures cleanup), `tests/testthat/` (DyNAMi
  boundary + abort snapshot tests).
- **Depends on** `multimode-network-support` (two-mode stocnet assembly and mode
  map for actors×groups) and the archived `refactor-single-data-object`
  (`as_goldfish()`, validator, stamp).
- **Sequenced before** `spec-driven-dispatch` and `residuals-gof` in the 2.0.0
  release plan; `refactor-dynami-engine` (post-release) retires the bridge.
- DyNAMi frozen coefficient baselines (1e-6, `NOT_CRAN=true`, PASS not SKIP)
  are the regression floor; the DyNAMi constructor path keeps working unchanged.
