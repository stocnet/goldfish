## Why

goldfish's data entry is a constellation of legacy constructors (`make_nodes()`,
`make_network()`, `link_events()`, `make_dependent_events()`, `make_global_attributes()`,
`make_data()`) producing an *environment* of interlinked objects with events hanging off
attributes — a surface users must learn per package, that preprocessing consumes via
`get()` lookups. The stocnet ecosystem now has a shared data container:
`manynet::make_stocnet()` — one list of `info` / `nodes` / `ties` / `changes` / `global`
tibbles with rich metadata — plus a full construction/manipulation toolkit (`as_stocnet()`,
`bind_ties()`, `join_nodes()`, `filter_nodes()`, `add_info()`, …). goldfish should consume
that single object directly, delete its duplicated data-manipulation surface, and add only
what manynet deliberately leaves open: a stricter validation contract (stocnet reserves —
but does not require — `time`, `update`, `directed`, `focal`, `observation`) and the
conversion into the recipe input contract.

> **Sequencing satisfied (2026-07-12).** `support-constraint-risk-set`,
> `support-constraint-as-stat`, and `refactor-likelihood-compute` are archived on
> `refactor/rate_prep`; this change consumes their state: the `active_1`/`active_2` mask
> factors (composition routing), the shared layer/variable name resolver serving effect
> *and* constraint formulas, and a pipeline in which
> `set_preprocessing_opt(opportunities_list =)` is already deprecated (the opportunity
> list is a preprocessing option, never part of the data — out of scope here).

## What Changes

- **stocnet objects become the data input**: `estimate_dynam()` / `estimate_rem()` /
  `make_specification()` accept a stocnet directly (validated at specification time), and a
  new **`as_goldfish()`** offers optional early validate-and-stamp. Both paths share one
  internal validator; manynet enters **Imports (>= 2.1.0)** — the legacy wrappers delegate
  to `manynet::make_stocnet()`/`from_ties()` and deprecation messages recommend manynet
  code, so a hard dependency is honest; the validator still reads components structurally.
- **No bridge era**: the builders' `envir`/`get()` resolution seam is replaced by the data
  object outright — no version-bridging helpers; the wrappers, builder rewiring, and
  acceptance flip land as one baseline-gated milestone.
- **Two-mode via declared mode *sets***: `info$sender`/`receiver` take one or more
  `nodes$mode` values — identical sets = one-mode over a subset, disjoint sets = two-mode,
  partial overlap aborts. The mode map (id/label ⇄ local index) is carried onto results
  and gather/db exports for postestimation identity.
- **Formula syntax**: nodal attributes resolve as `ego(var)` from `nodes` and global
  attributes from `global` — the `df$var` prefix is deprecation-translated for one cycle.
- **Minimal flavor support**: reserved `ties$flavor` column (American spelling);
  `make_specification()` accepts a flavor-keyed formula list with exactly one modeled
  flavor (unlisted flavors update state only) — required to express the Fisheries
  creation-conditional-on-dissolution model without `make_dependent_events()`, whose
  wrapper stamps flavor on matched rows. `add_flavor()` sugar and multi-flavor
  estimation are deferred.
- **State-at-t helpers**: the vectorized update engine (`methods_update.R`) becomes the
  internal initial-state materializer (replacing the per-event `startTime` fold, and the
  seam for future chunked/parallel preprocessing); new exported helpers evaluate network/
  attribute state at a time point on the new object (upstream `to_time.stocnet` explored
  as a late task).
- **Contract narrowing at the boundary**: required per-layer `update` / `directed` /
  `focal`; `time` required on event layers with a **strict class contract**
  (numeric/POSIXct/Date; `time = NA` rows are pre-observation history initializing state);
  `observation` limited to `event` / `panel`; deterministic event ordering with a reserved
  **`order`** column as final tie-break and an abort on genuinely ambiguous same-time
  `replace` events.
- **Panel layers** (`observation = "panel"`) enter as exogenous covariates under
  **change-list semantics** (rows are updates at wave times; dissolutions are explicit
  value-0 rows); windows on panel layers are forbidden; a panel layer cannot be `focal`. A
  per-layer panel-semantics flag is **reserved** for the future DyNES change
  (snapshot-diff + MCMC/EM augmentation), not implemented.
- **Two-mode via declared metadata only**: a layer with `info$sender`/`info$receiver` maps
  through a mode map (global node id ⇄ per-mode local id) onto goldfish's n1×n2 local index
  spaces, with mode-purity validation; a layer without the declaration is one-mode over all
  nodes. Composition (`nodes$active`, `changes` with `var == "active"`) splits by mode into
  `active_mode1`/`active_mode2`.
- **Observation window**: explicit `start_time` / `end_time` arguments; default is the
  focal layer's dependent-event span.
- **BREAKING (deprecation)**: legacy constructors keep working for one deprecation cycle as
  **stocnet-assembling wrappers** whose lifecycle warnings show the equivalent replacement
  *code* (cli code block interpolating the user's object names); the `make_data_goldfish()`
  alias rides with `make_data()`, deprecated but returning the new structure. Named
  exceptions: `make_groups_interaction()` (rides with the DyNAMi engine change) and the
  1.7.0 camelCase aliases (chain to the new warnings).
- **BREAKING (hard error)**: a legacy `data.goldfish` *environment* (from a saved
  `.rds`/`.RData`) passed to `make_specification()`/`estimate_*()` aborts;
  `as_goldfish(legacy_env)` converts it to the stocnet structure as the one-line migration.
- **Datasets and examples**: prebuilt stocnet data objects `social_evolution` and
  `fisheries_treaties` ship in `data/` (built in `data-raw/`); Rd examples load them
  (no manynet, no deprecation noise at check time), with construction workflows shown in
  the dataset help pages. `Social_Evolution`'s raw `time` columns convert to POSIXct
  `tz = "GMT"` (coefficient-neutral; baselines untouched).
- **Vignettes**: `teaching1.Rmd.orig`/`teaching2.Rmd.orig` rewritten to the stocnet
  workflow and re-knit via the existing precompile flow (precompilation retained;
  dropping it is a release-prep decision).
- **Pre-work**: a dedicated cleanup session sweeps the ~262 OpenSpec-artifact references
  out of code comments (inlining the rationale) plus a DRY pass over the files this change
  touches.

## Capabilities

### New Capabilities
- `single-data-object`: the stocnet input contract — dual entry path (`as_goldfish()` +
  direct), the narrowing validator, event ordering, time/history contract, panel change-list
  semantics, two-mode mode mapping, observation window, component mapping onto the recipe
  input contract, and the legacy-constructor wrapper deprecation. *(Replaces the stale
  2026-06-11 draft spec authored before `refactor-formula-parsing` landed.)*

### Modified Capabilities
- `model-specification`: `make_specification()`/`estimate_*()` `data` argument accepts a
  stocnet (validated, stamped); `layer` resolves against stocnet layer names / `info$focal`
  in place of the bridge-era `dependent.goldfish` lookup.

## Impact

- **R**: new conversion/validation module (boundary + mode map + schedule assembly +
  legacy-environment conversion); `R/make_data.R` constructors become wrappers
  (lifecycle); `R/make_specification.R` and `R/model_estimate.R` data acceptance (stocnet
  in, environments abort); builder functions in `R/preprocess_builders.R` /
  `R/model_preprocess.R` read stocnet components instead of `get(name, prepEnvir)` — the
  environment plumbing goes away. **Recipe loops, writers, estimation, and `src/` are
  untouched** (1e-6 equivalence to the legacy path is the regression gate).
- **DESCRIPTION**: manynet moves from Suggests to **Imports (>= 2.1.0)**.
- **Policy**: strict snake_case for all functions/arguments/objects (`.lintr`
  `object_name_linter`, project CLAUDE.md, memory); camelCase internals migrate as files
  are touched.
- **data/ + data-raw/**: prebuilt `social_evolution` / `fisheries_treaties` stocnet
  objects; `Social_Evolution` times to POSIXct GMT.
- **Docs/vignettes**: construction workflow rewritten around `as_stocnet()` /
  `make_stocnet()`; Rd examples load the prebuilt data objects; teaching vignette `.orig`
  sources rewritten and re-precompiled.
- **Downstream**: DyNAMi (`make_groups_interaction()`, `R/model_preprocess_group.R`) stays
  on the legacy path until its own engine change; the reserved panel-semantics flag is the
  DyNES seam.
