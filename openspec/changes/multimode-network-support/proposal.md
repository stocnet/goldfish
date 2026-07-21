## Why

`refactor-single-data-object` built the **mode map** (its design D7): per-layer
`info$sender`/`info$receiver` mode sets that give a directly-constructed stocnet
two local index spaces (n1×n2) and let one object mix one-mode and two-mode
layers. But two-mode / **multipartite** DyNAM and REM are not yet a first-class,
tested, documented case:

- the legacy `make_data()` path **cannot assemble** two-mode input —
  `is_stocnet_assemblable()` explicitly returns `FALSE` for it, so two-mode data
  built with the constructors falls back to a legacy `data.goldfish`
  **environment** (the last non-DyNAMi producer of the environment the
  single-object change set out to retire);
- there is no defined contract for **which effects are valid on a two-mode
  layer** (one-mode reciprocity/transitivity are meaningless across disjoint
  sides) — the `is_two_mode` dispatch is threaded through effect families but not
  systematically validated;
- the estimation surface's **side-pair resolution** (the model's `nodes`/`nodes2`
  from the focal layer's mode map) and **node identity** on results/exports are
  proven only on tiny hand-built fixtures;
- there is **no shipped two-mode dataset**, construction doc, or vignette section
  for the multipartite workflow, so the capability is effectively undiscoverable.

This change makes multipartite DyNAM/REM a first-class case on the existing mode
map, canonicalizing the representation and closing the legacy two-mode assembly
gap.

## What Changes

- **Multipartite = multi-mode object, dyadic layers.** An object may carry any
  number of `nodes$mode` values; each *layer* remains a dyad over a pair of
  mode-sets (the engine stays n1×n2), and a *model* is over one focal layer's
  side pair. A single process spanning 3+ modes at once (hyper-edges) is **out of
  scope** — a documented reserved seam only.
- **The mode map is the one canonical representation.** The legacy **two
  node-set** form (`nodes` + `nodes2`) is translated onto the single
  nodes-tibble + `mode`-sets mode map at the boundary, so one internal path
  serves both; the two-node-set input surface deprecates with the constructors.
- **Legacy `make_data()` assembles two-mode into a stocnet.** Extend
  `is_stocnet_assemblable()` + the wrapper assembler to build a `nodes` tibble
  with a `mode` column and set `info$sender`/`info$receiver` mode sets from the
  supplied node sets — so `make_data()` two-mode input returns a **stocnet, never
  an environment**. **This removes two-mode as a legacy-environment producer**
  (DyNAMi remains the only other; together with `refactor-dynami-engine` this
  unblocks the deferred single-object legacy-environment abort — the abort itself
  is not implemented here).
- **Effect validity per two-mode layer.** Define and enforce which effects are
  valid on a two-mode layer (e.g. four-cycle / per-side degree) versus rejected
  (one-mode reciprocity, transitivity, `mutual`-style closure), with `cli` errors
  naming the effect and the layer; harden the `is_two_mode` dispatch across the
  DyNAM rate/choice/coordination and REM effect families.
- **Nodal state keyed by mode set** (added 2026-07-21, design D13). Network
  state is already keyed by layer and remapped into that layer's local index
  space; nodal attribute state is not — it sits in exactly two buckets bound to
  a side at parse time, and its event streams keep global node ids. Two defects
  reproduced from that asymmetry: a two-mode **rate** model crashes
  (`'x' is too short`), and **any time-varying nodal covariate on a two-mode
  layer** crashes or writes the wrong node. Nodal state becomes keyed by the
  mode set it is read on, with streams split per view — one rule for the whole
  state container, and the cap of two node spaces lifted.
- **Estimation surface & identity.** `make_specification()` / `estimate_dynam()`
  / `estimate_rem()` accept multipartite objects (mixed one/two-mode layers);
  the model's side pair (`nodes`/`nodes2`) resolves from the focal layer's mode
  map; the `node_lookup` (side, local, global, label) carries onto
  results/exports for two-mode.
- **Flagship two-mode dataset + docs.** `manynet::irps_nuclear` (Haunss &
  Hollway 2023, the two-mode nuclear-discourse DyNAM paper) is the flagship —
  consumed live from manynet (already in Imports, no shipped copy) in a
  dedicated precompiled vignette showing the mnet → stocnet conversion (support
  and contestation as separate two-mode layers) and a paper-inspired model,
  plus a `?goldfish_data` multipartite section; the two-mode baseline runs on a
  frozen `tests/` subset.
- Coefficient equivalence (two-mode stocnet vs the legacy two-node-set path, and
  mixed one/two-mode-layer objects) to 1e-6 is the regression floor throughout.

## Capabilities

### New Capabilities

- `multimode-networks`: multipartite (multi-mode object, dyadic layers over
  mode-pairs) support for DyNAM/REM — the canonical mode-map representation and
  legacy two-node-set translation, the legacy two-mode → stocnet assembly, the
  per-two-mode-layer effect-validity contract, node identity on two-mode
  results/exports, and the shipped two-mode dataset.

### Modified Capabilities

- _None._ The specification/estimation-surface behavior (accepting a multipartite
  object, resolving the model's side pair from the focal layer's mode map) is
  captured as ADDED requirements in `multimode-networks` rather than as a delta
  against `model-specification`, whose "layer identifies the dependent process"
  requirement is still being modified by the active `refactor-single-data-object`
  change — a second concurrent delta would collide at archive.

## Impact

- `R/legacy_wrappers.R` (`is_stocnet_assemblable` + the assembler: two-mode
  branch), `R/mode_map.R` / `R/data_source.R` (side-pair + `node_lookup` on the
  two-mode path), the effect families
  (`R/functions_effects_DyNAM_*`, `R/functions_effects_REM.R`) and
  `R/formula_parser.R` (effect-validity dispatch), `R/make_specification.R` /
  `R/model_estimate.R` (multipartite acceptance), `tests/testthat/` (frozen
  `irps_nuclear` baseline subset), `man/` + `vignettes/` (new dedicated
  vignette; no `data/` addition — the dataset stays in manynet).
- **Depends on** `refactor-single-data-object` (the mode map D7, `node_lookup`,
  the validator's identical-or-disjoint side-purity rules) being in place.
- **Coordinates with** the DyNAMi changes (both model DyNAMi as a two-mode
  actors×groups stocnet — this change hardens the two-mode foundation): the
  `dynami-stocnet-boundary` change (public-surface stocnet acceptance, scheduled
  after this one) owns the deferred single-object **legacy-environment abort**
  (removing two-mode as an env producer here is its prerequisite); the full
  engine conversion stays with the post-release `refactor-dynami-engine`.
- **No `flavored-processes` dependency** (revised 2026-07-21): the
  `irps_nuclear` ±1 increments are *not* creation/dissolution flavors of one
  process — support and contestation are separate processes never summed, so
  they convert to two **layers** (focal `support`; `contestation` with −1
  flipped to +1, covariate only), used directly in formulas. The paper's four
  periods are reproduced with fully-interacted `global(period)` dummies (one
  estimation per submodel, joint vcov), cross-checked against per-period
  `start_time`/`end_time` windowed fits — no flavors anywhere.
- Frozen DyNAM/REM one-mode baselines MUST stay PASS (not SKIP); new two-mode
  baselines join the floor.
