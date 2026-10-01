---
depends-on: likelihood-site-cleanup-and-cost
---

## Why

**Every statistic's initial value is stored as a full dyad grid, and every
broadcast update is first materialized as a grid of rows and then collapsed,
although the broadcast kind that says a value varies on one axis is known at
plan time.** The 2026-06-29 `compact-broadcast-updates` change (living spec
`broadcast-stat-updates`) made the update stream kind-shaped: an `alter()`
change is one `stat_mat_broadcast` column, not `n1` point cells. It left two
things dense:

- **The initial value.** `flat-preprocess-output` requires `initialStats` to
  be `n1 x n2 x p` for every dyad model ("engine-native form"). Each
  vector-kind effect's init therefore builds the grid: `alter` with
  `matrix(attribute, n1, n2, byrow = TRUE)`, `ego`, `global`, `indeg`,
  `outdeg`, `degree` and `node_trans` by spreading the rate-side vector over
  the grid and zeroing the diagonal. Measured on Social Evolution (84
  actors), `alter`, `ego`, `indeg` and `node_trans` each add one dense slice
  to the initial value, the same as `inertia` does; at CollegeMsg scale that
  is 27.5 MB per effect for a value two vectors of 7.6 KB describe. A
  constraint atom of vector kind was seeded through the same dense init and
  reduced back to a vector; `availability-one-object-per-loop` fixed the
  atom store (commit `7e57527`) and handed the init itself here.
- **The update fan-out.** `update_*` for the vector-kind effects still
  returns the full fan-out through `to_ego()` / `to_alter()`, one row per
  other actor built by `do.call(rbind, lapply(...))`, and the walk's
  `broadcast_entries_from_updates()` collapses those rows back into one
  entry after checking they carry one value. The per-event cost rules
  record the result: a degree effect allocates 0.20 MB per event against
  0.03 MB for a dyadic effect on the budget fixture.

The engines already know how to expand a kind-shaped update
(`apply_broadcast_updates()` in C++, the R and gather decoders), so an
initial value at kind needs one expansion at state reconstruction, which
today's `stat_mat = stat_mat_init` copy already pays for. The effects
registry (`effect-term-registry`, D2d) plans a boolean
`broadcast_eligibility` on `term_def`; what the init needs is the axis
itself, which `classify_broadcast_kind()` already derives from the effect
and its `type` argument. This change uses that classification now and leaves
the registry to absorb it as `broadcast_kind` on `term_def`.

## What Changes

- **The initial statistics are stored at each effect's broadcast kind.** The
  preprocessed object carries `stat_init`, one slice per effect at its kind
  (a scalar for global, a length-`n1` or length-`n2` vector for ego or
  alter and the degree and `node_trans` projections, an `n1 x n2` matrix
  for point-kind effects; a sender-loop slice is a length-`n1` vector or a
  scalar), and `stat_init_kind`, the kind per effect. The dense
  engine-native array is produced on demand by one accessor,
  `initial_stats(prep)`, which projects each slice with the shared
  `project_value()` and applies the one-mode diagonal rule once. The field
  `initial_stats` leaves the object; an object from before this change,
  carrying a dense `initial_stats` and no `stat_init`, is read by the
  accessor unchanged (no format version bump before 2.0.0, Alvaro
  2026-10-01). **BREAKING** for code reading `prep$initial_stats` directly:
  the accessor is the replacement.
- **Inits return their statistic at kind.** The REM-choice and DyNAM-choice
  inits of `alter`, `ego`, `global`, `indeg`, `outdeg`, `degree` and
  `node_trans` stop spreading the rate-side vector over the grid and return
  that vector (or scalar) as `stat`; point-kind inits are unchanged. The
  diagonal is no longer zeroed in an init: a kind-shaped value carries the
  broadcast, not the diagonal (ADR-0063), and the rule is applied at
  expansion and at every dense read, as the walk's `op_drop_diagonal`
  already does. A conformance test enumerates every effect and family and
  asserts the init's shape is the one its kind implies.
- **Updates are emitted at kind.** The vector-kind update functions return
  `(node, replace)` or `(replace)` entries directly; `to_ego()` and
  `to_alter()` go; the walk maps an entry to a `stat_mat_broadcast` column
  without the fan-out and the collapse. `broadcast_entries_from_updates()`
  survives only as a test oracle that the eager expansion of the kind
  emission equals the old fan-out, cell for cell.
- **Consumers expand once.** The compiled-backend interface, the gather
  builder, the r backend's per-evaluation state, the walk handle's live
  seed, the flavored per-fid projection, the `preprocessed =` effect merge
  and the walk's own pre-window folds and interaction-operand seeds read
  or write the slices at kind through the shared core
  (`project_value()`, `write_entries()`, `read_value_at_entries()`).
  DyNAM-i keeps its dense 3D initial value (kind 0 for every effect) and
  passes through the accessor untouched.
- **Not changed**: numbers. The dense expansion equals today's initial
  value byte for byte on every reference family and parity golden, the
  broadcast stream is byte-identical, the frozen baselines and C++ goldens
  stay PASS. **Not changed**: which effects are broadcast-eligible; the
  missing `node_trans` classification is `closure-effects-refactor`'s. **Not
  in scope**: the registry's metadata (`effect-term-registry` records
  `broadcast_kind` on `term_def` and the init-shape contract as its own
  D2d, noted there by this change); the effect caches' shape
  (`closure-effects-refactor` D2 already puts them at kind).

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `flat-preprocess-output`: MODIFIED "initialStats retained in engine-native
  form" (stored at kind as `stat_init` plus `stat_init_kind`; the
  engine-native dense form is an accessor's product at call time; both
  scenarios kept with the accessor's dimensions).
- `broadcast-stat-updates`: ADDED "A broadcast effect seeds and emits at its
  kind" (init shape per kind, update entries emitted without a fan-out,
  the eager expansion as the equivalence oracle, the diagonal applied at
  the read).

## Impact

- **Code**: `R/functions_effects_DyNAM_choice.R`, `R/functions_effects_REM.R`
  (the vector-kind inits and updates; `init_DyNAM_choice_coordination`
  dispatches to the choice inits), `R/functions_effects_DyNAM_rate.R`
  (`global` and `ego` updates, unchanged shape), `R/utils_effects.R`
  (`to_ego()`, `to_alter()` deleted), `R/preprocess_builders.R`
  (`broadcast_entries_from_updates()` to a test oracle; the kind-entry
  mapping), `R/preprocess_joint.R` (`build_walk_engine()`'s initial value
  at kind, the pre-window folds and interaction seeds on kind-shaped
  slices, `walk_seed_live_stats()`), `R/preprocess_writers.R`
  (`assemble_default_output()` writes `stat_init` and `stat_init_kind`;
  the gather render expands through the accessor), `R/preprocess_flavored.R`
  (`project_initial_stats()` over the slice list), `R/cpp_interface.R`
  (`make_engine_evaluator()` expands once per fit), `R/estimation_core.R`
  (`estimate_int()` expands per evaluation), `R/model_estimate.R` (the
  `preprocessed =` merge), `R/dynami_bridge.R` (dense pass-through),
  `R/methods_residuals.R`, `R/utils.R`, `R/methods_display.R` (dimension
  readers use the accessor), new accessor in `R/broadcast_kind.R`.
- **Per-event code touched**: the vector-kind update functions (an entry
  instead of an `n`-row fan-out) and the walk's broadcast mapping. No new
  walk, buffer format or second pass: the slices live at the kinds
  `R/broadcast_kind.R` defines, written through `write_entries()` /
  `set_entries()`, expanded through `project_value()`, and the update stream
  keeps the `stat_mat_broadcast` format unchanged.
- **Tests**: 26 test files read `prep$initial_stats`, most asserting
  dimensions or cells; they move to the accessor (the two DyNAM-i
  preprocessing files, 129 reads, keep their dense semantics through it).
  New: the init-shape conformance test, the fan-out equivalence oracle, the
  dense-expansion byte-identity against every reference family and parity
  golden captured before the change, a stale-object read, and the two cost
  guards (object size with vector-kind effects; per-event allocation of a
  degree effect).
- **Numbers**: none move; the expanded initial value and the broadcast
  stream are asserted byte-identical before any old path is deleted.
- **Dependencies**: none added. `depends-on: likelihood-site-cleanup-and-cost`
  (it rewrites `make_engine_evaluator()`, the r backend's state and the
  gather builder, where the expansion lands; its pins are the detector for
  the engines) and through it `availability-one-object-per-loop`. Coordinate
  with `closure-effects-refactor` (its D2 puts the closure caches at kind
  and owns the `node_trans` classification; either order works, the later
  one re-runs the conformance test) and precede `effect-term-registry`
  (which absorbs the kind into `term_def`). Branch `feature_simulation`;
  NEWS.d fragment, no Version bump (ADR-0040).
- **ADRs**: ADR-0112 (initial statistics at kind; inits and updates at kind;
  the accessor as the engine-native form), ADR-0061 (kind-shaped state
  maintained in place), ADR-0063 (the broadcast, not the diagonal),
  ADR-0109 (the atom store seeds at kind, the init handed here), ADR-0054
  (retire what no consumer calls), ADR-0021 (no baseline moves).
