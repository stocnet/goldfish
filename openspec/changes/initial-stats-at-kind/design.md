## Context

On `feature_simulation` after `availability-one-object-per-loop` fixed the
atom store (`7e57527`) and before `likelihood-site-cleanup-and-cost` applies.
See `proposal.md` for the motivation; this section records the shapes the
design starts from, read off the tree on 2026-10-01.

**Kinds.** `R/broadcast_kind.R` defines the lattice (3 global, 2 ego, 1
alter, 0 point), `kind_length()`, `project_value(value, from, to, n1, n2)`
(widen), `reduce_value()` (narrow), `write_entries()` (in place through
`set_entries()`), `read_value_at_entries()`. `classify_broadcast_kind()`
(`R/preprocess_builders.R:400`) maps an effect and its `type` argument to
the kind at plan build; `plan$effects$broadcast_kind` carries it; the sender
loop uses only kind 3 (`global`), the dyad loop kinds 1, 2, 3 for `alter`,
`ego`, `global` and the degree projections, kind 0 for everything else,
including `node_trans` (an omission `closure-effects-refactor` owns).

**Inits.** The dyad-loop inits of the vector-kind effects build the grid
from the rate-side vector: `init_DyNAM_choice.alter` (`matrix(attribute,
n1, n2, byrow = TRUE)`, diagonal zeroed), `init_REM_choice.ego` and
`.global` (matrix from `init_DyNAM_rate.*`), `init_REM_choice.indeg`,
`.outdeg`, `.node_trans` (matrix from the rate-side `stat`, `byrow = (type
== "alter")`), with the DyNAM-choice versions aliasing them and
`init_DyNAM_choice_coordination` dispatching to the choice inits. The rate
loop's inits already return vectors (`init_DyNAM_rate.ego` returns
`attribute`, `.global` `rep(attribute, n1)`, `.indeg` the degree vector)
and a `cache`.

**Updates.** The vector-kind updates return `to_ego(rate_changes, n2)` or
`to_alter(rate_changes, n1)` (`R/utils_effects.R:23-60`): `do.call(rbind,
lapply(...))` over the other actors, skipping the diagonal on one-mode
data. `merged_covariate_step()` hands the rows to
`broadcast_entries_from_updates()` (`R/preprocess_builders.R:520`), which
asserts one value per held index and emits one `(kind, fixed, effect,
replace)` column.

**Storage and consumers of the initial value.** `build_walk_engine()`
assembles `initial_stats` as `array(unlist(lapply(ctx$stat_cache, "[[",
"stat")), dim = c(n1, n2, n_fun))` (`R/preprocess_joint.R:1705`); the
pre-window folds write into it by cell (`:1154-1160`) and the interaction
seeds by cell or by broadcast seed (`:1200-1260`); the interaction operand
state is seeded from its dense slices (`:1714-1745`). The writer stores it
(`assemble_default_output()`), the flavored path projects it per fid by
`effect_map` (`project_initial_stats()`), the compiled interface flattens
each slice with `t()` into `stat_mat_init` (`R/cpp_interface.R:173-177`,
again in the gather render `R/preprocess_writers.R:698-702`), the r backend
copies it into `state$statsArray` per evaluation, the handle seeds its live
dense statistics from it (`walk_seed_live_stats()`), the `preprocessed =`
merge copies slices by index (`R/model_estimate.R:2424-2446`), the DyNAM-i
bridge builds its own 3D array, and `methods_residuals.R`, `utils.R`,
`methods_display.R` read its dimensions. Twenty-six test files read it,
two DyNAM-i preprocessing files 129 times.

**Registry.** `effect-term-registry` D2d plans `stat_kind` and a boolean
`broadcast_eligibility` on `term_def`, with a "review needed" against the
live predicate; no axis value and no init-shape contract.

**References.** The byte-identity families (`constraint_atoms_refs.rds`),
the parity goldens and the per-event pins of `likelihood-site-cleanup-and-cost`
compare the dense initial value and the update stream; the frozen 1e-6
baselines and C++ goldens compare numbers.

Constraints: numbers do not move; no format version bump; the per-event
cost rules apply to the update functions and the walk; a branch writes a
`NEWS.d/` fragment (ADR-0040).

## Goals / Non-Goals

**Goals:**
- `stat_init` at kind on the object; one accessor for the engine-native
  dense form; every consumer through it.
- Inits and updates of broadcast effects at kind; the fan-out gone.
- Byte-identical dense expansion and update stream; baselines and goldens
  PASS.

**Non-Goals:**
- Changing which effects are broadcast-eligible (`node_trans`:
  `closure-effects-refactor`).
- The registry's metadata (`effect-term-registry` D2d), beyond a dated note.
- The effect caches' shape (`closure-effects-refactor` D2).
- Expanding inside the C++ kernels instead of in the R interface; the
  interface's one expansion per fit is the copy the kernels already pay
  (`stat_mat = stat_mat_init`), so moving it buys nothing until the kernels'
  state reconstruction is revisited (an open question of
  `likelihood-site-cleanup-and-cost`).
- DyNAM-i, which keeps its dense 3D initial value behind the accessor.

## Decisions

### D1 — `stat_init` at kind plus `stat_init_kind`; the dense form is an accessor

The object stores `stat_init`, a list with one slice per effect at its
kind, and `stat_init_kind`, an integer vector. `initial_stats(prep)` (new,
in `R/broadcast_kind.R`) returns the engine-native dense form: for a dyad
model an `n1 x n2 x p` array built by `project_value(slice, kind, 0L, n1,
n2)` per slice with the one-mode diagonal zeroed once; for a sender model
the `n1 x p` matrix (a kind-3 slice repeated). It accepts an object carrying
a dense `initial_stats` and no `stat_init` and returns the field, so stale
objects need no bump. Dimension readers use `prep_n_effects()` /
`prep_dims()` helpers rather than the accessor, so they never expand.
*Rejected:* keeping the field name `initial_stats` for the list — the 26
test files and 14 package files that index it by dimension would break
silently or late; a new name fails loudly where a reader was missed, and
the accessor is one line to adopt. *Rejected:* storing a dense array and
only fixing the inits — removes the init-time allocation but keeps the
27.5 MB per effect on the object, which is the cost rule's first clause.

### D2 — Inits return at kind; the diagonal is applied at the read

The REM-choice inits of `alter`, `ego`, `global`, `indeg`, `outdeg`,
`degree` and `node_trans` return the rate-side value (`attribute`, the
degree vector from the rate init's `cache`, the scalar) as `stat` and stop
building the matrix and zeroing its diagonal; `init_DyNAM_choice.alter`
returns `attribute`. Point-kind inits are unchanged. The shape of every
init is pinned by a conformance test over effects x families against
`classify_broadcast_kind()`. The diagonal rule moves to the two places a
grid is formed: the accessor's expansion and the walk's dense reads, which
already carry `op_drop_diagonal` (ADR-0063: kind-shaped state carries the
broadcast, not the diagonal). *Rejected:* keeping a zeroed diagonal in a
vector-kind init — not representable at kind, and the atom store's
`reduce_value()` had to tolerate it; one rule at the read is simpler.

### D3 — Updates emit at kind; the fan-out becomes the oracle

`update_REM_choice_ego/alter/global`, the degree projections and
`node_trans` (once classified) return the rate-side entries, `(node,
replace)` or `(replace)`, as their `changes`; `to_ego()` and `to_alter()`
are deleted. `merged_covariate_step()` maps a kind-1 or kind-2 entry to a
`stat_mat_broadcast` column with `fixed = node` and a kind-3 entry to
`fixed = 0`, without `broadcast_entries_from_updates()`'s grouping; that
function moves to a test helper where it collapses the eager fan-out and
the equality of the two streams is asserted over whole sequences. The
pre-window branch writes a kind entry into the `stat_init` slice through
`write_entries()` instead of a cell block into the dense array. *Rejected:*
keeping the fan-out and collapsing faster — the allocation is the fan-out.

### D4 — Consumers expand once through the accessor

`make_engine_evaluator()` and the gather render build `stat_mat_init`
(`n1*n2 x p`, sender-major) from the slices directly, one projection per
slice into the flat matrix, with no intermediate 3D array; the r backend's
`estimate_int()` expands once per evaluation into `state$statsArray`
(today's copy, same size); `walk_seed_live_stats()` does the same for the
handle; `project_initial_stats()` subsets the slice list by `effect_map`;
the `preprocessed =` merge copies slices by index and keeps `stat_init_kind`
aligned; the DyNAM-i bridge sets `stat_init_kind` to zeros with its 3D
slices, so the accessor is the identity there. The walk's interaction
operand seeds read operands at kind (`read_value_at_entries()` and
`project_value()` to the union kind), replacing the dense-slice copies at
`R/preprocess_joint.R:1714-1745`. *Rejected:* a lazy memoized dense array
on the object — a 27.5 MB per effect cache that defeats the storage change.

### D5 — Detector-first; references compare the dense expansion

Before any edit, the dense `initial_stats` and the update stream of every
reference family and parity golden are captured; after D1 to D4 the
accessor's output and the stream are asserted byte-identical to them. The
kept-field comparisons of `helper-constraint-atoms.R` and
`helper-parity-fixtures.R` compare `initial_stats(prep)` rather than the
field. The per-event pins of `likelihood-site-cleanup-and-cost`, the frozen
baselines and the C++ goldens are the end-to-end floor.

### D6 — The registry gets a dated note, not an edit of its decisions

`effect-term-registry` design D2d replaces `broadcast_eligibility` with
`broadcast_kind`: `0` to `3` as `classify_broadcast_kind()` encodes them,
plus `4` for a kind that depends on a user argument (the degree family and
`node_trans`, whose `type` selects `1` or `2`), resolved to `1` or `2` by
`construct_term()` at construction so nothing is looked up per event
(Alvaro, 2026-10-01); the init-shape contract (`stat` at that kind) joins
it. Written at proposal time as a dated note in its design and task 1.1a,
claimed first per ADR-0036. `closure-effects-refactor`'s D2
(caches at kind) is consistent with this change's contract and needs no
note beyond the conformance test re-run by whichever lands second.

## Storage and per-event cost

`n1`, `n2` the sender and receiver counts, `p` effects, `v` of them
vector-kind, `E` stored events, `C` attribute changes of vector-kind
effects.

**D1 (storage).** Stored object: `stat_init` replaces the dense array: a
point-kind slice is `n1 * n2` doubles as before, a vector-kind slice `n1`
or `n2` doubles, a global one. Size moves by `-(v * n1 * n2 - sum of v
vectors) * 8` bytes: at CollegeMsg scale 27.5 MB per vector-kind effect
down to 15 KB. Prediction: the budget fixture (`choice ~ inertia + recip +
trans`, all point-kind) does not move; a `choice ~ inertia + indeg +
alter(x)` object at 1899 actors shrinks by about 55 MB; Fisheries and
Social Evolution objects shrink by one slice per vector-kind effect.
Allocation of the walk: the dense array is not built, so minus `v` grids
per preprocessing.

**D2 (inits).** Per init: a vector of `n1` or `n2` instead of a grid plus
its `diag<-` copy; the atom store's seed already reads at kind.

**D3 (updates).** Per event with a vector-kind attribute change: one
entry (two doubles) instead of an `(n - 1) x 3` matrix built by `rbind` over
a list plus the grouping pass. Prediction on the cost-rules reference
fixture: a degree effect's per-event allocation from 0.20 MB to about 0.03
MB, the dyadic baseline; the broadcast stream byte-identical.

**D4 (expansion).** Per fit on the compiled and gather backends: one
projection of each slice into the flat `n1*n2 x p` matrix, the same
`n1 * n2 * p` writes today's `t()` flatten performs, without the 3D
intermediate (today: dense array stored plus flat copy; after: flat copy
only). Per evaluation on the r backend: the same expansion today's copy
performs. The handle's seed likewise. Reads: `prep_dims()` for dimensions,
never an expansion.

**Guards.** D1: object size of a one-mode choice model with `alter(x) +
indeg(net)` against `inertia` alone on a 1500-actor generated fixture, the
difference under 1 MB (today two grids, 36 MB), recorded failing first. D3:
`bench::bench_memory()` of one vector-kind update bounded independent of
`n`, recorded failing first. D4: a count of dense arrays built per fit
(one, through the accessor), and `bench::bench_memory()` of the compiled
interface's `stat_mat_init` build against today's.

## Risks / Trade-offs

- [A reader of `prep$initial_stats` is missed] → the field is gone, so a
  missed reader fails at once with a `NULL` dimension; the 26 test files
  and 14 package files are enumerated in the tasks; the accessor is the
  one-line fix.
- [The diagonal rule applied at the read differs from the inits' zeroing on
  some path (gather export, DyNAM-i, two-mode)] → the dense-expansion
  byte-identity covers every reference family, the gather stack
  byte-identity covers the export, two-mode has no diagonal rule, and
  DyNAM-i is dense behind the accessor.
- [A kind-1 or kind-2 entry's `fixed` index differs from the collapsed
  fan-out's held index] → the stream byte-identity over full sequences, and
  the oracle test that eager-expands both.
- [The interaction operand seeds at kind change the initial product on a
  one-mode diagonal] → the seeds use the same `read_value_at_entries()` with
  `drop_diagonal` the per-event path already uses; the interaction fixtures
  (`test-interaction_operand_kind.R`) are pinned.
- [Expanding per evaluation on the r backend costs more than today] → it is
  the same number of writes as today's copy; D4's measurement confirms.
- [Stale objects break] → the accessor reads the dense field; a stale-object
  test estimates one on every backend.

## Migration Plan

Commit per task on `feature_simulation`; `NOT_CRAN=true` green with
baselines and goldens PASS at each. Order: references and guards (D5) →
accessor and storage with every consumer moved (D1, D4) while inits are
still dense (byte-identity holds trivially) → inits at kind with the
conformance test (D2) → updates at kind with the oracle (D3) → the registry
note (D6) → close. `status: landed (feature_simulation, awaiting fold)` at
the end; no archive on the branch.

## Open Questions

- Whether the compiled kernels should later take the slices and expand
  inside, saving the R-side flat copy per fit; tied to
  `likelihood-site-cleanup-and-cost`'s open question on caching the
  reconstructed state across iterations.
- Whether the sender loop's `initial_stats` should also store its kind-3
  effects as scalars (today a length-`n1` vector); small, decided when the
  rate inits are next touched.
