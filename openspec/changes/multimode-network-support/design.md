## Context

`refactor-single-data-object` (D7) gave the stocnet input a **mode map**:
`build_mode_map(info, nodes, layers)` resolves, per layer, a `side1`/`side2` pair
of global node-id vectors from `info$sender`/`info$receiver` mode-set
declarations, plus an `is_two_mode` flag and an n1/n2. `remap_layer_refs()`
converts `ties$from`/`to` to per-side local indices; `layer_node_lookup()`
recovers (side, local, global, label). The validator enforces the
identical-or-disjoint side rule (partial overlap aborts) and side purity. The
engine already carries two local index spaces (n1×n2 matrices, per-mode
`active_mode1`/`active_mode2` composition) and threads an `is_two_mode` formal
through the effect families.

What is **missing** for first-class multipartite DyNAM/REM:

1. The legacy wrapper assembler (`is_stocnet_assemblable()` +
   `assemble_stocnet_from_legacy()` in `R/legacy_wrappers.R`) returns `FALSE` for
   two-mode, so `make_data()` two-mode input falls back to a `data.goldfish`
   environment instead of a stocnet.
2. No systematic contract for which effects are meaningful on a two-mode layer;
   `is_two_mode` is honored where threaded but not validated at parse time.
3. Side-pair resolution and `node_lookup` on the two-mode path are exercised only
   on tiny hand-built fixtures — no end-to-end two-mode model, dataset, or
   baseline.

Sequencing: this change depends on the single-object mode map being in place; it
coordinates with `refactor-dynami-engine` (which plans to model DyNAMi as a
two-mode actors×groups stocnet atop this machinery) and is a prerequisite for the
deferred single-object legacy-environment abort.

## Goals / Non-Goals

**Goals:** make multipartite DyNAM/REM (an object with ≥2 modes, each layer a dyad
over a mode-pair) a first-class, tested, documented case; one canonical internal
representation (the mode map); legacy two-mode `make_data()` assembling to
stocnet; a defined effect-validity contract per two-mode layer; a shipped
two-mode dataset + docs; 1e-6 equivalence with the legacy two-node-set path.

**Non-Goals:** a single process spanning 3+ modes at once (hyper-edges /
genuine multipartite events) — the engine stays n1×n2, a *model* is over one side
pair; changing effect **definitions** or the C++ gather; DyNAMi (owned by
`refactor-dynami-engine`, which consumes this foundation); implementing the
legacy-environment abort (waits until DyNAMi is also off the environment).

## Decisions

### D1 — Multipartite = multi-mode object with dyadic layers; engine stays n1×n2
An object carries any number of `nodes$mode` values. Each **layer** is a dyad
over a (sender-set, receiver-set) pair — one-mode when the sets are identical,
two-mode when disjoint (the D7 rule). A **model** is over exactly one focal
layer's side pair, so the engine's two local index spaces are untouched; other
layers (with possibly different mode-pairs) enter as exogenous covariates, and
the existing mixed-effect validation applies to the remapped objects. A single
process over 3+ modes at once would need a >2-way gather; it is **reserved as a
documented seam, not built**. *Rejected:* a 3+-way index space now — a large C++
change for a model class no goldfish user has requested, when the multipartite-
object/dyadic-layer form already covers affiliation, attendance, and membership
processes.

### D2 — The mode map is the one canonical representation; legacy two node-sets translate in
Downstream code sees exactly one two-mode representation: the single `nodes`
tibble + `mode` column, with per-layer sender/receiver mode sets resolved by the
mode map. The historical **two node-set** input (`nodes` + `nodes2`, distinct
`nodes.goldfish` objects) is translated onto it **at the boundary**: the assembler
(D3) fuses the node sets into one tibble, giving each source set its own `mode`
value, and derives `info$sender`/`info$receiver` from which set each layer's
endpoints came from. No downstream branch consumes two node-set objects; the
two-node-set *input surface* deprecates with the constructors. *Rejected:* keeping
two-node-set as a parallel first-class path — double the surface (composition,
identity, effect dispatch) to maintain and keep in numeric sync, for a
representation the stocnet already subsumes.

### D3 — Legacy two-mode → stocnet assembly
`is_stocnet_assemblable()` drops its `two_mode` early-return; the assembler gains
a two-mode branch. For a legacy bundle whose layers reference two node sets:
build one `nodes` tibble as the row-bind of the sets with a `mode` column
(each set → a distinct mode value, preserving labels), remap each layer's
`from`/`to` into the fused id space, and set
`info$sender`/`info$receiver` as the repeated-name character vector (D7 encoding)
naming, per layer, the mode(s) of its sender side and receiver side. A one-mode
layer over one set keeps identical sender/receiver sets; a two-mode layer gets
disjoint sets. Composition/attribute events route by the node's mode into
`active_mode1`/`active_mode2`. The result is a plain stocnet that flows through
the unchanged single-object path — so `make_data()` two-mode returns a stocnet,
**never an environment**. *Rejected:* a separate two-mode assembler distinct from
the one-mode one — the fused-tibble + mode-set output is the same shape either
way; one assembler with a mode-derivation step is simpler.

### D4 — Effect-validity contract per two-mode layer
On a two-mode (disjoint-sides) layer the sender and receiver spaces are different
node sets, so one-mode-symmetry effects are ill-defined. A **validity taxonomy**
is defined per effect and enforced at parse time (a `cli` error naming the effect
and the layer, listing the valid alternatives):

- **Valid on two-mode:** dyadic memory (`inertia`, `tie`), per-side degree
  (`indeg`/`outdeg` read the appropriate side), four-cycle closure (`four`), and
  attribute effects (`ego`/`alter`/`same`/`diff`) with `ego` reading the
  sender-side slice and `alter` the receiver-side slice of the single `nodes`
  tibble.
- **Valid on two-mode (extended, resolved 2026-07-19):** the shared-partner
  effects `common_sender`/`common_receiver` (their two-mode reading is
  four-cycle-like shared-partner counting), and the mixed two-network effects
  (`mixed_trans` family) **when the layer dimensions conform** (e.g. one-mode
  n1×n1 composed with two-mode n1×n2 yields a conforming n1×n2 statistic) —
  each validated against hand-computed counts.
- **Invalid on two-mode:** reciprocity (`recip`), one-mode triadic closure
  (`trans`, `cycle`, `node_trans`), and any effect assuming a square/symmetric
  adjacency; mixed effects whose dimensions do not conform.
- `directed` is vacuous on a two-mode layer (validation notes and ignores it, as
  D7 already states); mask symmetrization never applies.

**Mechanism (resolved 2026-07-19): revise the existing init-method gate, with
the mode map as the source of truth.** Effects already carry a user-fed
`is_two_mode` argument (~69 effect surfaces) that the `init_*` methods read via
`formals(effect_fun)` and answer with ad hoc `stop()` calls — but the flag is
never checked against the data, so a wrong declaration silently computes
meaningless one-mode statistics on a two-mode layer. This change: (a) the
layer's mode map decides two-modeness; the declared `is_two_mode` argument is
validated against it and a disagreement raises a `cli` warning naming the
effect, the declared value, and the layer's actual mode pair; (b) the
per-effect init stops are revised into the taxonomy above (consistent `cli`
errors listing valid alternatives) rather than scattered hand-written stops.
*Rejected:* a new `two_mode_valid` attribute/registry flag — the argument +
init mechanism already exists and `effect-term-registry` (post-release) will
absorb the metadata; silently computing whatever the one-mode code produces —
meaningless statistics; per-call ad hoc guards — they drift and miss families.

### D5 — Estimation surface: side pair from the focal layer's mode map
`make_specification()` / `estimate_*()` resolve the model's `nodes`/`nodes2` (row
and column node spaces) from `mode_map$layers[[focal]]$side1`/`side2` rather than
from two node-set names — the `ds_side_names()`/`ds_side_ids()` seam already does
this; this change makes it the sole path and covers the mixed-layer case (a
covariate layer with a different mode-pair than the focal). The `node_lookup`
(side, local, global, label) already produced by `ds_node_lookup()` carries onto
two-mode results and gather/db exports (single-object task 4.3) — verified here
end-to-end on a real two-mode model. *Rejected:* re-deriving the side pair from
node-set names — that is the legacy comparison the mode map exists to replace.

### D6 — Flagship dataset: `manynet::irps_nuclear` (resolved 2026-07-19)
The flagship two-mode data is **`manynet::irps_nuclear`** — the Haunss & Hollway
(2023, *Network Science*, doi:10.1017/nws.2022.31) German nuclear-phase-out
discourse network: 337 actors × 54 concepts (`type` marks the mode), 1164
claim events with `time` (Date) and `increment = ±1` (supportive vs critical
claims). manynet is already in Imports, so the data is always installed —
goldfish ships **no copy**. Consumption is two-pronged:

- **Vignette (live)**: a **dedicated precompiled vignette** loads
  `data(irps_nuclear, package = "manynet")` and shows the real mnet → stocnet
  conversion, the two-mode effect-validity story, and a DyNAM close to the
  paper's specification. Because the ±1 increments are creation/dissolution
  flavors, the vignette uses the flavor-keyed formula syntax — so
  `flavored-processes` must land **before** this change's vignette task.
- **Baselines (frozen)**: the 1e-6 two-mode coefficient baseline runs on a
  **small frozen subset stored under `tests/`**, not on the live manynet copy —
  an upstream data revision must never break the frozen floor.

*Rejected:* shipping a goldfish `.rda` copy (duplicates data both packages
install); a purely synthetic toy (a real, published DyNAM dataset makes the
vignette and the effect-validity story concrete); pinning baselines to the live
manynet object (outside the repo's control).

### D7 — Coordination with the DyNAMi boundary change and the legacy-environment abort
This change **removes two-mode as a legacy-environment producer** (D3), leaving
DyNAMi as the only other. It does **not** implement the single-object
legacy-environment abort: that is safe only once DyNAMi accepts stocnet at the
public surface. (2026-07-19 release plan: that public-surface rewire is the new
`dynami-stocnet-boundary` change — scheduled after this one — which owns the
abort and the load-time-fixtures cleanup; the full engine conversion and the
internal `data_source_envir`/`is_legacy` seam deletion stay with the post-release
`refactor-dynami-engine`.) *Rejected:* implementing the abort here with
an internal-vs-saved-env stamp hack — fragile, and the clean abort is one step
away once the DyNAMi boundary lands.

### D8 — Baseline strategy: two-mode stocnet ≡ legacy two-node-set to 1e-6
The regression floor is coefficient equivalence between (a) a two-mode model
built as a stocnet with mode sets and (b) the same model built with the legacy
two-node-set constructors (now assembling to stocnet via D3), plus a
mixed-one/two-mode-layer object. Frozen one-mode DyNAM/REM baselines MUST stay
PASS. New two-mode baselines are added under the same `NOT_CRAN=true`
`skip_on_cran()` regime. *Rejected:* asserting only internal shape — coefficients
are the contract that catches a wrong remap.

## Risks / Trade-offs

- **Effect-validity taxonomy is large** (every effect × two-mode) → drive it from
  one `two_mode_valid` registry flag with a single parse-time gate, and cover the
  boundary with a table-driven test, rather than per-effect guards.
- **Legacy two-node-set translation drift** (fused ids, composition routing) →
  the D8 1e-6 equivalence test against the legacy path is the guard; the mode map
  and `node_lookup` are already the tested identity carrier.
- **Baseline stability against upstream data** → the 1e-6 two-mode baseline runs
  on a frozen `tests/` subset of `irps_nuclear`, never the live manynet copy
  (D6); a synthetic fixture backstops the tests until the frozen subset lands.
- **Coordination with `refactor-dynami-engine`** (shared two-mode foundation) →
  keep the mode-map/assembler API stable; DyNAMi consumes, does not rebuild.

## Migration Plan

1. Assembler first (D3): `make_data()` two-mode returns a stocnet, behind the D8
   equivalence test — the legacy one-mode path and env fallback for DyNAMi stay
   untouched.
2. Effect-validity gate (D4) + estimation-surface hardening (D5) on the two-mode
   path.
3. Dataset, docs, vignette section (D6).
4. Rollback = revert the assembler two-mode branch (one-mode and DyNAMi env paths
   are unchanged, so the tree returns to today's behavior).

## Open Questions

- ~~**D6 dataset source**~~ — resolved 2026-07-19: `manynet::irps_nuclear`
  (Haunss & Hollway 2023), consumed live in the vignette with a frozen `tests/`
  subset for the baselines; see D6.
- ~~**D4 boundary cases**~~ — resolved 2026-07-19: `common_sender`/
  `common_receiver` valid (shared-partner/four-cycle reading); mixed effects
  (`mixed_trans` family) valid when dimensions conform; `node_trans` stays
  one-mode-only. See D4.
- ~~Flag location~~ — resolved 2026-07-19: no new flag; the existing
  `is_two_mode` effect argument + init-method gate is revised, with the mode
  map as source of truth and a mismatch warning (see D4 mechanism).
