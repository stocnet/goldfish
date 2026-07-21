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

### D4 — Effect validity derived per argument (rewritten 2026-07-21)
Validity is **not** a property of "effect × two-mode layer". Each effect
statistic is a typed expression over mode sets, and validity is derived from
which side every index position reads. Three distinct roles that a single
boolean cannot separate: the **focal pair** (S1 → S2, from the focal layer's
mode map), each **network argument's own pair** (R1 → R2, from *that
argument's* layer), and each **attribute argument's mode slice**. The contract
is four rules:

- **R1 — Conformability.** Every index into a network argument must type-check
  against the argument's own mode pair: `w[a, b]` needs a's mode in R1 and b's
  in R2; mixed effects `list(A, B)` chain as A: S1 → M, B: M → S2 for a shared
  inner mode M. Compared as **mode sets, never dimensions** — two distinct
  modes of equal cardinality must fail (the current `n1 != n2 ||
  nrow(network) != n1` checks are unsound under multipartite).
- **R2 — Non-degeneracy.** A type-resolved read of a side the argument cannot
  have (e.g. `indeg(type = "ego")` reads column-side membership of the sender:
  needs S1 ⊆ R2; on a same-pair two-mode argument senders never receive) is
  structurally constant → non-identified → `cli` error naming the effect, the
  type value, and the layer. R2 is R1 applied to the type-resolved signature —
  one check covers both.
- **R3 — Definedness.** Each attribute read requires the attribute not-all-NA
  on the mode slice that position reads: `ego` → S1 modes, `alter` → S2,
  `same`/`diff`/`sim`/`ego_alter_interaction` → both sides (scale comparability
  across modes stays the user's substantive call), `tertius`/`tertius_diff` →
  the aggregated side (R1 of the network argument). The gate is
  `!all(is.na(slice))` — a wholly missing slice, not any missing value, since
  partial missingness is imputation's business (D9). Evaluated and reported
  **per mode**, not per side: a side declared over several modes that carries
  values for one and nothing for another is exactly the case the message must
  make legible, so the abort names the offending mode(s) rather than the side
  (decided 2026-07-21).
- **R4 — Per-argument resolution.** Two-modeness resolves per network
  argument's **own** layer via the mode map — never as a blanket from the focal
  layer. The declared `is_two_mode` argument is validated against it; a
  disagreement raises a `cli` warning naming the effect, the declared value,
  and the actual mode pair, and the mode map's reading wins. Effects without
  the `is_two_mode` formal (`tie`, `inertia`, rate `degree`/`triangle`) get the
  same parse-time gate — protection must not key on the formal's presence.

**Corrected taxonomy on a two-mode focal (S1 → S2)**, each entry validated
against hand-computed counts in the table-driven test:

- **Valid:** `inertia`/`tie`; `indeg(type = "alter")` (receiver popularity —
  the Haunss & Hollway core effect); `outdeg(type = "ego")` (sender activity);
  `four` (the canonical two-mode closure); `ego` (S1 slice) and `alter` (S2
  slice); `same`/`diff`/`sim`/`ego_alter_interaction` under both-side
  definedness; `tertius(type = "alter")` with the attribute on the sender side;
  `tertius_diff` (both reads on S1); mixed-family chains that conform by mode
  sets; `global`.
- **Degenerate (rejected, naming the type):** `indeg(type = "ego")`,
  `outdeg(type = "alter")`, `tertius(type = "ego")` on same-pair arguments —
  structural zeros. A one-mode covariate over S1 makes the ego-type readings
  valid again (e.g. rate `indeg` on a one-mode network among senders).
- **Untypeable (rejected):** `recip` (needs a reverse-pair S2 → S1 argument —
  reserved seam, documented); `trans`/`cycle`/`node_trans`/`triangle` (square
  arguments over S1 = S2); `common_sender`/`common_receiver` **on a two-mode
  focal**.
- **Correction to the 2026-07-19 resolution:** `common_sender`/
  `common_receiver` are *not* valid on a two-mode focal — their formulas index
  i and j into the *same* side of the covariate (Σₖ x_ki·x_kj resp.
  Σₖ x_ik·x_jk), impossible over disjoint S1/S2. Their genuine two-mode reading
  is a **one-mode focal with a two-mode covariate** (shared-affiliation
  projection — the "four-cycle-like shared-partner counting" the earlier
  resolution intended). Supported and tested as that case.
- `directed` is vacuous on a two-mode layer (validation notes and ignores it,
  as D7 already states); mask symmetrization never applies.

**Code-vs-derivation findings the implementation must fix** (2026-07-21
exploration; full derivation table in `progress.md`):

1. `init_DyNAM_rate.outdeg` over-rejects two-mode — sender activity is
   well-defined (REM's own gate agrees, rejecting only `type = "alter"`); lift.
2. `init_DyNAM_rate.ego` over-rejects two-mode — the stop guarded the old
   two-node-set "which set owns the attribute" ambiguity that the fused-tibble
   slice dissolves; replace with the S1-slice read (task 3.3).
3. `init_DyNAM_choice.indeg`/`.outdeg` have **no** type gates while their REM
   twins do — `indeg(x, type = "ego")` on two-mode silently computes an
   all-zero statistic (singular Hessian); add the R2 gates.
4. `common_*`/`mixed_*` conformability compares dimensions — switch to
   mode-set comparison through the mode map (R1).
5. The effects vignette's blanket "cannot be used for two-mode networks" is
   wrong for `four`, `same`, `diff`, `sim`, `mixed_trans`, and misleading for
   `common_sender`/`common_receiver` — doc errata task 5.4.

**Mechanism:** as resolved 2026-07-19 (no new registry flag; the existing
argument + init machinery), but the **parse-time gate is the primary site** —
only the parser sees each argument's layer — with the init stops revised into
consistent `cli` errors listing the valid alternatives, and R1/R2 sharing one
signature check. *Rejected:* the per-effect boolean taxonomy this decision
previously specified — it cannot express type-variants (`indeg` ego vs alter),
argument roles (`common_*`'s covariate two-modeness vs focal two-modeness), or
chain conformability; a registry flag now — `effect-term-registry`
(post-release) absorbs the signature vocabulary instead (its D23, see D10);
silently computing whatever the one-mode code produces; per-call ad hoc guards.

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

**Amended 2026-07-21 — what "the sole path" turned out to require.** An audit of
the surviving two-node-set vocabulary found that `ds_side_names()`'s
`"nodes_side1"` / `"nodes_side2"` are synthetic keys, not objects — but three
sites still decide two-modeness by **comparing those two strings**, and they
work only because `ds_side_names()` manufactures `rep("nodes", 2)` versus two
distinct literals specifically to satisfy them. None survives a per-layer answer
on a multipartite object:

- `model_estimate.R:1349` — `if (!identical(.nodes, .nodes2)) is_two_mode <- TRUE`
  on the `preprocessing_init` re-entry path.
- `utils.R:332` — `ReduceBroadcastFlat()`, where it drives diagonal exclusion in
  broadcast expansion.
- `model_spec.R:285` — `if (identical(nodes, nodes2)) abort(...)`.

Plus two user-facing consequences: `new_model_spec()` drops `nodes2` for sender
specs and hard-codes `is_two_mode = FALSE`, which is the two-mode rate crash
recorded in D13; and `print.specification.goldfish` (`methods_display.R:423`)
shows the synthetic keys verbatim, so a two-mode user is told
`nodes_side1 -> nodes_side2` where `ds_layer_mode_pair()` would give the real
mode names.

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
  paper's specification. The ±1 increments are **not** flavors of one process
  (revised 2026-07-21): support and contestation are separate processes never
  summed, so they become two layers used directly in formulas — no
  `flavored-processes` dependency.
- **Baselines (frozen)**: the 1e-6 two-mode coefficient baseline runs on a
  **small frozen subset stored under `tests/`**, not on the live manynet copy —
  an upstream data revision must never break the frozen floor.

*Rejected:* shipping a goldfish `.rda` copy (duplicates data both packages
install); a purely synthetic toy (a real, published DyNAM dataset makes the
vignette and the effect-validity story concrete); pinning baselines to the live
manynet object (outside the repo's control).

**Conversion & model contract (added 2026-07-21, from the paper read +
data inspection; open items in `.plan/irps_nuclear_author_questions.md`):**

- **Activation, not `present`.** The shipped `present` equals `!type` (pure
  mode duplication, no composition info) and MUST NOT map onto stocnet
  `active`. Derived instead: actors always active; each claim activates at
  **its first event time minus a small epsilon** (one second on the
  numeric-converted Date axis) and never deactivates — claims are available at
  their own introduction (decided 2026-07-21), so introduction events are
  ordinary modeled choices and no first-touch exclusion exists. Matches the
  paper's "already introduced" choice-set constraint.
- **Layers, not flavors, for support/contestation.** Ties split by increment
  sign into a `support` layer (focal, +1 increments) and a `contestation`
  layer (−1 flipped to +1 so `indeg(contestation)` counts contestations).
  Contestation is never modeled — covariate only.
- **Dependent events = `default & increment == 1`** (545 events, settled
  2026-07-21 as the working set): the per-period split 104/161/202/78 matches
  the paper's Table 1 (104/185/200/80) exactly in P1 and within ±2 in P3/P4,
  with the whole 24-event deficit concentrated in P2 — strong evidence
  `default` is the paper's modeled-event flag over data missing ~24 P2 events.
  The vignette states this; the author questions track confirmation. The
  non-default support events remain on the support layer as context (they
  update statistics, matching "a claim's popularity can be indicated by any
  actor's support").
- **Model formulas.** Rate: `outdeg(support)` + `ego(power)` + `ego(office)` +
  `ego(individual)`; individual is derived
  `(name != org | is.na(org)) & !office` — imperfect on the 76 `org = NA` rows
  (mixed persons/collectives; author question B4). Choice:
  `indeg(support)` + `indeg(contestation)` + `four(support)` +
  `tertius(support, power, summarizer_fn = max-with-empty→0)` +
  `tertius_diff(support, govt, summarizer_fn = mean)` (the paper's M9 is
  **govt**, not coalition; `govt` has no NAs — non-politicians are
  legitimately FALSE) +
  `tertius(support, party, summarizer_fn = shannon)` for M10.
- **M10 Shannon route: tertius with a custom summarizer over a sentinel
  category — no new covariate.** The tertius update applies `summarizer_fn` to
  the full raw supporter attribute vector (not an incremental cache), so an
  arbitrary Shannon function is admissible. But NA-based exclusion **cannot
  work** (corrected 2026-07-21): `ds_impute_missing()` runs at preprocessing
  entry (`model_preprocess.R:452`), *before* any effect init/update — party's
  202 NAs would be mode-imputed, assigning the most common party to every
  company and NGO. Route: the conversion recodes `party` NA → `0`
  ("nonpartisan") so no NA exists and imputation never fires; the summarizer
  excludes the sentinel:
  `function(x) {x <- x[x != 0]; if (!length(x)) return(0);
  p <- proportions(table(x)); -sum(p * log(p))}`. Returning 0 for the empty
  set also keeps the tertius cache-impute branch (cross-claim mean) dead. Same
  sentinel treatment for the one `power = NA` actor (→ 0); M9 (`govt`) has no
  NAs and needs nothing.
- **Periods via fully-interacted `global()` dummies; the observation window is
  the cross-check** (settled 2026-07-21). Periods encode as replace-only
  global attributes `period2/3/4` changing at the three boundary dates. Rate:
  period mains + period×effect interactions; choice: period×effect
  interactions **only** (a bare global main is constant across the choice set
  and drops out of the conditional logit; `global()` mains stay rate/REM-only).
  The interaction machinery supports a global operand as its own stat kind
  (`kind = 3` single-global entry in the product grouping,
  `preprocess_builders.R:237ff`), so `global(period2) * indeg(support)` is a
  dyad-varying, identified product. Fully interacted, the MLE decouples and
  equals the paper's four separate fits — as ONE estimation per submodel with
  a **joint vcov**, enabling period-difference tests the paper could not do.
  The `start_time`/`end_time` window route
  (`control_preprocessing`, threaded at `model_estimate.R:764-765`) is kept as
  the vignette's **equivalence cross-check**: per-period windowed fits must
  agree coefficient-for-coefficient with the interacted fit. *Rejected:*
  flavor-per-period — exact for choice but wrong for the rate (full-timeline
  exposure charged to a time-partition flavor; periods partition time, flavors
  partition event types on a shared clock).
- **Plots.** Fig 1 (claims/actors per day + period lines) from ties; Fig 2
  (period network slices) via manynet; Figs 3–4 (per-period coefficient
  dot-whiskers) from the interaction fit.

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

### D9 — Per-mode attribute imputation (spike, added 2026-07-21)
`ds_attribute()` already slices by side (`data_source.R:396` via
`ds_side_ids()`), so a *declared* layer imputes per side today. Two
contamination cases remain, matching the "one-mode base polluted by other
modes" concern:

- **(a) Undeclared one-mode layer in a multi-mode object** — the mode map's
  undeclared branch sets side1 = *all* nodes, so `impute_attribute()`'s
  `mean(value, na.rm = TRUE)` (and the categorical mode rule) pools across
  every mode in the fused tibble.
- **(b) Cross-side attributes** (`same`/`diff`/`sim`/`ego_alter_interaction`,
  newly valid under D4) — the `att_override` cache is keyed per *nodeset*, so
  "impute employee NAs from employees, supervisor NAs from supervisors" is
  inexpressible for a read spanning both sides.

A third site (found 2026-07-21): **(c) the tertius effect-cache imputation** —
`update_DyNAM_rate_tertius`'s `impute_changes` branch replaces NA cache entries
with the cross-*claim* cache mean, so a receiver with an undefined summary
inherits the average of all other receivers. The D6 vignette dodges it with
never-NA summarizers; the spike decides whether deliberate missingness
(attributes defined only for one stratum, e.g. `party`) needs a first-class
"no imputation, summarizer handles NA" contract on `tertius`/`tertius_diff`.

Ordering fact that frames the whole decision (confirmed 2026-07-21):
`ds_impute_missing()` runs at preprocessing entry (`model_preprocess.R:452`),
**before** effect init — so attribute NAs never reach any effect, and
summarizer-level NA handling is unreachable today. Pooled imputation therefore
does not merely cross mode slices; it **destroys deliberate stratum
missingness** (party defined only for politicians) before any downstream code
can honor it. The D6 vignette works around this with sentinel categories; the
spike weighs the real fix — per-mode-slice imputation plus a per-attribute
opt-out (or sentinel-aware contract) at the `ds_impute_missing()` seam.

Rule to implement (task 3.5, spike first): imputation computes **within each
mode slice** of the read side — mode is the strongest stratum available — with
the existing warning extended to name the attribute and the mode(s). The spike
reproduces (a) on a fixture, checks whether mean-centering anywhere pools the
same way, and sizes (b) (it may fold into the R3 definedness pass). *Rejected:*
pooled imputation with a warning only — a silently wrong number, exactly the
class of defect the 1e-6 floor exists to catch; deferring to the registry —
the fix is orthogonal to where validity metadata lives.

### D10 — Hand-off to `effect-term-registry` (added 2026-07-21)
The signature vocabulary this change derives (per-argument mode signatures,
attribute reads, type-variant resolution) is recorded in
`effect-term-registry` design **D23** as the replacement for its D2(e)
`one_mode`/`two_mode` booleans, and the corrected taxonomy plus the
table-driven boundary test (task 3.4) become the seed and verification input
for registry population (its task 1.3). This change implements the checks in
the existing init/parse mechanism; the registry later absorbs them as
declarative metadata evaluated by a single signature interpreter — the checks
move, the derivation does not change. *Rejected:* implementing the registry
schema now — it is post-release Layer-1 work with its own change.

### D11 — DyNAMi is assembled too; `estimate_dynami()` carries the abort (decided 2026-07-21)
D3 removes "is two-mode" as the signal that routes a legacy bundle to the
environment — but that was exactly the signal keeping **DyNAMi** on the
environment path. A DyNAMi bundle is structurally indistinguishable from any
other legacy two-mode bundle: `make_network(nodes = actors, nodes2 = groups)`
plus a two-mode `make_dependent_events()` (the vignette's own construction).
`opportunitiesList` is not a discriminator (optional, `NULL` by default).

**Decided:** do not discriminate. `make_data()` assembles *every* legacy bundle
with a node set and a layer, DyNAMi included; `estimate_dynami()` gains the
abort, pointing at `dynami-stocnet-boundary` for stocnet support. Chosen over
stamping the bundle at `make_groups_interaction()` (a constructor-level marker
would be missed by hand-built DyNAMi data) and over shape heuristics (identity
initial matrix + negative increments — a loose conjunction a legitimate two-mode
DyNAM could trip).

Consequence, accepted: the DyNAMi workflow is broken between this change and
`dynami-stocnet-boundary`. Blast radius measured — the DyNAMi tests call the
effect/preprocess functions directly with `model = "DyNAMi"` and never go
through `make_data()`, and `vignettes/dynami-example.Rmd` is precompiled, so
`R CMD check` is unaffected; only `vignettes/precompile.R` and user scripts
break. *Rejected:* a `make_data(..., .legacy = TRUE)` opt-out — a transitional
internal concern does not belong on the deprecated public surface.

### D12 — Attribute effects resolve their read side from the mode map, not from a flag (decided 2026-07-21)
The 0.1 map's §E: the injection guard requires an effect to have **both** a
`network` and an `is_two_mode` formal, but the attribute-only effects (`ego`,
`alter`, `same`, `diff`, `sim`, `ego_alter_interaction`) have `is_two_mode` and
**no `network` formal**. They are therefore never injected and always see the
hardcoded `FALSE` — making their two-mode `stop()`s dead code that also
contradicts D4, which lists them as valid on a two-mode focal.

**Decided (superseded 2026-07-21 by D13 — see below):** leave the injection
guard as it is; each attribute effect's init resolves its **read side** directly
from the mode map, rather than being told a boolean.

**Revised decision (2026-07-21):** inject the flag, sourced **per attribute
position** from the mode map. The original rejection of injection was that it
"would resolve two-modeness as a blanket from the focal layer, exactly what D4's
R4 rule forbids". Under D13 that objection dissolves: an attribute position's
key *is* the mode set it reads, so `is_two_mode` for an attribute effect is
derived from whether its two positions resolve to the same mode-set key — a
per-position fact, not a blanket. What was wrong with `is_two_mode` was never
the flag but its **source**: `!identical(nodes, nodes2)` compared node-set
names, and the mode map replaces that comparison without replacing the argument.

The effect is therefore *told* what the mapping already derived instead of
re-deriving it, which is where the derivation belongs. The ~69 signatures stay
untouched: the flag already exists on them.

*Rejected:* having each init recompute `identical(ego_ids, alter_ids)` —
the mapping resolved this once and can pass it, and duplicating the derivation
in ~20 inits is how the two readings drift apart.

### D13 — Nodal state is keyed by mode set, as network state is keyed by layer (decided 2026-07-21)

**The asymmetry.** `split_stocnet_streams()` treats the three kinds of state
three different ways, and the difference is the whole bug class:

```
network[[layer]] <- remap_layer_refs(mode_map, layer, from, to, nodes)  # LOCAL to that layer
composition      <- split_composition(stream, mode_map, focal)          # LOCAL per focal side
attribute[[v]]   <- data.frame(node = node_global[sel])                 # GLOBAL, never remapped
```

Network state is already per-layer: `state$networks[["member"]]` is `n1 x n2` of
**`member`'s own** side pair, not the focal layer's. Composition is already
per-side with local indices. Only nodal attributes are stored in exactly two
static buckets (`state$nodal` / `state$nodal2`), bound to a side **at parse
time** by `entry$nodeset == nodes` / `== nodes2` — while which side a node
belongs to is a property of the *node*, knowable only at event time. Hence the
walk's `state[[component]][[key]][event_args$node] <- replace` indexes a
side-local vector with a global id.

Two defects reproduced on the multipartite fixture (2026-07-21):

- **Two-mode rate model crashes**: `'x' is too short` — `new_model_spec()` short
  circuits sender specs with `constructor(nodes = nodes, ...)`, dropping
  `nodes2`; `dynam_rate_spec(nodes2 = nodes)` then collapses `n2` to `n1`.
- **Any time-varying nodal covariate on a two-mode layer crashes or corrupts**:
  `alter(size)` with a change on a receiver-side node gives
  `missing value where TRUE/FALSE needed` (`attribute[4]` on a length-2 slice).
  The sender side survives only by the accident that the assembler numbers
  senders `1:n1`. Every existing test misses this because one-mode makes global
  == local.

**Decided:** finish the architecture that networks already have. One rule for
the whole state container:

> Every piece of state is keyed by the node space it lives on, and every event
> stream is remapped into that space's local index space.

For networks the node space is the layer's side pair (already true). For nodal
attributes it is a **mode set**, canonicalized (sorted, `+`-joined): the mode
set determines the ids, since `side1 <- all_ids[mode_col %in% sender_set]`.

```
attend  : actor -> event     views:  "actor"   "event"
member  : actor -> org       views:  "actor"   "org"
coauthor: actor -> actor     view:   "actor"
                                      \___ ONE view, shared by all three layers
```

Keying by mode set rather than by `(layer, side)` matters: two layers declaring
the same side share one view, so there is one copy and one write per event, and
key equality *means* "same node set" rather than being another string
comparison of the kind this change exists to retire.

**Attribute positions.** Each effect declares how many attribute positions it
has and which node space each reads — a property of the effect's signature, not
of the data:

| effect | positions | reads |
|---|---|---|
| `ego(z)` | 1 | focal S1 |
| `alter(z)` | 1 | focal S2 |
| `same`/`diff`/`sim(z)` | 2 | focal S1 and S2, same variable |
| `ego_alter_interaction(z1, z2)` | 2 | focal S1 and S2, different variables |
| `tertius(w, z)` / `tertius_diff(w, z)` | 1 | **R1 of `w`'s own layer**, not S1 |

The dyad-position family resolves against the focal layer — correct, the dyad
*is* the focal layer's. The neighbor-aggregate family resolves against its
network argument's layer: with focal `actor -> event` and a covariate
`w: org -> event`, `tertius(w, z)` type-checks (R2 = event = S2) and reads `z`
on the **org** mode. Resolving it from the focal sender side, as the current
single `effect == "alter"` branch does, reads the wrong mode silently.

**The one-mode path is preserved for free.** On a one-mode focal both positions
of `same(z)` resolve to the same mode-set key, so `get_data_objects()`'s
existing `unique()` collapses them: arity 1, a plain vector, the existing update
path, and the frozen 1e-6 baselines never change code path. On a two-mode focal
the keys differ, arity is 2, and `call_effect_template()`'s existing
`n_attributes > 1` branch hands the init a list — the same machinery
`ego_alter_interaction` already uses. Arity following key identity is why no
special case is needed.

**No dispatch axis.** A second S3 axis (`sim` vs `sim_cross_side`) was
considered and rejected: the entire delta is unwrapping a list instead of a
vector and skipping one `diag()` line, so it reduces no time while doubling the
method surface — and it would re-inflate exactly what the effect refactor's move
from model-dependency to object-dependency deflates. The update functions are
also not generic (they are resolved by name), so the axis would cover inits
only. The information is in an argument; reading an argument is the plainest way
to consume it. Revisit only for per-position side declarations in
`effect-term-registry` (D10/D23), where `tertius`'s "my network argument's
sender side" rule is the seed.

*Rejected:* **(A) split the attribute streams by the focal layer's two sides**
(mirror `split_composition` and stop) — smallest diff and it fixes both crashes,
but it caps the object at two node spaces and leaves `tertius` over a covariate
layer with a different pair reading the wrong mode; this change's own D1
promises exactly that case. *Rejected:* **(B) one global nodes table plus
per-effect id vectors** — a single source of truth, but a network cannot
sensibly be stored globally, so the container would carry two mental models
(layer-local networks, global-plus-ids covariates), and it adds an `ids`
argument to ~69 effect surfaces. The memory and write-amplification arguments
that favored B do not survive contact: node tables are hundreds to thousands of
rows and a variable read three ways is three small vectors.

## Risks / Trade-offs

- **Effect-validity surface is large** (every effect × type-variant × argument
  mode pair) → drive it from the D4 signature rules evaluated in one parse-time
  gate, and cover the boundary with a table-driven test (effect × type ×
  argument pair → accepted/rejected), rather than per-effect guards.
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
- ~~**D4 boundary cases**~~ — resolved 2026-07-19, **corrected 2026-07-21**:
  the per-argument derivation showed `common_sender`/`common_receiver` are
  untypeable on a two-mode *focal* (i and j index the same covariate side);
  their valid two-mode reading is a one-mode focal with a two-mode covariate.
  Mixed effects conform by **mode sets**, not dimensions. `node_trans` stays
  one-mode-only. See the rewritten D4.
- ~~Flag location~~ — resolved 2026-07-19: no new flag; the existing
  `is_two_mode` effect argument + init-method gate is revised, with the mode
  map as source of truth and a mismatch warning (see D4 mechanism).
