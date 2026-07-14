> **⚠ Maturity: SECOND-PASS DESIGN (2026-07-09).** The explore session settled
> scope layering, the cache contract, the specialization strategy gate, and the
> naming/argument decisions for the successor changes (see D13–D22 and
> `.plan/effect_refactor_proposal.tex`, the working catalogue). Still required
> before implementation: the **code-grounded sweeps** of task 1.5 (`stat_kind`
> ownership, broadcast-eligibility predicate, full abort inventory,
> `refactor-single-data-object` sequencing) and the **encoding benchmark spike**
> (D15) that gates D4's mechanism choice.

## Context

Today an effect/term is implicitly defined by the coexistence of several
artifacts discovered by naming convention:

- `update_<model>_<submodel>_<name>()` — the per-event update; located by
  `eval(parse(text = paste("update", model, sub_model, name, sep = "_")))` in
  `parse_multiple_effects()` (`R/formula_parser.R:201`).
- `init_<model>_<submodel>.<name>()` — the S3 initializer; located by
  `getS3method()` (`R/formula_parser.R:214`).
- Argument behaviour encoded as runtime branches inside those functions, e.g.
  `weighted` switching `sign()` vs `identity()`/transformer
  (`update_DyNAM_choice_tie`, `R/functions_effects_DyNAM_choice.R:282`), and
  `history` branching in closure effects.
- Validity rules scattered: `is_two_mode` inference + warnings in the parser
  (`R/formula_parser.R:246-267`), window-not-allowed `cli_abort`s, and `stop()`s
  inside init methods.
- Presentation metadata fragmented: `@aliases` roxygen tags, the endogenous
  alias done by `init_DyNAM_choice.inertia` *calling* `init_DyNAM_choice.tie`
  (`R/functions_effects_DyNAM_choice.R:308`), and the hard-coded
  `.goldfishEffectShort` abbreviation map plus `.decoderColumns` added by
  `compact-term-summary` (`R/utils.R`).

There is no object you can point at that answers "what is the `trans` effect:
what does it accept, where is it valid, what is its short name, what is its
endogenous/exogenous twin?". Every consumer re-derives a slice of that.

Constraints: estimation numerics and the frozen coefficient baselines
(`tests/testthat/_baselines/`) must not change (1e-6 floor, PASS not SKIP). User
formula syntax must not change. The package supports DyNAM (rate/choice/
choice_coordination), REM (rate/choice/rate_ordered) and DyNAMi (rate/choice),
one-/two-mode and directed/undirected networks.

## Goals / Non-Goals

**Goals:**
- One declarative **registry entry per term** that fully defines it: identity,
  recipes, validity, argument schema, and presentation metadata.
- A **term constructor** that resolves a parsed formula term against the
  registry into a fully-specified constructed-term object, **encoding arguments
  once** instead of branching at runtime.
- A single, consistent validation + error path driven by the registry's
  declarations (wrong argument, wrong network type, wrong model/sub_model).
- Make the metadata already consumed by `compact-term-summary` (short names,
  abbreviations, endogenous/exogenous, families) registry-sourced.
- Adding a new effect = adding one registry entry (+ its init/update bodies),
  with the parser/preprocessor/printer unchanged.

**Non-Goals:**
- No change to estimation math, preprocessing numerics, or coefficient
  baselines.
- No change to user-facing formula syntax or effect names.
- Not redesigning the C++ update path or `model-recipe-dispatch`; the registry
  *feeds* those, it does not replace the recipe loop.
- Not (initially) exposing a public user-facing "register your own effect" API —
  the registry is internal first; a public API is an Open Question.

## Decisions

### D1: Registry is an internal, name-keyed table of `term_def` objects
Maintain an internal registry (an environment or named list populated at package
load) mapping a term's **canonical name** to a `term_def` object (D2). The
parser resolves terms via `get_term_def(name)` rather than string-built
`get`/`getS3method`. Registration happens via an internal `register_term()`
called once per effect at load time, co-located with each effect's
implementation file so the definition lives next to its code.
*Alternative:* derive the registry by scanning `init_*`/`update_*` symbols
(keep convention) — rejected: that is the current implicit coupling we want to
remove; it cannot carry validity/metadata.

### D2: The complete `term_def` schema (define all the elements)
Each registry entry SHALL carry the following groups. (Field names below are the
working contract for the specs/tasks.)

**(a) Identity & taxonomy**
- `name` — canonical term name (e.g. `inertia`, `trans`).
- `family` — hierarchical group for organisation/search, e.g. `closure`,
  `degree`, `attribute`, `attribute+dyadic`, `covariate`, `structural`.
- `label` — human-readable label.
- `description` — one-paragraph description (doc source of truth).

**(b) Presentation metadata**
- `short` — short effect name used in compact summaries.
- `abbrev` — abbreviation used for `coef()`/`vcov()`/print labels (supersedes
  `.goldfishEffectShort`).
- `aliases` — additional search/lookup names (folds `@aliases`).

**(c) Endogenous / exogenous pairing**
- `kind` — `endogenous` or `exogenous`.
- `twin` — the paired term name (e.g. `inertia`↔`tie`, `trans`↔`closure_ff`).
- `object_default` — for endogenous terms, the rule that the object defaults to
  the dependent network layer (so `inertia` needs no object while `tie` does).

**(d) Recipes (computation)**
- `init` — reference to the initialization function/method that produces the
  starting statistic matrix/cache.
- `update` — reference to the per-event update recipe (the function the
  constructor fills with encoded arguments).
- `variants` — which `(model, sub_model)` combinations the recipe supports
  (replacing the implicit "does `update_<m>_<s>_<name>` exist?" test).
- `stat_kind` — `"sender"` or `"dyad"`, the statistic shape this effect produces
  (today assigned in `formula-parsing-link`'s `effects` registry / the plan
  builder). The registry is the natural single owner; the constructed term must
  carry it so the plan/recipe routes correctly. **Review needed**: confirm where
  `stat_kind` is currently decided and whether it is per-effect or per-call.
- `broadcast_eligibility` — whether the effect's per-event update is a
  constant-value fan-out eligible for the `broadcast-stat-updates` encoding
  (today a separate eligibility list). Folding it here removes that fragmentation
  and pairs naturally with `reflexive` (e). **Review needed**: reconcile with the
  existing eligibility predicate so encoding behaviour is byte-identical.

**(e) Validity constraints**
- `network` — required object kind(s): network layer, actor attribute, global
  attribute, or none.
- `directed` / `undirected` — allowed network directionality.
- `one_mode` / `two_mode` — allowed mode (replaces parser `is_two_mode`
  inference + warnings, which the constructor now sets authoritatively).
- `models` / `sub_models` — allowed model/sub_model contexts.
- `interaction` — whether the term is valid inside an interaction term.
- `reflexive` — whether the diagonal is meaningful (informs broadcast/decode).

**(f) Argument schema**
- For each accepted argument: `arg` name, `type`/`allowed` values, `default`,
  a `validate` rule, and the `error` message to raise on misuse. Arguments not
  in the schema for a term are rejected with a consistent error (replacing
  silent acceptance or scattered `stop()`s).
- An `encode` hook per argument (D3/D4) describing how the argument is turned
  into a concrete recipe input at construction time.

*Alternative:* a flat metadata table separate from behaviour — rejected: splits
the single-source-of-truth again; validity/encoding must travel with the recipe.

### D3: Term constructor produces a fully-specified constructed-term object
Add `construct_term(term_def, formula_args, context)` run during parsing.
`context` carries the model, sub_model, the resolved objects, and their
attributes (directed/mode). The constructor SHALL:
1. validate `formula_args` against the schema (D2f) and the validity
   constraints (D2e), raising the declared error on any violation;
2. apply defaults, including endogenous `object_default` (D2c);
3. **encode arguments once** into concrete recipe inputs (D4);
4. set authoritative `is_two_mode`/`directed` from object attributes (removing
   the parser's inference/warning dance);
5. return a constructed-term object carrying the encoded recipe + the resolved
   metadata needed downstream (including the decoder fields the
   `compact-term-summary` builder consumes).
*Alternative:* keep `formals(FUN)` signature-patching (current approach) —
rejected: it defers all encoding/validation to runtime and is why the if/else
branches exist.

### D4: Encode arguments at construction, not at runtime
Arguments whose effect is a fixed transformation SHALL be resolved to that
transformation once:
- `weighted`: `FALSE` → the `sign()`-style binarising transformer, `TRUE` →
  `identity()`/the user `transformer_fn`; the constructed term stores the chosen
  transformer so `update_*` no longer branches on `weighted`.
- `history`: the named branch (e.g. consecutive/sequential/pooled) resolves to
  the selected subroutine reference stored on the constructed term.
- Other booleans/enums that today gate `if/else` inside init/update follow the
  same pattern via their `encode` hook (D2f).
The existing `init_*`/`update_*` bodies are refactored to read the encoded
recipe field instead of re-deciding (numerics identical).
*Alternative:* leave runtime branching, only add metadata — rejected: misses the
constructor half of the user's request and keeps the duplication.

### D5: One validation/error path from registry declarations
All effect-argument and validity errors SHALL be raised by the constructor from
the `term_def` declarations using `cli` with a consistent shape: what was given,
what is allowed, and how to fix. This replaces the parser's bespoke warnings
(`is_two_mode` mismatch), the window-not-allowed aborts, and init-method
`stop()`s. Unknown-effect resolution also reports "did you mean" using `aliases`.

### D6: Metadata consumers read the registry (supersede `.goldfishEffectShort`)
`GetDetailPrint()`/`.decoderColumns()` and the compact-term builder
(`compact_term_strings`, `.shortEffect`) SHALL source `short`/`abbrev`/`family`
from the registry instead of the hard-coded `.goldfishEffectShort` map. The map
is migrated into registry entries and then removed. Coefficient
(`.coef_name`), export (`.term_export`) and console renderings keep their
current output for existing effects (the registry seeds the same strings).
*Trade-off:* this couples `compact-term-summary` output to the registry; covered
by reusing that change's tests as a regression gate.

### D7: Endogenous/exogenous alias as data, not call-chaining
The `inertia`→`tie` relationship (and `trans`→`closure_ff`, etc.) becomes the
`twin`/`kind`/`object_default` fields (D2c) instead of one init method calling
another. The constructor uses `object_default` to inject the dependent network
layer for endogenous terms. The shared init/update body is referenced by both
entries' recipes.
*Alternative:* keep `init.inertia` calling `init.tie` — rejected: the aliasing
is invisible to the parser/printer and can't be searched or validated.

### D8: Adapter-first migration keeps numerics frozen
Phase the migration so behaviour never changes in one big step:
1. Build the registry + constructor; seed entries that **reference the existing
   `init_*`/`update_*` functions unchanged** (adapter recipes). Parser resolves
   via registry but calls the same code → numerics identical, baselines green.
2. Move validity/metadata into entries; switch parser/printer to read them.
3. Migrate argument encoding (D4) effect-by-effect, refactoring each
   `update_*`/`init_*` to read encoded fields, re-running baselines per effect.
Each phase is independently shippable and baseline-verified.

### D9: Registry population
Populate at package load via `register_term()` calls in the
`functions_effects_*` files (or a dedicated `R/effects_registry.R` that sources
them). The structure is a frozen environment after load to avoid mutation
surprises during a session, except via the public API (D11).

### D10: Scope — all three model families in this change (confirmed)
The registry, constructor, validity, metadata, and argument-encoding migration
SHALL cover **DyNAM** (rate/choice/choice_coordination), **REM**
(rate/choice/rate_ordered) **and DyNAMi** (rate/choice). The inventory task is
therefore the full effect catalogue across every `functions_effects_*` file.
DyNAMi's more bespoke argument handling (joining/subType/history) is migrated
with extra per-effect baseline checks. *Rationale:* the user wants a single
source of truth that is actually complete; a partial registry would leave the
parser straddling two resolution paths.

### D11: Public registration API in this change (confirmed)
`register_term()` and the registry lookup/metadata accessors SHALL be **exported
and documented** as a stable public API so advanced users can register custom
effects. This adds: `@export` + roxygen for `register_term()`,
`get_term_def()`/listing/search accessors; a validation step in
`register_term()` that rejects malformed `term_def`s with a clear error; and a
vignette/section showing how to add an effect. The internal effects register via
the same public API (dogfooding). *Trade-off:* the `term_def` schema (D2) becomes
a stability surface — versioned and documented; schema changes are then
breaking.

### D12: Strict validity from the constructor (confirmed)
The constructor SHALL **error immediately** on any argument or
context (model/sub_model/mode/direction/interaction) that a term's `term_def`
does not declare valid — no permissive/warn grace period. *Consequence:* the
validity declarations (D2e) MUST be complete and accurate **before** the parser
is routed through the registry, or currently-working formulas would break.
Mitigation: the inventory (Migration step 1) captures the *current* effective
validity (what works today, derived from existing checks + the test suite + the
baselines), and that becomes the initial declared matrix, so strictness
reproduces today's accept/reject behaviour exactly rather than tightening it.
The full test suite + baselines are the gate that the strict matrix matches
current behaviour. *Alternative (permissive-then-strict):* rejected by the user.

The strict matrix MUST **import existing rules rather than invent fresh ones**.
Known rules that already live elsewhere and must be migrated verbatim:
- global-in-choice / choice-coordination rejection — `model-recipe-dispatch`
  ("global() rejected in choice sub-models"); the bare-`global()` abort must
  remain until interaction terms exist (`compact-broadcast-updates`'s interaction
  reservation in `formula-parsing-link`).
- `ignore_repetitions` rejected with a clear error — `flat-preprocess-output`.
- window not allowed on attribute-only effects, and `list(net1, net2)` handling —
  `fix-window-list-effects` (window-list-attribute/network-effects).
- `is_two_mode requires both node sets` — `model-recipe-dispatch`; D3.3 must use
  this rule, not a re-derivation.
- `opportunities_list` handling (`estimation_core.R`) — **review** whether it
  implies per-effect/per-model validity the registry should declare.
**Review needed**: this list is almost certainly incomplete; the inventory
(task 1.3) must sweep every `stop()`/`cli_abort()`/`warning()` in the parser and
effect files and attribute each to a term/context.

## Decisions — second pass (2026-07-09 explore session)

The working catalogue backing these decisions (naming rules, full effect
tables, argument catalogue, registry skeleton, cross-package comparisons) is
`.plan/effect_refactor_proposal.tex` (local-only).

### D13: Scope layering — this change is Layer 1 only (confirmed)
Three layers, three changes:
1. **Layer 1 (this change)**: registry mechanics — `term_def` schema,
   `construct_term()`, build-time specialization, validity, file
   reorganisation. Numerics frozen; baselines gate everything.
2. **Layer 2 (successor `effect-naming-scheme`)**: the snake_case renaming
   (twin split `trans`/`closure_ff`, `mix_` prefix, `node_` rate twins),
   argument renames (`type`→`perspective`, `ignore_repetitions`→
   `retain = "first"`), all via `lifecycle::deprecate_soft()` with old names
   living **permanently as registry `aliases`** — no rename shims in code.
3. **Layer 3 (successor `effect-statistic-extensions`)**: new statistics and
   arguments — `open`, `retain`, `combiner_fn`, `normalizer_fn`
   (`center`/`scale`/`proportion`/`total`/exp-decay), `summarizer_fn`,
   categorical expansion, P-shifts, `node_*`/undirected new effects. Each
   needs *new* baselines; cannot ride the frozen ones.
The Layer-1 schema **reserves** every field Layers 2–3 need (D18) so the
successors add entries/values, not schema surgery. The `perspective` rename
moves from this change (see inbound-recommendations section) to Layer 2.

### D14: Cache is a named list with an argument-activated field catalogue
The effect cache becomes `list(stat = <current cache>, ...)`. `term_def`
gains **`cache_spec`**: the catalogue of *possible* fields — name, type,
update/broadcast kind, and an **activation predicate on argument values**.
`construct_term()` resolves the active set; `init_*` creates only active
fields; preprocessing updates only active fields, sparsely at entry level.
- Field naming: content-named with `last_event_` prefix — `stat`,
  `last_event_time` (active when `normalizer_fn` is a decay),
  `last_event_seq` (global event counter at last update; active when
  `history = "consecutive"` or `retain = "last*"`). P-shifts ride this same
  route (no separate state kind needed).
- Layer 1 ships the machinery + the `list(stat = …)` wrap (done inside the
  D4 encoding pass that already rewrites those bodies; numerics-neutral).
  The extra fields activate only when Layer-3 arguments exist.
- Caution: sparse in-place updates vs R copy-on-modify — needs
  single-reference discipline or an environment-backed container; the
  benchmark (D15) informs the choice.

### D15: Specialization strategy = function factories, benchmark-gated
Default encoding mechanism is **(c) construction-time specialized closures**
(function factories; decisions hoisted out of the per-event path). **(d)
state specialization** (e.g. binarized cache for `weighted = FALSE`) is
adopted per-argument only where a benchmark shows it beats (c) by a margin
justifying the memory. A **benchmark spike gates D4** (new task group):
- Variants: (a) current if/else, (b) stored-fn-per-arg, (c) factory,
  (d) pre-transformed cache; cases `tie(weighted=)`, `trans(history=)`, plus
  a deliberately flattened all-if-else `trans`.
- Grid: actors {10, 100, 500, 1000, 3000} × events {1e2, 1e3, 1e4, 1e5} as
  **prefixes** of one generated stream per actor size; skip ≥3000 × ≥1e4
  except a capability probe of the winner. `bench::mark()` for
  time + `mem_alloc`. Replicates tiered 10/5/3/1 by runtime; 5h wall-clock
  cap; auto-skip variant×combo over ~30 min except probes.
- Human-in-the-loop: scripts co-written in `.plan/bench/` (never committed),
  run on the user's HPC against real half-hour datasets (local paths);
  judgment call weighted toward large combos.

### D16: Constructed term carries the build plan
`window`, `retain`/`ignore_repetitions`, and categorical expansion are
**build-time object-creation promises**: the constructed-term object carries
an `expands_to`/`build_plan` field (windowed network copies, dummy-expanded
term lists) that preprocessing fulfills — replacing the parser's ad-hoc
window-object creation and giving `refactor-single-data-object` a single
interception point.

### D17: Validity source of truth is per-effect metadata; the matrix is transitional
The inventory matrix (task 1.3) is a **migration artifact only** — it seeds
and verifies the initial `term_def` entries (D12's strictness gate). The
permanent source of truth is each effect's registry metadata; a new effect
declares its own validity and the matrix is never maintained as a table.

### D18: Reserved schema surface for Layers 2–3
Layer 1 defines (documented, validated, but unused by Layer-1 code):
- `wraps` — wrapper-of declaration: target term + pinned/mapped argument
  values. Consumers: DyNAMi effects as `tertius`/monadic wrappers (the
  `subType` vocabulary maps onto `summarizer_fn` × `normalizer` ×
  `transformer_fn`; mapping table in the tex), P-shift sugar terms.
- Reserved argument names in the schema vocabulary: `open` (logical; count
  structures not yet closed, closing events decrement — opportunity stats),
  `retain = c("all", "first", "last")` (absorbs `ignore_repetitions`;
  `"last"` per-dyad vs `"last_global"` semantics decided in Layer 3),
  `combiner_fn` (closure path weights; `prod` on binarized ≡ current
  count), `normalizer_fn`/`normalizer`, `summarizer_fn`, `levels`.
- `after_event_stream` — recipe hook slot for post-walk passes (D22).
- `cache_spec` reserved fields per D14.

### D19: Naming decisions recorded for Layer 2 (settled now, applied later)
Direction codes `f`/`b` settled (ERGM precedent). `mix_` (not `mixed_`)
prefix; mixed closures are always exogenous. `node_` prefix for rate/nodal
twins (`node_recip`, `node_triad_ff` ← `node_trans`, `node_same`).
Endogenous form defaults to dependent events; exogenous twin requires the
object. Argument rules: `_fn` suffix for function-valued args; enums as
nouns with character values, default first. Released names deprecated via
`deprecate_soft()` and kept permanently as `aliases`. Full catalogue in the
tex file.

### D20: Categorical expansion rules (recorded for Layer 3)
Drop-first reference level (`contr.treatment`; ergm `nodefactor(levels=-1)`
precedent). **No hard cap — cli warning above ~10 levels**; `levels =`
selects explicitly (values/indices/negative exclusion, ergm-style).
Generated terms named `<attr>_<level>`; decoder metadata keeps the parent
term for `coef()`/`tidy()` grouping. Dyadic categorical: future, `nodemix`
is the reference design; shares the build-plan machinery (D16).

### D21: Documentation surfaces are registry-generated
Effect reference (man tables + pkgdown "effect gallery" article) generated
from the registry; `search_effects()` queries name/alias/family/validity.
TikZ diagrams are **pkgdown-only, SVG rendered at site build**: the full set
as PNG in `man/` would be ~0.8–2 MB (75–85 diagrams) → installed-size NOTE
and PDF-manual bloat; SVG doesn't render in the CRAN PDF manual anyway.

### D22: Normalizers — post-walk pass and piecewise-constant decay (Layer 3 contract)
- `center`/`scale`: **accumulators** (count/sum/sum²) over the risk set
  actually used are maintained **during the event-stream walk regardless of
  output format**; the transform runs as a **post-walk recipe step**
  (`after_event_stream` hook) before estimation. `center` = mean only;
  `scale` = mean/sd (coxph-style; also seeds the rate-intercept init).
- Exponential decay (Brandes 2009): implemented **as a tool under the
  piecewise-constant assumption**, accepting the known bias (Wit/Ernst) —
  no `refactor-likelihood-compute` dependency; needs `last_event_time`
  (D14).
- `proportion`/`total` are event-triggered and piecewise-constant already.

## Risks / Trade-offs

- [Large surface; risk of silently changing numerics] → D8 adapter-first phasing
  with the frozen baselines run as the gate after every effect migration.
- [Registry duplicates info still living in `formals()`] → during phase 1 derive
  the schema defaults from `formals()` where possible to avoid divergence; phase
  3 makes the schema authoritative and drops signature-patching.
- [Validity matrix is large (model×sub_model×mode×direction) and strict from day
  one (D12)] → the matrix MUST reproduce *current* accept/reject behaviour before
  the parser is routed through it; the inventory derives the declared matrix from
  today's effective behaviour and the full suite + baselines gate that it matches
  (no newly-broken formulas, no newly-permitted ones).
- [Coupling `compact-term-summary` output to the registry] → seed identical
  strings; reuse that change's tests as a regression gate (D6).
- [DyNAMi/REM coverage] → the registry must span all three model families;
  enumerate every existing `update_*`/`init_*` first (inventory task) so none is
  missed.
- [Sequencing collision with `refactor-single-data-object`] → that (unscheduled)
  change redefines object binding ("effects reference layers/variables by name"),
  endogenous defaulting ("dependent events via the focal layer" ≈ this design's
  `object_default`), and validity ("panel layers are exogenous covariates;
  windows forbidden"). These are the *same* resolution/validity machinery this
  design introduces (D2c/D3/D7, D2e). If the registry lands first against the
  current "dependent network layer" notion, single-data-object must re-point
  `object_default` at the focal-layer contract and register its panel/window
  validity through the registry. → Co-design or explicitly sequence the two;
  decide before implementing D3/D7. **Review needed.**
- [Design maturity] → this whole design is DRAFT (see top banner); the
  `term_def` schema, validity matrix, constructor resolution, and metadata
  ownership each need a code-grounded review pass before their tasks start.
- [Inherited delta: interaction-term rendering] → once this change makes the
  compact term-string surfaces registry-metadata-driven (D6), it owns a latent
  cross-change gap from `compact-term-summary`: that builder's grammar
  (`effect/obj·obj2 [args]`) has **no form for interaction terms**
  (`global(x):alter(y)`), which `compact-broadcast-updates` reserved as a future
  capability in `formula-parsing-link`. When interaction parsing lands, the
  builder, the second legend, and the coef/export names must gain an interaction
  rendering driven by `term_def` metadata (and the `term_def.interaction`
  validity field, D2e, must agree). Scope: not built here, but the schema and
  metadata surfaces should not foreclose it. **Review during the design pass.**

## Migration Plan

1. Inventory every existing effect across `functions_effects_*` (name, model,
   sub_model, args, current validity checks, current metadata) into a table.
2. Implement `term_def` + registry + `register_term()` + `get_term_def()` and
   `construct_term()` with adapter recipes (no behaviour change).
3. Route the parser through the registry/constructor; baselines must stay green.
4. Migrate validity + metadata; switch printer/coef/tidy/export to registry.
5. Migrate argument encoding (weighted, history, then the rest) effect-by-effect.
6. Remove `.goldfishEffectShort` and the string-built lookups once unused.

Rollback: each phase is a separate commit; reverting a phase restores the prior
(working, baseline-green) state.

## Open Questions

Resolved with the user during proposal:
- **Scope** → all three families (DyNAM + REM + DyNAMi) in this change (D10).
- **Public registration API** → exported & documented in this change (D11).
- **Validity strictness** → strict/error immediately (D12).

Resolved in the second pass (2026-07-09) — see D13–D22:
- Scope layering (Layer 1 only; successors `effect-naming-scheme`,
  `effect-statistic-extensions`) — D13.
- Cache contract, field names, ownership of the list-wrap — D14.
- Specialization strategy + benchmark gate — D15.
- Build-plan ownership — D16. Validity source of truth vs matrix — D17.
- Reserved schema surface (`wraps`, `open`, `retain`, `combiner_fn`,
  `normalizer_fn`, `summarizer_fn`, `levels`, `after_event_stream`,
  `cache_spec`) — D18.
- Naming + argument conventions (recorded for Layer 2) — D19; categorical
  rules — D20; docs/tikz placement — D21; normalizer contract — D22.
- **Family taxonomy**: settled as the file split (D13/tex §File
  organisation): `closure`, `dyadic`, `degree`, `attribute`,
  `attribute+structural` (+ `pshift` later); one family per term.

Still to settle — **these gate implementation; resolve during the review pass,
not mid-apply:**
- **Pre-implementation review (blocking)**: every component must be validated
  against the live code first. Minimum sweeps: where `stat_kind` is assigned
  today; the exact broadcast-eligibility predicate; the full
  `stop()`/`cli_abort()`/`warning()` inventory feeding the strict validity
  matrix (D12); how `opportunities_list` (`estimation_core.R`) constrains
  effects; and the `refactor-single-data-object` sequencing decision.
- **`stat_kind` & `broadcast_eligibility` ownership**: confirm the registry is
  the right owner (vs the plan builder) and that values reproduce current
  routing/encoding exactly (D2d).
- **Schema representation**: plain nested list vs S7/S4 class for `term_def`.
  Since the schema is now a public stability surface (D11), a class with a
  validator is more defensible; weigh against added dependency. Lean S7/list
  with an explicit validator.
- **Encoding benchmark outcome (D15)**: which of (c)/(d) per argument, and the
  cache container (single-reference list vs environment) — decided from the
  HPC runs before task group 6.
- **Layer-2/3 detail decisions deferred with their changes**: `retain`
  `"last"` vs `"last_global"`, exogenous name for `four`, `node_sim`
  inclusion, P-shift equivalence verification (relevent/remstats/dream) — 
  tracked in `.plan/effect_refactor_proposal.tex` Open items.

## Inbound recommendations from `refactor-formula-parsing`

The `refactor-formula-parsing` change implements native `type = "ego"` in DyNAM
choice, interaction terms, and a per-`(model, sub_model)` validity matrix using a
**local validity table** (it does not block on this registry). To let that table
migrate into the registry — making the parser's checks a lookup and removing the
duplicate source of truth — the `term_def` schema (D2) should additionally carry:

- **`valid_types`** — the allowed `type` (perspective) values per `sub_model`
  (e.g. `choice → c("alter", "ego")`, default `"alter"`), so the choice
  `type = "ego"` acceptance and the rate `ego`-only rule are declared, not coded.
- **`interaction_valid`** — whether the term may appear as an **interaction
  operand**, per `model`/`sub_model` (drives which `a:b` combinations are legal:
  REM any; DyNAM-rate ego/global; DyNAM-choice ego/global operands).
- **`global_rule`** — in which sub_models a `global` term is valid as a **main**
  effect (rate yes; `rate_ordered` no; choice no — only via interaction), encoding
  the existing global-in-choice abort declaratively.
- **`perspective` alias** — record the planned `type` → `perspective` rename (the
  dev-plan name that disambiguates from the ties `flavour`/`type` field) so the
  registry exposes both names during the transition. ~~This change owns the rename
  itself~~ **superseded by D13 (2026-07-09): the rename ships with Layer 2
  (`effect-naming-scheme`); this change only ensures the schema can host both
  spellings.** (Original note, 2026-07-03; it was scheduled nowhere): unlike the manynet-owned
  metadata names (never released, superseded without ceremony), `type =` is a
  *released* goldfish argument, so it follows the r-lib:lifecycle argument-rename
  pattern — effect signatures gain `perspective`, a supplied `type` maps onto it
  and warns via `lifecycle::deprecate_warn()` (which by design fires **once per
  session** per calling context — the "warn once" handling is the lifecycle
  default, do not hand-roll it), plus badge + NEWS entry. One consistent shim for
  all effects (registry-driven, not per-effect copies). Interlock: the
  `support-constraint-risk-set` documentation (its task 8.1) teaches constraint
  formulas using these arguments — if this rename lands first, those docs use
  `perspective`; otherwise they use `type` with a pointer here.

These extend D2's validity/argument-schema groups; they do not change the
registry's lifecycle or the rest of this design.

### Implementation notes (what actually landed, `refactor-formula-parsing`)

`refactor-formula-parsing` is now implemented; these details make the migration a
faithful lookup rather than a re-derivation:

- **Two validity tiers, not one boolean.** The local table
  (`R/formula_validate.R`) distinguishes **unavailable** (no bare effect
  implementation → abort in *every* phase, incl. preprocessing/`compute_stats`)
  from **unidentified** (computable as a design column but not identified as a
  bare main effect → rejected *only when estimating*). After task 1.5 made
  `global` computable in DyNAM choice, the *unavailable* tier is empty and
  `global`/`ego` in choice are *unidentified* (produced by `compute_stats`,
  usable as interaction operands / random-effect design columns, rejected as
  estimated main effects). So `global_rule` / `valid_types` should encode a
  **tri-state** per `(model, sub_model)`: `main` (estimable), `computable`
  (produced but not an identified main effect), `unavailable` (not computable) —
  a single boolean loses the `compute_stats`-vs-`estimate` distinction the parser
  relies on.
- **Interaction operand validity is an axis-union rule, not a per-term flag.** An
  interaction's broadcast kind is the **union of its operands' variation axes**
  (`global`=∅, `ego`=row, `alter`=col, point=both); `global` is the identity,
  point is absorbing, and `ego:alter` unions to point (`axis_union_kind()`,
  `R/preprocess_builders.R`). An interaction is an identified main effect iff its
  *product* kind varies on the estimated axis — so `interaction_valid` is best
  derived from operand kinds + the same identification rule as main effects,
  rather than enumerated per operand pair. The registry should expose each
  effect's broadcast kind (`broadcast_eligibility` already planned) and let the
  product rule compute interaction legality.
- **`estimate = FALSE` operands are kept, not dropped.** A `:`-only operand is
  retained in the preprocessed design and held out of estimation by fixing its
  coefficient at 0 (the score is already zeroed for fixed components, so no C++
  change). The registry does not own this — it is an estimation-front-end concern
  — but `interaction_valid` / role metadata is what marks a term as an operand.
- **Rendering.** An interaction's compact name is the `:`-join of its operands'
  rendered names (`GetDetailPrint()`), so the registry-driven metadata renderer
  (D6) only needs correct *operand* short/export forms; interaction names compose
  from them (no separate interaction naming table).
- **Scope actually shipped:** interaction computation is dyad-indexed only (DyNAM
  choice/choice_coordination, REM); sender-indexed (DyNAM/DyNAMi rate) and DyNAMi
  interactions are guarded as not-yet-supported. A registry `variants` /
  `interaction_valid` entry should therefore also be able to express "operand
  valid but interaction unsupported for this `stat_kind`".
