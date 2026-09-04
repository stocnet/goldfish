## Context

The formula parser (`R/formula_parser.R`) turns a one-sided formula into a list of effect
descriptors and S3-dispatched `init_<model>_<submodel>.<name>` / `update_*` functions
(`parse_formula` lines 34-117; `type_parameter` extraction lines 72-75; effect dispatch
lines 196-274). The parsed bundle is assembled in `R/model_estimate.R:564` and flows into
preprocessing. Today:

- DyNAM **choice** effects default to `type = "alter"` and have no working `type = "ego"`
  path — the choice inits alias the REM inits (`R/functions_effects_DyNAM_choice.R:371-385`),
  and REM already supports both perspectives via `to_ego`/`to_alter`
  (`R/functions_effects_REM.R:144-227`, `R/utils_effects.R:24-50`).
- There is **no interaction syntax**; the RHS is split by a hand-rolled recursive walker
  `extract_formula_terms` (`R/formula_parser.R:294-305`) with a vestigial, wrong `*` branch
  (treats `*` like `+`) and no `:` handling (`a:b` falls through as one opaque term). The
  parser does **not** use `stats::terms()`.
- `global()` is rejected in choice sub-models by a post-parse abort
  (`R/model_estimate.R:585-596`); `classify_broadcast_kind` already encodes `alter`/`ego`/
  `global` as broadcast kinds 1/2/3 for dyad models (`R/preprocess_builders.R:119-139`).
- `make_specification()` does not exist; users pass formulas directly to `estimate_*()`.
- The formula→engine wiring is a two-stage **bridge**: `parse_formula` /
  `create_effects_functions` / `get_*_link` run in `R/model_estimate.R` before preprocessing,
  then `build_update_plan` (which also carries the call **templates**) / `build_state_container`
  / the schedule are rebuilt **inside** `preprocess()` each call (`R/model_preprocess.R:361, 868`).

The active `effect-term-registry` change introduces declarative `term_def` metadata
(`variants`, `sub_models`, `interaction`, `broadcast_eligibility`) that would make the
validity checks below a lookup. Per the scope decision this change does **not** depend on
that change landing; it implements the checks now and recommends registry additions so the
later code simplifies (see "Registry recommendations").

## Goals / Non-Goals

**Goals:**
- Native `type = "ego"` in DyNAM choice / choice_coordination, numerically identical to the
  REM-derived expansion, with no special model type.
- R-standard interaction terms (`:` and `*`) whose statistic is the product of two operand
  effects.
- One consistent validity error per `(model, sub_model)` rule set.
- `make_specification()` v1 covering DyNAM and REM (DyNAMi deferred with its preprocessing-path
  change), simple rate + choice formulas, returning the parsed objects preprocessing needs.
- ERGM-style `offset()` fixed-coefficient terms in the formula, with values supplied via a
  new `offset_coef`, superseding the positional `fixed_parameters` vector.
- Build the formula→engine structures **upfront** (drop the preprocessing bridge): a
  specification mapping producing the effects template + update plan, consumed by
  `preprocess()`; with interaction links and a multivariate `formula_effects` seam.
- Full backward compatibility for existing formulas.

**Non-Goals:**
- The `support_constraint` engine, `presence2`→matrix refactor, opportunity-list deprecation,
  and the risk-set / intercept-denominator estimation changes — all in
  `support-constraint-risk-set`. (The `offset()` front-end here only *assembles* the existing
  `fixedParameters` vector — it adds no estimation-core math.)
- Flavoured processes, multivariate `make_multivariate_spec()`, `add_flavour()` — future.
- Renaming `type` → `perspective` — recorded as a future alias only (D6).
- Implementing the `effect-term-registry` itself.
- **`ignore_repetitions` re-enablement** — stays disabled; only noted as the next consumer of
  the `plan$derivations` seam (D8). Its realizer (and whether it needs evolving state) is a
  future change.
- **DyNAMi *engine* conversion** (`preprocessInteraction` monolith → recipe loop / `stat_state`)
  — deferred to the dedicated `refactor-dynami-engine` change (the previously phantom "separate
  change", now a real stub). `make_specification` + the spec-object estimate path also stay scoped
  to **DyNAM/REM** in v1 (DyNAMi's spec-object path arrives with the engine change).
- **DyNAMi preprocessing stays unchanged in this change** (revised — see D8). The original
  Non-Goal is restored: DyNAMi keeps its current group-preprocessing front-end — old parse-time
  windowing (`parse_time_windows` `assign()`), `cleanInteractionEvents`, and the
  `preprocessInteraction` monolith. Rather than dragging DyNAMi through the shared data-boundary
  realizer here (which would touch shared-with-DyNAMi code and produce a throwaway adaptation the
  engine change discards), DyNAMi is **isolated** into its own preprocessing front-end (task 2.3e)
  so the shared DyNAM/REM path sheds every DyNAMi `if`-branch. Nothing in this change's deliverables
  needs DyNAMi unified: every feature (`type = "ego"`, interactions, `offset()`,
  `make_specification()`) is DyNAM/REM only, and DyNAMi's spec-object path is already deferred to
  `refactor-dynami-engine`. DyNAMi is unified onto the shared `spec_map`/realizer **once**, at the
  engine rewrite, on the new foundation — not twice (principle: don't half-migrate a component you
  are about to rewrite). The metadata/data boundary relocation (2.3c/2.3d) is therefore
  **DyNAM/REM only**.

## Decisions

### D1 — Native `type = "ego"` in DyNAM choice
Choice degree-family effects (`indeg`, `outdeg`, `degree`, and any effect already exposing a
`type` formal) accept `type = c("alter", "ego")`, default `"alter"`. Implementation reuses
the REM expansion: the `update_DyNAM_choice_*` path threads `type` and calls
`to_ego`/`to_alter` (`R/utils_effects.R:24-50`) exactly as REM does
(`R/functions_effects_REM.R:220-226`). Because the choice inits already alias REM
(`R/functions_effects_DyNAM_choice.R:371-385`), the change is in the effect-function /
parsing layer; **no C++/gather change** (choice already materializes dyad stats — the
discovery task confirms this). Two-mode `type = "ego"` keeps REM's existing guard.
*Alternative rejected:* a separate `model = "DyNAMRE"` (the removed hack) — rejected because
it duplicates REM preprocessing and was already deleted upstream.

### D2 — Interaction terms via `stats::terms()`
Derive the RHS terms with **`stats::terms(formula, keep.order = TRUE)`** instead of the
hand-rolled `extract_formula_terms` walker (`R/formula_parser.R:294-305`), which currently has
a vestigial, semantically wrong `*` branch (it treats `*` like `+`, no `a + b + a:b`
expansion) and lets `:` fall through as one opaque term. `terms()` does the expansion
correctly and yields everything the parser needs: `attr(., "variables")` (the effect calls as
language objects — the same list the old walker produced for non-interaction formulas),
`attr(., "factors")` (the variable→term incidence matrix), `attr(., "order")` (interaction
degree), and `attr(., "intercept")`. `get_rhs_names` is rewritten to consume this; the
hand-rolled walker and its broken `*` branch are **deleted**.

Each unique variable is parsed once with goldfish's existing call-parser (effect + args
preserved — `:`/`*` only operate at the top level, never inside a call such as
`outdeg(net, type = "ego")`). A `:` term builds an **interaction term object** referencing
its operand terms (read off the `factors` column); `a*b` expands to `a + b + a:b` for free.
The interaction statistic is the elementwise product of the operands' stat columns, computed
incrementally in the recipe loop (mechanics, `stat_state`, and the n-ary generalization in D9;
the upfront compile that produces the plan/templates in D8).
Constructs goldfish does not model (`I()`, `|`) are rejected with a consistent `cli` error so
`terms()` cannot silently admit something downstream cannot dispatch. `offset()` is **not**
rejected — it is unwrapped and tagged as a fixed-coefficient term (see D7).

*Validated downstream:* `goldfish.latent` already runs `terms()`/`reformulate()` on
goldfish-style effect formulas, and its interaction **guard**
(`any(attr(terms(x), "order") != 1)` → abort) is exactly the seam that flips to support here.
*Alternative rejected:* extending the hand-rolled walker — perpetuates a fragile parser and
re-implements `*`/`:` precedence that `terms()` gives for free. *Alternative rejected:* the
`Formula` package — its value is `|`-separated multi-part formulas, redundant with the
`make_specification(rate=, choice=, support_constraint=)` separate-argument API, and a new
dependency for no gain (revisit only if those parts ever collapse into one `~ … | … | …`).
*Alternative rejected:* an `interaction(a, b)` wrapper — diverges from R formula conventions.

**Broadcast classification of a product = the union of the operands' variation axes.** Each
broadcast kind (`classify_broadcast_kind`, `R/preprocess_builders.R:119-139`) is really a
statement about which axis of the dyad grid the statistic varies on:

| kind | effect | varies across |
|---|---|---|
| 3 | `global` | — (per-time scalar): axes `{}` |
| 2 | `ego` / degree `type="ego"` | senders (rows): axes `{row}` |
| 1 | `alter` / degree `type="alter"` | receivers (cols): axes `{col}` |
| 0 | point/dyadic | senders and receivers: axes `{row, col}` |

An elementwise product varies on an axis iff **either** operand does, so
`axes(A:B) = axes(A) ∪ axes(B)`. `global` (axes `{}`) is the **identity** — multiplying by
it preserves the other operand's kind; point (`{row,col}`) is **absorbing**. The full table:

```
            │ global 3 │  ego 2  │ alter 1 │ point 0
 ───────────┼──────────┼─────────┼─────────┼─────────
  global 3  │    3     │    2    │    1    │    0
  ego 2     │    2     │    2    │  → 0    │    0
  alter 1   │    1     │  → 0    │    1    │    0
  point 0   │    0     │    0    │    0    │    0
```

So `global(price):outdeg(net)` → kind 1 (adopts `alter`), `global:ego` → kind 2 (adopts
`ego`), and the one non-obvious corner, `ego:alter` → **point (0)** (different single axes
union to both). Rate (sender-indexed) models project onto the single sender axis, so the
lattice collapses to `{3, 0}`: a product is `global(3)` iff **both** operands are global,
else point. This realizes the seam reserved in `compact-broadcast-updates` /
`effect-term-registry`.

### D3 — Per-(model, sub_model) validity matrix
A single validity helper (in `R/class_checks.R` or a new `R/formula_validate.R`) consulted at
parse/validate time, raising one consistent `cli::cli_abort`:

| model | rule |
|---|---|
| **REM** | any interactions allowed; `global` **main** term only in `sub_model = "rate"`, not `"rate_ordered"` (constant across dyads → cancels in softmax). |
| **DyNAM-rate** | only `type = "ego"` or `global` main terms (as today) **plus** their interactions. |
| **DyNAM-choice / choice_coordination** | interactions involving `type = "ego"` or `global` allowed; bare `global` and `type = "ego"` / constant-across-alternatives **main** effect stays disallowed (extend the abort at `R/model_estimate.R:585-596` to permit the interaction form). |

The helper reads `effect-term-registry` metadata when present, else a local table.

**Identification view (the same axis bit governs softmax identification).** In DyNAM-choice
the alternatives are the receivers (cols), so a term is identified as a **fixed main effect**
iff it varies on the receiver axis — i.e. broadcast kind ∈ {1 `alter`, 0 `point`}. Kinds
{2 `ego`, 3 `global`} are constant across alternatives and cancel in the softmax. Hence:

- DyNAM-rate accepts broadcast kinds 2 and 3
- DyNAM-rate_ordered broadcast kind 2
- DyNAM-choice and DyNAM-choice_coordination broadcast 0 and 1
- REM any broadcast kind
- REM-ordered broadcast kind 0, 1, 2
- For DyNAM-choice this implies that a `type = "ego"` effect/column **is allowed in choice**,
  but **not** as a fixed-identified
  main effect: one use case it is a design column for the two-level random-effects model (multiplied by a
  random *sender* effect, restoring sender-axis variation downstream) or for an interaction
  with an `alter`/dyadic partner that restores receiver-axis variation. So the validity rule
  is asymmetric by design — reject `global` and `type = "ego"` main effects, permit `global` and
  `ego` in interactions — because restores the variation.
  The product rule above and this identification rule are two views of
  the same per-axis variation.

### D4 — `make_specification()` v1
```r
make_specification(
  rate = NULL, choice = NULL,
  model = c("DyNAM", "REM"),          # DyNAMi deferred to its preprocessing-path change
  rate_sub_model = c("rate", "rate_ordered"),
  choice_sub_model = c("choice", "choice_coordination"),
  layer = NULL,                # names the dependent process
  support_constraint = NULL,
  data = NULL
)
```
Returns an S3 `specification.goldfish` object holding, per submodel, the parsed bundle
preprocessing consumes (`rhs_names`, `type_parameter`, interaction structure, `has_intercept`,
window/weighted/trans/summ params — the same fields assembled at `R/model_estimate.R:564`),
plus model metadata and validation results. `support_constraint` is parsed and stored but its
engine lands in `support-constraint-risk-set` (validation + storage only here).
`estimate_dynam`/`estimate_rem` gain an S3-dispatched path: when passed a
`specification.goldfish` they skip re-parsing; the formula path is unchanged. `estimate_dynami`
keeps its current formula path (its spec-object path lands with the DyNAMi change).
*Alternative rejected:* a brand-new single `estimate()` entry — keeps two paths separate and
breaks fewer call sites by extending the existing estimators.

**Dependent identification — `layer`, not the LHS.** Unlike the legacy
`estimate_*(dep ~ ...)` interface, a specification's rate/choice formulas leave the **LHS
empty**; the `layer` argument names the dependent process. This removes the need to check
that the rate and choice LHS match (there is no LHS), and it scales to flavours and the
multivariate case. The LHS carries **only** a flavour selector: empty = the whole layer; a
bare flavour symbol (e.g. `creation ~ ...`) when rate/choice are supplied as a
flavour-keyed `list`. A dependent-events object placed on the LHS is a **clear error** that
points the user at `layer`.

**`layer` resolution (bridge now, single-data-object later) — same surface syntax.**
- *Bridge (current data objects):* `layer` is resolved with the existing parser path —
  `get(layer, envir = data)` must return a `dependent.goldfish` (the same lookup
  `get_dependent_name()` → `get(dep_name)` does today at `R/formula_parser.R:35`, just
  sourced from the argument instead of the LHS). In this era the `layer` string is the
  dependent-events **object name**.
- *Future (`refactor-single-data-object`):* only the resolver swaps — `layer` becomes an
  `info$layers` entry of the single data object (the relation/layer name). The user-facing
  call and the print are unchanged.
The print anchors on the resolved **network** (`attr(dep_obj, "default_network")`), which is
stable across both eras even though the `layer` label the user types shifts meaning.

**Class hierarchy.** v1 returns class `specification.goldfish`. To let the future
`make_multivariate_spec()` reuse print/validate/estimate dispatch, the multivariate object
SHOULD inherit it: `c("multivariate_specification.goldfish", "specification.goldfish")`.

**Experimental lifecycle.** `make_specification()` is marked **experimental** via
`lifecycle::badge("experimental")` (lifecycle is already in Imports). The v1 surface is
deliberately going to move — `layer` resolution swaps when `refactor-single-data-object`
lands, the `type`→`perspective` rename is planned (D6), and the flavour/multivariate shapes
are designed but unbuilt — so the experimental badge sets that expectation and avoids a
deprecation cycle for early signature changes.

**Print, no summary.** There is **no `summary` method** — `print` is the whole overview (see
the `specification-print` requirement in the spec). It shows: model + which submodels are
present; a **Dependent** block (the `layer`, and from the resolved object: number of events,
time span, sender→receiver nodesets, network) with flavours nested beneath the layer when
present; the rate/choice formula(s); the support-constraint formula when given; and the
validation result. Right-censored events are **not** shown — they are unknown until
preprocessing.

The print is **rendered with `cli`** (the first cli-*rendered* output in the package; cli is
already a dependency, used today only for `cli_abort`/`cli_warn`). Mapping: `cli_rule(left=…)`
title, `cli_text`/`cli_dl` for the Model and Rate/Choice/Support labels, `cli_bullets` for the
Dependent facts, `cli_alert_success`/`cli_alert_danger` for validation; inline markup `{.val}`
(layer/network/counts), `{.field}` (nodesets/submodels), `{.code}` (formulas), `{.cls}` (the
class). **Formulas are interpolated as pre-deparsed strings** (`deparse1(fml)`), never literal
markup, so interaction syntax (`*`, `:`, `()`) renders verbatim. This is scoped to the
specification print — it does not migrate the existing `cat()`-based `print.*.goldfish`
methods. *Layout:* the Dependent facts use a bulleted block (own lines); a `cli_dl` aligns the
formula labels. Because cli output is width/colour dependent, snapshot tests MUST pin a
reproducible context (`testthat::local_reproducible_output(width = 80, crayon = FALSE)` or
`cli::test_that_cli()`).

### D5 — Interaction rendering
The compact term string for an interaction follows the registry's reserved `effect/obj·obj2`
form; `coef()`/`vcov()`/print names get an interaction rendering built on the archived
`compact-term-summary` shared renderer.
A simplify version should be derive to avoid long text, something like short name effect : short name effect and only 
disambiguate with object when more interactions become equal.

### D6 — Backward compatibility and the `type`→`perspective` note
Existing formulas, `model`/`sub_model` combinations, and numerics are unchanged; `type="ego"`
in choice and interactions are purely additive. The dev plan renames `type` → `perspective`
(to disambiguate from the ties `flavour` field); v1 keeps `type` and records `perspective` as
a future alias — not implemented here.

### D7 — `offset()` fixed-coefficient terms (ERGM-style)
A formula term may be wrapped in `offset(...)` to mark its coefficient as **fixed** rather
than estimated, e.g. `~ inertia + offset(ego(sex)) + recip`. The value(s) are supplied via a
new `offset_coef` argument to `set_estimation_opt()`, aligned to the offset terms in formula
order (the ERGM `offset()` + `offset.coef` pattern). This **supersedes** the positional
`fixed_parameters` vector (the error-prone "count the coefficient position" NA-vector), which
is kept working under a soft-deprecation (lifecycle `superseded`) that points at
`offset()` + `offset_coef`.

**Front-end over the existing backend — no estimation-core change.** `stats::terms()` already
separates offsets: `attr(., "offset")` gives the indices into `attr(., "variables")`, and the
inner call is `variables[[i]][[2]]` (e.g. `offset(ego(sex))[[2]]` → `ego(sex)`). The parser
**unwraps** that inner call, parses it through the normal path (so the stat column is computed
exactly like any term), and **tags** the term `offset = TRUE` with its order. The estimation
front-end then assembles the existing full-length `fixedParameters` vector
(`R/cpp_interface.R:72-97`, `R/estimation_core.R:84-109`) from `(offset positions) +
(offset_coef values)`. The Newton-Raphson core is untouched.

```
  ~ a + offset(ego(sex)) + b        set_estimation_opt(offset_coef = 2)
        │ terms() attr "offset"             │
        ▼ unwrap ego(sex), tag offset       │  by NAME, not by counting positions
        └───────────────┬───────────────────┘
                        ▼ assemble
        fixedParameters = c(NA, 2, NA)  →  EXISTING estimation core (unchanged)
```

**Critical distinction from GLM / `model.matrix`:** offsets there are *dropped* from the
design matrix; here the stat column is **kept** (the term contributes `coef·stat`). `terms()`'s
offset attribute is used only to *tag*, never to drop.

**Offsets obey the D3 identification axis.** Fixing a coefficient does not change geometry: a
constant-across-alternatives offset in DyNAM-choice (`offset(global(...))`, `offset(ego(...))`)
still cancels in the softmax. So D3's per-axis rule still governs *usefulness* — but for an
offset the response is a **warning** ("offset is constant across alternatives → no effect"),
not an abort, since nothing is being estimated. In rate/REM an offset genuinely shifts the
rate. *Alternatives rejected:* inline `offset(term, coef = 2)` (overloads base `offset()` with
a non-standard argument) and repurposing `fixed_parameters` to mean offset-aligned values
(silently changes that argument's semantics).

### D8 — Upfront specification mapping (drop the preprocessing "bridge")
Today the formula→engine wiring is split across two stages: `parse_formula` /
`create_effects_functions` / `get_*_link` run in `R/model_estimate.R` *before* preprocessing,
then `build_update_plan` / `build_state_container` / the schedule are rebuilt *inside*
`preprocess()` (`R/model_preprocess.R`) on every call. That two-stage split was a bridge while
the parser was refactored. This change drops it: the data-light compiled structures are built
**upfront** as the outcome of parsing, and `preprocess()` just consumes them.

A new umbrella **`build_spec_map()`** (in the repurposed `R/formula_parser.R` — no new file)
returns a **`spec_map`** = `list(parsed_terms, effects_template, plan)`, orchestrating
the trio:

```
 parse_formula()           parsed_terms      terms()-based: {effect, objects, args, operands,
                                             is_offset, intercept}  (formula_parser.R)
 build_effects_template()  effects_template  per gid: {init_fn, update_fn, formal_names,
                                             args_by_shape, net_keys, att_keys}  ← SPLIT OUT of
                                             build_update_plan (preprocess_builders.R)
 build_update_plan()       plan              registries ONLY (templates removed): effects,
                                             objects, effect_objects, routing, interactions,
                                             operand_of, stat_state_spec, formula_effects
```

`build_effects_template()` is the dedicated split the templates were extracted into;
`build_update_plan()` keeps only the registries and **moves upfront** (out of `preprocess()`).
The link matrices (`objects_effects_link`, `events_*_link`) become **internal** to the compile,
not separately threaded. `specification.goldfish` carries the `spec_map` (data-light,
serializable). `state` and `build_schedule()` are **data-derived and built at preprocess** from
`(plan, data)` — not stored on the spec. *Alternative rejected:* a new `R/model_compile.R` and a
`compile_model()` name — the user repurposes `formula_parser.R` as the specification-mapping
module instead. *(Name `build_spec_map` provisional.)*

**`build_spec_map()` owns the print/naming metadata.** The effect print/naming metadata is
formula-derived (`GetDetailPrint(objects_effects_link, parsed_formula)` → `effect_description`,
`R/utils.R:565`) and is today regenerated at gather/export (`R/preprocess_export.R:175`,
`R/model_estimate.R:947`). It moves into `build_spec_map()` as the **single source of truth**
`spec_map$effect_description` (covering interactions, `role`/`estimate`, `hasWindows`). The
**short names are rendered on demand** from it per context via `compact_term_strings` /
`CreateNames` (console width, db-safe `max_length = 63`, export) — not baked once — so the db
column names, gather `colnames`, `coef()`/`vcov()` names, and print all derive from one place and
stay consistent.

**`spec_map` + `effects_template` subsume the old preprocess inputs.** `preprocess()` no longer
takes `effects`, `windowParameters`, `eventsObjectsLink`, `eventsEffectsLink`,
`objectsEffectsLink`: `effects` → `effects_template`; the link matrices → internal to `plan`;
`windowParameters` → `parsed_terms` (per-term window) + the effect closures; the
`isWindowEffect` mask (`R/model_preprocess.R:248`) reads `parsed_terms`. New signature:
`preprocess(spec_map, data, control…)` building only `state` + schedule.

**Metadata/data boundary — `build_spec_map()` is pure (reads metadata, mutates nothing).**
Parsing today does heavy **data** work that must move to state creation: `get_events_and_objects_link`
fetches event tables (`get(dep_name)`, `lapply(ev_names, get)`), remaps labels→ids
(`sanitizeEvents`, `R/formula_parser.R:328/362/385`), and assembles the events list; and
`create_windowed_events` (`R/formula_parser.R:276`) fabricates dissolve event tables **and** a
windowed network copy, **assigning them into the caller's environment** (`assign(..., envir)`,
lines 637/639/661). The boundary:
- **`build_spec_map()` (metadata recipe):** reads object **attributes only** — event-stream
  *names* (`attr(obj, "events")`), `nodes`, `directed`, `dynamic_attributes`, dims/two-mode — to
  build the link incidence and a **windowed-stream recipe** (effect E needs a `window = w` dissolve
  stream + windowed network derived from object X: names + parameters, not rows). Its output holds
  **no event/network tables** and it **creates/assigns nothing** (testable invariant).
- **State creation (`build_state_container` / `build_schedule(spec_map, data)`):** fetches the
  tables, runs `sanitizeEvents` (label→id), and runs `create_windowed_events` + the windowed network
  **into the state container** (not the user's envir), then assembles events + merges the schedule.

This makes `spec_map` data-free and **reusable across data snapshots** (bootstrap, simulation/GOF,
rolling windows, multivariate) and removes a surprising environment mutation. So
`get_events_and_objects_link` splits: the incidence half (names) stays in the mapping; the table
half (fetch/sanitize/window/assemble) moves to state creation. *Verify at implementation:* the
window value reaches both windowed-stat init (`initializeCacheStat`) and window-expiry event
generation through `effects_template`/`parsed_terms`; and audit `getElementFromDataObjectTable` /
the `net_ids` resolution (`R/formula_parser.R:467`) for whether it touches values (move) or only
names (stays).

**Derived-input promises — one `plan$derivations` registry.** The windowed-stream recipe
generalizes: an effect may read not the raw object but a **derived view** of it. Today window does
this by eagerly creating a derived network + expire streams and rewiring the effect's object
reference (`rhs_names[[i]][[2]] <- paste(name, window_name, "_")`, `R/formula_parser.R:620-661`);
`ignore_repetitions` is the same shape (the effect reads a repeats-removed view) but is a runtime
flag today and is **currently disabled** (`R/model_estimate.R:574-576`). Unify them as a
**`plan$derivations`** registry — one row per derived input: `{derived_name, kind, source,
source_streams (names), params, gids}`. `build_spec_map()` fills it from **metadata only** and
**rewires the affected effects' object references in the plan** to `derived_name` (no tables, no
`assign`). State creation **recognizes each promise** by iterating `plan$derivations` and
dispatching on `kind` to a realizer that builds the derived network/stream **into the state
container** (window: empty derived network + `create_windowed_events` expire streams). The effects
already reference `derived_name`, so the loop routes correctly. A future `kind` (e.g.
`ignore_rep`, a decayed view) is a new realizer behind the same seam.

```
 build_spec_map  ──promises──▶  plan$derivations  ──realized by──▶  state creation
   (metadata: names,            kind = window |                     (derived network/streams
    params; rewire gids)          (future) ignore_rep                 into the state container)
```

**Scope:** this change implements the registry + the **window** realizer (relocating the eager
block). `ignore_repetitions` **re-enablement is deferred** (details unclear) — it stays disabled
and is noted only as the next consumer of the seam. Its realizer may need the evolving state
(whether an event is a "repeat" can depend on the tie's current presence and increment/replace
semantics), which a future change must settle before re-enabling.

**The realizer serves the DyNAM/REM recipe path only.** For DyNAM/REM, `parse_time_windows`
stops `assign()`-ing windowed networks/streams into the caller env; the single realizer runs at
state creation and writes the realized objects into the recipe state container. **DyNAMi is
isolated** (task 2.3e): its own preprocessing front-end keeps the current parse-time windowing
`assign()` + `cleanInteractionEvents` + `preprocessInteraction`, contained to the DyNAMi wrapper
and its `work_env` clone. So the boundary relocation (2.3c/2.3d) is DyNAM/REM only, and the
`assign()` pollution survives — localised to the DyNAMi island — until `refactor-dynami-engine`
rewrites that loop and unifies it onto the shared `spec_map`/realizer in one move. The estimation
tail (§4 print → §5 estimate → §6 results in `estimate_wrapper`) is already model-agnostic and
stays shared; only the preprocessing front-end forks by model (2.3e).

**A derived object is source metadata + fresh data — so metadata never needs the realized
object (revised at implementation, Session 7).** The original boundary above assumed the only
data-touching consumers were `get_events_and_objects_link` and the `net_ids` resolution. In
practice **three** consumers between parse and `preprocess()` reach for the *realized* derived
network: `create_effects_functions` (`eval()`s the `network` arg to read `attr(., "nodes")` for the
two-mode guard), `build_object_keys` (`get()` + `is.matrix()` to classify), and
`build_events_objects_link`'s network branch (`attr(., "events")`/`"nodes")`. Each reads **only
metadata**, and a windowed derived network is metadata-identical to its source — `realize_windowed_network`
copies `nodes`, `directed`, `class`, `dim`/`dimnames` verbatim and only replaces the *data* (an
empty matrix + `create_windowed_events` dissolve streams). So a derived object carries two flavours
of metadata, **both fully known at spec_map time**:

```
 derived windowed net  =  INHERITED metadata        +  PROMISE-DEFINED metadata  +  fresh DATA
                          (identical to source:         (new names computed from     (empty matrix +
                           nodes, directed, class,       source + params:             windowed dissolve
                           dims, is_two_mode)            derived_name, stream          streams — the ONLY
                          → read from the source         names, kind, params)          thing realized, in
                            object                      → computed from the recipe      state creation)
```

This yields the boundary principle: **`spec_map` is the single source of truth for every name and
every piece of metadata (inherited *and* promise-defined); `build_state_container` is the single
place a name becomes data. Nothing between parse and `preprocess()` reads a realized derived
object — inherited metadata resolves to the source, promise-defined metadata is computed from the
recipe.** The three consumers therefore do **not** move to state creation; they stay put and
*resolve `derived → source`* (read the attribute from the source object / recipe instead of the
realized derived one). Only `realize_windowed_network` + `fetch_events` move into
`build_state_container`, driven by **`plan$derivations`** — which closes the loop: the promises are
*defined* in `build_spec_map` (the registry) and *consumed* in state creation (the registry stops
being "built but unused" and becomes the realizer's driver). Session 7 landed the first half of
this: `parse_formula` no longer mutates the env on the recipe path (realization relocated to a
single registry-recipe-driven call in the recipe front-end); the derived→source resolution + the
push into `build_state_container` + the `events`→`data` rename is the remaining slice (task 2.3f).

**Phasing (two stages, architecture first).** This change is implemented in two stages with a
version-bump milestone between:
- **Stage 1 — upfront spec mapping (pure refactor, no numeric change):** terms()-based
  `parse_formula`, `build_spec_map`/`build_effects_template`, `build_update_plan` upfront, the
  plan registry *schema* (incl. `role`/`estimate`/`interactions`/`stat_state` fields, populated
  trivially), `effect_description` ownership + per-context rendering, the
  `preprocess(spec_map, data)` signature, and the metadata/data boundary (relocate
  `sanitizeEvents` + `create_windowed_events` + event-table assembly to state creation; pure
  `build_spec_map`). Verified green against the
  frozen baselines (behaviour-preserving).
- **Stage 2 — features on the new foundation:** `type = "ego"` in choice, interaction `stat_state`
  computation, the validity matrix, `make_specification()`, `offset()`, interaction rendering, and
  the multivariate `formula_effects` use.
Stage 1 is first because the features are built *on* the new structures (features-first would
implement them on the bridge, then rip it out), and because a pure refactor is verifiable in
isolation by the baselines. Splittable into two separate changes later if review prefers.

### D9 — Interaction computation in the loop (Option A): `stat_state`, n-ary
Interactions are computed **incrementally in the recipe loop** (not deferred to gather). Because
the loop keeps only per-effect `cache` + emitted deltas — never a live full stat — Option A adds
`stat_state`: a materialized current value **only for** the gids in `{interaction operands} ∪
{interactions}`, seeded from the effect `init`'s `stat` (which the loop currently discards) and
kept current by replaying that operand's deltas. `stat_state` is stored **by broadcast kind**, at
each effect's natural dimensionality:

```
 stat_state <- list(
   dyad   = array (n1 × n2 × p0),   # kind 0
   ego    = matrix(n1 × p2),        # kind 2  (split "node" into ego/alter — in two-mode
   alter  = matrix(n2 × p1),        # kind 1   n1 ≠ n2, so they cannot share one matrix)
   global = vector(p3)              # kind 3
 )                                  # index: gid → (slot, column)
```

**Init of an interaction** — derived, no new `init_*` method: init each operand via its existing
`init_*`, seed its `stat_state` slot, then seed the interaction's slot with the broadcast-product
`P0[cell] = Π_k broadcast(stat_state[op_k])[cell]` (broadcast: global→scalar, ego→`e[i]`,
alter→`a[j]`, dyad→`d[i,j]`). The interaction's slot lives at its axis-union kind (D2).

**Update (n-ary, uniform)** — `interactions` maps `gid → operands = c(gid_1, …, gid_k)` (k from
`terms()` `order`; the operand set from the `factors` column). On any operand `op_k`'s delta:
apply it to `stat_state[op_k]`; for each `g ∈ operand_of[op_k]`, recompute the touched cells as
the **product over all operands** and emit `ΔP = P_new − stat_state[g]`. Binary is just `k = 2`;
no pairwise special-casing.

**Link-object additions** (in `plan$effects`): `role ∈ {main, operand, interaction}`,
`estimate` (logical), plus `interactions`, `operand_of`, and `stat_state_spec`. Routing gains the
second hop object→operand→interaction.

**Keep operands by default.** An operand that is not itself a requested main effect is kept in the
preprocessed object with `estimate = FALSE` (the estimator selects `estimate = TRUE` columns) —
**not dropped** — because the original operand values power downstream analysis: marginal-effect /
partial-dependence plots, contribution/decomposition plots, simple-slopes analysis, collinearity/
VIF diagnostics, nested-model comparison without re-preprocessing, counterfactual prediction,
simulation-based GOF, centering before interacting, and reuse across multiple interactions. An
optional `drop_operands = TRUE` is offered for memory-tight runs. This harmonizes with `offset()`
(kept, coefficient fixed): both are "computed, present, not freely estimated." `*` vs `:` differs
only in `keep`/`estimate` — `*` makes both operands `estimate = TRUE` (they are requested mains);
`:` alone leaves non-requested operands `estimate = FALSE`.

### D10 — Multivariate seam: `formula_effects (fid, lid, gid)`
`plan$formula_effects` carries rows `(fid, lid, gid)` = formula id, local effect index within the
formula, unique effect id. **Single-formula:** `fid = 1`, `lid = position` (uniform, no special
case). **Multivariate (future):** several formulas contribute effects; shared cross-process
effects (e.g. `outdeg(calls)` in both the calls and friendship models) **dedup to one `gid`**
computed once, and `(fid, lid)` maps each formula's local slot back to it, so each process's
coefficient vector is assembled by selecting its `gid`s. The structure is built **now** (with
`fid = 1`); the *use* is future multivariate preprocessing (`make_multivariate_spec` remains a
non-goal of this change).

## Registry recommendations (append to the active `effect-term-registry` change)
To make D1/D3 a lookup once the registry lands, recommend these `term_def` fields:
`valid_types` (allowed `type` per sub_model), `interaction_valid` (operand eligibility per
model/sub_model), `global_rule` (which sub_models allow the main effect), and a `perspective`
alias note. These are added to that change's `design.md`/specs, not as deltas here.
*(Done — appended to `openspec/changes/effect-term-registry/design.md` under "Inbound
recommendations from `refactor-formula-parsing`", including the implementation-informed
refinements: the two-tier `main`/`computable`/`unavailable` validity, the axis-union interaction
rule, and the operand-fix / rendering notes.)*

## Downstream: `goldfish.latent` consumption path
`goldfish.latent` currently pre-processes goldfish formulas with a
**`modify_formula_re()`** helper and remaps to a private **`model = "DyNAMRE"`** to obtain the
`type = "ego"` design column that vanilla goldfish did not expose in choice. Both were
workarounds for gaps this change closes:
- **Native choice `type = "ego"` (group 1)** makes `outdeg(net, type = "ego")` etc. produce the
  ego-perspective column directly in `sub_model = "choice"` (numerically equal to the REM
  expansion, 1e-6), and `compute_stats()` yields it as a design column even though it is not an
  identified *main* effect — exactly the random-effects design column `DyNAMRE` fabricated.
- **`global()` computable in choice (task 1.5)** exposes the constant/global design column the
  same way.
- **Interaction terms (`:`/`*`, groups 2/6)** provide the `random_slope × covariate` products
  `modify_formula_re()` built by string surgery; e.g. a two-level random-effects design column is
  now `ego(x):alter(y)` or `global(x):outdeg(net, type = "ego")` in a plain formula, computed by
  the recipe loop and rendered with composed names.

**Recommendation for `goldfish.latent`:** drop `modify_formula_re()` and the `DyNAMRE` remap and
pass the effect / interaction formula straight to `estimate_dynam(sub_model = "choice")` /
`compute_stats()`. The `type = "ego"` and `global` columns are produced natively (as unidentified
design columns), and random-slope products are ordinary interaction terms — no private model type
or formula rewriting is needed. (This change does not modify `goldfish.latent`; the note records
the now-available path so that package can simplify on its own schedule.)

## Risks / Trade-offs
- **`type="ego"` numeric drift** → guard with a baseline test asserting choice `type="ego"`
  equals the REM-derived expansion to 1e-6.
- **Interaction broadcast-kind misclassification** → a product of two broadcasts could be
  encoded wrongly; mitigate by defaulting interaction products to **point** updates unless a
  proven-safe broadcast combination, with a cross-engine 1e-6 check.
- **`*` expansion ambiguity in choice** (it can introduce a disallowed `global` main effect)
  → the D3 validity helper runs **after** `*` expansion so the illegal main effect is caught.
- **Two parsing entry points** (formula vs `make_specification`) drifting → both funnel
  through the same `parse_formula` core so behaviour stays identical.

## Open Questions
- Exact compact-string form for nested/argumented interaction operands (resolve in the
  rendering task against the `compact-term-summary` renderer).
- Whether `degree` (undirected) needs a distinct `type="ego"` guard like REM's two-mode case.
