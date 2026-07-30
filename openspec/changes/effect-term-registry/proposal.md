## Why

Effect (term) definitions are currently spread across many places: the
`init_<model>_<submodel>.<name>` S3 methods and `update_<model>_<submodel>_<name>`
functions (located by string-built names), argument semantics handled by
runtime `if/else` inside those functions (e.g. `weighted` switching between
`sign()` and `identity()`/the transformer, `history` branching), validity rules
scattered as ad-hoc `stop()`/`warning()` calls in the parser and init methods,
and presentation metadata fragmented across `@aliases` roxygen tags, the
`inertia`→`tie` init-aliasing trick, and the `.goldfishEffectShort` abbreviation
map. There is no single source of truth describing *what an effect is*. This
makes adding an effect error-prone, makes "is this effect valid for this model /
network?" checks inconsistent, and forces every consumer (parser, preprocessor,
printer, tidy, export) to re-derive facts that the effect itself should declare.

This change introduces a **central, declarative effect/term registry** so each
term is defined exactly once with all of its elements, and constructs a
fully-specified term object at parse time so downstream code stops re-deciding
arguments at runtime.

## What Changes

- **New** declarative **effect/term registry**: one entry per term holding its
  init recipe, update recipe, validity constraints, argument schema, and
  metadata. The parser, preprocessor, and display/export surfaces consult the
  registry instead of string-built lookups and scattered rules.
- **New** **term constructor**: after formula parsing, each formula term is
  resolved against the registry into a constructed term object whose
  arguments are **encoded once** (e.g. `weighted` → the matrix transformer to
  apply; `history` → the selected subroutine) rather than re-evaluated by
  `if/else` on every init/update call.
- **Registry validity declarations** replace ad-hoc checks: each term declares
  the network layers / attributes / model / sub_model / interaction contexts it
  is valid for, and an **argument schema** (which arguments are accepted, their
  types/allowed values, and the error to raise for misuse). Invalid use produces
  one consistent, actionable error.
- **Registry metadata** consolidates: short name, printing abbreviation
  (folding in `.goldfishEffectShort`), endogenous/exogenous alias pairing (e.g.
  `inertia`↔`tie`, `trans`↔`closure_ff`) with the rule that the endogenous form
  defaults its object to the dependent network layer, search aliases, label,
  description, and a hierarchical **family** (e.g. `closure`, `attribute`,
  `attribute+dyadic`).
- **BREAKING (internal)**: the effect-resolution path in
  `parse_formula()`/`parse_multiple_effects()` changes from string-built
  `get`/`getS3method` lookups to registry lookups; existing exported
  `init_*`/`update_*` functions are wrapped/registered rather than discovered by
  name. No change to estimation numerics or user formula syntax.

## Capabilities

### New Capabilities
- `effect-term-registry`: the declarative registry data structure and lookup —
  each term entry's init/update recipe references, validity constraints,
  argument schema, and consolidated metadata (short name, abbreviation,
  endogenous/exogenous alias, search aliases, label, description, family).
- `effect-term-construction`: resolving a parsed formula term against the
  registry into a constructed term object with arguments encoded once
  (transformers, history subroutine, defaults, endogenous object defaulting),
  consumed by preprocessing and the display/export surfaces.

### Modified Capabilities
- `formula-parsing-link`: effect resolution and validation move from
  string-built `update_*`/`init_*` discovery and inline `if/else` checks to
  registry lookup + the term constructor; argument/validity errors are raised
  from the registry's declarations.

## Impact

- Code: `R/formula_parser.R` (resolution + validation), the
  `R/functions_effects_*.R` families (registered rather than name-discovered;
  argument `if/else` moved to constructor-encoded recipes), `R/model_preprocess*.R`
  (consume constructed terms), `R/utils.R` (`GetDetailPrint`, the compact-term
  builder + `.goldfishEffectShort` migrate to read registry metadata).
- Surfaces already added by `compact-term-summary` (short names, abbreviations,
  `.coef_name`/`.term_export`) become registry-driven instead of hard-coded.
  Note (2026-07-29): those strings gained a second role in `residuals-gof`,
  which exports the fit-scoped `model_terms()` and makes the compact string the
  key that selects a term in `initial_parameters` and in every `diagnose_*` /
  `test_*` effect argument. Registry-driving them must therefore preserve them
  exactly; they are no longer display-only. The registry's own
  `search_effects()` stays package-scoped (which effects exist) and does not
  subsume `model_terms()` (which terms this fit has).
- No user-facing change to formula syntax, estimation results, or coefficient
  baselines (the 1e-6 regression floor must still pass).
- Related changes: complements `refactor-preprocess-estimate` and
  `model-recipe-dispatch`; supersedes the hard-coded abbreviation map introduced
  in `compact-term-summary`.

## Scope layering (settled 2026-07-09; design D13)

This change is **Layer 1 (mechanics) only** of a three-layer plan; the schema
*reserves* what the successors need but implements none of it:

- **Layer 2 — `effect-naming-scheme`** (successor change): snake_case effect
  renames (endogenous/exogenous twin split, `mix_`/`node_` prefixes) and
  argument renames (`type`→`perspective`, `ignore_repetitions`→
  `retain = "first"`), all via lifecycle with old names permanently kept as
  registry `aliases`.
- **Layer 3 — `effect-statistic-extensions`** (successor change): new
  statistics/arguments (`open`, `retain`, `combiner_fn`, `normalizer_fn`,
  `summarizer_fn`, categorical expansion, P-shifts, new `node_*`/undirected
  effects) — these need new baselines, so they cannot ride this change's
  frozen ones.
- **In scope here additionally**: the encoding **benchmark spike** (design
  D15) that decides the specialization mechanism before argument encoding, and
  the cache named-list wrap + `cache_spec` machinery (design D14).
- Working catalogue for all three layers:
  `.plan/effect_refactor_proposal.tex` (local-only).
- `refactor-dynami-engine` absorbs the "DyNAMi effects as tertius wrappers"
  insight; this change only guarantees the `wraps` declaration can express it.
