## Why

Goldfish issue #2 (interaction effects) and the new-interface vision
(`make_specification()`) require the formula layer to express things it currently
cannot: `type = "ego"` effects in the DyNAM **choice** sub-model, **interaction terms**,
and a structured **specification object**. The downstream package `goldfish.latent`
(two-level DyNAM with random effects) hacks around these gaps — it injects `type = "ego"`
into choice terms and routed them through a now-removed `model = "DyNAMRE"`
(`check_model_par()` only allows `c("DyNAM","REM","DyNAMi")`), so the hack is broken and
the capability belongs natively in goldfish's parser.

## What Changes

- DyNAM **choice** and **choice_coordination** natively accept `type = "ego"` effects
  (default stays `type = "alter"`), reusing the existing REM `to_ego`/`to_alter` expansion.
- Formula parsing recognizes **interaction terms** via R-standard `:` (interaction only)
  and `*` (`a*b` → `a + b + a:b`); an interaction's statistic is the elementwise product of
  its two operands' per-dyad statistics.
- A per-`(model, sub_model)` **validity matrix** rejects invalid term/interaction
  combinations with one consistent `cli` error (e.g. bare `global()` main effect stays
  disallowed in choice but its interaction form is permitted).
- New `make_specification()` constructor returns a `specification.goldfish` object holding
  the parsed rate/choice formula objects preprocessing needs; `estimate_dynam`/`estimate_rem`
  accept this object **or** a formula (legacy path unchanged). Scoped to **DyNAM/REM**; DyNAMi
  rides a separate deferred change (see below).
- ERGM-style `offset()` marks a term's coefficient as fixed (e.g. `~ offset(ego(sex))`), with
  values supplied via a new `offset_coef` argument; this **supersedes** the positional
  `fixed_parameters` vector (soft-deprecated via lifecycle). Reuses the existing
  `fixedParameters` estimation backend unchanged — `offset()` is only a name-based front-end.
- Interaction terms gain a compact-term-string and `coef()`/`vcov()` rendering, realizing
  the interaction-rendering seam reserved by `effect-term-registry`.
- **Architecture**: the formula→engine structures are built **upfront** as a specification
  mapping (`build_spec_map()` in `R/formula_parser.R`) producing a separate `effects_template`
  and an `effects`/`interactions`/`stat_state` **plan**, consumed by `preprocess()` — dropping
  the current two-stage bridge. Interactions are computed incrementally in the loop via
  `stat_state` (n-ary), operands kept by default (`estimate = FALSE`). A `formula_effects
  (fid, lid, gid)` link seeds future multivariate preprocessing.
- **Not in this change** (linked follow-up `support-constraint-risk-set`): the
  `support_constraint` engine, the `presence2`→matrix refactor, and opportunity-list
  deprecation. `make_specification()` accepts and stores a `support_constraint` formula but
  only parses/validates it here (pass-through).
- **Not in this change** (separate deferred change): the **DyNAMi / group preprocessing-path**
  refactor onto the new architecture. DyNAMi keeps its current preprocessing
  (`R/model_preprocess_group.R`) unchanged; this change refactors the DyNAM/REM path only.

## Capabilities

### New Capabilities
- `choice-ego-effects`: DyNAM choice / choice_coordination natively accept `type = "ego"`
  effects with `type = "alter"` as the unchanged default.
- `interaction-terms`: parsing of `:`/`*` into interaction term objects whose statistic is
  the product of two operand effects, with derived broadcast/`stat_kind` classification.
- `model-specification`: `make_specification()` v1 and the `specification.goldfish` object
  consumed by `estimate_*()`.
- `offset-fixed-terms`: `offset()` in the formula marks a fixed-coefficient term; values via
  `offset_coef`; supersedes the positional `fixed_parameters` vector.

### Modified Capabilities
- `formula-parsing-link`: the parsed-formula / update-plan derivation is built upfront (a
  specification mapping with a separate effects template + registries-only plan), carries
  per-term `type` validity, the interaction-term structure (n-ary operand references) and
  `stat_state` needs, and a `formula_effects (fid, lid, gid)` multivariate seam.

## Impact

- **Parser**: `R/formula_parser.R` (`parse_formula`, `get_rhs_names`,
  `extract_formula_terms`, `create_effects_functions`, the `type_parameter` extraction).
- **Effect functions**: `R/functions_effects_DyNAM_choice.R` (thread `type`),
  `R/utils_effects.R` (`to_ego`/`to_alter`), `R/preprocess_builders.R`
  (`classify_broadcast_kind` for interaction products).
- **Estimation entry**: `R/model_estimate.R` (formula bundle, global-in-choice abort,
  model/sub_model lists) gains a `specification.goldfish` acceptance path.
- **New file**: `make_specification()` (empty-LHS formulas + `layer` naming the dependent) +
  S3 `print` (no `summary`).
- **Validation**: `R/class_checks.R` validity helpers.
- **Offset front-end**: `set_estimation_opt()` gains `offset_coef`; `estimate_*()` assemble the
  existing `fixedParameters` vector (`R/cpp_interface.R:72-97`, `R/estimation_core.R:84-109`)
  from offset tags + values; `fixed_parameters` soft-deprecated (lifecycle `superseded`).
- **Downstream**: enables `goldfish.latent` to delete `modify_formula_re()` and the
  `DyNAMRE` remap.
- **Docs/tests**: roxygen + `devtools::document()`; new tests; `DESCRIPTION`/`NEWS.md` bump.
