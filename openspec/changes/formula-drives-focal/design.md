## Context

goldfish resolves the dependent process three ways: the two-sided formula LHS
(`estimate_dynam(support ~ ...)`), `make_specification`'s `layer` argument, and
`info$focal` as the fallback. `parse_formula()` unifies these into `dep_name` and
builds *its* data source with `focal = dep_name`, with a comment stating the
intent: the formula's LHS — not `info$focal` — is the focal layer.

But estimation builds **several** data sources. `create_effects_functions()`
(`R/formula_parser.R`) and the preprocessing builders
(`R/model_preprocess.R`, `R/preprocess_builders.R`) each call
`new_data_source(data =, envir =)` with no `focal`, and `new_data_source()`
resolves `focal <- focal %||% info$focal`. So every source except
`parse_formula`'s falls back to `info$focal`. With `info$focal` unset, `src$focal`
is `character(0)`, and `ds_layer_map(src, src$focal)` (`src$derived[[name]]`) and
`ds_side_ids()`'s `src$mode_map$layers[[src$focal]]` abort with
`get1index`. The first crash is in `check_effect_sides()` (the two-mode
side-validity gate); the next is in `ds_side_ids()`/`mode_view_key()`.

The prebuilt datasets and `make_data()`-assembled objects always set
`info$focal`, so the whole test suite and the frozen baselines pass — the gap
only bites a hand-built object that omits it.

## Goals / Non-Goals

**Goals:**

- A `stocnet` with no `info$focal` estimates when the formula/`layer` names the
  dependent — one-mode and two-mode.
- The **modeled** layer drives every focal/side/mode lookup; a mismatched
  `info$focal` never wins.
- Frozen coefficient baselines unchanged (the fix is a no-op when `info$focal`
  equals the modeled layer, which every baseline object satisfies).
- A clear error when *no* dependent can be resolved (no LHS, no `layer`, no
  `info$focal`).

**Non-Goals:**

- Changing how the dependent is *named* (the LHS / `layer` / `info$focal`
  precedence in `model-specification` stays).
- Reworking the DyNAMi/legacy-envir path (it has no mode map; the crash is
  stocnet-only).
- Any new required field or breaking change to `add_info()`.

## Decisions

### D1 — Stamp the resolved dependent as the working object's focal (chosen)

At estimation entry, once `dep_name <- parsed_formula$dep_name` is resolved,
stamp it onto the working copy: `if (!is.null(work_data)) work_data$info$focal
<- dep_name`. Every downstream `new_data_source(data = work_data)` then resolves
`focal` to the modeled layer through the existing `%||% info$focal` fallback —
one change point covering all ~14 source builders.

Rejected **Option B** (thread `focal = dep_name` through
`create_effects_functions()` and every `new_data_source()` call site): correct
but ~14 edits across four files, and easy to miss a site as the code grows.
The stamp is a single semantic statement — *the layer being modeled is the focal
layer of this estimation* — and makes the mismatch case (D3) fall out for free.

`work_data` is a local copy (R copy-on-modify), so stamping never mutates the
caller's object.

### D2 — The stamp location covers both entry surfaces

`estimate_dynam(formula, ...)` and `estimate_dynam(spec)` both converge on the
same preprocessing/estimation core where `dep_name` is resolved, so stamping
there covers both. `make_specification()`'s *build* path does not reach
`create_effects_functions()` (it parses and validates effects only), so it does
not crash on an unset focal at build time; the crash is an estimation-time
concern. This SHALL be verified, not assumed.

### D3 — Mismatched `info$focal` resolves to the modeled layer

Because the stamp overwrites the working focal with the modeled `dep_name`, an
object whose `info$focal` names a *different* layer than the one being modeled
now resolves sides/modes against the modeled layer — fixing the latent
two-mode side-validity correctness bug without extra code.

### D4 — Defensive guard on focal-name lookups

`ds_layer_map(src, name)` SHALL return `NULL` for a zero-length or `NA` name
instead of erroring in `src$derived[[name]]`. This is hygiene, not the primary
fix (D1 makes the focal well-formed): it turns any *other* unresolved-name path
into the existing "not a layer, skip" branch (`check_effect_sides` already treats
a `NULL` focal map as "nothing to check") rather than a `get1index` abort.

### D5 — No dependent resolvable is still a clear error

When the LHS is empty, `layer` is absent, and `info$focal` is unset, no
dependent can be named. That SHALL abort with a clear, actionable message
(name the missing declaration), not the `get1index` crash. The existing
`ds_check_dependent()` / `layer` resolution is the place to assert this.

### D6 — `add_info(focal =)` stays optional; document, do not warn

Omitting `focal` when the formula/`layer` names the dependent is now valid and
silent — warning on every focal-less object would be noise. `?goldfish_data` /
`add_info()` gain a sentence that `focal` is an optional default. No lifecycle
change.

### D7 — Docs reframe to focal-less-first; `focal` stays an optional override

The docs lead with the pattern the fix enables — the formula LHS or
`make_specification`'s `layer` names the dependent, no `add_info(focal =)`
required — and mention `focal` once as an **optional override** (D6 keeps it
supported, so this is reframing, not removal). Scope: `?goldfish_data` /
`add_info()`; the `teaching1`, `teaching2`, `two-mode` vignettes (edit the
`.orig` sources, re-precompile); and the roxygen `@examples` on `goldfish_data`,
`as_goldfish`, `fisheries_treaties`, `social_evolution_stocnet`.

Two caveats:

- `two-mode.Rmd`'s printed object annotates `support (focal)` from `info$focal`.
  Reframing that vignette focal-less drops the annotation, so the print output
  and surrounding narration change together — or keep `focal` set there and label
  it inline as the optional override. Decide per-vignette at apply time; the
  optional-override framing (not wholesale removal) is the invariant.
- The `focal` *argument* of `network_state_at()` / `nodes_state_at()` is a
  function parameter, not `add_info` metadata — leave it untouched.

### D8 — Sequence after `multimode-network-support` (close that first)

This change is a successor bugfix on the two-mode substrate
`multimode-network-support` builds: the crash and the latent side-validity bug
only manifest on a two-mode object, and the proposal's "silent today because the
flagship's layers share one mode pair" is precisely that change's flagship. Two
concrete couplings fix the ordering:

- **Baselines:** the no-op claim (the stamp cannot move coefficients) is proven
  against the *frozen two-mode baselines* that `multimode` adds. Those must exist
  and be PASS before this change can assert it.
- **Vignette:** this change reframes the very `two-mode.Rmd` that `multimode`
  ships. Reframing it while `multimode` is still mid-flight means two in-progress
  changes editing one file.

`multimode` is 28/33 (docs done; only the parser choice-set task, the two-mode
baselines, and the milestone remain). Finish and archive it, then start this
change against its stable, frozen two-mode surface.

## Risks / Trade-offs

- **Baselines:** the stamp is a no-op whenever `info$focal` already equals the
  modeled layer, which holds for every prebuilt/assembled object and every frozen
  baseline — so the 1e-6 floor cannot move. This SHALL be confirmed by a full
  `NOT_CRAN=true` run reporting the baselines PASS (not SKIP).
- **Mutating `work_data$info$focal`:** safe under copy-on-modify, but the design
  relies on `work_data` being the object handed to every downstream builder; a
  builder that instead reads the original `data` would miss the stamp. D2's
  verification SHALL check the preprocessing sources observe the stamped focal.
- **Scope creep into per-source threading:** deliberately avoided (D1); if a
  future refactor removes the `%||% info$focal` fallback, the stamp must be
  revisited — noted here so the coupling is explicit.
