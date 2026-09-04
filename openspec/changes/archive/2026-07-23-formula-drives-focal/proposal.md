## Why

A hand-built `stocnet` whose `info$focal` is unset crashes at estimation even
though the model formula names the dependent layer. Building the discourse
network with `manynet` verbs and `add_info(...)` — but omitting
`focal = "support"` — and then calling

```r
estimate_dynam(support ~ 1 + outdeg(support) + ego(power),
               sub_model = "rate", data = nuclear)
```

aborts with `Error in src$derived[[name]]: attempt to select less than one
element in get1index`. The formula's left-hand side already names `support` as
the dependent process, so this should just work.

`parse_formula()` gets it right — it builds its data source with
`focal = dep_name` (the formula's LHS) and documents the intent: the LHS, not
`info$focal`, is the focal layer a parse resolves against. But the other data
sources built during estimation — in `create_effects_functions()` and the
preprocessing builders — call `new_data_source(data =, envir =)` **without** the
dependent, so `new_data_source()` falls back to `focal <- focal %||% info$focal`.
When `info$focal` is unset, `src$focal` is empty, and both
`ds_layer_map(src, src$focal)` (in the two-mode side-validity check) and
`ds_side_ids()`'s `src$mode_map$layers[[src$focal]]` index with a zero-length
name and abort.

Beyond the crash, this is a latent **correctness** bug: when `info$focal` is set
but *differs* from the layer being modeled, the two-mode side-validity check
resolves the focal dyad's sides against `info$focal` rather than the modeled
layer — so it can wrongly accept or reject a two-mode effect. It is silent today
only because the flagship two-mode object's layers share one mode pair.

The layer being modeled **is** the focal layer of that estimation. `info$focal`
should be only a default for *which* layer to model when the formula/`layer`
does not say — never a hard requirement, and never the source of truth once a
specific layer is being modeled.

## What Changes

- The resolved dependent layer (the formula LHS, or `make_specification`'s
  `layer`, or `info$focal` as the fallback default) SHALL drive **every** focal /
  side / mode lookup during preprocessing and estimation, not `info$focal`
  directly.
- A `stocnet` with no `info$focal` SHALL estimate when the formula/`layer` names
  the dependent — for one-mode and two-mode objects alike.
- Modeling a layer that is not `info$focal` SHALL resolve sides and modes against
  the **modeled** layer.
- Focal-name lookups (`ds_layer_map()` and the side resolvers) SHALL degrade to
  "no layer" on an absent focal rather than erroring in `[[`.
- `add_info(focal = ...)` remains supported and, when set to the modeled layer,
  behavior is unchanged; the frozen coefficient baselines SHALL be unaffected
  (they all set `info$focal` to the layer they model).
- The docs SHALL lead with the focal-less pattern — the formula LHS or
  `make_specification`'s `layer` names the dependent — and mention
  `add_info(focal = ...)` only as an **optional override**. The vignettes and
  roxygen `@examples` that currently teach `focal =` as required metadata SHALL
  be reframed accordingly (not removed: `focal` stays supported).

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `model-specification`: the requirement that the dependent process is named by
  `layer` / `info$focal` (with `layer` overriding) is extended — the resolved
  dependent drives focal/side resolution everywhere downstream, `info$focal`
  becomes an optional default, and a mismatch between `info$focal` and the
  modeled layer resolves in favor of the modeled layer.

## Impact

- **Code:** `R/model_estimate.R` (stamp the resolved dependent as the working
  object's focal at estimation entry), `R/data_source.R` (`ds_layer_map()` /
  side resolvers robust to an absent focal). Possibly `R/formula_parser.R` /
  `R/model_preprocess.R` if per-source threading is preferred over the stamp
  (weighed in `design.md`).
- **Behavior:** only changes for objects with an unset or mismatched
  `info$focal`; objects that set `info$focal` to the modeled layer are
  unaffected.
- **Tests:** new coverage for a focal-less hand-built object (one-mode and
  two-mode) and for modeling a non-default layer; frozen coefficient baselines
  must remain PASS (not SKIP) under `NOT_CRAN=true`.
- **Docs:** reframe the focal narrative to focal-less-first, `focal` shown only
  as an optional override:
  - `?goldfish_data` / `add_info()` note `focal` is an optional default;
  - vignettes `teaching1`, `teaching2`, `two-mode` (edit the `.orig` sources,
    re-precompile via `vignettes/precompile.R`) lead with the formula/`layer`
    naming the dependent;
  - roxygen `@examples` on `goldfish_data`, `as_goldfish`, `fisheries_treaties`,
    `social_evolution_stocnet` follow suit. The `focal` *argument* of
    `network_state_at()` / `nodes_state_at()` is unrelated (a function parameter,
    not `add_info` metadata) and is left as-is.
- **Sequencing:** this change is a successor to `multimode-network-support`; it
  reframes the two-mode vignette that change ships and proves its no-op claim
  against that change's frozen two-mode baselines. Close
  `multimode-network-support` first (see `design.md`).
