# Discovery notes — refactor-formula-parsing (S1, group 0)

Working notes (local, gitignored). Records the old-vs-new behaviour audit gating the
`terms()` parser rewrite (2.1) and the Stage-2 features.

## 0.1 — Where `type` / interactions / global are parsed and validated today

- **`type` extraction** (`R/formula_parser.R:72-75`): `type_parameter` is read per term by
  `getElement(x, "type")` then `eval(parse(text = v))`. The value rides as a normal named
  argument inside the effect call; the parser does not validate it — validity is enforced
  downstream by the effect `init`/`update` functions.
- **Choice inits alias REM** (`R/functions_effects_DyNAM_choice.R:371-381`):
  `init_DyNAM_choice.indeg` injects `type = "alter"` into the effect formals then delegates to
  `init_REM_choice.indeg`. REM's init (`R/functions_effects_REM.R:144-159`) builds the n1×n2
  `stat` matrix per `type` and carries the two-mode ego guard
  (`is_two_mode && type == "ego"` → stop).
- **REM `to_ego`/`to_alter` expansion** (`R/utils_effects.R:24-50`,
  `R/functions_effects_REM.R:207-227`): the REM choice *update* threads `type` and expands the
  rate effect's 2-col `(node1, replace)` change into the 3-col dyad `(node1, node2, replace)`
  form via `to_ego` (repeat across alters) / `to_alter` (repeat across egos).
- **Choice update hardcodes `type = "alter"`** (`R/functions_effects_DyNAM_choice.R:434-446`):
  `update_DyNAM_choice_indeg` delegates to `update_REM_choice_indeg` with a literal
  `type = "alter"` and exposes no `type` formal. **This is the Stage-2 (task 1.1) seam** — the
  parser already preserves a user-supplied `type` arg, but the choice update discards it.
- **Global-in-choice abort** (`R/model_estimate.R:585-596`): a post-parse guard aborts when
  any term's effect name is `"global"` under `sub_model ∈ {choice, choice_coordination}`.
  Task 3.2 extends this to permit the *interaction* form while still rejecting the bare main
  effect.
- **`classify_broadcast_kind`** (`R/preprocess_builders.R:119-139`): maps an effect to a
  broadcast kind in `{0 point, 1 alter, 2 ego, 3 global}` from `effect_name` + resolved `type`
  + plan-level `stat_kind` (`sender` collapses to `{3, 0}`). This is the axis the D2 product
  lattice and the D3 identification rule both build on.
- **Parsed-formula bundle site** (`R/model_estimate.R:564`): `parse_formula(formula,
  envir = work_env)` produces the `rhs_names` + per-term parameter lists consumed by
  preprocessing. The `build_spec_map` umbrella (S2/S3/S4) is assembled around this seam.

## 0.2 — Does choice `type = "ego"` need a C++/gather change? **No.**

Scratch run (`/tmp/disc_misc.R`): `update_REM_choice_indeg(..., type = "ego")` and
`type = "alter"` both return change matrices with identical shape — columns
`(node1, node2, replace)`, same row count. Choice already materializes **dyad-format** stats
(the init builds an n1×n2 matrix; updates emit `(node1, node2, replace)` rows), and the
gather/C++ path already consumes that format. `type = "ego"` only changes *which* dyads receive
the fanned-out value (`to_ego` vs `to_alter`), not the format. **Verdict: Stage-2 task 1.1 is
an effect-function/parsing-layer change only** — thread `type` through `update_DyNAM_choice_*`
and drop the hardcoded `type = "alter"`; no C++/gather change. Confirms design **D1**.

## 0.3 — `terms(f, keep.order = TRUE)` reproduces `extract_formula_terms`? **Yes**, with caveats.

Scratch run (`/tmp/disc_03.R`): for every baseline (non-interaction) formula tested (f1–f8:
plain effects, object args, `type=` args, leading `1`, `$` attributes, `window=`, `weighted=`,
`list(...)`), the `terms()` `variables` list — after dropping the response and the intercept —
reproduces the old walker's per-term deparsed structure **exactly** (`all.equal == TRUE`).
Differences to handle in the 2.1 rewrite:

1. **Response variable**: `attr(., "variables")` includes the LHS at index
   `attr(., "response")`. Drop it.
2. **Intercept moves to an attribute**: `terms()` records the intercept in
   `attr(., "intercept")` (default **1**) and removes the literal `1` term. goldfish semantics
   are the *opposite* of R's default — `has_intercept` is TRUE only for an **explicit leading
   `1`** (`parse_intercept`, order-sensitive), and `terms()` cannot distinguish implicit from
   explicit. **The rewrite must keep a dedicated explicit-`1` check** (descend the leftmost
   operand of the RHS `+`-tree and test for literal `1`) — *not* read `attr(., "intercept")`,
   which is 1 for every default formula. This is the one place `terms()` is insufficient.
3. **`variables` ≠ model terms**: `attr(., "variables")` lists each **unique operand call once**
   (e.g. `inertia * recip` → variables `inertia, recip`); the **model terms** (incl.
   interactions) are `attr(., "term.labels")` / the columns of `attr(., "factors")`, with arity
   in `attr(., "order")`. The rewrite parses each *variable* once with the existing call-parser,
   then builds *terms* from the `factors` columns. For non-interaction formulas every term has
   order 1 and maps 1:1 to a variable, which is why the walker matched — but the two concepts
   must be kept distinct (this is the D2 / interaction-object foundation).
4. **`*` expands for free**: `a*b` → `a + b + a:b` (order `1,1,2`). The old walker's `*` branch
   was wrong (treated `*` like `+`, no interaction) — deleting it is a strict fix.
5. **`:` builds an interaction term**: operands of an interaction term = the nonzero rows of its
   `factors` column (excluding response). 3-way `a:b:c` → one order-3 term with three operands.
   (`factors` codes interaction operands as `2` vs main-effect `1`; treat any nonzero as
   operand.)
6. **Dedup**: `terms()` collapses *identical* terms (`inertia + inertia` → one). The old walker
   kept duplicates. Harmless/desirable; no baseline has exact-duplicate terms (terms differing
   only by args — e.g. `indeg(net, type="ego") + indeg(net, type="alter")` — are distinct labels
   and are preserved).
7. **`offset()`**: appears in `variables` *and* is indexed by `attr(., "offset")`; unwrap the
   inner call `variables[[i]][[2]]` (group 5 / D7). Not rejected.
8. **`I()` / `|` are NOT rejected by `terms()`**: `y ~ I(x^2) + a` keeps `I(x^2)` as a label and
   `y ~ a | b` keeps `a | b`. The rewrite must **explicitly reject** `I()` and `|` with a
   consistent `cli` error (D2), since `terms()` admits them silently.

Confirms design **D2** swap is regression-safe for the term structure; the intercept and
I()/`|` caveats are handled in code.

## 0.4 — Window value reaches both windowed-stat init and expiry generation? **Yes.**

`window_parameters <- lapply(rhs_names, getElement, "window")` (`R/formula_parser.R:57`)
captures the per-term window (a name/string) *before* `parse_time_windows` strips it.
- **Window-expiry generation**: `parse_time_windows` (`R/formula_parser.R:490-667`) evaluates
  the window to a numeric and eagerly builds the dissolve stream via `create_windowed_events`
  (`R/formula_parser.R:276-292`) + a zeroed windowed network, rewiring the term's object ref to
  the windowed name.
- **Windowed-stat init**: `preprocess()` passes `windowParameters` straight into
  `initializeCacheStat` (`R/model_preprocess.R:294-304`) and derives the `isWindowEffect` mask
  from it (`R/model_preprocess.R:248`).

Both consumers derive from the **per-term window field** of the parsed terms, so
`windowParameters` need not remain a separate `preprocess()` argument — it is recoverable from
`parsed_terms` (and the effect closures). Confirms design **D8**: the window becomes a
`plan$derivations` row (kind `window`) realized at state creation. *(Mechanics relocation is
S3, tasks 2.3c/2.3d — this only confirms the value is reachable.)*

## 0.5 — `getElementFromDataObjectTable` / `net_ids` (`R/formula_parser.R:467-473`): values or names?

In `parse_multiple_effects`, the `!multiple_param` branch calls
`getElementFromDataObjectTable(table, envir)` (`R/utils.R:86-126`), which **`get()`s the actual
objects**, then `vapply(..., inherits, what = "network.goldfish")` to find which object is a
network and take its `name`. So it *fetches the object value* but only inspects its **class**
(`inherits`) — it never reads network cells or event rows; the only output used is the
network's `name`.

**Verdict (D8 boundary):** this is **name resolution**, satisfiable from object **class
metadata** (which `getDataObjects` row is a network). It can stay in the metadata mapping — it
does not need event/network table contents — but the *implementation* must switch from
`get()`-and-`inherits()` to a metadata/class lookup so `build_spec_map()` stays pure (no
`get()` of data values). Practically moot today because this branch only feeds
`ignore_repetitions`, which is **disabled** (`R/model_estimate.R:574-583`); the `net_ids`
name is otherwise unused. Note for the future `ignore_repetitions` realizer (`plan$derivations`
kind), not re-enabled here.
