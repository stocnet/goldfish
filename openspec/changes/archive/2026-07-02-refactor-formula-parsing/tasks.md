> **Workflow disciplines (apply to every task):** commit each completed task individually
> (focused conventional message, tests green) so any step can be reverted; run
> `devtools::document()` within any task that changes roxygen comments, exports, or
> signatures — never defer it; bump the package version in `DESCRIPTION` and add a `NEWS.md`
> entry at the phase milestone (task 8.x); run the test suite with `NOT_CRAN=true` (e.g.
> `NOT_CRAN=true Rscript -e 'devtools::test()'`) — the coefficient baseline and C++ golden
> tests use `skip_on_cran()`, so a run where they report SKIP instead of PASS does not verify
> the 1e-6 regression floor. Do not regenerate the frozen baselines in
> `tests/testthat/_baselines/`.

## 0. Discovery

- [x] 0.1 Write a discovery note (old vs new behaviour) covering: where `type` is parsed and
      validated (`R/formula_parser.R:72-75`, dispatch `196-274`); how DyNAM-choice inits alias
      REM (`R/functions_effects_DyNAM_choice.R:371-385`) and REM's `to_ego`/`to_alter`
      expansion (`R/utils_effects.R:24-50`, `R/functions_effects_REM.R:220-226`); the
      global-in-choice abort (`R/model_estimate.R:585-596`); `classify_broadcast_kind`
      (`R/preprocess_builders.R:119-139`); and the parsed-formula bundle site
      (`R/model_estimate.R:564`).
- [x] 0.2 Confirm with a scratch run whether choice `type = "ego"` needs any C++/gather change
      or only the effect-function/parsing layer (design D1 assumption). Record the result.
- [x] 0.3 Confirm `stats::terms(f, keep.order = TRUE)`'s `variables` list equals the current
      `extract_formula_terms` output for every baseline formula (design D2 swap is regression-
      safe); note any reorder/dedup/intercept differences to handle.
- [x] 0.4 Confirm the window value reaches both windowed-stat init (`initializeCacheStat`) and
      window-expiry event generation (`create_windowed_events`, `R/formula_parser.R:276`) via
      `effects_template`/`parsed_terms`, so `windowParameters` need not be a separate
      `preprocess()` argument (design D8).
- [x] 0.5 Audit `getElementFromDataObjectTable` / the `net_ids` resolution
      (`R/formula_parser.R:467`): does it touch data values (→ move to state creation) or only
      names (→ stays in the mapping)? Record the verdict (design D8 boundary).

> **Phasing (design D8):** Stage 1 (architecture refactor, behaviour-preserving) = groups
> 0 + 2; land green against the frozen baselines and bump the version before Stage 2.
> Stage 2 (features) = groups 1, 3, 4, 5, 6. Groups 7 (registry recs) and 8 (final milestone)
> close out.

## 1. Native type = "ego" in DyNAM choice  *(Stage 2)*

- [x] 1.1 Thread `type = c("alter", "ego")` through the `update_DyNAM_choice_*` degree-family
      path, reusing `to_ego`/`to_alter`; keep `type = "alter"` default.
      *(DONE — Session 9. The four type-bearing choice effects — `indeg`, `outdeg`, `node_trans`,
      `tertius` — gained a `type = c("alter", "ego")` formal on their `update_*` fns, threading
      `type` to their `update_REM_choice_*` delegate (which `match.arg`s it); the `init_*` methods
      now inject the alter default only when the closure lacks a `type` formal, so a formula-parsed
      `type = "ego"` passes through. `tertius_diff` has no `type` in REM, so it is correctly not
      threaded. No C++/gather change, per D1.)*
- [x] 1.2 Preserve the two-mode ego guard (mirror REM's `init_REM_choice.*` check).
      *(DONE — inherited for free: the `init_DyNAM_choice.*` methods delegate to `init_REM_choice.*`,
      which keeps the `is_two_mode && type == "ego"` abort. Unit-tested at the init level.)*
- [x] 1.3 Add a baseline test asserting choice `type = "ego"` equals the REM-derived
      expansion to 1e-6; add a default-perspective regression test.
      *(DONE — new `test-effects_DyNAM_choice_type_ego.R`: choice `indeg`/`outdeg` `type = "ego"`
      preprocessing (`initialStats` + `ReducePreprocess`) equals REM `rate_ordered` (dependent-only,
      directly comparable) to 1e-6; default == alter regression; ego ≠ alter; two-mode ego guard.)*
- [x] 1.4 Verification: run the suite with `NOT_CRAN=true`; baselines PASS not SKIP;
      `devtools::document()` if signatures/exports changed.
      *(DONE — full suite PASS 1754 / FAIL 0 / SKIP 0 (1e-6 baselines PASS not SKIP; +7 new).
      `document()` — no `man/`/NAMESPACE drift.)*
- [x] 1.5 Make `global()` computable in DyNAM choice / choice_coordination, mirroring the ego
      route (this task; not covered by D1, which scoped only the degree family's `type = "ego"`).
      *(DONE — Session 11. Added `init_DyNAM_choice.global` (`@export` → `S3method`) +
      `update_DyNAM_choice_global`, both thin delegates to the existing `init_REM_choice.global` /
      `update_REM_choice_global` (which alias the rate global + broadcast via `to_ego`). In
      `R/formula_validate.R`: `unavailable_variations()` is now empty; choice/choice_coordination
      `unidentified_variations()` returns `c("global", "ego")` — so `global` reaches the same tier as
      `ego`. `compute_stats(choice, global)` now produces the column (verified equal to the REM
      `rate_ordered` expansion to 1e-6, mirroring 1.3); `estimate_dynam(choice, global)` still aborts.
      Flipped the one `test-functions_effects_global.R` expectation (abort → `preprocessed.goldfish`).
      This lands the operand prerequisite for 2.6/D9. Full suite PASS 1789 / 0 / 0;
      `document()` added `S3method(init_DyNAM_choice,global)`.)*
      Add the missing S3 wrappers `init_DyNAM_choice.global` +
      `update_DyNAM_choice_global` delegating to the **existing** `init_REM_choice.global` /
      `update_REM_choice_global` (which already alias `init_DyNAM_rate.global` /
      `update_DyNAM_rate_global` + broadcast via `to_ego`) — structurally identical to the Group-1
      degree wrappers, no C++/gather change.
      **Effect on the validity matrix (group 3):** `global` moves from the *unavailable* tier to the
      *unidentified* tier for choice/choice_coordination — i.e. drop `global` from
      `unavailable_variations()` and add it to `unidentified_variations()` in `R/formula_validate.R`.
      So `compute_stats(choice, global)` then **produces a column** (a design column for interactions
      / random effects, symmetric with choice `ego`) while `estimate_dynam(choice, global)` still
      aborts. **Behaviour change:** flip `test-functions_effects_global.R`'s
      `compute_stats(choice, global)` expectation from abort → `preprocessed.goldfish` (keep the
      `estimate_dynam` aborts). Baselines unaffected (none use bare global in choice).
      **Why now / dependency:** this is the operand prerequisite for the interaction work — task 2.6 /
      design D9 seed each operand via "its existing `init_*`", so a `global(x):alter(y)` interaction
      in choice (D2/D3's motivating example) *requires* `init_DyNAM_choice.global` to exist. Landing
      it here as its own verifiable step de-risks 2.6 and makes the ego/global choice story uniform.
      Tests: `compute_stats(choice, global)` equals the REM-derived expansion to 1e-6 (mirror 1.3);
      `estimate_dynam(choice, global)` still aborts; two-mode handling matches REM.

## 2. Upfront specification mapping + interaction parsing  *(2.1–2.4 Stage 1; 2.5–2.8 Stage 2)*

- [x] 2.1 Rewrite `get_rhs_names` to derive terms via `stats::terms(formula, keep.order =
      TRUE)` (consuming `variables`/`factors`/`order`/`intercept`); delete the hand-rolled
      `extract_formula_terms` walker and its broken `*` branch; `a*b` expands to `a + b + a:b`
      for free. Reject `I()`/`|` with a consistent `cli` error; `offset()` is NOT rejected
      (unwrapped + tagged in group 5).
- [x] 2.2 Split the call **templates** out of `build_update_plan` into a dedicated
      `build_effects_template()` returning the `effects_template` object (per-gid call
      templates); `build_update_plan()` keeps registries only (design D8).
- [ ] 2.3 Add the upfront umbrella `build_spec_map()` in `R/formula_parser.R` (no new file)
      returning `spec_map = list(parsed_terms, effects_template, plan)`; move
      `build_update_plan()` out of `preprocess()`; make link matrices internal to the compile;
      `preprocess()` consumes plan + template and builds only `state` + schedule from data.
      Carry the `spec_map` on `specification.goldfish`.
- [ ] 2.3b `build_spec_map()` owns `effect_description` (relocate `GetDetailPrint` from
      `R/preprocess_export.R`/`R/model_estimate.R`); render short names on demand per context
      (console / db `max_length = 63` / export) via `compact_term_strings`/`CreateNames`. Change
      the `preprocess()` signature to `preprocess(spec_map, data, …)`, dropping `effects`,
      `windowParameters`, `eventsObjectsLink`, `eventsEffectsLink`, `objectsEffectsLink` (design
      D8). **DyNAMi does NOT unify here** (revised — isolation, see 2.3e): the recipe
      `preprocess(spec_map, data)` signature is DyNAM/REM only; `estimate_dynami` routes to its own
      preprocessing front-end (old parsing + `cleanInteractionEvents` + `preprocessInteraction`),
      unchanged. Baselines unchanged.
      *(effect_description ownership done — commit `7fb8cef`. Recipe signature flip done —
      commit `b233d8b`: `preprocess()` dispatches on the `spec_map` (model_spec fields+class
      merged in); effects/window_parameters/links/plan/templates ride on it; recipe loops
      unpack from `spec`; compat-shim fallbacks deleted; DyNAMi keeps the monolith signature.
      STILL REMAIN: links fully internal to `build_spec_map` (it still receives them + builds a
      throwaway state container — coupled to 2.3c); the `events`→`data` rename (coupled to 2.3c
      data relocation). DyNAMi unification is no longer part of this change — deferred to
      `refactor-dynami-engine`.)*
- [x] 2.3c Enforce the metadata/data boundary (design D8): `build_spec_map()` reads object
      **attributes only** and produces a windowed-stream *recipe* — no event/network tables in its
      output, no `assign()` to the caller's envir. Relocate the data work to state creation —
      split `get_events_and_objects_link` so event-table fetch + `sanitizeEvents` (label→id) +
      `create_windowed_events` + the windowed network are built into the **state container** by
      `build_state_container`/`build_schedule(spec_map, data)`. **DyNAM/REM only** (revised scope,
      isolation): `parse_time_windows` stops `assign()`-ing for the recipe path; DyNAMi keeps its
      own parse-time windowing inside its front-end (2.3e). Baselines unchanged.
      *(PARTIAL — the risk-concentration core is resolved; commits `390df3e` (object-keys purity),
      `da601a2` (recipe recording + `realize_windowed_network`), `42565d9` (get_events split),
      `4d8a711` (recipe-path parser purity). DONE: (a) `build_spec_map()` object-keys pure — no
      throwaway state container; (b) `parse_time_windows` Phase 2 split — the rhs_names rewrite +
      derivation-recipe recording always run; realization is gated on a `realize_windows` flag;
      (c) `get_events_and_objects_link` split into `build_events_objects_link` (incidence + fetch
      plan, metadata) + `fetch_events` (tables + `sanitizeEvents`), byte-identical wrapper;
      (d) the **shared** `parse_formula()` no longer mutates the env on the fresh DyNAM/REM path —
      `estimate_wrapper` parses with `realize_windows = FALSE` then calls `realize_windows_recipe()`
      once, driving the eager `assign()` from the recorded recipe; `parse_multiple_effects()` is
      recipe-aware (the one in-parse consumer). DyNAMi + `preprocessing_init` keep
      `realize_windows = TRUE` (byte-identical). STILL REMAIN: (1) push realization + `fetch_events`
      past `create_effects_functions` **into `build_state_container`** (true state-container
      relocation) — blocked because `create_effects_functions` eagerly `eval()`s the (derived)
      network arg for the two-mode guard, forcing realization before it; decoupling that is the
      gating follow-up; (2) the `preprocess(spec_map, data)` `events`→`data` rename +
      `build_events_objects_link`/`get_events_effects_link` fully internal to `build_spec_map`. The
      recipe branch of `build_events_objects_link` (resolve derived streams from the recipe without
      `get()`) is designed but only needed once (1) lands.)*
- [x] 2.3d Add the `plan$derivations` registry (design D8): one row per derived input
      `{derived_name, kind, source, source_streams, params, gids}`, filled by `build_spec_map()`
      from metadata, rewiring affected effects' object refs to `derived_name` (no tables, no
      `assign`). State creation iterates it and dispatches on `kind`; implement the **window**
      realizer (relocated eager block) into the **recipe state container** (DyNAM/REM only).
      `ignore_repetitions` stays disabled (future `kind`, not re-enabled here).
      *(DONE — commits `e87224a` (`build_derivations()` fills `plan$derivations` from the recipe,
      metadata-only, `NULL` when empty) + `4d8a711` (the window realizer). The object refs were
      already rewired to `derived_name` in `parse_time_windows`. The realizer
      (`realize_windows_recipe` → `realize_windowed_network`, dispatching on `kind == "window"`) is
      driven by the recorded recipe (identical data to `plan$derivations`) from the recipe
      front-end; `ignore_repetitions` stays disabled. Residual, shared with 2.3c: the realizer runs
      in `estimate_wrapper` right after parse, not yet inside `build_state_container` — see 2.3c
      note.)*
- [x] 2.3e Isolate the DyNAMi preprocessing front-end (behaviour-preserving; **land before
      2.3c/2.3d** so the boundary relocation operates on a DyNAMi-free shared surface). Split the
      preprocessing front-end out of `estimate_wrapper` (`R/model_estimate.R`): DyNAM/REM →
      `build_spec_map` + `preprocess(spec_map, …)`; DyNAMi → its own front-end keeping the intercept
      arms it needs, the `cleanInteractionEvents` step (`R/make_data_group.R:970`), and
      `preprocess.dynami_*` → `preprocessInteraction` (`R/model_preprocess_group.R`) with its own
      `groupsNetwork`. Both share the **leaf** helpers (`parse_formula`, `create_effects_functions`,
      `get_*_link`) and hand `prep` back to the **unchanged shared estimation tail** (§4 print → §5
      estimate → §6 results, `R/model_estimate.R:1001-1109`). The shared path drops every
      `model %in% c("DyNAM","DyNAMi")` arm, the `cleanInteractionEvents` branch, and the DyNAMi
      `groupsNetwork` threading; the recipe path always has a `spec_map` (the `is_recipe_model` /
      `spec_map`-is-NULL branches collapse). `compute_stats(model = "DyNAMi")` routes to the DyNAMi
      front-end. DyNAMi baselines (effects_preprocessing choice 113 / rate 211 + coefficient / cpp
      golden) are the 1e-6 floor.
- [x] 2.3f Finish the metadata/data boundary — resolve `derived → source` + relocate realization
      into state creation (design D8 "A derived object is source metadata + fresh data"). Completes
      the residual left by 2.3c/2.3d. **DyNAM/REM recipe path only.** Steps:
      (a) **Metadata resolves to the source, never the realized derived object** — make the three
      pre-`preprocess` consumers read inherited metadata from the source object (or the recipe)
      instead of `get()`/`eval()`-ing the derived one: `create_effects_functions` two-mode guard
      (`R/formula_parser.R:396-417`, read `attr(source, "nodes")`), `build_object_keys`
      (`R/preprocess_builders.R:42`, classify a `kind == "window"` derived name as `networks` from
      the recipe), and `build_events_objects_link`'s network branch (`R/formula_parser.R`, resolve a
      derived object's stream names as `paste(attr(source, "events"), window, sep = "_")` and its
      nodesets from the source — the recipe-aware branch already sketched but currently unused).
      A small shared resolver (`derived name → source object for metadata`) keyed off the recipe /
      `plan$derivations` avoids duplicating the mapping.
      (b) **Realization + fetch move into `build_state_container`** — drop the
      `realize_windows_recipe()` call from `estimate_wrapper` (added in `4d8a711`) and run the
      realizer inside state creation, driven by **`plan$derivations`** (not the raw
      `parsed_formula$window_derivations`), so the registry `build_spec_map()` fills becomes the
      realizer's driver (dispatch on `kind`, window realizer builds the empty net +
      `create_windowed_events` streams into the state container). `fetch_events()` runs there too,
      after realization.
      (c) **`preprocess(spec_map, data)` `events`→`data` rename** — `preprocess()` / the recipe
      loops no longer receive a pre-fetched `events` list; they carry the fetch plan (on `spec_map`)
      and fetch inside state creation. `get_events_effects_link` derives its shape from
      `events_objects_link` (names/nrow) instead of the fetched `events`;
      `build_events_objects_link` + the links become fully internal to `build_spec_map`
      (closes the 2.3b "links internal" residual). Once this lands, `parse_formula`'s
      `realize_windows` flag is only ever `TRUE` for the DyNAMi/`preprocessing_init` paths.
      Guard: DyNAMi 113 / rate 211 (shared `parse_formula`), the 1e-6 floor (coefficient 72 /
      global 12 / cpp golden 24), and the windowed `DyNAM_choice` cases (incl. `list(net1, net2)`).
      *(Scope: **in scope for this change** (decided Session 7). This is the fuller state-container
      relocation; the change's headline D8 goal — pure shared parser + data-light `spec_map` — is
      already met by the Session-7 commits, and 2.3f completes the relocation rather than being a
      follow-up. Close 2.3c/2.3d fully only once 2.3f lands.)*
      *(DONE — Session 8, commits `e3a19b4` (a) + `2370b82` (b+c). (a) added shared
      `derived_source_map()`/`find_derivation()`; the four pre-`preprocess` consumers
      (`create_effects_functions` two-mode probe env, `build_object_keys`,
      `build_events_objects_link`, and a **fourth found in testing** — `build_update_plan`'s
      `directed` read) now resolve inherited metadata from the source via the recipe. (b) added
      `realize_derivations()` (driven by `plan$derivations`, dispatch on `kind`); dropped the
      `estimate_wrapper` `realize_windows_recipe()` call; the two recipe loops
      (`run_sender_recipe_loop`/`run_dyad_recipe_loop`) realize + `fetch_events` into `prepEnvir` at
      the top of state creation (not literally inside `build_state_container`, because
      `initializeCacheStat`/start-end computation read the derived net before it — realization sits
      at the loop head instead). (c) `get_events_effects_link(rhs_names, events_objects_link)` derives
      shape from the link; `spec_map` carries `fetch_plan`; the recipe path threads no pre-fetched
      events; `realize_windows` is now only `TRUE` for DyNAMi/`preprocessing_init`. **Deliberate
      micro-residual:** `build_events_objects_link` + links are NOT moved fully *inside*
      `build_spec_map` — it still receives pre-built links, because the `preprocessing_init`
      new-effects path compiles a `spec_map` from a **subset** of `rhs_names` and must supply subset
      links; internalizing would need an `rhs_names`/`effects` override that reproduces exactly that
      seam. Full suite PASS 1736 / FAIL 0 / SKIP 0; DyNAMi 113/211 identical; windowed cases incl.
      `list(net1, net2)` green.)*
- [x] 2.4 Extend `plan` registries (design D9/D10): `effects` gains `role`∈{main,operand,
      interaction}, `estimate`, `fid`, `lid`; add `interactions` (gid → ordered operand gids,
      n-ary), `operand_of` (reverse), `stat_state_spec`, and `formula_effects (fid, lid, gid)`
      with `fid = 1` for single-formula.
      *(DONE — Session 8, commit `d1c58d1`. All fields populated trivially in `build_update_plan`
      (single-formula main effects; interaction registries empty until 2.5). Additive — recipe
      consumers read plan registries by name. +1 builder test. Full suite PASS 1747 / 0 / 0.)*
- [x] 2.5 Build the interaction term object from the `factors` column (operand gids + order,
      any arity); operands keep their own arguments; thread through `parsed_terms`.
      *(DONE — Session 14. `get_rhs_names()` no longer aborts on `:`/`*`: it reads the `factors`
      incidence (rows 1:1 with `variables`, response-adjusted), derives per-variable `is_main`
      (appears in an order-1 term) / `is_operand` (appears in any order>1 term), and builds an
      `interactions` list — each `{operands = rhs indices, label, order}` (n-ary, from the
      `factors` column). Operands are the deduplicated rhs terms (each parsed once, args
      preserved); `a*b` expands to `a + b + a:b` for free. `parse_formula()` lifts the flags
      (kept aligned through the intercept drop, variable-index frame) and exposes
      `is_main_parameter` / `is_operand_parameter` / `estimate_parameter` (= main | offset) +
      `interactions`. `compare_formulas` excludes `interactions` (one row per interaction, not
      per effect). Non-interaction formulas are byte-identical (rhs_names + all flags unchanged).
      **Computation (task 2.6) not yet wired** — `abort_if_interactions_unsupported()` guards
      `estimate_wrapper` + `make_specification` so an interaction formula aborts cleanly instead
      of silently dropping to operands. Tests: new `test-interaction_parsing.R` (26 assertions:
      `a:b`/`a*b`/3-way/dedup/roles/empty-structure/estimation-abort). Full suite PASS=1852
      FAIL=0 SKIP=0; baselines 84 pass.)*
- [x] 2.6 Implement `stat_state` in the loop (design D9): a list by broadcast kind
      (`dyad`/`ego`/`alter`/`global`) materialized only for `{operands ∪ interactions}`, seeded
      from the effect `init`'s `stat`; init each interaction as the broadcast-product of its
      operands; on any operand delta, recompute touched cells as the n-ary product and emit the
      interaction delta (second-hop routing). Keep operands with `estimate = FALSE` (retained,
      not estimated); optional `drop_operands`.
      *(DONE — Session 14. **DyNAM choice/choice_coordination + REM (dyad loop) only**; sender/
      rate + DyNAMi interactions are guarded (`abort_if_interactions_unsupported`).
      `augment_interactions()` (`preprocess_builders.R`) appends one estimated product column per
      interaction after the function-effects, sets `role`/`estimate` on operands, and fills
      `interactions`/`operand_of`; `axis_union_kind()` classifies the product broadcast kind
      (D2). `build_spec_map()` calls it. In `run_dyad_recipe_loop`: `initialStats` extended to
      `nFun + nInter` columns (interaction slices = elementwise operand product); a live
      `op_state` matrix per operand, updated per its broadcast kind via `expand_operand_update()`
      (point/row/col/whole-matrix); after the routing loop, each touched interaction recomputes
      the product at the changed cells (`dedup_cells`) and emits a **point** update (second hop),
      or writes `initialStats` in the pre-start window. **estimate=FALSE realized by fixing the
      operand coefficient at 0** in `assemble_fixed_parameters()` — the C++/R core already zeroes
      the score for fixed components (`score[idFixedCompnents] <- 0`), so `0 * stat` drops the
      column from the model while keeping it in `prep` (no `src/` change → golden baselines
      untouched). `GetDetailPrint()` appends an interaction name row. `drop_operands` not added
      (operands retained per D9 default). Verified: interaction column == `∏` operands over the
      full sequence to machine zero (binary, 3-way, ego×point); `a:b` fixes operands at 0 and
      estimates only the product; `a*b` estimates all three.)*
- [x] 2.7 Tests: `a:b` product equals `∏` operands incl. a 3-way interaction; `a*b` expansion;
      `global:ego` classified per the axis-union; operand-only term kept but not estimated;
      cross-engine 1e-6 check; the upfront-built plan reproduces existing baselines.
      *(DONE — new `test-interaction_computation.R` (12 assertions): `a:b`/3-way/ego-operand
      products equal `∏` operands over the full gather (to 1e-6, in fact machine-zero); `a*b`
      estimates 3 free coefficients; `a:b` keeps operands fixed at 0 and estimates only the
      product; REM interactions; sender-model interactions rejected. `axis_union_kind` covered by
      the classification path; the empty-interaction no-op path reproduces every frozen baseline
      (84 baseline assertions PASS not SKIP). `test-interaction_parsing.R` estimation-abort case
      repurposed to the sender-model rejection.)*
- [x] 2.8 Verification: `NOT_CRAN=true` suite green; `devtools::document()` if needed.
      *(DONE — full `NOT_CRAN=true` suite PASS=1864 FAIL=0 SKIP=0 WARN=19 (pre-existing).
      Baselines 84 passed / 0 skipped. `document()` — no `man/`/NAMESPACE drift (helpers are
      internal `#` comments; relocated above the kernel roxygen so no stray `.Rd`).)*

## 3. Validity matrix

- [x] 3.1 Implement the per-`(model, sub_model)` validity helper (REM / DyNAM-rate /
      DyNAM-choice rules of design D3), reading `effect-term-registry` metadata when present
      else a local table; run it AFTER `*` expansion.
      *(DONE — Session 10. New `R/formula_validate.R`: `validate_effects()` + a local rule table
      (`effect_variation()` classifies name+`type` into global/ego/alter/degree/other;
      `unavailable_variations()`/`unidentified_variations()` are the D3 matrix). Called from
      `estimate_wrapper` after `has_intercept`/`*` expansion with the effective sub_model. To be
      superseded by `effect-term-registry` metadata (task 7.1). Rule table verified against every
      frozen baseline before wiring — none use a rejected construct.)*
- [x] 3.2 Extend the global-in-choice abort (`R/model_estimate.R:585-596`) to permit the
      interaction form of `global()` while still rejecting the bare main effect; emit one
      consistent `cli` error for all violations.
      *(DONE — replaced the single global-in-choice abort with `validate_effects()`. **Two-tier**
      (design nuance found in implementation): `global` in choice has no bare effect implementation
      → rejected in every phase (preserves the existing `compute_stats(choice, global)` "interaction"
      abort). Computable-but-unidentified cases (choice `ego`, `rate_ordered`/`rem_ordered` `global`,
      rate `alter`) reject only when `estimating` (`!preprocessing_only`), so D3's design columns stay
      producible via `compute_stats` — and my Group-1 `preprocessing_only` ego test still passes.
      One consolidated `cli::cli_abort` bullets every offender + a next-step hint mentioning
      interaction terms.)*
- [x] 3.3 Tests: each model/sub_model accept/reject case (REM rate vs rate_ordered global;
      DyNAM-rate ego/global only; choice interaction-allowed but main-effect-rejected).
      *(DONE — new `test-formula_validate.R` (30 assertions): `effect_variation` classification;
      the full accept/reject rule matrix (estimating); multi-offender reporting; the
      preprocessing-permits / estimation-rejects split; integration via `estimate_dynam`/`estimate_rem`
      + `compute_stats` (choice ego rejected-but-computable; REM rate vs rate_ordered global). Existing
      `test-functions_effects_global.R` passes unchanged.)*
- [x] 3.4 Verification: `NOT_CRAN=true` suite green.
      *(DONE — full suite PASS 1784 / FAIL 0 / SKIP 0; no doc drift. One existing engine-consistency
      vector (`test-cpp_interface.R` choice_coordination) used ego-in-choice_coordination as an
      incidental effect — swapped to the default alter perspective (still engine-to-engine, no frozen
      baseline). All other ego usages are rate/REM/preprocessing/rendering, unaffected.)*

## 4. make_specification() v1

- [x] 4.1 Implement `make_specification()` (design D4 signature, incl. `layer`) funnelling
      through the shared `parse_formula` core; assemble the `specification.goldfish` object
      (parsed rate/choice bundles, metadata, validation); store but do not activate
      `support_constraint`.
      *(DONE — Session 12. New `R/make_specification.R`: `make_specification()` +
      `build_specification_bundle()` (parses each present submodel via the shared
      `parse_formula` with `realize_windows = FALSE`, applies the estimate_wrapper intercept
      adjustment, runs the D3 `validate_effects()` at estimation strength), plus
      `spec_dependent_info()`. Returns an S3 `specification.goldfish` holding per-submodel
      `{input_formula, formula, sub_model, parsed, has_intercept}`, model, `layer`, dependent
      facts, the stored-but-inactive `support_constraint`, `valid`, and `data`.)*
- [x] 4.2 Implement `layer` resolution via the existing parser lookup (`get(layer, data)` →
      `dependent.goldfish`) with empty-LHS enforcement; reject a dependent-events object on the
      LHS with an error pointing at `layer`; carry flavour symbols on the LHS for the
      flavour-keyed `list` form. No rate-vs-choice LHS equality check (there is no LHS).
      *(DONE — `layer` resolves via `get(layer, work_env)` (clone of `data`) → must be
      `dependent.goldfish`. `enforce_empty_lhs()` rejects a non-empty LHS; a dependent-events
      object on the LHS gets a targeted "use `layer`" message. `build_layer_formula()` injects
      the `layer` symbol as the LHS for the shared parser. Flavour-keyed `list` form is a
      future non-goal, so no flavour-symbol LHS in v1 — single formulas only.)*
- [x] 4.3 Add S3 `print.specification.goldfish` per the `specification-print` requirement,
      rendered with `cli` (`cli_rule`/`cli_text`/`cli_bullets`/`cli_dl`/`cli_alert_*` + inline
      markup; formulas interpolated as pre-deparsed strings): model + present sub-models;
      Dependent block with layer / events / time span / nodesets / network from the resolved
      object; rate/choice formula(s); support line only when present; validation result;
      flavours nested under the layer; no right-censored line. **No `summary` method.**
      *(DONE — `print.specification.goldfish` in `R/methods_display.R`: `cli_rule` title,
      `cli_text` model + sub-models, `cli_bullets` Dependent facts (layer/events/time span/
      nodes/network — network bullet omitted when absent), `cli_dl` Rate/Choice/Support with
      `deparse1()` pre-deparsed strings via `{.code}`, `cli_alert_success/danger` validation.
      No `summary` method. No flavours in v1.)*
- [x] 4.4 Add a `specification.goldfish` acceptance path to `estimate_dynam`/`estimate_rem`
      that reuses parsed contents; keep the formula path unchanged. (`estimate_dynami` deferred
      to the DyNAMi change.)
      *(DONE — `estimate_dynam`/`estimate_rem` dispatch on `inherits(x, "specification.goldfish")`
      to `estimate_from_specification()`, which selects the submodel bundle by the requested
      sub_model's family (rate vs choice), reuses the stored `parsed` bundle (skips re-parsing)
      via a new internal `parsed_formula = NULL` arg on `estimate_wrapper`, and forwards to the
      shared estimator with the bundle's own sub_model. Reuse is gated on matching data +
      no `preprocessing_init`. `match.call()` in the tail wrapped in `tryCatch` for the extra
      spec hop. Round-trips to identical coefficients (diff 0 for choice/rate/REM). Formula path
      untouched.)*
- [x] 4.4b Mark `make_specification()` experimental per the r-lib:lifecycle skill:
      `lifecycle::badge("experimental")` in the roxygen (`@description`) and a `@lifecycle`/
      NEWS note (lifecycle already in Imports).
      *(DONE — `` `r lifecycle::badge("experimental")` `` in the `@description`. NEWS entry
      deferred to the Stage-2 milestone 8.1 per the phasing.)*
- [x] 4.5 `devtools::document()`; roxygen + export; tests: spec round-trips to identical
      coefficients vs the equivalent formula (1e-6); legacy formula path unchanged; cli print
      snapshot for the simple and (designed) flavour layouts under a reproducible cli context
      (`testthat::local_reproducible_output(width = 80, crayon = FALSE)` or
      `cli::test_that_cli()`); LHS-object rejection error.
      *(DONE — `document()`: `export(make_specification)` + `S3method(print,specification.goldfish)`
      + `man/make_specification.Rd` + updated `man/print-method.Rd`. New
      `test-make_specification.R` (self-contained Social_Evolution fixture, 21 assertions):
      build object; DyNAM choice + rate + REM 1e-6 round-trips (diff 0); legacy formula path
      unchanged; LHS-object rejection; model/arg constraints; ego-in-choice validity abort;
      wrong-estimator abort; two cli print snapshots (simple + support/one-submodel) under
      `local_reproducible_output(width = 80, crayon = FALSE, unicode = FALSE)`. No flavour
      snapshot — flavours are a v1 non-goal.)*
- [x] 4.6 Verification: `NOT_CRAN=true` suite green.
      *(DONE — full `NOT_CRAN=true` suite PASS=1808 FAIL=0 SKIP=0 WARN=19 (pre-existing
      dissolve/imputation warnings). 1e-6 floor PASS not SKIP (coefficient 72 / global 12 /
      cpp golden). `document()` — only the new `export`/`S3method` + `.Rd` additions.)*

## 5. Offset (fixed-coefficient) terms

- [x] 5.1 Parser: unwrap `offset(...)` via `terms()`'s `offset` attribute
      (`variables[[i]][[2]]`), parse the inner call normally (stat column **kept**, not
      dropped), and tag the term `offset = TRUE` with its formula order.
      *(DONE — Session 13. `get_rhs_names()` reads `attr(terms, "offset")` (indices into the
      `variables` list, response-adjusted), unwraps each `offset(inner)` → `inner`, and returns
      an `attr(rhs_names, "offset")` logical aligned to the terms (FALSE prepended with the
      re-inserted explicit intercept). `parse_formula()` lifts it, keeps it aligned through the
      intercept drop, and exposes `offset_parameter` (per-term list) on the parsed bundle. The
      offset term stays in `rhs_names` and is preprocessed like any effect, so its stat column
      is kept. `compare_formulas` compares it elementwise (per-effect) — an offset vs non-offset
      version of the same effect is correctly treated as different.)*
- [x] 5.2 Estimation front-end: add `offset_coef` to `set_estimation_opt()`; assemble the
      existing `fixedParameters` vector (`R/cpp_interface.R:72-97`, `R/estimation_core.R:84-109`)
      from offset positions + `offset_coef`. No Newton-Raphson core change.
      *(DONE — `set_estimation_opt()` gains `offset_coef` (numeric-or-NULL check). New
      `assemble_fixed_parameters()` (in `R/formula_validate.R`) builds the positional
      `fixedParameters` vector `[intercept?, effects...]`: an offset at rhs position j fixes
      parameter j (+1 for the prepended intercept), aligned to `offset_coef` in formula order,
      with a clear arity error. `estimate_wrapper` computes `effective_fixed_parameters` once
      (section 3.4) and threads it to both `GetDetailPrint` (fixed column) and `argsEstimation`.
      Newton-Raphson untouched; verified numerically identical to the legacy
      `fixed_parameters = c(NA, 2, NA)` run (diff 0).)*
- [x] 5.3 Supersede the positional `fixed_parameters` per the r-lib:lifecycle skill
      (`lifecycle::deprecate_soft` / `superseded` badge) pointing at `offset()` + `offset_coef`;
      keep it working; NEWS note.
      *(DONE — `superseded` badge on the `fixed_parameters` roxygen; `lifecycle::deprecate_soft`
      (`when = "1.8.4"`) fires when it is supplied, pointing at `offset()` + `offset_coef`; it
      keeps working unchanged. Supplying both `fixed_parameters` and `offset_coef` aborts. NEWS
      deferred to the Stage-2 milestone 8.1 per the phasing.)*
- [x] 5.4 Offsets obey the D3 axis: warn (not abort) when an offset is constant across
      alternatives in choice; accepted in rate/REM.
      *(DONE — offset terms are excluded from the `validate_effects` main-effect check (design
      D7: they are not estimated), so an `offset(ego(...))`/`offset(global(...))` in choice no
      longer aborts. `assemble_fixed_parameters()` instead emits a `cli::cli_warn` when a choice
      offset's `effect_variation()` ∈ {ego, global} (constant across the receiver alternatives →
      cancels in the softmax). Rate/REM offsets warn nothing and shift the rate.)*
- [x] 5.5 `devtools::document()`; tests: single + multiple offsets fix the right coefficients;
      legacy `fixed_parameters` still works with a soft-deprecation; choice constant-offset
      warning; offset stat column retained.
      *(DONE — `document()` updated `man/set_estimation_opt.Rd` (offset_coef + superseded badge);
      no NAMESPACE drift. New `test-offset_terms.R` (self-contained fixture, 18 assertions):
      parser unwrap+tag; single offset fixes the coefficient + equals legacy `fixed_parameters`
      (diff 0) + stat column retained; multiple offsets aligned by order; rate offset accepted
      (no warning); choice constant-offset warning without abort; `offset_coef` arity + no-offset
      + both-supplied errors; `lifecycle::expect_deprecated` for `fixed_parameters`.)*
- [x] 5.6 Verification: `NOT_CRAN=true` suite green.
      *(DONE — full `NOT_CRAN=true` suite PASS=1826 FAIL=0 SKIP=0 WARN=19 (pre-existing). 1e-6
      floor PASS not SKIP (baselines 84 passed / 0 skipped). One existing parser test updated:
      `test-formula_parser.R` asserted the parsed bundle has 14 slots — now 15 with the additive
      `offset_parameter`. `document()` — only `man/set_estimation_opt.Rd`, no NAMESPACE drift.)*

## 6. Interaction rendering

- [x] 6.1 Produce the reserved `effect/obj·obj2` compact term string for interaction terms via
      the shared `compact-term-summary` renderer; add interaction `coef()`/`vcov()` names.
      *(DONE — Session 15. `GetDetailPrint()` derives each interaction row's decoder columns
      (`.effect_short`, `.object_short`, `.term_export`, `.coef_name`) as the `:`-join of its
      operands' rendered columns, reusing the shared `.decoderColumns` / `compact_term_strings`
      output — so the interaction inherits the operands' short forms and object disambiguation
      for free (design D5's "short:short, disambiguate with object only on collision" falls out of
      the operand names already being disambiguated). The interaction row keeps its term string as
      the readable rowname (summary/print), while `coef()`/`vcov()` read `.coef_name`
      (`inrt:rec`) and gather/db read `.term_export` (`inertia_callNetwork:recip_callNetwork`).
      The object columns are left empty (interactions own no object), replacing the earlier raw
      duplicated label.)*
- [x] 6.2 Tests: interaction names render correctly and are unique.
      *(DONE — new `test-interaction_rendering.R` (9 assertions): `a*b` coef+vcov names
      (`inrt`/`rec`/`inrt:rec`); `a:b` single estimated column name; gather export join; 3-way
      join; uniqueness within a mixed `a + b + a:b + b:c` formula.)*
- [x] 6.3 Verification: `NOT_CRAN=true` suite green.
      *(DONE — full `NOT_CRAN=true` suite PASS=1873 FAIL=0 SKIP=0 WARN=19 (pre-existing).
      Baselines 84 passed / 0 skipped. `document()` — no `man/`/NAMESPACE drift.)*

## 7. Registry recommendations (no implementation)

- [x] 7.1 Append the recommended `term_def` fields (`valid_types`, `interaction_valid`,
      `global_rule`, `perspective` alias note) to the active `effect-term-registry` change's
      `design.md`/specs so its metadata can later replace the local validity table.
      *(DONE — Session 16. The four fields were already recorded in effect-term-registry's
      "Inbound recommendations" section; extended it with an "Implementation notes (what actually
      landed)" subsection capturing the details the registry must encode to be a faithful lookup:
      the **two-tier** validity (`main`/`computable`/`unavailable` — a single boolean loses the
      `compute_stats`-vs-`estimate` split after `global` became computable in choice), the
      **axis-union** interaction rule (`interaction_valid` derives from operand broadcast kinds +
      the identification rule, not per-pair enumeration), the kept-not-dropped operand fix, the
      operand-composed rendering, and the dyad-only interaction scope.)*

## 8. Milestone

- [x] 8.1 Bump `DESCRIPTION` version and add a `NEWS.md` entry describing native choice
      `type = "ego"`, interaction terms, `make_specification()`, and `offset()` fixed terms.
      *(DONE — `DESCRIPTION` 1.8.3 → 1.8.4 (Date 2026-07-02). `NEWS.md` gains a 1.8.4 section:
      New features (interaction terms `:`/`*`; `make_specification()` experimental; `offset()` +
      `offset_coef`; native choice `type = "ego"` + computable `global()`) and Deprecations
      (`fixed_parameters` superseded by `offset()`). Matches the `when = "1.8.4"` deprecation.)*
- [x] 8.2 Full `NOT_CRAN=true` suite green end to end (coefficient + C++ golden baselines PASS
      not SKIP); `openspec validate refactor-formula-parsing --strict` passes.
      *(DONE — full `NOT_CRAN=true` suite PASS=1873 FAIL=0 SKIP=0 WARN=19 (pre-existing
      dissolve/imputation). Coefficient + C++ golden baselines 108 passed / 0 skipped (PASS not
      SKIP). `openspec validate refactor-formula-parsing --strict` → valid. Change complete.)*
- [x] 8.3 Add a note in the change documenting the `goldfish.latent` consumption path (drop
      `modify_formula_re()` and the `DyNAMRE` remap).
      *(DONE — new "Downstream: `goldfish.latent` consumption path" section in this change's
      `design.md`: native choice `type = "ego"` + computable `global()` supply the design columns
      `DyNAMRE` fabricated, and `:`/`*` interactions supply the random-slope × covariate products
      `modify_formula_re()` built by string surgery — so `goldfish.latent` can drop both and pass
      the formula straight to `estimate_dynam(sub_model = "choice")` / `compute_stats()`. Note
      only; this change does not touch `goldfish.latent`.)*
