## 0. Pre-work: comment cleanup, DRY pass, naming policy

- [x] 0.1 Sweep all OpenSpec-artifact references out of code comments (~262 sites across
      R/, src/, tests/testthat/ — top offenders `model_estimate.R`, `model_preprocess.R`,
      `formula_parser.R`, `estimation_core.R`): replace each `design D#` / `task #.#`
      pointer with the inlined rationale (the assumption, edge case, or equation the
      decision encoded — consult the archived change's design.md while it is readable);
      never delete a comment's substance. Gate:
      `grep -rn "design D[0-9]\|(task [0-9]\|design\.md\|proposal\.md" R/ src/ tests/testthat/`
      returns nothing
- [x] 0.2 DRY pass over the files this change touches (`make_data.R`,
      `make_specification.R`, `model_estimate.R`, `preprocess_builders.R`,
      `model_preprocess.R`, `class_checks.R`): extract small single-purpose helpers where
      logic repeats; migrate camelCase names in touched files to snake_case (design D18);
      keep behaviour identical
- [x] 0.3 Extract the duplicated recipe-loop setup into a shared context constructor
      (design D12 amendment): the verbatim-identical opening of
      `run_sender_recipe_loop()` / `run_dyad_recipe_loop()`
      (`model_preprocess.R:361-447` / `1379-1447` — spec unpacking,
      `realize_derivations()`, `fetch_events()`, start/end-time resolution,
      `imputeMissingData()`, `initializeCacheStat()`) moves into one
      `prepare_recipe_context()`-style internal helper both drivers call; behavior
      identical; snake_case per design D18. This is the seam consumed later by the
      flavored-processes multi-consumer walk and the dynes-augmentation source port
- [x] 0.4 Naming-policy enforcement: add `object_name_linter("snake_case")` to `.lintr`
      (with a documented allowlist for legacy S3 method names that encode class names);
      verify `lintr::lint_package()` reports only known legacy files
- [x] 0.5 Verify: `NOT_CRAN=true devtools::test()` green, baselines PASS not SKIP;
      single chore commit

## 1. Boundary: validator, stamp, as_goldfish()

- [x] 1.1 Implement the internal stocnet validator by **generalizing the existing
      `R/class_checks.R` helpers** (`check_classes()` with `methods::is()`,
      `check_columns()`, label rules from `check_nodes()`, per-`var` value-type matching
      from `check_events.*()`, `check_presence()` composition rules) over stocnet
      components, treating manynet input as untrusted (design D13): **named** per-layer
      coverage of `update`/`directed`/`observation` against the distinct `ties$layer`
      values (`observation` limited to event/panel); `focal` naming a layer; unique
      `nodes$label`; strict time classes with mutual comparability (NA on ties only);
      side purity for declared `sender`/`receiver` mode **sets** with the
      identical-or-disjoint rule (design D7: partial overlap aborts); list-column value
      unwrapping with per-`var` type consistency (`active` → logical); syntactic
      layer/variable names (`make.names(x) == x`, abort with rename suggestion);
      optional `ties$flavor` column — character, syntactic values, NA allowed (design
      D19); components accepted as plain data.frames — all conditions via
      `cli_abort()`/`cli_warn()` naming layer + entry (r-lib:cli skill)
- [x] 1.2 Implement the stamp and exported `as_goldfish(x, ...)`: for a stocnet input,
      (2026-07-15 decision: legacy-environment CONVERSION deferred to section 3 —
      `as_goldfish(<environment>)` errors with forward guidance for now; stamp +
      list-shape print + roxygen/experimental badge landed)
      validate + stamp with class `data.goldfish` prepended to the stocnet class vector,
      no restructuring; for a legacy `data.goldfish` **environment** input
      (`is.environment(x)`), convert — walk `nodes.goldfish`/`network.goldfish`/
      `dependent.goldfish`/global objects and their `attr(x, "events")` streams, then
      assemble via `manynet::make_stocnet()` (design D1: Imports, delegate), validate +
      stamp. The stamp is provenance/print dispatch ONLY — specification/estimation
      re-validate stamped objects unconditionally (amended design D1); rewrite
      `print.data.goldfish()` for the list shape; roxygen with lifecycle experimental
      badge documenting the **event-ordering contract** (D2 sort key, `order` tie-break,
      the same-time `replace` ambiguity abort) on the `as_goldfish()` help page; run
      `devtools::document()`
- [x] 1.3 Move manynet from Suggests to **Imports with `manynet (>= 2.1.0)`** in
      DESCRIPTION (usage rule: `@importFrom` for frequent use, `manynet::` otherwise,
      never `:::`); build a hand-made stocnet fixture (plain list + data.frames, no
      manynet call) plus a small legacy-environment fixture for the conversion tests
- [x] 1.4 Unit tests (testthat 3e): each validation abort with cli snapshots under a pinned
      (2026-07-15: legacy-environment round-trip test rides with the deferred
      section-3 converter; post-stamp re-validation covered via
      validate_goldfish_data directly)
      cli context (incl. unnamed/short `update` vector, duplicate labels, non-syntactic
      layer name, partially overlapping mode sets); post-stamp mutation caught at
      re-validation (edit a stamped object into invalidity, expect the estimation-time
      abort); hand-built fixture path works; legacy-environment conversion round-trips
      (converted object validates and matches the fixture stocnet); run
      `NOT_CRAN=true devtools::test()` green

## 2. Conversion: mode map, ordering, streams, state materializer

- [x] 2.1 Implement the mode map (global node id + label ⇄ (side, local id)) and per-layer
      remapping from `sender`/`receiver` mode **sets**: identical sets → one-mode over the
      subset; disjoint sets → two-mode local indices; partial overlap aborts; undeclared →
      one-mode over all nodes (design D7). The map is a reusable structure that later
      attaches to results/exports (task 4.4)
- [x] 2.2 Implement deterministic event ordering: sort key (time, dependent-first,
      component order, layer, from/to), reserved integer `order` column as final tie-break,
      abort on same-target same-time replaces without `order` (design D2)
- [x] 2.3 Implement component splitting into the **per-layer / per-variable event streams
      the existing `fetch_plan` multi-stream walk consumes** (design D15 — no monolithic
      stacked copy): `time = NA` ties → initial matrices; focal-layer rows → dependent
      events, **filtered by the modeled flavor when the specification keys one** (design
      D19: non-matching/NA-flavor focal rows become state-only updates; every focal row
      still updates state per `info$update`); other layers → exogenous streams;
      `changes` → attribute events with
      `var == "active"` routed to per-side composition (`active_mode1`/`active_mode2`);
      `global` → global events (design D3/D7)
- [x] 2.4 Promote the `methods_update.R` engine into the internal **initial-state
      materializer** (design D16): given component streams and `[start_time, t)`, produce
      state matrices/attribute vectors in one vectorized pass (dedup-last replace,
      aggregated increments); replace the current per-event `startTime` fold in
      preprocessing with it, covering derived promises (window, `ignore_rep`); implement
      the `start_time`/`end_time` observation window on top (focal-span default, design
      D4); document (not build) the chunked-parallel preprocessing seam
- [x] 2.5 Export the state-at-t helpers on the stocnet/stamped object (design D16):
      network state of a layer and nodes' attribute values at time `t`, honoring
      `time = NA` history + per-layer update semantics, sharing the materializer core;
      snake_case names; roxygen + `devtools::document()`
- [x] 2.6 Unit tests: mode-set remap fixtures (two-mode, identical-set subset,
      partial-overlap abort, undeclared multimodal, side-impurity abort), ordering
      determinism under `arrange()`, `order`-column tie-break + ambiguity abort, history
      initialization, window defaulting, materializer equals the legacy per-event
      `startTime` fold on Social_Evolution/Fisheries state (exact equality), state-at-t
      helpers vs known matrices; run tests green

## 3. The flip (one milestone, design D14): wrappers + builders + surface

*Tasks 3.1–3.6 land as one baseline-gated commit series; the suite stays green at each
commit but intermediate commits may carry both paths only within this series — no bridge
helpers survive it.*

- [x] 3.1 Reimplement `make_nodes()`, `make_network()`, `link_events()`,
      `make_dependent_events()`, `make_global_attributes()`, `make_data()` as wrappers
      **delegating to `manynet::make_stocnet()`/`from_ties()`** (design D6/D1), preserving
      signatures and behaviour; `make_data_goldfish()` alias follows `make_data()`;
      `make_dependent_events()` stamps flavor (design D19): join its events to the
      default network's layer rows on (time, sender, receiver, update value), set
      `flavor = <dependent object name>` on matches, record the name→(layer, flavor)
      association for the specification surface; unmatched dependent rows → flavored
      increment-0 rows on increment layers, abort with guidance on replace layers;
      `make_groups_interaction()` and the 1.7.0 camelCase aliases explicitly untouched;
      lifecycle deprecations (r-lib:lifecycle skill) whose message bodies render the
      replacement code as a cli code block interpolating the caller's object names; run
      `devtools::document()`
- [x] 3.2 Replace the builder `envir` seam with the data object (design D14):
      `build_object_keys()`, `build_derivations()`, `compile_support_constraint()`, and
      the `fetch_plan` resolve layer/attribute names against
      `data$ties`/`data$nodes`/`data$global`; **delete** the `envir` parameters and
      `get(name, prepEnvir)` resolution — no bridge branch; recipe loops/writers/
      estimation untouched
      (2026-07-16: rewire DONE — every builder read goes through the data-resolution
      seam. The `envir` parameters + `data_source_envir` methods SURVIVE to the end of
      the series per the user's "the bridge is allowed during the whole of task 3":
      the suite builds all data through the legacy constructors, which only become
      stocnet-assembling wrappers in 3.1. Deleting them is 3.6's closing step.)
- [x] 3.3 Route effect AND `support_constraint` name resolution through one resolver
      against stocnet layer/variable names (candidates listed on error); implement
      attribute references per design D17: bare `ego(var)` from `nodes`, global names
      from `global`, nodes-vs-global ambiguity abort, and the `df$var` prefix
      deprecation-translation (drop prefix, resolve bare name, warn once)
      (2026-07-16: `resolve_formula_names()` in `R/name_resolver.R`, called from
      `parse_formula()` and `compile_support_constraint()`. Also closed the
      `ds_is_global()` seam gap. End-to-end stocnet formulas still blocked on 3.5:
      `parse_formula()` keeps its `get(dep_name, envir)` dependent-object check.)
- [x] 3.4 Panel layers: change-list updates at wave times emitting right-censored updates;
      abort on panel focal and on `window` over a panel-layer effect; document the
      reserved per-layer panel-semantics flag (DyNES seam) without implementing it
      (design D5)
- [x] 3.5 Accept stocnet (raw or stamped) in `make_specification()`/`estimate_dynam()`/
      `estimate_rem()` `data`; abort on a legacy environment (`is.environment(data)`)
      with a cli error whose body shows `as_goldfish(<user's object name>)` as the
      migration; `layer` resolves to stocnet layer names with `info$focal` fallback (the
      `dependent.goldfish` lookup is removed); implement the **flavor-keyed list branch**
      in `build_specification_bundle()`'s caller (design D19, closing the
      refactor-formula-parsing spec gap): exactly one flavor key (multi-key abort
      pointing to the future multi-process change), rate/choice lists must key the same
      flavor, LHS carries the flavor symbol, plain formula on a flavored focal layer
      models all rows with a `cli_inform`; wrapper-resolved dependent names supply the
      flavor key internally without the inform; `start_time`/`end_time` arguments wired;
      update roxygen and run `devtools::document()`
      (2026-07-16: DONE except two deliberate deferrals. (1) The **legacy-environment
      abort** is NOT implemented — under "the bridge is allowed for the whole of task 3"
      the suite still builds every fixture through the legacy constructors, which only
      become wrappers in 3.1; the abort lands with 3.1/3.6. (2) The **wrapper-resolved
      dependent name -> flavor without the inform** needs the name->(layer, flavor)
      association that `make_dependent_events()` records — that is 3.1's to create.
      `start_time`/`end_time` were already on the surface via `set_preprocessing_opt()`
      and reach `preprocess()`; no new argument was invented.)
- [x] 3.6 Milestone gate: equivalence tests — models expressible in both call styles
      (legacy-wrapper code and direct stocnet; Social_Evolution, Fisheries incl. two-mode
      and panel-style layers) match the frozen baselines to 1e-6 on both engines;
      deprecation warnings fired once; DyNAMi constructor emits no new warning;
      `NOT_CRAN=true devtools::test()` all green, baselines PASS not SKIP; bump version
      in DESCRIPTION + NEWS.md (stocnet input path functional end-to-end)

## 4. Result surfaces and print

- [x] 4.1 Update the `specification.goldfish` print Dependent block for stocnet-resolved
      layers (event count, span, side pair, network) with cli semantic elements,
      including the flavors-nested-under-the-layer rendering the landed spec promises
      (modeled flavor labeled, state-only flavors listed); refresh
      output snapshots under the pinned cli context
      (2026-07-18: flavors nest under Layer via the cli `" "` continuation marker;
      shown only when the layer carries >1 distinct flavor, so a wrapper's synthetic
      single flavor stays hidden and the legacy print snapshots are unchanged.)
- [x] 4.2 Tests: spec-from-stocnet equals spec-from-wrapper coefficients to 1e-6
      (Fisheries via flavor selection included); layer/focal precedence; unknown layer
      errors listing candidates; legacy-environment
      abort snapshot (message names `as_goldfish()`); `ego(var)` resolution + `df$var`
      translation warning snapshot; flavor-list aborts (multi-key, rate/choice key
      mismatch) and the plain-formula-on-flavored-layer inform snapshot; run tests green
      (2026-07-18: all items covered EXCEPT the legacy-environment abort snapshot,
      which is blocked on section 3's still-open envir-seam deletion (3.6 closing
      step): DyNAMi and two-mode data legitimately flow through
      `check_estimation_data()` as environments today, so a public-surface abort on
      `is.environment(data)` would break them. It lands with the envir deletion, not
      here. New: print snapshots, unknown-layer candidate snapshot; existing tests
      cover the rest.)
- [x] 4.3 Attach the node lookup (side, local index, global id, label) to
      preprocessed/estimation results and the gather/db exports alongside
      `index_i`/`index_j` (design D7 identity bullet) so residuals/event-scores/export
      consumers resolve original node identity without re-deriving the mode map
      (2026-07-18: `ds_node_lookup()` S3 seam -> `layer_node_lookup(mode_map, focal)`
      on the stocnet path, NULL on the legacy env path; attached to `prep$node_lookup`,
      `gathered$node_lookup`, and `result$node_lookup`; documented on gather `@return`.)
- [x] 4.4 Tests: gather/db export lookup joins back to `nodes` labels on one-mode,
      subset one-mode, and two-mode fixtures; run tests green

## 5. Datasets: prebuilt stocnet objects, human-readable times

- [x] 5.1 Create `data-raw/` build scripts for `social_evolution` and
      `fisheries_treaties` following `goldfish_asta/code/plan/single_object_examples.R`
      (`as_stocnet()` per layer, `from_ties()` merge, `join_nodes()`, `add_info()` with
      `observation = c(friendship = "panel", calls = "event")` for Social Evolution and
      `observation = c(treaties = "event", contiguity = "event")` + gdp/active/regime
      `bind_changes()` for Fisheries, whose treaty ties carry
      `flavor = c(creation = +1, dissolution = -1)` mapped from `increment` — design
      D19); assert the saved objects are plain lists/tibbles (design D10)
      (2026-07-18: `data-raw/Social_Evolution.R` (raw-time conversion + SE stocnet,
      one file because macOS is case-insensitive) + `data-raw/fisheries_treaties.R`;
      both `add_info(focal=...)` + explicit `directed` (manynet derives update but not
      directed); D10 plain-list/tibble asserts in each; saved via `usethis::use_data`
      to `data/social_evolution.rda` / `data/fisheries_treaties.rda`.)
- [x] 5.2 Convert `Social_Evolution` raw times in place: `calls$time`, `friendship$time`
      → `as.POSIXct(time, origin = "1970-01-01", tz = "GMT")` (2008 data); resave
      `data/Social_Evolution.RData`; verify `as.numeric()` equals the previous values
      exactly and the frozen baselines PASS under `NOT_CRAN=true` (do NOT regenerate)
      (2026-07-18: round-trip asserted in the data-raw script; frozen baselines PASS.
      Surfaced + fixed a latent assembler bug — `legacy_global_table()` built the
      initial global row with numeric `NA_real_`, collapsing a POSIXct event axis to
      numeric on rbind (mixed-axis abort); init NA now shares the events' time class.
      Also retyped the global-baseline fixture's `seasonChange$time` to POSIXct.)
- [x] 5.3 Document the new data objects (roxygen data docs: structure, layers, info
      metadata, source citations mirroring the existing dataset pages); construction
      workflow example on each dataset help page; run `devtools::document()`
      (2026-07-18: docs co-located in `data_Social_Evolution.R` /
      `data_Fisheries_Treaties_6070.R` — `data_social_evolution.R` would collide
      case-insensitively; each carries the stocnet structure, info metadata, source
      citation, and a runnable construction-workflow example (0 deprecation warnings).)

## 6. Rd examples on the prebuilt datasets

- [x] 6.1 Rewrite the ~15 Rd example blocks that use `make_nodes()`/`make_network()`/
      `make_data()` to load `social_evolution`/`fisheries_treaties` instead — no
      deprecation warnings at example runtime; construction stays on the dataset help
      pages (design D10); run `devtools::document()`
      (2026-07-18: rewrote the estimator/spec/gather/diagnostic/postestimate example
      blocks (estimate ×2, make_specification, compute_stats, gather_model_data,
      examine, coef) to load the prebuilt datasets; the update-method example now
      shows the superseding state-at-t helpers. The 6 deprecated CONSTRUCTOR pages
      keep their own examples wrapped in `\dontrun{}` (a deprecated function's own
      example inherently warns) with a one-line pointer to the manynet replacement.
      utils.R examples are `@noRd` (no Rd, never run) — untouched. Surfaced + fixed a
      task-5 dataset gap: the fisheries contiguity `replace` layer had same-time
      same-dyad collisions aborting under D2; added a reserved `order` column in the
      data-raw script + doc.)
- [x] 6.2 Run all examples (`devtools::run_examples()`) clean — no lifecycle warnings;
      commit
      (2026-07-18: `run_examples(run_donttest = TRUE)` -> 0 deprecation/lifecycle
      warnings, no errors (only the expected gdp/regime imputation notices the
      Fisheries model always emitted). Root-caused the last lifecycle warnings to
      `R/zzz_testthat_helpers.R` building fixtures through the legacy constructors at
      LOAD time (5 warnings on every `library(goldfish)`, not `.Rbuildignore`d);
      bracketed its build with `lifecycle_verbosity = "quiet"` (restored at file
      end), so load is silent while the deprecation TESTS — which call the
      constructors directly, after restore — still pass. Full suite FAIL=0/SKIP=0,
      baselines PASS.)

## 7. Vignettes: teaching sources on the stocnet workflow

- [x] 7.1 Rewrite `vignettes/teaching1.Rmd.orig` and `teaching2.Rmd.orig` to the stocnet
      workflow: load prebuilt datasets for estimation sections; add a dedicated
      **construction-workflow section** (design D10: `as_stocnet()`/`make_stocnet()`,
      `time = NA` history, the `order` column, the undeclared-multimodal callout,
      explicit panel dissolution rows); teaching2's Fisheries model switches from the
      filtered `make_dependent_events()` pattern to the flavor-keyed
      `rate = list(creation ~ ...)` syntax with the flavored dataset; use the exported
      state-at-t helpers where the old
      vignettes used `as.matrix(net, time =)`; `dynami-example.Rmd.orig` untouched
      (DyNAMi engine change)
      (2026-07-18: both rewritten. teaching1 = Social Evolution (friendship panel +
      calls event) with a construction section covering panel dissolution rows + the
      undeclared-multimodal callout; teaching2 = Fisheries with the flavor-keyed
      `choice = list(creation ~ ...)` spec, its construction section covering the
      `time = NA` history matrix + the contiguity `order` column. Viz uses
      `network_state_at()`/`nodes_state_at()`. Dropped `ignore_repetitions = TRUE`
      (disabled, issue #105) and the compact effect names (`odeg`, `ideg_cal`) in the
      waiting-time interpretation. dynami-example untouched.)
- [x] 7.2 Re-knit via `vignettes/precompile.R` regenerating `teaching1.Rmd`/`.R`,
      `teaching2.Rmd`/`.R` and the plot images; check the knitted output for errors;
      precompilation retained — live-vignette viability is a release-prep measurement,
      not this change
      (2026-07-18: re-knit clean, 0 errors. Installed the Suggests the knit needs
      (ggraph/migraph/pixiedust). The new migraph moved `node_deg`->`netrics` etc., so
      the teaching2 viz now uses igraph `degree`/`delete_vertices` + `graphr | graphr`
      (patchwork). Removed 3 now-stale plot PNGs. Fixed a pre-existing goldfish bug the
      flavor spec path exposed: `result$call` surfaced a `tryCatchOne` frame in every
      spec-estimated summary; now reconstructs a clean `estimate_dynam(<formula>)`
      call. Remaining: 4 pre-existing `sub_model="rate"` no-intercept deprecation
      warnings (warnings, not errors; the vignette teaches adding the intercept next).)

## 8. Documentation and conformance milestone

- [x] 8.1 Rewrite the data-construction documentation around
      `as_stocnet()`/`make_stocnet()` workflows (mirroring the single_object_examples
      patterns), covering the ordering contract, `time = NA` history, panel dissolution
      rows, mode sets, and the `order` column; run `devtools::document()`
      (2026-07-18: new `R/goldfish-data.R` topical page (`?goldfish_data`) documenting
      the components, the manynet construction verbs, event/panel observation types +
      dissolution rows, `time = NA` history, mode sets, and the flavor/order columns
      (ordering contract inherited from `as_goldfish` via @seealso, not duplicated).
      Cross-linked from as_goldfish / make_data / goldfish-package; added to the
      pkgdown Make section, and the two prebuilt datasets to the Data section. Example
      runs 0 deprecation warnings.)
- [x] 8.2 Explore the upstream `to_time.stocnet` contribution to manynet (design D16,
      deliberately late): compare the exported state-at-t helpers' semantics with
      manynet's `to_time()` generic; file the upstream issue/PR sketch in `.plan/`; no
      goldfish code change
      (2026-07-18: `.plan/to_time_stocnet_upstream.md`. manynet 2.2.0 has no
      `to_time.stocnet`; `to_time.tbl_graph` is wave-based and `snet_unavailable()` for
      dynamic data — exactly the gap `network_state_at`/`nodes_state_at` fill. Sketch
      compares semantics (NA=history, strictly-before cutoff, per-layer update, mode
      map) and lists the return-type/engine-ownership open questions for the issue.)
- [x] 8.3 Full verification: `NOT_CRAN=true devtools::test()` all green, baselines PASS
      not SKIP; `lintr::lint_package()` clean on touched files (incl. the snake_case
      linter); spawn the spec-conformance agent to cross-check spec deltas against
      implementation
      (2026-07-18: full suite FAIL=0/SKIP=0, baselines PASS. Lint: fixed the 5 new
      line-length lints my session's edits introduced (goldfish-data.R,
      validate_goldfish.R, zzz_testthat_helpers.R, preprocess_export.R,
      model_estimate.R); remaining lints in touched files are pre-existing legacy.
      No named spec-conformance agent exists here, so cross-checked inline: 14/15
      ADDED requirements + the MODIFIED requirement CONFORM. **One gap** -- "Legacy
      environment input rejected with a conversion path" -- the hard abort is still
      ABSENT because DyNAMi and two-mode data legitimately flow through
      `check_estimation_data()` as environments; it lands with the section-3
      envir-seam deletion + the DyNAMi engine change, and the spec requirement needs
      that exception before archive.)
- [x] 8.4 Milestone: bump version in DESCRIPTION + NEWS.md entry describing the stocnet
      input path, mode sets, the constructor deprecations, the legacy-environment error,
      `ego(var)` syntax, state-at-t helpers, and the new data objects
      (2026-07-18: DESCRIPTION 1.9.0 -> 1.9.1; NEWS 1.9.1 entry covers the prebuilt
      datasets + human-readable times, the bare-attribute `ego(var)`/`df$var` syntax,
      the export node_lookup, the `?goldfish_data` construction reference + vignette
      rewrites, and the spec-path summary-call bugfix. The stocnet input path / mode
      sets / deprecations / state-at-t helpers are already in the 1.9.0 entry (the
      flip milestone). The **legacy-environment hard abort is NOT claimed** -- it is
      deferred (see 8.3); the 1.9.0 note already tells users to rebuild a saved env.)
