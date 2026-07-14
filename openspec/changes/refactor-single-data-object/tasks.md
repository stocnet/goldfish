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

- [ ] 1.1 Implement the internal stocnet validator by **generalizing the existing
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
- [ ] 1.2 Implement the stamp and exported `as_goldfish(x, ...)`: for a stocnet input,
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
- [ ] 1.3 Move manynet from Suggests to **Imports with `manynet (>= 2.1.0)`** in
      DESCRIPTION (usage rule: `@importFrom` for frequent use, `manynet::` otherwise,
      never `:::`); build a hand-made stocnet fixture (plain list + data.frames, no
      manynet call) plus a small legacy-environment fixture for the conversion tests
- [ ] 1.4 Unit tests (testthat 3e): each validation abort with cli snapshots under a pinned
      cli context (incl. unnamed/short `update` vector, duplicate labels, non-syntactic
      layer name, partially overlapping mode sets); post-stamp mutation caught at
      re-validation (edit a stamped object into invalidity, expect the estimation-time
      abort); hand-built fixture path works; legacy-environment conversion round-trips
      (converted object validates and matches the fixture stocnet); run
      `NOT_CRAN=true devtools::test()` green

## 2. Conversion: mode map, ordering, streams, state materializer

- [ ] 2.1 Implement the mode map (global node id + label ⇄ (side, local id)) and per-layer
      remapping from `sender`/`receiver` mode **sets**: identical sets → one-mode over the
      subset; disjoint sets → two-mode local indices; partial overlap aborts; undeclared →
      one-mode over all nodes (design D7). The map is a reusable structure that later
      attaches to results/exports (task 4.4)
- [ ] 2.2 Implement deterministic event ordering: sort key (time, dependent-first,
      component order, layer, from/to), reserved integer `order` column as final tie-break,
      abort on same-target same-time replaces without `order` (design D2)
- [ ] 2.3 Implement component splitting into the **per-layer / per-variable event streams
      the existing `fetch_plan` multi-stream walk consumes** (design D15 — no monolithic
      stacked copy): `time = NA` ties → initial matrices; focal-layer rows → dependent
      events, **filtered by the modeled flavor when the specification keys one** (design
      D19: non-matching/NA-flavor focal rows become state-only updates; every focal row
      still updates state per `info$update`); other layers → exogenous streams;
      `changes` → attribute events with
      `var == "active"` routed to per-side composition (`active_mode1`/`active_mode2`);
      `global` → global events (design D3/D7)
- [ ] 2.4 Promote the `methods_update.R` engine into the internal **initial-state
      materializer** (design D16): given component streams and `[start_time, t)`, produce
      state matrices/attribute vectors in one vectorized pass (dedup-last replace,
      aggregated increments); replace the current per-event `startTime` fold in
      preprocessing with it, covering derived promises (window, `ignore_rep`); implement
      the `start_time`/`end_time` observation window on top (focal-span default, design
      D4); document (not build) the chunked-parallel preprocessing seam
- [ ] 2.5 Export the state-at-t helpers on the stocnet/stamped object (design D16):
      network state of a layer and nodes' attribute values at time `t`, honoring
      `time = NA` history + per-layer update semantics, sharing the materializer core;
      snake_case names; roxygen + `devtools::document()`
- [ ] 2.6 Unit tests: mode-set remap fixtures (two-mode, identical-set subset,
      partial-overlap abort, undeclared multimodal, side-impurity abort), ordering
      determinism under `arrange()`, `order`-column tie-break + ambiguity abort, history
      initialization, window defaulting, materializer equals the legacy per-event
      `startTime` fold on Social_Evolution/Fisheries state (exact equality), state-at-t
      helpers vs known matrices; run tests green

## 3. The flip (one milestone, design D14): wrappers + builders + surface

*Tasks 3.1–3.6 land as one baseline-gated commit series; the suite stays green at each
commit but intermediate commits may carry both paths only within this series — no bridge
helpers survive it.*

- [ ] 3.1 Reimplement `make_nodes()`, `make_network()`, `link_events()`,
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
- [ ] 3.2 Replace the builder `envir` seam with the data object (design D14):
      `build_object_keys()`, `build_derivations()`, `compile_support_constraint()`, and
      the `fetch_plan` resolve layer/attribute names against
      `data$ties`/`data$nodes`/`data$global`; **delete** the `envir` parameters and
      `get(name, prepEnvir)` resolution — no bridge branch; recipe loops/writers/
      estimation untouched
- [ ] 3.3 Route effect AND `support_constraint` name resolution through one resolver
      against stocnet layer/variable names (candidates listed on error); implement
      attribute references per design D17: bare `ego(var)` from `nodes`, global names
      from `global`, nodes-vs-global ambiguity abort, and the `df$var` prefix
      deprecation-translation (drop prefix, resolve bare name, warn once)
- [ ] 3.4 Panel layers: change-list updates at wave times emitting right-censored updates;
      abort on panel focal and on `window` over a panel-layer effect; document the
      reserved per-layer panel-semantics flag (DyNES seam) without implementing it
      (design D5)
- [ ] 3.5 Accept stocnet (raw or stamped) in `make_specification()`/`estimate_dynam()`/
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
- [ ] 3.6 Milestone gate: equivalence tests — models expressible in both call styles
      (legacy-wrapper code and direct stocnet; Social_Evolution, Fisheries incl. two-mode
      and panel-style layers) match the frozen baselines to 1e-6 on both engines;
      deprecation warnings fired once; DyNAMi constructor emits no new warning;
      `NOT_CRAN=true devtools::test()` all green, baselines PASS not SKIP; bump version
      in DESCRIPTION + NEWS.md (stocnet input path functional end-to-end)

## 4. Result surfaces and print

- [ ] 4.1 Update the `specification.goldfish` print Dependent block for stocnet-resolved
      layers (event count, span, side pair, network) with cli semantic elements,
      including the flavors-nested-under-the-layer rendering the landed spec promises
      (modeled flavor labeled, state-only flavors listed); refresh
      output snapshots under the pinned cli context
- [ ] 4.2 Tests: spec-from-stocnet equals spec-from-wrapper coefficients to 1e-6
      (Fisheries via flavor selection included); layer/focal precedence; unknown layer
      errors listing candidates; legacy-environment
      abort snapshot (message names `as_goldfish()`); `ego(var)` resolution + `df$var`
      translation warning snapshot; flavor-list aborts (multi-key, rate/choice key
      mismatch) and the plain-formula-on-flavored-layer inform snapshot; run tests green
- [ ] 4.3 Attach the node lookup (side, local index, global id, label) to
      preprocessed/estimation results and the gather/db exports alongside
      `index_i`/`index_j` (design D7 identity bullet) so residuals/event-scores/export
      consumers resolve original node identity without re-deriving the mode map
- [ ] 4.4 Tests: gather/db export lookup joins back to `nodes` labels on one-mode,
      subset one-mode, and two-mode fixtures; run tests green

## 5. Datasets: prebuilt stocnet objects, human-readable times

- [ ] 5.1 Create `data-raw/` build scripts for `social_evolution` and
      `fisheries_treaties` following `goldfish_asta/code/plan/single_object_examples.R`
      (`as_stocnet()` per layer, `from_ties()` merge, `join_nodes()`, `add_info()` with
      `observation = c(friendship = "panel", calls = "event")` for Social Evolution and
      `observation = c(treaties = "event", contiguity = "event")` + gdp/active/regime
      `bind_changes()` for Fisheries, whose treaty ties carry
      `flavor = c(creation = +1, dissolution = -1)` mapped from `increment` — design
      D19); assert the saved objects are plain lists/tibbles (design D10)
- [ ] 5.2 Convert `Social_Evolution` raw times in place: `calls$time`, `friendship$time`
      → `as.POSIXct(time, origin = "1970-01-01", tz = "GMT")` (2008 data); resave
      `data/Social_Evolution.RData`; verify `as.numeric()` equals the previous values
      exactly and the frozen baselines PASS under `NOT_CRAN=true` (do NOT regenerate)
- [ ] 5.3 Document the new data objects (roxygen data docs: structure, layers, info
      metadata, source citations mirroring the existing dataset pages); construction
      workflow example on each dataset help page; run `devtools::document()`

## 6. Rd examples on the prebuilt datasets

- [ ] 6.1 Rewrite the ~15 Rd example blocks that use `make_nodes()`/`make_network()`/
      `make_data()` to load `social_evolution`/`fisheries_treaties` instead — no
      deprecation warnings at example runtime; construction stays on the dataset help
      pages (design D10); run `devtools::document()`
- [ ] 6.2 Run all examples (`devtools::run_examples()`) clean — no lifecycle warnings;
      commit

## 7. Vignettes: teaching sources on the stocnet workflow

- [ ] 7.1 Rewrite `vignettes/teaching1.Rmd.orig` and `teaching2.Rmd.orig` to the stocnet
      workflow: load prebuilt datasets for estimation sections; add a dedicated
      **construction-workflow section** (design D10: `as_stocnet()`/`make_stocnet()`,
      `time = NA` history, the `order` column, the undeclared-multimodal callout,
      explicit panel dissolution rows); teaching2's Fisheries model switches from the
      filtered `make_dependent_events()` pattern to the flavor-keyed
      `rate = list(creation ~ ...)` syntax with the flavored dataset; use the exported
      state-at-t helpers where the old
      vignettes used `as.matrix(net, time =)`; `dynami-example.Rmd.orig` untouched
      (DyNAMi engine change)
- [ ] 7.2 Re-knit via `vignettes/precompile.R` regenerating `teaching1.Rmd`/`.R`,
      `teaching2.Rmd`/`.R` and the plot images; check the knitted output for errors;
      precompilation retained — live-vignette viability is a release-prep measurement,
      not this change

## 8. Documentation and conformance milestone

- [ ] 8.1 Rewrite the data-construction documentation around
      `as_stocnet()`/`make_stocnet()` workflows (mirroring the single_object_examples
      patterns), covering the ordering contract, `time = NA` history, panel dissolution
      rows, mode sets, and the `order` column; run `devtools::document()`
- [ ] 8.2 Explore the upstream `to_time.stocnet` contribution to manynet (design D16,
      deliberately late): compare the exported state-at-t helpers' semantics with
      manynet's `to_time()` generic; file the upstream issue/PR sketch in `.plan/`; no
      goldfish code change
- [ ] 8.3 Full verification: `NOT_CRAN=true devtools::test()` all green, baselines PASS
      not SKIP; `lintr::lint_package()` clean on touched files (incl. the snake_case
      linter); spawn the spec-conformance agent to cross-check spec deltas against
      implementation
- [ ] 8.4 Milestone: bump version in DESCRIPTION + NEWS.md entry describing the stocnet
      input path, mode sets, the constructor deprecations, the legacy-environment error,
      `ego(var)` syntax, state-at-t helpers, and the new data objects
