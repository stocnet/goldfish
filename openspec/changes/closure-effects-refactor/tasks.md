**Detector first, one member per commit, the frozen 1e-6 baselines and the
parity goldens PASS not SKIP at every commit** (`NOT_CRAN=true`,
not-cran-test skill). The family is on the path every baseline runs, so the
baselines are a real detector for the members they cover; the oracle tests
of group 0 are the detector for the rest.

## 0. Gates, audit and oracles (design D2, D10)

- [ ] 0.1 Tree clean on `feature_simulation`; full `NOT_CRAN=true` suite
      green with the six baseline files PASS not SKIP; record the counts in
      `progress.md`
- [ ] 0.2 Before timing on CollegeMsg subsets (1k/5k/10k, median of three
      warm runs, `bench::bench_memory()` for allocation) of
      `choice ~ inertia + trans`, `choice ~ inertia + trans(history =
      "sequential")`, `choice ~ inertia + four` on the two-mode fixture and
      `rate ~ 1 + node_trans`, extending
      `.plan/sp/mask_readers_timing_2026-09-27.R`; numbers in `progress.md`
- [ ] 0.3 Aliasing audit as a test (r-lib:testing-r-packages): every closure
      member's init on an empty, sparse, dense and two-mode network returns a
      `cache$stat` address-distinct from the input and from `stat`; the
      `default` init likewise; fails today on the `default` init (the
      detector), passes once 1.2 lands
- [ ] 0.4 Brute-force oracles in a test helper: from-scratch counts of every
      shape (two-path, cycle, in-star, out-star, mixed, three-path, node
      roles, undirected triangle and two-path) on the current state, plus
      the weighted sums for `prod`/`min`/`sum`/`max`; a test per member
      asserting cache equality after every event of a seeded random directed
      fixture and a seeded undirected fixture (`withr::local_seed()`). All
      pass on today's code for the existing members; `degree` on the
      undirected fixture is recorded as failing (twice the degree) and
      skipped until 4.4
- [ ] 0.5 Consecutive-under-REM detector: the consecutive-history walk
      fixture preprocessed as an REM with `trans(history = "consecutive")`
      expects one closed two-path per event after the first; fails today
      (`event_order = 0` in the wrapper), green at 3.4
- [ ] 0.6 Verification: `NOT_CRAN=true` suite green, baselines PASS not SKIP

## 1. The in-place sink (design D2)

- [ ] 1.1 `apply_closure_delta(cache, entries, increments, transformer_fn)`
      in `R/utils_effects_closure.R`: linear entries at the buffer's kind,
      `pmax(0, ·)` clamp kept, write through `write_entries()`, `changes`
      rows for the touched entries with the transformer applied; every
      current caller of `apply_two_path_update()` routed to it (same rows,
      same order). Test: the two `closure-effects-dry` helper scenarios plus
      the address-identity scenario on a 300-actor layer; `git diff` of the
      parity goldens empty
- [ ] 1.2 Ownership: `build_walk_engine()` drops `ctx$stat_cache` after
      extracting `engine$stat_cache`; `initialize_cache_stat()` /
      `init_DyNAM_choice.default()` return distinct `cache` and `stat`
      objects; `four`'s edgeless branch re-checked. Test: 0.3 passes; two
      `simulate()` replicates hold address-distinct caches
- [ ] 1.3 `consecutive` state moves from the `last_update` attribute to the
      cache fields `last_sender`, `last_receiver`, `last_event_seq`
      (length-one vectors); `merged_covariate_step()` stops copying the
      attribute. Test: `test-consecutive_history.R` unchanged and green; the
      buffer carries no attribute
- [ ] 1.4 After timing (same cells as 0.2); `progress.md` records before and
      after, and the allocation per 1000 events
- [ ] 1.5 Verification: `NOT_CRAN=true` suite green, baselines PASS not SKIP,
      parity goldens byte-identical

## 2. Readers and the factory (design D1, D3, D4)

- [ ] 2.1 Neighborhood readers `neigh_in()`, `neigh_out()` and the
      undirected reader, each taking an exclusion set; unit tests against
      `which()` on the dense matrix, one-mode and two-mode
- [ ] 2.2 `make_closure_update()` in `R/utils_effects_closure.R` per design
      D3, with the shape table of D1 as data (positions → readers → cells),
      the position filters for `pooled` / `sequential` / `consecutive`, the
      cell orientation (identity, transpose, role column) and the exclusion
      set resolved from `allow_reflexive` (pinned `FALSE` until
      `allow-reflexive-user` lands, then `excludes_self_dyads()` at
      `prepare_recipe_context()`). Unit tests: a factory-built `trans` update
      equals `update_DyNAM_choice_trans()` row for row on the oracle fixtures
      under every `history`
- [ ] 2.3 Verification: `NOT_CRAN=true` suite green, baselines PASS not SKIP

## 3. Dyad members on the factory (design D1, D6)

- [ ] 3.1 `trans` and `cycle` (DyNAM choice, REM, coordination wrappers)
      built by the factory; the three `compute_update_two_path_*` functions
      retired. Test: parity goldens byte-identical; 0.4 oracles green;
      `test-functions_effects_DyNAM_choice_trans.R` / `_cycle.R` green
- [ ] 3.2 `common_sender`, `common_receiver` on the factory (in-star,
      out-star rows). Test: unit files green, 0.4 oracles green
- [ ] 3.3 The four `mixed_*` members on the factory (`legs = c(1, 2)`,
      `history` pooled/sequential, `consecutive` refused as today). Test:
      parity fixtures (`mixed_trans`, `mixed_cycle`) byte-identical, unit
      files green, `closure-effects-history-mixed` scenarios green
- [ ] 3.4 REM and coordination wrappers pass the walk's `event_order`
      through (the `event_order = 0` defect); 0.5 goes green. NEWS.d
      fragment `closure-effects-refactor--rem-consecutive.md` (`Fixed`)
- [ ] 3.5 Undirected one-way evaluation for dyad members: the factory built
      for an undirected object runs on the canonical orientation and emits
      both orientations' rows in today's order; the mirrored call returns no
      changes. Test: the undirected probe fixture's `trans` stream is
      byte-identical to today's; the second evaluation is a no-op
- [ ] 3.6 Verification: `NOT_CRAN=true` suite green, baselines PASS not SKIP,
      parity goldens byte-identical

## 4. four, the node family and the undirected members (design D1, D5, D8)

- [ ] 4.1 `four` on the factory's three-path rows, one-mode and two-mode,
      the product-based update kept in the test helper as the oracle until
      every fixture matches, then deleted. Test: `test-preprocess_parity.R`
      and `test-twomode_baselines.R` byte-identical /
      PASS; `test-functions_effects_DyNAM_choice_four.R` green; the
      two-mode vignette model (`four(support)`) preprocesses to the same
      statistics
- [ ] 4.2 `node_trans(role = c("source", "broker", "sink"))`: the reduction
      columns of design D5, `role` resolved at construction through
      `resolve_effect_args()` (out-of-set aborts with a cli error naming
      effect, argument and allowed values, snapshot). Test: `"source"`
      byte-identical to today on the parity fixture; broker and sink equal
      the 0.4 oracles; the three roles sum to three times the triangle count
- [ ] 4.3 `triangle` renamed `node_triangle` (rate; init, update, validity
      row, `NAMESPACE`, docs; no lifecycle, the effect is unreleased) in
      delta form with the common-neighbor rule and the one-way guard; aborts
      at construction on a directed network. Test: the three-tie scenario;
      0.4 oracle on the undirected fixture; the directed refusal snapshot
- [ ] 4.4 `degree` fix: init reads the stored symmetric matrix without adding
      its transpose, the update takes the one-way guard. Test: one tie gives
      degree 1; 0.4's skipped case un-skipped and green. NEWS.d fragment
      `closure-effects-refactor--degree-undirected.md` (`Fixed`)
- [ ] 4.5 New dyad `triangle` (DyNAM choice, REM, coordination): the
      two-path shape with the undirected readers, aborting at construction
      on a directed network; validity rows added. Test: equals `trans` on
      the undirected probe fixture; the refusal snapshot;
      `test-effect_validity.R` extended
- [ ] 4.6 `devtools::document()` (new S3 inits, the effects reference in
      `R/formula_parser.R` gains `node_triangle`, `triangle`, `role`);
      NEWS.d fragment `closure-effects-refactor--triangle-names.md`
      (`Renamed`, `Added`)
- [ ] 4.7 Verification: `NOT_CRAN=true` suite green, baselines PASS not SKIP,
      parity goldens byte-identical

## 5. weighted and path_fn (design D7)

- [ ] 5.1 `weighted` and `path_fn` accepted by every closure member (parser
      signature, `GetDetailPrint()` flags, compact term strings); `path_fn`
      validated as a function at construction (cli abort, snapshot);
      `weighted = FALSE` ignores `path_fn`. Test: `trans(net, weighted =
      FALSE, path_fn = min)` byte-identical to `trans(net)`
- [ ] 5.2 The factory's per-embedding contribution: presence of every leg
      required, `path_fn` of the leg weights, new minus old contribution on
      a leg change. Test: the brokers-2-and-3 scenario (13/3/11/8, then
      16/4/12/8); the lone-leg scenario; 0.4's weighted oracles on every
      member and each of `prod`, `min`, `sum`, `max`
- [ ] 5.3 Roxygen for the two arguments with the existence rule and the
      ergm equivalences (geometric = `prod` + `sqrt`); `devtools::document()`;
      NEWS.d fragment `closure-effects-refactor--weighted-paths.md`
      (`Added`, closes #104, #106)
- [ ] 5.4 Verification: `NOT_CRAN=true` suite green, baselines PASS not SKIP
      (no baseline covers a weighted closure; none may move)

## 6. Measurements (design D3, D9)

- [ ] 6.1 After timing of the whole change on the 0.2 cells; table
      before / after sink / after factory in `progress.md`
- [ ] 6.2 If/else versus factory: the `trans` update written with today's
      `history` branching against the factory-built one, same cells,
      `bench::mark()` time and allocation; the list cache against an
      environment-keyed buffer (design D2's alternative) on the same cells.
      Numbers and the maintenance verdict in `progress.md` and in the design
      (a dated note under D3 and D2)
- [ ] 6.3 Column scan versus adjacency list for one neighborhood read at
      n = 1899, recorded under design D9 as the input for the C++ port
      decision in `effect-term-registry`

## 7. Close

- [ ] 7.1 `openspec validate closure-effects-refactor --strict` clean;
      `bash .plan/opsx-spec-placement-check.sh closure-effects-refactor`
      exit 0; `lintr` on the touched files after `air format`
- [ ] 7.2 Comment sweep of the touched files (no OpenSpec references; the
      rationale inline); the `closure-effects-dry` capability's helper name
      in the living spec is reconciled at the fold
- [ ] 7.3 Notes to the neighbors, claiming each first (ADR-0036):
      `effect-term-registry` (D14/D15 implemented for this family;
      `combiner_fn` retired for `path_fn`; the undirected walk contract and
      per-effect undirected validity handed over; R2 gains the cache audit;
      R4's port target), `recency-effects` (the readers and the cache list it
      plugs into), `allow-reflexive-user` (task 2.2 consumer)
- [ ] 7.4 Vault: ADR-0102 `spec:` and open questions current; session note
      closed; dashboard fragment in `_dashboard-inbox/`
- [ ] 7.5 Final verification: full `NOT_CRAN=true` suite green, baselines
      PASS not SKIP; `status: landed (feature_simulation, awaiting fold)` in
      `proposal.md`; no `NEWS.md` or Version edit (ADR-0040)
