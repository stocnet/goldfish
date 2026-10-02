## 0. Gates, references and guards (D5)

- [ ] 0.1 Confirm `likelihood-site-cleanup-and-cost` is landed on
      `feature_simulation` (its per-event pins exist and its engine
      interface is the one this change edits) and the tree is clean;
      `NOT_CRAN=true` suite green (not-cran-test; six baseline files and the
      C++ goldens PASS, not SKIP). Record counts in `progress.md`.
- [ ] 0.2 Capture the dense `initial_stats` and the `stat_mat_broadcast` /
      `stat_mat_update` streams of every reference family
      (`helper-constraint-atoms.R`), every parity golden
      (`helper-parity-fixtures.R`) and the interaction fixtures
      (`test-interaction_operand_kind.R`) into a `dense_init_refs.rds`
      fixture with a generator script; a test asserts `identical()` on the
      unfixed tree (r-lib:testing-r-packages).
- [ ] 0.3 Cost guards, recorded failing first: (a) object size
      (`lobstr::obj_size()`) of a one-mode choice model with `alter(x) +
      indeg(net) + inertia(net)` minus the same with `inertia(net)` alone on
      a 1500-actor generated fixture is under 1 MB (today about 36 MB, two
      grids); (b) `bench::bench_memory()` of one `update_REM_choice_alter()`
      call on that fixture does not grow with `n` (today an `(n - 1) x 3`
      matrix); (c) a count of dense arrays built per compiled-backend fit
      equals one. Commit skipped where it must, named by state.
- [ ] 0.4 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS not SKIP.

## 1. Accessor and storage, inits still dense (D1, D4)

- [ ] 1.1 `R/broadcast_kind.R`: `initial_stats(prep)` (dense engine-native
      form from `stat_init` / `stat_init_kind` through `project_value()`,
      the one-mode diagonal zeroed once; a dense `initial_stats` field
      passed through), `prep_dims(prep)`, `prep_n_effects(prep)`. Unit
      tests: every kind projects to the expected slice; the stale field
      passes through; dimensions never expand (allocation of `prep_dims()`
      is constant).
- [ ] 1.2 `R/preprocess_joint.R` `build_walk_engine()`: the engine holds
      `stat_init` slices at kind (a dense init reduced with `reduce_value()`
      in this group, so inits stay as they are); the pre-window folds write
      through `write_entries()` into the slice at kind; the interaction
      operand seeds read operands at kind and seed the product at the union
      kind; `walk_seed_live_stats()` expands through the accessor's core.
      `R/preprocess_writers.R`: `assemble_default_output()` writes
      `stat_init` and `stat_init_kind`, no `initial_stats`; the gather render
      builds `stat_mat_init` from the slices. Test: 0.2 references
      `identical()` through the accessor; `test-interaction_operand_kind.R`
      PASS.
- [ ] 1.3 Consumers: `make_engine_evaluator()` builds `stat_mat_init` from
      the slices (one projection per slice into the flat matrix, no 3D
      intermediate); `estimate_int()` expands per evaluation;
      `project_initial_stats()` over the slice list; the `preprocessed =`
      merge in `R/model_estimate.R` copies slices and kinds; the DyNAM-i
      bridge stores 3D slices with kinds of zero; `methods_residuals.R`,
      `utils.R`, `methods_display.R` read `prep_dims()`. Test: baselines and
      goldens PASS; backend-parity PASS; the DyNAM-i baselines PASS.
- [ ] 1.4 Move the 26 test files from `prep$initial_stats` to the accessor
      or `prep_dims()` (the two DyNAM-i preprocessing files keep their dense
      assertions through the accessor); a grep for `\$initial_stats` over
      `R/` and `tests/` finds only the accessor's stale-field branch. Test:
      suite green.
- [ ] 1.5 Stale-object test: a preprocessed object saved before 1.2
      (`_fixtures/stale_prep/`) with a dense `initial_stats` estimates on
      every backend equal to a fresh object within 1e-10.
- [ ] 1.6 Measure 0.3 (a) and (c) after 1.3 against the design's prediction
      (0.3 (a) still red: inits are dense, so the slices are reduced grids);
      numbers in `progress.md`.
- [ ] 1.7 Verification: `NOT_CRAN=true` suite green; baselines, goldens and
      the 0.2 references PASS; `devtools::document()` for the new accessor.

## 2. Inits at kind (D2)

- [ ] 2.1 Conformance test, written first: for every effect and every family
      it supports, on a small fixture, the init's `stat` has the shape
      `classify_broadcast_kind()` implies (scalar, `n1`, `n2`, or `n1 x n2`),
      naming the effect and family on failure; recorded failing on the
      vector-kind dyad inits.
- [ ] 2.2 `R/functions_effects_REM.R` and `R/functions_effects_DyNAM_choice.R`:
      `init_REM_choice.ego`, `.global`, `.indeg`, `.outdeg`, `.node_trans`
      and `init_DyNAM_choice.alter` return the rate-side value as `stat`
      without the matrix and without zeroing the diagonal; `degree` through
      its existing path; coordination through its dispatch. The walk's
      `build_walk_engine()` stops reducing (1.2's interim). Test: 2.1 PASS;
      0.2 references `identical()` through the accessor (the diagonal now
      zeroed at expansion); 0.3 (a) passes; the atom store's seed reads the
      kind-shaped `stat` directly where it still took `cache`.
- [ ] 2.2a `init_at_kind()` joins the at-kind init contract (added 2026-10-02,
      from the `availability-one-object-per-loop` conformance report).
      `R/model_preprocess.R:653-689` seeds vector-kind constraint atoms at
      their kind by a `switch()` on the effect name, duplicating the rate-side
      inits; it is what lets that change's walk guard pass and no artifact
      describes it. Fold it into 2.2's per-effect init-at-kind so the atom
      store seeds through the same function as the statistics, and retire
      the switch. Verify: the 2.1 conformance test covers the constraint
      atom seed; `build_constraint_atom_store()` has no effect-name switch.
- [ ] 2.3 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      references PASS; `air format` and `lintr` on the touched files.

## 3. Updates at kind (D3)

- [ ] 3.1 Oracle test, written first: over the full event sequence of a dyad
      model with `alter()`, `ego()`, `indeg()` and `global()` with attribute
      changes, the eager expansion of the kind-shaped update stream equals
      the eager expansion of the fan-out collapsed by
      `broadcast_entries_from_updates()` (moved to a test helper), at every
      stored event; and the estimates on r, cpp and gather are unchanged
      within 1e-10.
- [ ] 3.2 The vector-kind update functions return rate-side entries;
      `to_ego()` and `to_alter()` deleted (ADR-0054); `merged_covariate_step()`
      maps an entry to a `stat_mat_broadcast` column without grouping;
      `broadcast_entries_from_updates()` leaves the package for the helper.
      Test: 3.1 PASS; 0.2 stream references `identical()`; 0.3 (b) passes.
- [ ] 3.3 Measure the per-event allocation of a degree and an alter effect on
      the cost-rules reference fixture against the recorded 0.20 MB and the
      prediction (about 0.03 MB); update the "Reference costs" table in
      `.claude/rules/walk-cost.md` with the new numbers (a tracked file, in
      the task commit); numbers in `progress.md`. NEWS.d bullet (vector-kind
      statistics stored and emitted at kind; `prep$initial_stats` replaced by
      `initial_stats(prep)`).
- [ ] 3.4 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      references PASS; walk budget PASS (lower the record if the floor
      fails).

## 4. Registry note and close (D6)

- [ ] 4.1 The `effect-term-registry` note was written at proposal time
      (2026-10-01, Alvaro): its design D2d now reads `broadcast_kind` with
      the encoding `0`-`3` plus `4` for "resolved from the `type` argument
      at `construct_term()`", and its task 1.1a records the kind per effect
      against this change's conformance test. At apply, re-read that note
      against the final conformance test and amend it if the encoding moved;
      add the hand-off line to `closure-effects-refactor`'s `progress.md`
      (claim first) so its D2 caches and the `node_trans` classification are
      checked against the conformance test.
- [ ] 4.2 `bash .plan/opsx-spec-placement-check.sh initial-stats-at-kind` and
      `openspec validate initial-stats-at-kind --strict` clean.
- [ ] 4.3 `NEWS.d/initial-stats-at-kind--initial-stats-at-kind.md`
      consolidating 3.3's bullet. No `NEWS.md` or Version edit on the branch.
- [ ] 4.4 Vault: ADR-0112 `spec:` and open questions current; dashboard
      fragment in `_dashboard-inbox/`; session note closed.
- [ ] 4.5 Final verification: full `NOT_CRAN=true` suite green; baselines
      and goldens PASS not SKIP; `status: landed (feature_simulation,
      awaiting fold)` in `proposal.md`.
