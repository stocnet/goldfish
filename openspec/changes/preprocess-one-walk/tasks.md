## 0. Gate and baseline

- [x] 0.1 Record the go/no-go rule before measuring (design D6): the recipe
      loops stay as the batch path if the merged walk exceeds 1.10x their wall
      time on CollegeMsg with the full model. Then run `process-simulation`
      task 1.2 (Social Evolution full model with windows, an interaction term
      and a support constraint; CollegeMsg from `.plan/datasets/CollegeMsg.txt`;
      one `walk_open()` replay) and record wall time, peak memory and the
      merged/oracle ratio in `.plan/` and `progress.md`. Decide: groups 1–2
      only, or 1–4.
      — done 2026-09-09 (session `goldfish-50`). CollegeMsg base model: two
      recipe loops 219.6 s, merged walk 67.2 s, **ratio 0.31**; Social Evolution
      reversed, merged 1.31x-1.78x. Allocation showed the ratio is a 2-to-1
      count of per-event adjacency-matrix copies, not a design property (D9,
      D10). **Verdict: no-go** — groups 1 and 2 only; group 3 waits on the
      re-run after 0.4-0.6. Numbers and scripts in
      `.plan/sp/preprocess_timing_2026-09.md`; ADR-0057.
- [ ] 0.2 Branch `refactor/preprocess-one-walk` off `develop`; confirm no
      incremental `preprocessed=` walk path exists on the recipe loops (a
      supplied `preprocessed` skips the walk); inventory every caller of
      `run_sender_recipe_loop()`, `run_dyad_recipe_loop()` and the helpers
      only they use, and the `writer`/`new_writer` selection in
      `estimate_wrapper()`; note it in `progress.md`.
- [ ] 0.3 Verification: `NOT_CRAN=true` suite green on the branch start;
      frozen baselines and C++ goldens PASS not SKIP.
- [ ] 0.4 Remove the per-event state copy (design D10). The state write that
      follows a closure call duplicates the whole n1 x n2 matrix, in both recipe
      loops and in `merged_apply_state_update()`; the same pattern folds
      `stat_mat` in the R estimation backend (`R/cpp_interface.R`) and
      `live_stats` in `walk_fold_engine()`. Land an in-place write for all four
      sites; the C++ engines already mutate in place through
      `src/broadcast_updates.cpp`, and the `cpp-recompile` skill applies to any
      `src/` edit. Measured on the same shape: 3.67 ms/event -> 0.02 ms/event,
      no copy at all. Argue the mutation hazard explicitly in the commit — a C++
      write bypasses R's copy semantics, so any other live reference to the
      matrix would see it. The pure-R alternative, should that hazard prove
      real, is slice-passing (hand the closure only the row/column it needs,
      measured at 0.02 ms/event against 0.48 for the whole matrix), which is
      safe but changes the effect-update contract across every effect in three
      model families. Passing the closure the state environment does NOT work:
      slicing inside it binds the matrix just the same (0.39 ms/event, still
      copying). Tests: `tracemem` reports no duplication across a multi-event
      walk on a repeated-dyad fixture; allocation per event falls below one
      n1 x n2 matrix; frozen 1e-6 baselines and C++ goldens PASS, since no value
      changes.
- [ ] 0.5 Store the support mask by its axis-union kind (design D11).
      `preprocess_support_mask()` returns one dense n1 x n2 logical per snapshot
      time; a separable constraint is a length-n1 or length-n2 vector, or a
      scalar. `active_dyad_encoding_decide()` already classifies point / alter /
      ego / scalar and `test-active_dyad_fold.R` already asserts the alter fold
      allocates no dense matrix, so this is storage only. Tests: existing
      support-constraint fixtures unchanged; a constrained model on a
      1899-actor node set preprocesses without exhausting memory.
- [ ] 0.6 Parity fixture for the weighted/unweighted divergence (blocks the
      re-run). The merged walk computes weighted degree where the recipe loops
      compute unweighted degree on an accumulating layer
      (`.plan/bug-merged-walk-weighted-degree.md`); every fixture in
      `test-preprocess_joint.R` gives each dyad one event, which is why the
      byte-identity assertions pass. Add both: (a) a five-node four-event toy
      whose dyad 1 -> 2 repeats, as the cheap unit-level guard, and (b) a
      complex parity specification in the shape of the asta Copenhagen
      `rate_1` / `choice_1` (`.plan/bench/euler/PLAN.md`), on Social Evolution —
      rate `1 + indeg + indeg(weighted) + outdeg(weighted, log1p) +
      indeg(window = "2 hours") + node_trans + indeg(friendshipNetwork) +
      ego(actors$floor)`; choice `inertia + inertia(weighted) + recip +
      recip(weighted, log1p) + trans + trans(history = "sequential") + cycle +
      common_sender + indeg + indeg(weighted) + inertia(window = "2 hours") +
      tie(friendshipNetwork) + alter(actors$floor) + same(actors$gradeType) +
      inertia:recip`. The weighted/unweighted twins of the same effect on the
      same object are the point; the windowed terms need a no-window variant
      until task 1.3 lands. A parity fixture, not a baseline: it must not touch
      the frozen 1e-6 set.
- [ ] 0.7 Make the window abort reachable. `build_walk_engine()` carries "The
      merged walk does not yet support window effects", but a windowed term puts
      the derived object into the merged registry and `build_shared_state()`
      dies first with `non-numeric matrix extent`, so task 1.3 has nothing to
      lift until the abort fires. Test: `expect_snapshot(error = TRUE)` on a
      windowed spec through `preprocess_joint()` names window effects.

- [ ] 0.8 Decide the statistics layout on the task 0.6 fixture (design D12), and
      do it before 0.4 picks where the in-place write lands. The `n1 x n2 x p`
      array is not the shape utilities are computed in — both the handle and the
      C++ boundary flatten it to `(n1*n2) x p` first, and R being column-major
      that flattening is what makes a sender's block contiguous. Compare the
      flattened matrix against a length-`p` list of `n2 x n1` matrices: the list
      localizes copy-on-modify to one effect, the matrix keeps the product as a
      single BLAS call. Measure both on a realistic `p`; record the numbers
      beside the task 1.2 write-up.
## 1. Merged-walk parity with the recipe loops

- [ ] 1.1 Writers (design D5): `build_walk_engine()` takes the `writer` /
      `new_writer` pair and hands it to `init_consumers()`; `preprocess_joint()`
      and `run_merged_walk()` thread it from the preprocessing controls. Tests:
      `output = "gather"`, `"data.frame"`, `"db"` on a single-process fixture
      through `preprocess_joint(single_process_joint(spec))` equal the recipe
      loops' rendered output.
- [ ] 1.2 Observation window (design D4): `start_time` / `end_time` bound the
      shared schedule; rows before the start update state unwritten, rows after
      the end are dropped, every timed engine writes the closing right-censored
      row at the end time; lift the `run_merged_walk()` abort. Tests: bounded
      fixtures (start only, end only, both, end past the last event) equal the
      recipe loops byte-for-byte, including `total_time` and
      `avg_active_entity`.
- [ ] 1.3 Window effects (design D3): each unit's `plan$derivations` realize
      the derived windowed network into the shared object registry
      (deduplicated by derived identity) and its expiry streams into
      `build_joint_schedule()` with the stream index the recipe context assigns;
      lift the `build_walk_engine()` abort. Tests: the `window-list-*`
      windowed fixtures and a tied-time expiry fixture through the merged walk
      equal the recipe loops byte-for-byte; a two-process join windowing the
      same source shares one derived object.
- [ ] 1.4 Restricted opportunity sets and user support constraints on a
      single-process unit through the merged walk: parity tests against the
      recipe loops on the `active_dyad_fold` and support-constraint fixtures
      (both already exist on the joint side; this task proves the single-unit
      entry reaches them).
- [ ] 1.5 Verification: `NOT_CRAN=true` suite green; baselines PASS; the
      joint test file's byte-identity assertions now cover windowed, bounded,
      writer-varied and constrained single-process fixtures.

## 2. One compile stage

- [ ] 2.1 Extract the compile stage of `estimate_wrapper()` (parse, effects,
      links, `new_model_spec()`, `build_spec_map()`, imputation policy stamp)
      into one internal `compile_spec_map()`; `compile_recipe_spec_map()`
      becomes a call to it with the joint path's per-focal working copy as a
      parameter; `parsed_formula` reuse stays a parameter (design D2).
      Roxygen for the internal function; `devtools::document()`.
- [ ] 2.2 `build_merged_blocks()` accepts pre-compiled units: the joint path
      compiles per process with `compile_spec_map()` and hands units in; a
      one-unit entry wraps a single compiled `spec_map` (with its consumer
      specs for a flavored plan) for the single/flavored callers.
- [ ] 2.3 Verification: `NOT_CRAN=true` suite green; baselines PASS; the joint
      tests' compile assertions (one spec_map per process, grouped by block)
      unchanged.

## 3. Dispatch flip and deletion (task 0.1 said NO-GO; opens only on a passing re-run)

**Closed as of 2026-09-09.** The gate came back 0.31x, but the ratio counts
per-event matrix copies rather than architecture (design D9), so this group does
not open on it. It opens when tasks 0.4, 0.5 and 0.6 have landed and
`.plan/sp/preprocess_timing_2026-09.R` has been re-run on the fixed tree,
including a CollegeMsg subset at 10k-20k events to separate the per-call setup
constant from the copy. If the re-run puts the merged walk within 1.10x this
group proceeds as written; if not it stays closed, and task 4.1's NEWS.d
fragment records the merged walk's new parity and why the loops stay.


- [ ] 3.1 `preprocess.goldfishKind()` routes every `input_shape = "standard"`
      spec to the merged walk through the one-unit entry (design D1): a
      single-process call unwraps fid 1; a flavored plan returns the fid-keyed
      list with `process_map`. `preprocess_recipe()` passes the compiled
      `spec_map` and consumer specs through unchanged. The grouped branch is
      untouched. Full `NOT_CRAN=true` suite: baselines PASS. This is the
      bisectable commit.
- [ ] 3.2 `preprocess_flavored()` drops its per-family loop and calls the
      merged walk once for all families; its union planning, consumer specs,
      `validate_prep_support()` stamping and `process_map` contract stay.
      Tests: the flavored fixtures and the `estimate_flavored()` container
      tests unchanged.
- [ ] 3.3 Delete `run_sender_recipe_loop()`, `run_dyad_recipe_loop()` and the
      helpers only they called (inventory from task 0.2); keep the finalizer
      helpers the merged walk shares. `air format` the touched files, then
      `lintr::lint()` on them. The descriptor spec's guard test and
      `grep -n "eval(parse\|assign(" R/model_preprocess.R` stay clean.
- [ ] 3.4 Re-run the task 0.1 measurement on the flipped tree and record it
      beside the pre-flip numbers; confirm the ratio is within the recorded
      rule.
- [ ] 3.5 Verification: full `NOT_CRAN=true` suite green; frozen baselines and
      C++ goldens PASS not SKIP; `devtools::document()` if any roxygen changed.

## 4. Close

- [ ] 4.1 NEWS.d fragment (Internal): one batch preprocessing loop, the recipe
      loops deleted (or, on a no-go, the merged walk's new parity and the
      recorded reason the loops stay). No `NEWS.md` or Version edit on the
      branch (ADR-0040).
- [ ] 4.2 `bash .plan/opsx-spec-placement-check.sh preprocess-one-walk`
      clean; `openspec validate preprocess-one-walk --strict` clean; on a
      no-go, trim the three MODIFIED deltas to what landed.
- [ ] 4.3 Note the consequences on the neighbors in their `progress.md` when
      next claimed: `process-simulation` task 2.0c shrinks to the breakpoint
      API; ADR-0045's inspection records this as its first closed finding
      and `compile_spec_map()` as its second.
- [ ] 4.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      PASS not SKIP; `devtools::document()`; ready for the trunk merge to
      fold.
