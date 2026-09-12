**Test-driven, and the order is load-bearing.** Task 0.6 writes every parity
fixture before 0.4a-0.4e, 0.5b or group 1 touches code. Each fixture must fail
today on the divergence it targets, otherwise it is not a detector. The frozen
1e-6 baselines are NOT the detector for this change: none of their models reach
the imputation path, the edgeless-`four()` cache, a windowed constraint atom, or
a repeated dyad with unweighted degree. A step whose fixture passed before the
step landed proves nothing.

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
- [x] 0.2 Work on **`feature_simulation`**, alongside `process-simulation`; no
      new branch (decided 2026-09-09). That branch already carries both changes'
      artifacts, and `process-simulation` task 2.0a waits on task 0.5a here, so
      splitting them across branches would mean folding one before the other
      could start. ADR-0041 covers the shape: two changes on one variant branch,
      each flipped to `status: landed (feature_simulation, awaiting fold)` when
      complete rather than archived there, with two `NEWS.d/` fragments the
      trunk merge folds together. The changes' spec deltas do not overlap
      (`flavored-processes` / `model-recipe-dispatch` / `multi-process-walk`
      here against `fit-class-hierarchy` / `process-simulation`), so neither
      declares `depends-on` — the dependency is code sequencing, recorded in the
      tasks. Then: confirm no
      incremental `preprocessed=` walk path exists on the recipe loops (a
      supplied `preprocessed` skips the walk); inventory every caller of
      `run_sender_recipe_loop()`, `run_dyad_recipe_loop()` and the helpers
      only they use, and the `writer`/`new_writer` selection in
      `estimate_wrapper()`; note it in `progress.md`.
      — done 2026-09-09 (session `goldfish-64`). No new branch. The
      `preprocessed=` finding CORRECTS the design's Risks bullet: a supplied
      object skips the walk only when every effect matches; an added effect
      runs a second full recipe walk over the new effects and column-merges,
      and that call drops the support constraint and flavor plan positionally.
      Recipe-loop callers, exclusive helpers (five), shared helpers (ten, all
      of which must survive the deletion), the dead ~690-line
      `preprocess_monolith()` and the writer/`new_writer` threading are
      tabulated in `progress.md`.
- [x] 0.3 Verification: `NOT_CRAN=true` suite green on the branch start;
      frozen baselines and C++ goldens PASS not SKIP.
      — done 2026-09-09 (session `goldfish-64`). PASS 7296, FAIL 0, SKIP 5,
      WARN 1140 (the expected deprecation/dissolve noise). Every
      `test-coefficient_baselines*.R`, `test-baselines_*.R`,
      `test-dynami_baselines.R` and `test-twomode_baselines.R` row reports
      `skipped = FALSE`, so the 1e-6 floor ran.
- [x] 0.4a Remove the copy at the two FOLD sites (design D10, low risk).
      `stat_mat <- .gather_apply_stat(stat_mat, ...)` in the R estimation
      backend (`R/cpp_interface.R`, three call sites) and `live_stats` in
      `walk_fold_engine()` duplicate their buffer per event only because the
      helper takes it as an argument. These buffers are engine-local and never
      reach an effect closure, so there is no aliasing question. Measured: 0.430
      ms/event through the helper against 0.003 ms/event with the same
      subassignment inline, identical result. Either remove the binding or write
      through C++ (`cpp-recompile` skill on any `src/` edit); C++ keeps the
      helper shared across its callers. Tests: `tracemem` shows no duplication
      across a multi-event fold; the R backend and the C++ backend still agree
      to 1e-10 on the timing-ledger fixtures; baselines PASS.
      — done 2026-09-09 (session `goldfish-64`). Neither route as written: the
      helpers keep the index arithmetic and return WHAT to write, and only the
      write moves to the caller. Pure R, no inlining, no C++, one definition
      instead of five copies. `.gather_apply_stat()` becomes
      `.gather_stat_cells()` (paired cells) and `.gather_apply_broadcast()`
      becomes `.gather_broadcast_blocks()` (a row selection, a column, one value
      to recycle) — the shapes differ because a broadcast is an axis, and paired
      cells made the full fan-out SLOWER than the copy at 1200 actors
      (43.5 -> 74 ms; blocks give 5.5). Five call sites updated: three in
      `R/cpp_interface.R`, one in `R/process_state_evaluators.R`, one in
      `walk_fold_engine()`. Per fold, old -> new at 400 / 1200 actors: point
      2.31 -> 0.005 and 9.97 -> 0.033 ms; axis broadcast 1.44 -> 0.02 and
      6.67 -> 0.067; full fan-out 7.97 -> 0.60 and 43.5 -> 5.5. Three tests in
      `test-cpp_interface.R`, the first asserting `tracemem` prints nothing
      across a four-event fold (verified as a detector: the old helper prints
      four copy lines). `test-backend_parity.R` 273 pass at 1e-10; every
      baseline and golden file PASS. End to end on Social Evolution it is
      invisible (0.125 s and 135 MB either way) because the old helpers copied
      only when they wrote, and that model has 83 point updates over 440 events
      on a 0.2 MB buffer; task 0.9 measures it where the buffer is 27 MB.
- [x] 0.4b Clear the aliases the state write would expose (design D10). An
      in-place write is correct only where nothing else holds a live reference.
      Three sites, none reached by the frozen baselines' models, so each needs
      its own regression test: (i) `ds_impute_missing()` caches an imputed
      matrix in `src$net_override[[name]]` and `ds_network()` returns that same
      object on every later call, so two containers from one source share the
      SEXP — have `ds_network()` copy when it serves an override, one copy per
      container build rather than per event; needs `NA` in a network to trigger;
      (ii) `init_DyNAM_choice.four()` returns `list(cache = network, stat =
      network)` on an edgeless network, so give the cache its own matrix. The
      legacy `ds_network.goldfishSourceEnvir()` path is deliberately NOT
      hardened: the environment path retires with `refactor-dynami-engine`, so
      work there is not worth doing. This lands BEFORE 0.4c so a baseline move
      is attributable to one step.
      — done 2026-09-09 (session `goldfish-64`). Two sites, as scoped (the
      legacy envir path left alone). `ds_network.goldfishSourceStocnet()` now
      returns a fresh `matrix()` built from the override rather than the cached
      object; the ordinary branch already materialized per call.
      `init_DyNAM_choice.four()`'s edgeless branch allocates its own cache and
      stat with the input's dimnames instead of handing the state matrix back
      as both. Two regression tests in `test-preprocess_parity.R`, both on
      object identity via `tracemem` rather than on values: under R's copy
      semantics the alias is harmless today, so a value comparison would pass
      over it, and it stops being harmless exactly when 0.4c lands. No value
      changes: 401 assertions across nine affected files, frozen baselines and
      C++ goldens PASS.
- [x] 0.4c Remove the copy at the two STATE sites (design D10). Both recipe
      loops and `merged_apply_state_update()` write
      `state$networks[[key]][sender, receiver] <- replace` after the effect
      closures have bound the matrix. Inlining does not help here: the sharing
      comes from the closure call, and the write is already inline. Write
      through C++ in place — measured 3.67 ms/event -> 0.02 ms/event on a
      1899^2 matrix with no copy at all. The pure-R alternative is
      slice-passing (hand the closure only the row or column it needs, 0.02
      ms/event against 0.48 for the whole matrix), reached for only if the write
      cannot be made safe, since it changes the effect-update contract across
      every effect in three model families. Passing the closure the state
      environment does NOT work: slicing inside it binds the matrix just the
      same (0.39 ms/event, still copying). Tests: `tracemem` reports no
      duplication of the state matrix across a multi-event walk on a
      repeated-dyad fixture; `bench::bench_memory()` on the CollegeMsg fixture
      falls well below today's 27.53 MB per event; the 0.4b fixtures still pass;
      frozen 1e-6 baselines and C++ goldens PASS, since no value changes.
      — done 2026-09-09 (session `goldfish-64`), AFTER 0.4d rather than before
      it: until the merged walk's state write actually persisted there was no
      copy in it to remove. `src/state_write.cpp` exports `set_matrix_cells()`,
      an in-place REALSXP writer; `state_set_tie()` in `R/preprocess_builders.R`
      routes the three sites through it and keeps the R subassignment as the
      fallback for a non-double matrix. On a 300-actor, 398-event fixture the
      whole-call allocation drops: recipe rate 281 -> 8.0 MB, recipe choice
      297 -> 23.6 MB, merged 297 -> 24.1 MB. Before, 398 events times a 0.69 MB
      matrix was 275 of the 281 MB — the copy was 98 percent of the walk.
      **A THIRD alias, which the design's enumeration missed.**
      `init_DyNAM_choice.tie()` (and `inertia`, which delegates to it) returned
      the state's own matrix as `stat` under the default identity transformer,
      because `unname()` hands its argument back unchanged when there are no
      names to strip. Found by auditing every effect initializer against its
      input by object address rather than by reading them; the same audit now
      reports no aliases at all. Fixed with an explicit `matrix()`, own test.
      Tests: `tracemem` prints nothing across a four-event state walk; the
      undirected write reaches both cells; 1200 assertions across fourteen
      affected files pass, frozen baselines, C++ goldens and
      `test-backend_parity.R` included.
- [x] 0.4d Make the merged walk compute an unweighted degree unweighted
      (design D14). On a layer whose events accumulate, `preprocess_joint()`
      tracks WEIGHTED degree where the recipe loops track unweighted, so the two
      substrates do not compute the same statistic and no ratio between them is
      a decision input (ADR-0057). Minimal reproduction, five nodes and four
      calls with the dyad `1 -> 2` repeating at t = 3: the recipe loop leaves
      `indeg(N2)` at 1 and emits no update, the merged walk emits one carrying
      the value 2. Confirmed against ground truth on Social Evolution, where the
      recipe loop reproduces `colSums(A > 0)` and the merged walk reproduces
      `colSums(A)` (`.plan/bug-merged-walk-weighted-degree.md`).
      **The cause is not yet established** — do not assume one. The single
      diagnostic clue: writing the effects as `indeg(calls, weighted = TRUE)`
      makes the rate block agree byte-for-byte, which pins the divergence on how
      the `weighted` flag reaches the merged engine rather than on the schedule,
      the shared state or the event-argument resolution (`merged_build_event_
      args()` resolves increment against the live state exactly as the recipe
      loop does, so both hand the closure the same `replace`). The choice side
      diverges differently and needs its own reading: for unweighted `inertia`
      the merged walk emits a redundant update carrying the SAME value, which is
      numerically harmless but breaks byte-identity and costs stored work.
      Starts AFTER 0.6 has written the failing fixtures, and must land BEFORE
      0.9 re-runs the gate. Tests: task 0.6 fixtures (a) and (b) go green,
      byte-identical between substrates; the reconstruction assertion on Social
      Evolution matches `colSums(A > 0)`; frozen 1e-6 baselines and C++ goldens
      PASS, since the recipe loops are the side that is already right.
      — done 2026-09-09 (session `goldfish-64`). **The cause is not the
      `weighted` flag, and the recorded clue pointed the wrong way. The merged
      walk's shared state never advanced at all.**
      `merged_apply_state_update()` subassigns into `state$networks[[key]]` and
      returns `invisible(NULL)`; the state container is a plain list, so the
      only route back to the caller is a return value that did not exist. Both
      callers discarded it — `run_merged_walk()` and the stepping handle's
      `walk_apply_object_event()` — so every effect read every tie as absent
      however often it had fired. An unweighted `indeg` reading a frozen zero
      counts events, which on a layer of +1 increments IS `colSums(A)`; and
      `weighted = TRUE` agreed because a weighted degree adds `replace`
      whatever the cell held, which made the clue a coincidence. Fix: return the
      state, both callers bind it. Fixtures (a), (b) and (d) go byte-identical
      on both families; `test-preprocess_joint.R`'s 120 assertions unchanged;
      the frozen baselines, the C++ goldens and `test-backend_parity.R` PASS.
      The handle needed its own detector — its 43 assertions pass either way,
      because its fixtures give each dyad one event and its batch-versus-replay
      contract compared two substrates frozen in the same way. Bug note updated
      with the real cause; D14 rewritten.
- [x] 0.4e Impute a missing network cell on the merged walk (design D15).
      Found while writing task 0.6's fixture (e), which the fixture list
      expected to touch `src$net_override` for 0.4b's aliasing test and nothing
      more. A network carrying `NA` does not preprocess on the merged walk at
      all: `merged_build_event_args()` resolves an increment against an
      unimputed cell and dies on `if (replace_value < 0)`. The cause is one
      missing call. `prepare_recipe_context()` runs `ds_impute_missing()` before
      it builds its state container (`model_preprocess.R:328`);
      `build_merged_blocks()` builds its own `new_data_source()` and hands it
      straight to `build_shared_state()` (`preprocess_joint.R:625-635`) with
      nothing in between, so the `NA` survives into the shared state.
      **Networks only** — the merged walk already carries the per-object policy
      on `build_shared_object_props()` and recodes nodal values at walk time in
      `impute_nodal_value()`, and the initial nodal vector is byte-identical to
      the recipe loop's on a fixture with a missing covariate and a complete
      network. The crash is the benign case: a `replace` layer never reaches the
      sign test, so there the `NA` would pass through the effect closures into
      the statistics. Call `ds_impute_missing()` where the recipe path calls it,
      on the source, not inside `build_shared_state()` — the step belongs to the
      source and the merged walk already threads one. Reproduction in
      `.plan/bug-merged-walk-skips-network-imputation.md`. Lands AFTER 0.4b,
      which touches the same two functions, and BEFORE 0.9. Tests: task 0.6
      fixture (e) goes green, byte-identical between substrates; a `replace`
      layer carrying `NA` preprocesses without an `NA` reaching the statistics;
      the nodal-only case stays unchanged; frozen 1e-6 baselines and C++ goldens
      PASS, since no baseline model carries a missing network cell.
      — done 2026-09-09 (session `goldfish-64`). One call, where the recipe
      path makes it: `build_merged_blocks()` now runs `ds_impute_missing()` on
      the shared source between `new_data_source()` and `build_shared_state()`,
      with the registry's object names as the link and the control's policy.
      Three tests: fixture (e) goes byte-identical on both families; a
      `replace` layer carrying `NA` keeps `NA` out of `initial_stats` and out of
      the update stream; and the nodal-only case is unchanged, which is the
      control for the scope claim. `test-preprocess_parity.R` is down to two
      remaining defects (0.5b and 0.7).
- [x] 0.5a Store the support mask by its axis-union kind (design D11).
      `preprocess_support_mask()` returns one dense n1 x n2 logical per snapshot
      time; a separable constraint is a length-n1 or length-n2 vector, or a
      scalar. `active_dyad_encoding_decide()` already classifies point / alter /
      ego / scalar and `test-active_dyad_fold.R` already asserts the alter fold
      allocates no dense matrix, so this is storage only. Tests: existing
      support-constraint fixtures unchanged; a constrained model on a
      1899-actor node set preprocesses without exhausting memory.
      — done 2026-09-09 (session `goldfish-64`). Storage only, as scoped, and
      half the machinery was already there: `support_to_grid()` expands a mask
      held at its kind and `assemble_model_mask()` already takes one, so only
      the inverse was missing. `support_from_grid()` in `R/support_mask.R`;
      `eval_constraint_mask()` reduces each snapshot before storing it; the six
      reader sites expand one snapshot at a time. Measured at 1899 actors and
      2998 snapshots, alter kind: 7.5 KB per snapshot, 21.9 MB for the timeline,
      against 40.3 GB dense. **Symmetrising destroys separability** (`m & t(m)`
      of a row-constant mask is an outer product), so a symmetric mask is stored
      dense whatever its atoms say. That needed a second field rather than a
      reinterpretation of the first: the output carries `stored_kind` beside
      `mask_kind`, readers expand with `stored_kind`, and `mask_kind` keeps
      meaning what it meant to `render_gather()` and the encoding decision.
      Three tests in `test-support_mask_maintain.R`;
      `test-active_dyad_fold.R`'s from-scratch check reads the vector instead of
      a row of a grid.
- [x] 0.5b Register a windowed constraint atom's derived object in
      `plan$derivations`, like any other windowed term (design D13). A
      `window =` inside a `support_constraint` is silently ignored today:
      `~ !tie(call_network, window = 5)` and
      `~ !tie(call_network, window = "365 days")` give masks byte-identical to
      `~ !tie(call_network)`, and a matching window in the estimated formula
      does not rescue it (`.plan/bug-constraint-window-ignored.md`). Measured:
      a windowed formula term puts one `{kind = "window", derived_name =
      "call_network_5"}` entry in `plan$derivations`, while a windowed
      constraint atom puts none — the merged units report zero derivations and
      the shared registry holds only the source layer, so `ds_realize_
      derivations()` has nothing to build and the atom's reference still names
      the source. The atoms are plain effects, so give them the same treatment:
      their windowed terms contribute `kind = "window"` entries beside the
      `kind = "support_mask"` one, references rewired to the derived names,
      deduplicated by derived identity against the formula's. The realize step
      needs no change. Do NOT reach for `parse_time_windows()` as the entry
      point — it is metadata-only on the recipe path and is the pre-registry
      framing; the work lands on what enters `plan$derivations`. Starts AFTER
      0.6 has written the failing test. Tests: task 0.6 fixture (c) goes green,
      with the two windows differing from each other and from no window; a
      formula and a constraint windowing the same object at the same width
      resolve to ONE derived object and one expiry stream; existing unwindowed
      support-constraint fixtures unchanged.
      — done 2026-09-09 (session `goldfish-64`). `compile_support_constraint()`
      runs `parse_time_windows()` over the atom names with
      `realize_windows = FALSE`, which is what both records the derivation AND
      rewires the atom's object reference to the derived name — the rewrite is
      the half that binds an atom to its derived object, so registering without
      it would change nothing. `register_constraint_windows()` folds the
      results into `plan$derivations`, deduplicated by derived identity against
      the formula's; the realize step is untouched, which was the point.
      Verified on the toy fixture: `~ !tie(calls)` gives 139 live cells,
      `window = 2` gives 141, and `window = 1000` — longer than the whole
      sequence — returns to 139, the right answer for a window that expires
      nothing. Bug note marked fixed.
- [x] 0.6 Parity fixtures, written BEFORE any of 0.4a-0.4e, 0.5b or group 1
      touches code (see the TDD note at the head of this file). They are the
      detector for every later step, so a fixture that does not fail today on
      the divergence it targets is not yet a fixture. Five, each with a stated
      target:

      (a) **Five-node repeated-dyad toy, complex specification.** The cheap
      unit-level guard for the weighted/unweighted divergence
      (`.plan/bug-merged-walk-weighted-degree.md`). Small enough to read by eye,
      but the specification must move EVERY statistic it declares at least once
      over the sequence, so a silently frozen statistic fails rather than
      passing vacuously. Assert that: every column of `stat_mat_update` is
      touched, and every declared effect appears in the update stream.
      Terms to cover, chosen for the update paths they exercise:
      `global(x)` and `global(x):inertia` (a global operand feeding an
      interaction, broadcast kind 3 into the second hop); `ego(a):alter(b)` on
      the choice side (an ego-axis operand crossed with an alter-axis one, the
      two broadcast kinds meeting in one product); `mixed_trans(net2, window =
      ...)` and `mixed_cycle(net2, window = ...)` (a mixed effect whose derived
      object expires while a second network drives it); the weighted and
      unweighted twins of `indeg` / `inertia` on the same object, which is the
      divergence itself.

      (b) **Complex specification on Social Evolution**, asta Copenhagen
      `rate_1`/`choice_1` shape (`.plan/bench/euler/PLAN.md`) — rate
      `1 + indeg + indeg(weighted) + outdeg(weighted, log1p) +
      indeg(window = "2 hours") + node_trans + indeg(friendshipNetwork) +
      ego(actors$floor)`; choice `inertia + inertia(weighted) + recip +
      recip(weighted, log1p) + trans + trans(history = "sequential") + cycle +
      common_sender + indeg + indeg(weighted) + inertia(window = "2 hours") +
      tie(friendshipNetwork) + alter(actors$floor) + same(actors$gradeType) +
      inertia:recip`. Realistic `p`, so it doubles as task 0.8's benchmark.

      (c) **Windowed support constraint**, e.g. `~ !tie(exoNet, window = 5)`.
      This one FAILS TODAY and must be written as a failing test first: the
      window is silently ignored, and a 5-second and a 365-day window give
      byte-identical masks (`.plan/bug-constraint-window-ignored.md`). The
      constraint's atoms are parsed separately from the formula, so their window
      parameters never become derivations. Fixing it means unioning the
      constraint's derivations into what `compile_support_constraint()`
      receives, or aborting rather than ignoring.

      (d) **Flavored creation/deletion with a state-forced constraint.** Two
      flavors on one layer, creation constrained to `~ !tie(net)` and deletion
      to `~ tie(net)`, so the risk set is exactly complementary and moves with
      the state. `inertia` is unusable by construction (identically 0 for
      creation, 1 for deletion), which is the point: it forces the fixture onto
      the constraint machinery rather than letting a tie-history effect carry
      the signal. Targets the mask timeline, not the statistics.

      (e) **A missing-data variant of one of the above**, with `NA` in a network
      and in a nodal covariate, exercising the imputation policies. This is also
      the only fixture that reaches `src$net_override`, which is 0.4b's first
      aliasing site, so 0.4b's regression test rides on it. Written 2026-09-09:
      it fails harder than expected. The merged walk does not preprocess a
      missing network cell at all, because its state creation skips the
      imputation step the recipe path takes — a sixth defect, now owned by task
      0.4e (design D15).

      Constraint on all five: parity fixtures, not baselines. They must not
      touch the frozen 1e-6 set. Windowed terms need a no-window variant for the
      merged walk until task 1.3 lands.
      — done 2026-09-09 (session `goldfish-64`).
      `tests/testthat/helper-parity-fixtures.R` and
      `tests/testthat/test-preprocess_parity.R`, 14 tests. All five fixtures
      fail on arrival, on the divergence each targets:
      2, 3, 5 and 11 on the weighted/unweighted degree (0.4d); 4 on the
      unreachable window abort (0.7); 7 and 8 on the ignored constraint window
      (0.5b); 13 on the merged walk's missing network imputation (0.4e, a
      defect this task surfaced); 14 on the `net_override` alias (0.4b).
      Five pass as guards rather than detectors: 1 (every declared effect moves
      at least once, reading `stat_mat_broadcast` beside `stat_mat_update`
      because ego/alter/global updates land there), 6 (the recipe loops
      reconstruct `colSums(A > 0)`), 9 (window derivations deduplicate),
      10 (an unwindowed constraint is unchanged), 12 (one mask snapshot per
      recorded event).
- [x] 0.7 Make the window abort reachable. `build_walk_engine()` carries "The
      merged walk does not yet support window effects", but a windowed term puts
      the derived object into the merged registry and `build_shared_state()`
      dies first with `non-numeric matrix extent`, so task 1.3 has nothing to
      lift until the abort fires. Test: `expect_snapshot(error = TRUE)` on a
      windowed spec through `preprocess_joint()` names window effects.
      — done 2026-09-09 (session `goldfish-64`). `abort_merged_window_effects()`
      runs in `build_merged_blocks()` right after the shared object registry is
      assembled and before the shared state is built, so it fires several frames
      ahead of the `non-numeric matrix extent` crash. It reads
      `plan$derivations` rather than the effect flags, which is the registry a
      window actually travels through and which now also carries a constraint
      atom's window (task 0.5b). The message names the derived objects and
      points at `compute_statistics()`; class
      `goldfish_merged_unsupported`. The `build_walk_engine()` abort is left in
      place as the second line of defence; task 1.3 lifts both.

- [x] 0.8 Decide the statistics layout on the task 0.6 fixture (design D12), and
      do it before 0.4 picks where the in-place write lands. The `n1 x n2 x p`
      array is not the shape utilities are computed in — both the handle and the
      C++ boundary flatten it to `(n1*n2) x p` first, and R being column-major
      that flattening is what makes a sender's block contiguous. Compare the
      flattened matrix against a length-`p` list of `n2 x n1` matrices: the list
      localizes copy-on-modify to one effect, the matrix keeps the product as a
      single BLAS call. Measure both on a realistic `p`; record the numbers
      beside the task 1.2 write-up.
      — done 2026-09-09 (session `goldfish-64`). **The flattened matrix**, and
      the list is rejected rather than deferred. Its stated advantage does not
      survive 0.4a: with the write left to the caller there is no
      copy-on-modify to localize, so a point update and an axis broadcast are
      free in all three layouts and the two allocate identically. That leaves
      the read, where the matrix is twice as fast on the full linear predictor
      (2.9 ms against 5.8 at 1200 actors) and never slower on a sender's slice.
      The array is worst: reshaping it to what both consumers actually use costs
      65.6 ms per call at 1200 actors. Numbers in D12.
      Two scope notes. Changing what `initial_stats` STORES is out of group 0 —
      it is part of the `goldfishStat` contract estimation, the diagnostics and
      the frozen baselines read, so it needs its own change. And this does not
      gate 0.4c after all: the state write is on an n1 x n2 adjacency matrix,
      a different object from the statistics buffer in every candidate layout.
- [x] 0.9 Re-run the gate and record the contrast (design D9). The pre-fix
      numbers are already recorded and are NOT re-measured: CollegeMsg base
      model, recipe loops 219.6 s, merged walk 67.2 s, ratio 0.31; Social
      Evolution merged 1.31x (windowless) and 1.78x (plain); allocation 27.53 /
      36.56 / 27.67 MB per event. This task re-runs
      `.plan/sp/preprocess_timing_2026-09.R` unchanged on the fixed tree and
      tabulates before against after, adding a **10k-event CollegeMsg subset**
      (not 20k) so the per-call setup constant is separated from the copy:
      Social Evolution is setup-dominated and the full 59,835 events are
      copy-dominated, and neither isolates the architecture on its own. The
      ratio this produces is the one group 3 reads.
      — done 2026-09-10 (session `goldfish-64`). **CollegeMsg full, base model:
      the two recipe loops go 219.6 s -> 24.3 s and the merged walk 67.2 s ->
      24.1 s, so the ratio moves from 0.31 to 0.99 — parity.** The rate loop is
      59x faster and the choice loop 4.5x, which is the copy's own shape: the
      rate loop's per-event work was almost entirely duplication, the choice
      loop also computes an n1 x n2 statistic per event. The full contrast:
      Social Evolution 1.78 -> 1.34 (plain) and 1.31 -> 1.26 (no_window);
      CollegeMsg 10k 1.05 (base) and 1.16 (constrained, which previously
      errored on the 24 GB limit). Against the 1.10x threshold the answer is now
      size-dependent and close to the line rather than three times under it.
      **The verdict is not stated in the write-up**: group 3 needs Alvaro's
      explicit approval on top of a number (D7, task 3.3), and a result this
      close is the case judgement exists for.
      **Second finding, and it needs an owner.** A support constraint now costs
      three orders of magnitude in TIME where it used to fail on memory: at 10k
      events on 1899 actors, `batch_rate` is 362 s constrained against 0.349 s
      unconstrained. **CORRECTED 2026-09-10**: this entry first named the dense
      grid rebuilt per snapshot as the cause. Profiling says that is secondary.
      `apply_atom_event` is 64.7 percent, `eval_constraint_mask` 16.0, and
      `fold_active_sender_support` 18.8, and allocation is 44.3 GB on a
      600-actor 1498-event call, about eleven matrix-sized allocations per
      event. The dominant cost is the atom maintenance: the atoms are stored
      dense, `call_atom_template()` binds the state matrix and the walk then
      writes it with `<<-`, which is ADR-0057's copy-on-modify pattern surviving
      in the one file group 0 deliberately skipped. Owned by the successor
      change `support-mask-sparse-updates` (ADR-0061), which also found that the
      living spec already requires the fix. This is why the full CollegeMsg
      re-run is scoped to the base model — at 59,835 events the constrained
      cells would be hours each and would measure the mask maintainer, not the
      gate's question.
      **CORRECTED AGAIN 2026-09-10 by `support-mask-sparse-updates`, which
      fixed it.** The profile's attribution was right about the largest single
      LINE and wrong about the largest single COST. Removing the atom
      maintenance (atoms stored at their kind, the `<<-` state write replaced by
      the in-place write) took the constrained rate loop from 75.57 s to
      39.66 s at 1500 events — 47 percent, not 64.7. The other 53 percent was
      the mask evaluation and the consumers, and the biggest item inside the
      consumers appeared on NO profile line: every dyad-side reader expanded an
      n1 x n2 grid per event to read one row of it, inside `support_to_grid()`
      under several different callers, which was 41 GB of grids built and
      discarded in the choice loop alone. A profile that names one function
      cannot see a cost spread across four callers of a fifth.
      **The constrained gate cell re-read, same script, same 10k subset.** The
      constrained rate loop is 362 s -> 1.237 s. The ratios move from 1.05 to
      **0.98** (base) and from 1.16 to **0.93** (constrained), so the
      constrained cell is now the merged walk's BEST rather than its worst: it
      builds one atom pool per process where the two recipe loops build one
      each. Both cells now sit under 1.0 against the 1.10x threshold, where the
      earlier reading had them straddling it. **The verdict is still not stated
      here** — D7 and task 3.3 require Alvaro's explicit approval on top of a
      number — but the number the decision reads is no longer measuring the mask.
      Script changes: a 10k subset, separate `cm10k` / `cmfull` selectors, and
      an `only_models` filter. Numbers in
      `.plan/sp/preprocess_timing_2026-09.md`.
- [x] 0.10 Re-measure the gate on the archived tree (design D6, D9). Task 0.9's
      table is not one reading. Its unconstrained cells were taken before
      `support-mask-sparse-updates` existed, its constrained cell was re-read
      after that change's group 6 and before its groups 8 and 10, and no cell
      was measured on the tree that archived it. Re-run
      `.plan/sp/preprocess_timing_2026-09.R` unchanged, at `b3d54bd` or later,
      and report THREE columns per cell rather than two: pre-group-0,
      post-group-0 (task 0.9), post-mask. Wall time and peak memory both — the
      mask change moved memory by more than it moved time on several cells, and
      the gate rule is stated on time alone.
      — done 2026-09-11 (session `goldfish-8f`), at `44c8a98` with a clean
      working copy; the only commits since the archive are documentation.
      **Only the constrained cells moved.** Pre-group-0 / post-group-0 /
      post-mask: Social Evolution plain 1.78 / 1.34 / **1.36**, Social
      Evolution no_window 1.31 / 1.26 / **1.03**, CollegeMsg 10k base
      — / 1.05 / **1.02**, CollegeMsg 10k constrained error / 1.16 / **0.97**,
      CollegeMsg full base 0.31 / 0.99 / **0.96**. The two Social Evolution
      rows are the control: `no_window` carries a constraint and `plain` does
      not, and only the first moved. Memory on the constrained 10k cells:
      merged 1589 -> 1086 MB, batch_choice 1297 -> 826, batch_rate 697 -> 608.
      A caution recorded with them: `peak_mb` comes from one cold run, and
      `college_msg_full` `batch_rate` read 513 MB in one run against 288 in
      another on the same tree, so no memory claim under about 2x survives a
      re-run without medians. Numbers in
      `.plan/sp/preprocess_timing_2026-09.md`.
      — **re-run 2026-09-11 late** on the settled branch (`d049329`; source
      unchanged since `29c16cb`), two full sweeps at five warm runs per cell, on
      a quiet machine with the other session holding its R work. Run 1 / run 2:
      Social Evolution no_window 0.888 / 0.889, plain 1.109 / 1.130, CollegeMsg
      10k base 0.997 / 1.004, 10k constrained 0.932 / 0.946, full base
      **1.007 / 1.002**, full constrained 0.956 / 0.934. **Against D6 the
      measured value on the dataset the rule names is 1.00**, every CollegeMsg
      cell passes, and the only cell above the line is plain Social Evolution at
      439 events, 0.071 s against 0.064 s.
      **The noise caveat above is corrected by this pair.** The 10 percent floor
      described absolute seconds compared across runs an hour apart under load
      8. The RATIO reproduces to 2.3 percent worst case and under 1 percent on
      four of six cells, because both arms are measured seconds apart inside one
      run. Peak memory also reproduces exactly on five of six merged cells, so
      the earlier 513-against-288 MB spread was contention rather than a
      property of the measurement. Read ratios from a quiet machine and a 3
      percent difference is real.
- [x] 0.10a Add the constrained cell at full CollegeMsg size. Task 0.9 scoped
      the full run to the base model because a constrained cell would have been
      hours and would have measured the mask maintainer rather than the
      substrate. At 1.237 s for the constrained 10k rate loop that objection is
      gone, and the hole it left is exactly the cell design D6 names.
      — done 2026-09-11 (session `goldfish-8f`). Measured for the first time:
      batch 35.816 s (rate 7.140 + choice 28.676) against merged 33.739 s,
      **ratio 0.942**, which is the merged walk's best full-size cell. The base
      model re-measured in the same run gives 26.809 against 25.463, 0.950. The
      constraint costs the batch path 9.0 s on top of the base model and the
      merged walk 8.3 s, because a layer's sub-models share one atom pool where
      the two recipe loops build one each. The script's dispatch now runs both
      models at both sizes; the constrained cells keep `reps = 1L` so they stay
      comparable with the recorded before-numbers, and the two comments that
      justified the old scoping were rewritten rather than dropped.
- [x] 0.10b Add the single-family cells to the script (rate-only, choice-only,
      REM, REM ordered), measured the same way as the two-family cells rather
      than by hand at an R prompt. They carry the decision:
      `compute_statistics()` and `estimate_dynam()` each take one sub-model, so
      the two-family cell the threshold is read on describes no user surface.
      The hand measurement put the single-family overhead at 1.41 to 1.56 and
      found it does not amortize with size (1.43 / 1.42 / 1.45 at 2k / 10k /
      60k events); none of it has been re-measured since.
      — done 2026-09-11 by session `goldfish-8b`, working in a separate git
      worktree; script `.plan/sp/preprocess_single_family_2026-09-11.R`,
      write-up alongside it, medians of three warm runs on CollegeMsg
      first-`n` events. **The overhead was the RATE family alone.** At
      `44c8a98`, rate_only / choice_only / rem by size: 500 2.33 / 1.10 / 1.21,
      1k 1.40 / 1.10 / 1.09, 5k 1.46 / 1.05 / 1.06, 10k **1.58** / 1.04 / 1.04.
      Choice and REM were already at parity; only the rate cell was high, and
      it got worse with size rather than amortizing. In seconds at 10k it was
      0.362 batch against 0.573 merged, beside a choice cell costing 4.8.
      **None of it was in the event loop.** The n1 x n2 adjacency matrix was
      materialized eight times per merged call against the recipe loop's five
      (an existence check and an NA check that both discarded the grid, plus a
      per-unit state container the walk never reads), and two helper calls per
      covariate event cost more than the degree kernel. Fixed in `53add23`
      (task 0.11). After it, rate_only / choice_only / rem / rem_ordered: 500
      1.22 / 1.07 / 1.19 / 1.07, 1k 1.20 / 0.95 / 1.02 / 0.95, 5k
      1.18 / 1.02 / 1.08 / 1.02, 10k **1.17** / 1.02 / 1.02 / 1.05. Rate-only
      seconds at 10k 0.364 batch against 0.426 merged; allocation per call 229
      MB batch against 237 merged, from 303 against 404.
      What remains is about 6 microseconds per covariate event inside
      `merged_covariate_step()` itself, 35.4 against 26.9 for the same work
      inline: one call per engine with seven arguments and `engine$` / `ctx$`
      reads. That is the price of hosting N engines in one loop, and it was
      left rather than special-cased for a single engine.
- [x] 0.10c Report by APPENDING a dated section to
      `.plan/sp/preprocess_timing_2026-09.md`. Do not edit the earlier
      sections; they are the recorded contrast. Fold task 0.9's 0.98 / 0.93
      constrained re-read into that file as its own dated section in the same
      pass — it currently lives only in this task list, so the write-up the
      task points at is missing its last reading.
      — done 2026-09-11 (session `goldfish-8f`). Appended as "Re-run on the
      archived tree (2026-09-11)"; the earlier sections are untouched. Task
      0.9's 0.98 / 0.93 re-read is folded in as its own dated section, marked
      as superseded by the archived-tree columns, which it agrees with to
      within run-to-run noise.
- [ ] 0.10d **Open question for Alvaro, to be answered before group 3 reads any
      number.** D6 fixes the threshold on CollegeMsg, full model, two-family
      cell. If the single-family path is where users actually are, that rule
      reads the wrong workflow, and restating it is a decision rather than a
      measurement. Record the answer as a new design decision (D16) with the
      rejected alternative, not as a remark inside the write-up.
- [x] 0.11 Cut the merged walk's setup and per-event overhead on a one-family
      spec (design D10's argument, applied to setup rather than to the event
      loop). **Filed after the fact**: the work landed as `53add23` from session
      `goldfish-8b` while task 0.10b was being measured, and group 0 is its
      honest home for the same reason 0.4a-0.4e sit here — a ratio measured
      against an accidental cost does not answer the gate's question. Five
      sites: `ds_check_network()` checks a layer exists without materializing
      it, `ds_impute_missing()` reads the initial stream rows for NA before
      materializing, `prepare_recipe_context(build_state = FALSE)` lets the
      walk engine skip the per-unit state and schedule it discards,
      `init_consumers()` takes a no-projection path on an identity effect map,
      and `run_merged_walk()` inlines the event arguments and the state write
      (the walk handle keeps the helpers for injected events). Two tests in
      `test-preprocess_joint.R` and `test-preprocess_builders.R`; `NAMESPACE`
      gains two S3 registrations for the new internal generic, no new export.
      Numbers under 0.10b. **One user-visible consequence to keep in view**: a
      missing stocnet layer now aborts by name instead of failing on
      `non-numeric matrix extent`, which is an improvement but is a message
      nothing specified.

## 1. Merged-walk parity with the recipe loops

**Ordering against `support-mask-sparse-updates` (added 2026-09-10).** Tasks 1.1,
1.2 and 1.3 are independent of the constraint and can proceed. **Task 1.4 should
wait**: it wires the merged walk into the mask machinery that change replaces,
and it touches `fold_active_sender_support()` / `fold_active_dyad_support()`,
which that change rewrites. Running both as concurrent sessions on one branch
would collide in source even though ADR-0036's claim protocol keeps the
artifacts apart.

- [x] 1.1 Writers (design D5): `build_walk_engine()` takes the `writer` /
      `new_writer` pair and hands it to `init_consumers()`; `preprocess_joint()`
      and `run_merged_walk()` thread it from the preprocessing controls. Tests:
      `output = "gather"`, `"data.frame"`, `"db"` on a single-process fixture
      through `preprocess_joint(single_process_joint(spec))` equal the recipe
      loops' rendered output.
      — done 2026-09-11 (`f7b79f4`). The signature mirrors the pair
      `preprocess_recipe()` already takes; the writer pair comes from the
      wrapper's `output`, not from the preprocessing controls, which carry only
      the db target. **"Rendered output" is not what this step can deliver**,
      and the tests say so rather than working around it: the recipe path
      continues into `finalize_gather_output()`, which attaches the labels,
      effect descriptions, timespan and column names that turn a stack into an
      export, and the merged walk reaches that stage only at task 3.1. So the
      assertions are at the writer's own stack, six fields with the
      render-attached column names stripped. A db writer is a gather writer
      that remembers where to persist, the write happening at the same export
      boundary, so its assertion is that the pair is accepted, the stack
      matches gather, and the target survives. `output = "data.frame"`
      preprocesses as a gather and reshapes at that boundary, so the gather
      case covers it. Detector confirmed live by ablation: dropping the factory
      back to the hardcoded default fails both gather assertions.
      `NOT_CRAN=true` PASS 8282, FAIL 0, SKIP 5, 54 baseline and golden rows
      none skipped; lint on the touched file unchanged at 29.
- [x] 1.2 Observation window (design D4): `start_time` / `end_time` bound the
      shared schedule; rows before the start update state unwritten, rows after
      the end are dropped, every timed engine writes the closing right-censored
      row at the end time; lift the `run_merged_walk()` abort. Tests: bounded
      fixtures (start only, end only, both, end past the last event) equal the
      recipe loops byte-for-byte, including `total_time` and
      `avg_active_entity`.
      — done 2026-09-11 (`089f32f`). `resolve_walk_window()` mirrors
      `prepare_recipe_context()`'s branches, so a bound means the same thing on
      either substrate. **The task text missed half the work**: a burn-in
      update is not an unwritten update, it folds into `initial_stats`. The
      recipe loops do that at four emission sites — plain effects and
      interaction products, sender and dyad shaped — and without it a
      start-bounded model begins from the state at time zero rather than the
      state at the window open, which is a different model, not a smaller one.
      `merged_covariate_step()` gained an `is_valid_event` argument and mirrors
      all four; the walk handle's own call keeps the default and is unchanged.
      The per-engine clock is what does not advance during burn-in, which is
      how the first written interval comes out measured from the window; the
      two event counters DO advance, because `event_order` is their difference
      and a gap there would be visible to an effect reading the event stream.
      Four detectors, one per branch (start alone, end alone, both, end past
      the last event), all four erroring on the abort before this and
      byte-identical after. `NOT_CRAN=true` PASS 8290, FAIL 0, SKIP 5, 54
      baseline and golden rows none skipped; lint 29 -> 28.
- [x] 1.3 Window effects (design D3): each unit's `plan$derivations` realize
      the derived windowed network into the shared object registry
      (deduplicated by derived identity) and its expiry streams into
      `build_joint_schedule()` with the stream index the recipe context assigns;
      lift the `build_walk_engine()` abort. Tests: the `window-list-*`
      windowed fixtures and a tied-time expiry fixture through the merged walk
      equal the recipe loops byte-for-byte; a two-process join windowing the
      same source shares one derived object.
      — done 2026-09-11 (`0bf372a`). Both aborts lifted, the one in
      `build_merged_blocks()` and the one in `build_walk_engine()`. The shared
      source realizes every unit's window derivations, deduplicated on the
      derived name, which IS the derived object's identity since it encodes the
      source and the length. **The fixture caught what reading the code did
      not**: dissolve rows sit one window length AFTER the events they expire,
      so the shared schedule's last row is beyond the last thing that happened,
      and reading the observation window's extent from it extended the
      observation period by the longest window — a trailing right-censored row
      at time 8 on a dataset whose last event is at 6. The recipe path reads
      its extent over the non-window streams; the walk now does too, which is
      where 1.2 and 1.3 meet, and `merged$window_derived` is what lets the walk
      tell an expiry row from a real one.
      The test that pinned the abort MOVED rather than being deleted: it
      asserts the walk runs and the derived object is live in the shared state,
      because a windowed statistic read off a missing layer would be silently
      zero rather than loud. Its snapshot went with it. `NOT_CRAN=true` PASS
      8300, FAIL 0, SKIP 5, 54 baseline and golden rows none skipped.
- [x] 1.4 Restricted opportunity sets and user support constraints on a
      single-process unit through the merged walk: parity tests against the
      recipe loops on the `active_dyad_fold` and support-constraint fixtures
      (both already exist on the joint side; this task proves the single-unit
      entry reaches them).
      **Ordering settled 2026-09-11 by Alvaro (`2517ef0`):
      `constraint-availability-encoding` runs AFTER this task**, not before it.
      The reason answers the objection rather than overruling it: this task's
      parity fixtures are RELATIONAL — merged output against the recipe loop's
      — so they do not name an encoding and survive the later encoding change
      untouched. The encoding literals live in `test-active_dyad_fold.R`, which
      that change owns and rewrites with itself. So write the fixtures here as
      comparisons, never as assertions on `active_dyad_encoding`, or the
      ordering stops being free.
      — done 2026-09-11 (`1e67502`). **No source change was needed.** Both
      restrictions were already wired; the single-unit entry already reached
      them, which is what this task existed to establish rather than to build.
      Four relational fixtures, none naming an encoding. An anti-vacuity guard
      sits beside the opportunity comparison, because two objects that agree
      because NEITHER applied the restriction would pass a parity test on their
      own; the restricted run's folded availability must differ from the plain
      run's. Confirmed live by ablation: disabling
      `fold_active_dyad_opportunity()` fails three of the four.
      Two things the fixtures taught. `opportunities_list` is deprecated, and
      the merged walk still has to reproduce it — a deprecated surface has
      users until it is removed, and a substrate swap must not be what breaks
      them. And `~ !tie(calls)` is unusable on the toy: it repeats the dyad
      1 -> 2, so by the second event the tie exists and the constraint excludes
      the dyad the data observes; both substrates reject it, correctly. The
      fixture uses `~ !tie(net2)`, whose ties the focal layer never observes.
- [x] 1.5 Verification: `NOT_CRAN=true` suite green; baselines PASS; the
      joint test file's byte-identity assertions now cover windowed, bounded,
      writer-varied and constrained single-process fixtures.
      — done 2026-09-11 (`1e67502`). `NOT_CRAN=true` PASS 8307, FAIL 0, SKIP 5,
      54 baseline and golden rows none skipped. The parity file now covers all
      four: writer-varied (gather and db), bounded (four window cases),
      windowed (plain and composed with a bound), and restricted (opportunity
      list and user constraint, each composed with a bound).


**Group 1 re-opened 2026-09-12 by the conformance check
(`.plan/develop/preprocess-one-walk-group1-conformance.md`).** The check's
document half found the aborts lifted, `window_derived` set and read, the
three covariate-body copies in agreement, and 1.4's fixtures relational; its
data half found a divergence no group-1 fixture windows: **a windowed term
read by a TIMED engine is not byte-identical to the recipe loop.** Reproduced
on the parity toy with `rate = ~ 1 + indeg + indeg(calls, window = 2)`: the
merged rate output stores 8 events (`is_dependent` 11111100, `total_time` 7)
where the recipe loop stores 6 (`total_time` 5); the choice side is
identical. Cause: `prepare_recipe_context()` sets `hasEndTime` whenever a
window effect exists, which enables the recipe loop's clip branch, while
`resolve_walk_window()` has no equivalent, so expiry rows past `end_time` are
walked as covariate events and hand timed engines right-censored rows. The
group-1 parity fixture windows only choice-side terms, which is why it saw
nothing. Tasks 1.6-1.9 close it; the unit is not done until 1.9 is green.

- [x] 1.6 Detector first: a rate-side windowed parity fixture, and the same
      on REM (`rate = ~ inertia(calls, window = 2)`), asserting the merged
      output equals `compute_statistics()`'s in stored events, `is_dependent`,
      `intervals`, `total_time` and `end_time`. Both must fail on the current
      tree with exactly the divergence above (8 vs 6 stored events). Keep the
      existing choice-side windowed fixture; it is the control that shows the
      defect is timed-engine specific.
      — done 2026-09-12 (`e057f48`, together with 1.7 so no commit is red).
      Both detectors observed red first on `f045791`: `event_time[4:8]` `4 5
      6 7 8` against `4 5 6`, `is_dependent[4:8]` `1 1 1 0 0` against `1 1
      1`, `total_time` 7 against 5, on the DyNAM rate and the REM rate
      alike; four failures, two per fixture. **The REM fixture carries an
      explicit intercept** (`~ 1 + inertia(calls, window = 2)`): with the
      task's literal formula the comparison fails on a different,
      pre-existing difference — the legacy single-process entry auto-adds
      the REM time intercept (`has_intercept`, `is_exact_time`, the timed
      scalars) while the joint entry keeps the formula as written — which
      would have masked the windowed divergence rather than detected it.
      That intercept difference is recorded, not fixed, here.
- [x] 1.7 Fix: the merged walk's extent is bounded the way the recipe loop's
      is when a window effect exists — expiry rows past the observation end
      are not walked as covariate events and write no right-censored row. Do
      it in `resolve_walk_window()` / the schedule's extent logic, not by
      filtering rows inside the loop, so the walk handle inherits the same
      bound. `merged$window_derived` is what tells an expiry row from a real
      one; the extent read over non-window streams (task 1.3) stays.
      — done 2026-09-12 (`e057f48`). `resolve_walk_window()` takes
      `has_window_effect` and closes the window on it when no end bound is
      given, the branch `prepare_recipe_context()` has always had; the
      real-event extent read moved out of `run_merged_walk()` into
      `resolve_walk_extent(merged, control_preprocessing)`, so the bound is
      a property of the substrate that either loop asks for. Nothing was
      filtered inside the loop: with `has_end` set, the first expiry row
      past the last real event takes the existing clip branch, and the
      clipped interval is zero there, so no right-censored row is written.
      The walk handle does not call the extent helper today (it opens at
      `min(schedule$time)` and computes no end), so it changes nothing; when
      it needs an end bound, `resolve_walk_extent()` is the one place to
      take it from. Detectors green after; the choice-side windowed control
      unchanged. `NOT_CRAN=true` on that tree: 0 failures, 5 skipped (none
      baseline or golden), about 8317 passes by dot count.
- [x] 1.8 The task 1.3 tests that did not land, now with detectors: (a) a
      `window-list` fixture (a window given as a list, per the task text) or
      a recorded reason it is not a shape the parser accepts; (b) a dedicated
      tied-time expiry fixture, an expiry at the same time as a dependent
      event, asserting the recipe loop's order; (c) "two processes windowing
      the same source" on a REAL two-process join, not
      `single_process_joint()`, asserting one derived stream and one set of
      expiry rows; (d) an assertion on the schedule's ordering rule
      (`build_joint_schedule()`'s `(time, stream_index)`, dependent streams
      before their own covariate stream), since the task text says "the
      stream index the recipe context assigns" and that is not what the
      builder does — assert what it does. Also rewrite the stale comment in
      `R/walk_handle.R` ("no window effects, no explicit window bounds") to
      what the walk now supports.
      — done 2026-09-12 (`4ffd037`). (a) The `window-list-*` name is the
      living spec's `window-list-network-effects`: a windowed effect whose
      networks are given as `list(net1, net2)`, one derivation per member,
      not a window given as a list. The fixture is
      `mixed_trans(list(calls, net2), window = 2)`; it asserts both derived
      names and full parity for both families. Ablation: realizing only the
      first derivation per unit errors on the missing `net2_2` layer. (b)
      The tied-time fixture captures the recipe loop's own schedule (mocking
      `build_event_schedule()` the way `parity_plan()` mocks `preprocess()`)
      and compares it row by row with the merged schedule, then names the
      order at each of the four ties (dependent, own creation, expiry).
      Ablation: a reversed tie-break fails nine of its expectations. (c)
      Rebuilt on a real two-process join (`parity_two_process_windowed()`,
      `friendship` event-observed because a window on a panel layer is
      rejected): one registry entry, one stream index, creation and expiry
      rows once per source event; the calls process, whose events set the
      shared clock's extent, equals its standalone output, and the emails
      process is deliberately not compared — the shared clock and
      cross-process right-censored rows are the joint contract. Ablation:
      dropping the stream dedup fails four expectations. (d) Asserts what
      the builder does: `order(time, stream_index)` is the identity and each
      focal's dependent index sits below its covariate index. Ablation:
      adding covariate streams before the dependent stream fails it. The
      walk-handle comment now says the merged walk hosts both. `NOT_CRAN=true`
      PASS 8337, FAIL 0, SKIP 5, none baseline or golden.
- [x] 1.9 Verification: `NOT_CRAN=true` suite green on the documented runner
      (`load_all()` + `test_dir()`; `devtools::test()` carries a pre-existing
      unrelated snapshot failure), baselines PASS not SKIP; the two
      detectors from 1.6 green; the walking-time gate re-read on the landed
      tree (rate-only sweep and cm10k), within 3 percent of the unit-1
      reference in `.plan/develop/coordination.md`. Task 1.1's documented
      test deviations (db compared to merged gather, `output = "data.frame"`
      not exercised, rendered-output equality deferred) stay with task 3.1.
      — suite half done 2026-09-12 on `4ffd037` with the documented runner:
      PASS 8337, FAIL 0, SKIP 5 (autograph class names, `estimate_dynes()`,
      `simulate()`), no baseline or golden row skipped; both 1.6 detectors
      green. **The walking-time gate is pending**: it is read by the
      coordinator against the unit-1 reference in
      `.plan/develop/coordination.md`, not by the applying session.

## 2. One compile stage

**Unit 1b conformance (2026-09-12, `.plan/develop/preprocess-one-walk-unit1b-conformance.md`):
conformant, one comment to fix, carried as 2.0 so group 2's agent lands it
first.** Group 2's agent also reads the report's note on the walk handle: it
computes no end extent and steps expiry rows past the last real event without
writing rows (identity recorder, no right-censoring consumers), which is the
state group 2 must not change.

- [x] 2.0 Rewrite the comment at `R/walk_handle.R` (around lines 321-327) so
      it says what the handle does: no explicit window bounds are read
      (`walk_open()` reads only `control_preprocessing$impute`), the walk
      opens at the schedule's first time and computes no end extent, and
      expiry rows past the last real event are stepped but write nothing.
      Task 1.8's rewrite overstated it ("supports that class"). Comment only;
      no code.
      — done, committed before this session at `1abfa85`; box ticked here.
- [x] 2.1 Extract the compile stage of `estimate_wrapper()` (parse, effects,
      links, `new_model_spec()`, `build_spec_map()`, imputation policy stamp)
      into one internal `compile_spec_map()`; `compile_recipe_spec_map()`
      becomes a call to it with the joint path's per-focal working copy as a
      parameter; `parsed_formula` reuse stays a parameter (design D2).
      Roxygen for the internal function; `devtools::document()`.
      — done 2026-09-12 (`1c482ba`). `compile_spec_map()` +
      `compile_model_terms()` (the terms stage) + `legacy_sub_model_of()`;
      `compile_recipe_spec_map()` is a thin call; `preprocess_recipe()` now
      runs a compiled map; the wrapper branches early (recipe → compile_spec_map,
      else → compile_model_terms). Detector (plain + windowed) red-then-green on
      the `model_spec` attribute, the one seam — every list field, windowed
      derivations included, already agreed.
- [x] 2.2 `build_merged_blocks()` accepts pre-compiled units: the joint path
      compiles per process with `compile_spec_map()` and hands units in; a
      one-unit entry wraps a single compiled `spec_map` (with its consumer
      specs for a flavored plan) for the single/flavored callers.
      — done 2026-09-12 (`fb497b7`). `compile_process_unit()` split into compile
      + `assemble_process_unit()`; `build_merged_blocks(..., units = NULL)`;
      `preprocess_one_unit(spec, family, spec_map, ...)`. Detectors: pre-compiled
      units reach the same substrate; the entry equals the joint path on a plain
      and a flavored spec.
- [x] 2.3 Verification: `NOT_CRAN=true` suite green; baselines PASS; the joint
      tests' compile assertions (one spec_map per process, grouped by block)
      unchanged.
      — done 2026-09-12. Documented runner (`load_all()` + `test_dir()`):
      PASS 9663, FAIL 0, ERROR 0, SKIP 5 (the known autograph/`estimate_dynes()`
      /`simulate()`/pinned-rate/second-consumer skips); baseline+golden 160 rows
      none skipped. The grouped-block compile test is unchanged and green.

## 3. Dispatch flip and deletion — OPEN (approved 2026-09-11)

**Approved 2026-09-11 by Alvaro**, on task 0.10's re-read: CollegeMsg full at
1.007 and 1.002 against the 1.10 rule, every CollegeMsg cell passing at both
sizes with and without a constraint, ratio reproducing to 2.3 percent worst
case over two sweeps of five warm runs on a quiet branch at `d049329`. Both
halves of D7 are now satisfied — the number passes and the approval is
recorded, rather than the second being read off the first. The history below is
kept because it is why the group was shut twice and what reopened it.

**Closed as of 2026-09-09.** The gate came back 0.31x, but the ratio counts
per-event matrix copies rather than architecture (design D9), so this group does
not open on it.

**Second reason to stay closed (added 2026-09-10).** Task 0.9's re-run put the
unconstrained ratio at 0.99 and the CONSTRAINED one at 1.16, but that 1.16
measures the support-mask maintainer, not the substrate: a constrained model
spends 362 s of a 362.4 s cell inside it, and both substrates build two atom
maintainers rather than one. Reading group 3 on a constrained cell today would
repeat exactly the error ADR-0057 exists to record. The constrained cell becomes
a decision input only after `support-mask-sparse-updates` lands. It opens when tasks 0.4a-0.4e, 0.5a, 0.5b and 0.6 have landed
and task 0.9 has re-run the gate on the fixed tree with the 10k-event CollegeMsg
subset.
If the re-run puts the merged walk within 1.10x this group proceeds as written;
if not it stays closed, and task 4.1's NEWS.d fragment records the merged walk's
new parity and why the loops stay.

**Deleting the recipe loops needs Alvaro's explicit approval**, separately from
the measurement passing. A passing ratio is a necessary condition, not the
decision: the loops are what the frozen 1e-6 baselines have always run through,
and the whole suite moves with them. Task 3.3 does not start on a green number
alone.


- [x] 3.0a Decide the REM intercept default at the flip (found 2026-09-12 by
      the unit 1b conformance check, pre-existing and window-independent).
      `compute_statistics(spec, "REM", "rate")` on `rate = ~ inertia` reports
      `has_intercept` / `is_exact_time` TRUE with the timed scalars, because
      `estimate_wrapper()` raises the flag for a REM rate model; the joint
      entry keeps the formula as written (`make_specification()` only lowers
      the flag, the process map copies it, the merged consumer reads the map),
      so `preprocess_joint(single_process_joint(spec))` reports FALSE with no
      scalars. The living spec `model-recipe-dispatch` (`estimate_rem`
      sub_model `rate` = full dyadic hazard with an intercept) sides with the
      legacy entry, so the flipped dispatch must raise the flag where the
      wrapper did, and 3.0's goldens will show it on every REM rate baseline
      that lacks an explicit `1`. Detector first: the unit 1b REM parity
      fixture with the `1` removed must go red today and green after.
- [x] 3.0 Freeze the recipe loops' output as serialized goldens BEFORE 3.1, not
      before 3.3. `test-preprocess_parity.R` compares
      `preprocess_joint(single_process_joint(spec))` against
      `compute_statistics(spec, ...)`, and 3.1 routes `compute_statistics()` to
      the merged walk, so at that moment all seventeen assertions compare the
      merged walk with itself — silently, with the suite green, one step before
      the deletion anyone would think to guard. Capture each parity fixture's
      preprocessed object from the loops while they are still reachable, store
      it under `tests/testthat/_fixtures/`, and point one side of each assertion
      at the file. The frozen 1e-6 coefficient baselines do not cover this: they
      catch numeric drift in estimates, not a statistics object that changed
      shape. This golden set is also what makes a later resurrection checkable,
      since recovered loop code reads the contracts of its own era.
- [x] 3.1 `preprocess.goldfishKind()` routes every `input_shape = "standard"`
      spec to the merged walk through the one-unit entry (design D1): a
      single-process call unwraps fid 1; a flavored plan returns the fid-keyed
      list with `process_map`. `preprocess_recipe()` passes the compiled
      `spec_map` and consumer specs through unchanged. The grouped branch is
      untouched. Full `NOT_CRAN=true` suite: baselines PASS. This is the
      bisectable commit.
- [x] 3.2 `preprocess_flavored()` drops its per-family loop and calls the
      merged walk once for all families; its union planning, consumer specs,
      `validate_prep_support()` stamping and `process_map` contract stay.
      Tests: the flavored fixtures and the `estimate_flavored()` container
      tests unchanged.
- [x] 3.3 **Alvaro's approval required before this task starts** (see the group
      header): a passing ratio is necessary, not sufficient.
      Delete `run_sender_recipe_loop()`, `run_dyad_recipe_loop()` and the
      helpers only they called (inventory from task 0.2); keep the finalizer
      helpers the merged walk shares. `air format` the touched files, then
      `lintr::lint()` on them. The descriptor spec's guard test and
      `grep -n "eval(parse\|assign(" R/model_preprocess.R` stay clean.
- [x] 3.4 Re-run the task 0.1 measurement on the flipped tree and record it
      beside the pre-flip numbers; confirm the ratio is within the recorded
      rule.
- [x] 3.5 Verification: full `NOT_CRAN=true` suite green; frozen baselines and
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
- [ ] 4.5 Sweep this change's own OpenSpec references out of source, by hand,
      **including `R/walk_handle.R` (lines 38, 54, 579 carry `(D8a)` / `(D9)`;
      missed by the first inventory, found 2026-09-12),**
      one site at a time — the standing rule is that a decision id or task
      number in code is a dangling pointer once the change is archived, and
      these are also **stale in content**, which is the sharper reason. Six
      comments in `tests/testthat/test-preprocess_parity.R` read "FAILS until
      task 0.4b / 0.4d / 0.5b"; all three tasks are done and all six tests
      pass, so the comments now assert the opposite of the truth. There are no
      `skip_if()` gates on them, only prose. Comment the reasoning itself — the
      defect each fixture was written to catch and why it could not be seen
      before — so the fixture still explains itself to a reader who cannot open
      the change. Do not delete the comments; rewrite them.

      Also in this change's files: `R/preprocess_joint.R` carries three
      `design D` references (line 698's `design D5 / D8a` among them),
      `R/preprocess_multivariate.R` one, `test-preprocess_flavored.R` one.

      **Residual, NOT this change's to fix, recorded so it is not lost**:
      roughly eight more sites belong to other archived changes —
      `R/joint_parameters.R`, `R/intercept_only_rate.R`,
      `test-model_evaluate.R`, `test-model_terms.R`,
      `test-diagnostic_primitives.R`, `test-stale_result_detection.R`,
      `test-flavored_dependent_events.R`, `test-event_reductions.R`. Whoever
      next opens one of those files sweeps that file, per the same rule. A
      package-wide sweep in one commit would bury a real diff in noise, which
      is why this is not that.
