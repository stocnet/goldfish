**Test-driven, and the order is load-bearing.** Task 0.6 writes every parity
fixture before 0.4a-0.4d, 0.5b or group 1 touches code. Each fixture must fail
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
- [ ] 0.2 Work on **`feature_simulation`**, alongside `process-simulation`; no
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
- [ ] 0.3 Verification: `NOT_CRAN=true` suite green on the branch start;
      frozen baselines and C++ goldens PASS not SKIP.
- [ ] 0.4a Remove the copy at the two FOLD sites (design D10, low risk).
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
- [ ] 0.4b Clear the aliases the state write would expose (design D10). An
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
- [ ] 0.4c Remove the copy at the two STATE sites (design D10). Both recipe
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
- [ ] 0.4d Make the merged walk compute an unweighted degree unweighted
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
- [ ] 0.5a Store the support mask by its axis-union kind (design D11).
      `preprocess_support_mask()` returns one dense n1 x n2 logical per snapshot
      time; a separable constraint is a length-n1 or length-n2 vector, or a
      scalar. `active_dyad_encoding_decide()` already classifies point / alter /
      ego / scalar and `test-active_dyad_fold.R` already asserts the alter fold
      allocates no dense matrix, so this is storage only. Tests: existing
      support-constraint fixtures unchanged; a constrained model on a
      1899-actor node set preprocesses without exhausting memory.
- [ ] 0.5b Register a windowed constraint atom's derived object in
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
- [ ] 0.6 Parity fixtures, written BEFORE any of 0.4a-0.4d, 0.5b or group 1
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
      aliasing site, so 0.4b's regression test rides on it.

      Constraint on all five: parity fixtures, not baselines. They must not
      touch the frozen 1e-6 set. Windowed terms need a no-window variant for the
      merged walk until task 1.3 lands.
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
- [ ] 0.9 Re-run the gate and record the contrast (design D9). The pre-fix
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
not open on it. It opens when tasks 0.4a-0.4d, 0.5a, 0.5b and 0.6 have landed and task
0.9 has re-run the gate on the fixed tree with the 10k-event CollegeMsg subset.
If the re-run puts the merged walk within 1.10x this group proceeds as written;
if not it stays closed, and task 4.1's NEWS.d fragment records the merged walk's
new parity and why the loops stay.

**Deleting the recipe loops needs Alvaro's explicit approval**, separately from
the measurement passing. A passing ratio is a necessary condition, not the
decision: the loops are what the frozen 1e-6 baselines have always run through,
and the whole suite moves with them. Task 3.3 does not start on a green number
alone.


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
- [ ] 3.3 **Alvaro's approval required before this task starts** (see the group
      header): a passing ratio is necessary, not sufficient.
      Delete `run_sender_recipe_loop()`, `run_dyad_recipe_loop()` and the
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
