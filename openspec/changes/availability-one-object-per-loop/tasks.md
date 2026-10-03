## 0. Gates and references (D6)

- [x] 0.1 Confirm `constraint-objects-on-shared-walk` groups 0-4 are landed on
      `feature_simulation` (its 4.5b effect and 4.6 retirement are the
      producer this change reshapes) and the tree is clean; `NOT_CRAN=true`
      suite green (not-cran-test; six baseline files and the C++ goldens
      PASS, not SKIP). Record the counts in `progress.md`.
- [x] 0.2 Re-capture every byte-identity reference family
      (`helper-constraint-atoms.R`, including the joint and flavored
      families) and the parity goldens with today's field list, and add a
      second comparison restricted to the fields this change keeps
      (`active_*_init`, `active_*_update`, `active_*_update_pointer`,
      `active_dyad_encoding`, the mask stream, statistics, events, scalars).
      Test: both comparisons PASS on the unfixed tree.
- [x] 0.3 Add a reference family for a vector-kind REM constraint
      (`~ alter(attr) > 0` with receiver composition changes) and a
      vector-kind coordination constraint on `constraint_atoms_coord_data()`,
      captured on today's point fold: the dense cell values at every stored
      row (from the point grid) plus the estimates on r, cpp and gather.
      These are the oracle D3's outer fold must equal cell-for-cell.
      *Note (added 2026-10-03, 6.5):* no ego coordination family was
      captured. Coordination symmetrises a vector mask m to m[i] & m[j],
      so an ego and an alter atom on the same attribute give the same
      cells and the same fold into both factors; the `coord_alter` family
      covers both. The REM families carry alter, ego and global.
- [x] 0.4 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS not SKIP.

## 1. Kernels take their loop's object (D1)

- [x] 1.1 `src/DyNAM_rate_default.cpp`, `src/DyNAM_rate_ordered_default.cpp`:
      remove the `active_dyad_init`, `active_dyad_update`,
      `active_dyad_update_pointer` arguments and their update walk;
      cpp-recompile (verify the `RcppExports.*` diff shows the three
      arguments gone on both kernels); `make_engine_evaluator()` and the
      rate gather dispatch stop passing them. Test: C++ goldens and the rate
      baselines PASS; a snapshot of `formals(estimate_DyNAM_rate)` names no
      `active_dyad` argument.
- [x] 1.2 `src/REM_default.cpp`, `src/REM_ordered_default.cpp`,
      `src/DyNAM_MM_default.cpp`: rename the sender-axis arguments to
      `active_dyad_sender_init` / `_update` / `_update_pointer` (the outer
      encoding's first factor), read them only when `!active_dyad_is_point`,
      and accept a zero-length factor at point; cpp-recompile per kernel;
      `make_engine_evaluator()` passes zero-length buffers at point. Test:
      goldens PASS; a constrained REM, REM-ordered and coordination fit at
      point equals the pre-change fit to 1e-12 on cpp (reference captured in
      0.2).
- [x] 1.3 `R/estimation_core.R`: drop `folded_full`; a point-encoded dyad
      object means no sender reduction and the grid as the mask, an
      outer-encoded one the two factors, and the statistics-array sender
      reduction runs only for the sender loop. `R/cpp_interface.R` gather
      loops: `present1_ids` from the sender factor at outer, from the grid's
      rows at point. Test: r = cpp = gather on the constrained REM,
      coordination and rate fixtures (`test-support_constraint_rem.R`,
      `_coordination.R`, `_rate.R`) within their existing tolerances.
- [x] 1.4 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS; both 0.2 comparisons PASS; `devtools::document()` for the
      regenerated bindings.

## 2. Availability stored once, as buffers (D2)

- [x] 2.1 Cost guard, recorded failing first: on a composition-change fixture
      with `C` changes (Fisheries, 62; and a generated one with 10x), the R
      backend's allocation for one `compute_step.default` call on an
      unconstrained choice and rate does not grow with `C`. Measure first;
      if the `compChange*` filter's per-event allocation does not separate
      from noise at 10x, the guard is the absence of the data-frame filter
      (no `compChange` binding in the engine context) and `progress.md`
      says so.
- [x] 2.2 Unit test, written before the deletion: over a sequence with a
      composition change stamped at a dependent event's own time, the
      pointer-buffer walk and the `<= t` data-frame filter give the same
      presence at every stored row (the writer's rule: a change lands on the
      next stored row). Record which rule the buffer encodes in the test.
- [x] 2.3 `R/preprocess_writers.R`: the writers emit `active_*_init`,
      `active_*_update`, `active_*_update_pointer` only (no
      `active_*_changes`); `R/dynami_bridge.R`, `R/methods_display.R`'s
      field list, `R/preprocess_joint.R`'s `finish_output()` follow.
      `R/estimation_core.R`: `estimate_int()` builds no `compChange*`
      frame; `compute_step.default` applies every axis through
      `.availability_event_cols()`; the `active_*_folded` flags lose their
      reader and go. Test: Fisheries baselines on r PASS; the 2.1 guard
      passes; `names(prep)` snapshot carries no `_changes` field.
- [x] 2.4 `R/availability_effect.R` `availability_finish()`: write the
      stream and the encoding only; no `sender_presence_*` /
      `receiver_presence_*` on `support_mask`. Move any test reading a stash
      (`test-support_mask_sparse.R`, `test-support_mask_maintain.R`,
      `helper-parity-fixtures.R:492`) to the mask stream or to the effect.
      Test: the kept-field comparison of 0.2 PASS for every family; a
      `names(prep$support_mask)` snapshot shows the stream fields only.
- [x] 2.5 Stale-object test: a preprocessed object saved before 2.3 (fixture
      under `_fixtures/`) estimates on every backend with the retired fields
      ignored; an object missing `active_sender_update_pointer` is rejected
      with the outdated-format error. No format version bump (design
      Context).
- [x] 2.6 Measure walk time, allocated memory and preprocessed object size on
      the CollegeMsg budget fixture (unconstrained and constrained) and on
      Fisheries before 2.3 and after 2.4 against the design's prediction;
      update `tests/testthat/_budgets/walk_cost.csv` if the floor fails,
      saying in the commit what moved. Numbers in `progress.md`.
- [x] 2.7 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS; `devtools::document()` if any roxygen moved.

## 3. REM and coordination at the outer encoding (D3)

- [x] 3.1 Cost guard, recorded failing first: on a 1500-actor generated REM
      fixture (`constraint_cost_data()` shape) with `~ alter(attr) > 0` and
      receiver composition changes, `lobstr::obj_size()` of the preprocessed
      object is under 4 x `(n1 + n2)` x 4 bytes above the unconstrained
      object, and `bench::bench_memory()` of the walk shows no allocation of
      `n1 * n2` logicals or doubles. Record the failing numbers (today one
      grid plus columns).
- [x] 3.2 `R/preprocess_joint.R` `build_engine_availability()`: a constrained
      REM or coordination fid takes `active_dyad_encoding_decide()`'s
      verdict; point for a point-kind atom or an opportunity list, outer
      otherwise; coordination with `n1 != n2` falls back to point (asserted).
      `R/availability_effect.R`: the outer step folds an alter-kind (1) or
      global (3) mask into the receiver factor and an ego-kind (2) mask into
      the sender factor through `write_entries()`; `symmetric` folds a vector
      mask into both factors; the count cache is the product of the factor
      sums. Test: on the 0.3 references every cell of the outer fold equals
      the point fold's cell at every stored row (`f1[i] & f2[j]`, read
      through `active_dyad_cell()`), including a global atom.
      *Note (added 2026-10-03, 6.5):* the coordination `n1 != n2` point
      fallback is in `build_engine_availability()`
      (`R/preprocess_joint.R`, `(!symmetric || ctx$n1 == ctx$n2)`) but is
      not asserted: it cannot be reached, since the estimator refuses
      `choice_coordination` on a two-mode layer before preprocessing
      (`R/model_estimate.R`), so no fixture can build the case. What let
      the 3.1 walk guard pass, beside the outer variant, is
      `init_at_kind()` (`R/model_preprocess.R`, called from
      `initialize_cache_stat()` for the constraint atom store): a
      vector-kind atom (`alter`, `ego`, `global`, `indeg`, `outdeg`) is
      seeded at its kind instead of through its dense grid init, with the
      grid path kept as the fallback when the seeded length does not match
      the kind. It was `identical()` to the grid path on 27 atom x family
      combinations (progress journal). No artifact of this change
      describes it; its contract moves to `initial-stats-at-kind` 2.2a.
- [x] 3.3 Estimates: the 0.3 families on r, cpp and gather equal the
      point-fold references within 1e-10; `walk_replay_against_batch()` on
      the vector-kind REM and coordination fixtures with `n_excluded > 0`
      after 4.1. Re-save the 0.3 references in their outer form once equal;
      every other entry `identical()`.
- [x] 3.4 Measure walk time, allocation and object size on the 3.1 fixture
      before 3.2 and after 3.3 against the design's prediction (one grid
      down); numbers in `progress.md`. NEWS.d bullet: a vector-kind
      constraint on REM or coordination no longer allocates a dyad grid.
- [x] 3.5 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS; 3.1 guard passes.

## 4. Handle and simulation read the effect (D4, D5)

- [x] 4.1 `R/walk_handle.R` `walk_risk_set()`: an outer-encoded REM or
      coordination fid returns `encoding = "outer"` with the effect's two
      factors; a choice fid keeps the receiver factor with `encoding =
      "alter"` (D4). Update the living scenario's test for the ego-kind
      choice constraint (`test-support_constraint_ego_fold.R`): estimates
      equal the unconstrained fit when every observed sender is allowed; a
      gated-out observed sender fails fast. Test: every replay fixture
      equal before and after; the new outer fixtures replay.
- [x] 4.2 Cost guard, recorded failing first: one deferred-fid evaluation in
      the simulation driver on a constrained REM (point and outer) allocates
      less than one `n1 x n2` logical grid at 1500 actors.
      *Note (amended 2026-10-03, 6.1):* the deferred-fid path serves DyNAM
      only; a REM process is one rate fid with no sibling to defer to, so
      the constrained-REM case cannot occur. The guard runs a rate-only
      DyNAM spec on `constraint_cost_data()` (1500 actors) completed for
      simulation, its choice deferred, under a point (`~ !tie(emails)`) and
      an alter (`~ indeg(emails) < 3`) constraint. Recorded failing: one
      `simulation_fid_state()` call 34.55 / 34.33 MB (about four logical
      grids); after 4.3: 0.23 / 0.01 MB.
- [x] 4.3 `R/simulate_driver.R` deferred-fid path: read the fid's maintained
      availability (step the effect to the cursor, return its value and
      encoding; the buffer, not a copy) instead of `matrix(present2, ...) &
      support_to_grid(...)`. Test: the 4.2 guard passes; a seeded simulation
      from a constrained REM fit draws the same sequence before and after;
      the free-running and time-anchored simulation tests stay green.
      *Note (deviation, amended 2026-10-03, 6.1; design D5 "As landed"):*
      a deferred fid has no availability effect of its own to step (its
      family carries no effect, so it has no engine; the sibling's effect
      is the other loop's object). The deferred choice instead reads the
      drawing sender's row of the sibling's live mask at the mask's kind
      (`support_row()`, linear index) AND receiver presence, at the alter
      encoding; the deferred rate reads the sender gate off the same mask
      (`sender_gate_from_mask()`) AND sender presence. No grid is composed
      per call. The seeded before/after sequence check (point, alter and
      ego constraints, 40 events each, identical draws on the pre-4.3
      worktree and after) was run by hand and is not committed: the "before"
      side needs the pre-4.3 code, and a sequence pinned on today's code
      alone would freeze draws rather than test the read; the cost is held
      by the 4.2 guard and the support by the deferred-rate simulation
      tests in `test-support_gate_self_exclusion.R`.
- [x] 4.4 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS; every reference family PASS.

- [x] 4.5 Added in apply (Alvaro, 2026-10-01, from goldfish-0c's review): a
      dyad-loop output stores no raw sender presence. A choice output keeps
      no `active_sender_*` (its loop never folds the sender), nor does a
      point-encoded REM or coordination output; an outer-encoded one keeps
      them as the object's own sender factor. Readers rerouted
      (`model_estimate.R`'s sender count, the R engine's sender filter and
      row accessor, the gather paths); tests that used the field as a
      presence oracle move to an independent source. Test: numbers
      unchanged (baselines, goldens, backend parity); the retired-field
      assertion of 2.3 extends to `active_sender_*` on choice and point
      outputs.
- [x] 4.6 Added in apply (Alvaro, 2026-10-01): the observed-event check
      allocates no per-event row. `n_allowed` comes from the count cache,
      the observed receiver is one cell read at the mask's kind, and the
      candidate set is written only when a candidate can be new. Guard,
      recorded failing first: on the 3.1 fixture the constrained walk's
      allocation exceeds the unconstrained one by less than the n2-length
      rows of its dependent events would take (today 69 MB of rows).

## 5. Close

- [x] 5.1 `bash .plan/opsx-spec-placement-check.sh
      availability-one-object-per-loop` (the `active-availability-stat` and
      `support-constraint` blocks name living requirements, so a clean exit
      is expected) and `openspec validate availability-one-object-per-loop
      --strict` clean.
- [x] 5.2 `NEWS.d/availability-one-object-per-loop--one-object-per-loop.md`
      (developer-facing: engines take one availability object per loop; the
      preprocessed object stores composition changes once and stashes no raw
      presence; REM and coordination vector-kind constraints at outer). No
      `NEWS.md` or Version edit on the branch.
- [x] 5.3 Vault: ADR-0109 `spec:` and open questions current; dashboard
      fragment in `_dashboard-inbox/`; session note closed; hand-off notes
      to the estimation-side cost change (the kernels' per-event `allowed`
      vector, the R backend's list-held grid copy) in its proposal once it
      exists, else in this change's `progress.md`.
- [x] 5.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      and goldens PASS not SKIP; `status: landed (feature_simulation,
      awaiting fold)` in `proposal.md`.

## 6. Conformance follow-up (added 2026-10-02)

From `.plan/develop/availability-one-object-per-loop-conformance.md` and its
`-run2.md`. Two independent inspections: the code and the stored objects match
D1-D4, every cost prediction is met and every guard goes red under ablation.
The gaps are artifact drift the fold would carry into the living spec, plus
one latent contract hole. No blocking gap; all before archive.


- [x] 6.1 **should-fix**: D5 landed as a read of the sibling process's live
      mask row at its kind (`R/simulate_driver.R:333-391`, `support_row()`
      and presence), not "the fid's maintained availability"; a REM
      deferral does not exist. Amend design D5, the delta scenario "the
      simulation reads the maintained object", and the 4.2/4.3 tick notes
      so the deviation is in tracked artifacts, not only `progress.md`.
      Optionally commit the seeded before/after sequence check 4.3 ran by
      hand.
      *Done 2026-10-03:* design D5 gains an "As landed" paragraph (no
      effect to step for a deferred fid; DyNAM-only path; the sibling's
      live mask row at its kind plus presence; guard numbers), and the D5
      storage paragraph and the Guards line say what was measured. The
      delta scenario keeps its name and splits its WHEN: the walk handle
      reads the fid's maintained availability at its encoding; the
      driver's deferred fid reads its sibling's live mask at kind and
      presence; no grid at either call. 4.2 and 4.3 carry notes. The
      optional sequence check is not committed (reason in 4.3's note).
      Docs only: `openspec validate --strict` and the placement check
      clean.
- [x] 6.2 **should-fix**: the ego-choice scenario in
      `specs/active-availability-stat/spec.md` names the
      "observed-event-excluded" error; the code raises and the test asserts
      "empty risk set" (`R/availability_effect.R:1121`,
      `test-support_constraint_ego_fold.R:182`). Align the scenario.
      *Done 2026-10-03:* the scenario "ego constraint in choice zeroes
      rows, validation guards observed events" now names the empty-risk-set
      error and why: an ego atom zeroes the gated sender's count of allowed
      receivers, so `availability_check_event()` fails on the count before
      it reads the observed cell. Matches the code and
      `test-support_constraint_ego_fold.R` ("empty risk set"). Docs only:
      `openspec validate --strict` and the placement check clean.
- [x] 6.3 **should-fix**: since 4.5 an ego-kind choice output carries
      `active_dyad_encoding = "outer"` with no sender factor;
      `walk_risk_set()` relabels it `"alter"` and the accessors special-case
      a NULL sender. Store it as `"alter"` or state in the encodings
      requirement that a choice object at outer carries one factor.
      *Done 2026-10-03, second option (spec states it):* the encodings
      requirement now says a one-sided choice object at outer (an ego-kind
      atom, which the choice loop does not fold) carries the receiver
      factor alone, no sender factor and no `active_sender_*`; the row and
      cell accessors read it as that factor for any sender, and the walk
      handle hands it on at the alter encoding. The ego-choice scenario's
      "stored at the receiver axis's own encoding" now says the outer
      encoding with the receiver factor alone, and design D4 gains an "as
      landed" note. Why not relabel the stored encoding `"alter"`: it
      changes a stored field value of every ego-constrained choice object
      and the `test-support_constraint_ego_fold.R` expectation
      (`"outer"`), for no change in any number, while the wording change
      moves nothing stored; and the decision function
      (`active_dyad_encoding_decide()`) returns `"outer"` for an ego atom
      whatever the loop, so a relabel would need a choice-only exception
      there too. Also rewrote that test's comment, which still called the
      object two factor vectors. Not touched: `active_dyad_count()` at
      outer with no sender factor returns 0 (`sum(NULL == 1)`); nothing
      calls it for a choice object today. Suite (`NOT_CRAN=true`):
      PASS 13184, FAIL 0, ERROR 0, SKIP 7; six baseline files PASS with 0
      skipped. `openspec validate --strict` and the placement check clean.
- [x] 6.4 **should-fix**: comments now false, rewrite by hand:
      `R/estimation_core.R:2111-2120` and `R/walk_handle.R:1284-1286` say a
      REM or coordination constraint always folds to a dense point grid.
      *Done 2026-10-03:* rewritten by hand in `compute_iteration_step()`
      (the REM and the coordination blocks) and above `walk_risk_set()`:
      a point-kind atom (or an opportunity list) gives the dense point
      grid, consumed whole; an alter-, ego- or global-kind one gives the
      outer encoding's two factors, which reduce senders and receivers like
      presence, with coordination's vector mask m folded into both factors
      as m[i] & m[j]. The comment under `if (ctx$dyad_point)` was already
      accurate and is untouched. Comments only; `air format` and `lintr`
      clean on the changed lines. Suite (`NOT_CRAN=true`): PASS 13184,
      FAIL 0, ERROR 0, SKIP 7; six baseline files PASS with 0 skipped.
- [x] 6.5 **should-fix**, record in the 3.2 and 0.3 tick notes: the
      coordination `n1 != n2` point fallback (`R/preprocess_joint.R:1435`)
      is unasserted because unreachable; the ego coordination reference
      family was omitted because symmetrisation makes ego and alter the
      same fold; `init_at_kind()` (`R/model_preprocess.R:653`) seeds
      vector-kind atoms at kind and is what lets the 3.1 walk guard pass,
      described in no artifact (its contract moves to
      `initial-stats-at-kind` 2.2a).
      *Done 2026-10-03:* notes added under 0.3 (the omitted ego
      coordination family) and 3.2 (the unreachable fallback and
      `init_at_kind()`). Docs only: `openspec validate --strict` clean.
- [x] 6.6 **should-fix**: the carried scenario "stale preprocessed object
      rejected" reads against the delta's own "retired fields read and
      ignored, no bump" clause; reword to the rejection the code performs
      (a missing engine field).
      *Done 2026-10-03:* the scenario keeps its name; its WHEN is now an
      older format stamp, or an axis's composition changes held only as the
      retired per-change list with no update pointer
      (`preprocessed_format_status()` / `lacks_availability_buffers()` in
      `R/format_version.R`), and its THEN adds that an object carrying the
      list beside the buffers is read with the list ignored (the 2.5
      fixture). Asserted by `test-stale_result_detection.R` ("composition
      changes held only as a list mark the object outdated" and the
      pre-buffer object estimating on every backend). Docs only:
      `openspec validate --strict` and the placement check clean.
- [x] 6.6a **should-fix** (added 2026-10-03, from the
      `support-gate-self-exclusion` follow-up re-check,
      `.plan/develop/support-gate-self-exclusion-followup-conformance.md`):
      the coordination self-exclusion in `symmetric_receiver_count()`
      (`R/availability_effect.R:427`, from `7e57527`) has no detector;
      ablating it leaves the self-exclusion, coordination,
      shared-availability and constrained-capability test files green. Add
      a coordination fixture with a self-only sender. Verify: the ablation
      is red.
      *Done 2026-10-03:* `test-support_gate_self_exclusion.R` "a
      coordination self-only sender meets an empty risk set": a four-actor
      undirected stocnet, `choice_sub_model = "choice_coordination"`,
      `~ alter(x) > 0`, stored at outer with the symmetric flag. Case 1,
      only N1 has `x > 0` and N1 sends the first event: the check must fail
      on the count with "empty risk set at event 1". Case 2, N1 and N2
      allowed: each reaches only the other, so both events are forced
      choices ("2 events with a single candidate"). The error is captured
      with `expect_error()` and matched apart so a wrong error fails an
      expectation, not the test, and case 2 still runs. Red under the
      single-site ablation `own <- NULL` (copy and restore, checked with
      `cmp`; never committed): FAIL 2 in this test (case 1 gets "the
      observed dyad is excluded", case 2 loses the forced-choice warning),
      file PASS 100 / FAIL 2; the coordination, shared-availability and
      constrained-capability files stay green (11 / 6 / 13), as the report
      found. Clean: file PASS 102 / FAIL 0. Suite (`NOT_CRAN=true`):
      PASS 13184, FAIL 0, ERROR 0, SKIP 7 (environmental or opt-in); six
      baseline files PASS with 0 skipped. No R code changed.
- [x] 6.6b **should-fix** (added 2026-10-03, Alvaro; from the
      `constraint-objects-on-shared-walk` groups 0-4 inspection,
      `.plan/develop/constraint-objects-on-shared-walk-groups0-4-conformance.md`
      gap 4, its task 8.4): the unconstrained exact-time rate walk is 20-33
      percent slower since the availability rows joined the shared schedule
      (1500-actor generated fixture, 3000 calls + 3000 emails: 0.117 s at
      `663096d`, 0.141-0.156 s at `0a99c74`, 0.143 s at `3cb92d9`), about
      two thirds of it `availability_step()` stepping the presence effect in
      R on every stored row; CollegeMsg unconstrained joint walk 3.65 to
      4.01 s. Add the R fast path for availability rows with nothing to
      apply (62 percent of steps on CollegeMsg 10k), and remove the per-row
      `c()` growth and by-name `[[fid]]` lookups in the same step code
      (walk-cost rules 4 and 5). Decide whether a C++ step is still worth it
      from the measured result; do not build it in this task. Cost guard
      first: a guard that fails on the current code (e.g. a count of
      `availability_step()` calls or of the R work per row with nothing to
      apply, on a fixture large enough to separate it from noise) is
      recorded red before the fix. Verify: guard red then green; walk time
      on the 1500-actor fixture (exact-time rate unconstrained, joint
      unconstrained) and the CollegeMsg walk budget measured back to back
      against `3cb92d9`, median of at least 5 warm runs on optimized builds
      in detached worktrees; the time recorded in the tick note against the
      0.117 s pre-regression figure; stored objects byte-identical
      (`identical()` on the preprocessed outputs before and after);
      baselines PASS not SKIP.
      *Done 2026-10-03:* `availability_step()` returns early on an idle row
      (`availability_row_is_idle()`: no queued flip, neither side's
      composition cursor behind its target, no opportunity list, a value
      already open or a presence-only variant), which only closes an empty
      stream row (and the sender factor's, at outer) and adds the row's
      exposure at the count the previous row left. The presence variant
      keeps its sender count incrementally (`n_senders`) instead of
      `sum(p1)` per row. Rule 4: the per-row `x <- c(x, ...)` growth in
      `availability_step()`, `_step_receivers()`, `_step_point()` and
      `_step_factors()` is collected once per row. Rule 5: the walk's
      routes are resolved before the loop (`resolve_engine_routes()`:
      each consumer's availability effect, live mask entry and stream,
      and the owner's non-own right-censoring routes;
      `resolve_live_readers()`: each atom store's readers), so a stored
      row looks nothing up by fid name; `engine_owning_fid()` folds into
      `merged_route_dependent()` (its flavor `match()` on the unit's short
      flavor vector stays, a position, not a `[[fid]]` lookup).
      Guard (`test-availability_step_cost.R`), red first on the unfixed
      code: the unconstrained exact-time rate on `constraint_cost_data()`
      entered the presence step 2995 times (of 2995 rows; 0 expected), and
      the unconstrained REM with three composition changes 439 times
      (<= 3 expected); green after (0, and between 1 and 3). Stored
      objects: 16 preprocessed outputs (unconstrained rate and joint,
      gate and point constraints, REM with composition, the four
      vector-fold families, the six constraint-atom families, the
      self-only rate) serialize identically before (`3cb92d9`) and after,
      formula environments aside (they hold the package's own functions,
      whose srcrefs name the worktree). Timing, `-O2` builds in detached
      worktrees, base and fix alternated twice, medians of 11 warm walks
      (1500 actors) or 7 (CollegeMsg budget fixture):
      unconstrained exact-time rate 0.141 / 0.143 s -> 0.128 / 0.128 s
      (-10%; the pre-regression 0.117 s is still 9% below); joint
      unconstrained 0.251 / 0.251 -> 0.232 / 0.230 s (-8%); CollegeMsg
      unconstrained 3.653 / 3.681 -> 3.676 / 3.678 s and constrained
      3.020 / 3.022 -> 3.000 / 3.000 s (no change); CollegeMsg allocation
      and object size unchanged to the byte (43656.8 / 44226.5 MB, 84.02 /
      111.71 MB), so the budget CSV does not move. *Amended 2026-10-03
      (6.6c):* the step/routing split first written here came from a
      stub probe taken before the routing fix and is withdrawn. At
      `0ebd2a0` the follow-up re-check stubbed `availability_step()` out
      and measured 0.116-0.118 s against 0.126 s with it, so the remaining
      gap to the 0.117 s pre-regression figure (about 0.009 s over 2995
      rows, some 3 us a row) is the step call itself (the two
      `availability_presence_target()` calls, the idle predicate and the
      idle-row writes) and the resolved routing costs nothing measurable.
      C++ step: not built in this change. A compiled step would remove at
      most those 3 us a row on the unconstrained rate while still paying
      an R-to-C call per row, and the CollegeMsg budget walk does not move
      with the step at all (time, allocation and size identical), so it
      buys about 7% on one narrow path. The cheaper remaining lever is in
      R: test idleness in `merged_row_availability()` before computing the
      two presence targets. Suite (`NOT_CRAN=true`):
      PASS 13189, FAIL 0, ERROR 0, SKIP 7; six baseline files PASS with 0
      skipped; `git diff 3cb92d9 -- tests/testthat/_baselines` empty.
- [ ] 6.6c **should-fix** (added 2026-10-03, from the follow-up re-check,
      `.plan/develop/availability-one-object-per-loop-followup-conformance.md`,
      G-A/B/C/F): (a) the 6.6b tick note attributes the remaining 0.011 s
      to routing (0.007 s) and the step (0.004 s) from a probe taken before
      the routing fix; at `0ebd2a0`, stubbing the step gives 0.116-0.118 s
      against 0.126 s, so the remaining gap is the step call and routing
      costs nothing measurable; amend the note and restate the reason the
      C++ step is not built in this change; (b) two comments still
      describe the ego choice object as two factor vectors: the header of
      `tests/testthat/test-support_constraint_ego_fold.R` and the roxygen
      of `active_dyad_encoding_decide()` and the accessors in
      `R/preprocess_writers.R`; rewrite by hand; (c) assert in the ego fold
      test that an ego-kind choice object carries no `active_sender_*`
      field; (d) rewrap the ragged comment at `R/walk_handle.R` around
      lines 1288-1289. Verify: the new assertion fails when a sender
      factor is stored (ablate in a detached worktree or by copy and
      restore with cmp); devtools::document() if roxygen changed, with
      man/ unchanged or regenerated; suite green with baselines PASS.
- [ ] 6.7 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS not SKIP; `openspec validate --strict` and the placement check
      clean; a scoped re-check by a fresh conformance-inspector agent.
      Handed elsewhere, not this change's: the `symmetric` flag that
      `walk_live_support()` drops (`process-simulation` 2.11d); the
      empty `2 x 0` update buffers that switch on the R engine's per-event
      axis filter, the flipping-point-mask row reads the 4.6 guard does not
      cover, and the per-event `match()` / gather `.rowSums` passes
      (`likelihood-site-cleanup-and-cost`, see its conformance report).
