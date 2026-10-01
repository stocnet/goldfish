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
- [x] 4.3 `R/simulate_driver.R` deferred-fid path: read the fid's maintained
      availability (step the effect to the cursor, return its value and
      encoding; the buffer, not a copy) instead of `matrix(present2, ...) &
      support_to_grid(...)`. Test: the 4.2 guard passes; a seeded simulation
      from a constrained REM fit draws the same sequence before and after;
      the free-running and time-anchored simulation tests stay green.
- [x] 4.4 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS; every reference family PASS.

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
- [ ] 5.3 Vault: ADR-0109 `spec:` and open questions current; dashboard
      fragment in `_dashboard-inbox/`; session note closed; hand-off notes
      to the estimation-side cost change (the kernels' per-event `allowed`
      vector, the R backend's list-held grid copy) in its proposal once it
      exists, else in this change's `progress.md`.
- [ ] 5.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      and goldens PASS not SKIP; `status: landed (feature_simulation,
      awaiting fold)` in `proposal.md`.
