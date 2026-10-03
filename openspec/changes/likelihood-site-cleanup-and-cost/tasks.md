## 0. Budget and pins first (D6, D7)

- [x] 0.1 Confirm `availability-one-object-per-loop` is landed on
      `feature_simulation` (kernel argument lists settled) and the tree is
      clean; `NOT_CRAN=true` suite green (not-cran-test; six baseline files
      and the C++ goldens PASS, not SKIP). Record counts in `progress.md`.
- [x] 0.2 Per-event output pins (r-lib:testing-r-packages): a helper that
      evaluates one likelihood pass at a fixed parameter vector on each
      backend and returns `logLikelihood`, `score`, `informationMatrix`,
      `intervalLogL`, `event_scores`, `event_probabilities`, margins, ranks
      and availability for the six families on Social Evolution and
      Fisheries; references frozen under `_references/likelihood_v1/` with
      a README; test asserts 1e-10 against them. PASS on the unfixed tree.
- [x] 0.3 Estimation budget: `tests/testthat/_budgets/record-estimation-cost.R`
      and `estimation_cost.csv` (cells: Social Evolution REM / choice /
      rate and CollegeMsg 1k REM, on cpp, r, gather; metrics `alloc_mb` for
      r and gather, `seconds` per backend behind
      `GOLDFISH_WALK_COST_SECONDS`, `iterations`); `test-likelihood_cost_budget.R`
      with the walk budget's ceiling and floor rules. Record on the unfixed
      tree (optimized build via cpp-recompile); the test PASSES at the
      recorded values and runs under `NOT_CRAN=true`.
- [x] 0.4 Before measurements for the design's predictions, median of three
      warm fits on the optimized build, all budget cells plus wall-clock of
      one evaluation with and without diagnostics (the evaluation-level
      diagnostics ratio, recorded as a budget cell with a provisional
      ceiling at the measured value times 1.1 until 2.5 re-records it) and
      the fit-level default-against-`"scores"` decomposition on Social
      Evolution REM, for comparison with the 6.01 / 4.72 s record; numbers
      in `progress.md`.
- [x] 0.5 Verification: `NOT_CRAN=true` suite green; baselines, goldens, pins
      and budget PASS not SKIP.

## 1. Cleanup (D1)

- [x] 1.1 Delete commented-out traces and code in `src/DyNAM_rate_default.cpp`
      (`sender_corr`, the `Rcout` lines), `R/estimation_core.R` (snowfall
      blocks, `print()`, commented `nActors`/`nParams`/`likelihoods`/
      `keepIn` lines) and `R/cpp_interface.R` (`print()`, `intermediateData`).
      Convert or delete history markers per D1, one site at a time by hand,
      recording in `progress.md` each `to be make` whose behavior is still
      wanted. Test: suite green; a grep for `Rcout`, `snowfall`, `CHANGED`,
      `to be make`, `TO check` over `src/` and the two R files finds nothing.
- [x] 1.2 Remove the `impute` argument and block from the six kernels
      (cpp-recompile after each kernel, verify the `RcppExports.*` diff
      drops the argument), from `make_engine_evaluator()`, `estimate_c_int()`,
      `gather_()` and the r engine's equivalents. Test: goldens PASS; a
      snapshot of each kernel's `formals()` has no `impute`.
- [x] 1.2a The gather backend returns DyNAM-rate per-event probabilities as
      `n x 1` matrices where the r backend returns vectors (hand-off from
      `availability-one-object-per-loop`); return the vector. Test: the
      backend-parity probability comparison asserts identical shape, not
      only values, on a rate fit.
- [x] 1.3 Delete `reduce_mat_to_vector()` from both rate kernels and
      `.gather_reduce()`; the rate kernels read `stat_mat` directly
      (cpp-recompile). Remove `parallelize`, `cpus`, unread `verbose`,
      `get_data_matrix` and `state$time` from the signatures and state they
      travel through; `devtools::document()` if any roxygen moves. Test:
      rate baselines and goldens PASS; pins PASS.
- [x] 1.4 Correct `src/event_reductions.h`'s calling-convention comment and
      the REM kernel's roxygen (duplicated `active_dyad_update_pointer`,
      missing `stat_mat_broadcast*`, `active_dyad_is_point`, `return_*`).
      Test: `devtools::document()` clean; cpp-reviewer on the `src/` diff.
- [x] 1.5 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      pins and budget PASS (budget unchanged or lowered; lower the record if
      it fails below).

## 2. Kernels allocate per evaluation; the risk set is maintained in place (D2)

- [x] 2.1 Cost guard, recorded failing first: an allocation counter behind a
      compile-time macro in the test build (or, failing that, the budget's
      time cells on CollegeMsg 1k REM) asserts zero event-sized allocations
      inside each kernel's event loop; record the failing counts per kernel
      in `progress.md`.
- [x] 2.2 `src/log_sum_exp.cpp`: fill a caller-owned `weights`, `set_size()`
      only when the size differs. `src/event_reductions.{h,cpp}`:
      `event_score_row()` takes the weighted sum; the R mirror in
      `R/estimation_core.R` follows. Test: pins PASS; goldens PASS.
- [x] 2.3 One kernel per commit, cpp-recompile each, cpp-reviewer on each
      diff: REM, REM-ordered, MM, choice, rate, rate-ordered. Hoist
      `lin_pred`, `weights`, `e`, `probabilities`, `W` (Fisher weighted
      copy), `sides`; build dyad index maps only under `return_margins ||
      return_availability`; `allowed` initialized once with the self-tie
      rule and written by the availability lambda (point cell, outer row or
      column, sender gate); masked zeroing as a plain pass. Test per
      kernel: pins PASS; the "risk set follows the flips" test compares the
      maintained `allowed` to a from-scratch mask at every event on the
      constrained, composition-change and two-mode fixtures (exposed
      through a test-only return or by asserting the masked `e`); goldens
      PASS.
- [x] 2.3a Exact-time diagnostics from the likelihood pass (D2a), tests
      written before the fast branch, in `test-cpp_stable_softmax.R`'s
      home for the stability fixtures (r-lib:testing-r-packages): (i) at a
      fixed parameter vector on Social Evolution REM and DyNAM rate, the
      conditional component, `total_rate` and the stored primitives agree
      to 1e-10 between the shifted pass (forced through the evaluator seam)
      and the raw pass; (ii) the guard reached naturally both ways: one
      effect scaled so every linear predictor exceeds about 710 (the raw
      sum is `Inf`) and one so every predictor is below about minus 745 (the
      raw sum is 0), far past the thresholds so the fixture is not fragile;
      the seam reports the fallback taken, the conditional component is
      finite in the underflow case, and in the overflow case the likelihood
      is non-finite exactly as before so the Newton step rejection is
      unchanged; (iii) partial underflow: a few predictors far below a
      representable maximum; `log T` agrees to 1e-10, the probability vector
      sums to one, and the measured difference in the tiny probabilities is
      recorded in `progress.md` as the ADR-0021 number; (iv) the r backend
      at the same extreme fixtures: finite and agreeing with the cpp
      fallback (the exact-time twin of the living "backends agree under
      extreme predictors" scenario); (v) `diagnostics = "loglik"` alone
      forms no probability vector (the seam reports the buffer's size, or
      the allocation counter); backend-parity at 1e-10 on the benign
      fixtures. Then, in `src/REM_default.cpp` and
      `src/DyNAM_rate_default.cpp` (cpp-recompile each): `log_normalizer =
      log(normalizer)` and `probabilities = e / normalizer` under
      `isfinite(normalizer) && normalizer > 0`, `log_sum_exp_masked()` as
      the fallback; `probabilities` filled only under `return_probabilities
      || return_margins || return_conditional_scores`; the rate kernel's
      recomputed `lin_pred` removed. Test: the two tests above PASS;
      goldens and pins PASS (the conditional component within 1e-10, every
      other component unchanged); ADR-0021 prediction recorded in
      `progress.md` before the run.
- [x] 2.4 `src/compute_*_selection.cpp`: event-block views instead of
      copies, the duplicated probability pass removed, per-candidate
      `row.t() * row` replaced by one weighted cross-product. Test: pins
      PASS on gather; backend-parity PASS.
- [x] 2.5 Measure the budget cells and the 2.1 counter after 2.4 against
      0.4 and the design's prediction; re-record the diagnostics ratio
      cell's ceiling from the post-2.3a measurement with headroom for a
      structural regression, saying in the commit how it was derived;
      numbers in `progress.md`; lower the budget records that fail below in
      the same commit. NEWS.d bullet (compiled kernels allocate once per
      evaluation; the exact-time default path runs one `exp` pass).
- [x] 2.6 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      pins and budget PASS; cpp-reviewer passed on every `src/` diff.

## 3. Diagnostics once (D4)

- [x] 3.1 Guard, recorded failing first: a counter (test seam) of evaluator
      calls made with any diagnostic flag set; `k` on the unfixed code for a
      `k`-iteration fit.
- [x] 3.2 Equality test, written before the change: for the six families on
      both backends' loops, every stored diagnostic component of a fit
      equals the component from one evaluation of the pre-change evaluator
      at the fit's returned parameters (1e-10 for BLAS sums, identical for
      ranks and counts); includes a fit stopped at the iteration limit.
- [x] 3.3 `run_nr_loop()` (both backends) evaluates with diagnostics off;
      `estimate_int()` / `estimate_c_int()` evaluate once at the returned
      parameters with the requested flags and assemble the fit from that
      call; `has_unexpected_na()` inspects the three estimation components.
      Test: 3.1 counter reads 1; 3.2 PASS; `test-model_evaluate.R` and the
      diagnostic-primitives tests PASS unchanged.
- [x] 3.4 Measure the budget cells after 3.3 against 0.4 and the prediction;
      numbers in `progress.md`; lower records that fail below. NEWS.d bullet
      (diagnostics computed once per fit).
- [x] 3.5 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      pins and budget PASS.

## 4. The r backend writes in place (D3)

- [x] 4.1 Cost guard, recorded failing first: `bench::bench_memory()` of one
      `compute_step.default` call on a 1500-actor generated REM fixture
      (and a one-mode choice) is bounded independent of `n1 x n2 x p`;
      record the failing numbers (today one array copy per update, per
      diagonal zeroing, per subsetting).
- [x] 4.2 State as an environment created per evaluation; `apply_flat_update()`
      / `apply_broadcast_update()` write through `set_entries()` /
      `set_matrix_cells()` and return nothing; diagonal zeroed on the
      updated diagonal cells only (init zeroed once); accumulators
      preallocated and written in place. Test: pins PASS; a test asserts the
      input `prep` is `identical()` before and after a fit.
- [x] 4.3 Risk-set subsetting once per event into a reused buffer, `which()`
      once per axis; at the outer encoding the REM reduction by sender and
      receiver factors keeps its rectangular shape and the self-dyad
      positions (`self_dyads`, today a `match()` per event when the slot
      sets differ) are derived from the two index vectors without a
      per-event `match()`; `event_contribution_rate()` receives the array at
      the shape it reads and forms its weighted products into reused
      buffers; `accumulate_margins()` via `rowsum()`. Test: pins PASS;
      margins and availability tests PASS.
- [x] 4.4 Coordination path: `stable_softmax(rowwise = TRUE)` row maximum
      without `apply()`; the `n^2 * p` temporaries replaced by index
      arithmetic into two reused buffers. Test: pins PASS on coordination
      (both datasets); the living spec's r-backend scenario (no per-row
      closure) asserted by a grep-free check of the call path (a test seam
      counting closure calls, or `profvis`-free timing on a 300-actor
      fixture).
- [x] 4.5 Measure the budget cells and the 4.1 guard after 4.4 against 0.4
      and the prediction (Social Evolution REM r under 700 MB, choice under
      150 MB); numbers in `progress.md`; lower records that fail below.
      NEWS.d bullet (the r backend writes its state in place).
- [x] 4.6 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      pins and budget PASS; `air format` and `lintr` on the touched R files.

## 5. Gather builders (D5)

- [x] 5.1 Cost guard, recorded failing first: `bench::bench_memory()` of one
      gather build for a constrained REM on the 1500-actor fixture against
      `rows x p` doubles plus a constant.
- [x] 5.2 Preallocated candidate index per event; point presence grid
      through `set_matrix_cells()`; `.gather_broadcast_blocks()` without a
      per-event closure or a `seq_len(n1 * n2)`. Test: the gather stack is
      byte-identical to the pre-change build on every parity golden; 5.1
      guard passes; `test-support_constraint_estimate.R` gather and db tests
      PASS.
- [x] 5.3 Measure the gather budget cells after 5.2; numbers in
      `progress.md`; lower records that fail below.
- [x] 5.4 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      pins and budget PASS.

## 6. Close

- [x] 6.1 `bash .plan/opsx-spec-placement-check.sh
      likelihood-site-cleanup-and-cost` (four ADDED blocks and one MODIFIED
      naming a living requirement, a clean exit is expected) and `openspec
      validate likelihood-site-cleanup-and-cost --strict` clean.
- [x] 6.2 `NEWS.d/likelihood-site-cleanup-and-cost--likelihood-cost.md`
      consolidating the bullets of 2.5, 3.4, 4.5 and the removals of 1.2
      and 1.3. No `NEWS.md` or Version edit on the branch.
- [x] 6.3 Vault: ADR-0110 `spec:` and open questions current; dashboard
      fragment in `_dashboard-inbox/`; session note closed;
      `.claude/rules/walk-cost.md`'s "Estimation has no recorded budget
      yet ... is the `exact-time-diagnostic-cost` change" sentence updated
      to name the new budget and this change (that change was absorbed and
      deleted on 2026-10-01, so the sentence is a dangling pointer until
      then; a tracked file, so in a task commit).
- [x] 6.4 Final verification: full `NOT_CRAN=true` suite green; baselines,
      goldens, pins and budget PASS not SKIP; `status: landed
      (feature_simulation, awaiting fold)` in `proposal.md`.

## 7. Conformance follow-up (added 2026-10-02)

From `.plan/develop/likelihood-site-cleanup-and-cost-conformance.md`
(evidence in the folder of the same name). The code computes correctly:
suite 13111 / 0 / 7, six baseline files and both C++ goldens PASS,
`_baselines/` byte-identical, pins and budget PASS, bindings regenerate
with no diff. The artifacts claim a cost the r backend does not meet, and
one kernel path has no detector. 7.1 and 7.2 block the fold.

- [ ] 7.1 **blocks**: the r backend still allocates about 3.5 statistics
      arrays per REM event (4.7 under an outer-encoded filter); a Social
      Evolution REM fit allocates 3290 MB where D3, the
      `likelihood-computation` delta and tasks 4.3/4.5 claim nothing of size
      `n1 x n2 x p` per event and under one tenth of 7325 MB. The miss was
      accepted in `progress.md` only.
      *Decided 2026-10-03 (Alvaro, ADR-0113), after a measurement spike at
      `79dca89` (coordinator scratchpad `spike-7.1/`, patches per
      prototype):* close the cost in pure R and amend the artifacts to the
      bound that reaches. The exact-time REM's 3.5 arrays are one weighted
      array (`crossprod(S, S * p)`) plus about 7.5 throwaway vectors of
      length `n1 n2`; the subset copy adds 1.17 on filtered paths; the
      ordinal REM allocates 9.8 arrays per event (reshape copies and a
      materialized derivative array) and the 4.1 guard has no ordinal
      fixture. Land the pure-R stack: (a) an empty `2 x 0` buffer is no
      filter (shared with 7.4); (b) the rate contribution trimmed: `-Inf`
      mask in place, no risk-set subset for the maximum, the probability
      vector and risk-set membership built only when a diagnostic asks;
      (c) axis filters applied as a mask on the full grid, not a subset;
      (d) the ordinal REM on the flat matrix, no `dim<-` on a shared
      value; (e) the ordinal REM information in moment form,
      `crossprod(S, S * p) - tcrossprod(E)`, score `S[obs, ] - E`. The
      contract: a REM event allocates exactly one weighted statistics
      array and vectors of the risk set's size, at most `1 + 3/(p+1)`
      arrays per event; it never copies, reshapes or subsets the statistics
      array; the r backend stays pure R. The 4.1 guard gains the ordinal
      REM fixture and states that bound (about 2.5 on its fixture); the
      delta's ADDED allocation requirement and scenario, D3 and its storage
      paragraph, and the 4.3/4.5 tick notes are amended to it, with the
      Social Evolution REM r scenario at under about 2100 MB (spike: 3310
      to 2070 MB, REM-ordered 6072 to 1507 MB, results within 1.5e-11).
      The budget CSV is lowered in the same commit. Check the moment-form
      information on the fixtures with the widest statistic ranges (an
      ADR-0113 open question). Verify: guard red first on the shipped code
      (the ordinal fixture at 9.8), green after; the scenario and the guard
      state the same bound; pins at 1e-10 and baselines PASS.
- [x] 7.2 **blocks** (reopens 2.3): the "risk set follows the flips" test
      cannot see the REM and REM-ordered point-flip writes
      (`src/REM_default.cpp:394`, `src/REM_ordered_default.cpp:230`):
      disabling them leaves it and 29 related files green because the
      constrained REM fixture has zero point flips, while a REM with
      `~ !tie(emails)` (378 flips) shifts per-event log-likelihoods by up to
      1.81 on 778 of 782 events under the same ablation. Add a flipping
      point-constraint REM and REM-ordered fixture to that test; the
      current code agrees with the r backend there to 2e-11, so the gap is
      the fixture. Verify: the ablation is red.
      *Done 2026-10-03:* `risk_set_point_flip_prep()` in
      `test-likelihood_risk_set.R` builds `constraint_cost_data(120, 400,
      400)` with `~ !tie(emails)` on the exact-time and the ordinal REM
      (`rem_point_flips`, `rem_ordered_point_flips`), point encoding, more
      than 100 flips during the sequence (asserted by a second test, so the
      fixture cannot silently lose its flips). Red first: in a detached
      worktree at `237c8dd` with `src/REM_default.cpp:394` and
      `src/REM_ordered_default.cpp:230` commented out and an optimized
      recompile, the risk-set test fails 2 of 12 expectations, exactly the
      two new fixtures; restored, 0 of 16. The ablation was not committed.
      Suite `NOT_CRAN=true`: 13265 / 0 / 7, six baseline files, both
      goldens and the pins PASS with 0 skipped.
- [ ] 7.3 **should-fix**: the living requirement "C++ multinomial
      normalizers use the shared stable softmax" still says the exact-time
      conditional log-probability and probabilities come from the shifted
      log-sum-exp; the 2.3a fast branch contradicts it. Add a MODIFIED block
      to the `likelihood-computation` delta; placement check clean.
- [x] 7.4 **should-fix** (hand-off received from the
      `availability-one-object-per-loop` inspection, owned here as the r
      engine's): the filter test at `R/estimation_core.R:2090-2094` treats
      an empty `2 x 0` update buffer as a filter, so an outer-encoded
      constraint that excludes nothing copies the full array every event:
      +1446 MB, +33 percent per evaluation on a 300-actor no-op REM (+8
      percent on choice); cpp and gather show no difference. Test
      `length(update) > 0` or store `NULL` for an empty stream, and add a
      no-op guard (constrained-excluding-nothing equals unconstrained on
      the r backend). Verify: the no-op difference is zero.
      *Added 2026-10-03* (from the `availability-one-object-per-loop`
      follow-up re-check, G-D: its 6.7 handed these here, and no task
      received them): gather's per-event `.rowSums` pass at point
      (`R/cpp_interface.R:1493`) and the per-event `match()`. Close them in
      this task if they are on the same filter path; otherwise record them
      in 7.5 with a measured size.
      *Done 2026-10-03:* `compute_iteration_step()` tests
      `length(update) > 0L` on both availability buffers, so an empty
      `2 x 0` buffer is no filter; this is part (a) of the 7.1 stack, landed
      here. Guard first: "a constraint that excludes nothing costs the r
      backend nothing" (`test-likelihood_r_backend_cost.R`) compares one r
      evaluation with `~ alter(allow) > 0` on an all-ones `allow` against the
      unconstrained one on `constraint_cost_data(300, 60, 10)`: estimates
      equal to 1e-12, allocation ratio under 1.005. Red on the shipped code
      (`237c8dd`, in a detached worktree): REM 613.2 against 462.4 MB (ratio
      1.33), choice 4.223 against 4.006 MB (1.054), 2 failures; after the fix
      REM 461.1 against 462.4 MB (the constrained pass is 1.29 MB cheaper per
      evaluation, a constant independent of the event count) and choice
      identical, so the no-op difference per event is zero. Gather's
      `.rowSums` at point and the coordination `match()` are in the gather
      stack builder, run once per fit, not on the r engine's filter path:
      recorded in 7.5 with their measured sizes, not closed here. Suite
      `NOT_CRAN=true`: 13269 / 0 / 7; baselines, goldens, pins and budget
      PASS, 0 skipped (the budget cells are unconstrained and do not move).
- [ ] 7.5 **should-fix**, record in tracked artifacts what `progress.md`
      alone holds: the CollegeMsg budget cell is one cpp evaluation on 200
      messages, not a 1k fit on three backends (task 0.3, D6); the in-place
      r writers are new helpers, not rewrites of the old ones (4.2); 2.3a
      says `isfinite && > 0` where the code uses `isnormal`; the SE REM cpp
      prediction was met by removing the second `exp` pass and evaluating
      diagnostics once, not by the kernel buffer rewrite, which cut
      allocations (2635 to 5 per evaluation) but not time (ADR-0110's log
      too).
- [ ] 7.6 **should-fix**: the diagnostics-once test does not cover the
      non-finite-step exit nor assert that the convergence status is
      unchanged; the 2.1 kernel probe guard is dormant (no documented run
      builds the probe, so it SKIPs in every ordinary run) and blind below
      its size threshold: name the probe run in 2.6 / 6.4 and in the
      not-cran-test procedure.
- [ ] 7.7 **should-fix**, hygiene: `tests/testthat/_budgets/estimation_cost.csv`
      comments cite task numbers (dangling after archive); `src/REM_default.cpp:103`
      is a 143-character roxygen line; rewrap by hand.
- [ ] 7.8 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      pins and budget PASS not SKIP; a scoped re-check of gaps 1-9 by a
      fresh conformance-inspector agent. Notes not owned here: gather
      choice allocation sits 0.5 percent under its ceiling; the walk
      `seconds` cell failed inside a full suite (0.5 of `progress.md`, walk
      budget maintainer); a global-kind broadcast in
      `write_broadcast_updates()` builds four `n1 x n2` vectors per update
      (`initial-stats-at-kind` 2.2b); commit `206e173` carries other
      changes' artifacts (branch fold).
