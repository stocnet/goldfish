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
      *Deviation, recorded 2026-10-03 (task 7.5; agreed with Alvaro at
      the time, then in `progress.md` only):* the CollegeMsg cell is not a
      1k-event fit on three backends but one compiled REM evaluation on the
      first 200 CollegeMsg messages over the full 1899-actor node set, at a
      fixed vector, timed with and without diagnostics
      (`college_rem_200_evaluation`, `diagnostics_ratio college_rem_200`):
      an r or gather fit at that grid size would take minutes per budget
      run. The grid-dominated allocation of the r and gather backends is
      therefore not a budget cell; the generated-fixture cost guards
      (`test-likelihood_r_backend_cost.R`, `test-gather_build_cost.R`)
      carry it instead. D6 is amended.
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
      *Deviation, recorded 2026-10-03 (task 7.5):* the fast branch's
      guard is `std::isnormal(normalizer)`, not `isfinite && > 0`: a
      subnormal normalizer passes the latter and keeps only a few
      significant bits in `e / normalizer`, so it takes the shifted
      fallback too. D2a and the delta were amended at the time; this task
      text was not.
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
      *Attribution, recorded 2026-10-03 (task 7.5):* the Social Evolution
      REM cpp fit met D2's 0.45 s prediction (0.575 to 0.446 s), but not by
      D2's mechanism. One evaluation without diagnostics took 0.036 s
      before and after, and the grid-dominated CollegeMsg evaluation did
      not fall either (4.754 to 4.813 s); the fit-level gain is D2a's
      second `exp` pass removed and D4's diagnostics evaluated once. The
      kernel buffer rewrite bought allocations, 2635 to 5 per REM
      evaluation (and 2638 / 879 / 2196 / 2199 to single digits on the
      other kernels), not time.
- [x] 2.6 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      pins and budget PASS; cpp-reviewer passed on every `src/` diff.
      *Added 2026-10-03 (task 7.6):* this verification includes the kernel
      allocation probe run, `test-likelihood_kernel_alloc.R` on the probe
      build (command in that file's header, named in the not-cran-test
      skill for any `src/` change), which SKIPs in the ordinary suite.

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
      *Deviation, recorded 2026-10-03 (task 7.5):* `apply_flat_update()`
      and `apply_broadcast_update()` in `R/utils.R` were kept as they were,
      functional (they return the updated array), because tests and the
      decode checks call them as references; the r engine writes through
      new in-place helpers beside them, `write_flat_updates()` and
      `write_broadcast_updates()`, which return nothing. The
      `compute_step()` header comment is corrected to name them.
- [x] 4.3 Risk-set subsetting once per event into a reused buffer, `which()`
      once per axis; at the outer encoding the REM reduction by sender and
      receiver factors keeps its rectangular shape and the self-dyad
      positions (`self_dyads`, today a `match()` per event when the slot
      sets differ) are derived from the two index vectors without a
      per-event `match()`; `event_contribution_rate()` receives the array at
      the shape it reads and forms its weighted products into reused
      buffers; `accumulate_margins()` via `rowsum()`. Test: pins PASS;
      margins and availability tests PASS.
      *Amended 2026-10-03 (ADR-0113, task 7.1):* the subsetting "into a
      reused buffer" landed as a fresh copy of the kept rows per filtered
      event (pure R has no reused subset buffer), and the REM contribution
      still allocated about 3.5 statistics arrays per event. 7.1 replaces
      it: a REM event allocates one weighted statistics array plus vectors
      of the risk set's size, at most `1 + 3/q` arrays (`q` the array's
      columns), never copies, reshapes or subsets the statistics array, and
      applies an outer-encoded filter as a mask on the whole grid, so the
      self-dyad positions no longer need deriving (the `self_dyads` path is
      gone).
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
      *Amended 2026-10-03 (ADR-0113, task 7.1):* the REM prediction was
      refuted here (3290 MB against under 700), and that refutation lived
      only in `progress.md`. The contract is now one weighted statistics
      array plus vectors of the risk set's size per REM event, at most
      `1 + 3/q` arrays, and the Social Evolution REM r fit under about
      2100 MB (2035 MB after 7.1; choice 61.8 MB met the 150 MB
      prediction).
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
      *Added 2026-10-03 (task 7.6):* this verification includes the kernel
      allocation probe run, `test-likelihood_kernel_alloc.R` on the probe
      build (command in that file's header, named in the not-cran-test
      skill for any `src/` change), which SKIPs in the ordinary suite.

## 7. Conformance follow-up (added 2026-10-02)

From `.plan/develop/likelihood-site-cleanup-and-cost-conformance.md`
(evidence in the folder of the same name). The code computes correctly:
suite 13111 / 0 / 7, six baseline files and both C++ goldens PASS,
`_baselines/` byte-identical, pins and budget PASS, bindings regenerate
with no diff. The artifacts claim a cost the r backend does not meet, and
one kernel path has no detector. 7.1 and 7.2 block the fold.

- [x] 7.1 **blocks**: the r backend still allocates about 3.5 statistics
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
      *Done 2026-10-03.* Part (a), the empty-buffer filter, landed as 7.4
      (`be57715`) with its own no-op guard; this commit carries (b)-(e).
      Deviations from the spike stack, recorded here: (1) a point mask is
      not written as `x[!mask] <- -Inf` (the complement and the logical
      subscript cost one more risk-set vector, 2.34 arrays per event on a
      flipping point REM, over the bound); the state keeps the point mask
      again as its log (0 / `-Inf`, one `n1 x n2` double vector per
      evaluation, written at the same flips) and the contribution adds it
      to the product inside the product's buffer, bit-identical to the
      shipped values; (2) the outer-filter path no longer reduces the grid,
      so `self_dyads` (step block, both REM contributions and
      `getMultinomialProbabilities()`) is removed as unreachable, and the
      ordinal REM's now-unused `compute_first_derivative_rem()` and
      `getInformationMatrixREM()` go with it (their test becomes a
      moment-against-centered check); (3) the bound is written `1 + 3/q`,
      `q` the statistics array's columns: ADR-0113's `1 + 3/(p+1)` counts
      the exact-time REM's intercept, and the ordinal REM has `q = p`,
      which is where its 2.5 comes from; (4) `need_probabilities` is
      probabilities, ranks, margins or availability; `total_rate` and
      `conditional_scores` read the contribution's scalars and vectors,
      not the probability vector (every reader grepped: `r_reduce_event()`,
      `expand_event_probabilities()`; coordination is not a REM and always
      returns its matrix). Guard first (`test-likelihood_r_backend_cost.R`,
      fixtures REM, point-constrained REM, ordinal REM and choice, limit
      `1 + 3/q + 0.1`, the 0.1 being the evaluation's fixed allocations over
      60 events): on `be57715` 3.57 / 3.55 / 9.77 against 2.1 / 2.1 / 2.6,
      red; after 2.07 / 2.06 / 2.52 (choice 0.046 against 0.25). Arrays per
      event on the spike's seven variants, before -> after: static 3.57 ->
      2.07, composition 3.51 -> 2.01, flipping point 3.50 -> 2.01, no-op
      alter 4.68 -> 2.01, alter excluding 5 4.63 -> 2.02, ordinal static
      9.77 -> 2.52, ordinal excluding 5 9.42 -> 2.52. Results against
      `237c8dd`: identical on four variants, at most 1.5e-11 (an outer
      filter's score, summed over the full grid) and 1.1e-14 on the
      ordinal, absolute on these fixtures (relative where the information
      is large: on the Fisheries REM, whose information reaches 1.7e13, the
      re-check measured 9.8e-4 absolute, 5.8e-17 relative, and 1.15e-10
      absolute, 4.6e-14 relative, on the Fisheries REM-ordered; the pins
      compare at 1e-10 relative); Social Evolution fits identical on REM, choice and rate,
      ordinal within 9.2e-14 on the parameters and 3.6e-12 on the
      log-likelihood. Optimized builds, back to back against `237c8dd`,
      median of five, r backend: Social Evolution REM 3310 -> 2035 MB,
      1.00 -> 0.88 s; REM-ordered 6072 -> 1436 MB, 2.03 -> 0.69 s; rate
      36.8 -> 29.1 MB; choice 61.95 MB unchanged. Budget lowered in this
      commit: `se_rem` r 3290 -> 2015, `se_rate` r 36.76 -> 29.12 (the
      recorder's default-diagnostics fit). Moment form against the centered
      form, largest deviation over every event: Social Evolution ordinal
      7.97e-14 at the pin vector and 4.05e-13 at the fitted one; Fisheries
      ordinal (widest range, 20) 2.36e-12 and 1.56e-11 absolute, 4.8e-13
      relative to the information; the guard fixtures below 1e-16. The
      loss is about `(mean / sd)^2` machine epsilons: a synthetic statistic
      with mean 1e2 over unit spread gives 4.4e-11 relative, 1e3 gives
      1.9e-9; the package's fixtures reach mean over sd 3.65, so the
      centered fallback stays an open question of ADR-0113, not a defect
      here. Delta requirement and scenario, D3 and its storage paragraph,
      the D3 guard sentence and the 4.3 / 4.5 notes amended to the same
      bound. Suite `NOT_CRAN=true`: 13274 / 0 / 7; six baseline files, both
      goldens, the pins and the budget PASS with 0 skipped;
      `_baselines/` unchanged.
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
- [x] 7.3 **should-fix**: the living requirement "C++ multinomial
      normalizers use the shared stable softmax" still says the exact-time
      conditional log-probability and probabilities come from the shifted
      log-sum-exp; the 2.3a fast branch contradicts it. Add a MODIFIED block
      to the `likelihood-computation` delta; placement check clean.
      *Done 2026-10-03:* `## MODIFIED` block for the requirement in the
      delta: the ratio-and-log bullet keeps the shifted log-sum-exp as the
      rule and names the exact-time exception (`log(normalizer)` and the
      raw ratio when the normalizer is a normal positive double, the shared
      helper otherwise). It carries all five living scenarios by their exact
      names, unchanged, and adds "an exact-time diagnostic takes the raw
      normalizer when it is normal". `openspec validate --strict` valid;
      the placement check (run against this worktree's `openspec/`) reads
      `OK (6 requirement placements)`, exit 0; a scripted comparison finds
      no living scenario name missing from either MODIFIED block. Suite
      `NOT_CRAN=true` at `75ba874` (artifact-only, code as `125fe60`):
      13274 / 0 / 7, baselines, goldens, pins and budget PASS.
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
      REM 461.1 against 462.4 MB and choice identical, so the no-op
      difference per event is zero. (Corrected 2026-10-03, task 7.11: the
      REM gap first read here as "1.29 MB cheaper per evaluation, a
      constant independent of the event count" is a measurement-order
      artifact; the re-check measured -1.31 MB at 30 events and exactly 0
      at 60 and 120.) Gather's
      `.rowSums` at point and the coordination `match()` are in the gather
      stack builder, run once per fit, not on the r engine's filter path:
      recorded in 7.5 with their measured sizes, not closed here. Suite
      `NOT_CRAN=true`: 13269 / 0 / 7; baselines, goldens, pins and budget
      PASS, 0 skipped (the budget cells are unconstrained and do not move).
- [x] 7.5 **should-fix**, record in tracked artifacts what `progress.md`
      alone holds: the CollegeMsg budget cell is one cpp evaluation on 200
      messages, not a 1k fit on three backends (task 0.3, D6); the in-place
      r writers are new helpers, not rewrites of the old ones (4.2); 2.3a
      says `isfinite && > 0` where the code uses `isnormal`; the SE REM cpp
      prediction was met by removing the second `exp` pass and evaluating
      diagnostics once, not by the kernel buffer rewrite, which cut
      allocations (2635 to 5 per evaluation) but not time (ADR-0110's log
      too).
      *Done 2026-10-03:* notes added under 0.3 (with D6 amended), 2.3a,
      2.5 and 4.2, and a dated line in ADR-0110's discussion log for the
      attribution. Also recorded here, from 7.4 and the availability
      follow-up's G-D, with measured sizes: the gather stack builder (run
      once per fit, not per Newton iteration) reads, at the point encoding,
      the senders at risk with `which(.rowSums(active_dyad, n1, n2) > 0)`
      every event (`R/cpp_interface.R`, the `present1_ids` line): an
      `n1 x n2` pass and `n1` doubles per event, 28 us and 6 KB at 300
      actors, 0.96 ms and 37 KB at 1899. On coordination it also pairs each
      candidate row with `match()` twice per event (`sender_of_row`,
      `dyad_partner`): at a full grid 0.1 ms and 229 KB per event at 84
      actors, 7.3 ms and 3.1 MB at 300. Neither is on the r engine's filter
      path, so neither is closed here; a maintained per-sender count and an
      arithmetic partner index would remove them. Owner: the gather
      builder's next change (the `initial-stats-at-kind` successor or a
      gather follow-up). The availability check's flipping-point row reads
      (`availability_mark_candidates()`, about 12 KB per event) are walk
      code, not the likelihood, and stay with the availability change's
      owner. Suite `NOT_CRAN=true`: 13274 / 0 / 7; baselines, goldens, pins
      and budget PASS, 0 skipped.
- [x] 7.6 **should-fix**: the diagnostics-once test does not cover the
      non-finite-step exit nor assert that the convergence status is
      unchanged; the 2.1 kernel probe guard is dormant (no documented run
      builds the probe, so it SKIPs in every ordinary run) and blind below
      its size threshold: name the probe run in 2.6 / 6.4 and in the
      not-cran-test procedure.
      *Done 2026-10-03:* "a fit stopped short of convergence keeps its
      status" (`test-likelihood_diagnostics_once.R`) fits Social Evolution
      REM on cpp and r with `max_iterations = 3`, once as is (the
      iteration-limit exit) and once with its last Newton pass made to
      return a `NaN` log-likelihood through a mocked evaluator (the
      non-finite-step exit, which returns the parameters the iteration
      before accepted; asserted equal to a `max_iterations = 2` fit). For
      both exits: not converged, the stored diagnostics equal one pass at
      the returned parameters to 1e-10, and parameters, convergence list
      and iteration count are identical to the same fit with the default
      diagnostics. Red check: with the final diagnostics pass moved to the
      rejected step (`nr$parameters + nr$update`, in a detached worktree,
      not committed) the new test fails 10 of 38 and the existing one 61 of
      216. The probe run is named in the not-cran-test skill (a new
      section, any `src/` change) and in 2.6 / 6.4 above; the probe test
      header records its blind spot (only Armadillo allocations of at least
      one double per alternative are counted, so a masked-cell `find()`
      index or a `std::vector` passes unseen). Probe run on the probe build
      at `75ba874`: 6 / 0, not skipped. Suite `NOT_CRAN=true`: 13312 / 0 / 7;
      baselines, goldens, pins and budget PASS, 0 skipped.
- [x] 7.7 **should-fix**, hygiene: `tests/testthat/_budgets/estimation_cost.csv`
      comments cite task numbers (dangling after archive); `src/REM_default.cpp:103`
      is a 143-character roxygen line; rewrap by hand.
      *Done 2026-10-03:* the CSV header keeps its dates, commit hashes and
      reasons and drops the four task citations; the 2026-10-03 lowering
      note added by 7.1 carries none. The roxygen line is split by hand
      into two `@param` groups of 76 and 77 characters (the kernel block is
      `@noRd`); cpp-recompile: `compileAttributes()` changes
      `R/RcppExports.R` in those comment lines only, `src/RcppExports.cpp`
      not at all; clean optimized rebuild (no debug info);
      `devtools::document()` writes nothing. No roxygen line of
      `src/REM_default.cpp` now exceeds 80 characters (six code lines of 81
      to 83 predate the change and are not touched). Probe run on the
      probe build with these sources: 6 / 0, not skipped. Suite
      `NOT_CRAN=true`: 13312 / 0 / 7; baselines, goldens, pins and budget
      PASS, 0 skipped.
- [x] 7.9 **blocks** (added 2026-10-03, from the follow-up re-check, N1;
      decided by Alvaro, ADR-0113 log): applying outer filters as a
      full-grid mask scales with the grid, not the risk set:
      `grid_exclusions()` rebuilds an excluded x n index on every filtered
      event, so arrays per event grow from 2.09 to 2.62 as absent actors go
      from 10 to 200 of 300 (ordinal 3.37 against 2.6), and at 200 absent an
      evaluation allocates 4.5 times and runs 6.3 times slower than at
      `237c8dd` (151 against 24 ms). Decided: mask only, maintained in
      place. The outer filter's exclusions are written at the composition
      and constraint changes, like the point log mask, so nothing per event
      grows with the number excluded; the full-grid time under heavy
      composition is accepted and recorded in D3 and the delta. Guard: add a
      heavy-exclusion outer-filter fixture (at least 200 of 300 absent; REM
      and ordinal), red first on the shipped code, within `1 + 3/q + 0.1`
      after. Verify: arrays per event flat across 10 to 200 absent; time
      recorded against `237c8dd` on that sweep; deviation at most 1e-10
      (relative where the information is large); pins and baselines PASS.
      *Done 2026-10-03:* the REM outer encoding keeps its two factors also
      as a log mask over the grid (0 / `-Inf`), set once per evaluation
      from the initial factors; at each event the step rewrites the rows
      of the senders and the columns of the receivers that flipped, from
      the factors after both are current (`write_outer_log_mask()`), and
      the contributions add the mask inside the product's buffer, as for
      the point mask. `grid_exclusions()` and the `excluded_dyads`
      argument are gone; the REM step no longer computes kept slots, and
      checks the observed dyad against the factors (same errors). Guard
      first: `rem_absent` and `rem_ordered_absent` (300 actors, 200 leave
      at 0.5 and stay away, 120 calls among the other 100, outer
      encoding) in the cost guard; on `876b5f7` 2.59 and 3.36 against 2.1
      and 2.6, red; after 2.05 and 2.54. Detector of the maintenance:
      with the flip writes disabled (detached worktree, not committed) the
      risk-set test fails 6 and the pins 23. Sweep, 300 actors, 60 calls,
      composition (actors leave at 0.5), arrays per event / ms per
      evaluation, median of 7, optimized, one tree after another:

      | absent | `237c8dd` | `876b5f7` | after |
      | --- | --- | --- | --- |
      | 10 | 4.425 / 155.0 | 2.094 / 112.8 | 2.077 / 112.2 |
      | 50 | 3.305 / 118.7 | 2.206 / 117.7 | 2.082 / 109.2 |
      | 100 | 2.138 / 72.8 | 2.344 / 131.8 | 2.089 / 109.7 |
      | 200 | 0.581 / 21.9 | 2.622 / 147.8 | 2.102 / 109.9 |
      | ordinal 10 | 8.909 / 356.0 | 2.573 / 81.9 | 2.547 / 80.8 |
      | ordinal 50 | 6.628 / 154.6 | 2.740 / 90.9 | 2.555 / 82.1 |
      | ordinal 100 | 4.252 / 119.8 | 2.948 / 99.8 | 2.565 / 79.4 |
      | ordinal 200 | 1.083 / 18.2 | 3.365 / 120.2 | 2.585 / 76.9 |

      Flat in the share absent (the 0.025 drift is the one-off flip
      writes over 60 events); time is the full grid's, about 110 ms (80
      ordinal), slower than `237c8dd` from about 50 absent and 5 times
      slower at 200 (4 on the ordinal), as accepted. Deviation against
      `237c8dd` on the sweep at most 8.6e-16 relative; on the seven spike
      variants identical, 1.46e-11 (outer-filter score) and 1.1e-14
      (ordinal), as before. D3 and the delta's requirement and scenario
      record the in-place mask and the accepted time. Suite `NOT_CRAN=true`:
      13316 / 0 / 7; baselines, goldens, pins and budget PASS, 0 skipped.
- [x] 7.10 **should-fix, high priority** (added 2026-10-03, re-check N2,
      pre-existing at `237c8dd`): the r backend's two-mode ordinal REM
      excludes the `(i, i)` cells, which are valid dyads (no self-dyads in
      two-mode): observed `(i, i)` events read `-Inf`, other events are
      biased, and fits abort with "Matrix cannot be inverted"; cpp and
      gather match a hand oracle. Exclude the diagonal only when
      `!allowReflexive && !is_two_mode`, correct the comment that says the
      family always excludes it, and add a two-mode REM-ordered fixture
      (plain, sender-absent, receiver-absent) asserting r == cpp == gather
      == the oracle. Verify: the fixture red at `876b5f7`, green after; the
      two-mode baselines PASS. NEWS.d bullet (an r-backend fix users can
      observe).
      *Done 2026-10-03:* the ordinal REM contribution excludes the
      diagonal only under `!allowReflexive && !is_two_mode`, as the
      exact-time one does, and its comment says so. New
      `test-twomode_rem_ordered.R`: the estimable two-mode fixture (4
      senders, 3 receivers, `membership ~ inertia` at 0.8), plain, sender
      P1 absent at 1.5-3.5 and receiver O1 absent at 7.5-9.5, per-event
      log-likelihood of r, cpp and gather against a hand oracle (softmax
      over every available cell) at 1e-10. Red at `876b5f7` (in a detached
      worktree): 3 failures, the r backend in each variant; cpp and gather
      pass. Green after. The moment-form test in
      `test-likelihood_equivalence.R` now passes `allowReflexive` and
      `is_two_mode` explicitly, since the contribution reads them. NEWS.d
      bullet added. Suite `NOT_CRAN=true`: 13325 / 0 / 7; two-mode
      baselines (9) and the other five baseline files, goldens, pins and
      budget PASS, 0 skipped.
- [x] 7.11 **should-fix** (added 2026-10-03, re-check G2-G5): (G2) no test
      detects the `return_availability` disjunct of `need_probabilities`
      (without it an availability-only r REM evaluation errors); add one,
      red under its removal; (G3) the delta states the bound
      unconditionally, while with diagnostics on an event takes 2.7-6.9
      arrays (REM) and 4.0-7.5 (ordinal), mostly margins; scope the clause
      to the default diagnostics and record the per-event margins `rep()`
      vectors as a successor's (name the owner in the note); (G4) the NEWS.d
      fragment says "a fifth of the memory"; 2035 against 7325 MB is about a
      quarter; (G5) the not-cran-test probe section says `load_all(recompile
      = TRUE)`, which leaves a -O0 build; point it at the cpp-recompile
      skill. Also correct 7.4's "constant -1.29 MB cheaper" note (the
      re-check shows -1.31 MB at 30 events and 0 at 60 and 120, a
      measurement artifact) and make the Fisheries REM "within 1e-10" claims
      relative (G6, G7).
      *Done 2026-10-03:* (G2) "an availability-only evaluation agrees
      across backends" in `test-likelihood_risk_set.R`: r against cpp
      availability, only `availability` requested, on the outer alter REM
      and the two point-flip fixtures (REM and REM-ordered); with the
      `return_availability` disjunct removed from `need_probabilities` (in
      a detached worktree at `f3b8bf5`, not committed) it errors, 2
      failures; green after. (G3) the delta's r-backend clause is scoped
      to the log-likelihood, score and information with at most the
      default diagnostics, and names the opt-in reductions as outside the
      bound; the margins' per-event `rep()` index pair over the grid is
      left to the residuals-gof change, the owner of the diagnostic
      primitives, as a successor's item. (G4) NEWS.d: "a quarter of the
      memory" (2035 against 7325 MB). (G5) the not-cran-test probe section
      points at the cpp-recompile skill for the rebuild. (G6) 7.4's note
      corrected in place, marked. (G7) 7.1's deviation claim states where
      it is absolute and gives the Fisheries figures relative. Suite
      `NOT_CRAN=true`: 13328 / 0 / 7; baselines, goldens, pins and budget
      PASS, 0 skipped.
- [ ] 7.8 Verification: `NOT_CRAN=true` suite green; baselines, goldens,
      pins and budget PASS not SKIP; a scoped re-check of gaps 1-9 by a
      fresh conformance-inspector agent. Notes not owned here: gather
      choice allocation sits 0.5 percent under its ceiling; the walk
      `seconds` cell failed inside a full suite (0.5 of `progress.md`, walk
      budget maintainer); a global-kind broadcast in
      `write_broadcast_updates()` builds four `n1 x n2` vectors per update
      (`initial-stats-at-kind` 2.2b); commit `206e173` carries other
      changes' artifacts (branch fold).
