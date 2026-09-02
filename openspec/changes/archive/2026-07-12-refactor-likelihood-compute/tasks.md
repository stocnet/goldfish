## 1. Benchmark harness + BEFORE evidence

- [x] 1.1 Add a local (gitignored or tests/helper) timing script that runs the
      isolated hot cells — REM timed + DyNAM-choice-coordination, `default`
      engine, Social Evolution + Fisheries fixtures — and the full baseline
      suite, with repeated `system.time` medians; record BEFORE numbers in
      `progress.md` AND fill the seeded BEFORE rows in the long-format ledger
      `.plan/refactor-likelihood-compute_timings.csv` (gitignored; one row per
      engine×fixture×dataset — set `recorded_date`, `git_commit`, `median_sec`).
      This CSV is the machine-readable companion to the `progress.md` timing
      prose; every timing-bearing session (5.4/5.5/5.6, 5.10, 8.2) APPENDS its
      rows here, never overwrites
- [x] 1.2 Capture the current implementations of the contribution helpers as a
      test-side reference (test helper holding the original function bodies) for
      the D4 old-vs-new equivalence harness
- [x] 1.3 Add the equivalence test skeleton: deterministic small fixtures for all
      six sub-models × both datasets, comparing logLikelihood, score,
      informationMatrix, pMatrix per event at 1e-10 (initially old-vs-old, green)

## 2. Rate/REM core (event_contribution_rate)

- [x] 2.1 Replace the copying flatten `apply(statsArray, 3, c)` with a `dim<-`
      merge of the first two dimensions (design D2)
- [x] 2.2 Replace `ratesStatsStatsSum` (`colSums(t(apply(..., outer)) * rates)`)
      with `crossprod(statsArray, statsArray * rates)`; delete the
      single-parameter special-case loop (dimensions now preserved) (design D3)
- [x] 2.3 Verify: equivalence tests ≤ 1e-10 (incl. reflexive-exclusion and
      riskMask zeroing scenarios); frozen baselines PASS at 1e-6; APPEND
      `phase=after` timing rows for the touched sub-models (`dynam_rate`,
      `rem_timed`) to `.plan/refactor-likelihood-compute_timings.csv`
      (`session=B`; the 1.1 seed is the shared BEFORE); commit per task

## 3. Softmax / probabilities (getMultinomialProbabilities + rate_ordered)

- [x] 3.1 Replace `exp(apply(weightedStatsArray, c(1, 2), sum))` (3-D branch) and
      `exp(rowSums(sweep(...)))` (2-D branch) with reshape (`dim<-`) + one
      `%*% parameters` matrix product; same for
      `rowSums(t(t(statsMatrix) * parameters))` in
      `compute_event_contribution.dynam_rate_ordered_spec`
- [x] 3.2 Verify: equivalence tests ≤ 1e-10 (both branches, reflexive-exclusion
      on); baselines PASS; APPEND `phase=after` timing rows for the touched
      sub-models (`dynam_choice`, `rate_ordered`) to the timings CSV
      (`session=B`); commit

## 4. Derivatives and information matrices

- [x] 4.1 Rewrite `getMultinomialInformationMatrixM`, `getInformationMatrixREM`,
      and the `dynam_rate_ordered` information block as weighted cross-products
      `crossprod(D, D * w)` on the flattened derivative matrices (design D3)
- [x] 4.2 Rewrite `getMultinomialInformationMatrix` (coordination) as a weighted
      cross-product with the upper-triangle mask folded into the weights
- [x] 4.3 Reduce `compute_first_derivative_choice_coord` temporaries to matrix
      products where the profile justifies it; keep the C-level `aperm`
      symmetrization unless measured otherwise (design open question)
- [x] 4.4 Verify: equivalence tests ≤ 1e-10 for choice, coordination, REM(-ordered)
      contributions; baselines PASS; APPEND `phase=after` timing rows for the
      touched sub-models (`dynam_choice`, `dynam_choice_coordination`,
      `rem_ordered`) to the timings CSV (`session=C`); commit per task

## 5. In-house stable softmax + C++ profile

- [x] 5.1 Implement the R stable-softmax helper (design D7: one exp pass →
      probabilities + log-normalizer) and route the four multinomial
      contributions through it, computing the observed event's logL from the
      shifted predictor; the timed `event_contribution_rate` path stays plain
      `exp()` (Non-Goal)
- [x] 5.2 Tests for the new numerical behavior: extreme linear predictors
      (overflow) give finite logL/score/information matching a high-precision
      reference; an underflowing observed event's logL is finite (not `-Inf`);
      benign fixtures still agree with the pre-change results at 1e-10; timed
      hazard path unchanged
- [x] 5.3 Implement the shared C++ stable-softmax helper (`src/`, mirroring the
      `flat_updates.h` pattern) and adopt it in the `default_c` multinomial
      normalizer loops; cpp-reviewer on the diff before commit; cross-engine
      R-vs-C++ agreement tests pass (incl. an extreme-parameter fixture); force
      recompile
- [x] 5.4 Rewrite `estimate_REM` / `estimate_REM_ordered` in the staged BLAS
      form (design D8): GEMV linear predictor → masked exp vector (presence /
      reflexive / risk-set as zeros) → GEMV weighted sum → one weighted-crossprod
      GEMM for the Fisher; guard the per-event imputation scan if the profile
      confirms it; cpp-reviewer, cross-engine 1e-10 fixtures, per-estimator
      BEFORE/AFTER timing (measure the `default_c` REM cell before rewriting,
      then after; APPEND both rows to
      `.plan/refactor-likelihood-compute_timings.csv`, `session=F`), force recompile
- [x] 5.5 Rewrite `DyNAM_MM_default.cpp` in the dyad-triangle form (design D9):
      length-d log-space weights via the D7 stable softmax (replaces
      `p % p.t()` / `accu(P)/2` and the final `log(P(s,r))`), per-sender GEMV
      expected statistics, ONE reused d×p deviation buffer replacing the
      per-event `P_3 = stat_mat` copy, Fisher as one weighted-crossprod GEMM;
      cpp-reviewer, cross-engine 1e-10 fixtures, timing (APPEND before/after
      `default_c` coordination rows to the timings CSV, `session=G`), force recompile
- [x] 5.6 Apply the same dyad-triangle core to
      `compute_coordination_selection.cpp` (gather engine, per-event candidate
      layout retained); cpp-reviewer, cross-engine 1e-10 fixtures, timing
      (APPEND the `gather_compute` coordination before/after rows to the timings
      CSV, `session=G`), force recompile
- [x] 5.7 Emit the index-based dyad-list gather layout (design D9/D13):
      `gather_sender_receiver_model_r` emits, per candidate row, the sanitized
      `index_i`/`index_j` plus the precomputed structures the per-iteration
      compute consumes — per-sender CSR offsets (each sender's
      allowed-receiver group) and the dyad-pairing permutation matching each
      (i,j) row to its (j,i) partner; stop forcing
      `twomode_or_reflexive = TRUE` for DyNAM-MM in `gather_` (one-mode
      coordination emits NO diagonal rows); constrained (point `active_dyad`)
      models emit only mask-allowed rows — the symmetric fold guarantees both
      directions of every kept dyad are present; retire the rectangularity
      metadata (`n_candidates1`/`n_candidates2`, whose second entry is only
      the LAST sender's count) in favor of the offsets. Verify: unconstrained
      emit reproduces today's row multiset minus the diagonal; constrained
      emit matches the folded mask exactly
- [x] 5.8 Consume the index structures in the coordination kernel and lift the
      redirect (design D9/D13): the dyad-triangle core (5.5/5.6) reads ONLY
      the 5.7 index structures — no `n_candidates == n1 × n2` assumption, no
      square reshape (`arma::mat p(..., n2, n1)` / `p % p.t()`), no
      `p.diag().zeros()`; unconstrained = the full off-diagonal list,
      constrained = a shorter list, ONE code path. LIFT the
      support-constraint-as-stat §5.6 `gather_compute`→`default_c` redirect +
      its `cli_inform`. Tests: constrained coordination `gather_compute` ==
      `default_c` == `default` to 1e-6 on the fisheries fixture; unconstrained
      cross-engine per-event agreement at 1e-10; the redirect message is gone;
      cpp-reviewer; force recompile
- [x] 5.9 Surface the index vocabulary on the exported long formats (design
      D13): `gather_model_data()` returns per-row `index_i`/`index_j`
      (roxygen documents them and the coordination row-set change: no
      diagonal rows, masked rows filtered; `devtools::document()`);
      `write_gather_to_db` adds `index_i`/`index_j` columns to the db long
      table (today it has only `event_id`/`is_selected`/`stat_<i>` — rows
      are unidentifiable in SQL); tests: export/db indices decode to the
      correct node labels, db round-trip matches the in-memory gather,
      `n_candidates`/`selected` stay consistent on the filtered stack
- [x] 5.10 Profile the `default_c` residuals beyond 5.3-5.9 (incl. the choice
      estimator, design D5c); if no measurable hot spot, record "no further
      change warranted" in `progress.md`; any adopted micro-opt: plain
      C++/Armadillo, cpp-reviewer, cross-engine tests, force recompile, and
      APPEND its before/after rows to the timings CSV (`session=I`)

## 6. Process-state evaluators (internal, design D12) + per-event scores (D11)

- [x] 6.1 While landing 5.3-5.6, factor the per-event probability/rate
      computation into named internal helpers with the D12 state contract
      (choice → receiver probabilities; rate/REM → actor/dyad hazards;
      rate-ordered → sender probabilities); exclusions enter as exact zeros;
      returns labeled with the D13 sanitized-index vocabulary so
      post-estimation joins use the same `index_i`/`index_j` keys as the
      gather/export surfaces
- [x] 6.2 Implement the internal state materializer replaying the preprocessed
      init + update streams up to an event index, reusing the shared
      `apply_flat_updates()` helper; no preprocessing format changes
- [x] 6.3 Consistency tests: materialize the state at observed event indices
      and assert the evaluators reproduce the estimation path's per-event
      quantities (pMatrix / intervalLogL inputs) at 1e-10, for all six
      sub-models; confirm nothing new is exported (NAMESPACE unchanged)
- [x] 6.4 Add the opt-in per-event score matrix (n_events × p) to the rewritten
      `default_c` evaluators, default off, mirroring `intervalLogL`;
      column sums equal the aggregate derivative at 1e-10; cpp-reviewer on
      the diff; force recompile
- [x] 6.5 Add `return_event_scores` to `set_estimation_opt()` (default FALSE;
      roxygen documents the uses: sandwich/clustered SEs, score-process
      diagnostics, event influence) and the `event_scores` result component
      (columns named by effect): `default_c` via the 6.4 flag, `default` R
      engine captured in its contribution loop, `gather_compute` aborts
      informatively; tests: both engines agree at 1e-10, column sums ≈ 0 at
      convergence, off-by-default leaves no component;
      `devtools::document()`

## 7. optimizer argument + maxLik adapter (design D10)

- [x] 7.1 Add `optimizer = c("newton_raphson", "bfgs", "bhhh", "nelder_mead")`
      to `set_estimation_opt()` (match.arg, return component, roxygen with
      lifecycle "experimental" badge); add maxLik to `Suggests`; run
      `devtools::document()`
- [x] 7.2 Implement the maxLik adapter: closure factory over the `default_c`
      evaluator with per-β memoization shared by logLik/grad/hess closures;
      BHHH wired to the per-event score matrix from 6.4; offsets/fixed
      parameters resolved before the adapter as on the NR path
- [x] 7.3 Guards: `requireNamespace("maxLik")` with cli install-hint abort;
      informative abort for maxLik optimizer × `engine = "gather_compute"` or
      `"default"`
- [x] 7.4 Map the maxLik result into the goldfish result object (coefficients,
      Fisher-based vcov at the optimum, logLik, iterations, convergence code)
      so `summary()`/`vcov()`/`logLik()` and post-estimation methods work
      unchanged
- [x] 7.5 Tests (conditional: skip without maxLik): BFGS and BHHH coefficients
      agree with Newton-Raphson at cross-engine tolerance on baseline
      fixtures; missing-package and engine-combination errors snapshot-tested
      (pinned cli context); result-object parity checks

## 8. Cleanup, AFTER evidence, milestone

- [x] 8.1 Delete the captured reference implementations from the test helper once
      every equivalence test passed against the shipped code; keep the 1e-10
      fixtures as regression tests against the refactored helpers' outputs
- [x] 8.2 Record AFTER timings (same procedure as 1.1) in `progress.md` AND
      APPEND the final AFTER rows to
      `.plan/refactor-likelihood-compute_timings.csv` (`phase=after`,
      `session=M`) — re-run the same engine×fixture×dataset cells as the 1.1
      BEFORE seed so the ledger holds a matched before/after pair per cell; full
      `NOT_CRAN=true devtools::test()` green, baselines PASS not SKIP;
      `lintr::lint_package()` clean on touched files
- [x] 8.3 Milestone: bump `DESCRIPTION` + `NEWS.md` (default-engine estimation
      speedup; numerically stable softmax in both engines — finite likelihoods
      under extreme parameters; new `optimizer` argument with maxLik-backed
      methods, maxLik in Suggests; no new Imports; constrained coordination
      native on `gather_compute` — redirect lifted; `gather_model_data()`/db
      long formats gain `index_i`/`index_j`, one-mode coordination export no
      longer ships diagonal rows); spawn the spec-conformance agent to
      cross-check the spec delta against the implementation
