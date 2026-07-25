# Tasks — backend-parity

Disciplines (openspec/config.yaml): one focused conventional commit per task,
tests green at every commit, `devtools::document()` inline whenever roxygen /
exports / signatures change, `air format` only the R files a task touched before
`lintr` runs on them, `NOT_CRAN=true` with the frozen baselines PASS (not SKIP)
before each commit, r-lib skills (r-package-development, testing-r-packages,
cli, lifecycle) invoked before the work they cover.

**`src/` discipline is the spine of this change.** Every task touching
`src/*.cpp` / `src/*.h` invokes the `cpp-recompile` skill (Rcpp::compileAttributes()
plus a forced recompile) BEFORE testing, so no coefficient or golden baseline is
ever compared against a stale `.o`/`.so`. Section 1 (the vocabulary push) is
pure R and sequenced first so every later task is written under the final
names; section 6 (the frozen `cpp` engines) is sequenced last on purpose
(design D7).

## 0. Ground the design against the code

- [x] 0.1 Re-verify the design's Context "key code facts" before any edit, since
      the whole change rests on them: the nine-kernel flag table; that
      `compute_multinomial_selection.cpp` computes and discards the per-event
      score; that the gather stack materializes `index_i` / `index_j` for every
      model while only coordination's `sender_of_row` / `dyad_partner` reach the
      kernel dispatch; that the `r` backend forms the probability vector per
      event; that `baselines_backends` is `c("r", "cpp")` so no gather
      coefficient is frozen; and the push facts — the backend branch is decided
      in R with no token reaching C++ as data, `estimate_c_int()` is unexported,
      and `result.goldfish` records no backend. Record any drift in progress.md
      and adjust the design before implementing, rather than implementing
      against a stale fact.
- [x] 0.2 Verify the D12 margins facts before any margin code is written: the
      `cpp` timed kernels accumulate `timespan * rate` (expected counts) while
      the multinomial kernels accumulate probabilities summing to the event
      count. D12 already fixes the design (scale marker on the stored
      component, attached at the R-side result assembly; family-specific `c_e`
      in the helper; the shape requirement and its implementation live in
      `residuals-gof`, its task 1.10) — this task confirms the kernels match
      those facts and records any drift in progress.md before the helper is
      written.

## 1. The backend vocabulary push (design D8–D10, pure R, no numerics)

- [x] 1.1 The control object speaks backend (D8, D9): `set_opt.R` stores the
      resolved value as `backend` and drops the `$engine` component; the
      `BACKEND_ENGINE_TOKENS` / `LEGACY_ENGINE_BACKENDS` pair survives only as
      the input-side legacy map (`engine =` sentinel and legacy values still
      fold with one warning, unchanged); `engine_backend()` is deleted. Roxygen
      `@return` documents the `backend` component and drops `engine`;
      `devtools::document()` inline. Tests: `test-set_opt.R` migrated,
      `_snaps/set_opt.md` regenerated, new expectations for the two spec
      scenarios (control object carries `backend`, has no `engine`).
- [x] 1.2 The estimation path reads backend (D8): the four gates and the silent
      downgrade in `model_estimate.R:1223-1291` and the dispatch at
      `model_estimate.R:2147-2155` compare backend values; the read shim
      resolves a control list carrying only a legacy `engine` component (the
      pre-2.0.0 spec scenario); `estimate_c_int()` moves to
      `backend = c("cpp", "gather")` in `cpp_interface.R` (signature,
      `match.arg`, the four branches); the synthetic fit in
      `zzz_testthat_helpers.R` follows. Sweep the remaining test files off the
      tokens (~11 files; the heaviest after `test-set_opt.R` are the
      support-constraint and diagnostic-primitives suites). `NOT_CRAN=true`
      with the frozen baselines PASS is the proof that the rename touched no
      numerical path.
- [x] 1.3 Internal callers off the deprecated surface (D8):
      `functions_preprocess_em.R`'s five `set_algorithm_newton(engine =
      "default")` calls become `backend = "r"`. **Verification revised:** the
      planned behavioral test (no deprecation signal from the EM path) is not
      achievable — that code is unexported, referenced by no test, and has
      unresolved free variables (`nChains`, `net1`, `formulas`, ...), i.e.
      research prototype that cannot execute. The gate is instead the static
      one: no `set_algorithm_newton(` call anywhere in `R/` passes `engine =`,
      and the migrated file parses.
- [x] 1.4 The fit records its backend (D10): one write in the `### 6. RESULTS`
      assembly in `model_estimate.R`; the `@return` component list documents
      it. Tests: each backend's fit carries the matching value (the three-way
      spec scenario); a fit stripped of the component passes the
      NULL-tolerance contract everywhere the package itself gates on it.
- [x] 1.5 Remove the superseded EM prototype from `R/` (D18): `git rm`
      `R/functions_estimate_emdynam.R` and `R/functions_preprocess_em.R`,
      moving the working copies into `.plan/DyNES/` beside the helper half they
      call (`sgd_refactor()`, `get_weights()`, the resampling schemes,
      `getChainSample()`, `computeSupportConstrain()`). Neither function is
      exported, and nothing in `R/`, `tests/`, `man/`, `inst/`, `_pkgdown.yml`
      or `vignettes/` references either, so nothing else moves with them.
      Verification: `devtools::load_all()` clean; the package's undefined-global
      surface drops to zero (all 14 came from these two files); `NOT_CRAN=true`
      suite green with the frozen baselines PASS. Do **not** revert task 1.3 —
      the removal subsumes it.

## 2. The shared reduction

- [x] 2.1 `src/event_reductions.h` (+ `.cpp` if any helper is out-of-line):
      `rank_of_observed()`, `accumulate_margins()`, `event_score_row()`, taking
      only `(w_e, c_e, X_e, observed index, actor index, allowed mask)`.
      `accumulate_margins()` takes a **side list** (task 0.2): three of the six
      `cpp` kernels scatter one contribution into two accumulators (REM and
      REM_ordered into sender+receiver, MM into both tie endpoints), and
      calling a one-sided helper twice would iterate REM's `n1 x n2` risk set
      twice. Header
      doc states the `(w_e, c_e)` contract and both families' instantiation, in
      the register `stable_softmax.h` established. No kernel wired yet; a
      standalone C++ unit exercise plus the R mirror in 2.2 is the proof.
- [x] 2.2 R mirror of the three reductions in `estimation_core.R`, alongside the
      existing `stable_softmax()` mirror, with a comment naming the C++ header
      as its counterpart. Tests: helper-level parity of the R mirror against the
      C++ helper on constructed `(w, c, X, obs)` inputs — exact for ranks, 1e-10
      for margins and scores — so the two implementations are pinned to each
      other before either is wired into a backend.

## 3. The gather backend

- [x] 3.1 Thread the per-row actor index into the gather kernel dispatch:
      `index_i` / `index_j` from the gather stack through
      `R/cpp_interface.R` into `compute_multinomial_selection()` and
      `compute_poisson_selection()` signatures (coordination already receives
      `sender_of_row` / `dyad_partner`). No behavior change yet; verify the
      `RcppExports` interface diff shows exactly the intended arity change.
- [x] 3.2a Rename `stable_softmax_masked()` -> `log_sum_exp_masked()` (D19):
      the helper returns a log-sum-exp and the shifted weights, never a softmax,
      and the misnomer already produced a wrong task line here. Pure symbol
      rename, no arithmetic -- 7 call sites across 4 kernels plus
      `stable_softmax.{h,cpp}` and the prose in `event_reductions.h`. The R-side
      `stable_softmax()` keeps its name (it does return probabilities). Own
      commit, so the numerical tasks that follow have a clean diff.
      `NOT_CRAN=true` with the frozen baselines PASS is the whole gate -- a
      rename cannot move a coefficient.
- [x] 3.2b Gather **multinomial** kernel takes its normalizer from the shared
      helper (spec: likelihood-computation). Its likelihood is a ratio, so the
      shift cancels exactly and `log(exp_obs/normalizer)` becomes
      `x_obs - lse` -- the same underflow fix the `default_c` kernels already
      have. Own commit, before the accumulators, so any movement is
      attributable. Tests: cross-backend agreement unchanged on the
      well-conditioned fixtures; an extreme-parameter fixture where the naive
      path underflows to `-Inf` and the shifted one does not.
- [ ] 3.2c Gather **Poisson** kernel moves to one shifted `exp` pass (D19), from
      which it derives `T = exp(lse)` (bit-identical to the old raw normalizer,
      and overflowing at the same point -- the likelihood is NOT stabilized and
      must not be), the derivative with the shift restored, `p = w/sum(w)`, and
      the log-normalizer that D13's `total_rate` and D17's conditional component
      need. Tests: per-event logLik / score / information unchanged against the
      pre-change kernel on the baseline fixtures; `p` exact where the raw ratio
      gives `NaN` (overflow) or a spurious `1` (subnormal underflow).
- [ ] 3.3 Gather kernels gain `return_event_scores`, `return_ranks`,
      `return_margins` via the shared helpers, plus `total_rate` from the
      Poisson kernel's existing `normalizer`. All three kernels
      (multinomial, Poisson, coordination); the Poisson kernel accumulates
      margins on both exact-time scales from **one** weight vector (D20:
      `w = p`, with `c = 1` for the probability scale and `c = Δt * T` for the
      compensator), and stores the conditional log-probability as
      `x_obs - lse` (D17 revised -- computed here, not assembled R-side from an
      identity that loses digits away from the MLE).
      Tests: gather `event_scores` column
      sums equal the aggregate score; gather ranks/margins present.
- [ ] 3.4 Return the new gather components through `R/cpp_interface.R` onto the
      result object under the same names the `cpp` backend uses, so no consumer
      branches on backend. Tests: component names and shapes identical across
      `cpp` and `gather` for the same fit.

## 4. The r backend

- [ ] 4.1 `r` backend accumulates `ranks` and `margins` in its contribution loop
      via the 2.2 mirror (margins on both exact-time scales from one `p`
      vector, D20), **without** materializing the per-event probability matrix.
      The multinomial path already has what it needs from `stable_softmax()`.
      The exact-time path does **not**: `event_contribution_rate()` computes
      `rates <- exp(objectiveFunctions); ratesSum <- sum(rates)` raw and
      unshifted, and task 0.1 found the R backend has no `total_rate` at all
      (it exists only in the two timed `cpp` kernels). So that function gets the
      **same shifted-weights treatment as task 3.2c** — one shifted pass giving
      `total_rate = exp(lse)` (value unchanged), exact `p`, and the conditional
      component `x_obs - lse` (D13, D16, D17). This is not a rename of
      `stable_softmax()`, which the exact-time path never calls (D19).
      Tests: a fit requesting `margins` but not `probabilities` carries margins
      and carries no probability matrix (the spec scenario, and the point of
      accumulating rather than reconstructing); exact-time `p` correct where the
      raw ratio gives `NaN` or a spurious `1`.
- [ ] 4.2 Rewrite the two existing parity tests that reconstruct `r` ranks and
      margins from `return_probabilities` into direct `r`-vs-`cpp` comparisons,
      and keep one reconstruction as an independent third expectation on a
      single fixture (design D5) so a shared reduction error is not
      self-confirming.

## 5. The cpp backend and the contract

- [ ] 5.1 `cpp` backend returns per-event probabilities natively, ending the
      `return_probabilities` redirect onto `r` — defined on every family per
      D16 (exact-time: next-event probabilities, so DyNAM-rate gains the
      primitive) — and the six timed/`*_default.cpp` kernels gain the
      probability-scale margins accumulator beside the existing
      expected-count one (D12). Tests: a `cpp` fit with
      `probabilities` requested carries them and emits no
      backend-substitution warning; exact-time probabilities sum to 1 per
      event; the storage guardrail still fires above the
      threshold.
- [ ] 5.2 The `(backend, primitive)` support table plus the single check in
      `estimate_wrapper()`, replacing the `gather`+`scores` abort, the silent
      `return_event_scores <- FALSE` drop beside it, and the
      `return_probabilities` redirect. One cli error naming the primitive, the
      requested backend and the supporting backends — written in the backend
      vocabulary section 1 already landed. `opportunities_list` keeps
      its own redirect (Non-Goal). Tests: snapshots for the error; the
      ordering-artifact scenario — the same primitive requested alone and
      alongside others reaches the same verdict on the same backend.
- [ ] 5.3 Full three-way parity suite at a **fixed** parameter vector
      (`max_iterations = 0`, `initial_parameters` from a converged fit) across
      `cpp` / `r` / `gather`, for one multinomial and one timed sub-model plus
      a choice_coordination fixture (D14: rank over the event's whole realized
      risk set, not within a CSR group): ranks exact, margins (both exact-time
      scale variants, D12) / scores /
      probabilities (incl. exact-time next-event probabilities, D16) /
      exact-time `total_rate` and the conditional log-probability (D13, D17)
      within 1e-10 -- the conditional compared across backends directly, with
      the algebraic identity `intervalLogL − log T + Δt·T` asserted only at the
      MLE, where the per-event expected count is order one and the identity is
      still accurate (away from it, it loses digits faster than the tolerance
      allows, which is why D17 was revised). This
      is the
      requirement's teeth; it must be green before section 6 touches a frozen
      path.

## 6. Fold the frozen engines onto the shared header (last, design D7)

- [ ] 6.1 Move the six `*_default.cpp` engines onto `event_reductions.h`,
      deleting their private copies of the reduction arithmetic and changing
      nothing they compute. One commit per family (multinomial engines, then
      timed engines) so a baseline movement is bisectable to a family.
      `NOT_CRAN=true` with the frozen 1e-6 baselines PASS is the gate on each.
- [ ] 6.2 If 6.1 perturbs a baseline, stop and record the finding rather than
      regenerating: the frozen coefficients are the regression floor, the header
      is already delivering its value on `gather` and `r`, and the fold can be
      dropped without stranding the change (design D7). Record the outcome
      either way in progress.md.

## 7. Cross-change coherence and closure

- [ ] 7.1 Enforce the revised D6 division: `residuals-gof` carries **no**
      `optimizer-selection` delta (its deprecation prose lives in
      `diagnostic-primitives`; this change owns the renamed "Per-event scores
      primitive" requirement wholesale) — verify the deleted delta has not
      reappeared. Verify `revise-gather-output`'s deltas carry
      no legacy engine tokens (its capability is exempt from the push as a
      Non-Goal, but its prose must not reintroduce the values the code no
      longer contains). Verify every `## MODIFIED` header in both
      changes still exists verbatim in `openspec/specs/<capability>/spec.md` —
      `openspec validate` checks SHALL and scenario structure but NOT `##`
      section placement, so a block under the wrong header silently becomes an
      ADD at archive and leaves the old wording in place. `openspec validate`
      green for both changes.
- [ ] 7.2 Update `residuals-gof` where this change removes an assumption it was
      written under: its design and tasks describe primitives as
      backend-dependent and `gather` as unable to expose a per-event
      decomposition, and its consumers can now gate on the fit's recorded
      `backend` component (design D10). Its `diagnostic-primitives` capability
      stays its own (D6); only the backend claims move.
- [ ] 7.3 Freeze a gather coefficient baseline (D15), after the section-6 fold
      so the frozen numbers capture this change's final numerics: extend the
      baseline tooling with a gather-keyed set as a **separately versioned**
      addition (the frozen `global_v1` files and their `default` / `default_c`
      keys are never regenerated); `baselines_backends` gains the gather
      mapping; a `NOT_CRAN=true` run reports the new set PASS. Record the
      version row in the `.plan/goldfish_versions.csv` ledger convention on
      archive.
- [ ] 7.4 NEWS entry: all five primitives on all three backends; the removed
      `probabilities` redirect and the dropped `$engine` control-object
      component flagged as the observable behavior changes; the single failure
      mode; the fit's new `backend` component. DESCRIPTION version bump. Full
      `NOT_CRAN=true` suite green with the frozen baselines PASS, not SKIP.
