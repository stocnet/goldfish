## 0. Baseline evidence

- [ ] 0.1 Record BEFORE timings with a fixed procedure: Social Evolution REM and
      DyNAM-rate, default `diagnostics` vs `diagnostics = "scores"`, median of 3,
      on the current build. Write them into `progress.md` — every later claim is
      a ratio against these.
- [ ] 0.2 Resolve design open question 1: decide how the fallback branch is
      exercised in a test (internal force-fallback seam vs. a fixture scaled into
      the subnormal regime) and record the decision and its reason in `design.md`.

## 1. Guard the probability vector

- [ ] 1.1 In `src/REM_default.cpp` and `src/DyNAM_rate_default.cpp`, declare
      `arma::vec probabilities` empty and fill it only under
      `return_probabilities || return_margins || return_conditional_scores`,
      matching the four kernels that already do this.
- [ ] 1.2 Run the cpp-recompile skill; confirm `compileAttributes()` reports NO
      interface delta (these are body-only edits) before rebuilding.
- [ ] 1.3 Verify with the not-cran-test skill: frozen coefficient and C++ golden
      baselines PASS, not SKIP. Record the timing delta against 0.1.
- [ ] 1.4 Close the task with the close-task skill (conventional commit).

## 2. Derive the normalizer from the likelihood pass

- [ ] 2.1 In both exact-time kernels, take `log_normalizer = log(normalizer)` and
      the probability vector from the raw weights when
      `std::isfinite(normalizer) && normalizer > 0`; keep
      `log_sum_exp_masked` as the fallback branch. Comment the *why* inline —
      the absolute-scale constraint on the compensator — without referencing any
      OpenSpec artifact.
- [ ] 2.2 Run the cpp-recompile skill; confirm no interface delta.
- [ ] 2.3 Assert the ADR-0021 prediction explicitly: every coefficient, every
      standard error and the aggregate log-likelihood unmoved at the 1e-6 floor.
      If any frozen baseline moves, STOP — the prediction was that none would,
      and a moved baseline is a defect in this change, not a value to re-freeze.
- [ ] 2.4 Verify `backend-primitive-parity`'s cpp/r/gather agreement to 1e-10 at
      a fixed parameter vector still holds for the conditional component.
- [ ] 2.5 Verify with the not-cran-test skill; record the timing delta against
      0.1. Close the task with the close-task skill.

## 3. Pin both branches

- [ ] 3.1 Add a test that fixes a parameter vector and asserts the conditional
      component agrees between the fast and fallback branches to 1e-10, using the
      seam chosen in 0.2 (testthat 3e, per the r-lib:testing-r-packages skill).
- [ ] 3.2 Add a test that `diagnostics = "loglik"` alone forms no per-event
      probability vector, on both an exact-time and an ordinal sub-model — the
      second is a guard against the defect reappearing in the kernels that are
      currently correct.
- [ ] 3.3 Verify with the not-cran-test skill; close the task.

## 4. The standing cost gate

- [ ] 4.1 Measure the post-change ratio (default vs `diagnostics = "scores"`) on
      the gate fixture and pick a ceiling with headroom; record the measured value
      and the chosen ceiling side by side, with the reason for the headroom.
- [ ] 4.2 Add the cost gate as a test: one small fixture, both fits timed in the
      same session, median of repeats, failing with the measured ratio and the
      ceiling named in the message (cli, per the r-lib:cli skill).
- [ ] 4.3 Confirm the gate runs under `NOT_CRAN=true` and reports PASS, not SKIP,
      and that it fails when the guard from 1.1 is reverted — a gate never seen
      to fail is not known to work.
- [ ] 4.4 Verify with the not-cran-test skill; close the task.

## 5. Milestone

- [ ] 5.1 Write a `NEWS.d/` fragment (protocol in `NEWS.d/README.md`) covering the
      cost fix and the new gate. Do NOT bump the DESCRIPTION Version or edit
      `NEWS.md` unless this lands directly on the integration branch — check the
      branch first.
- [ ] 5.2 Append the BEFORE/AFTER timings and the gate ceiling to `progress.md`,
      and re-run the placement pre-flight
      (`bash .plan/opsx-spec-placement-check.sh exact-time-diagnostic-cost`).
- [ ] 5.3 Full `NOT_CRAN=true` suite green with the frozen baselines PASS, not
      SKIP.
