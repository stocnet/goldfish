## 0. Branch and baseline

- [x] 0.1 Confirm with Alvaro the base branch (design Migration Plan), and
      run the `NOT_CRAN=true` full
      suite there (not-cran-test, in the background). Verify: FAIL = 0 and
      the six frozen baseline files PASS with 0 skipped; record the totals
      in `progress.md` as the before-state
      — *done 2026-09-17* (goldfish-a3): on `feature_simulation` directly
      (Alvaro; develop lacks the counter, the live gate and `simulate()`).
      Before-state = the `d349576` run (8994 / 0 / 4, six baseline files
      PASS), the code trees being identical

## 1. Detector first: fixtures, oracle, measurement (D4)

- [ ] 1.1 Add to `tests/testthat/helper-support-mask.R` a one-mode
      flavored fixture of four actors whose history ties one actor to every
      other actor, so that actor's creation row allows only its self-dyad
      over a stretch of events while the others create and dissolve; and an
      oracle `oracle_sender_gate(grid, present, one_mode)` computing
      `rowSums(grid & present & off_diagonal) > 0` from a dense grid without
      calling package code (r-lib:testing-r-packages). Verify: a test
      asserts the fixture reaches the case (at some event the saturated
      actor is present and its creation row, off the diagonal, is all
      `FALSE`), and the oracle's two-mode branch keeps the diagonal.
- [ ] 1.2 Measure on the current tree, without committing any production
      change: fold output (`active_sender` for the creation rate,
      `n_candidates`, `avg_active_entity`) and `estimate_dynam()`
      coefficients on the 1.1 fixture. Then apply a scratch patch of the
      corrected gate (not committed), run the `NOT_CRAN=true` suite, and
      list every test whose expectation changes. Verify: `progress.md`
      holds the before numbers, the scratch-patch after numbers and the
      list of moved tests with the reason each one moves; the working tree
      is back to the 1.1 commit.
- [ ] 1.3 Verification: `NOT_CRAN=true` suite green on the 1.1 commit;
      baselines PASS not SKIP.

## 2. The maintained counter (D2, D3)

- [ ] 2.1 Counter tests first, in a new
      `tests/testthat/test-support_gate_self_exclusion.R`: for global, ego,
      alter and point masks, on a one-mode and a two-mode layer, with mask
      flips on and off the diagonal and receiver presence crossings, the
      folded `active_sender` equals `oracle_sender_gate()` at every event.
      Include the alter case from the spec (`~ alter(x) > 0`, one actor
      with `x > 0` is gated out, mask still at alter kind) and an undirected
      one-mode flavored layer. Watch the one-mode cases fail for the
      diagonal and the two-mode cases pass.
- [ ] 2.2 Implement D3: `drop_diagonal` (required) through
      `fold_active_sender_support()`, `initial_receiver_count()`,
      `apply_receiver_count_flips()`, `apply_receiver_presence_flips()` and
      `receiver_reach()`; `finish_output()` in `preprocess_joint.R` passes
      `!isTRUE(model_spec$is_two_mode)`, never
      `engine$twomode_or_reflexive`. Comments state the self-dyad rule
      inline, without OpenSpec references. Verify: 2.1 passes; the 1.1
      fixture's creation rate no longer gates in the saturated actor, and
      its `n_candidates` and `avg_active_entity` equal the scratch-patch
      numbers from 1.2; every moved test from 1.2 is updated with its
      reason in the commit message.
- [ ] 2.3 Relational and panel risk-set sizes agree (D5): on a fixture both
      paths read, the relational pin's `|R|` from preprocessing equals the
      panel path's `support_grid_at()` count. Verify: the test fails on the
      commit before 2.2 (check it by reverting 2.2 locally) and passes after.
- [ ] 2.4 Verification: `air format` then `lintr` on the touched R files;
      `NOT_CRAN=true` suite green; the six baseline files PASS not SKIP.

## 3. The from-scratch reduction and its live callers (D2, D3)

- [ ] 3.1 Tests first: `sender_gate_from_mask()` equals
      `oracle_sender_gate()` per kind, one-mode and two-mode; on the 1.1
      fixture the live walk's rate gate (`walk_risk_set()`) equals the
      batch folded gate at every event (extend the existing batch-vs-replay
      assertion in `test-walk_handle.R`); a one-mode rate walk excludes the
      self-only sender, which would fail if the caller passed the rate
      engine's always-`TRUE` `twomode_or_reflexive`.
- [ ] 3.2 Implement: `sender_gate_from_mask(..., drop_diagonal)` with no
      default; callers pass the model spec's one-mode flag —
      `walk_risk_set()` and `simulation_fid_state()` (the sibling engine's
      model spec) in `R/walk_handle.R` / `R/simulate_driver.R`, and both
      validation loops in `R/model_estimate.R`. Verify: 3.1 passes, and
      `grep -n 'sender_gate_from_mask(' R/` shows every call passing the
      flag.
- [ ] 3.3 Simulation regression: a free-running run on a small one-mode
      flavored fixture whose creation sender saturates asserts that no drawn
      event has a missing receiver and that every drawn creation sender had
      a non-empty choice set. Verify: the test aborts on the commit before
      3.2 (the 2026-09-17 `merged_build_event_args()` NA) and passes after.
- [ ] 3.4 `NEWS.d/support-gate-self-exclusion.md`: which models' estimates
      can change (constrained or flavored one-mode DyNAM rates in which a
      present sender's only allowed receiver is itself), the measured size
      on the 1.1 fixture, and a note to re-preprocess such models saved with
      `preprocessed =`. No DESCRIPTION or NEWS.md edit on the branch
      (ADR-0040). Verify: the fragment follows `NEWS.d/README.md`.
- [ ] 3.5 Verification: `air format` then `lintr` on the touched R files;
      `NOT_CRAN=true` full suite green with the six baseline files PASS not
      SKIP; `openspec validate support-gate-self-exclusion` passes; tick
      tasks with commit hashes and append the session to `progress.md`.
