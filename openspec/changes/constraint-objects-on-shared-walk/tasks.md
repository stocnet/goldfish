## 0. Gates and references (D2 step one)

- [x] 0.1 Confirm `constraint-atoms-as-operands` is landed on
      `feature_simulation` with only its task 3.2 open, and the tree clean;
      `NOT_CRAN=true` suite green (not-cran-test; baselines PASS not SKIP).
      Record in `progress.md` the pre-existing order-dependent
      `test-complete_generative_spec.R` snapshot failure so it is not
      mistaken for a regression.
      — *done 2026-09-29 (goldfish-ce), at `e5ba0f8`*: only 3.2 open; tree
      clean; 9295 PASS / 0 FAIL / 4 SKIP, six baseline files PASS. The
      order-dependent snapshot failure did not occur in this run (nor at
      2.2n); recorded in `progress.md` as not reproduced.
- [x] 0.2 Add a fifth reference family to `helper-constraint-atoms.R` on the
      uncovered shape (`walk_constrained_data()`, `choice ~ inertia`,
      `support_constraint = ~ !tie(emails)`; plus a rate variant) and
      capture its byte-identity reference (mask stream + folded
      `active_sender`/`active_dyad`) with the private walk still
      authoritative. Assert `mask_call_counts()` shows the fallback was
      used, so the reference is known to come from the path being retired.
      — *done 2026-09-29 (goldfish-ce), `9892125`*: families
      `uncovered_rate` and `uncovered_choice` (one DyNAM spec, `rate = ~ 1 +
      indeg`, `choice = ~ inertia`) added to `constraint_atoms_refs.rds`
      (Alvaro: same file, not a new one); old entries `identical()`. N6
      emails every node so the rate gates a sender. The fallback pin counts
      `build_atom_maintainer()` directly (exactly one per family):
      `mask_call_counts()` sums it with the inline store, so it cannot tell
      the two apart.
- [x] 0.3 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP.
      — *done 2026-09-29, on `9892125`'s tree*: 9299 PASS / 0 FAIL / 4
      SKIP, six baseline files PASS, `_snaps/` unchanged.

## 1. Simulation harness and the baseline fixture (D5, D6, D7)

- [x] 1.1 `tests/testthat/helper-simulate-dgp.R`:
      `simulate_relational_sequence()` per D5 — exact-time rate
      (`1 + indeg + outdeg` on present-tie counts) and choice
      (`recip + trans` with the two-path count), 0/1 state,
      `withr::local_seed()`, preallocated frames, censored rows for every
      non-dependent event, `flavored =`, `constraint =`, `composition =`
      hooks (r-lib:testing-r-packages). Test: reproducible under a seed;
      the frame rules of the capability spec hold on a hand-checked tiny
      case.
      — *done 2026-09-29 (goldfish-ce), `0c4434d`*: `simulate_relational_
      sequence()` + `dgp_stocnet()`; frames rate / choice / dyad (REM);
      hooks `processes` (flavors), `exogenous` + `constraint`,
      `composition`, `unmodeled`. Rule test proven by breaking the row
      reduction, composition and the censored break. 9310 / 0 / 4, six
      baseline files PASS.
- [x] 1.2 `test-simulation_cross_validation.R`, fixture F1: goldfish rate
      ↔ `glm(poisson, offset(log(dt)))` including the `sum(log dt)`
      log-likelihood offset; REM rate ↔ dyadic `glm`; choice ↔ `mlogit`
      (`dfidx(idx = c("event", "receiver"))`) and `clogit(strata(event))`
      behind `skip_if_not_installed()`; frame identity against
      `compute_statistics(output = "data.frame")` on `(event, index_i,
      index_j)` and every statistic column; gate 1e-6. Record the measured
      deltas in a short `_references/simulation_v1/README.md` (no frozen
      numbers).
      — *done 2026-09-29 (goldfish-ce), `f55b6b5`*: frames identical for
      DyNAM rate, choice and REM; worst 2.7e-08 (mlogit), rate 2.3e-11,
      REM 2.6e-13, clogit 1.8e-14. goldfish fitted with `score_tol =
      1e-10` (default stop leaves 3.5e-07 / 8.8e-07, most of the gate).
      The frame arm caught the harness's REM `outdeg` (default is the
      receiver, `type = "alter"`). Arms proven by breaking `trans` and
      `dt`. 9339 / 0 / 4, six baseline files PASS.
- [x] 1.3 Document the coordination exclusion in that README and in the
      test file header, citing the classical README's numbers.
      — *done 2026-09-29 (goldfish-ce), `055f1a3`*: README section and test
      header; cites 0.2849 vs 0.0575 and the `coord` reference as the gate.
- [x] 1.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP.
      — *done 2026-09-29, on `055f1a3`'s tree*: 9339 PASS / 0 FAIL / 4 SKIP,
      six baseline files PASS, `_snaps/` unchanged.

## 2. Thread constraint-only objects into the shared substrate (D1, D4)

- [x] 2.0 Cost guard, recorded failing first (added 2026-09-29, per the
      per-event cost rules; design "Storage and per-event cost"): a test
      helper generates data where time and memory are measurable (at least
      1000 actors, so one double grid is 8 MB; a constraint-only layer with
      enough events to gate senders), and a guard asserts the uncovered
      constraint is preprocessed in one walk: no private atom walk is
      built, and the shared schedule visits the constraint layer's events.
      It fails on the current tree, where the private walk builds its own
      container and schedule; record the failing numbers in `progress.md`.
      — *revised 2026-09-29 after measuring*: allocation is not asserted.
      The covered twin (a term reading the layer) carries an extra
      statistic, so it allocates more (234 against 160 MB at 1000 actors),
      and threading moves the object from the private container into the
      shared one rather than removing it. Fixture sized for 2.3b: 1500
      actors, 3000 calls, 3000 constraint-layer events (uncovered 0.47 s,
      543 MB, 34.8 MB object; unconstrained 0.12 s, 146 MB, 17.5 MB).
      — *done 2026-09-29 (goldfish-ce), `95b974f`*: `constraint_cost_data()`
      in `helper-constraint-atoms.R`; guard recorded failing (one private
      walk; schedule layers `calls` only), committed with a `skip()` naming
      the state, removed by the threading commit. 9341 / 0 / 5 (the fifth
      skip is the guard), six baseline files PASS.
- [x] 2.1 `build_merged_blocks()`: compile support constraints before
      `build_shared_objects()`; append each constraint sub-plan's objects
      after every unit's objects; `shared_to_local` `NA` for them;
      `build_shared_state()` over the extended registry, atoms realized on
      `shared_src`. Test: the registry of every unconstrained and covered
      reference family is byte-identical before and after (oids, names,
      order).
      — *done 2026-09-29 (goldfish-ce), `b1b2103`, with 2.2, 2.3 and 3.1*
      (none is green alone: registered but unscheduled, the object makes
      the coverage predicate true and the atoms never move; threaded, the
      handle's refusal no longer fires). Only a changing constraint object
      joins (a static one, e.g. `allowedNet`, stays seeded from the data),
      which keeps the `rem` and `coord` registries byte-identical. Registry
      and schedule captured per family before the edit into
      `constraint_atoms_refs.rds` (`substrate`). 9382 / 0 / 4, six baseline
      files PASS; mutation (no stream append) turns byte-identity, guard
      and schedule tests red.
- [x] 2.2 `build_joint_schedule()`: append the constraint sub-plans' streams
      after all unit streams, deduplicated by stream key; `run_merged_walk()`
      covariate branch skips engines with a `NA` local id (no statistic, no
      right-censoring row, `i_total` unchanged — interim, so the threading is
      byte-identical to the task 0.2 references; the boundary lands in 2.3a,
      ADR-0103); `resolve_walk_extent()`
      exempts constraint-only rows through one named predicate with a
      comment pointing at the open extent cell (D4). Test: the joint
      schedule of every covered reference is identical; the uncovered
      fixture's constraint stream is present and ordered after the unit
      streams.
      — *done 2026-09-29, `b1b2103` (see 2.1)*
- [x] 2.3 `build_walk_recorders()` builds a store for every constraint (the
      coverage skip removed); `realize_pending_masks()` always receives
      recorders. Assert every reference family, including task 0.2's,
      matches byte-identically, and `mask_call_counts()` reports no
      `build_atom_maintainer` call.
      — *done 2026-09-29, `b1b2103` (see 2.1)*
- [x] 2.3a A constraint-only object's event closes the rate interval (added
      2026-09-29, ADR-0103; design D1 revision): each row of an object only
      a support constraint reads becomes a right-censored boundary for every
      exact-time rate fid whose mask reads the object, folding no
      statistic. Detector-first: before the edit, write the expected
      difference against the task 0.2 reference `uncovered_rate`: exactly
      the new censored rows, every existing row's statistics unchanged;
      then re-capture it into `constraint_atoms_refs.rds` with every other
      entry `identical()`. The choice families do not move (a choice reads
      its mask at its own events). Test: on the harness's constrained rate
      (the probe shape: `~ !tie(emails)` gating senders mid-interval),
      goldfish's exported rate rows equal the harness frame and the fit
      equals the Poisson oracle within 1e-6; before the edit it missed by
      6.5e-3. No frozen baseline carries such a constraint. NEWS.d bullet
      (a constrained rate's estimates move when its constraint reads a
      layer no term reads)
      — *done 2026-09-29 (goldfish-ce), `be71b07`*: per-engine constraint-only
      oids resolved before the walk; `merged_constraint_boundary()` emits the
      censored row with no statistic step. Harness oracle: frames identical,
      Poisson parity. `uncovered_rate` diff = 48 censored rows at the 8
      emails times, dependent rows unchanged. Moved and refrozen on the
      branch with Alvaro's approval (exception): parity goldens
      `user_constraint__rate`, `constraint_bound__rate` (other 38 entries
      identical). `support_mask_sparse` gate test's twin reads `contignet`.
      9422 / 0 / 4, six baseline files PASS.
- [x] 2.3c Preallocate the atom recorder's log (added 2026-09-29, Alvaro;
      found by 2.3b): `advance_constraint_atom_store()` appends one entry
      per constraint event to four vectors (`record_time`, `record_gid`,
      `record_entries`, `record_values`), and in the store each append
      copies the whole vector, so the log grows quadratically (line
      profile: ~670 MB over three runs on two lines at 1500 actors; the
      uncovered choice went 543 -> 840 MB and the uncovered rate 465 ->
      945 MB once threading routed them through it). Keep the log in
      preallocated buffers that double in capacity with a fill count, as
      the writers do, trimmed where `constraint_replay_atoms()` and the
      handle read it; no per-event copy. Guard first, recorded failing: on
      `constraint_cost_data()`, allocation of advancing the store over its
      events grows linearly, not quadratically, in the number of events
      (compare 1x and 2x the events). The log exists only for the
      post-walk replay, which 4.6 retires with it; this removes the cost
      for covered constraints too until then. Re-measure 2.3b after it.
      — *done 2026-09-29 (goldfish-ce), `9738cca`*: doubling buffers taken out
      of the store before each write, filled to `record_n`; the handle
      resets the count and keeps the buffers. Guard failed at ratio 4.0,
      passes after. Recorder allocation 62 / 246 / 981 MB -> 0.5 / 0.2 /
      0.4 MB at 1500 / 3000 / 6000 events. 9423 / 0 / 4, baselines PASS.
- [x] 2.3b Measure walk time, allocation and preprocessed object size on
      the 2.0 generated fixture (median of three warm runs,
      `bench::bench_memory()`), uncovered and covered, before 2.1 and after
      2.3a, against the design's prediction; numbers in `progress.md`.
      — *done 2026-09-29 (goldfish-ce), at `9738cca`* (median of 3 warm runs,
      detached worktrees, same script): before `05aad2f` / after:
      unconstrained choice 0.125 s 146 MB / 0.120 s 146 MB; uncovered
      choice 0.511 s 543 MB 34.8 MB / 0.490 s 595 MB 34.8 MB; unconstrained
      rate 0.114 s 94 MB / 0.110 s 95 MB; uncovered rate 0.364 s 465 MB 9.03
      MB / 0.589 s 699 MB 9.24 MB. Against the prediction: choice time
      flat (predicted a fall; allocation +9%); rate +62% time, +50%
      allocation, above the predicted bound of C/(E+C) = 50%. Cause (profile):
      `fold_active_sender_support()` after the walk costs per stored row
      (~60 KB each) and 2.3a doubles the rate's rows; 4.6 retires that fold.
      The first measurement (before 2.3c) found the quadratic recorder log.
- [x] 2.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; all reference families PASS.
      — *done 2026-09-29, on `9738cca`'s tree*: 9423 PASS / 0 FAIL / 4 SKIP,
      six baseline files PASS, every reference family PASS, `_snaps/`
      unchanged.

## 3. Handle conformance and the tied stamp (D3)

- [x] 3.1 Remove `walk_assert_covered_constraints()` and its snapshot from
      `_snaps/walk_handle.md`; replace the refusal test with a replay test
      on the uncovered fixture (`walk_replay_against_batch()`,
      `expect_gt(n_excluded, 0)`).
      — *done 2026-09-29, `b1b2103` (see 2.1)*
- [x] 3.2 Deferred atom moves: `walk_collect_atom_moves()` holds moves from
      an object event stamped at the current clock in a pending set that
      `walk_advance()` folds once the clock passes the stamp, so
      `walk_evaluate()` at `t` reads atoms strictly before `t`. Fixture: an
      `emails` event at a call's own stamp ordered before it; test
      batch-vs-replay equality there, and that disabling the deferral
      diverges (detector).
      — *revised 2026-09-29 (Alvaro, ADR-0105)*: no deferral in the handle
      (it broke free-running simulation). `walk_advance(t, inclusive =
      FALSE)` stops strictly before `t` (composition up to and including
      `t`); the time-anchored driver steps with it. Tests: the tied-stamp
      fixture replayed through the handle with that stepping equals the
      batch; a time-anchored simulation on it never draws 2 -> 4 at the tie
      and does draw 2 -> 5; with inclusive stepping it fails (detector).
      — *done 2026-09-29 (goldfish-ce), `58b806d`*: as revised. 9433 / 0 / 4,
      six baseline files PASS. Deferral patch kept in the session scratch.
- [x] 3.3 Update the stale comments: `test-support_mask_sparse.R`'s
      `mask_call_counts()` note about a flavored derived mask,
      `process-simulation` design D4's "refuses user support constraints"
      re-grounding note (a dated correction, not a rewrite, in that
      change's design — claim it first per ADR-0036), and the
      `build_atom_maintainer()` docstring.
      — *done 2026-09-29 (goldfish-ce), `527a9d2`* (process-simulation claimed
      for its D4 note).
- [x] 3.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP.
      — *done 2026-09-29, on `527a9d2`'s tree*: 9433 PASS / 0 FAIL / 4 SKIP, six
      baseline files PASS, `_snaps/` unchanged.

## 4. Retire the private walk (D2 step three)

- [x] 4.1 Delete `build_atom_maintainer()`'s state container, schedule and
      event loop (keep only what `build_constraint_atom_store()` still
      calls, or inline it), `recorder_covers_constraint()`,
      `constraint_atom_object_keys()` if unused, the fallback branch of
      `recorder_atoms_factory()`, and the `atoms_factory =
      build_atom_maintainer` default of `preprocess_pooled_support_masks()`;
      `test-constraint_atoms_operands.R`'s "no private atom walk" test now
      covers every family. Check `preprocess_support_mask()` for test
      callers; delete it or move it to a test helper per ADR-0054, and say
      which in `progress.md`.
      — *done 2026-09-29 (goldfish-ce), `0835fb3`*: per ADR-0054 the private
      walk (`build_atom_maintainer()`) and `preprocess_support_mask()` MOVED
      to `tests/testthat/helper-atom-maintainer-oracle.R` rather than being
      deleted: the mask-maintainer tests use them as an independent oracle.
      `recorder_covers_constraint()` deleted; `constraint_atom_object_keys()`
      kept (three callers); fallback and default removed. 9440 / 0 / 5 (the
      fifth skip is another session's cost-budget timing check).
- [x] 4.2 Tick `constraint-atoms-as-operands` task 3.2 with a dated note
      pointing here (claim that change first), and add a dated note under
      `process-simulation` task 2.0's gap sentence.
      — *done 2026-09-29 (goldfish-ce)*: both changes claimed; notes dated.
- [x] 4.3 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; every reference family PASS; `devtools::document()` for the
      changed signatures.
      — *done 2026-09-29, the `0835fb3` run*: 9440 / 0 / 5, six baseline
      files PASS, every reference family PASS; `document()` changed nothing
      (the signatures are `@noRd`).
- [x] 4.4a Cost guard for live masks, recorded failing first (added
      2026-09-29): on the 2.0 generated fixture, a constrained choice and a
      constrained rate plus choice allocate no `n1 x n2` object per event
      in the mask fold (total allocation bounded by a small multiple of the
      stored mask stream, not by events x grid), and no mask is realized
      after the walk. Measure first whether today's post-walk realization
      breaks the bound; if it does not, the guard is the no-post-walk
      realization count alone, and `progress.md` says so.
      — *done 2026-09-29 (goldfish-ce), `168f22a`* (guard = no post-walk
      realization or fold call; committed skipped), *unskipped 2026-09-30
      in `532471a`* with the retired functions asserted absent.
- [x] 4.4 Live masks in the batch walk, references first (D9, added
      2026-09-27; `process-simulation` 2.11b waits for this group, so a
      constrained choice's exposure is a plain running sum): capture folded `active_sender` / `active_dyad` streams and
      mask streams for every constrained reference family (rate, choice,
      rate plus choice, REM, flavored derived masks, coordination) with the
      post-walk realization still authoritative.
      — *done 2026-09-29 (goldfish-ce), `8e36702`* (joint and flavored
      families per fid; ticked 2026-09-30).
- [x] 4.5 `run_merged_walk()` builds live masks with the handle's
      construction (one builder shared by both, not a copy), collects atom
      moves per object event under D3's strictly-before rule, and folds each
      constrained fid's availability at each stored row. Assert every 4.4
      reference byte-identically.
      — *done 2026-09-29 (goldfish-ce), `926a8d9`, the mask half*: the batch
      walk builds its masks with `walk_build_live_masks()`, records atom
      moves without writing them, writes those strictly before each stored
      row and collapses each fid's flips into its row; `realize_pending_
      masks()` is not called; every 4.4 reference matches; 9448 / 0 / 6. The
      availability half is 4.5b (design D9 amendment 2026-09-29).
- [x] 4.5b Availability maintained as an effect (added 2026-09-29, Alvaro;
      design D9 amendment and its same-day revision): per exact-time rate
      fid (constrained or not) and per constrained choice / REM /
      coordination fid, the stat `active_sender` or `active_dyad` at its
      encoding, held in memory and updated in place (`write_entries()` /
      `set_entries()`), with a cache of the per-sender count of allowed
      present receivers and the count of entities at risk; fed by the live
      mask's flips and the presence changes (presence on the writer's rule,
      a change lands on the next stored row); its changes written by the
      existing writers; replaces `fold_active_sender_support()`,
      `fold_active_dyad_support()` and `build_active_dyad_point()`; the
      handle's `walk_risk_set()` reads the same stat and cache. The choice
      point stream changes: the initial grid is the true availability for
      every row and every flipped cell is emitted (a receiver presence
      change writes its column). Guard first: fold allocation per stored
      row on the 2.0 fixture does not grow with `n` (today 40-63 KB a row).
      Tests: every 4.4 reference byte-identical except the point-encoded
      choice entries; for those, assert every dependent event's sender row
      equals the old stream's, then refreeze them in
      `constraint_atoms_refs.rds` and the point-choice entries of the
      parity goldens on the branch (Alvaro approved); REM and coordination
      unchanged; handle replays equal; the 4.4a guard's fold counts reach
      zero.
      — *answered 2026-09-30 (Alvaro)*: the opportunity list is a dyadic
      mask like any other (the event sender's row set from its entry at
      its row), not a lazy special case; the rate stores the ego logical
      gate, its per-sender counts are cache; an unconstrained rate keeps
      the writer's stream (count-only effect); the handle returns the
      effect's buffer, no copy; guard = per-row slope, constrained minus
      unconstrained, 750 vs 1500 actors, growth < 4 KB.
      — *done 2026-09-30 (goldfish-ce), guard `1a08c6d`, code `ec4b3c0`*:
      `R/availability_effect.R` (variants gate, presence, alter, outer,
      point, dyad); the three folds and the opportunity fold deleted;
      `build_active_dyad_point()` kept for the DyNAM-i bridge only. Guard
      red at +33.2 / +32.5 KB per-row growth (rate / choice), green at -2 B
      / +13 B, bites when a whole-vector recompute is forced. Moved and
      refrozen (sender rows asserted equal): refs `choice`,
      `uncovered_choice`, `joint_rate_choice`/2, `flavored`/2, /4; parity
      `flavored_oracle__3`, `__4`, `user_constraint__choice`,
      `constraint_bound__choice`; `opportunity__choice` did not move;
      `test_time` snapshot sizes follow. Suite 9502 / 1 / 6, the failure
      that snapshot (accepted, its file re-run 52 / 0); six baseline files
      PASS; other `_snaps/` md5 unchanged. The 4.4a
      guard passes unskipped (kept skipped until 4.6 as written).
- [x] 4.6 Retire `realize_pending_masks()`'s replay and the post-walk folds
      (`fold_active_sender_support()`, `fold_active_dyad_support()`,
      `build_active_dyad_point()` as post-walk steps); settle the open
      question on where the observed-dyad check runs, and snapshot it.
      *Settled 2026-09-28 (D9):* the walk checks each dependent event
      against the live mask and active status and aborts there (observed
      dyad excluded, empty risk set, observed sender gated out), with
      today's messages; the whole-sequence warnings stay at the end
      — *amended 2026-09-29 (D9 amendment)*: the check reads the 4.5b
      tracker (one mask cell, one presence entry, one count); the
      whole-sequence warnings read counters the tracker accumulates, so the
      validation's per-event mask replay (`mask_cursor()`, a full mask copy
      per event, ~13 GB on the joint generated cell) goes too
      — *amended again 2026-09-29 (Alvaro)*: the exposure
      (`exposure_entity_time`, `avg_active_entity`) switches here to the
      effect's running count of entities at risk, asserted equal to
      `risk_set_exposure()` on every fixture (and the 2.11 MLE identity test
      green) before that post-walk loop is deleted; `mask_cursor()` and
      `mask_flips()`, left without readers, are deleted (ADR-0054)
      — *note 2026-09-29*: the atom recorder's log (`record_*`, made linear
      by 2.3c) exists only for the replay retired here; it goes with it,
      the batch walk keeping only per-event moves as the handle does
      — *answered 2026-09-30 (Alvaro)*: the deferred path keeps a verdict
      on the output that estimation raises (an object without one asks to
      be preprocessed again); the choice's candidate warning reads its
      sender's row per dependent event (linear, accepted)
      — *done 2026-09-30 (goldfish-ce), guard `8021b82`, code `532471a`*:
      check in the walk off the effect (rate O(1); choice one row); the
      whole-sequence warnings from crossings the effect follows;
      `validate_support_constraint()` raises a verdict (snapshots
      unchanged); exposure = the effect's running sum, asserted
      `identical()` to `risk_set_exposure()` over the whole suite, then the
      loop deleted. Retired: the post-walk validation and its readers,
      `mask_cursor()`, `mask_flips()`, the stale-mask refusal,
      `realize_pending_masks()`, the replay; the batch record already held
      only pending moves (compacted at each flush). The pooled realization
      and `flip_reader()` moved to test helpers (oracle). Guard red +16.7 KB
      / +3.18 MB, green -0.45 KB / +17.7 KB; rate bound tightened to 1 KB
      when a per-event recount slipped under 4 KB. 4.4a guard unskipped.
      Suite 9513 / 0 / 5, six baseline files PASS, `_snaps/` unchanged but
      the deleted `support_mask_sparse.md`. Budget constrained alloc 44270
      MB (recorded 44910; inside the band).
- [x] 4.7 Timing and allocation on CollegeMsg subsets (1k/5k/10k, median of
      three warm runs) for a constrained choice and a constrained rate plus
      choice, before 4.5 and after 4.6. Include the in-walk observed-event
      check against the end-of-walk check; if the in-walk check adds walk
      time the end check avoids, keep the abort at the end (D9)
      — *extended 2026-09-29*: also preprocessed object size, and the 2.0
      generated fixture beside CollegeMsg, each against the design's
      prediction (object sizes byte-identical, walk time down by the
      post-walk realization)
      — *extended 2026-09-29 (D9 amendment)*: add the generated fixture's
      joint rate plus choice cell with the observed-event validation ON
      (the timing script stubs it for CollegeMsg, whose repeats the
      constraint rejects): 8.3-9.3 s and ~14.3 GB before, ~13 GB of it the
      validation's mask copies; predicted near zero after 4.6
      — and the stored point-stream sizes of the point-choice cells before
      and after 4.5b (the stream grows where composition meets a constraint)
      — *done 2026-09-30 (goldfish-ce), tree `8890bfb`, detached worktree,
      alone*: allocation down in every cell (CollegeMsg 10k: choice 1589 ->
      801 MB, rate 1226 -> 496, joint 2423 -> 1371; generated joint 1036 ->
      597), object sizes identical (+0.03 MB where the point stream grew).
      Walk time UP 2-28% (10k joint 1.60 -> 1.89 s; generated joint 0.74 ->
      0.91 s): the prediction (time down) is refuted. Split in one session:
      the in-walk check is 5-19% (10k joint +0.31 s; it runs under the
      script's stub, which only stubs the raise, while "before" skipped
      validation entirely); without it the walk is still +4-18% (per-row
      effect stepping in R against the old vectorized post-walk folds).
      Validated joint generated cell (validation ON): 1.60 s / 14335 MB at
      `8e36702` -> 0.98 s / 597 MB, so the in-walk check costs less than the
      end-of-walk check it replaced; the abort stays in the walk (D9).
      Point streams (update cols): CollegeMsg 1k 464 -> 547, 5k 1846 ->
      2019, 10k 3506 -> 3766; generated 1722 -> 2990 (every flipped cell,
      not only event senders' rows; <= 0.09 MB).
- [x] 4.8 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; every reference family PASS.
      — *done 2026-09-30, the `532471a` run* (4.7 changed no code): 9513 /
      0 / 5, six baseline files PASS, every reference family PASS.

## 5. Fixtures F2–F5 with oracle and replay arms (D6, D8)

- [x] 5.1 F2 creation/dissolution: the harness's flavored mode (mutually
      exclusive, derived masks); per-flavor rate and choice oracles; three
      goldfish routes (container, standalone single-flavor specs,
      de-flavored layers with hand-written masks) equal within 1e-6 per
      process; frame identity per process; handle replay with exclusion.
      — *done 2026-10-01 (goldfish-ce), `25766e4`*: 8 actors, 200 events
      (111 creation, 89 dissolution); frames identical per flavor and
      family; routes equal exactly; oracle worst 5.6e-12 (dissolution
      choice). `walk_replay_against_batch()` moved to
      `helper-walk-replay.R`. Bites when both flavors derive `!tie`.
      9971 / 0 / 5, six baseline files PASS.
- [x] 5.2 F3 constraint on an object no formula reads: exogenous `emails`
      stream driving `~ !tie(emails)` on `choice ~ recip` (and a rate
      variant with the row-reduction); oracle, frame identity, handle
      replay with exclusion.
      — *done 2026-10-01 (goldfish-ce), `445e3cc`*: the gating fixture (8
      actors, 120 events, senders 1-3 email every other node); choice
      frame identical, clogit 1.7e-14; replay 240 checks, 8 excluded; the
      rate variant is 2.3a's test on the same fixture (frame + glm). Bites
      when the choice's availability effect is dropped. 10221 / 0 / 5, six
      baseline files PASS.
- [x] 5.3 F4 composition change on F1: `present`/`active` changes with
      actors leaving and returning; absent actors have no rows; presence
      flips are censored rows; rate and choice oracles; frame identity;
      handle replay with `walk_advance_presence()`.
      — *revised 2026-09-29 (ADR-0103)*: goldfish closes the rate interval
      at a presence change only from `process-simulation` task 2.11c, which
      is applied at the trunk merge, so F4's rate arm (rate oracle and rate
      frame identity) moves there. This task keeps the choice oracle and
      frame, and the handle replay, none of which depends on the boundary.
      The harness names the variable `active` (`present` is its former
      name, which manynet renames on a node frame only).
      — *done 2026-10-01 (goldfish-ce), `759873f`*: N3 and N7 leave and return
      (10 actors, 150 events); choice frame identical, no row for an absent
      node, clogit 2.0e-11; replay 300 checks, 13 excluded. Bites when the
      writer gets no receiver presence changes. 10533 / 0 / 5, six baseline
      files PASS.
- [x] 5.4 F5 one modeled flavor on F2's data: `rate = list(creation ~ ...)`,
      `choice = list(creation ~ ...)`; dissolution events as censored rows
      and state updates; equality with F2's creation blocks; oracle; handle
      replay.
      — *done 2026-10-01 (goldfish-ce), `d47139b`*: creation alone keyed on
      F2's sequence; 89 dissolution events are censored rows; equals F2's
      creation blocks; glm 3.1e-14, clogit 1.8e-12; replay 222 checks, 377
      excluded (the replay helper now maps an unflavored dependent row to
      the layer's processes). Bites when a focal-layer covariate event
      closes no interval. 10774 / 0 / 5, six baseline files PASS.
- [x] 5.5 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; the simulation README's delta table filled for every fixture.
      — *done 2026-10-01, the `d47139b` run* (5.5 adds only the README,
      `3fff624`): 10774 / 0 / 5, six baseline files PASS; the delta table
      covers F1-F5.

## 6. Close

- [x] 6.1 Re-check the three deltas against the living spec and the
      predecessor's wording: `bash .plan/opsx-spec-placement-check.sh
      constraint-objects-on-shared-walk` (the `support-constraint` block is
      written against the post-predecessor text, so a pre-fold failure there
      is expected deferral); `openspec validate
      constraint-objects-on-shared-walk --strict` clean.
      — *done 2026-10-01 (goldfish-ce)*: two stale scenarios corrected
      (Alvaro, via goldfish-0c, and the same reversal in the second): the
      `support-constraint` scenario now says a constraint-only object's
      events close every exact-time rate interval whose mask reads them
      (ADR-0103, task 2.3a), renamed "is threaded onto the shared walk";
      the `simulation-cross-validation` composition scenario now covers the
      choice and hands the rate arm to `process-simulation` 2.11c (task
      5.3's revision). Placement check OK (7 placements, no deferral);
      `validate --strict` clean.
- [x] 6.2 `NEWS.d/constraint-objects-on-shared-walk--shared-walk-constraints.md`
      (constraints on any object maintained by the one walk; handle no
      longer refuses them) and
      `NEWS.d/constraint-objects-on-shared-walk--simulation-tests.md`
      (developer-facing: the cross-validation harness). No `NEWS.md` or
      Version edit on the branch.
      — *done 2026-10-01 (goldfish-ce)*: the shared-walk fragment (created
      by 2.3a) gains the walk, handle and check bullets and the refusal of
      constrained objects from an earlier version (4.6); the
      simulation-tests fragment added. No `NEWS.md` or Version edit.
- [x] 6.3 Vault: ADR-0073 `spec:`/open questions current; dashboard
      fragment in `_dashboard-inbox/`; session note closed; the open extent
      cell handed to `observation-tail-right-censoring` in its own
      `progress.md` note.
      — *done 2026-10-01 (goldfish-ce)*: ADR-0073 discussion log brought
      current (ADR-0103, ADR-0105, ADR-0106, ADR-0108, the fixtures; its
      ADR-0068 link path repaired); dashboard fragment written; session note
      `2026-10-01-constraint-objects-availability-effect-and-fixtures`
      closed; the extent cell handed to `observation-tail-right-censoring`
      in its `progress.md` (claimed for the write, released).
- [x] 6.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      PASS not SKIP; `status: landed (feature_simulation, awaiting fold)` in
      `proposal.md`.
      — *done 2026-10-01 (goldfish-ce), suite on `d79f200`*: 10774 / 0 / 5,
      six baseline files PASS; `proposal.md` marked landed.

## 8. Conformance follow-up, groups 0-4 (added 2026-10-03)

From
`.plan/develop/constraint-objects-on-shared-walk-groups0-4-conformance.md`
(evidence in the folder of the same name), inspected at `0a99c74` over
`663096d..7b56821`. The code computes correctly: suite 10774 / 0 / 5, six
baseline files PASS, `_baselines/` untouched, the stored fields and encodings
unchanged, every detector the groups added red under ablation except two. The
spec still requires a mechanism ADR-0105 replaced, and one tie rule has no
detector. 8.1 and 8.2 block the fold; they apply before group 7.

- [ ] 8.1 **blocks**: `specs/multi-process-walk/spec.md:77-79` says atom
      moves stamped at the current clock "SHALL be held until the clock
      passes that stamp"; the handle steps with
      `walk_advance(t, inclusive = FALSE)` instead (ADR-0105). The same
      wording is in `proposal.md:69-72` and the design's D3 storage
      paragraph (`design.md:446-449`). Rewrite all three to the mechanism
      that landed, carrying every living scenario. Verify: `openspec
      validate --strict` and the placement check clean; no "held until"
      wording left.
- [ ] 8.2 **blocks**: the batch strictly-before tie rule
      (`R/support_mask_maintain.R:310`) has no detector: `<` to `<=` keeps
      every test green (8 files at `0a99c74`, all 35 `support_constraint`
      files at tip), because the tied fixtures place the constraint row
      after the call and never reach "precedes it in schedule order". The
      case is reachable: a choice-only `emails` process ahead of a
      constrained choice-only `calls` process gives N2 the allowed set
      {1, 2, 3, 5, 6} under the shipped rule and {1, 2, 3, 6} under `<=`
      (the report's `probe-tie-flush-joint.R`). Commit that fixture.
      Verify: the `<=` ablation is red.
- [ ] 8.3 **should-fix**: the D4 extent exemption has no detector (flipping
      `is_constraint_only_row()` stays green), and the 2.3a boundary rows
      reach past the exempt end (a trailing `emails` event at t = 9 stores
      a censored row with interval 4 while `end_time` stays 5). Add a
      detector for the predicate, and record in D4 and the 2.3a tick note
      that the boundary rows answer part of the extent question D4 leaves
      open; the keep-or-trim decision is handed to
      `observation-tail-right-censoring` 0.3 (note added there).
- [ ] 8.4 **should-fix**, record: the walk-time regression has no tracked
      owner. The unconstrained exact-time rate walk is 20-33 percent slower
      (0.117 s at `663096d`, 0.141-0.156 s at `0a99c74`, 0.143 s at
      `3cb92d9`), about two thirds of it `availability_step()` stepping the
      presence effect in R on every stored row; the CollegeMsg unconstrained
      joint walk 3.65 to 4.01 s. The 4.9 fast path proposed for it lives
      only in `progress.md`. Record the measurement in tasks.md and name
      the successor that owns 4.9. *Owner 2026-10-03 (Alvaro):*
      `availability-one-object-per-loop` task 6.6b, which owns
      `availability_step()` and takes the fast path with the per-row
      `c()` growth and by-name `[[fid]]` lookups. This task closes by
      recording that hand-off in D4's storage note.
- [ ] 8.5 **should-fix**: the design's Storage section is stale. The
      observed-event check reads the sender's row (about 36 KB per
      dependent event), not one mask cell (accepted only in 4.6's note);
      the predicted time drop was refuted; the 2.3a row bound is computed
      as C/(E+C) where rows grow by C/E, and the measured +56 percent sits
      above it. Amend D4's storage paragraph and the 2.3a/2.3b tick notes.
      *Added 2026-10-03* (from the `availability-one-object-per-loop`
      follow-up re-check, `.plan/develop/`
      `availability-one-object-per-loop-followup-conformance.md`, G-D; the
      hand-off its 6.7 sent to `likelihood-site-cleanup-and-cost` had no
      receiving task there and is walk code): under a flipping point mask
      the observed-event check still reads about two `n2` rows per
      dependent event. Record it in the same storage paragraph as the
      accepted row read, with the measured size.
- [ ] 8.6 **should-fix**: `proposal.md:142` says no reference moves for
      unconstrained or covered models; 4.5b moved the covered `choice`
      reference and two covered point-choice parity goldens. Correct the
      Impact line.
- [ ] 8.7 **should-fix**: the "byte-identical" reference tests
      (`test-constraint_atoms_operands.R:46,98`) use `expect_equal()`; use
      `expect_identical()`. Verify: they stay green.
- [ ] 8.8 Verification: `NOT_CRAN=true` suite green; baselines and goldens
      PASS not SKIP; `openspec validate --strict` and the placement check
      clean; a scoped re-check of gaps 1-7 by a fresh conformance-inspector
      agent. Notes not owned here: per-row `c()` growth and by-name
      `[[fid]]` lookups go with 4.9; censored rate rows restating held
      values (`preprocess-output-writers`, pre-existing); the 4.4a guard
      checks function names only (optional); the walk budget's band cannot
      see constraint-path costs (budget tooling).
      *Added 2026-10-03* (same re-check, G-H, pre-existing at `9e195dc`):
      on the 1500-actor rate fixture `~ indeg(emails) >= 0` excludes
      nothing yet costs 0.61 s and 822 MB against 0.127 s and 95 MB
      unconstrained, about 0.15 MB per event in the constraint atom's
      per-event maintenance. Owner pending Alvaro (this change or
      `effect-term-registry`).


## 7. Conformance follow-up, groups 5-6 (added 2026-10-02)

From
`.plan/develop/constraint-objects-on-shared-walk-groups5-6-conformance.md`.
Groups 5-6 conform: every fixture arm has a test, every detector bites under
its named mutation, the suite reproduces at 10774 / 0 / 5. Three small items
before the fold.


- [ ] 7.1 **should-fix**: OpenSpec references in code and documentation:
      the comment at `tests/testthat/test-simulation_cross_validation.R:648-649`
      names "process-simulation 2.11c", and
      `tests/testthat/_references/simulation_v1/README.md:88,92` cite
      ADR-0103 and task 2.11c. Rewrite by hand to state the rule (presence
      changes close the rate interval; the rate arm waits on that rule) with
      no artifact pointer.
- [ ] 7.2 **note**, record only: commit `759873f` (F4) also deleted
      `openspec/changes/exact-time-diagnostic-cost/` (389 lines) without
      saying so; the deletion belongs to `likelihood-site-cleanup-and-cost`,
      which absorbed that change (its proposal says so; recoverable at
      `e42d05e`). Noted here for whoever folds the branch.
- [ ] 7.3 Handed to `estimation-contract-matrix` 2.3a: the F2 standalone
      and de-flavored routes are compared with the container, not the
      oracle (a 2e-6 bound where the spec says 1e-6 each; measured gap 0),
      and `skip_if_not_installed("survival")` sits inside the F2 loop so a
      missing package would skip the `glm` arms the design says never skip.
