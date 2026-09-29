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

- [ ] 2.0 Cost guard, recorded failing first (added 2026-09-29, per the
      per-event cost rules; design "Storage and per-event cost"): a test
      helper generates data where time and memory are measurable (at least
      1000 actors, so one double grid is 8 MB; a constraint-only layer with
      enough events to gate senders), and a guard asserts the uncovered
      constraint is preprocessed in one walk: one `fetch_events()` pass and
      one state container, and allocation within half a grid of the covered
      twin (the same constraint with a term reading the layer). It fails on
      the current tree, where the private walk builds its own container
      and schedule; record the failing numbers in `progress.md`.
- [ ] 2.1 `build_merged_blocks()`: compile support constraints before
      `build_shared_objects()`; append each constraint sub-plan's objects
      after every unit's objects; `shared_to_local` `NA` for them;
      `build_shared_state()` over the extended registry, atoms realized on
      `shared_src`. Test: the registry of every unconstrained and covered
      reference family is byte-identical before and after (oids, names,
      order).
- [ ] 2.2 `build_joint_schedule()`: append the constraint sub-plans' streams
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
- [ ] 2.3 `build_walk_recorders()` builds a store for every constraint (the
      coverage skip removed); `realize_pending_masks()` always receives
      recorders. Assert every reference family, including task 0.2's,
      matches byte-identically, and `mask_call_counts()` reports no
      `build_atom_maintainer` call.
- [ ] 2.3a A constraint-only object's event closes the rate interval (added
      2026-09-29, ADR-0103; design D1 revision): each row of an object only
      a support constraint reads becomes a right-censored boundary for every
      exact-time rate fid whose mask reads the object, folding no
      statistic. Detector-first: before the edit, write the expected
      difference against the task 0.2 references (the `rate` family, whose
      `call_network` no formula term reads, and `uncovered_rate`): exactly
      the new censored rows, every existing row's statistics unchanged;
      then re-capture both into `constraint_atoms_refs.rds` with every other
      entry `identical()`. The choice families do not move (a choice reads
      its mask at its own events). Test: on the harness's constrained rate
      (the probe shape: `~ !tie(emails)` gating senders mid-interval),
      goldfish's exported rate rows equal the harness frame and the fit
      equals the Poisson oracle within 1e-6; before the edit it missed by
      6.5e-3. No frozen baseline carries such a constraint. NEWS.d bullet
      (a constrained rate's estimates move when its constraint reads a
      layer no term reads)
- [ ] 2.3b Measure walk time, allocation and preprocessed object size on
      the 2.0 generated fixture (median of three warm runs,
      `bench::bench_memory()`), uncovered and covered, before 2.1 and after
      2.3a, against the design's prediction; numbers in `progress.md`.
- [ ] 2.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; all reference families PASS.

## 3. Handle conformance and the tied stamp (D3)

- [ ] 3.1 Remove `walk_assert_covered_constraints()` and its snapshot from
      `_snaps/walk_handle.md`; replace the refusal test with a replay test
      on the uncovered fixture (`walk_replay_against_batch()`,
      `expect_gt(n_excluded, 0)`).
- [ ] 3.2 Deferred atom moves: `walk_collect_atom_moves()` holds moves from
      an object event stamped at the current clock in a pending set that
      `walk_advance()` folds once the clock passes the stamp, so
      `walk_evaluate()` at `t` reads atoms strictly before `t`. Fixture: an
      `emails` event at a call's own stamp ordered before it; test
      batch-vs-replay equality there, and that disabling the deferral
      diverges (detector).
- [ ] 3.3 Update the stale comments: `test-support_mask_sparse.R`'s
      `mask_call_counts()` note about a flavored derived mask,
      `process-simulation` design D4's "refuses user support constraints"
      re-grounding note (a dated correction, not a rewrite, in that
      change's design — claim it first per ADR-0036), and the
      `build_atom_maintainer()` docstring.
- [ ] 3.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP.

## 4. Retire the private walk (D2 step three)

- [ ] 4.1 Delete `build_atom_maintainer()`'s state container, schedule and
      event loop (keep only what `build_constraint_atom_store()` still
      calls, or inline it), `recorder_covers_constraint()`,
      `constraint_atom_object_keys()` if unused, the fallback branch of
      `recorder_atoms_factory()`, and the `atoms_factory =
      build_atom_maintainer` default of `preprocess_pooled_support_masks()`;
      `test-constraint_atoms_operands.R`'s "no private atom walk" test now
      covers every family. Check `preprocess_support_mask()` for test
      callers; delete it or move it to a test helper per ADR-0054, and say
      which in `progress.md`.
- [ ] 4.2 Tick `constraint-atoms-as-operands` task 3.2 with a dated note
      pointing here (claim that change first), and add a dated note under
      `process-simulation` task 2.0's gap sentence.
- [ ] 4.3 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; every reference family PASS; `devtools::document()` for the
      changed signatures.
- [ ] 4.4a Cost guard for live masks, recorded failing first (added
      2026-09-29): on the 2.0 generated fixture, a constrained choice and a
      constrained rate plus choice allocate no `n1 x n2` object per event
      in the mask fold (total allocation bounded by a small multiple of the
      stored mask stream, not by events x grid), and no mask is realized
      after the walk. Measure first whether today's post-walk realization
      breaks the bound; if it does not, the guard is the no-post-walk
      realization count alone, and `progress.md` says so.
- [ ] 4.4 Live masks in the batch walk, references first (D9, added
      2026-09-27; `process-simulation` 2.11b waits for this group, so a
      constrained choice's exposure is a plain running sum): capture folded `active_sender` / `active_dyad` streams and
      mask streams for every constrained reference family (rate, choice,
      rate plus choice, REM, flavored derived masks, coordination) with the
      post-walk realization still authoritative.
- [ ] 4.5 `run_merged_walk()` builds live masks with the handle's
      construction (one builder shared by both, not a copy), collects atom
      moves per object event under D3's strictly-before rule, and folds each
      constrained fid's availability at each stored row. Assert every 4.4
      reference byte-identically.
- [ ] 4.6 Retire `realize_pending_masks()`'s replay and the post-walk folds
      (`fold_active_sender_support()`, `fold_active_dyad_support()`,
      `build_active_dyad_point()` as post-walk steps); settle the open
      question on where the observed-dyad check runs, and snapshot it.
      *Settled 2026-09-28 (D9):* the walk checks each dependent event
      against the live mask and active status and aborts there (observed
      dyad excluded, empty risk set, observed sender gated out), with
      today's messages; the whole-sequence warnings stay at the end
- [ ] 4.7 Timing and allocation on CollegeMsg subsets (1k/5k/10k, median of
      three warm runs) for a constrained choice and a constrained rate plus
      choice, before 4.5 and after 4.6. Include the in-walk observed-event
      check against the end-of-walk check; if the in-walk check adds walk
      time the end check avoids, keep the abort at the end (D9)
      — *extended 2026-09-29*: also preprocessed object size, and the 2.0
      generated fixture beside CollegeMsg, each against the design's
      prediction (object sizes byte-identical, walk time down by the
      post-walk realization)
- [ ] 4.8 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; every reference family PASS.

## 5. Fixtures F2–F5 with oracle and replay arms (D6, D8)

- [ ] 5.1 F2 creation/dissolution: the harness's flavored mode (mutually
      exclusive, derived masks); per-flavor rate and choice oracles; three
      goldfish routes (container, standalone single-flavor specs,
      de-flavored layers with hand-written masks) equal within 1e-6 per
      process; frame identity per process; handle replay with exclusion.
- [ ] 5.2 F3 constraint on an object no formula reads: exogenous `emails`
      stream driving `~ !tie(emails)` on `choice ~ recip` (and a rate
      variant with the row-reduction); oracle, frame identity, handle
      replay with exclusion.
- [ ] 5.3 F4 composition change on F1: `present`/`active` changes with
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
- [ ] 5.4 F5 one modeled flavor on F2's data: `rate = list(creation ~ ...)`,
      `choice = list(creation ~ ...)`; dissolution events as censored rows
      and state updates; equality with F2's creation blocks; oracle; handle
      replay.
- [ ] 5.5 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; the simulation README's delta table filled for every fixture.

## 6. Close

- [ ] 6.1 Re-check the three deltas against the living spec and the
      predecessor's wording: `bash .plan/opsx-spec-placement-check.sh
      constraint-objects-on-shared-walk` (the `support-constraint` block is
      written against the post-predecessor text, so a pre-fold failure there
      is expected deferral); `openspec validate
      constraint-objects-on-shared-walk --strict` clean.
- [ ] 6.2 `NEWS.d/constraint-objects-on-shared-walk--shared-walk-constraints.md`
      (constraints on any object maintained by the one walk; handle no
      longer refuses them) and
      `NEWS.d/constraint-objects-on-shared-walk--simulation-tests.md`
      (developer-facing: the cross-validation harness). No `NEWS.md` or
      Version edit on the branch.
- [ ] 6.3 Vault: ADR-0073 `spec:`/open questions current; dashboard
      fragment in `_dashboard-inbox/`; session note closed; the open extent
      cell handed to `observation-tail-right-censoring` in its own
      `progress.md` note.
- [ ] 6.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      PASS not SKIP; `status: landed (feature_simulation, awaiting fold)` in
      `proposal.md`.
