## 0. Gates and references (D2 step one)

- [ ] 0.1 Confirm `constraint-atoms-as-operands` is landed on
      `feature_simulation` with only its task 3.2 open, and the tree clean;
      `NOT_CRAN=true` suite green (not-cran-test; baselines PASS not SKIP).
      Record in `progress.md` the pre-existing order-dependent
      `test-complete_generative_spec.R` snapshot failure so it is not
      mistaken for a regression.
- [ ] 0.2 Add a fifth reference family to `helper-constraint-atoms.R` on the
      uncovered shape (`walk_constrained_data()`, `choice ~ inertia`,
      `support_constraint = ~ !tie(emails)`; plus a rate variant) and
      capture its byte-identity reference (mask stream + folded
      `active_sender`/`active_dyad`) with the private walk still
      authoritative. Assert `mask_call_counts()` shows the fallback was
      used, so the reference is known to come from the path being retired.
- [ ] 0.3 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP.

## 1. Simulation harness and the baseline fixture (D5, D6, D7)

- [ ] 1.1 `tests/testthat/helper-simulate-dgp.R`:
      `simulate_relational_sequence()` per D5 — exact-time rate
      (`1 + indeg + outdeg` on present-tie counts) and choice
      (`recip + trans` with the two-path count), 0/1 state,
      `withr::local_seed()`, preallocated frames, censored rows for every
      non-dependent event, `flavored =`, `constraint =`, `composition =`
      hooks (r-lib:testing-r-packages). Test: reproducible under a seed;
      the frame rules of the capability spec hold on a hand-checked tiny
      case.
- [ ] 1.2 `test-simulation_cross_validation.R`, fixture F1: goldfish rate
      ↔ `glm(poisson, offset(log(dt)))` including the `sum(log dt)`
      log-likelihood offset; REM rate ↔ dyadic `glm`; choice ↔ `mlogit`
      (`dfidx(idx = c("event", "receiver"))`) and `clogit(strata(event))`
      behind `skip_if_not_installed()`; frame identity against
      `compute_statistics(output = "data.frame")` on `(event, index_i,
      index_j)` and every statistic column; gate 1e-6. Record the measured
      deltas in a short `_references/simulation_v1/README.md` (no frozen
      numbers).
- [ ] 1.3 Document the coordination exclusion in that README and in the
      test file header, citing the classical README's numbers.
- [ ] 1.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP.

## 2. Thread constraint-only objects into the shared substrate (D1, D4)

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
      right-censoring row, `i_total` unchanged); `resolve_walk_extent()`
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
