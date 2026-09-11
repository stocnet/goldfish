**Test-driven, and the order is load-bearing.** Group 1 records the baselines and
group 2 writes every fixture before any code moves. A fixture that does not fail
today, or does not assert something no current test asserts, is not a detector.
The frozen 1e-6 baselines are the floor but NOT the detector here: no baseline
model carries a constraint, so they cannot see any of this.

**Byte-identity is the acceptance test throughout.** `active_sender` /
`active_dyad` are what estimation consumes and their shape does not change, so
every phase asserts them byte-identical against the pre-change implementation.

## 1. Baselines

- [x] 1.1 Write `.plan/sp/constraint_baseline.R`, a re-runnable grid recording
      **wall time, peak memory (`bench::bench_memory`), and stored-object size**
      for each cell. Families: DyNAM rate, DyNAM choice, REM, DyNAM
      choice-coordination. Each constrained and unconstrained. Two sizes: Social
      Evolution (84 actors) and a CollegeMsg subset (1899 actors) small enough
      that a constrained cell finishes, since the constrained rate loop is
      currently 362 s at 10k events. Record per cell: seconds, peak MB, size of
      the whole preprocessed object, size of `support_mask$support`, size of
      `active_sender_update` / `active_dyad_update`, number of stored mask
      entries, and number of mask entries that ever change.
- [x] 1.2 Record the grid in `.plan/sp/constraint_baseline_2026-09.md` and in
      `progress.md`. These figures are the contrast the change reports against
      and are NOT re-measured later. Known starting points to reproduce or
      correct: rate loop at 10k events 0.349 s unconstrained against 362 s
      constrained; `support_mask$support` 21.90 MB at 3000 events; 98 percent of
      the preprocessed object; 5,697,000 entries stored against 4 that change
      under `~ indeg(msg) < 20`.
- [x] 1.3 Record the atom-pool count per substrate: how many atom maintainers a
      constrained model builds and how many times the mask is evaluated, for the
      recipe loops and for the merged walk. Today both report two maintainers and
      880 evaluations on a two-family Social Evolution model; the shared-mask
      requirement makes that one and 440.
- [x] 1.4 Verification: `NOT_CRAN=true` suite green at the branch start, frozen
      baselines and C++ goldens PASS not SKIP. This is the reference point every
      later commit is read against.

## 2. Fixtures, written before any code moves

- [x] 2.1 **One layer, rate and choice, one mask.** A specification carrying both
      sub-models for one layer with a `support_constraint`. Assert the atom pool
      is seeded once and the mask maintained once, and that the rate gate equals
      a from-scratch `rowSums(mask & receiver-availability) > 0` on the same
      mask. Fails today: two maintainers, and the gate is recomputed per event by
      row reduction.
- [x] 2.2 **Creation and deletion flavors with a state constraint plus an
      exogenous one.** Two flavors on one layer, so the derived complementary
      `~ !tie(net)` / `~ tie(net)` pair applies, AND a user `support_constraint`
      on a second, exogenous tie layer whose ties change over time. The mask then
      moves for two independent reasons, one endogenous and one exogenous.
      Assert one mask per flavor covering its sub-models, and the mask timeline
      byte-identical to today's.
- [x] 2.3 **Mixed-kind atoms.** A constraint combining a scalar atom, an
      ego-axis atom and an alter-axis atom. Assert the mask's kind is the
      axis-union of the three, that storage is at that kind, and that no dense
      n1 x n2 value is allocated when the union is separable. The living spec's
      "ego-gate constraint stores at ego kind" scenario is the partial case;
      this extends it to a genuine mixture.
- [x] 2.4 **Composition change under constraint.** Nodes entering and leaving
      while a constraint is active. Assert the maintained availability collapses
      both the constraint and the time-varying presence, and that an absent node
      is excluded regardless of what the constraint allows.
- [x] 2.5 **No per-event dense list.** Assert directly the living-spec scenario
      that fails today: after preprocessing a point-kind constraint over a full
      sequence, the preprocessed object carries the mask as one initial value
      plus a flat update buffer, not one dense matrix per event.
- [x] 2.6 **The incremental mask equals the from-scratch mask.** Over a full
      sequence, after every event the maintained mask is elementwise equal to a
      from-scratch evaluation of the constraint tree on the atoms' current
      values. This is the living spec's own scenario and it is the correctness
      backbone of the whole change.
- [x] 2.7 Verification: run the new fixtures and record which fail and why, in
      `progress.md`, with each failure mapped to the task that fixes it.

## 3. The shared core, proven on interaction operands (design D1, D2)

- [x] 3.1 Extract the kind-shaped vocabulary: `kind_length()`,
      `project_value()`, `reduce_value()`, `project_entries()`,
      `write_entries()`, `emit_crossings()`. `support_to_grid()` and
      `expand_operand_update()` become the `to = point` cases of the projections;
      `support_from_grid()` becomes the reduction; `set_matrix_cells()` grows to
      cover the buffer kinds. Extraction only, no behavior change.
- [x] 3.2 Route interaction operands on the dyad branch through the core, stored
      at their `bcast_kind` rather than expanded to dense cells. The sender
      branch is the reference shape and should end up calling the same functions.
- [x] 3.3 Tests: an alter operand's update writes one vector entry and builds no
      cell matrix; the interaction product and its update stream are
      byte-identical; a point operand still stores dense. `tracemem` shows no
      duplication of the operand buffer across a multi-event walk.
- [x] 3.4 Verification: `NOT_CRAN=true`, frozen baselines and C++ goldens PASS
      not SKIP. Record the interaction-model timings against the group 1 grid.

## 4. Constraint atoms at their kind (design D3)

- [x] 4.1 Store each atom at `atom_kinds[gid]` and write it in place, removing
      `expand_operand_update()` from the atom path and the per-event copy of the
      atom matrix.
- [x] 4.2 Remove the `<<-` state write in `apply_atom_event()`, which is
      ADR-0057's copy-on-modify pattern surviving in this file. It comes out
      under ADR-0059's invariant, so it needs the same aliasing clearance and an
      identity test, not a value test.
- [x] 4.3 Tests: `tracemem` reports no duplication of the atom buffers or the
      constraint's state across a multi-event walk; `bench::bench_memory()` on
      the group 1 fixture falls far below the recorded 30 MB per event.
- [x] 4.4 Verification: fixtures 2.3 and 2.6 go green; the folded availability
      objects stay byte-identical; baselines PASS.

## 5. The mask as a stream (design D4, D5)

- [x] 5.1 Implement mixed-kind elementwise evaluation: project each atom into the
      mask's kind through the core, evaluate the tree only at the entries the
      changed atoms project onto, and append flips to the mask's update buffer.
      This is the one genuinely new piece of logic in the change.
- [x] 5.2 Emit the mask as an initial value plus a flat update buffer with a
      per-event pointer and its kind, replacing the snapshot list produced by
      `preprocess_support_mask()`.
- [x] 5.3 Maintain the DyNAM-rate sender gate as the per-sender allowed-receiver
      counter the living spec requires: a mask flip adjusts one counter and a
      sender crossing is emitted only on a zero crossing. Delete the per-event
      `rowSums()` row reduction in `fold_active_sender_support()`.
- [x] 5.4 Fold the availability incrementally from the mask's flip stream,
      collapsing time-varying composition into the maintained object rather than
      intersecting a snapshot with a presence vector afterwards.
- [x] 5.5 Tests: fixtures 2.1, 2.4, 2.5 and 2.6 go green; a symmetrised
      coordination mask is maintained at point kind whatever its atoms say
      (`m & t(m)` of a row-constant mask is an outer product); the availability
      objects stay byte-identical throughout.
- [x] 5.6 Verification: `NOT_CRAN=true`, baselines PASS. Re-measure the group 1
      grid and record the contrast. **The memory and time wins land here**, so a
      stop after this task still delivers most of the value.

## 6. One mask per process (design D6)

- [x] 6.1 Compile the constraint once per `(layer, flavor)` process rather than
      once per sub-model family, and have every sub-model of that process read
      the shared maintained mask.
- [x] 6.2 A constraint whose atoms are all sender-axis maintains no dyad mask:
      its kind is ego or scalar and the gate is the mask itself.
- [x] 6.3 Tests: fixtures 2.1 and 2.2 assert one maintainer and one evaluation
      stream per process; task 1.3's counts halve on both substrates.
- [x] 6.4 Verification: `NOT_CRAN=true`, baselines PASS; the merged walk's
      constrained ratio re-measured against `preprocess-one-walk` task 0.9's
      1.16.

## 7. Atoms as operands of the main plan (design D8) — DESCOPED to a successor

**Descoped 2026-09-10, with the user, after group 6 measured.** The design names
this step as removable without losing the wins ("It can be descoped to a
successor without losing the memory and time wins, at the cost of leaving the
two-maintainers-per-model duplication in place"), and the measurements say the
remaining duplication is worth about 0.13 s on a 1500-event constrained model,
against 0.29 s of total constrained overhead. Group 6 already gives the merged
walk one atom pool per process, so what group 7 would still buy is retiring the
private walk itself rather than sharing it.

The living-spec requirement it implements — constraint atoms as non-estimated
operands carrying `role = "constraint"` — remains unmet and is the successor's
reason to exist. None of the tasks below were done.

- [ ] 7.1 Compile constraint atoms into `plan$effects` with
      `role = "constraint"`, which the column already carries but never holds,
      excluded from `initialStats` and from the output columns while their state
      stays live. This is the living spec's "Constraint atoms are non-estimated
      operands" requirement, unimplemented since it was written.
- [ ] 7.2 Retire `build_atom_maintainer()`'s private state container, schedule
      and event loop; the atoms ride the main walk. `preprocess_pooled_support_
      masks()` has nothing left to pool and retires with it.
- [ ] 7.3 Confirm windowed constraint atoms still resolve: their derivations
      already enter `plan$derivations` (`preprocess-one-walk` task 0.5b), so
      their expiry streams should ride the shared schedule with no
      constraint-specific branch. Assert a windowed constraint on the merged walk
      no longer needs its own realization step.
- [ ] 7.4 Tests: an availability-dependent atom is still rejected at parse time
      (the two-layer DAG the spec requires); all group 2 fixtures stay green.
- [ ] 7.5 Verification: `NOT_CRAN=true`, baselines PASS; a constrained model is
      walked once, asserted by the task 1.3 counters.

## 8. Drop the snapshot list (design D9)

- [x] 8.1 Rewrite `validate_support_constraint()` to advance the flat stream
      instead of indexing `support[[e]]`. It is already a sequential per-event
      walk, so this is a change of source, not of logic.
- [x] 8.2 Remove `support_mask$support` from the preprocessed object, gated on
      `prep_version`.
- [x] 8.3 Tests: the fail-fast validations (empty risk set, observed dyad
      excluded, sender gated out, never-a-candidate warning) are unchanged in
      message and in trigger condition; snapshot tests updated where the object
      size is reported.
- [x] 8.4 Verification: `NOT_CRAN=true`, baselines PASS.

## 10. Clean up after the change, before it archives

**Added 2026-09-11 from the post-landing conformance review
(`.plan/develop/review.md`).** Every item here is this change's OWN debris: code
it made dead, a detector it defanged, references it introduced, a claim it left
stale, and a fold it rewrote. The archive is append-only, so this is the last
moment for the artifact half; and no successor change touches
`R/support_mask_maintain.R`, so the code half would be orphaned if it waited.

- [x] 10.1 (review gap 11, artifact) `proposal.md` claimed no `src/` change and
      the change made one. Corrected in place, with the reasoning that held and
      the part that did not.
- [x] 10.2 (review gap 5, code) `fold_active_sender_support()` is handed
      `active_dyad_init` and never reads the receiver composition stream, so the
      sender axis is walked per event and the receiver axis is frozen. A sender
      whose only allowed receivers depart stays at risk. **Probed on Fisheries:
      the receiver set really moves (137 to 151 actors) and with
      `~ tie(contignet)` one event's gate differs by one sender.** Exposure is
      kind-dependent and the fixture must be POINT-kind: a separable mask shares
      its allowed set across senders, so all of it would have to depart, and an
      alter-kind fixture passes while the bug stands. Write that detector first.
      Two sites: `apply_receiver_count_flips()` must consume receiver crossings,
      and `validate_support_constraint()`'s rate branch reads the same frozen
      vector through `sender_gate_from_mask()`.
- [ ] 10.3 (review gap 8, code and test) `eval_constraint_mask()`,
      the maintainer's `atom_matrix()` member and `support_from_grid()` outside
      tests have no production caller — they are alive only because they call
      each other. `mask_call_counts()` counts `eval_constraint_mask`, so its
      `expect_lte()` has passed vacuously since the mask maintainer replaced it;
      re-point it at the honest unit and restate the bound. Two `skip_if()`
      gates no longer fire. The file header of `R/support_mask_maintain.R` still
      describes dense atoms and a deferred axis-union optimization; rewrite it
      BY HAND, do not delete it.
- [ ] 10.4 (review gap 9, test) Four OpenSpec and ADR references sit in test
      source against the standing rule, all introduced here: two `ADR-0059`
      mentions and the two task numbers on the dead gates. Replace the ADR
      mentions with the invariant in code terms — the buffer is materialized
      fresh at seeding, lives in exactly one environment binding, and is
      therefore safe to write in place. `R/preprocess_joint.R:698` carries
      `design D5 / D8a` from an earlier change; sweep it if the file is open,
      otherwise leave it and note it.
- [ ] 10.5 Verification: `NOT_CRAN=true`, frozen baselines and C++ goldens PASS
      not SKIP. Then archive.

## 9. Report

- [x] 9.1 Re-run the group 1 grid and tabulate before against after for every
      cell: time, peak memory, stored-object size, mask entries stored, atom-pool
      count. Report DyNAM rate, DyNAM choice, REM and DyNAM choice-coordination
      separately, since they differ in what they pay.
- [x] 9.2 Record what the change does NOT fix, with numbers: whatever remains of
      the constrained-versus-unconstrained gap, and where it now sits.
- [x] 9.3 `NEWS.d/` fragment under Internal (ADR-0040; no `NEWS.md`, no Version
      bump on a feature branch).
- [x] 9.4 Update `preprocess-one-walk` task 0.9's write-up to correct its stated
      cause for the constrained cost, and re-read its constrained gate cell now
      that it measures the substrate rather than the mask.
