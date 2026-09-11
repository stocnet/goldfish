**The case is duplication, not speed.** Group 6 of the predecessor already took
the sharing win; what remains is about 0.13 s on a 1500-event constrained model.
Do not report this change as a performance change — it would be evaluated on the
wrong axis and descoped again when the number disappoints.

`NOT_CRAN=true` with the frozen baselines PASS not SKIP at every commit. No
baseline model carries a constraint, so `test-preprocess_parity.R`,
`test-support_mask_maintain.R` and byte-identity of `active_sender` /
`active_dyad` are the real detectors.

## 1. Pin what must not move (design D3)

- [ ] 1.1 Assert an unconstrained model's `plan$effects` is identical before and
      after this change. Atoms enter the plan only when a constraint is present,
      which is a reasonable expectation and exactly the kind of expectation the
      predecessor found to be false about plan construction.
- [ ] 1.2 Capture the constrained reference: `active_sender` / `active_dyad` and
      the mask stream for a rate model, a choice model, a REM model and a
      coordination model, each with a constraint. These are the byte-identity
      targets for every later task.
- [ ] 1.3 Detector for the two-layer DAG (design D2): an atom whose inputs read
      the availability mask is rejected at parse time, with its current message.
      It passes today; it must still pass when atoms are ordinary plan effects,
      and that is the one invariant this move could quietly lose.
- [ ] 1.4 Verification: `NOT_CRAN=true` green, baselines PASS not SKIP.

## 2. Atoms enter the plan, beside the private walk (design D4)

- [ ] 2.1 `compile_support_constraint()` emits the atoms as plan effects and
      `augment_constraints()` writes `role = "constraint"` into `plan$effects`
      rather than into the sub-plan's own table. The private walk stays and
      stays authoritative.
- [ ] 2.2 Exclude constraint-role effects from `initialStats` and from the
      output statistic columns, while their state stays live across the loop.
      This is the requirement's own wording and the point of the role tag.
- [ ] 2.3 Resolve the kernel mismatch the design's open question names: atoms
      always use the dyad kernel, chosen by the constraint rather than by the
      estimated sub-model, while a sender-indexed model's plan is sender-shaped.
      Record what you find before changing anything.
- [ ] 2.4 Tests: the plan's atoms and the private walk's atoms hold the same
      value at every event, for each of the four families in task 1.2. This
      equivalence is the whole reason the walk is kept for one more step.
- [ ] 2.5 Verification: `NOT_CRAN=true`, baselines PASS not SKIP; task 1.1's
      unconstrained plan assertion still holds.

## 3. The mask reads the plan's atoms, and the private walk goes

- [ ] 3.1 Point the mask maintainer at the plan's atom state. The mask stays a
      stream; only where it reads its atoms changes.
- [ ] 3.2 Verify byte-identity against every reference captured in task 1.2,
      then retire `build_atom_maintainer()`'s state container, schedule and
      event loop.
- [ ] 3.3 Confirm windowed constraint atoms still resolve. Their derivations
      already enter `plan$derivations`, so their expiry streams should ride the
      shared schedule with no constraint-specific branch — wiring that exists
      today and is unused. Assert a windowed constraint needs no realization
      step of its own.
- [ ] 3.4 Confirm the task 1.3 DAG detector still passes, now that atoms and
      main effects share one table.
- [ ] 3.5 Verification: `NOT_CRAN=true`, baselines PASS not SKIP; a constrained
      model is walked once, asserted by counting the walks rather than inferred
      from a timing.

## 4. Close

- [ ] 4.1 Confirm `support-constraint`'s "Constraint atoms are non-estimated
      operands" requirement is met as written, including that `role =
      "constraint"` in `plan$effects` is now read and not merely written.
- [ ] 4.2 ADR on where the two-layer DAG check lives once atoms are plan
      effects. Claim the id in the vault ledger before drafting.
- [ ] 4.3 Report what this bought, honestly: the walk count, and the time, which
      is expected to be small. If it is smaller than 0.13 s, say so.
- [ ] 4.4 `NEWS.d/` fragment under Internal.
- [ ] 4.5 `bash .plan/opsx-spec-placement-check.sh constraint-atoms-as-operands`
      and `openspec validate constraint-atoms-as-operands --strict` clean.
- [ ] 4.6 Final verification: full `NOT_CRAN=true` suite green, baselines PASS
      not SKIP, `devtools::document()` if any roxygen changed.
