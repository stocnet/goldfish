## 1. Gates and the before timing

- [x] 1.1 Tree clean on `feature_simulation`; `NOT_CRAN=true` suite green
      with the six frozen baseline files PASS not SKIP (not-cran-test).
      Record the counts in `progress.md`
      — done 2026-09-28 (goldfish-ec): `da57be3` 9089 PASS / 0 FAIL / 4 SKIP,
      six baseline files PASS.
- [x] 1.2 Before timing on CollegeMsg subsets (1k/5k/10k, median of three
      warm runs after a timed warm-up, `bench::bench_memory()` for
      allocation): constrained choice (`choice ~ inertia + recip + trans`,
      `support_constraint = ~ !tie(msg)`) and constrained rate plus choice
      in one walk, with the constraint validation stubbed in the script
      (CollegeMsg repeats dyads) and the same cells without `trans`.
      Script in `.plan/sp/`, numbers in `progress.md`
      — done 2026-09-28: `.plan/sp/mask_readers_timing_2026-09-27.R`, run
      alone on `da57be3` (constrained choice 15.085 s at 10k)

## 2. Equality tests first (D4)

- [x] 2.1 Tests (testthat 3e) comparing `support_row()`,
      `sender_gate_from_mask()` and `initial_receiver_count()` against a
      grid-form oracle built in the test helper, on every mask kind (0-3),
      on one-mode and non-square two-mode shapes, diagonal dropped and
      kept, some receivers absent. They pass against today's code (they
      pin behavior, not the change)
      — done 2026-09-28, commit `3cae54b`; each proved to bite (the gate
      test only after hand-built gated senders were added)
- [x] 2.2 Capture the folded `active_sender` / `active_dyad` streams and
      the validation messages of the constrained reference families
      (`helper-constraint-atoms.R`) as byte-identity references
      — done 2026-09-28, commit `3cae54b`: the folded streams were already
      frozen (`fixtures/constraint_atoms_refs.rds`); added a snapshot of
      every validation verdict's full text

## 3. Readers without the grid (D1, D2)

- [x] 3.1 `support_row()` kind 0 reads the row by strided index. Verify:
      2.1 and 2.2 pass; an allocation test shows no grid-sized allocation
      per row read on a 300 x 300 mask
      — done 2026-09-28, commit `ec14a63` (red: 108.8 MB vs 10.8 MB)
- [x] 3.2 `sender_gate_from_mask()` and `initial_receiver_count()` count
      kind 0 with `.rowSums()` on the vector, subsetting present columns by
      linear index only when a receiver is absent. Verify: 2.1 and 2.2
      pass; `air format` then `lintr` on the touched files
      — done 2026-09-28, commit `b2bcad5`, `point_receiver_count()` (red:
      36.9 / 54.8 MB vs 1.8 MB); lint counts equal HEAD's

## 4. Validation gate kept from flips (D3)

- [x] 4.1 `validate_support_constraint()` keeps per-sender allowed-receiver
      counts seeded by `initial_receiver_count()` and adjusted from
      `mask_flips()` and receiver presence changes, through the helper the
      rate fold uses. Verify: a test asserting the maintained gate equals
      the from-scratch gate at every event of a constrained fixture with
      composition change; 2.2's validation messages unchanged; existing
      validation snapshots unchanged
      — done 2026-09-28, commit `d9ddb84`, `receiver_count_tracker()` shared
      by the rate fold and both validation branches; fold byte-identical

## 5. Close

- [x] 5.1 After timing with 1.2's script and cells; the before/after table
      in `progress.md`
      — done 2026-09-28 on `d9ddb84`: constrained choice 15.085 -> 5.339 s
      at 10k; without `trans` 12.090 -> 1.367 s (the rest is the two-path
      cache copy, out of scope)
- [x] 5.2 Final verification: full `NOT_CRAN=true` suite green, six
      baseline files PASS not SKIP, no `_snaps/` change; a
      `NEWS.d/rate-init-value-copy-free-updates--mask-readers.md` bullet
      (constrained DyNAM choice preprocessing no longer rebuilds the
      support grid per event); no DESCRIPTION/NEWS.md bump on this branch
      — done 2026-09-28: every commit `3cae54b`/`ec14a63`/`b2bcad5`/`d9ddb84`
      green (9232/9233/9235/9246 PASS, 0 FAIL, 4 SKIP, six baseline files
      PASS, no `_snaps/` change); fragment `3262454`
