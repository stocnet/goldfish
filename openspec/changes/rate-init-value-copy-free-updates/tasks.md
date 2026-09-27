## 1. Gates and the before timing

- [ ] 1.1 Tree clean on `feature_simulation`; `NOT_CRAN=true` suite green
      with the six frozen baseline files PASS not SKIP (not-cran-test).
      Record the counts in `progress.md`.
- [ ] 1.2 Before timing on CollegeMsg subsets (1k/5k/10k, median of three
      warm runs after a timed warm-up, `bench::bench_memory()` for
      allocation): constrained choice (`choice ~ inertia + recip + trans`,
      `support_constraint = ~ !tie(msg)`) and constrained rate plus choice
      in one walk, with the constraint validation stubbed in the script
      (CollegeMsg repeats dyads) and the same cells without `trans`.
      Script in `.plan/sp/`, numbers in `progress.md`

## 2. Equality tests first (D4)

- [ ] 2.1 Tests (testthat 3e) comparing `support_row()`,
      `sender_gate_from_mask()` and `initial_receiver_count()` against a
      grid-form oracle built in the test helper, on every mask kind (0-3),
      on one-mode and non-square two-mode shapes, diagonal dropped and
      kept, some receivers absent. They pass against today's code (they
      pin behavior, not the change)
- [ ] 2.2 Capture the folded `active_sender` / `active_dyad` streams and
      the validation messages of the constrained reference families
      (`helper-constraint-atoms.R`) as byte-identity references

## 3. Readers without the grid (D1, D2)

- [ ] 3.1 `support_row()` kind 0 reads the row by strided index. Verify:
      2.1 and 2.2 pass; an allocation test shows no grid-sized allocation
      per row read on a 300 x 300 mask
- [ ] 3.2 `sender_gate_from_mask()` and `initial_receiver_count()` count
      kind 0 with `.rowSums()` on the vector, subsetting present columns by
      linear index only when a receiver is absent. Verify: 2.1 and 2.2
      pass; `air format` then `lintr` on the touched files

## 4. Validation gate kept from flips (D3)

- [ ] 4.1 `validate_support_constraint()` keeps per-sender allowed-receiver
      counts seeded by `initial_receiver_count()` and adjusted from
      `mask_flips()` and receiver presence changes, through the helper the
      rate fold uses. Verify: a test asserting the maintained gate equals
      the from-scratch gate at every event of a constrained fixture with
      composition change; 2.2's validation messages unchanged; existing
      validation snapshots unchanged

## 5. Close

- [ ] 5.1 After timing with 1.2's script and cells; the before/after table
      in `progress.md`
- [ ] 5.2 Final verification: full `NOT_CRAN=true` suite green, six
      baseline files PASS not SKIP, no `_snaps/` change; a
      `NEWS.d/rate-init-value-copy-free-updates--mask-readers.md` bullet
      (constrained DyNAM choice preprocessing no longer rebuilds the
      support grid per event); no DESCRIPTION/NEWS.md bump on this branch
