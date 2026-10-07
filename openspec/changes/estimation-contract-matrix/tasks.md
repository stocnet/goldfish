## 0. Gates (D1, D2)

- [ ] 0.1 Confirm `constraint-objects-on-shared-walk` groups 0-5 are landed
      on `feature_simulation` (the harness, F1-F5 and their references) and
      the tree is clean; `NOT_CRAN=true` suite green (not-cran-test; six
      baseline files PASS, not SKIP). Record counts and the suite's
      wall-clock in `progress.md` as the runtime baseline for 4.1.
- [ ] 0.2 Move the model into the tests: `tests/testthat/_fixtures/estimation_contract/model.pict`
      (seven factors, four rules, comments naming the refusal each rule
      encodes), `seeds.tsv` (PICT order-3 output of the composition,
      constraint, flavor projection, 30 rows), `cases.tsv` (PICT order-2
      output seeded with it, 37 rows), `README.md` (the two commands, PICT
      3.7.4, the date, the counts 30 / 37 / 95 / 112 and why seeding was
      chosen). Test (r-lib:testing-r-packages): a rules test reads
      `model.pict`'s rules as R predicates and asserts no row of
      `cases.tsv` violates one, every admitted pair of factor values
      appears, and every admitted composition x constraint x flavor triple
      appears.
- [ ] 0.3 Verification: `NOT_CRAN=true` suite green; baselines PASS not SKIP.

## 1. Harness extensions (D4)

- [ ] 1.1 `helper-simulate-dgp.R`: two-mode mode (`n_senders`, `n_receivers`
      or a `two_mode = TRUE` switch; disjoint sets, `n1 x n2` state,
      self-dyads admitted, composition changes on either set, rate over
      present senders, choice over present allowed receivers, REM over
      present allowed dyads); `dgp_stocnet()` renders the two node sets.
      Test: a two-mode sequence names no receiver as a sender; an absent
      receiver has no row during its absence; the rate frame has one row
      per present sender per interval; reproducible under a seed.
- [ ] 1.2 Node attribute `grp` drawn once per node; the ego-only constraint
      `~ ego(grp) > 0` as a `constraint` closure the sequence is drawn
      under; the exogenous layer `emails` (no formula reads it) and the
      allowed panel layer `allowed` (a static 0/1 network built from the
      first draw plus a random half of the remaining dyads) always present
      in the rendered data. Test: every observed sender is allowed under
      the ego constraint; the allowed layer admits every observed dyad.
- [ ] 1.3 Ordinal frames as projections of the exact-time frames: the
      ordered rate frame (one row per sender at risk per dependent event,
      observed sender marked) and the ordered REM frame (one row per dyad
      at risk per dependent event, observed dyad marked); the symmetrized
      coordination dyad frame (dyads allowed both ways, chosen marked); the
      opportunity sets per event (the sender's allowed receivers under
      `~ tie(allowed)`). Test: the ordered REM frame's rows equal the
      exact-time dyad frame's rows for the same interval with exactly one
      chosen; the opportunity sets reproduce the constraint's frame.
- [ ] 1.4 Verification: `NOT_CRAN=true` suite green; the predecessor's F1-F5
      unchanged (their references `identical()`); `air format` and `lintr`
      on the touched helper.

## 2. The row runner (D3, D5)

- [ ] 2.1 `helper-contract-matrix.R`: `contract_data(modes, composition,
      flavored)` builds and caches the at most eight data objects (one-mode
      15 actors / 160 events, two-mode 10 x 12 / 160, fixed seeds);
      `contract_row(row)` maps a row to the specification form (plain,
      container, single modeled flavor), the family's formula, the
      constraint formula, the backend and the oracle kind;
      `contract_frames(row, data)` builds the row's frames from the same
      draw; `contract_arms(row)` lists the arms the row runs (oracle unless
      coordination; frame identity; backend equality; replay when masked;
      all-allowing identity for coordination; opportunity equivalence for
      the opportunity rows). Test: every row of `cases.tsv` maps to a
      buildable specification (`make_specification()` succeeds, or the
      row names a documented refusal, in which case the rules test of 0.2
      should have excluded it).
- [ ] 2.2 `test-estimation_contract_matrix.R`: one `test_that()` per row,
      named from the row's values, `skip_on_cran()`, `skip_if_not_installed()`
      per oracle package; arms in the order frame identity, oracle,
      backend equality, replay. Land it first on the one-mode,
      unconstrained, unflavored rows (they reproduce F1) and confirm the
      deltas match the predecessor's README.
- [ ] 2.3 Extend the runner row class by row class, each class a commit:
      constrained one-mode rows (static dyadic, covered, uncovered, ego);
      flavored rows (container, single modeled flavor; the flavored plus
      user-constraint rows are the probe of the survey, expected to pass);
      composition-change rows (the exact-time rate oracle arm marked as
      waiting on `process-simulation` 2.11c with the predecessor's F4 note;
      choice, ordered and frame arms live); two-mode rows; ordered rows
      with their `clogit` oracles; coordination rows; opportunity rows. For
      each class record the measured deltas per row in
      `_references/simulation_v1/README.md`'s table. A failing row is
      investigated as a defect (goldfish, then harness, then row
      construction) and recorded in `progress.md`; no reference is written
      from a failing row.
- [ ] 2.3a Hand-offs from the `constraint-objects-on-shared-walk` groups 5-6
      conformance report (added 2026-10-02): the F2 standalone and
      de-flavored routes (`test-simulation_cross_validation.R:547-579`) are
      compared with the container fit, never directly with the oracle, so
      they are bounded at 2e-6 where the spec says each route agrees with
      the oracle at 1e-6 (measured gap 0); and
      `skip_if_not_installed("survival")` sits inside the F2 loop (`:369`,
      reached from `:574`), so a missing package would skip the `glm` arms
      the design says never skip. The row runner compares every route to
      the oracle directly and places the `clogit` skip on the `clogit` arm
      alone. Verify: with `survival` mocked absent, the `glm` rows still
      run.
      *Added 2026-10-03* (from the `likelihood-site-cleanup-and-cost`
      second follow-up re-check, G16, pre-existing): an observed event whose
      sender or receiver is absent at its stamp aborts the r backend with
      the observed-dyad error, while cpp raises nothing and evaluates it.
      The matrix's composition rows assert the same verdict on every
      backend (the re-check's `observed_absent.R` is a fixture).
- [ ] 2.4 Verification: `NOT_CRAN=true` suite green; all 37 rows PASS not
      SKIP (the `mlogit` and `clogit` arms skip only where the package is
      absent); baselines PASS; `air format` and `lintr` on the touched test
      files.

## 3. Runtime budget (D6)

- [ ] 3.1 Measure the matrix file's wall-clock under `NOT_CRAN=true` on the
      optimized build (cpp-recompile first), median of three; record in
      `progress.md` and the fixtures README with the per-class split. If
      above three minutes, shrink the dominant class's data object (events,
      never arms) and re-measure; record the final sizes in the README.
- [ ] 3.2 Verification: full `NOT_CRAN=true` suite green; the suite's
      wall-clock against 0.1's baseline recorded.

## 4. Close

- [ ] 4.1 `bash .plan/opsx-spec-placement-check.sh estimation-contract-matrix`
      (the capability exists only in the predecessor's unarchived delta, so
      a pre-fold miss on `simulation-cross-validation` is expected deferral
      per the branching rules) and `openspec validate
      estimation-contract-matrix --strict` clean.
- [ ] 4.2 `NEWS.d/estimation-contract-matrix--contract-matrix.md`
      (developer-facing: the estimation contract is covered pairwise with
      the composition x constraint x flavor triple exhaustive, from a
      committed PICT model). No `NEWS.md` or Version edit on the branch.
- [ ] 4.3 Vault: ADR-0111 `spec:` and open questions current; dashboard
      fragment in `_dashboard-inbox/`; session note closed; a note under
      `process-simulation` task 2.11c that the rate oracle arm of the
      composition rows waits on it (claim that change first per ADR-0036).
- [ ] 4.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      PASS not SKIP; `status: landed (feature_simulation, awaiting fold)` in
      `proposal.md`.
