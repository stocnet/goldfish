Every task is verified with the `not-cran-test` skill before its commit (the
frozen 1e-6 baselines report PASS, not SKIP), closed with `close-task`, and
follows `r-lib:r-package-development` / `r-lib:testing-r-packages`. Touched R
files are `air format`-ed and then linted. This is a feature branch
(`feature/dynes`), so the milestone writes a `NEWS.d/` fragment and bumps no
Version.

## 1. Record the "before" and guard the schedule

- [ ] 1.1 Record the pre-change values, with no package code changed. Build
      the synthetic affected fixture from design "Storage and per-event cost"
      (200 actors, 5000 calls, `choice = ~ tie(friendship)`, a weighted
      `calls` outdegree constraint loose enough to hold on the data), plus
      one affected **timed-rate** fixture (a rate formula that reads no
      `calls`, the same constraint). For each fixture, record the schedule
      row counts per stream, the walk seconds, the allocated MB and the
      `lobstr::obj_size()` of the preprocessed output. For the timed-rate
      fixture, also save the preprocessed output for 2.4's comparison. Also
      save the `build_joint_schedule()` output of 2.1a's 4-actor fixture with
      `+ inertia` added (a term reads `calls`, so the schedule has no
      duplicate) for 2.4's identity check. Save every output, with its
      generator script, under the local-only `.plan/focal-stream-dedup/`.
      Record the values, the path, and the R and goldfish versions in
      `progress.md`.
- [ ] 1.2 Schedule stream-count check (D2, D5): for each object on the
      joint schedule, the number of distinct non-dependent `stream_index`
      values equals the object's number of source streams. Put it in
      `test-preprocess_joint.R` (present upstream) and use only shared
      fixtures, so the commit cherry-picks to `feature_simulation`. Assert
      it over:
      - every frozen constraint-atom family (`helper-constraint-atoms.R`);
      - a small inline single-process fixture with the pattern (a
        constraint on `calls` that no term reads);
      - a two-object constraint case, where a constraint-only object comes
        after `calls` in the link, which checks D1's re-index note;
      - an inline cross-process joint fixture, where one process's
        constraint reads the other's focal layer and no term reads it
        (built the way the file's existing `make_joint_specification()`
        fixtures are). Also assert that this layer's dependent rows and its
        single state-update stream are both on the schedule, so the
        dependent exemption of D2 is exercised across processes.

      No two-streams-on-one-object control: current data cannot build one
      (design, Risks), so it would pass on one stream. That case is covered
      by 2.1b's helper unit test. Do not use `stamp_tie_spec()` (DyNES-only;
      its case is in 2.2a) or 1.1's large cost fixtures. Record it FAILING on the unfixed code (the
      pattern fixtures), and commit it alone, as test-only code, as
      `test(focal-stream-dedup): … (RED)`.

## 2. Every added stream is recorded and skipped by key (D1, D2, D3, D4)

- [ ] 2.1a Failing test (D3, standalone `fix:`): the 4-actor plain
      `estimate_dynam()` fixture. N2 calls at t = 2 and again at t = 3, with
      `choice = ~ tie(friendship)` and `support_constraint = ~ outdeg(calls,
      type = "ego", weighted = TRUE) < 2`. The fit completes, and the schedule
      carries one state-update row per `calls` event. Add the control on the
      same data with `outdeg(calls, type = "ego") < 1`: it aborts with the
      empty-risk-set error, which it does today too. Add the exact-threshold
      control (D3): the weighted `< 2` constraint on data where N2 calls
      three times aborts at N2's third call. Assert this by matching the
      event number in the message (`"empty risk set at event <k>"`). Before
      the fix the abort is at the second call, so this control is RED too.
      Put all three in
      `test-support_constraint_estimate.R` (present upstream) with inline
      data and no DyNES helper (D5). Confirm RED (the weighted fit aborts)
      and commit the tests alone as `test(focal-stream-dedup): … (RED)`.
- [ ] 2.1b In `build_joint_schedule()` (D1, D2): `add_part()` takes the
      stream key of each state-update stream, checks it with the stream-key
      assertion helper and appends it to `stream_seen`. Dependent parts pass
      no key and are never recorded. Loop 2 skips a focal stream whose key
      is in `stream_seen`, and loop 3's `fresh` test becomes key-only;
      `targets_seen` is no longer a skip criterion (remove it if nothing
      else reads it). The helper aborts on a repeated key with
      `cli::cli_abort(.internal = TRUE, class =
      "goldfish_duplicate_schedule_stream")`, naming the key and the object.
      Unit-test it directly in `test-preprocess_joint.R`: `expect_error(class
      =)` on a repeated key, and no error on distinct keys, including two
      distinct keys on one object, because the fixed builder can no longer
      trigger it. Leave no comment that refers to this change.
      Confirm GREEN for 2.1a and 1.2.
      Confirm that the constraint-atom tests
      (`test-constraint_atoms_operands.R`, frozen captures unmoved) and
      `test-augment_legality.R` pass unchanged. Touch only
      `R/preprocess_joint.R` and `test-preprocess_joint.R`. Commit as
      `fix(focal-stream-dedup): … (GREEN)`.
- [ ] 2.2a Failing test (D4): on `stamp_tie_spec()` with calls 2 -> 3 at 2 and
      2 -> 4 at 3 under the weighted `< 2` constraint, a candidate with the
      first interval legal has a finite batch log-likelihood
      (`evaluate_candidate()`). Also assert 1.2's stream-count check on
      `stamp_tie_spec(constraint = ~ outdeg(calls, type = "ego") < 1)`,
      which stays on this branch (D5). Both `make_proposal_evaluator(legality =)` and
      `new_legality_checker()` say legal, and `walk_realizations()` holds one
      entry per focal timed tie of the candidate. Write it after 2.1b, so it
      is a characterization of the fix rather than a RED. Record in
      `progress.md` that it was GREEN when written and why, and commit it.
- [ ] 2.3 Rerun every seeded augmenter test (`test-augment_seq_random.R`,
      `test-augment_seq_mcmc.R`, `test-augment_mcmc_cost.R`,
      `test-augment_seq_sim.R`, `test-augment_sim_cost.R`) and
      `test-rate_queries.R`. Any expectation that moved must trace to a
      fixture whose constraint reads its own focal layer with no term
      reading it. Record each in `progress.md`.
- [ ] 2.4 Compare the timed-rate fixture's preprocessed output with 1.1's
      saved one. Expected: identical, or differing only by zero-width
      right-censoring rows at a dependent stamp (design, Risks). Any other
      difference stops the group for a design review. Also rebuild the
      `+ inertia` schedule and assert it is `identical()` to 1.1's saved
      one: one-time evidence that the key-only skip changes nothing on an
      unaffected specification. A difference there also stops the group.
      Then add a permanent test in `test-preprocess_joint.R` for the spec
      scenario "A term on the focal layer keeps the planned streams": on
      the same fixture, the state-update streams are those the formula
      planned, in order, with nothing appended. Record both results.
- [ ] 2.5 Close the group. Measure the walk seconds, allocated MB and object
      size on 1.1's fixtures against the recorded values and design's
      prediction (one state-update row fewer per focal event), and record
      them in `progress.md`. Confirm that `walk_cost.csv` and
      `estimation_cost.csv` are unmoved. Run the NOT_CRAN=true full suite
      (baselines PASS), and commit.

## 3. Milestone

- [ ] 3.1 Write the `NEWS.d/focal-stream-dedup--focal-constraint-stream.md`
      fragment (protocol in `NEWS.d/README.md`). The fix: a support
      constraint that reads its own layer with weights, where no formula
      term reads that layer, saw each event counted twice. Do not touch the
      DESCRIPTION Version or `NEWS.md`. Flip `proposal.md` to `status: landed
      (feature/dynes, awaiting fold)`. Commit.
- [ ] 3.2 Run `conformance-inspector` on the change. Triage its report and
      re-check the scoped findings before the change is trusted.
- [ ] 3.3 Offer the fix upstream (`feature_simulation`, the home of
      `constraint-objects-on-shared-walk`) as a cherry-pick of the 1.2,
      2.1a and 2.1b commits (D5), with a short note naming the two causing
      commits (`2822625b`, `b1b21033`). First confirm that the three apply
      cleanly to `upstream/feature_simulation` and that its suite passes
      with them. Record where it went in `progress.md`. This does not block
      the fold.
