## 0. Gates and baseline

- [ ] 0.1 Confirm the base branch with Alvaro (design Migration Plan proposes
      `feature_simulation`, where the walk handle, the simulation driver and
      `support-gate-self-exclusion` live), the tree clean, and
      `support-gate-self-exclusion` landed. Run the `NOT_CRAN=true` full suite
      (not-cran-test, background). Verify: FAIL = 0, the six frozen baseline
      files PASS with 0 skipped; record the totals in `progress.md` as the
      before-state.

## 1. The field and the one predicate (D1, D5)

- [ ] 1.1 Tests first: a one-mode DyNAM specification built with
      `allow_reflexive = TRUE` carries the flag on the spec and on its model
      spec; the default is `FALSE`; `excludes_self_dyads()` returns `FALSE` for
      a reflexive one-mode spec, `FALSE` for any two-mode spec and `TRUE` for a
      plain one-mode spec. Snapshot the two refusals (two-mode layer,
      `choice_coordination`). Watch every one fail.
- [ ] 1.2 Implement in `R/make_specification.R` and `R/model_spec.R`:
      the `allow_reflexive` argument (default `FALSE`), the field on
      `model_spec_structure()` beside `is_two_mode`, the
      `excludes_self_dyads(spec)` predicate, and the construction refusals
      (`refuse_reflexive_where_impossible()`, one helper, one wording) for a
      two-mode dependent layer and for `choice_coordination`. Roxygen for the
      new argument; `devtools::document()`. Verify: 1.1 passes; `print` of a
      specification is unchanged when the flag is `FALSE`.
- [ ] 1.3 Verification: `air format` then `lintr` on the touched R files;
      `NOT_CRAN=true` suite green; the six baseline files PASS not SKIP.

## 2. Resolve once, thread the value, delete the duplicates (D3, D4)

- [ ] 2.1 Tests first: the resolved value is carried, not recomputed —
      `excludes_self_dyads()` is called once per compiled unit for a run over
      many events (count calls with `local_mocked_bindings()` or a counting
      wrapper), and the rate gate, the walk risk set, the preprocessing
      validation and the simulation driver all read the same field. A one-mode
      reflexive specification keeps the self-dyad in the folded gate,
      `n_candidates` and `avg_active_entity`; the non-reflexive numbers are
      unchanged from the current tree (assert against values recorded in 0.1).
- [ ] 2.2 Implement: resolve at unit compile in `R/preprocess_joint.R`, store
      on the unit, the engine, the preprocessed object and the walk handle;
      `R/model_preprocess.R` (`fold_active_sender_support()` and its counter
      helpers) and `R/support_mask.R` (`sender_gate_from_mask()`) read the
      resolved field instead of taking `drop_diagonal` per call;
      `R/walk_handle.R`, `R/simulate_driver.R` and `R/model_estimate.R`
      (`validate_prep_support()`) drop their local derivations. Verify: 2.1
      passes and `grep -n 'is_two_mode' R/` shows no second derivation of the
      self-tie rule outside the predicate.
      *Note 2026-09-26 (`process-simulation` 2.11/2.11b, session
      goldfish-ec):* two more readers of the self-tie rule land there. The
      time-weighted exposure (`exposure_entity_time`, via
      `time_weighted_risk_set()`) counts a REM rate's dyads without the
      self-dyad on a one-mode layer, matching the likelihood's risk set; the
      endpoint approximation a specification-driven simulation pins from
      counts the same way. Both derive "one-mode" locally today and must read
      the resolved value here, so a reflexive REM's exposure includes the
      diagonal
- [ ] 2.3 Remove `abort_if_diagonal_off_grid()` and its snapshot test (D4),
      and rename `op_drop_diagonal` to say what it means (one-mode AND this
      unit has interactions), updating its uses in `R/preprocess_joint.R`.
      Verify: no caller of the removed helper remains; the interaction tests
      and the frozen baselines are unchanged.
- [ ] 2.3a Hand-offs from the `support-gate-self-exclusion` conformance
      report (added 2026-10-02, `.plan/develop/`
      `support-gate-self-exclusion-conformance.md`):
      (i) the rate-gate fold's caller (`R/preprocess_joint.R:1708`) derives
      the flag as `identical(ctx$nodes, ctx$nodes2)`, the fifth derivation
      the proposal lists; 2.2 replaces it with the resolved field and the
      self-exclusion change's 2.2 note records the deviation; (ii) while
      rewriting that path, build `own` (`R/model_preprocess.R:370`, an `n1`
      integer vector plus a logical subset allocated at every flipped event
      for every kind) only in the ego and global branches that read it
      (+57.9 MB, +15.7 percent of the fold's allocation on the
      point-constrained CollegeMsg rate today); (iii) `sender_gate_from_mask()`'s
      point branch (`R/support_mask.R:262`) builds the `n1 x n2` grid on
      every call, pre-existing, worth the same once-per-fit resolution;
      (iv) the living `active-availability-stat` requirement "Sender-loop
      dyadic constraints consumed via row-reduction" (`spec.md:126-131`)
      still defines the gate without the self-exclusion that change
      introduced; this change's delta on that capability gains a MODIFIED
      block for it, conditional on the resolved rule. Verify: (ii) by the
      fold's allocation on the point-constrained CollegeMsg fixture before
      and after; (iv) by the placement check.
      *Note 2026-10-03* (from the follow-up re-check,
      `.plan/develop/support-gate-self-exclusion-followup-conformance.md`):
      since `ec4b3c0` the gate sites live in `R/availability_effect.R`
      (`availability_gate_flips()`, `availability_gate_receiver()`). (i)'s
      flag is now the `drop_diagonal` default of
      `build_engine_availability()` (`R/preprocess_joint.R:1365`). (ii) is
      mostly done: `own` is built only in the alter branch (flipped entries,
      under `drop_diagonal`), the ego branch (flipped entries) and the
      global branch (`n1`, read there); the point branch allocates nothing.
      Re-measure before acting on (ii) and drop it if the fold shows no
      excess. (iv) stands: the living `active-availability-stat` and
      `support-constraint` specs disagree on the one-mode gate until this
      change folds.
- [ ] 2.4 Verification: `air format` then `lintr` on the touched R files;
      `NOT_CRAN=true` suite green; the six baseline files PASS not SKIP and
      byte-identical — the detector that the default path did not move.

## 3. The likelihood honors the flag (D1, proposal Impact)

- [ ] 3.1 Tests first: a one-mode DyNAM choice fit with `allow_reflexive =
      TRUE` equals a hand-built multinomial oracle that includes the self-cell
      in every event's candidate set (a small fixture, `mlogit` or a
      hand-written log-likelihood, 1e-6), and differs from the same fit at
      `FALSE`. A REM fit with self-ties allowed keeps the diagonal in its rate
      sum. Watch both fail.
- [ ] 3.2 Implement: `R/model_estimate.R` and `R/cpp_interface.R` pass the
      resolved value where `allowReflexive` / `allow_reflexive` is hard-coded
      `FALSE`, so `correctReflexive` and the `twomode_or_reflexive` broadcast
      fan-out read the model's own rule. Verify: 3.1 passes; the DyNAM-i tests
      are unchanged (its kernels keep their own defaults, design Non-Goals).
- [ ] 3.2a The `r` backend's per-event diagonal zeroing reads the resolved
      rule and stops copying the statistics array. `compute_step.default()`
      in `R/estimation_core.R` (the block commented "patch to avoid collision
      with dropping absent people") zeroes the self-dyad of every statistic
      slice on every event of every Newton iteration, under its own condition
      `!ctx$is_two_mode && !is_rate` — a fifth local derivation of the
      self-tie rule, and one that would silently erase the diagonal of a
      reflexive model. Its loop, `diag(statsArrayComp[,, k]) <- 0` per slice,
      extracts and rewrites each slice.
      *Added 2026-09-29 (session goldfish-92), from a measurement, not from
      the design:* on a REM fit (Social_Evolution, 84 actors, `backend =
      "r"`) the loop is about 30 percent of the fit: 5.30 s as written,
      3.60 s as one index assignment over all slices, 2.49 s skipped (median
      of 3, coefficients and log-likelihood identical on that fixture).
      Tests first: (i) a reflexive one-mode fit on the `r` backend keeps its
      diagonal statistics — covered by 3.1 once it runs on `backend = "r"`,
      so assert that backend there; (ii) an allocation guard on one
      `compute_step.default()` call with a 300 x 300 x 3 array, bounded well
      below one copy of the array per slice; watch it fail on the loop.
      Implement: condition on the resolved `allow_reflexive` (2.2), and zero
      the self-dyads with a single assignment through an index built once per
      fit, not per event. Keep the code readable for someone who does not
      read C++: name the index for what it is and say in the comment why the
      self-dyad is zeroed before absent actors are dropped. Check a fixture
      with composition change (actors joining or leaving), since that is the
      collision the original comment guards against. Verify: (i) and (ii)
      pass; the frozen baselines are unchanged; the `r` backend REM fit is
      measurably faster on an optimized build (cpp-recompile skill).
      *Note 2026-10-03* (from the `likelihood-site-cleanup-and-cost`
      follow-up re-checks, `.plan/develop/`
      `likelihood-site-cleanup-and-cost-followup2-conformance.md`, G12 and
      G16): that change (group 4 and 7.1, `3967fa4`) rewrote the r backend's
      per-event step: statistics are written in place, the diagonal is
      zeroed only on the cells an event updates, and no slice is copied, so
      the loop and the 30 percent above are gone; re-measure before acting
      and keep only the resolved-rule part. Two measured facts for 3.1/3.2:
      under the internal `allow_reflexive = TRUE` the one-mode ordinal REM
      now includes the diagonal on r and agrees with cpp (it did not at
      `237c8dd`: r -406.19, cpp -408.22); the one-mode exact-time REM still
      disagrees (r -3706 against cpp -877, the same at `237c8dd`,
      pre-existing). No public path reaches either today; this change's
      tests must cover both families on every backend.
- [ ] 3.3 Verification: `air format` then `lintr`; `NOT_CRAN=true` suite green;
      baselines PASS not SKIP, byte-identical.

## 4. The control surface and its precedence (D2, D5)

- [ ] 4.1 Tests first: `set_preprocessing(allow_reflexive = TRUE)` overrides a
      specification carrying `FALSE`, for `estimate_dynam()`,
      `compute_statistics()` and `simulate()`; `NULL` defers to the
      specification; a formula-based `estimate_dynam()` call with the control
      set gets a reflexive risk set; the impossible cases (two-mode,
      `choice_coordination`) abort at the call with a message naming
      `set_preprocessing()`, pinned by snapshot. Watch them fail.
- [ ] 4.2 Implement: the argument and its validation in `R/set_opt.R`, the
      resolution helper where the control meets the specification
      (`estimate_*()`, `compute_statistics()`, `simulate()`), reusing the
      refusal helper from 1.2. Roxygen for both surfaces states the precedence
      rule in one sentence; `devtools::document()`. Verify: 4.1 passes.
- [ ] 4.3 Verification: `air format` then `lintr`; `NOT_CRAN=true` suite green;
      baselines PASS not SKIP.

## 5. A preprocessed object carries its rule (D6)

- [ ] 5.1 Tests first: a `goldfishStat` built with self-ties excluded and
      supplied to a call resolving to `allow_reflexive = TRUE` aborts naming
      both values and re-preprocessing (snapshot), and the reverse case aborts
      too; a matching pair estimates as before. Watch them fail.
- [ ] 5.2 Implement: store the resolved value on the preprocessed object and
      check it beside the existing format-version rejection in
      `R/model_estimate.R`. Verify: 5.1 passes; an object built before this
      change (no field) is treated as self-ties excluded, which is what it was.
- [ ] 5.3 Verification: `air format` then `lintr`; `NOT_CRAN=true` suite green;
      baselines PASS not SKIP.

## 6. Documentation and close

- [ ] 6.1 `NEWS.d/allow-reflexive-user--allow-reflexive.md`: the new argument on
      both surfaces, the precedence rule, the `preprocessed =` mismatch abort,
      and that the default leaves every existing fit unchanged. No DESCRIPTION
      or NEWS.md edit on the branch (ADR-0040). Verify: follows
      `NEWS.d/README.md`.
- [ ] 6.2 Mention `allow_reflexive` where the risk set is documented: the
      `make_specification()` and `set_preprocessing()` topics, and the
      `support_constraint` documentation that describes the gate. Verify:
      `devtools::document()` clean, `pkgdown::check_pkgdown()` reports only the
      pre-existing `logLik.result.goldfish` topic.
- [ ] 6.3 Final verification: full `NOT_CRAN=true` suite with the six frozen
      baseline files PASS not SKIP and byte-identical; `openspec validate
      allow-reflexive-user` passes; tick tasks with commit hashes and append the
      session to `progress.md`; flip the proposal to `status: landed` per
      ADR-0040 rather than archiving on the branch.
