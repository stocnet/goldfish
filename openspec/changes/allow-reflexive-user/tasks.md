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
- [ ] 2.3 Remove `abort_if_diagonal_off_grid()` and its snapshot test (D4),
      and rename `op_drop_diagonal` to say what it means (one-mode AND this
      unit has interactions), updating its uses in `R/preprocess_joint.R`.
      Verify: no caller of the removed helper remains; the interaction tests
      and the frozen baselines are unchanged.
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
