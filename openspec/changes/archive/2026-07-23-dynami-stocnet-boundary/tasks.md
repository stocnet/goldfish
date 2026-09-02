> Follow `openspec/config.yaml` disciplines: commit-per-task, run
> `devtools::document()` inline when roxygen/exports/signatures change, test with
> `NOT_CRAN=true` (frozen DyNAMi baselines PASS not SKIP), bump `DESCRIPTION` +
> `NEWS.md` at the milestone. Depends on `multimode-network-support` (two-mode
> assembly + mode map) and the archived `refactor-single-data-object`
> (`as_goldfish()`, validator, stamp). The 2026-07-19 explore grounding
> (progress.md) already traced the opportunity semantics — do not re-derive.

## 1. Grounding: the bridge contract and the fixture

- [x] 1.1 Map what `preprocess_interaction` and the isolated DyNAMi front-end
      actually read from the environment (objects, attrs, event streams,
      windowing `assign()` products) — this is the bridge's contract. The
      opportunity semantics are ALREADY grounded (see progress.md: the list is
      occupied groups at event order, consumed at estimation via
      `compute_step.default`, never by the monolith) — verify only that the
      remaining env reads are enumerated. ALSO ground the D6 estimation-side
      wiring: how the DyNAMi choice engine can consume folded availability
      instead of the opportunities channel (`compute_step.default` adapter
      need, if any). Write findings to `progress.md`.
- [x] 1.2 Build an actors×groups two-mode stocnet fixture mirroring an existing
      DyNAMi test setup (same events both ways: legacy constructors and direct
      stocnet), extending `tests/testthat/helper-stocnet-fixtures.R`; verify the
      DyNAMi baselines PASS under `NOT_CRAN=true` before any change.

## 2. Assembly and the internal bridge

- [x] 2.1 `make_groups_interaction()` returns the assembled multipartite
      stocnet (design D8, BREAKING): transformation logic untouched; the
      output assembles actors+groups `nodes` with `mode`, the focal two-mode
      `interactions` layer (dependent + exogenous rows; `order` attributes →
      the reserved `order` column; `flavor = "join"`/`"leave"` on dependent
      rows, `NA` on exogenous — design D2 encoding), and the one-mode `past`
      covariate layer; the `opportunities` component is dropped from the
      return. Delete the dead `setopportunities_interaction()`
      (`make_data_group.R:1044` — never called, no return). Lift the DyNAMi
      refusal in `is_stocnet_assemblable()` so `make_data()` with DyNAMi
      components assembles too; roxygen + `devtools::document()`.
- [x] 2.2 Implement the internal stocnet→environment bridge the DyNAMi
      front-end calls (comment it as the temporary seam retired with the
      monolith by the engine conversion); exact-equivalence test: bridge-built
      env components equal constructor-built ones on the 1.2 fixture.
- [x] 2.3 Derive the availability constraint (design D6, corrected
      2026-07-21): the DyNAMi choice specification auto-derives
      `~ indeg(<focal>) >= 1 & !tie(<focal>)` (occupied AND not the joiner's
      own affiliation — own singleton EXCLUDED per the paper), AND-combines
      any user `support_constraint`, and compiles/folds it through the
      STANDARD support-constraint machinery — the internal
      `opportunitiesList` channel is NOT fed (estimation-side wiring per the
      1.1 grounding); equivalence test: derived per-event sets equal the
      construction-stored lists MINUS the joiner's own singleton,
      element-for-element on the existing DyNAMi fixtures.
- [x] 2.4 Regenerate the DyNAMi CHOICE baselines as a new versioned set
      under the own-exclusion correction (document the coefficient shift for
      the NEWS BREAKING entry); rate baselines untouched. Verify:
      `NOT_CRAN=true` green, rate baselines PASS unchanged, new choice
      baselines PASS; commit.

## 3. Public surface

- [x] 3.1 `estimate_dynami()` AND `make_specification()` (design D7: both
      surfaces) accept a stocnet `data` for DyNAMi models, resolving the
      actors×groups layer via the mode map and routing through the bridge —
      the DyNAMi spec classes stay thin (consumption remains the monolith);
      the choice specification attaches the derived support constraint
      (2.3); no public `opportunities` argument; roxygen updated,
      `devtools::document()`.
- [x] 3.2 Implement the D9 flavor-keyed rate surface:
      `rate = list(join ~ ..., leave ~ ...)` with keys validated against the
      focal layer's flavor values; desugar to the legacy `joining = 1/-1`
      single-formula encoding (per-flavor `~ 1` intercepts mapped onto the
      asymmetric `1 + intercept(<focal>, joining = 1)` legacy spelling —
      cover every intercept combination: both flavors, one, neither; an
      effect under both keys becomes two terms); flavor-labeled coefficient
      names; reject a flavor-keyed `choice` with a `cli` error explaining
      the leaving choice is deterministic (a plain `choice` formula is the
      joining choice); roxygen + `devtools::document()`.
- [x] 3.3 Keyed-vs-flag equivalence test: the keyed-rate specification ==
      the hand-written joining-flag formula to 1e-6 on the DyNAMi rate
      baselines (coefficient name mapping asserted), both engines where
      applicable.
- [x] 3.4 Coefficient equivalence test: stocnet path == constructor path to
      1e-6 on the DyNAMi rate and choice baselines, both engines where
      applicable. Verify + commit.

## 4. The legacy-environment abort and cleanup

- [ ] 4.1 DEFERRED to `make-data-always-assembles` (design D4 revised): the
      `is.environment(data)` abort premise ("make_data never returns an env") is
      false — subset-dependent-vs-fuller-network inputs still fall back to an env
      — so a blanket abort would break real workflows. Moved to the change that
      makes `make_data()` always assemble-or-abort. `as_goldfish()` env
      conversion also stays deferred.
- [x] 4.2 Vignette updated to the one-step stocnet workflow
      (`dynami-example.Rmd.orig`, re-knit via `vignettes/precompile.R`; M2
      choice uses unwindowed inertia — windowed DyNAM-i effects are broken at
      preprocessing, a separate follow-up). The `zzz_testthat_helpers.R` fixture
      cleanup became unnecessary once 4.1 was deferred (the legacy env fixtures
      stay valid), so it is dropped.
- [x] 4.3 Verify: full `NOT_CRAN=true` suite green (FAIL 0 / SKIP 0 / N 931),
      all frozen baselines PASS not SKIP; `lintr` on touched files; `document()`;
      `openspec validate --strict`; committed.

## 5. Milestone

- [x] 5.1 Bumped `DESCRIPTION` 1.9.7 → 1.9.8 + `NEWS.md`: DyNAM-i on the single
      stocnet data object (feature) + BREAKING `make_groups_interaction()`
      returns the stocnet, opportunities retired. The env-abort BREAKING entry
      was removed with the 4.1 deferral; the own-singleton-exclusion entry was
      dropped when D6 reverted to own-inclusion (no coefficient shift, choice
      baselines = the 1.7.0 values). `openspec validate --strict` passes.
