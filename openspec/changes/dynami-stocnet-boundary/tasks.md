> Follow `openspec/config.yaml` disciplines: commit-per-task, run
> `devtools::document()` inline when roxygen/exports/signatures change, test with
> `NOT_CRAN=true` (frozen DyNAMi baselines PASS not SKIP), bump `DESCRIPTION` +
> `NEWS.md` at the milestone. Depends on `multimode-network-support` (two-mode
> assembly + mode map) and the archived `refactor-single-data-object`
> (`as_goldfish()`, validator, stamp). The 2026-07-19 explore grounding
> (progress.md) already traced the opportunity semantics — do not re-derive.

## 1. Grounding: the bridge contract and the fixture

- [ ] 1.1 Map what `preprocess_interaction` and the isolated DyNAMi front-end
      actually read from the environment (objects, attrs, event streams,
      windowing `assign()` products) — this is the bridge's contract. The
      opportunity semantics are ALREADY grounded (see progress.md: the list is
      occupied groups at event order, consumed at estimation via
      `compute_step.default`, never by the monolith) — verify only that the
      remaining env reads are enumerated. Write findings to `progress.md`.
- [ ] 1.2 Build an actors×groups two-mode stocnet fixture mirroring an existing
      DyNAMi test setup (same events both ways: legacy constructors and direct
      stocnet), extending `tests/testthat/helper-stocnet-fixtures.R`; verify the
      DyNAMi baselines PASS under `NOT_CRAN=true` before any change.

## 2. Assembly and the internal bridge

- [ ] 2.1 `make_groups_interaction()` returns the assembled multipartite
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
- [ ] 2.2 Implement the internal stocnet→environment bridge the DyNAMi
      front-end calls (comment it as the temporary seam retired with the
      monolith by the engine conversion); exact-equivalence test: bridge-built
      env components equal constructor-built ones on the 1.2 fixture.
- [ ] 2.3 Derive the occupancy availability (design D6): the DyNAMi choice
      path evaluates `~ indeg(<focal>) >= 1` per dependent event with state
      maintained in `order` (own singleton INCLUDED — occupancy only, no
      `!tie()`), AND-combines any user `support_constraint`, and feeds the
      result through the existing internal `opportunitiesList` estimation
      channel; equivalence test: derived per-event sets equal the
      construction-stored lists element-for-element on the existing DyNAMi
      fixtures.
- [ ] 2.4 Verify: `NOT_CRAN=true` green, DyNAMi baselines PASS; commit.

## 3. Public surface

- [ ] 3.1 `estimate_dynami()` AND `make_specification()` (design D7: both
      surfaces) accept a stocnet `data` for DyNAMi models, resolving the
      actors×groups layer via the mode map and routing through the bridge —
      the DyNAMi spec classes stay thin (consumption remains the monolith);
      the choice specification attaches the derived occupancy constraint
      (2.3); no public `opportunities` argument; roxygen updated,
      `devtools::document()`.
- [ ] 3.2 Coefficient equivalence test: stocnet path == constructor path to
      1e-6 on the DyNAMi rate and choice baselines, both engines where
      applicable. Verify + commit.

## 4. The legacy-environment abort and cleanup

- [ ] 4.1 Implement the public abort: `make_specification()` / `estimate_*()`
      raise `cli_abort()` on `is.environment(data)` naming `as_goldfish()` as
      the migration; snapshot test under the pinned cli context (the deferred
      single-object requirement — its delta here removes the archive-time
      DyNAMi exception).
- [ ] 4.2 Clean up `R/zzz_testthat_helpers.R`: DyNAMi fixtures route through
      the stocnet boundary; shrink the quieted-lifecycle bracket to what the
      deprecation-cycle tests still need. Update
      `vignettes/dynami-example.Rmd.orig` to the one-step stocnet workflow
      (no opportunities handling) and re-knit via `vignettes/precompile.R`.
- [ ] 4.3 Verify: full `NOT_CRAN=true` suite green, all frozen baselines PASS
      not SKIP; commit.

## 5. Milestone

- [ ] 5.1 Bump `DESCRIPTION` + `NEWS.md` (BREAKING: legacy environments
      rejected everywhere with the `as_goldfish()` migration; BREAKING:
      `make_groups_interaction()` returns the stocnet, opportunities retired
      in favor of the derived occupancy constraint; DyNAMi on the single data
      object); `openspec validate dynami-stocnet-boundary --strict`.
