> Follow `openspec/config.yaml` disciplines: commit-per-task, run
> `devtools::document()` inline when roxygen/exports/signatures change, test with
> `NOT_CRAN=true` (frozen DyNAMi baselines PASS not SKIP), bump `DESCRIPTION` +
> `NEWS.md` at the milestone. Depends on `multimode-network-support` (two-mode
> assembly + mode map) and the archived `refactor-single-data-object`
> (`as_goldfish()`, validator, stamp).

## 1. Grounding: the bridge contract and the fixture

- [ ] 1.1 Map what `preprocessInteraction` and the isolated DyNAMi front-end
      actually read from the environment (objects, attrs, event streams,
      `opportunities`, windowing `assign()` products) — this is the bridge's
      contract. Confirm the composition-state reading of `opportunities`
      (design D6: groups available at event time) reproduces the lists the
      existing fixtures supply. Write findings to `progress.md`.
- [ ] 1.2 Build an actors×groups two-mode stocnet fixture mirroring an existing
      DyNAMi test setup (same events both ways: legacy constructors and direct
      stocnet), extending `tests/testthat/helper-stocnet-fixtures.R`; verify the
      DyNAMi baselines PASS under `NOT_CRAN=true` before any change.

## 2. Assembly and the internal bridge

- [ ] 2.1 Lift the DyNAMi refusal in `is_stocnet_assemblable()` and add the
      interaction branch to the assembler: fused actors+groups `nodes` with
      `mode`, the interaction layer with disjoint mode sets, composition/
      attribute changes routed per side — `make_data()` with DyNAMi components
      returns a stocnet, never an environment.
- [ ] 2.2 Implement the internal stocnet→environment bridge the DyNAMi
      front-end calls (comment it as the temporary seam retired with the
      monolith by the engine conversion); exact-equivalence test: bridge-built
      env components equal constructor-built ones on the 1.2 fixture.
- [ ] 2.3 Derive group availability from composition state (design D6): the
      bridge materializes the monolith's `opportunities` list from the
      object's state at event time, AND-combinable with a user
      `support_constraint`; equivalence test against the constructor-supplied
      lists on the existing DyNAMi fixtures.
- [ ] 2.4 Verify: `NOT_CRAN=true` green, DyNAMi baselines PASS; commit.

## 3. Public surface

- [ ] 3.1 `estimate_dynami()` AND `make_specification()` (design D7: both
      surfaces) accept a stocnet `data` for DyNAMi models, resolving the
      actors×groups layer via the mode map and routing through the bridge —
      the DyNAMi spec classes stay thin (consumption remains the monolith);
      no public `opportunities` argument; roxygen updated,
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
      deprecation-cycle tests still need.
- [ ] 4.3 Verify: full `NOT_CRAN=true` suite green, all frozen baselines PASS
      not SKIP; commit.

## 5. Milestone

- [ ] 5.1 Bump `DESCRIPTION` + `NEWS.md` (BREAKING: legacy environments
      rejected everywhere, `as_goldfish()` migration; DyNAMi on the single data
      object); `openspec validate dynami-stocnet-boundary --strict`.
