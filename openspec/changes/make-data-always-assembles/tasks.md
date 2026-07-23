> Follow `openspec/config.yaml` disciplines: commit-per-task, run
> `devtools::document()` inline when roxygen/exports/signatures change, test with
> `NOT_CRAN=true` (frozen baselines PASS not SKIP), bump `DESCRIPTION` +
> `NEWS.md` at the milestone. Depends on `dynami-stocnet-boundary` (which deferred
> the public environment abort to this change).

## 1. Grounding

- [ ] 1.1 Confirm the exact set of inputs for which `make_data()` currently
      returns a `data.goldfish` environment: enumerate the `FALSE` branches of
      `is_stocnet_assemblable()` (`R/legacy_wrappers.R`) and verify the
      user-facing one is the unresolvable node-set name (`nodes = fx$actors`
      deparse); confirm the subset-dependent path already assembles (flavored by
      dependent name) and is untouched. Record which in-repo fixtures hit the
      env fallback (`test-support_constraint_rate`/`_rem`, `test-model_spec`).

## 2. make_data assembles or aborts

- [ ] 2.1 Make `is_stocnet_assemblable()` surface the unresolved node-set
      reference(s) (return them, or expose a helper the caller uses) instead of a
      bare `FALSE`, without changing its assemblable/`TRUE` verdict.
- [ ] 2.2 In `make_data()`, replace the silent environment fallback for user
      input with a `cli_abort()` that names the unresolved node-set reference and
      shows the fix (bind the node set to a plain name); other non-assemblable
      shapes (no node set / no layer) abort with their own clear reason. Snapshot
      test the abort under a pinned cli context.
- [ ] 2.3 Verify the subset-dependent flavoring is unchanged: a strict-subset
      dependent still assembles to a stocnet flavored by the dependent object's
      name and models exactly its rows (equivalence guard, no behavior change).

## 3. Fixture migration

- [ ] 3.1 Bind a plain node-set name in the fixtures that build a covariate
      network with `nodes = fx$actors` (`test-support_constraint_rate`/`_rem`,
      `test-model_spec`) so they assemble to a stocnet; confirm those suites pass
      under `NOT_CRAN=true`.

## 4. The public environment abort

- [ ] 4.1 Restore the public `is.environment(data)` abort at
      `estimate_dynam()` / `estimate_rem()` / `estimate_dynami()` /
      `make_specification()` (a shared helper), naming the migration (rebuild via
      the constructors); keep it out of `estimate_wrapper()` so the DyNAM-i bridge
      environment still flows. Snapshot-test the abort on each surface under a
      pinned cli context.
- [ ] 4.2 Verify: the internal DyNAM-i bridge path (stocnet in, bridge env built
      after the guard) still estimates; `as_goldfish()` still aborts on an
      environment (conversion remains deferred).

## 5. Verify + milestone

- [ ] 5.1 Full `NOT_CRAN=true` suite green, all frozen baselines PASS not SKIP;
      `lintr` clean on touched files; `devtools::document()`; `openspec validate
      make-data-always-assembles --strict`.
- [ ] 5.2 Bump `DESCRIPTION` + `NEWS.md` (BREAKING: legacy environments rejected
      at the public surface; an unresolvable node-set name now errors instead of
      silently returning an environment). Then `/opsx:archive`.
