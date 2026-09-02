# imputation-contract — tasks

> **Precondition:** `multimode-network-support` task 3.9 is committed (single
> resolver, mode-category stratification, recorded type/missingness metadata,
> schedule-construction abort). Tasks 3.x and 4.x below extend that resolver
> and metadata; do not start them before 3.9 lands. Tasks 1.x and the doc half
> of 2.x can start early.

## 1. Audit and groundwork

- [x] 1.1 Missingness audit of the frozen baselines: enumerate missing values
      by shape (dyad / global / nodal numeric / nodal categorical) in every
      dataset the `global_v1` baselines, shipped `data/` objects, and examples
      touch. Record the per-shape result in `progress.md`. This decides
      whether the global abort (design D2) lands directly or behind a
      `lifecycle` deprecation — do not start task group 3 before this is
      recorded.
- [x] 1.2 Regression fixtures for the policy surface: a stocnet fixture with a
      categorical attribute missing by design (irps_nuclear-shaped: an
      attribute undefined for one mode category's real-world reason), a
      numeric attribute with sparse missingness, and a global attribute
      variant with a missing initial value and one with a missing `replace`
      event. Reuse `make_stocnet_fixture_multimode()` machinery where it fits.
      Verify + commit.

## 2. Contract documentation and tertius reclassification (design D1, D4)

- [x] 2.1 Write the shape × time contract as user-facing documentation: a
      "Missing data" section on the data-object help topic (canonical home,
      inherited elsewhere — never duplicated) and a matching subsection in the
      modeling vignette. Every cell stated, including the two abort cells and
      the dyad "missing tie is an absent tie, by definition" wording, plus the
      **imputation feedback disclosure** (design D1): an imputed value joins
      the state, so a value missing after the start of the window is
      summarized from a pool that may contain earlier imputed values, and
      single imputation therefore understates uncertainty.
      `devtools::document()` in the same task. Verify + commit.
- [x] 2.2 Reclassify the tertius empty-neighborhood sites
      (`functions_effects_DyNAM_choice.R:3019`, `:3217`, `:3230`): document
      the default in the tertius effect docs as part of the statistic's
      definition, rename internals/comments away from "impute" vocabulary,
      and add a regression test pinning the numeric behavior unchanged
      against current values. No coefficient may move — baselines PASS.
      Verify + commit.
- [x] 2.3 Multiple-imputation recommendation (design D6): "better strategies"
      subsection in the missing-data docs — impute before building goldfish
      objects (m completed datasets, including attribute event streams for
      time-varying covariates), fit per dataset, combine with
      `mitools::MIcombine()` via the existing `coef`/`vcov` methods; state
      the Rubin's-rules validity caveat (approximately normal, congenial
      estimates — simple specifications). Add `mitools` to `Suggests`, guard
      the example with `@examplesIf requireNamespace("mitools", quietly =
      TRUE)`, and add a verification test (`skip_if_not_installed("mitools")`)
      that combines two real fits and checks combined estimates and standard
      errors come back — the "works with MIcombine" claim is tested, not
      asserted. `devtools::document()` inline. Verify + commit.

## 3. Global-shape abort (design D2)

- [x] 3.1 Route the global shape through the metadata pass: record initial and
      event-stream missingness for global attributes (the pass already does
      this for nodal ones) and abort at schedule construction naming the
      object and, for an event value, the event time — cli error, tested via
      snapshot in a pinned cli context. Remove the walk-time `NA → 0` global
      branch and the initial `mean()`-of-length-one path. If task 1.1 found
      any baseline exercising either cell, ship the abort behind
      `lifecycle::deprecate_warn` instead and keep the old behavior as the
      warned default. Verify + commit.
- [x] 3.2 NEWS entry for the global-cell behavior change (its own bullet, so
      it stays traceable), DESCRIPTION patch bump for the phase milestone.
      Full suite `NOT_CRAN=true`, baselines PASS not SKIP. Commit.

## 4. Per-attribute imputation policy (design D3)

- [x] 4.1 Policy plumbing: `set_preprocessing_opt()` gains the imputation
      policy argument (named character vector keyed by attribute; settle the
      argument name and reserved-level string here, snake_case, American
      English). Validation aborts on unknown attribute names, unknown policy
      values, the reserved-but-unimplemented `locf` value (error lists
      supported values), and `as_category` on a numeric attribute. Default
      path with no policy declared is byte-identical behavior — assert via
      the existing baselines. `devtools::document()` inline. Verify + commit.
- [x] 4.2 `as_category` at the seam: in `ds_impute_missing()`, recode missing
      values of a declared categorical attribute to the reserved level in the
      initial table and in the attribute's event streams, before effect init;
      abort on reserved-level collision naming the attribute. Decide the
      legacy envir path here (honor identically or abort naming the
      limitation — design Open Question 2; silent divergence between paths is
      not an option) and test whichever is chosen on both paths. Verify +
      commit.
- [x] 4.3 Behavioral tests from the spec scenarios: deliberate missingness
      survives to a summarizer (effect sees the reserved level, no node gets
      the most common category), event-stream missing `replace` writes the
      reserved level with no pooling, numeric-declaration abort, collision
      abort, unknown-attribute abort, `locf` reserved abort. testthat 3e,
      self-contained, snapshots in a pinned cli context. Verify + commit.
- [x] 4.4 NEWS entry for the policy surface (opt-in, no default change),
      DESCRIPTION bump for the phase milestone. Full suite `NOT_CRAN=true`,
      baselines PASS not SKIP. Commit.

## 5. Coordination and close-out

- [x] 5.1 Coordinate with `multimode-network-support` 5.1/5.3: if the
      `irps_nuclear` vignette has landed, replace its hand-rolled sentinel
      recodes with the declared policy; if not, leave a note in that change's
      `progress.md` to use the policy from the start.
- [x] 5.2 Contract docs final pass: confirm the published table matches
      implemented behavior cell by cell (including which abort path shipped
      for D2), confirm no "mode"-as-statistic wording anywhere user-facing
      (say "most common value"), `devtools::document()`, rebuild the affected
      vignette. Verify + commit.
- [x] 5.3 Full `NOT_CRAN=true` suite green, baselines PASS not SKIP,
      `lintr::lint_package()` clean on touched files. Update `progress.md`
      with the session ledger. Ready for `/opsx:archive`.
