# Tasks — snake-case-result-components

Disciplines (openspec/config.yaml): one focused conventional commit per task,
tests green at every commit, `devtools::document()` inline when roxygen /
exports / signatures change, `air format` the touched R files before `lintr`,
`NOT_CRAN=true` with the frozen baselines PASS (not SKIP) before each commit,
r-lib skills (r-package-development, testing-r-packages, cli, lifecycle) invoked
before the work they cover. The lifecycle skill governs every rename here.

## 0. Settle scope before renaming anything

- [x] 0.1 Re-derive the catalogue from the code rather than trusting design.md's
      table: every component assigned onto a returned object, plus every
      `\item{}` in the estimators' and `gather_model_data()`'s `@return`. A
      partial sweep leaves the same guessing problem with a shorter list, so
      completeness is the gate
- [x] 0.2 Settle the design's remaining open question: where the format stamp
      lives on the object, and whether the preprocessed object needs one too (it
      changes shape at 2.0.0 for the same reasons). Everything else about the
      window and `nParams` is already decided — see D2, D2a, D2b, and do not
      re-open them
- [x] 0.3 Sweep the in-repo consumers that will break: `.plan/` scripts, the
      `residuals_comparison` harness, tests, vignettes. List them, so task 3.1
      has a checklist rather than a search

## 1. The fitted result

- [x] 1.1 Rename the result's own components (`standardErrors`, `logLikelihood`,
      `finalScore`, `finalInformationMatrix`, `nIterations`, `nEvents`,
      `nParams`, `intervalLogL`, `eventProbabilities`) at every assignment site
      across `R/estimation_core.R`, `R/cpp_interface.R`, `R/model_estimate.R`,
      and every reader in `R/methods_postestimate.R` / `R/methods_display.R`.
      Update the `@return` block; `devtools::document()` inline
- [x] 1.2 Rename the `convergence` sub-list (`isConverged`, `returnCode`,
      `maxAbsScore`, `maxAbsUpdate`), leaving `score_rel_norm` alone — it is
      already correct, and it is the reason this list is the clearest example of
      the problem
- [x] 1.3 Record the format version on newly fitted objects (D2), placed per
      0.2's decision

## 2. The export and preprocessed objects

- [x] 2.1 Gather export: `namesEffects`, `isDependent`. Update the
      `gather_model_data()` `@return`; check the db-export descriptor carries the
      same spelling
- [x] 2.2 Preprocessed object: `initialStats`, `rightCensoredIntervals`,
      `rightCensoredStatsChange`, `orderEvents`, `dependentStatsChange`
- [x] 2.3 Tests: the export and preprocessed objects expose snake_case
      component names, per the second scenario of the "Components of returned
      objects are named in snake_case" requirement

## 3. Recognize pre-rename objects

Severity is decided (D2a) and is not a matter of taste: `print` informs, the
computing surfaces error. The reason is the concrete bug this replaces — today a
CRAN 1.6.12 object returns `logLik()` with `df = NULL`, so `AIC()` and `BIC()`
report wrong numbers in silence, while `summary()` fails with "argument is of
length zero".

- [x] 3.1 One shared guard recognizing a pre-rename object, by its format stamp
      when present and otherwise by a retired camelCase component
      (`logLikelihood`, or the already-retired `subModel` — the 1.7.0 renames
      broke these objects before this change existed). It only ever recognizes;
      it never translates, which is what keeps it maintenance-free
- [x] 3.2 Call it from every S3 method on the class. `print` / `format` inform
      and still show what they can; `logLik`, `vcov`, `summary` and the
      `AIC`/`BIC` path error. Messages via cli, naming the cause and telling the
      user to re-fit
- [x] 3.3 Tests: build a 1.6.12-shaped object as a fixture and assert each
      surface's behavior; assert a currently fitted object triggers none of it.
      Snapshot the messages with a pinned cli context

## 4. Consumers and the reserved variant

- [x] 4.1 Migrate the in-repo consumers listed in 0.3. The
      `residuals_comparison` harness is the one that matters most — it is the
      cross-package parity evidence, and a silent `NULL` there would read as a
      parity failure rather than a rename
- [x] 4.2 Delete the reserved `engine` parameter and its abort from
      `R/model_spec.R` (roxygen at `:458-459`, parameter at `:471`, abort at
      `:480-486`) and the test pinning the abort in
      `tests/testthat/test-model_spec.R:118`. Keep the comments at
      `R/estimation_core.R:357` and `:1199` about state reuse by algorithm
      variants; drop only the naming of a variant that will not ship

## 5. Closure

- [x] 5.1 NEWS entry as **BREAKING**, listing every renamed component with its
      old and new spelling, and stating that the old names stop working
      immediately rather than after a deprecation period — with the reason, so a
      reader does not take it for an oversight. DESCRIPTION version bump
- [x] 5.2 Full `NOT_CRAN=true` suite green with the frozen baselines PASS (not
      SKIP), `openspec validate` green, and
      `bash .plan/opsx-spec-placement-check.sh snake-case-result-components`
      green

## 6. Follow-ups added after 5.2 (see design D4, D5, D5a, D6)

Phase 5 closed at 16/16 and the change was reopened deliberately for these. The
version work is a **replacement, not a deprecation**: neither constant was ever
public, so no lifecycle cycle is owed — but the r-lib:lifecycle skill still
governs the judgement that none is owed, per `openspec/config.yaml`.

- [x] 6.1 Rename the internal `order_events()` → `arrange_events()` (D4):
      definition `R/event_streams.R:31`, calls `R/data_source.R:833,896,918,932`,
      and the ~8 call sites in `tests/testthat/test-event_streams.R`. Rename that
      file's helper `make_order_events()` → `make_unorder_events()`, which is what
      it actually builds. The preprocessed component keeps `order_events` — only
      the function moves
- [x] 6.2 Regenerate the purled `vignettes/teaching2.R`, which still passes the
      `return_interval_loglik` argument deprecated at 1.9.11 and so contradicts
      the `.Rmd.orig` it derives from. `precompile.R` runs `knitr::knit()` **and**
      `knitr::purl()` per vignette; session 2 ran only the first. Knit against
      `devtools::load_all()`, not the installed build, or `print.stocnet` does not
      dispatch and the captured output becomes a raw list dump
- [x] 6.3 Replace both version constants with the two siblings `FIT_VERSION <- 2L`
      and `PREP_VERSION <- 2L` in `R/format_version.R`, parallel in structure and
      the only two defined (D5). Removes `goldfish_result_format` and
      `PREPROCESSED_GOLDFISH_VERSION` outright. Sites: constant
      `R/model_preprocess.R:4`, writers `R/model_preprocess.R:133` and
      `R/preprocess_writers.R:776`, guard `R/model_estimate.R:1236`, and the
      `R/format_version.R` comment pointing at its sibling
- [x] 6.4 Rename the components to match: `fit$format_version` → `fit$fit_version`
      and `prep$version` → `prep$prep_version`, so the two stamps stay distinct
      when a preprocessed object is carried inside a fit (D5). Update the
      estimators' `@return` and `devtools::document()` inline
- [x] 6.5 Gate each guard on the object's class before consulting its stamp
      (D5a), so a preprocessed object passed where a fit is expected is refused on
      kind rather than accepted because a stamp was absent
- [x] 6.6 Tests: `test-goldfish-deprecated.R:126` fakes staleness via
      `stale$version <- 0L` and will fail loudly once the component is renamed —
      repoint it at `prep_version`. Repoint `test-compute_statistics.R:243,246`
      and `test-preprocess_writers.R:191` at `PREP_VERSION`, and
      `test-format_version.R` / `test-stale_result_detection.R` at `FIT_VERSION`.
      Add the two new spec scenarios: a stale preprocessed object refused at
      estimation entry, and a preprocessed object refused on kind by a fit guard.
      Keep the present-and-non-empty premise guard in every new assertion
- [x] 6.7 Fold the NEWS version bullets into one statement covering both objects
      (D6), add `version` → `prep_version` to the preprocessed rename list, and
      keep `DESCRIPTION` at 1.9.16 — phase 6 does not earn a second bump
- [x] 6.8 Verification: full `NOT_CRAN=true` suite green with the frozen baselines
      PASS (not SKIP), `openspec validate` green, and
      `bash .plan/opsx-spec-placement-check.sh snake-case-result-components`
      green — the spec delta gained requirements in session 3, so re-run both
      gates rather than trusting the session-2 result

## 7. Collapse the fit/prep detection asymmetry (see design D7)

One rule for both objects: no version record means the object predates the record.
Behaviorally indistinguishable from phase 6 on every object a user can possess —
the fallback only ever changed the verdict for hand-built test fixtures — so this
is a simplification, not a behavior change, and NEWS needs no new bullet.

- [x] 7.1 Make absence conclusive for the fit as it already is for the
      preprocessed object: drop `retired_result_components` and the `when_absent`
      argument, leaving `format_status(stamp, current)` with an absent or `NA`
      stamp returning `"outdated"`. `result_format_status()` and
      `preprocessed_format_status()` become one-line siblings over their own slot
      and their own constant
- [x] 7.2 Stamp the two fixtures that were relying on the fallback, so each
      declares the layout it emulates rather than inheriting a default:
      `resModObject` (`R/zzz_testthat_helpers.R`, reached by `summary()` about a
      dozen times in `test-methods_display.R`) and `fake_fit()`
      (`tests/testthat/test-baselines_helper.R`, classed specifically so
      `coef()` / `logLik()` route through the package's own methods)
- [x] 7.3 Invert the "an unstamped object without retired components is left
      alone" test in `test-stale_result_detection.R`: an object carrying no record
      is now reported as predating it, by the same rule for both kinds, which is
      the new spec scenario. Keep asserting the fixtures' own stamps so a future
      layout move fails on them loudly
- [x] 7.4 Keep the class gate but correct what it is for (D5a revised): the
      verdict no longer depends on it, the *diagnosis* does — without it a
      freshly built preprocessed object is refused as "fitted before goldfish
      2.0.0", false about both its age and its kind. Adjust the comment, which
      currently claims the gate prevents silent acceptance
- [x] 7.5 Verification: full `NOT_CRAN=true` suite green with the frozen baselines
      PASS (not SKIP), `openspec validate` green, placement check green. The spec
      delta gained a scenario in session 5, so re-run both gates
