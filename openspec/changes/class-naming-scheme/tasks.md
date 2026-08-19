## 1. Groundwork

- [ ] 1.1 Build the authoritative rename inventory: for every class in the
  `class-naming` table, list its class-string sites in `R/`, its roxygen
  `@method`/`@export` tags, its `NAMESPACE` entries, its test assertions and its
  snapshot files. Record it in `progress.md`. This is the checklist every later
  task closes against — the diagnostic class names collide with exported function
  names, so the inventory must separate class-string sites from function-name
  sites before any edit (design D9).
- [ ] 1.2 Add the package-wide guard test (`tests/testthat/test_class_naming.R`,
  testthat 3e per the **r-lib:testing-r-packages** skill): construct one object of
  each live class from its documented constructor and assert (a) it inherits the
  `_goldfish` name, (b) it does not inherit the retired name, (c) no class string
  in its class vector contains a `.` except inherited base/tibble classes, and
  (d) the exported function names are unchanged. It fails at this point; it is
  the change's completion criterion.
- [ ] 1.3 Verification: run the **not-cran-test** skill (`NOT_CRAN=true`,
  background) and record the pre-change baseline — the frozen 1e-6 coefficient
  and C++ golden tests must report PASS, not SKIP, so that any later movement is
  attributable to this change.

## 2. Internal, algorithm and specification classes

- [ ] 2.1 Rename `goldfish.formulae` → `formulae_goldfish` (`R/formula_parser.R`,
  single site) and any `inherits()` guard that reads it.
- [ ] 2.2 Rename `algorithm.goldfish` → `algorithm_goldfish` and
  `algorithm_newton.goldfish` → `algorithm_newton_goldfish` (`R/set_opt.R`,
  `print` method, the `inherits(x, "algorithm_goldfish")` gate on `control_algo`
  in the estimators). Hand-edit the roxygen `@method` tags per design D9; run
  `devtools::document()` in this task per the **r-lib:r-package-development**
  skill.
- [ ] 2.3 Rename `specification.goldfish` → `specification_goldfish` and
  `spec_map.goldfish` → `spec_map_goldfish` (`R/make_specification.R`,
  `R/model_spec.R`, the print method, the estimator entry points that accept a
  specification in place of a formula).
- [ ] 2.4 Update the affected tests and re-record the print snapshots, reading
  each snapshot diff before accepting it (design D11) and stating in the commit
  what changed in them.
- [ ] 2.5 Verification: `air format` the touched files, `lintr::lint()` on the
  same files, `devtools::document()`, then the **not-cran-test** skill. Confirm
  the NAMESPACE diff shows the renamed `S3method` entries and no dropped export.

## 3. Preprocessing classes

- [ ] 3.1 Rename `preprocessing.goldfish` → `preprocessing_goldfish`
  (`R/set_opt.R`, the `control_prep` validation in all three estimators, the
  print method).
- [ ] 3.2 Rename `preprocessed.goldfish` → `preprocessed_goldfish` and
  `preprocessed_db.goldfish` → `preprocessed_db_goldfish`
  (`R/preprocess_writers.R`, `R/model_preprocess.R`, the `preprocessed =`
  argument gate on the estimators, `R/format_version.R`'s
  `abort_if_not_class()` call sites, the gather expansion).
- [ ] 3.3 Rename `flavored_preprocessed.goldfish` →
  `flavored_preprocessed_goldfish` and `flavored_statistics.goldfish` →
  `flavored_statistics_goldfish` (`R/preprocess_flavored.R`,
  `R/estimate_flavored.R`).
- [ ] 3.4 Update the affected tests and snapshots, reviewing each diff.
- [ ] 3.5 Verification: `air format`, `lintr`, `devtools::document()`, then the
  **not-cran-test** skill. The preprocessing round-trip tests are the ones that
  catch a dropped `S3method`, so confirm they exercise print dispatch and not
  only values.

## 4. Diagnostic classes

- [ ] 4.1 Rename the four list-shaped diagnostic classes: `test_gof` →
  `test_gof_goldfish` (`R/test_gof.R`), `test_time` → `test_time_goldfish`
  (`R/test_time.R`), `test_parameter` → `test_parameter_goldfish`
  (`R/test_parameter.R`, including the `setdiff(class(body), ...)` demotion),
  `diagnose_onset` → `diagnose_onset_goldfish` (`R/diagnose_onset.R`). Edit class
  strings and roxygen `@method` tags by hand; do **not** touch the identically
  named exported functions (design D9).
- [ ] 4.2 Rename the four tibble-shaped diagnostic classes: `diagnose_outliers`,
  `diagnose_changepoints`, `margin_table` and `evaluate_model` to their
  `_goldfish` forms (`R/diagnostic_tables.R`, `R/model_evaluate.R`), including
  the shared diagnostic-table constructor's `class` argument, the `[` methods,
  and the `margin_table` demotion in `R/diagnostic_tables.R`.
- [ ] 4.3 Update the three class strings in `register_diagnostic_reconstruct()`
  (`R/zzz.R`) so the `dplyr_reconstruct` registration follows the renamed tibble
  classes; verify with dplyr attached that a `filter()` on a diagnostic table
  still demotes on the documented rule.
- [ ] 4.4 Update the affected tests and snapshots. Assert explicitly that
  `export(test_gof)` and the other seven function exports survive in NAMESPACE —
  this is the guard against a search-and-replace having renamed the API.
- [ ] 4.5 Verification: `air format`, `lintr`, `devtools::document()`, then the
  **not-cran-test** skill.

## 5. autograph, in lockstep

- [ ] 5.1 In `/Users/ualvaro/Documents/repos/autograph` on
  `feature/goldfish-diag`, rename the six `plot` methods and their roxygen
  `@method` tags to the `_goldfish` classes (`plot.test_gof_goldfish`,
  `plot.test_time_goldfish`, `plot.diagnose_onset_goldfish`,
  `plot.diagnose_outliers_goldfish`, `plot.diagnose_changepoints_goldfish`,
  `plot.margin_table_goldfish`), and update the `inherits()` guards in
  `R/plot_diagnostics.R`. `test_parameter` and `evaluate_model` have no autograph
  method — confirm rather than assume (design D12).
- [ ] 5.2 Regenerate autograph's NAMESPACE with `devtools::document()` and
  confirm the six `S3method(plot, ...)` entries moved and none was dropped.
- [ ] 5.3 Verification: with both working copies loaded, build one object of each
  of the six classes from goldfish and plot it through autograph; run autograph's
  own test suite. Commit in the autograph repo separately from the goldfish
  commits.

## 6. Result classes and the retired-name stubs

- [ ] 6.1 Rename `result.goldfish` → `result_goldfish` across `R/` — the largest
  surface (131 class-string sites). Principal files: `methods_display.R`,
  `methods_postestimate.R`, `methods_predict.R`, `methods_residuals.R`,
  `methods_tests.R`, `model_estimate.R`, `model_evaluate.R`, `model_terms.R`,
  `format_version.R`, `diagnose_onset.R`, `test_*.R`, `diagnostic_tables.R`.
- [ ] 6.2 Rename `flavored_result.goldfish` → `flavored_result_goldfish` and the
  summary object's class to `summary_result_goldfish`, so the method
  (`summary.result_goldfish()`) and the class it returns are no longer the same
  string (design D6).
- [ ] 6.3 Add the third staleness diagnosis in `R/format_version.R` (design D5):
  a current-epoch object on the retired class is told its **class** was renamed,
  not that its components were. Extend `stale_result_bullets()` rather than
  duplicating it; render with cli semantic elements per the **r-lib:cli** skill,
  interpolating data not literal markup. Do **not** move `FIT_VERSION` or
  `PREP_VERSION` — no component of any object changes.
- [ ] 6.4 Write the two retired-name stubs, `print.result.goldfish` and
  `summary.result.goldfish`, alongside the existing retired-name shims in
  `R/goldfish-defunct.R` (or a sibling file), following the **r-lib:lifecycle**
  skill for the badge and the NEWS wording. Register exactly these two and no
  other generic on the retired name (design D4).
- [ ] 6.5 Test the stubs on both populations with constructed fixtures: an object
  carrying `result.goldfish` and no `fit_version` (released-goldfish shape), and
  one carrying `result.goldfish` and the current epoch (dev-line shape). Assert
  the two messages differ and that the second does not claim the components were
  renamed. Snapshot both under a pinned cli context.
- [ ] 6.6 Update the affected tests and snapshots — the largest set (54 test
  sites, plus every snapshot whose printed output names the class). Review each
  diff.
- [ ] 6.7 Verification: `air format`, `lintr`, `devtools::document()`, then the
  **not-cran-test** skill. The frozen 1e-6 baselines must be unchanged; a moved
  coefficient means something other than a name changed and the task stops rather
  than regenerating.

## 7. The data.goldfish split

- [ ] 7.1 Change `as_goldfish()` to stamp `data_goldfish` instead of
  `data.goldfish` (`R/as_goldfish.R`), leaving the legacy environment built by
  `make_data()` and the DyNAMi path on `data.goldfish` (design D3).
- [ ] 7.2 Split print dispatch: give the stamped stocnet its own
  `print.data_goldfish` and leave the legacy environment's method on the old
  class, so the two objects no longer share a method by accident. Audit every
  `inherits(x, "data.goldfish")` guard in `R/` and decide per site which of the
  two objects it means — this is the one task where a mechanical rename is
  actively wrong.
- [ ] 7.3 Test both producers in one session: assert each object inherits its own
  class, neither inherits the other's, and each prints under its own method.
- [ ] 7.4 Verification: `air format`, `lintr`, `devtools::document()`, then the
  **not-cran-test** skill. Confirm the DyNAMi path still builds its legacy
  environment unchanged.

## 8. Documentation and the living spec

- [ ] 8.1 Sweep `man/` by regenerating with `devtools::document()`, then grep the
  generated pages for every retired class string and resolve what remains (a hit
  after regeneration means a roxygen block still names the old class in prose,
  which must be hand-edited — never scripted).
- [ ] 8.2 Sweep the vignettes: edit the `.Rmd.orig` sources (never the generated
  `.Rmd`) where a class is named in prose or appears in output, then rebuild with
  `Rscript vignettes/rebuild-all.R` against the installed goldfish.
- [ ] 8.3 Sweep the living spec (design D8): hand-correct the stale class
  spellings in the requirements of `openspec/specs/**` that this change does not
  carry a delta for, then grep `openspec/specs/` for every retired class string
  and require an empty result — excluding the four deprecated data classes and
  the legacy `data.goldfish`, which are intentionally retained. Diff-review the
  result; do not script the edit.
- [ ] 8.4 Verification: `devtools::check()` for documentation completeness, and
  re-run `bash .plan/opsx-spec-placement-check.sh class-naming-scheme` plus
  `openspec validate class-naming-scheme --strict`.

## 9. Close

- [ ] 9.1 Confirm the guard test from 1.2 now passes in full, and that it covers
  every row of the `class-naming` rename table.
- [ ] 9.2 Bump `DESCRIPTION` 1.9.28 → 1.9.29 and add a single consolidated
  `NEWS.md` entry under a **Breaking changes** heading carrying the complete
  old → new table, the `data.goldfish` split, the retired-name stub behavior, and
  the statement that stored fits must be re-fitted (design D13).
- [ ] 9.3 Final verification: the **not-cran-test** skill on the whole suite, with
  the frozen 1e-6 coefficient and C++ golden baselines reporting PASS, not SKIP.
- [ ] 9.4 Answer [stocnet/autograph#60](https://github.com/stocnet/autograph/issues/60)
  with the final table, noting the three departures from the proposal: the
  `_goldfish` separator rather than `.goldfish` (with the
  `plot.test_gof.goldfish` ambiguity as the reason), the two classes the issue
  missed (`test_parameter`, `evaluate_model`), and that autograph's methods are
  already updated on `feature/goldfish-diag`.
