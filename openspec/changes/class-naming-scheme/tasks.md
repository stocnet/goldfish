# Tasks — class-naming-scheme

Revised 2026-08-19 for the camelCase scheme (ADR-0031). Folds next,
pre-2.0.0, before `parametric-rates`.

## 1. Groundwork

- [x] 1.1 Build the authoritative rename inventory: for every class in
  the `class-naming` table — now including the internal classes
  (`writer_*`, `data_source_*`, `model_spec` hierarchy,
  `support_constraint_plan`, `fixed_spec`/`initial_spec`) — list its
  class-string sites in `R/`, roxygen `@method`/`@export` tags,
  NAMESPACE entries, test assertions, and snapshot files; record the
  exact internal class strings and their `goldfish<Thing>` targets in
  `progress.md`. The old diagnostic class names collide with exported
  function names, so the inventory separates class-string sites from
  function-name sites before any edit (design D9). Confirm every
  effect-tag string stays off the list (D2 exemption).
- [x] 1.1a Confirm the `model_spec` hierarchy is in scope (design D18,
      settled 2026-09-05: the rows stay and the rename waste is accepted).
      The twelve identifiers are fixed in the rename table — nine
      `goldfishKind<Variant>` plus the parent, and
      `goldfishAxisSender`/`goldfishAxisDyad` for the axis. Note in
      `progress.md` which of them `model-spec-descriptor` is expected to
      dissolve, so the next change does not re-derive it.
- [x] 1.2 Lint spike (design D15): one throwaway file declaring a
  method per generic family on a camelCase class
  (`print.goldfishFit`, `diagnose_onset.goldfishFit`, …); run
  `lintr::lint()` with the project `.lintr`. If `object_name_linter`
  flags method names, adjust `.lintr` with a documented setting in this
  commit; no scattered `# nolint`.
- [x] 1.3 Add the package-wide guard test
  (`tests/testthat/test_class_naming.R`, testthat 3e per
  **r-lib:testing-r-packages**). It **enumerates the classes the package
  actually attaches** — from `S3method()` registrations, from literal
  strings at every class-assignment form (`class(x) <-`,
  `structure(class = )`, `attr(x, "class") <-`), and from literal strings in
  `inherits()`/`is()` calls — subtracts the three exempt categories
  (deprecated path, effect dispatch tags, `goldfish_<snake>` condition
  classes), and asserts every remainder matches `goldfish<Thing>`. It
  SHALL NOT read the rename table: a class minted later would pass by
  omission, which is how the table drifted twice. Also assert (a) no live
  class string contains a dot, inherited base/tibble classes excepted, and
  (b) the exported function names are unchanged. It fails now; it is the
  completion criterion.
      Known limit: the scan is best-effort. `dyad_spec` escaped a first
      attempt because it appears only inside `inherits(x, c(...))`, so
      verify the enumeration finds all twelve `model_spec` classes before
      trusting it.
- [x] 1.3a Converge the class idioms (design D20) **before** the rename
      clusters, so the later inventories see one form. Replace the six
      `attr(x, "class") <-` sites with `class(x) <-`: three stamp
      `result.goldfish` (`estimation_core.R:320`, `cpp_interface.R:756`,
      `:905`) and three the DyNAM-i interaction classes
      (`make_data_group.R:924,928,932`). Replace the single
      `methods::is(seed_randomization, "numeric")` (`make_data_group.R:59`)
      with `is.numeric()` — it is a type check, not a class test.
      `%in% class(x)` has zero sites; it is a prohibition in the spec, not a
      migration. Pure refactor: no class string moves here, so the suite must
      be green with an identical NAMESPACE.
- [x] 1.4 Amend the tracked `CLAUDE.md` naming policy (design D14):
  class strings follow `goldfish<Thing>` camelCase per ADR-0031 /
  autograph CONTRIBUTING; snake_case (and "never reintroduce
  camelCase") continues to govern functions, arguments, and objects.
- [x] 1.5 Verification: **not-cran-test** skill (`NOT_CRAN=true`,
  background); record the pre-change baseline — frozen 1e-6 and C++
  goldens PASS, not SKIP.

## 2. Formulae, algorithm, and specification classes

- [x] 2.1 Rename `goldfish.formulae` → `goldfishFormulae`
  (`R/formula_parser.R`, single site, plus any `inherits()` guard).
- [x] 2.2 Rename `algorithm.goldfish` → `goldfishAlgo` and
  `algorithm_newton.goldfish` → `goldfishAlgoNewton`
  (`R/set_opt.R`, print method, the `control_algo` gates in the
  estimators). Hand-edit roxygen `@method` tags (D9); run
  `devtools::document()` in-task per **r-lib:r-package-development**.
- [x] 2.3 Rename `specification.goldfish` → `goldfishSpec` and
  `spec_map.goldfish` → `goldfishSpecMap` (`R/make_specification.R`,
  `R/model_spec.R`, print method, estimator entry points accepting a
  specification).
- [x] 2.4 Update affected tests; re-record print snapshots, reading
  each diff before accepting (D11), stating in the commit what changed.
- [x] 2.5 Verification: `air format` touched files → `lintr` on the
  same files → `devtools::document()` → **not-cran-test**. NAMESPACE
  diff shows renamed `S3method` entries, no dropped export.

## 3. Internal classes (new scope)

- [x] 3.1 Rename the writer classes → `goldfishWriterDefault` /
  `goldfishWriterGather` / `goldfishWriterDB` (exact current strings
  from inventory 1.1; `R/preprocess_writers.R` and dispatch sites).
- [x] 3.2 Rename `data_source_envir` / `data_source_stocnet` →
  `goldfishSourceEnvir` / `goldfishSourceStocnet` (`R/data_source.R`
  and every `inherits()` seam, including the DyNAMi boundary guards).
- [x] 3.3 Rename the `model_spec` hierarchy → `goldfishKind*` (parent
  `goldfishKind`; axis classes → `goldfishAxisSender`/`goldfishAxisDyad`),
  `support_constraint_plan` → `goldfishSupportPlan`, and `fixed_spec` /
  `initial_spec` → `goldfishCoefFixed` / `goldfishCoefInit` (exact
  strings per inventory; `R/model_spec.R`, the support-constraint and
  fixed-parameter files).
- [x] 3.3a Rename the DyNAM-i interaction classes → `goldfishInterNet` /
      `goldfishInterGrp` / `goldfishInterWindow` (`R/make_data_group.R`
      stamps them; `R/model_preprocess_group.R:155,161,167` reads them; plus
      `R/zzz_testthat_helpers.R`). They lose their only consumer when
      `refactor-dynami-engine` converts the `preprocessInteraction` monolith —
      record that in `progress.md` so the successor knows they are already
      renamed.
- [x] 3.4 Update affected tests/snapshots (diff-reviewed).
- [x] 3.5 Verification: `air format` → `lintr` → `document()` →
  **not-cran-test**; full suite green before the larger clusters.

## 4. Preprocessing classes

- [x] 4.1 Rename `preprocessing.goldfish` → `goldfishPrepCtrl`
  (`R/set_opt.R`, the `control_prep` validation in all three
  estimators, print method).
- [x] 4.2 Rename `preprocessed.goldfish` → `goldfishStat` and
  `preprocessed_db.goldfish` → `goldfishStatDB`
  (`R/preprocess_writers.R`, `R/model_preprocess.R`, the
  `preprocessed =` argument gate, `R/format_version.R`
  `abort_if_not_class()` call sites, the gather expansion).
- [x] 4.3 Rename `flavored_preprocessed.goldfish` →
  `goldfishFlavPrep` and `flavored_statistics.goldfish` →
  `goldfishFlavStat` (`R/preprocess_flavored.R`,
  `R/estimate_flavored.R`).
- [x] 4.4 Update affected tests/snapshots (diff-reviewed).
- [x] 4.5 Verification: `air format` → `lintr` → `document()` →
  **not-cran-test**; the preprocessing round-trip tests must exercise
  print dispatch, not only values.

## 5. Diagnostic classes (autograph-fixed names)

- [x] 5.1 Rename the list-shaped diagnostics: `test_gof` →
  `goldfishGOF` (`R/test_gof.R`), `test_time` → `goldfishTimeTest`
  (`R/test_time.R`), `test_parameter` → `goldfishParamTest`
  (`R/test_parameter.R`, incl. the `setdiff(class(body), ...)`
  demotion), `diagnose_onset` → `goldfishOnset`
  (`R/diagnose_onset.R`). Class strings and roxygen `@method` tags by
  hand; do **not** touch the identically named exported functions (D9).
- [x] 5.2 Rename the tibble-shaped diagnostics: `diagnose_outliers` →
  `goldfishOutliers`, `diagnose_changepoints` →
  `goldfishChangepoints`, `margin_table` → `goldfishMargins`,
  `evaluate_model` → `goldfishEval` (`R/diagnostic_tables.R`,
  `R/model_evaluate.R`, the shared diagnostic-table constructor's
  `class` argument, the `[` methods, the demotions).
- [x] 5.3 Update the three class strings in
  `register_diagnostic_reconstruct()` (`R/zzz.R`); verify with dplyr
  attached that `filter()` on a diagnostic table still demotes on the
  documented rule.
- [x] 5.4 Update affected tests/snapshots. Assert explicitly that
  `export(test_gof)` and the other seven function exports survive in
  NAMESPACE — the anti-`sed` guard.
- [x] 5.5 Verification: `air format` → `lintr` → `document()` →
  **not-cran-test**.

## 6. autograph dispatch verification (no rename work)

- [ ] 6.1 With goldfish (post-cluster-5) and autograph@develop loaded:
  build one object of each of the seven autograph-plotted classes
  (`goldfishFit`, `goldfishGOF`, `goldfishTimeTest`,
  `goldfishOutliers`, `goldfishChangepoints`, `goldfishOnset`,
  `goldfishMargins`), plot each, and confirm dispatch reaches the new
  methods — not the defunct aliases (design D12). Confirm
  `goldfishParamTest` and `goldfishEval` have no autograph method.
- [ ] 6.2 Record in `progress.md` (and in issue #60's answer) that
  autograph's defunct aliases are now deletable; open/ping the
  autograph cleanup issue rather than deleting from this change.

## 7. Result classes and the retired-name stubs

- [x] 7.1 Rename `result.goldfish` → `goldfishFit` across `R/` — the
  largest surface (131 class-string sites; principal files
  `methods_display.R`, `methods_postestimate.R`, `methods_predict.R`,
  `methods_residuals.R`, `methods_tests.R`, `model_estimate.R`,
  `model_evaluate.R`, `model_terms.R`, `format_version.R`,
  `diagnose_onset.R`, `test_*.R`, `diagnostic_tables.R`).
- [x] 7.2 Rename `flavored_result.goldfish` → `goldfishFlavFit`; class
  the summary object `goldfishSummFit` (design D6, reversed 2026-09-05)
  and render it with `print.goldfishSummFit()`. The method stays
  `summary.goldfishFit()` — a method name, not a class name; the class it
  returns is `goldfishSummFit`, which is the whole point of the reversal.
  `summary`/`tidy`/`glance` are absent from the flavored class today, so
  whether a `goldfishSummFlavFit` exists is ADR-0038's contract-table
  question, not this task's.
- [x] 7.2a Keep the fit classes flat (design D19): `goldfishFlavFit` is
      renamed as a flat class, **not** `c("goldfishFlavFit", "goldfishFit")`.
      This change disclaims the shared-parent question; introducing
      inheritance here would answer it silently.
- [x] 7.3 Add the third staleness diagnosis in `R/format_version.R`
  (design D5): a current-epoch object on the retired class is told its
  **class** was renamed, not its components. Extend
  `stale_result_bullets()`; render via cli semantic elements per
  **r-lib:cli**. Do not move `FIT_VERSION`/`PREP_VERSION`.
- [x] 7.4 Write the two retired-name stubs (`print.result.goldfish`,
  `summary.result.goldfish`) alongside the retired-name shims in
  `R/goldfish-defunct.R` (or sibling), per **r-lib:lifecycle**;
  register exactly these two and no other generic (design D4).
- [x] 7.5 Test the stubs on both populations (no-epoch released shape;
  current-epoch dev shape); assert the messages differ and the second
  never claims components were renamed; snapshot under a pinned cli
  context.
- [x] 7.6 Update the affected tests and snapshots — the largest set
  (54 test sites plus every snapshot naming the class). Review each
  diff.
- [x] 7.7 Verification: `air format` → `lintr` → `document()` →
  **not-cran-test**. Frozen baselines unchanged; a moved coefficient
  stops the task.

## 8. The data.goldfish split

- [x] 8.1 `as_goldfish()` stamps `goldfishData` instead of
  `data.goldfish` (`R/as_goldfish.R`); the legacy environment stays on
  `data.goldfish` (design D3).
- [x] 8.2 Split print dispatch (`print.goldfishData` for the stamp;
  legacy keeps its method). Audit every
  `inherits(x, "data.goldfish")` guard per site — the one task where a
  mechanical rename is actively wrong.
- [x] 8.3 Test both producers in one session: each inherits its own
  class, neither the other's, each prints under its own method.
- [x] 8.4 Verification: `air format` → `lintr` → `document()` →
  **not-cran-test**; the DyNAMi path still builds its legacy
  environment unchanged.

## 9. Documentation, living spec, and active-change deltas

- [ ] 9.1 Sweep `man/` by regenerating with `devtools::document()`;
  grep for every retired class string; hand-edit roxygen prose that
  still names one (never scripted).
- [ ] 9.2 Sweep the vignettes: edit `.Rmd.orig` sources only, rebuild
  with `Rscript vignettes/rebuild-all.R` against the installed
  goldfish.
- [ ] 9.3 Sweep the living spec (design D8) **and, in the same
  commit, the active-change verbatim delta copies (design D8a, D8b)**:
  `parametric-rates/specs/model-specification`,
  `two-sided-coordination/specs/model-specification`,
  `two-sided-coordination/specs/multivariate-specification`,
  `parametric-rates` design D11's `_goldfish` mention, and — if still
  unarchived at this point — `joint-parameters/specs/multivariate-specification`
  (design D8b; skip if `joint-parameters` has already archived with its
  own rename done, task 3 there). Then grep
  `openspec/specs/` and all three changes' `specs/` for every retired class
  string and require an empty result — excluding the deprecated-path
  names, which are intentionally retained. Hand-edited, diff-reviewed.
- [ ] 9.4 Verification: `devtools::check()`;
  `bash .plan/opsx-spec-placement-check.sh class-naming-scheme`;
  `openspec validate class-naming-scheme --strict`; re-run the
  placement check for `parametric-rates` and `two-sided-coordination`
  (and `joint-parameters` if swept in 9.3) after the D8a/D8b sweep.

## 10. Close

- [ ] 10.1 The guard test from 1.3 passes in full over the enumerated
  class set, internals included. Cross-check once, by hand, that every
  rename-table row is reflected in the code — but the test's authority is
  the enumeration, not the table.
- [ ] 10.2 Bump `DESCRIPTION` to the next patch version with one
  consolidated `NEWS.md` **Breaking changes** entry: the complete
  old→new table, the `data.goldfish` split, the stub behavior, the
  CLAUDE.md policy carve-out, and the re-fit requirement (design D13).
- [ ] 10.3 Final verification: **not-cran-test** on the whole suite;
  frozen 1e-6 and C++ goldens PASS, not SKIP.
- [ ] 10.4 Answer [stocnet/autograph#60] with the final table, noting:
  goldfish adopted autograph@develop's seven names verbatim, the two
  classes the issue missed (`goldfishParamTest`, `goldfishEval`), the
  summary class (`goldfishSummFit` — goldfish does **not** follow the
  base-R/RSiena `summary.<class>` idiom; no live class carries a dot), and
  that autograph's defunct aliases are now deletable.
