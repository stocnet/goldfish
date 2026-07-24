# Tasks — algorithm-naming

Disciplines (openspec/config.yaml): one focused conventional commit per task,
tests green at every commit, `devtools::document()` inline whenever roxygen /
exports / signatures change, `NOT_CRAN=true` baseline PASS before each commit,
r-lib skills (r-package-development, testing-r-packages, cli, lifecycle)
invoked before the work they cover. This change implements BEFORE
revise-gather-output and residuals-gof phase 2.

## 1. Constructors and classes

- [ ] 1.1 Rename `set_estimation_opt()` → `set_algorithm_newton()` in
      `R/set_opt.R`: new class vector
      `c("algorithm_newton.goldfish", "algorithm.goldfish", "list")`, roxygen
      moved (title, `@return` class, examples), `print` method migrated to the
      new class; `devtools::document()`.
- [ ] 1.2 Rename `set_preprocessing_opt()` → `set_preprocessing()`: class
      `c("preprocessing.goldfish", "list")`, roxygen + `print` method
      migrated; `devtools::document()`.
- [ ] 1.3 Add soft-deprecated alias wrappers `set_estimation_opt()` /
      `set_preprocessing_opt()` (`deprecate_soft(when = "2.0.0")`, forwarding
      all arguments; emitted from the wrapper frame for caller attribution);
      tests: alias returns identical object + `expect_deprecated`, class
      hierarchy asserted.
- [ ] 1.4 Re-point every live deprecation and cli message off the old
      constructor names: `return_interval_loglik` / `return_probabilities` /
      `return_event_scores` (`with = "set_algorithm_newton(diagnostics)"`),
      `convergence_criterion`, `fixed_parameters`, `opportunities_list`
      detail text, and any cli abort/roxygen prose naming `set_estimation_opt`
      / `set_preprocessing_opt` (grep gate: no non-alias, non-NEWS reference
      to the old names remains); snapshot tests for the re-pointed messages.

## 2. Estimator argument surface

- [ ] 2.1 `estimate_dynam()` / `estimate_dynami()` / `estimate_rem()`:
      `control_algo` / `control_prep` / `preprocessed` become the real
      formals (defaults `set_algorithm_newton()` / `set_preprocessing()` /
      `NULL`); `control_estimation` / `control_preprocessing` /
      `preprocessing_init` stay as `deprecated()` sentinels folded in with a
      soft warning (`preprocessing_only` is NOT deprecated here — its
      retirement lands in revise-gather-output with its replacement);
      internal plumbing (`estimate_wrapper()`,
      `estimate_from_specification()`, flavored path) carries the new names;
      `inherits()` gates on `algorithm.goldfish` / `preprocessing.goldfish`;
      `devtools::document()` (canonical `@param` pages updated once,
      inherited elsewhere).
- [ ] 2.2 Migrate all internal callers and the test suite to the new
      argument names (grep gate: no internal use of the deprecated names);
      sentinel tests: old name forwards + warns, new name wins when both
      supplied; stale-format rejection via `preprocessed =`.

## 3. diagnose_* family

- [ ] 3.1 Rename `examine_outliers()` → `diagnose_outliers()` (with
      `parameter` → `threshold` sentinel) and `examine_changepoints()` →
      `diagnose_changepoints()` in `R/class_diagnostics.R`; soft-deprecated
      `examine_*` aliases; re-point the defunct `examineOutliers()` /
      `examineChangepoints()` shims directly at `diagnose_*()`;
      `devtools::document()`; identity + deprecation tests.

## 4. Docs, NEWS, and milestone

- [ ] 4.1 Sweep vignettes, README, and `_pkgdown.yml` references to the
      renamed surfaces; regenerate affected man pages; confirm no
      `\arguments` inheritance broke.
- [ ] 4.2 NEWS.md 2.0.0 naming-migration section with the complete old→new
      table (including the coming `compute_statistics(output =
      "preprocessed")` route for `preprocessing_only`, flagged as landing in
      the next change) and the ≥3.0.0 removal horizon (alias layer + 1.7.0
      defunct layer); DESCRIPTION version bump; full `NOT_CRAN=true` suite
      with baselines PASS.

## 5. Cross-change artifact alignment (no code)

- [ ] 5.1 residuals-gof artifacts: `keep_preprocessed` →
      `return_preprocessed`, `evaluate_engine()` → `evaluate_model()`,
      `examine_onset` → `diagnose_onset()`, `examine_*` → `diagnose_*`,
      `set_estimation_opt` → `set_algorithm_newton`, `preprocessing_init` →
      `preprocessed` across design.md (D3/D12/D13 + storage decision), the
      five spec files, and tasks.md (the replay-supply guiding-error wording
      is owned by revise-gather-output task 4.3 — do not re-point it here);
      log the alignment in its progress.md.
- [ ] 5.2 abmcem + dynes-augmentation artifacts — sweep ALL old names:
      `set_alg_em()` → `set_algorithm_em()` (superclass noted), nested
      `set_alg_*` flagged for component naming (decided there),
      `estimate_dynes(algorithm =)` → `control_algo =`, abmcem's
      `set_estimation_opt` references (9, incl. its optimizer-selection
      warm-start delta) → `set_algorithm_newton`, abmcem's
      `preprocessing_init` → `preprocessed`, dynes-augmentation's
      `evaluate_engine` → `evaluate_model`; gof-dynes `retain_pool`
      references checked against the new constructor name.
- [ ] 5.3 make-multivariate-spec artifacts: `evaluate_engine` →
      `evaluate_model` (3 references); `openspec validate` green for every
      change touched in 5.1–5.3.
