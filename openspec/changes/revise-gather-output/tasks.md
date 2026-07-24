# Tasks — revise-gather-output

Every implementation task: invoke `r-lib:r-package-development` before
package-machinery work, `r-lib:testing-r-packages` before writing tests,
`r-lib:cli` before any console output, `r-lib:lifecycle` for the
deprecations; `not-cran-test` before each commit (baselines PASS, not
SKIP); `devtools::document()` inline whenever roxygen changes. No `src/`
edits are expected; if any C++ is touched, `cpp-recompile` applies.

## 1. Consolidation core (D1–D3)

- [ ] 1.1 `compute_statistics()` — HEAD START LANDED 1.9.12: the function
      exists with the final signature (`x`-first, selectors-before-data,
      `output = c("preprocessed", "gather", "db")`, `control_prep =
      set_preprocessing()`) and `compute_stats()` is already deleted (no
      stub; NEWS records the rename). Remaining here: add the
      `"data.frame"` output value; absorb `max_length =` as an explicit
      formal; make `gather_model_data()` a lifecycle soft-deprecated
      wrapper with a direct-replacement warning (no two-hop chains).
      Tests: all four outputs on a small fixture; gather_model_data
      deprecation-warning snapshot + value identity; expect
      `"compute_stats" %in% getNamespaceExports("goldfish")` is FALSE and
      no `compute_stats` object remains in the package.
- [ ] 1.2 Delegated validation: drop all local `match.arg` on
      model/sub_model in the new function; upgrade `check_model_par()`
      (R/class_checks.R) to `cli_abort` listing the allowed sub_models per
      model; update the REM `"choice"` deprecation message in
      `estimate_wrapper()` to name both successors. Tests: rate_ordered
      flows through for REM/DyNAM, invalid-combo cli error snapshot,
      REM-choice warning snapshot; estimator behavior regression run
      (their front-door `match.arg` untouched).
- [ ] 1.3 `has_intercept`/`right_censored` attached in
      `finalize_gather_output()` so every output form and entry point
      reports them; documentation of the `sub_model = NULL` defaulting and
      its intercept consequence; name-based-indexing note. Tests:
      exact-time REM (flags TRUE, intercept column, censored rows carry
      timespan), ordinal (flags FALSE), flags present on the
      `"preprocessed"` replay object as well.
- [ ] 1.4 Soft-deprecate the estimators' `preprocessing_only` (warning
      names `compute_statistics(output = "preprocessed")`; `TRUE` still
      returns the preprocessed object). Tests: deprecation snapshot;
      parity — `compute_statistics(output = "preprocessed")` identical to
      the legacy route's object (formula and specification inputs).

## 2. Flavored and DyNAMi coverage (D4, D5)

- [ ] 2.1 Flavored specifications: every `output` form returns the
      fid-indexed list with the `process_map` attribute (reusing the
      flavored preprocessing driver's mapping); labels rendered from the
      map in any message/print. Tests: two-flavor fixture (Fisheries
      creation/dissolution) — fid keying matches the estimation
      container's mapping for the same specification; per-fid gather
      stacks match single-flavor runs with the derived constraint.
- [ ] 2.2 DyNAMi verification-then-routing: determine whether the isolated
      DyNAMi front-end can route the gather writer (or post-convert its
      legacy output); implement the supported forms; unavailable forms
      abort with a cli error naming the supported ones. Tests: DyNAMi
      fixture through `output = "gather"` (or the documented error), never
      a silent wrong/empty result. Record the routing findings in
      progress.md.

## 3. Frame output and recipes (D6)

- [ ] 3.1 `output = "data.frame"`: frame assembly from the gather stack
      with the spec'd column contract (`event`, `chosen`, `sender`,
      `receiver`, `index_i`, `index_j`, `timespan`, `is_dependent`, then
      `namesEffects` columns; `effect_description` attribute); per-fid
      list under flavoring. Tests: frame equals the stack row-for-row for
      both flavors; one `chosen` per dependent event; right-censored rows
      `is_dependent = FALSE`, `chosen = 0`.
- [ ] 3.2 Estimator-frame parity test: `estimate_dynam(sub_model =
      "choice")` vs `survival::clogit(chosen ~ <stats> + strata(event))`
      on the frame, small fixture, ≤1e-4 (document the observed
      tolerance); `skip_if_not_installed("survival")`.

## 4. Documentation and closure (D6–D8)

- [ ] 4.1 Roxygen: `compute_statistics()` canonical page (output
      vocabulary, frame contract, row-count formula, db pointer,
      flavored keying, DyNAMi coverage note) with the verified recipes
      under `@examplesIf requireNamespace(...)` and fits in `\donttest`
      (coxph `Surv(rep(1, n), chosen)` + strata; clogit; Poisson glm with
      `offset(log(timespan))`; mlogit via `dfidx` with
      `option = index_j`); deprecated pages point forward;
      `devtools::document()`; man render check.
- [ ] 4.2 Message-pointer sweep: `R/preprocess_writers.R` (and any other
      user-facing text) points to `compute_statistics(output = "db")` /
      `compute_statistics()`; grep for remaining `compute_stats(` /
      `gather_model_data(` references in messages and docs.
- [ ] 4.3 Cross-change coherence (D8): residuals-gof diagnostic-primitives
      guiding-error wording names `compute_statistics(output =
      "preprocessed")` as the replay-supply route (edit its delta before
      residuals-gof task 1.8 implements the message); sweep the other
      active changes for this change's retired names — effect-term-registry
      (3× `compute_stats` → `compute_statistics`) and
      gather-rem-coordination-format (13× `gather_model_data` → the
      canonical `compute_statistics(output = "gather")` surface);
      `openspec validate` green for the touched changes.
- [ ] 4.4 R CMD check pass deciding Suggests (survival/mlogit only if the
      guarded examples require it); NEWS (consolidation;
      `gather_model_data()` soft-deprecated; `compute_stats()` deleted —
      dev-line-only name, rename recorded so old scripts can grep; new
      outputs; reported fields); DESCRIPTION bump; full `NOT_CRAN=true`
      suite green.
- [ ] 4.5 Consumer simplification: `.plan/residuals_comparison.qmd`
      identity route switches to `compute_statistics(output =
      "data.frame")`; re-render confirms the ≤1e-4 parity table is
      unchanged.
