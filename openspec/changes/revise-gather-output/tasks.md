# Tasks — revise-gather-output

Every implementation task: invoke `r-lib:r-package-development` before
package-machinery work, `r-lib:testing-r-packages` before writing tests,
`r-lib:cli` before any console output, `r-lib:lifecycle` for the
deprecations; `not-cran-test` before each commit (baselines PASS, not
SKIP); `devtools::document()` inline whenever roxygen changes. No `src/`
edits are expected; if any C++ is touched, `cpp-recompile` applies.

## 0. What `backend-parity` changed under this change (added 2026-07-26)

`backend-parity` archived on 2026-07-26. It did not change this change's scope,
but it changed the ground two of its tasks stand on. Read before section 1.

- [x] 0.1 **The gather backend now has a frozen 1e-6 coefficient floor, and
      this change's rewrite is gated by it.** Until 2026-07-26 no gather
      coefficient was frozen (`baselines_backends` was `c("r", "cpp")` — the
      absence `backend-parity` D4 deliberately exploited). Its D15 closed that:
      `coefficient_baselines_v2.rds` adds 12 gather cells across the full model
      grid, so the suite went from 24 to 36 baseline cells. A storage rewrite
      that perturbs gather numerics now **fails** where it would previously
      have passed on tolerance tests alone. Re-read task 1.x's "estimation
      coefficients unchanged (baselines)" with that in mind: the statement is
      unchanged, its teeth are new. v2's r/cpp columns are v1's numbers carried
      forward bit-identically — do not regenerate them to make a gather change
      pass; use the `regen-baselines` skill and its recorded justification.
- [x] 0.2 **`index_i` / `index_j` are now load-bearing for a primitive, not
      only for margins.** `backend-parity` threaded them into the gather kernel
      dispatch (its task 3.1) and then built per-event `probabilities` on them:
      the gather rows are the realized risk set and carry no actor identity, so
      `scatter_event_probabilities()` in `event_reductions.h` uses those columns
      to place each probability at its actor index. A storage-format change that
      drops, reorders, lazily reconstructs or renames them silently breaks a
      primitive whose parity test has no obvious connection to the writer.
      Treat them as part of the format contract, and re-run
      `test-backend_parity.R` — not just the writer tests — after any change to
      how the gather stack is produced or read back.
- [x] 0.3 **Vocabulary: apply this change's own classification rule, do not
      sweep.** Task 4.3's rule stands — in scope when a reference names what a
      *user passes* (`engine = "default_c"`, "on the `gather_compute` engine");
      out of scope when it names the *implementation* (the `*_default.cpp`
      kernels and `estimate_default_c()`, whose real names `backend-parity`
      explicitly keeps as a Non-Goal) or is generic prose ("cross-engine
      tolerance"). One stale artifact to ignore rather than resurrect:
      `progress.md` records an `optimizer-selection` delta asserting
      `backend = "gather"` SHALL abort on scores. That delta is no longer part
      of this change, and the claim is now false — gather produces per-event
      scores, and the abort plus the silent default-sourced drop were both
      removed by `backend-parity` task 5.2, replaced by one capability table.

## 1. Consolidation core (D1–D3)

- [x] 1.1 (the `"data.frame"` output value lands with its assembly in 3.1 —
      adding the vocabulary here would ship an `output` choice that errors)
      `compute_statistics()` — HEAD START LANDED 1.9.12: the function
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
- [x] 1.2 Delegated validation: drop all local `match.arg` on
      model/sub_model in the new function; upgrade `check_model_par()`
      (R/class_checks.R) to `cli_abort` listing the allowed sub_models per
      model; update the REM `"choice"` deprecation message in
      `estimate_wrapper()` to name both successors. Tests: rate_ordered
      flows through for REM/DyNAM, invalid-combo cli error snapshot,
      REM-choice warning snapshot; estimator behavior regression run
      (their front-door `match.arg` untouched).
- [x] 1.3 `has_intercept`/`right_censored` attached in
      `finalize_gather_output()` so every output form and entry point
      reports them; documentation of the `sub_model = NULL` defaulting and
      its intercept consequence; name-based-indexing note. Tests:
      exact-time REM (flags TRUE, intercept column, censored rows carry
      timespan), ordinal (flags FALSE), flags present on the
      `"preprocessed"` replay object as well.
- [x] 1.4 Soft-deprecate the estimators' `preprocessing_only` (warning
      names `compute_statistics(output = "preprocessed")`; `TRUE` still
      returns the preprocessed object). Tests: deprecation snapshot;
      parity — `compute_statistics(output = "preprocessed")` identical to
      the legacy route's object (formula and specification inputs).

## 2. Writer ordering, flavored and DyNAMi coverage (D9, D4, D5)

- [x] 2.1 Writer render stage + encoding forward (D9), the prerequisite for
      2.2 — constrained gather/db output is broken today (pre-existing on
      HEAD, untested). (a) Split the writer contract: `finalize()` returns the
      assembled preprocessed shape, a new `render(out, spec)` produces the
      product (identity / `gather_from_prep()` / inherited by the db writer);
      `finalize_consumers()` calls `finish_output()` (mask realized + folded,
      per consumer) BEFORE `render()`. (b) `gather_from_prep()` forwards
      `active_dyad_encoding` to `gather_()` as `estimate_c_int()` already does.
      (c) `build_consumer_specs()` takes the requested output's writer factory
      instead of always `writer_default`. Tests: constrained single-flavor
      gather emits only allowed candidates (the writers-capability scenario
      that has never had a test); unconstrained gather still byte-identical to
      `gather_model_data()`; a point-encoded stored object renders correctly;
      estimation coefficients unchanged (baselines).
- [x] 2.2 Flavored specifications: every `output` form returns the
      fid-indexed list with the `process_map` attribute (reusing the
      flavored preprocessing driver's mapping); labels rendered from the
      map in any message/print. Tests: two-flavor fixture (Fisheries
      creation/dissolution) — fid keying matches the estimation
      container's mapping for the same specification; per-fid gather
      stacks match single-flavor runs with the derived constraint (every
      flavored gather is a constrained gather, so this rests on 2.1).
- [x] 2.3 Self-describing db export (D10). (a) Statistic columns named by
      effect (`names_effects`) instead of `stat_<i>`, at single and flavored
      output alike, with the reserved identity names (`event_id`,
      `is_selected`, `index_i`, `index_j`) in the uniqueness pass. (b) Write
      `<db_table>_nodes` (side, local, global, label), once per export.
      (c) One table per process at `<db_table>_<fid>` plus `<db_table>_map`
      (process_map columns + each fid's table name) — the SAME shape whether or
      not the specification is flavored, so a single process writes
      `<db_table>_1` and a one-row map (synthesized from the model: layer,
      family, has_intercept; `flavor` NA). This renames the single-process
      table, which is free: `output = "db"` landed in 1.8.0 and the last CRAN
      release is 1.6.12. The returned descriptor names the tables written; the
      R return stays asymmetric (descriptor vs fid-keyed list) by D10.
      (d) A mid-export failure names the fid alongside the last written event
      index; orphan tables from a previous larger run are left alone, the map
      being the authority. Tests (RSQLite, `skip_on_cran()`): column names
      equal the gather `names_effects`; a constrained export writes only allowed
      candidates (rests on 2.1); flavored writes
      `stats_1`/`stats_2`/`stats_map`/`stats_nodes` with the map's fid keying
      matching the estimation container's; a single-process export writes the
      same four-table shape with a one-row map; index columns join through
      `_nodes` to the original labels on a two-mode fixture. Update the
      existing db tests, which read the unsuffixed table.
- [x] 2.4 DyNAMi verification-then-routing: determine whether the isolated
      DyNAMi front-end can route the gather writer (or post-convert its
      legacy output); implement the supported forms; unavailable forms
      abort with a cli error naming the supported ones. Tests: DyNAMi
      fixture through `output = "gather"` (or the documented error), never
      a silent wrong/empty result. Record the routing findings in
      progress.md.

## 3. Frame output and recipes (D6)

- [x] 3.1 `output = "data.frame"`: frame assembly from the gather stack
      with the spec'd column contract (`event`, `chosen`, `sender`,
      `receiver`, `index_i`, `index_j`, `timespan`, `is_dependent`, then
      `names_effects` columns; `effect_description` attribute); per-fid
      list under flavoring. Tests: frame equals the stack row-for-row for
      both flavors; one `chosen` per dependent event; right-censored rows
      `is_dependent = FALSE`, `chosen = 0`.
- [x] 3.2 Estimator-frame parity test: `estimate_dynam(sub_model =
      "choice")` vs `survival::clogit(chosen ~ <stats> + strata(event))`
      on the frame, small fixture, ≤1e-4 (document the observed
      tolerance); `skip_if_not_installed("survival")`.

## 4. Documentation and closure (D6–D8)

**Runs after the `backend-vocabulary` change lands.** 4.1's help page and
4.4's NEWS/DESCRIPTION/full-suite milestone have to describe the surface as it
ships, and that change renames `engine` to `backend` on
`set_algorithm_newton()` — an argument this change's examples and docs use.

- [x] 4.1 Roxygen: `compute_statistics()` canonical page (output
      vocabulary, frame contract, row-count formula, db pointer,
      flavored keying, DyNAMi coverage note) with the verified recipes
      under `@examplesIf requireNamespace(...)` and fits in `\donttest`
      (coxph `Surv(rep(1, n), chosen)` + strata; clogit; Poisson glm with
      `offset(log(timespan))`; mlogit via `dfidx` with
      `option = index_j`); deprecated pages point forward;
      `devtools::document()`; man render check.
- [x] 4.2 Message-pointer sweep: `R/preprocess_writers.R` (and any other
      user-facing text) points to `compute_statistics(output = "db")` /
      `compute_statistics()`; grep for remaining `compute_stats(` /
      `gather_model_data(` references in messages and docs.
- [x] 4.3 Cross-change coherence (D8): residuals-gof diagnostic-primitives
      guiding-error wording names `compute_statistics(output =
      "preprocessed")` as the replay-supply route (edit its delta before
      residuals-gof task 1.8 implements the message); sweep the other
      active changes for this change's retired names — effect-term-registry
      (3× `compute_stats` → `compute_statistics`) and
      gather-rem-coordination-format (13× `gather_model_data` → the
      canonical `compute_statistics(output = "gather")` surface). The
      legacy engine-value sweep is NOT here — `backend-vocabulary` task 3.3
      owns it, including the user-facing-vs-implementation rule.
      `openspec validate` green for the touched changes.
- [x] 4.4 R CMD check pass deciding Suggests (survival/mlogit only if the
      guarded examples require it); NEWS (consolidation;
      `gather_model_data()` soft-deprecated; `compute_stats()` deleted —
      dev-line-only name, rename recorded so old scripts can grep; new
      outputs; reported fields); DESCRIPTION bump; full `NOT_CRAN=true`
      suite green.
- [x] 4.5 Consumer simplification: `.plan/residuals_comparison.qmd`
      identity route switches to `compute_statistics(output =
      "data.frame")`; re-render confirms the ≤1e-4 parity table is
      unchanged.
