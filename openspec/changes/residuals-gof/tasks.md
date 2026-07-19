# Tasks — residuals-gof

Every implementation task: invoke `r-lib:r-package-development` before
package-machinery work, `r-lib:testing-r-packages` before writing tests,
`r-lib:cli` before any console output, `r-lib:lifecycle` for the
deprecations; `cpp-recompile` after any `src/` edit; `not-cran-test`
before each commit (baselines PASS, not SKIP); `devtools::document()`
inline whenever roxygen/exports change; roxygen inheritance
(`@inheritParams`/`@inherit`) instead of duplicated docs. autograph work
happens in `/Users/ualvaro/Documents/repos/autograph` on branch
`feature/goldfish-diag` (create from `develop`).

## 1. Diagnostic primitives (phase 1)

- [ ] 1.1 `set_estimation_opt(diagnostics =)`: vocabulary validation
      (`loglik`/`scores`/`ranks`/`margins`/`probabilities`, TRUE/"all"/
      FALSE), default `c("loglik","scores")`, cli errors for unknown
      names; unit tests.
- [ ] 1.2 Lifecycle soft-deprecation of `return_interval_loglik`,
      `return_probabilities`, `return_event_scores` with one-to-one
      mapping and conflict error; tests for warning text and mapping.
- [ ] 1.3 Probabilities guardrail: pre-run cli warning with estimated
      size (`n_events × |riskset| × 8` bytes, human-readable), once per
      call; snapshot test.
- [ ] 1.4 C++ in-pass `observed_rank` accumulation behind a flag in
      `DyNAM_choice_default.cpp`, `DyNAM_rate_default.cpp`,
      `DyNAM_rate_ordered_default.cpp`, `DyNAM_MM_default.cpp`,
      `REM_default.cpp`, `REM_ordered_default.cpp`; wire through
      `cpp_interface.R`; cpp-recompile; correctness test vs R-enumerated
      probabilities on a small fixture; NOT_CRAN baselines green.
- [ ] 1.5 C++ in-pass margins accumulation (receiver/actor expected +
      observed counts; sender margins for rate with exact-time exposure);
      wire-through; consistency tests (expected counts sum to n_events).
- [ ] 1.6 `total_rate` under the `"loglik"` primitive for exact-time
      engines (per-event sum of fitted rates over the realized risk set;
      O(n) vector, on by default); consistency test: `total_rate × Δt`
      equals the Cox–Snell residuals; large-dataset cli storage note per
      spec.
- [ ] 1.7 Route `diagnostics` primitives through estimation to the result
      object (all engines); parity test `default` vs `default_c` for
      ranks/margins on a fixture.
- [ ] 1.8 `estimate_*(keep_preprocessed =)`: attach `preprocessed.goldfish`
      to the fit with a cli size message; consumer-side `preprocessed =`
      precedence helper + guiding cli error (both routes named); tests.
- [ ] 1.9 Milestone: DESCRIPTION bump + NEWS entry for the diagnostics
      surface (including the deprecations).

## 2. Evaluator and residual methods (phase 1)

- [ ] 2.1 `evaluate_engine()` generic + methods routing through the
      existing modelTypeCall dispatch: single pass at `at`, `return`
      subsetting, engine defaulting to the fit's engine; tests: loglik/
      score at MLE reproduce the fit (1e-10), evaluation at a constrained
      vector returns full-model score/information.
- [ ] 2.2 `residuals.result.goldfish()`: stored-primitive types
      (deviance, schoenfeld, score, dfbeta/dfbetas/cooks — the last three
      from stored scores + information matrix) with zero recompute;
      canonical roxygen page defining all types **including the explicit
      caveats section** (likelihood-vs-history deletion, onset/null-
      benchmark reading, cold-start zero-score property, cross-reference
      to `examine_onset()`).
- [ ] 2.3 `residuals()` recompute types: scaled_schoenfeld, cox_snell,
      response, martingale (margins) via
      `evaluate_engine()`; submodel-conditional semantics for DyNAM;
      cox_snell restricted to exact-time rate/REM with cli error
      elsewhere; tests incl. schoenfeld column sums ≈ 0 and cox_snell
      KS-vs-Exp(1) on simulated-null fixture.
- [ ] 2.4 `fitted()` (`outcome` from stored loglik; `probabilities` via
      evaluator) and in-sample `predict()` (`probabilities`/`ranks`,
      `events =` subset), documented as non-forecasting; tests: predict
      ranks equal stored `observed_rank`.
- [ ] 2.5 `augment.result.goldfish()` gains `.fitted`/`.resid` broom
      columns (censored rows NA); update `examine_*` internals to reuse
      them; tests.
- [ ] 2.6 Cross-package validation (NOT_CRAN): scaled Schoenfeld vs
      `survival::cox.zph`-consistent reference on a Cox-expressible REM
      fixture; residual comparison vs `remstimate::diagnostics()` on a
      shared small dataset.
- [ ] 2.7 Milestone: DESCRIPTION bump + NEWS entry for residual/fitted/
      predict/augment methods.

## 3. examine_* class alignment (phase 1, goldfish + autograph)

- [ ] 3.1 goldfish: `examine_outliers()` → class
      `c("outliers.goldfish","data.frame")`, `examine_changepoints()` →
      `c("changepoints.goldfish","data.frame")`; logical `outlier`/`cpt`
      columns; cli print methods dispatching on the new classes; NEWS
      breaking-change entry; tests + snapshots.
- [ ] 3.2 Term-wise `examine_*`: `effect =` argument (compact-term-string
      selection, single-series ambiguity error) switching
      `examine_changepoints()` to the term's scaled Schoenfeld series and
      `examine_outliers()` to dfbeta-based influence ranking; selected
      term recorded on the object for plot labeling; post-selection
      caveat documented; tests + snapshots.
- [ ] 3.3 `examine_onset()`: leave-initial-segment-out one-step parameter
      path + information-accrual curve from stored primitives (OPG
      default; exact-information option via the evaluator); plot-ready
      classed object with compact-term-string labels; descriptive cli
      print; cold-start vs warm-started fixture tests (drift-then-
      stabilize vs flat); docs with remedies and the
      why-changepoints-miss-this note.
- [ ] 3.4 autograph (`feature/goldfish-diag` off `develop`): fix
      `plot.outliers.goldfish` to consume logical `outlier` (drop the
      `"YES"` check), verify `plot.changepoints.goldfish` against the new
      objects; refresh the precooked `goldfish_outliers`/
      `goldfish_changepoints` fixtures; autograph tests.

## 4. Diagnostic tests (phase 2)

- [ ] 4.1 `test_gof()` core on `result.goldfish`: standardized cumulative
      score processes from stored `event_scores`, observed-information
      (fallback empirical) standardization, sup statistic, Kolmogorov
      p-value; object stores process paths + per-effect table; bridge
      property test (process ends at 0).
- [ ] 4.2 `test_gof()` on the specification fit: per-block tests,
      per-block and joint Cauchy omnibus; cli print method (grouped by
      block) + snapshot.
- [ ] 4.3 `test_parameter()` score test: constrained fit + candidate
      effects → `evaluate_engine()` at the constrained estimate →
      efficient-score LM with chi-square p-value; equivalence test
      LM = t(Δ) I Δ; omitted-reciprocity power fixture; docs pointing to
      `lmtest::lrtest`/`waldtest` for nested fitted pairs.
- [x] 4.4 ~~`test_parameter()` Wald form~~ — DESCOPED 2026-07-19 (user
      decision): trails post-release; `lmtest::waldtest()` covers nested
      pairs meanwhile. No implementation; the diagnostic-tests spec delta
      records the deferral.
- [ ] 4.5 `test_time(method = "trend")`: scaled Schoenfeld vs time
      transform (`identity`/`rank`/`km`), per-effect zero-slope score
      tests + global test, plot-ready residual data on the object; cli
      print + snapshot; null-uniformity fixture test.
- [ ] 4.6 `test_time(method = "periods")`: period-masked partial sums of
      stored `event_scores` (no preprocessing; first period = reference);
      `periods =` forms (integer equal-event default, cut times, grouping
      vector); `information = c("expected", "opg")` — expected via one
      evaluator pass accumulating period-wise Fisher blocks, OPG from
      stored scores; one-step per-period deltas in the output;
      regime-change detection fixture + expected-vs-OPG agreement test +
      no-replay test.
- [ ] 4.7 Effect selection by compact term strings (`effects =` /
      `effect =`): shared matching helper (exact string, family
      expansion in `test_*`, cli ambiguity error with candidates,
      integer fallback) reusing the compact-string builder; tests incl.
      the inertia-variants fixture.
- [ ] 4.8 Coverage/power simulation suite (NOT_CRAN): test_gof null
      uniformity at n ∈ {1000, 5000} and power vs non-linear reciprocity
      DGP, per the Boschi-Wit §4 design; seeds fixed; runtime documented.
- [ ] 4.9 Milestone: DESCRIPTION bump + NEWS entry for the test_* family.

## 5. autograph plot methods (phase 2)

- [ ] 5.1 `plot` method for the test_gof class: per-effect standardized
      process paths with Brownian-bridge reference bands, faceted by
      block; precooked fixture object; renders without goldfish attached;
      autograph tests.
- [ ] 5.2 `plot` method for the test_time class: scaled Schoenfeld
      scatter + weighted smooth per effect with zero reference; precooked
      fixture; autograph tests.
- [ ] 5.3 autograph one-call overview: `plot` method on the goldfish fit
      (remstimate-parity) composing via patchwork from **stored
      primitives only** — deviance trace with flagged outliers, scaled
      Schoenfeld smooths (top effects), test_gof bridge processes, and
      the waiting-time Q-Q from `total_rate` (panel drops out when the
      fit lacks the primitive or is ordinal); precooked fixture;
      autograph tests.
- [ ] 5.4 autograph plot method for the `examine_onset` class:
      per-coefficient path panel (with stabilization marker) +
      information-accrual panel; precooked fixture; autograph tests.
- [ ] 5.5 autograph NEWS + pkgdown reference entries for the goldfish
      diagnostic family; confirm no goldfish dependency added (dispatch
      on class only, stocnet pattern).

## 6. Documentation and closure

- [ ] 6.1 `vignettes/teaching1.Rmd.orig`: add a model-diagnostics section
      after the estimation walkthrough — deviance residuals and
      surprising events (`residuals()`, `fitted()`), `test_gof()` on the
      fitted DyNAM (per-block + omnibus reading), `test_time()` trend
      plot, with plots rendered via autograph (chunks gated on
      `requireNamespace("autograph")`; autograph added to Suggests);
      rebuild through the precompile workflow.
- [ ] 6.2 `vignettes/teaching2.Rmd.orig`: expand the existing
      `plot-examine` section — the `examine_*` calls gain their new
      classes/plots, plus REM-specific additions: Cox–Snell waiting-time
      Q-Q, scaled Schoenfeld/`test_time()` for the tie model,
      `test_parameter()` score-test example (test an effect without
      re-estimating), `lmtest::lrtest()` usage; same autograph gating and
      precompile rebuild.
- [ ] 6.3 Roxygen inheritance audit: canonical pages
      (`residuals.result.goldfish`, `evaluate_engine`, `test_gof`)
      inherited elsewhere; `devtools::document()`; man pages checked for
      resolved inheritance; full `NOT_CRAN=true` suite green.
- [ ] 6.4 Update `.plan/residuals-gof.md` status header (phases 1-2
      implemented; phase 3 pending DyNES); final DESCRIPTION/NEWS pass.
