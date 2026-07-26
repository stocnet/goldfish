# Residuals and goodness of fit for goldfish

## Why

goldfish estimates DyNAM/REM models but offers almost no model criticism: no
`residuals()`/`fitted()` methods, no goodness-of-fit test, no score test, and
`diagnose_outliers()`/`diagnose_changepoints()` are the only diagnostics.
The statistical design is settled in `.plan/residuals-gof.md` (survey of
relevent/remstimate/mlogit + Boschi & Wit 2024): all residual primitives are
already computed by the C++ engines (`event_scores`, `intervalLogL`), the
conditional (per-submodel) residuals are the exact score residuals of the
factorized DyNAM likelihood, and the Boschi–Wit cumulative-score bridge test
is fully analytic for goldfish's all-FLE models. This change implements
phases 1 and 2 of that design; phase 3 (`simulate()`, simulation-based GOF,
auxiliary statistics) waits for DyNES to land.

## What Changes

- `set_algorithm_newton()` gains a `diagnostics =` character vector naming
  stored *primitives* (`"loglik"`, `"scores"`, `"ranks"`, `"margins"`,
  `"probabilities"`; `TRUE` ≡ `c("loglik", "scores")`, `"all"` shorthand),
  soft-deprecating `return_interval_loglik` and `return_probabilities`
  (lifecycle; both public since CRAN/v1.7.0) and removing
  `return_event_scores` outright (never in a public release — no deprecation
  cycle owed; `backend-parity` design D11 records the audit), with a cli size
  guardrail before storing probabilities.
- New in-pass C++ derived quantities: `observed_rank` + recall (`"ranks"`)
  and per-actor observed-vs-expected margins (`"margins"`; both sender and
  receiver margins on REM; probability-scale everywhere with the
  expected-count compensator variant additionally stored on exact-time fits,
  each labeled — see the diagnostic-primitives spec and backend-parity's
  design appendix for the derivations) for all six engines — never
  materializing the full probability matrix. Margins are documented as calibration
  descriptives; per-event/sequence-level diagnostics remain the default
  surface (margins are opt-in).
- `estimate_*()` gains `return_preprocessed = FALSE` attaching the
  `preprocessed.goldfish` to the fit; diagnostic consumers accept
  `preprocessed =` and abort with a cli error naming both routes when a
  replay is needed but unavailable.
- New single-pass evaluator `evaluate_model()`: log-likelihood, score,
  information, per-event quantities at an arbitrary parameter vector using
  the estimation engine (shared by residuals-on-demand, the score test, and
  later DyNES ascent-based Monte Carlo).
- New S3 methods on fitted objects: `residuals()` (deviance default;
  schoenfeld, scaled_schoenfeld, score, cox_snell, response, martingale,
  dfbeta/dfbetas), `fitted(type = c("outcome", "probabilities"))`,
  in-sample `predict(type = c("probabilities", "ranks"))`, and
  `augment()` gaining `.fitted`/`.resid` columns (broom alignment).
- New test functions (test_* surface only; no `gof()` generic, avoiding the
  `ergm::gof` mask): `test_gof()` (Boschi–Wit bridge: per-effect Kolmogorov
  + per-block and joint Cauchy omnibus, dispatching on the
  `make_specification()`-based fit), `test_parameter()` (score/LM test at a
  constrained fit; the Wald form for multi-parameter combinations is
  deferred post-release, 2026-07-19 decision),
  `test_time()` (`method = c("trend", "periods")`: zph-style scaled-
  Schoenfeld slope test default, and a sienaTimeTest-style period-dummy
  score test computed by masking stored scores — no preprocessing;
  `information = c("expected", "opg")`). Effect-selecting arguments match
  the compact term strings shown in the printed summary; `diagnose_*` gain
  a term-wise `effect =` mode (changepoints on the scaled Schoenfeld
  series, outliers by dfbeta influence). New `diagnose_onset()` cold-start
  diagnostic (leave-initial-segment-out parameter path + information-
  accrual curve from stored primitives — the frequentist counterpart of
  the PSIS-LOO Pareto-k flag on history-less first events); `"cooks"`
  residual type; the canonical residuals page carries an explicit caveats
  section (likelihood- vs history-deletion, onset reading).
- Diagnostic result objects carry all plot-ready data with cli print
  methods; **all plotting lives in autograph** (branch
  `feature/goldfish-diag` off `develop`): new plot methods for the test and
  residual classes, and alignment of `diagnose_outliers()`/
  `diagnose_changepoints()` to autograph's existing `outliers.goldfish`/
  `changepoints.goldfish` methods (**BREAKING**: their return class changes
  from `diagnostic.goldfish`; the `outlier` column contract is fixed on the
  autograph side).
- **Fit-shape compatibility (added 2026-07-21)**: on flavored (per-fid)
  fits every method applies per process result exactly as on a single fit —
  the Fisheries Treaties creation/dissolution example is the reference;
  `test_gof()` blocks are per fid × submodel with the omnibus combining
  across processes. Two-mode fits (multimode mode-map models) are handled:
  margins and any actor-identified plot-data component report per side,
  labels joined via the model's `node_lookup` — settled in the
  `diagnostic-plot-classes` contract BEFORE the autograph branch work
  starts, since that contract is frozen for the parallel track.
- Documentation avoids roxygen duplication via `@inheritParams`/`@inherit`
  from canonical pages (`residuals.result.goldfish`, `evaluate_model`).
- A new long-form diagnostics vignette (`vignettes/diagnostics.Rmd.orig`,
  D14) is the canonical prose home for residual types, the margins
  calibration-descriptive reading, and the test workflow; the teaching
  vignettes keep short diagnostics sections pointing to it.

## Capabilities

### New Capabilities

- `diagnostic-primitives`: which per-event quantities estimation stores
  (`diagnostics =` vocabulary, defaults, memory guardrail, engine parity for
  ranks/margins) and how the preprocessed object is kept/fed
  (`return_preprocessed =` / `preprocessed =`).
- `model-evaluation-pass`: `evaluate_model()` — single no-iteration
  evaluation at arbitrary parameters on the estimation engine, returning
  requested quantities; numerical consistency with the fit.
- `residual-methods`: `residuals()`, `fitted()`, `predict()`, `augment()`
  semantics — formulas per type, stored-vs-recompute behavior, per-submodel
  conditional residuals for DyNAM, coordination handling.
- `diagnostic-tests`: `test_gof()`, `test_parameter()`, `test_time()` —
  statistics, null distributions, dispatch on specification fits, cli
  print output.
- `diagnostic-plot-classes`: the data contract of plot-ready diagnostic
  objects consumed by autograph (`outliers.goldfish`,
  `changepoints.goldfish`, test/residual classes) — columns, classes,
  and printing.

### Modified Capabilities

- `optimizer-selection`: no delta from this change. The scores requirement
  (renamed "Per-event scores primitive") is owned wholesale by
  `backend-parity` (its revised D6); this change's deprecation/removal prose
  for the legacy flags lives in its own `diagnostic-primitives` capability.
- `compact-term-strings`: the shared builder gains the diagnostic
  effect-selection surfaces (`effect =` / `effects =` of the `test_*` and
  `diagnose_*` families) as consumers — the printed term string is the
  selection key.

## Impact

- **goldfish R**: `R/set_opt.R`, `R/model_estimate.R`, `R/cpp_interface.R`,
  `R/estimation_core.R`, `R/class_diagnostics.R`, `R/methods_display.R`
  (augment), new files for residuals/tests/evaluator; NAMESPACE exports;
  lifecycle deprecations.
- **goldfish C++**: `src/DyNAM_choice_default.cpp`, `src/DyNAM_rate_default.cpp`,
  `src/DyNAM_rate_ordered_default.cpp`, `src/DyNAM_MM_default.cpp`,
  `src/REM_default.cpp`, `src/REM_ordered_default.cpp` — in-pass ranks and
  margins (cpp-recompile discipline applies).
- **autograph** (separate repo, branch `feature/goldfish-diag` off
  `develop`): plot methods for new classes; fix `outlier` column contract
  in `plot.outliers.goldfish`; no goldfish dependency added (dispatch on
  class only), consistent with its existing RSiena/ergm/MoNAn pattern under
  the stocnet umbrella.
- **Dependencies**: no new hard dependencies (cli, lifecycle, tibble
  already in Imports; stats only). autograph is Suggests-level at most.
- **Out of scope (phase 3, after DyNES)**: `simulate()`, simulation-based
  GOF, auxiliary-statistic multiplier bootstrap, forecasting `predict()`.
