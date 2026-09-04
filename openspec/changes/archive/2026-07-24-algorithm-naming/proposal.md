# Tidyverse-style naming for the estimation control surface

## Why

The stocnet workshop set family-wide naming conventions (verb_object
functions, `set_algorithm_*()` control constructors, boolean-prefix
arguments) and RSiena 1.6 already shipped its side of the migration
(`set_algorithm_saom()`, `test_gof()`, `test_parameter()`). goldfish's
estimation-control surface predates the guidelines
(`set_estimation_opt()`, `control_estimation =`, `preprocessing_init =` /
`preprocessing_only =`), and two in-flight tracks are about to mint new
exports on top of the old vocabulary: residuals-gof (`keep_preprocessed`,
`evaluate_engine()`, `examine_onset()`) and DyNES (`set_alg_em()`, flagged
provisional pending exactly this decision). The 2.0.0 release (CRAN
~mid-Aug) is the one-time window to rename with a single deprecation
cycle. Decisions were settled 2026-07-24 and recorded in
`.plan/Naming_guidelines.md` (local).

## What Changes

- `set_estimation_opt()` → `set_algorithm_newton()`; return class becomes
  `c("algorithm_newton.goldfish", "algorithm.goldfish", "list")`. The
  suffix names the algorithm family (direct Newton-type maximization) to
  contrast with the upcoming DyNES `set_algorithm_em()`; the shared
  `algorithm.goldfish` superclass is the single dispatch/validation hook.
- `set_preprocessing_opt()` → `set_preprocessing()`; return class becomes
  `c("preprocessing.goldfish", "list")`.
- `estimate_dynam()` / `estimate_dynami()` / `estimate_rem()` argument
  renames: `control_estimation` → `control_algo`, `control_preprocessing`
  → `control_prep`, `preprocessing_init` → `preprocessed`. This change
  implements FIRST (2026-07-24 alignment session), so
  `compute_statistics()` (revise-gather-output) is born fully final —
  `compute_statistics(x, model, sub_model, data, output =
  c("preprocessed", ...), control_prep = set_preprocessing())` — with no
  code task here; `gather_model_data()` is deprecated wholesale there.
- `preprocessing_only` is NOT touched here: its replacement is
  `compute_statistics(output = "preprocessed")`, born in
  revise-gather-output (implemented after this change), which owns that
  soft-deprecation — deprecating it here would point users at a function
  that does not exist yet. (The earlier `make_preprocessed()` sketch was
  dropped 2026-07-24 to avoid a duplicate replay route.)
- `examine_outliers()` / `examine_changepoints()` →
  `diagnose_outliers()` / `diagnose_changepoints()` (workshop
  `diagnose_[mode]()` verb), with soft-deprecated aliases.
- Deprecation layer: every old name stays exported as a thin wrapper with
  `lifecycle::deprecate_soft(when = "2.0.0")`; existing deprecation
  messages that point at renamed surfaces (`return_interval_loglik` /
  `return_probabilities` / `return_event_scores` /
  `convergence_criterion` / `fixed_parameters` /
  `set_estimation_opt(diagnostics)` cli references) are re-pointed to the
  final names — no two-hop chains. The 1.7.0 camelCase defunct layer is
  scheduled (not executed) for removal at 3.0.0.
- Cross-change artifact alignment (spec-only names born final, no
  deprecation): residuals-gof (`keep_preprocessed` →
  `return_preprocessed`, `evaluate_engine()` → `evaluate_model()`,
  `examine_onset` → `diagnose_onset()`, `examine_*` references →
  `diagnose_*`); abmcem + dynes-augmentation (`set_alg_em()` →
  `set_algorithm_em()`, nested `set_alg_*` marked for component naming,
  `estimate_dynes(algorithm =)` → `control_algo =`).

## Capabilities

### New Capabilities

- `preprocessing-controls`: `set_preprocessing()` constructor (name,
  arguments, `preprocessing.goldfish` class), the `control_prep =`
  argument contract on estimators (and `compute_statistics()` when it
  lands), and the unified `preprocessed =` supply argument replacing
  `preprocessing_init =`.
- `naming-deprecations`: the lifecycle alias layer — one-to-one old→new
  mapping, `deprecate_soft` at 2.0.0, no two-hop deprecation messages,
  removal horizon ≥3.0.0, and the `diagnose_outliers()` /
  `diagnose_changepoints()` renames (the `examine_*` functions have no
  owning living spec).

### Modified Capabilities

- `optimizer-selection`: the constructor requirement moves from
  `set_estimation_opt()` to `set_algorithm_newton()` with the
  `algorithm.goldfish` superclass; the `control_algo =` argument name on
  estimators; `diagnostics` vocabulary itself unchanged.
- `offset-fixed-terms`: wording-only — scenarios reference
  `set_estimation_opt()`; renamed to `set_algorithm_newton()`.
- `active-availability-stat`: wording-only — scenarios reference
  `preprocessing_init`; renamed to `preprocessed`.
- `flat-preprocess-output`: wording-only — one `preprocessing_init`
  reference; renamed to `preprocessed`.

## Impact

- **R**: `R/set_opt.R` (constructors, deprecation re-pointing),
  `R/model_estimate.R` (estimator signatures), `R/class_diagnostics.R`
  (`diagnose_*` renames), `R/goldfish-defunct.R` untouched (schedule note
  only). No C++ changes.
- **Docs/infra**: NAMESPACE exports, `man/` regeneration
  (`devtools::document()` inline per task), roxygen `@inheritParams`
  canonical pages move with the renames, vignette and README mentions,
  NEWS.md 2.0.0 migration section, DESCRIPTION bump at milestones.
- **Tests**: existing suites calling old names keep passing through the
  alias layer (deprecation warnings expected under testthat 3e's
  `deprecate_soft` handling); new tests for aliases, argument sentinels,
  and class hierarchy; frozen coefficient
  baselines (`NOT_CRAN=true`) must stay PASS — renames must not alter
  numerical paths.
- **Other active changes**: residuals-gof artifacts (design D3/D12/D13 +
  4 spec files), abmcem and dynes-augmentation designs/specs — edited as
  tasks here so residuals-gof phase 2 is born under final names.
- **Downstream**: autograph dispatches on result classes only
  (`outliers.goldfish` etc.) — result-class names are NOT renamed here,
  so no autograph impact.
