## Why

S3 dispatch has one flat, global namespace, so a class name is a claim on that
name for every package a user has attached. goldfish returns eight objects whose
classes are unqualified generic nouns — `test_gof`, `test_time`,
`test_parameter`, `diagnose_onset`, `diagnose_outliers`,
`diagnose_changepoints`, `margin_table`, `evaluate_model` — and
[stocnet/autograph#60](https://github.com/stocnet/autograph/issues/60) reports
the consequence: autograph 1.2.0 registers `plot` methods on those names, the
method bodies assume goldfish's columns and attributes, and **autograph has no
way to tell whose object it received**, because nothing in the class vector
identifies the package.

Where goldfish does qualify a class it uses a dot (`result.goldfish`,
`network.goldfish`) — seventeen such classes exist — which conflicts with the
package's strict snake_case policy and creates the `plot.test_gof.goldfish`
ambiguity the issue itself flags. So the package currently runs two conventions
and neither one is the one we want.

This line has never gone to CRAN, so there is no installed base to deprecate
around, and ADR-0016 already licenses renaming a user-facing name outright until
2.0.0. That window closes at the release; this is the cheapest this will ever be.

## What Changes

- **BREAKING** — Every class goldfish returns to a user is renamed to carry a
  snake_case `_goldfish` suffix appended to the constructor's own name
  (ADR-0020). No fallback class, no deprecation shim, per ADR-0016.
  - Diagnostics: `test_gof` → `test_gof_goldfish`, `test_time` →
    `test_time_goldfish`, `test_parameter` → `test_parameter_goldfish`,
    `diagnose_onset` → `diagnose_onset_goldfish`, `diagnose_outliers` →
    `diagnose_outliers_goldfish`, `diagnose_changepoints` →
    `diagnose_changepoints_goldfish`, `margin_table` → `margin_table_goldfish`,
    `evaluate_model` → `evaluate_model_goldfish`.
  - Results: `result.goldfish` → `result_goldfish`, `flavored_result.goldfish` →
    `flavored_result_goldfish`, `summary.result.goldfish` →
    `summary_result_goldfish`.
  - Preprocessing: `preprocessed.goldfish` → `preprocessed_goldfish`,
    `preprocessed_db.goldfish` → `preprocessed_db_goldfish`,
    `preprocessing.goldfish` → `preprocessing_goldfish`,
    `flavored_preprocessed.goldfish` → `flavored_preprocessed_goldfish`,
    `flavored_statistics.goldfish` → `flavored_statistics_goldfish`.
  - Specification and algorithm: `specification.goldfish` →
    `specification_goldfish`, `spec_map.goldfish` → `spec_map_goldfish`,
    `algorithm.goldfish` → `algorithm_goldfish`, `algorithm_newton.goldfish` →
    `algorithm_newton_goldfish`.
  - Internal: `goldfish.formulae` → `formulae_goldfish` (single site; the
    remaining internal dispatch classes are out of scope).
- **BREAKING** — `data.goldfish` splits, because one name currently serves two
  different objects. The `as_goldfish()` stamp on a `stocnet` becomes
  `data_goldfish`; the legacy `make_data()`/DyNAMi environment keeps
  `data.goldfish` as a deprecated-path name.
- **BREAKING** — The retired `result.goldfish` name becomes the staleness
  discriminator. `print.result.goldfish` and `summary.result.goldfish` survive
  only as stubs directing the user to re-fit; no other generic registers on the
  old name, so `coef()` and friends give R's own "no applicable method" error.
- The four deprecated data classes — `nodes.goldfish`, `network.goldfish`,
  `dependent.goldfish`, `global.goldfish` — are explicitly **not** renamed. Their
  constructors already `deprecate_warn()` toward `manynet::make_stocnet()`;
  renaming a name on its way out is churn.
- A package-wide class-naming rule is written into the living spec, replacing the
  "constructor name, no suffix" convention that `residuals-gof` left behind.
- autograph's `feature/goldfish-diag` plot methods and NAMESPACE are updated in
  lockstep, in the autograph working copy, because autograph 1.2.0 breaks against
  goldfish the moment the rename lands.

## Capabilities

### New Capabilities

- `class-naming`: the package-wide rule for naming S3 classes — which classes must
  carry the `_goldfish` suffix, which are exempt (deprecated path, internal
  dispatch), how a retired class name behaves, and the requirement that no
  goldfish class contains a dot.

### Modified Capabilities

Forty-four requirements across twenty capabilities name an old class string. A
delta is issued where the requirement's **rule** changes or where the class
string **is the subject** of the contract; the remaining mentions are stale
spellings of a name whose authoritative form `class-naming` now fixes, and are
corrected by a closing sweep task over `openspec/specs/**` rather than by
reissuing forty-four requirement bodies verbatim (a copy-heavy delta is itself a
drift risk, which is the objection ADR-0016 raised against renaming by sweep).

- `diagnostic-plot-classes`: the requirement "goldfish emits plot-ready data,
  autograph plots" states that new class names follow the constructor-name
  convention **with no suffix**. That contradicts the new rule and must be
  replaced in place, not merely supplemented — an additive capability would leave
  two contradicting SHALLs in the living spec, exactly what
  `.plan/opsx-spec-placement-check.sh` exists to catch. The companion requirement
  "examine functions adopt the constructor-named classes" names
  `diagnose_outliers` and `diagnose_changepoints` as literal class strings and
  moves with it.
- `preprocessing-controls`: states the `set_preprocessing()` return class as
  `c("preprocessing.goldfish", "list")` and the `preprocessed.goldfish`
  fast-path contract — the class string is the contract.
- `preprocess-output-writers`: states the default writer's output as a
  `preprocessed.goldfish` object in the capability text and scenarios.
- `model-specification`: states `specification.goldfish` as the object
  `make_specification()` returns.
- `optimizer-selection`: states `algorithm.goldfish` and
  `algorithm_newton.goldfish` as the classes the optimizer setters return and
  dispatch on.
- `multimode-networks`: names `data.goldfish` as the legacy environment. The
  requirement holds unchanged in substance, but the split of that name makes the
  reference ambiguous, so it is clarified to name the legacy environment
  specifically.

## Impact

**goldfish — code.** Class-string sites, `@method`/`@export` roxygen tags and
NAMESPACE entries across `R/`. Weighted by class-string occurrences (not function
names): `result.goldfish` is the bulk at 131 in `R/`, 54 in `tests/`, 95 in
`man/`; `preprocessed.goldfish` 43/18/23; `flavored_result.goldfish` 36/1/19;
the rest are single- and low-double-digit. Principal files: `methods_display.R`,
`methods_postestimate.R`, `methods_predict.R`, `methods_residuals.R`,
`methods_tests.R`, `model_estimate.R`, `model_evaluate.R`, `model_terms.R`,
`diagnostic_tables.R`, `diagnose_onset.R`, `test_gof.R`, `test_time.R`,
`test_parameter.R`, `format_version.R`, `set_opt.R`, `preprocess_writers.R`,
`make_specification.R`, `estimate_flavored.R`, `as_goldfish.R`,
`formula_parser.R`, `zzz.R` (the `dplyr_reconstruct` registration names three
classes as strings), and a new home for the stale stubs alongside
`goldfish-defunct.R`.

**goldfish — tests and docs.** testthat expectations on class vectors, snapshot
files whose printed output includes `<result.goldfish>`-style class tags, `man/`
regeneration via `devtools::document()`, and the vignettes' `.Rmd.orig` sources
where a class is named in prose or output.

**autograph.** `NAMESPACE` (six `S3method(plot, ...)` entries), `R/plot_diagnostics.R`
(the `inherits()` guards at lines ~890–899), and the roxygen `@method` tags, on
branch `feature/goldfish-diag`. Applied in the working copy at
`/Users/ualvaro/Documents/repos/autograph`.

**Users.** Any script testing `inherits(x, "result.goldfish")` or dispatching on
a goldfish class breaks. Objects saved from goldfish 1.9.x stop dispatching;
`print()` and `summary()` on a saved fit explain this and direct the user to
re-fit.

**Not affected.** The C++ core (`src/`) knows nothing about R class names, so no
recompile and no risk to the frozen 1e-6 coefficient baselines. Effect dispatch
tags (`inertia`, `recip`, `trans`, …) and the internal `writer_*`, `data_source_*`,
`model_spec`, `support_constraint_plan`, `fixed_spec`/`initial_spec` classes stay
as they are.
