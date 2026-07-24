# naming-deprecations Specification

## ADDED Requirements

### Requirement: Renamed functions keep working as soft-deprecated aliases
Each renamed function SHALL remain exported as a thin wrapper that forwards
all arguments unchanged to the new name and emits
`lifecycle::deprecate_soft(when = "2.0.0")` attributed to the direct
caller: `set_estimation_opt()` → `set_algorithm_newton()`,
`set_preprocessing_opt()` → `set_preprocessing()`, `examine_outliers()` →
`diagnose_outliers()`, `examine_changepoints()` →
`diagnose_changepoints()`. Alias results SHALL be identical to calling the
new name directly.

#### Scenario: alias forwards and warns
- **WHEN** `set_estimation_opt(max_iterations = 5)` is called directly by
  user code
- **THEN** a soft-deprecation warning names `set_algorithm_newton()` and the
  returned object is identical to `set_algorithm_newton(max_iterations =
  5)`.

#### Scenario: indirect calls stay quiet
- **WHEN** an alias is reached from another package function rather than
  user code
- **THEN** `deprecate_soft` emits no user-facing warning (lifecycle
  attribution rules).

### Requirement: Renamed arguments keep working as deprecated sentinels
Renamed arguments SHALL remain in the signatures as
`lifecycle::deprecated()` sentinels that, when supplied, emit a
soft-deprecation warning naming the new argument and are honored only when
the new argument is not supplied: `control_estimation` → `control_algo`,
`control_preprocessing` → `control_prep`, `preprocessing_init` →
`preprocessed` (estimators); `parameter` → `threshold`
(`diagnose_outliers()`). `preprocessing_only` is NOT deprecated by this
change — revise-gather-output owns that retirement once its replacement
(`compute_statistics(output = "preprocessed")`) exists.

#### Scenario: old argument forwards
- **WHEN** `estimate_dynam(f, data = d, control_estimation =
  set_algorithm_newton())` is called
- **THEN** a warning names `control_algo` and estimation proceeds as if
  `control_algo` had been supplied.

#### Scenario: preprocessing_only untouched here
- **WHEN** `estimate_rem(f, data = d, preprocessing_only = TRUE)` is called
  after this change and before revise-gather-output lands
- **THEN** it returns the `preprocessed.goldfish` object with no
  deprecation warning (the retirement belongs to revise-gather-output).

### Requirement: No two-hop deprecation messages
Every live deprecation or defunct message SHALL point at the final name in
the same release wherever it references a renamed surface: the
`return_interval_loglik` / `return_probabilities` / `return_event_scores`
warnings SHALL name `set_algorithm_newton(diagnostics)`; the
`convergence_criterion` and `fixed_parameters` warnings SHALL render
against `set_algorithm_newton()`; the 1.7.0 defunct shims
`examineOutliers()` / `examineChangepoints()` SHALL point at
`diagnose_outliers()` / `diagnose_changepoints()`; cli errors and roxygen
prose naming `set_estimation_opt` / `set_preprocessing_opt` SHALL be
updated. No message SHALL direct users to a name that is itself
deprecated.

#### Scenario: legacy diagnostics flag points at the final constructor
- **WHEN** `set_algorithm_newton(return_event_scores = TRUE)` is called
- **THEN** the warning names `set_algorithm_newton(diagnostics)` (not
  `set_estimation_opt`).

#### Scenario: defunct shim skips the middle name
- **WHEN** `examineOutliers(fit)` is called
- **THEN** its message names `diagnose_outliers()` directly.

### Requirement: diagnose_* replaces examine_* with unchanged behavior
goldfish SHALL export `diagnose_outliers(x, method = c("Hampel", "IQR",
"Top"), threshold = 3, window = NULL)` and `diagnose_changepoints(x,
moment = c("mean", "variance"), method = c("PELT", "AMOC", "BinSeg"),
window = NULL, ...)`, behaving identically to the `examine_*` functions
they rename
(`threshold` renames `examine_outliers()`'s vague `parameter`). Result
classes are NOT changed by this requirement (they change in
residuals-gof).

#### Scenario: identical diagnostics under the new verb
- **WHEN** the same fit is passed to `diagnose_outliers()` and to the
  deprecated `examine_outliers()`
- **THEN** the returned objects are identical apart from the deprecation
  warning.

### Requirement: NEWS documents the migration and the removal horizon
The 2.0.0 NEWS entry SHALL contain the complete old→new mapping table
(functions and arguments) and SHALL state the removal policy: the 2.0.0
alias layer and the 1.7.0 camelCase defunct layer are scheduled for
removal no earlier than 3.0.0.

#### Scenario: migration table present
- **WHEN** the 2.0.0 NEWS section is read
- **THEN** every rename in this change appears in an old→new table with the
  removal horizon stated.
