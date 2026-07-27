# naming-deprecations Specification

## Purpose
TBD - created by archiving change algorithm-naming. Update Purpose after archive.
## Requirements
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

### Requirement: Components of returned objects are named in snake_case

Every component of every object goldfish returns to a user SHALL be named in
snake_case, so that a reader of one component can predict the spelling of its
neighbours. This covers the fitted result and its nested convergence report, the
gather export, and the preprocessed object. The rule exists because the mixed
convention is not merely untidy: a component read under the wrong spelling
yields `NULL` rather than an error, and `NULL[[i]]` is itself `NULL`, so code
written against the wrong name can run and silently test or report nothing.

#### Scenario: neighbouring components share one convention
- **WHEN** a user reads any component of a fitted model
- **THEN** its name is snake_case, including inside the nested convergence
  report, where no component is spelled in a different convention from its
  siblings

#### Scenario: the export and preprocessed objects follow the same rule
- **WHEN** a user reads a component of a gather export or of a preprocessed
  object
- **THEN** its name is snake_case, matching the fitted result's convention

### Requirement: A fitted object from before the rename is recognized and reported

goldfish SHALL recognize a fitted object produced before the components were
renamed, and SHALL tell the user that it must be re-fitted rather than failing
obscurely. The renamed components get **no** deprecation window: an object from
the last CRAN release is already unusable for reasons unrelated to spelling — it
lacks components the current methods require, so its summary fails and its
log-likelihood carries no degrees of freedom, which makes model comparison wrong
without saying so. Preserving the old spellings would therefore protect nothing
while adding permanent bookkeeping. Newly built fitted **and preprocessed**
objects SHALL each record the format version they were built with, and the
absence of that record SHALL itself identify an object built before the record
existed — so recognition rests on one positive convention rather than on
inspecting which substantive components an object happens to be missing. Both
kinds of object SHALL be recognized by the same rule; neither gets a weaker test
than the other. The two records SHALL live under **distinct component names**,
because a preprocessed object can be carried inside a fitted object, and one
shared name would put two different facts under one key at two depths of the same
object.

#### Scenario: an old object is named as old, not met with an internal error
- **WHEN** any method is called on a fitted object produced before the rename
- **THEN** the message identifies it as predating the current version and says
  to re-fit, rather than reporting a missing value or a zero-length argument

#### Scenario: a computation that would be wrong is refused, not merely flagged
- **WHEN** a log-likelihood, variance-covariance matrix, summary, or information
  criterion is requested from a pre-rename object
- **THEN** the call fails with that message, because returning a value whose
  degrees of freedom are unknown would silently corrupt model comparison

#### Scenario: printing an unfamiliar object still tells the user what it is
- **WHEN** a pre-rename object is printed
- **THEN** the message is informative and what can be shown is still shown, so a
  user holding an unlabelled object can find out what they have

#### Scenario: a current object is recognized from its own record
- **WHEN** a model is fitted with the current version and any method is called
- **THEN** no such message appears, and the object carries the format version it
  was built with

#### Scenario: an object carrying no version record is treated as predating it
- **WHEN** a fitted or preprocessed object carries no version record at all
- **THEN** it is reported as predating the current version, by the same rule for
  both kinds of object, without consulting which other components it carries

#### Scenario: a stale preprocessed object is refused at estimation entry
- **WHEN** a preprocessed object built by an earlier version is passed to an
  estimator
- **THEN** estimation stops and tells the user to recompute it, rather than
  reaching the numerical core and failing on a component that is not there

#### Scenario: a nested preprocessed object keeps its own record distinct
- **WHEN** a fitted object carries the preprocessed object it was estimated from,
  so that post-estimation surfaces can replay from it
- **THEN** the fitted object's record and the preprocessed object's record are
  readable independently under their own names, and neither shadows or is
  mistaken for the other

#### Scenario: a version record is not mistaken for the wrong kind of object
- **WHEN** a preprocessed object is passed where a fitted object is expected
- **THEN** the call is refused on the object's kind, rather than being accepted
  because a version record happened to be absent

#### Scenario: NEWS records the rename and that there is no window
- **WHEN** the release notes for the version carrying this rename are read
- **THEN** they list every renamed component with its old and new spelling, and
  state that the old names stop working immediately rather than after a
  deprecation period

### Requirement: The reserved incremental estimation variant is removed

The internal model-specification constructor SHALL NOT carry a parameter
reserving a future incremental estimation variant. The parameter accepts only
its default and aborts on every other value, so nothing depends on it, and it
spells `engine` — the vocabulary replaced by `backend` — which reintroduces a
second meaning for a word this line of work removed. Explanatory comments about
estimation state being reusable by algorithm variants SHALL be kept, since that
reasoning is independent of whether any particular variant ships.

#### Scenario: the reserved parameter is gone
- **WHEN** the model-specification constructor is called
- **THEN** it accepts no parameter reserving an incremental variant, and no code
  path aborts on one
