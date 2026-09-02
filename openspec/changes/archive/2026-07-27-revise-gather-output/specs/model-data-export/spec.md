# model-data-export

The `compute_statistics()` contract: the single statistics-product
function (outputs: preprocessed replay object, gather stack,
ready-to-estimate frame, DBI stream), delegated model/sub_model
validation, per-sub_model intercept and right-censoring semantics,
flavored fid-list keying, DyNAMi coverage, cross-package example recipes,
and the deprecations of the previous surfaces. The gather stack format
itself is owned by the preprocess-output-writers capability.

## ADDED Requirements

### Requirement: compute_statistics is the single statistics-product function
goldfish SHALL export `compute_statistics(x, model, sub_model = NULL,
data = NULL, output = c("preprocessed", "gather", "data.frame", "db"),
control_prep = set_preprocessing(), progress, max_length, ...)` — `x` a
formula or specification, matching the `estimate_*()` first argument —
returning, per `output`: the `preprocessed.goldfish` replay object
(`"preprocessed"`), the gather stack (`"gather"`), the ready-to-estimate
long frame (`"data.frame"`), or the DBI stream handle (`"db"`).
`gather_model_data()` SHALL be a lifecycle soft-deprecated wrapper over
`compute_statistics()` for at least one release cycle, warning with the
direct replacement (no two-hop chains). The `compute_stats` name SHALL be
**deleted**: not exported and not defined — no stub of any kind (the name
only existed inside the unreleased 2.0.0 development line and never
shipped in a release); the NEWS entry SHALL record the rename to
`compute_statistics()`.
`compute_statistics(output = "preprocessed")` SHALL be the object the
diagnostic replay consumers accept via their `preprocessed =` argument.

#### Scenario: outputs map to products
- **WHEN** the same model is run through the four `output` values on a
  small fixture
- **THEN** `"preprocessed"` returns a `preprocessed.goldfish`, `"gather"`
  the gather stack, `"data.frame"` the long frame, and `"db"` streams to
  the supplied DBI table, all from the same preprocessing semantics.

#### Scenario: gather_model_data keeps working with a direct pointer
- **WHEN** `gather_model_data()` is called
- **THEN** a lifecycle soft-deprecation warning names
  `compute_statistics()` (with the matching `output`) and the result
  equals the `compute_statistics()` output.

#### Scenario: compute_stats is gone
- **WHEN** `goldfish::compute_stats` is accessed or `compute_stats()` is
  called after attaching goldfish
- **THEN** R reports the object is not exported / the function is not
  found (no stub exists), and the NEWS entry for the release records the
  rename to `compute_statistics()`.

### Requirement: preprocessing_only is superseded by output = "preprocessed"
The estimators' `preprocessing_only` argument SHALL be lifecycle
soft-deprecated with a warning naming
`compute_statistics(output = "preprocessed")`; while deprecated,
`preprocessing_only = TRUE` SHALL still return the preprocessed object
unchanged. The object returned by
`compute_statistics(output = "preprocessed")` SHALL be identical to the
one the legacy flag returns for the same inputs.

#### Scenario: legacy flag warns and still works
- **WHEN** `estimate_rem(f, data = d, preprocessing_only = TRUE)` is
  called
- **THEN** a soft-deprecation warning names
  `compute_statistics(output = "preprocessed")` and the call returns the
  `preprocessed.goldfish` object as before.

#### Scenario: parity with the legacy route
- **WHEN** the same formula, data, and preprocessing options run through
  `compute_statistics(output = "preprocessed")` and the deprecated
  `estimate_dynam(..., preprocessing_only = TRUE)`
- **THEN** the two `preprocessed.goldfish` objects are identical.

### Requirement: model and sub_model validate once, with cli errors
`compute_statistics()` SHALL NOT re-validate `model`/`sub_model` locally;
validation SHALL occur once in the shared estimation wrapper via
`check_model_par()`, which SHALL abort with a cli error listing the
allowed sub_models for the given model (DyNAM: choice, rate, rate_ordered,
choice_coordination; REM: rate, rate_ordered; DyNAMi: choice, rate). The
REM `"choice"` lifecycle warning at that single site SHALL name both
successors (`"rate"` exact-time, `"rate_ordered"` ordinal). The
`sub_model = NULL` defaulting (REM → `"rate"`, otherwise `"choice"`) SHALL
be documented together with its intercept consequence.

#### Scenario: rate_ordered flows through
- **WHEN** `compute_statistics(f, model = "REM",
  sub_model = "rate_ordered", output = "gather", data = d)` is called
- **THEN** it returns the ordinal gather output (no intercept column, no
  right-censored rows) without any deprecation warning.

#### Scenario: invalid combination gets a cli error with the allowed set
- **WHEN** `compute_statistics(f, model = "REM",
  sub_model = "choice_coordination", data = d)` is called
- **THEN** it aborts with a cli error listing `rate` and `rate_ordered`
  as REM's sub_models.

#### Scenario: REM choice deprecation names both successors
- **WHEN** `sub_model = "choice"` is used with `model = "REM"`
- **THEN** the lifecycle warning names `"rate"` (exact-time) and
  `"rate_ordered"` (ordinal) and the call proceeds with `"rate"`.

### Requirement: intercept and right-censoring semantics reported at finalization
For `sub_model = "rate"` (exact-time) outputs SHALL include the
force-added time intercept and right-censored rows carrying `timespan`,
exactly as estimation does; for `sub_model = "rate_ordered"` outputs SHALL
contain no intercept column and no right-censored rows, with a formula
intercept dropped under the estimator's message. `has_intercept` and
`right_censored` SHALL be attached at finalization so every output form
and entry point reports them (flavored outputs mirror the `process_map`
columns). Documentation SHALL mandate name-based statistic-column access.

#### Scenario: exact-time REM reports intercept and censoring
- **WHEN** `output = "gather"` runs for `model = "REM",
  sub_model = "rate"` on data with censoring intervals
- **THEN** `has_intercept` and `right_censored` are `TRUE`, an intercept
  column is present, and right-censored rows carry `timespan` with the
  dependent marker `FALSE`.

#### Scenario: ordinal flags are FALSE
- **WHEN** the same model runs with `sub_model = "rate_ordered"`
- **THEN** `has_intercept` and `right_censored` are `FALSE` and no
  intercept column exists.

### Requirement: flavored specifications return fid-indexed lists with process_map
On a flavored specification, every `output` form SHALL return a list
indexed by integer fid carrying the `process_map` table attribute (columns
as defined by the flavored-processes capability), matching the keying of
flavored preprocessing and the estimation container. Human-readable labels
SHALL be rendered from the process_map and SHALL NOT be parsed back from
list keys.

#### Scenario: flavored gather is fid-keyed
- **WHEN** a two-flavor specification runs through
  `compute_statistics(output = "gather")`
- **THEN** the result is a fid-indexed list of gather outputs whose
  process_map identifies (layer, flavor, family) per fid, consistent with
  the fid mapping of the flavored estimation container for the same
  specification.

### Requirement: DyNAMi is supported or fails loudly
`compute_statistics()` SHALL support `model = "DyNAMi"` for every output
form its preprocessing can produce; if an output form is unavailable for
DyNAMi, the call SHALL abort with a cli error naming the supported forms —
never returning a silently wrong or empty result. The DyNAMi output is a
single stack (joining/leaving are parameters within one fit, not fid
flavors).

#### Scenario: DyNAMi gather either works or errors informatively
- **WHEN** `compute_statistics(f, model = "DyNAMi", sub_model = "choice",
  output = "gather", data = d)` is called on the DyNAMi fixture
- **THEN** it returns the gather stack for the interaction model, or —
  where the routing is not yet available — aborts with a cli error naming
  the output forms DyNAMi supports.

### Requirement: ready-to-estimate frame output
With `output = "data.frame"` the return SHALL be one long base data frame
(per fid under flavoring) whose columns are, in order: `event`, `chosen`
(0/1), `sender`, `receiver` (labels; NA where not applicable), `index_i`,
`index_j` (1-based node indices), `timespan` (exposure; NA for multinomial
rows), `is_dependent` (`FALSE` marks right-censored rows), followed by the
statistic columns named by the effect short names (the printed term
strings), with `effect_description` attached as an attribute. Rows SHALL enumerate the realized
(constraint-filtered) candidate set per event, consistent with the gather
stack. The documentation SHALL state the row-count formula
(events × candidates) and point to `output = "db"` for out-of-memory
scale.

#### Scenario: frame reproduces the stack
- **WHEN** the same model runs with `output = "gather"` and
  `output = "data.frame"`
- **THEN** the frame's statistic columns equal `stat_all_events`
  row-for-row and `sum(chosen)` equals the number of dependent events.

#### Scenario: choice frame is clogit-ready
- **WHEN** a DyNAM-choice frame is fed to
  `survival::clogit(chosen ~ <stats> + strata(event))`
- **THEN** it runs without reshaping and reproduces the goldfish choice
  estimates within the documented optimizer tolerance on the shared
  fixture.

### Requirement: cross-package example recipes on the help page
The `compute_statistics()` help page SHALL include worked, verified
recipes on the frame output: ordinal fits via
`survival::coxph(Surv(rep(1, nrow(frame)), chosen) ~ <stats> +
strata(event))` and `survival::clogit`; exact-time fits via
`glm(chosen ~ <stats> + offset(log(timespan)), family = poisson)` over
dependent plus right-censored rows; conditional-logit fits via
`mlogit::mlogit` with `dfidx(idx = c("event", "option"))` where `option`
is `index_j`. Examples SHALL be guarded with `@examplesIf
requireNamespace(...)` and fits wrapped in `\donttest`.

#### Scenario: examples render and are guarded
- **WHEN** the man page is built and checked
- **THEN** the recipes appear with `requireNamespace()` guards and
  `R CMD check` passes without survival or mlogit installed.

### Requirement: estimator-frame parity is tested
A test SHALL fit the same specification through `estimate_*()` and through
the conditional-logit route on the frame output (small fixture) and assert
coefficient agreement within a documented optimizer tolerance (1e-4 or
tighter), guarding the export surface against drifting from estimation.

#### Scenario: identity-route parity test
- **WHEN** the parity test runs on the small fixture
- **THEN** goldfish and the frame-based conditional-logit coefficients
  agree within the documented tolerance.
