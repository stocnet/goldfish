## MODIFIED Requirements

### Requirement: set_preprocessing() constructs the preprocessing control object
The package SHALL export `set_preprocessing()` (the renamed
`set_preprocessing_opt()`) with the same arguments and semantics —
`start_time`, `end_time`, `opportunities_list` (already soft-deprecated),
`impute`, `db`, `db_table` — returning an object of class
`c("goldfishPrepCtrl", "list")`. The `print` method SHALL dispatch on
the new class. Argument validation SHALL be unchanged.

The documentation of `start_time` and `end_time` SHALL describe the behavior
the preprocessing implements, and SHALL point to the observation-window
semantics for what the bounds do: that events before `start_time` are replayed
to build the initial statistics rather than discarded, that traversal stops at
`end_time`, and that the closing exposure interval is stored by the sub-models
whose likelihood defines a compensator. It SHALL NOT state that preprocessing
continues past `end_time`.

#### Scenario: constructor returns the new class
- **WHEN** `set_preprocessing(start_time = 10)` is called
- **THEN** the returned object inherits `goldfishPrepCtrl`, carries
  `start_time = 10`, and prints via the new-class method.

#### Scenario: option semantics unchanged
- **WHEN** the same options are supplied to `set_preprocessing()` as were
  supplied to `set_preprocessing_opt()` before the rename
- **THEN** preprocessing behaves identically.

#### Scenario: the documented window behavior matches the implementation
- **WHEN** the `start_time` and `end_time` documentation is read
- **THEN** it states that traversal stops at `end_time`, and does not claim
  that processing continues past it

### Requirement: control_prep is the preprocessing-control argument everywhere
`estimate_dynam()`, `estimate_dynami()`, and `estimate_rem()` SHALL accept
the preprocessing control object through a `control_prep` argument
(default `set_preprocessing()`), validated with `inherits(x,
"goldfishPrepCtrl")`. Functions minted after this change that take
a preprocessing control (`compute_statistics()` in revise-gather-output,
future estimators) SHALL use the same `control_prep` name and validation.
`gather_model_data()` is deprecated wholesale by revise-gather-output and
SHALL NOT be re-signatured.

#### Scenario: estimator honors control_prep
- **WHEN** `estimate_rem(f, data = d, control_prep =
  set_preprocessing(start_time = 5))` is called
- **THEN** preprocessing starts at time 5, exactly as
  `control_preprocessing` produced before the rename.

#### Scenario: wrong object rejected
- **WHEN** `control_prep` receives an object that does not inherit
  `goldfishPrepCtrl`
- **THEN** the caller aborts with a cli error naming `set_preprocessing()`.

### Requirement: preprocessed= is the single supply argument on estimators
`estimate_dynam()`, `estimate_dynami()`, and `estimate_rem()` SHALL accept
a `preprocessed =` argument (default `NULL`) taking a
`goldfishPrep` object to skip preprocessing, with exactly the
semantics `preprocessing_init =` had (including the format-version check
rejecting stale objects). The argument name matches the diagnostic
consumers' `preprocessed =` so one name supplies the replay object
everywhere; the standalone producer of that object is
`compute_statistics(output = "preprocessed")` (revise-gather-output).

#### Scenario: preprocessed object reused
- **WHEN** a `goldfishPrep` object is passed as
  `estimate_dynam(..., preprocessed = prep)`
- **THEN** estimation skips preprocessing and produces the same fit as the
  full pipeline.

#### Scenario: stale object rejected
- **WHEN** a `goldfishPrep` with an outdated format version is
  supplied via `preprocessed =`
- **THEN** estimation aborts with the existing outdated-format error.
