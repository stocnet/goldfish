## MODIFIED Requirements

### Requirement: set_preprocessing() constructs the preprocessing control object
The package SHALL export `set_preprocessing()` (the renamed
`set_preprocessing_opt()`) with the same arguments and semantics —
`start_time`, `end_time`, `opportunities_list` (already soft-deprecated),
`impute`, `db`, `db_table` — plus `allow_reflexive`, returning an object of class
`c("goldfishPrepCtrl", "list")`. The `print` method SHALL dispatch on
the new class. Argument validation SHALL be unchanged for the pre-existing
arguments.

The documentation of `start_time` and `end_time` SHALL describe the behavior
the preprocessing implements, and SHALL point to the observation-window
semantics for what the bounds do: that events before `start_time` are replayed
to build the initial statistics rather than discarded, that traversal stops at
`end_time`, and that the closing exposure interval is stored by the sub-models
whose likelihood defines a compensator. It SHALL NOT state that preprocessing
continues past `end_time`.

`allow_reflexive` SHALL default to `NULL`, meaning "use the value the
specification carries", and a non-`NULL` value SHALL override the
specification's for that run. It is the surface reaching a formula-based
`estimate_*()` call, which builds its specification internally, and it reaches
`compute_statistics()` and `simulate()` through the same control object. A
`TRUE` override SHALL be refused, with the same reasons as at specification
construction, at the point where the model becomes known, since the control
object alone cannot see the layer or the sub-model.

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

#### Scenario: the control overrides the specification
- **WHEN** a specification carrying `allow_reflexive = FALSE` is estimated with
  `control_prep = set_preprocessing(allow_reflexive = TRUE)`
- **THEN** the run allows self-ties, and the same override reaches a
  `compute_statistics()` or `simulate()` call given the same control object.

#### Scenario: the control defers to the specification by default
- **WHEN** `allow_reflexive` is left at `NULL`
- **THEN** the specification's own value decides, and a formula-based call with
  no specification of its own uses the documented default of `FALSE`.

#### Scenario: an impossible override is refused
- **WHEN** `set_preprocessing(allow_reflexive = TRUE)` is used with a two-mode
  dependent layer or a DyNAM `choice_coordination` sub-model
- **THEN** the call aborts with the same reason the specification constructor
  gives, naming the control argument as the source.

### Requirement: preprocessed= is the single supply argument on estimators
`estimate_dynam()`, `estimate_dynami()`, and `estimate_rem()` SHALL accept
a `preprocessed =` argument (default `NULL`) taking a
`goldfishStat` object to skip preprocessing, with exactly the
semantics `preprocessing_init =` had (including the format-version check
rejecting stale objects). The argument name matches the diagnostic
consumers' `preprocessed =` so one name supplies the replay object
everywhere; the standalone producer of that object is
`compute_statistics(output = "preprocessed")` (revise-gather-output).

A supplied object SHALL carry the reflexivity its risk set was built under, and
a call whose resolved reflexivity differs from it SHALL abort, naming the stored
value, the requested value and that the object must be preprocessed again. A
baked risk set and the likelihood reading it cannot disagree about which cells
exist.

#### Scenario: preprocessed object reused
- **WHEN** a `goldfishStat` object is passed as
  `estimate_dynam(..., preprocessed = prep)`
- **THEN** estimation skips preprocessing and produces the same fit as the
  full pipeline.

#### Scenario: stale object rejected
- **WHEN** a `goldfishStat` with an outdated format version is
  supplied via `preprocessed =`
- **THEN** estimation aborts with the existing outdated-format error.

#### Scenario: a reflexivity mismatch is refused
- **WHEN** a `goldfishStat` built with self-ties excluded is supplied to a call
  that resolves to `allow_reflexive = TRUE`, or the reverse
- **THEN** the call aborts naming both values and pointing to re-preprocessing,
  rather than estimating a likelihood over a risk set built under the other rule.
