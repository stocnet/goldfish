## MODIFIED Requirements

### Requirement: set_preprocessing() constructs the preprocessing control object
The package SHALL export `set_preprocessing()` (the renamed
`set_preprocessing_opt()`) with the same arguments and semantics —
`start_time`, `end_time`, `opportunities_list` (already soft-deprecated),
`impute`, `db`, `db_table` — returning an object of class
`c("preprocessing.goldfish", "list")`. The `print` method SHALL dispatch on
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
- **THEN** the returned object inherits `preprocessing.goldfish`, carries
  `start_time = 10`, and prints via the new-class method.

#### Scenario: option semantics unchanged
- **WHEN** the same options are supplied to `set_preprocessing()` as were
  supplied to `set_preprocessing_opt()` before the rename
- **THEN** preprocessing behaves identically.

#### Scenario: the documented window behavior matches the implementation
- **WHEN** the `start_time` and `end_time` documentation is read
- **THEN** it states that traversal stops at `end_time`, and does not claim
  that processing continues past it
