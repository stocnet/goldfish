# global-attribute-object Specification

## Purpose
Define the `global.goldfish` one-row attribute-snapshot object built by `make_global_attributes()` and its time-stamped `events` history for global covariates.

## Requirements

### Requirement: make_global_attributes produces a one-row snapshot data frame
`make_global_attributes(df)` SHALL accept a one-row numeric `data.frame` as its single argument and return it with class `c("global.goldfish", "data.frame")` prepended. The returned object SHALL have an `attr(..., "events")` attribute initialised to `character(0)`. The function name is plural to signal that one object holds multiple named attributes.

#### Scenario: Single column
- **WHEN** user calls `make_global_attributes(data.frame(winter = 1))`
- **THEN** result is a one-row `data.frame` with column `winter = 1`, class includes `"global.goldfish"`, and `attr(result, "events")` is `character(0)`

#### Scenario: Multiple columns
- **WHEN** user calls `make_global_attributes(data.frame(winter = 0, spring = 1, summer = 0, autumn = 0))`
- **THEN** result has 1 row and 4 columns with names `c("winter", "spring", "summer", "autumn")` and corresponding values

#### Scenario: Multi-row input rejected
- **WHEN** user calls `make_global_attributes(data.frame(winter = c(0, 1)))`
- **THEN** function raises an informative error indicating the input must have exactly one row

#### Scenario: Non-numeric column rejected
- **WHEN** user calls `make_global_attributes(data.frame(label = "spring"))`
- **THEN** function raises an error indicating that all columns must be numeric

---

### Requirement: check_global_attribute validates snapshot format
`check_global_attribute(x)` SHALL pass when `x` is a one-row numeric `data.frame` with class `global.goldfish`, and raise an error when any of the following conditions are violated: `x` is not a `data.frame`; `nrow(x) != 1`; any column is non-numeric; class does not include `"global.goldfish"`.

#### Scenario: Valid object passes
- **WHEN** `check_global_attribute()` is called with a valid `global.goldfish` one-row data frame
- **THEN** no error is raised

#### Scenario: Multi-row object rejected
- **WHEN** `check_global_attribute()` is called with a `global.goldfish` data frame having 2 rows
- **THEN** an informative error is raised mentioning the row count

#### Scenario: Non-numeric column rejected
- **WHEN** `check_global_attribute()` is called with a `global.goldfish` data frame that has a character column
- **THEN** an informative error is raised mentioning the column type

---

### Requirement: link_events.global.goldfish links replace-only timestamped global updates
`link_events(global_obj, events_df, replace = "replace")` SHALL:
1. Validate `global_obj` passes `check_global_attribute()`.
2. Validate `events_df` is a `data.frame` with at least columns `time` and the column named by `replace`.
3. Reject any `events_df` that contains an `increment` column — only replace semantics are supported for global attributes.
4. Record the target column name on the events data frame via `attr(events_df, "replace") <- replace`.
5. Append the name of `events_df` to `attr(global_obj, "events")`.
6. Return the updated `global_obj` invisibly.
No `node` column SHALL be present or required in `events_df`.

#### Scenario: Successful link
- **WHEN** user calls `link_events(seasons, season_events)` where `season_events` has columns `time` and `replace`
- **THEN** `attr(seasons, "events")` includes `"season_events"` and no error is raised

#### Scenario: Events with node column rejected
- **WHEN** `events_df` contains a column named `"node"`
- **THEN** an informative error is raised indicating that global events must not have a node column

#### Scenario: Events with increment column rejected
- **WHEN** `events_df` contains a column named `"increment"`
- **THEN** an informative error is raised indicating that only replace events are supported for global attributes

#### Scenario: Missing time column rejected
- **WHEN** `events_df` does not contain a `"time"` column
- **THEN** an informative error is raised

#### Scenario: Missing replace column rejected
- **WHEN** `events_df` does not contain the column named by the `replace` argument
- **THEN** an informative error is raised naming the missing column
