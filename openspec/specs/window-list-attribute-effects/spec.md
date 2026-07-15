# window-list-attribute-effects Specification

## Purpose
Have `parse_time_windows()` reject a time window applied to attribute-only effects, where windowing has no defined meaning.

## Requirements

### Requirement: parse_time_windows rejects window on attribute-only effects
`parse_time_windows()` SHALL detect every windowed effect in the formula whose object
argument resolves to a nodal attribute — whether as a bare `nodeset$attribute` reference
or as a `list(...)` expression containing one or more `nodeset$attribute` elements — and
raise a `cli::cli_abort` error listing all violations found in the formula.

The error SHALL NOT be raised on the first violation and then stop; it SHALL collect all
attribute+window violations in the formula before aborting.

Each violation entry in the error SHALL use the format:

> Effect `` `{effect_name}` `` on `` `{nodeset$attribute}` `` must not have a `window`
> argument, remove it from the formula.

The overall error header SHALL be:

> `window` is not supported for attribute effects.

with a footer bullet:

> Remove the `window` argument from the listed effects in the formula.

#### Scenario: single attribute reference with window raises error
- **WHEN** a model formula includes `alter(nodes$age, window = 5)`
- **THEN** `parse_time_windows()` raises a `cli::cli_abort` error containing the message
  "Effect `alter` on `nodes$age` must not have a `window` argument, remove it from the
  formula." and preprocessing does not continue

#### Scenario: list with attribute references and window raises error
- **WHEN** a model formula includes
  `ego_alter_interaction(list(nodes$attr1, nodes$attr2), window = 5)`
- **THEN** `parse_time_windows()` raises a `cli::cli_abort` error containing entries for
  `nodes$attr1` and `nodes$attr2` (one "x" bullet per attribute in the list)

#### Scenario: multiple attribute+window violations reported together
- **WHEN** a model formula includes both `alter(nodes$age, window = 5)` and
  `same(nodes$gender, window = 5)`
- **THEN** `parse_time_windows()` raises a single `cli::cli_abort` error with two "x"
  bullets — one for each violation — rather than stopping on the first one

#### Scenario: network effects with window are not affected
- **WHEN** a model formula includes `trans(friendship, window = 5)` (single network) or
  `mixed_trans(list(net1, net2), window = 5)` (network list)
- **THEN** no error is raised for these effects; they proceed through normal windowing

#### Scenario: mixed formula — one attribute violation, one valid network window
- **WHEN** a model formula includes both `alter(nodes$age, window = 5)` and
  `trans(friendship, window = 5)`
- **THEN** only the `alter` effect appears in the `cli::cli_abort` error; the error still
  fires (even though `trans` is valid) because there is at least one violation
