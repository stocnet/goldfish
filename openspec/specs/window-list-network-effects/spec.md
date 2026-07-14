# window-list-network-effects Specification

## Purpose
Let `parse_time_windows()` accept `list(net1, net2)` network arguments on windowed effects, applying the time window to each supplied network.

## Requirements

### Requirement: parse_time_windows handles list(net1, net2) network arguments
`parse_time_windows()` SHALL detect when the network argument of a windowed effect is a
`list(...)` expression, evaluate each named network individually, create a windowed copy
of each network object (zero-filled, same `nodes`/`directed`/`dimnames`/`class` attributes),
link the dissolution events for each, and rewrite the formula reference as
`list(net1_<w>, net2_<w>)`.
The existing single-object path SHALL be unchanged.

#### Scenario: mixed_trans with window — formula reference is rewritten
- **WHEN** a model formula includes `mixed_trans(list(net1, net2), window = 5)` and
  `net1` and `net2` are valid network objects in scope
- **THEN** `parse_time_windows()` assigns `net1_5` and `net2_5` windowed network objects
  to the formula environment and sets the effect's network reference to
  `"list(net1_5, net2_5)"` (no error is raised)

#### Scenario: windowed network objects have correct structure
- **WHEN** `net1_5` is created for a windowed `mixed_trans` effect with `window = 5`
- **THEN** `net1_5` is a zero-filled matrix with the same `nrow`, `ncol`, `dimnames`,
  `nodes`, `directed`, and `class` attributes as `net1`, and `attr(net1_5, "events")` is
  non-empty (contains the dissolution event data frame names)

#### Scenario: dissolution events are created for each network in the list
- **WHEN** `net1` and `net2` each have one linked event data frame and `window = 5`
- **THEN** the environment contains four event data frames: the original two and two
  windowed versions, each windowed version containing paired creation and dissolution rows
  with dissolution times offset by 5

#### Scenario: single-network effects with window are unaffected
- **WHEN** a model formula includes `trans(friendship, window = 5)` (single network, no list)
- **THEN** `parse_time_windows()` behaves exactly as before: one windowed network object
  `friendship_5` is created via the existing code path

### Requirement: mixed_* effects with window produce correct windowed preprocessing
The four `mixed_*` init functions SHALL return an empty (zero-filled) cache and stat when
`window` is finite (`mixed_trans`, `mixed_cycle`, `mixed_common_sender`,
`mixed_common_receiver`), consistent with the behavior already implemented in each init function.
After `parse_time_windows` is fixed, the windowed list reference evaluates to a valid list
of two windowed networks, and the existing `if (!is.null(window) && !is.infinite(window))`
early-return in each init function handles the rest.

#### Scenario: mixed_trans with finite window initialises empty cache
- **WHEN** `init_DyNAM_choice.mixed_trans()` is called with `window = 5` and
  two valid network matrices
- **THEN** the returned `cache` is a zero matrix and `stat` is a zero matrix

#### Scenario: mixed_trans windowed preprocessing integration
- **WHEN** a DyNAM choice model is preprocessed with
  `~ mixed_trans(list(net1, net2), window = 5)` on a sequence of events
- **THEN** preprocessing completes without error and the computed statistics reflect
  only two-paths formed by events within the 5-unit window (edges outside the window
  are dissolved and removed from the count)

#### Scenario: all four mixed_* effects with window complete preprocessing
- **WHEN** each of `mixed_trans`, `mixed_cycle`, `mixed_common_sender`,
  `mixed_common_receiver` is used with a finite `window` parameter
- **THEN** preprocessing runs without error for each effect
