# closure-effects-history-mixed Specification

## Purpose
Give the four `mixed_*` closure effects (`mixed_trans`, `mixed_cycle`, `mixed_common_sender`, `mixed_common_receiver`) a consistent `history` parameter and mixed-network semantics.

## Requirements

### Requirement: history parameter accepted by all four mixed_* functions
All four `mixed_*` effect functions SHALL accept a `history` argument with valid values
`"pooled"` (default) and `"sequential"` (`mixed_trans`, `mixed_cycle`, `mixed_common_sender`,
`mixed_common_receiver`). The corresponding `init_DyNAM_choice.mixed_*` functions SHALL
read this parameter and initialize an empty cache when `history != "pooled"`.

`history = "consecutive"` for `mixed_*` effects is explicitly out of scope for this change
(requires cross-network event tracking) and SHALL NOT be accepted; passing
`history = "consecutive"` to a mixed_* function SHALL raise an informative error.

#### Scenario: mixed_trans init with pooled history computes full cache
- **WHEN** `init_DyNAM_choice.mixed_trans()` is called with non-empty networks,
  no window, and `history = "pooled"`
- **THEN** the returned `cache` equals `sign(net1) %*% sign(net2)` (current behavior)

#### Scenario: mixed_common_sender init with sequential history returns empty cache
- **WHEN** `init_DyNAM_choice.mixed_common_sender()` is called with `history = "sequential"`
- **THEN** the returned `cache` is a zero matrix

#### Scenario: mixed_* with history = "consecutive" raises an error
- **WHEN** any `mixed_*` effect function is called with `history = "consecutive"`
- **THEN** a `cli::cli_abort()` error is raised indicating that consecutive history is not
  supported for mixed-network effects

### Requirement: history = "pooled" preserves current behavior for mixed_* effects
When `history = "pooled"` (default), the output of all four `mixed_*` update functions SHALL
be identical to their pre-refactor output for the same inputs.

#### Scenario: mixed_trans pooled — no regression
- **WHEN** `update_DyNAM_choice_mixed_trans()` is called with `history = "pooled"`
- **THEN** `res$cache` and `res$changes` match the pre-refactor output exactly

### Requirement: history = "sequential" for mixed_* uses argument-order rule
For `history = "sequential"`, the network position in `list(net1, net2)` SHALL define temporal
order: **net1 is "first"** and **net2 is "second"**. A valid sequential two-path requires the
net1-edge to have been formed before the net2-edge.

Update logic:
- `netUpdate == 2 && replace >= 1` (adding a net2 / "second" edge): count current net1
  neighbors as valid sequential paths (net1 edge already existed → correct temporal order).
- `netUpdate == 1 && replace >= 1` (adding a net1 / "first" edge): return no new paths
  (net2 edges already exist, meaning the "second" edge came before the "first" → wrong order).
- `replace < 1` (removing a tie, either network): always clear all affected entries regardless
  of history.

#### Scenario: mixed_trans sequential — adding net2 tie counts net1 in-neighbors
- **WHEN** `update_DyNAM_choice_mixed_trans()` is called with `history = "sequential"`,
  `netUpdate = 2`, `replace = 1`
- **THEN** `res$changes` contains entries for all sender-inNeighbors-in-net1 that form valid
  two-paths (same neighbor set as the pooled case with `netUpdate = 2`)

#### Scenario: mixed_trans sequential — adding net1 tie produces no new paths
- **WHEN** `update_DyNAM_choice_mixed_trans()` is called with `history = "sequential"`,
  `netUpdate = 1`, `replace = 1`
- **THEN** `res$changes` is NULL (no new sequential paths created)

#### Scenario: mixed_cycle sequential — adding net2 tie counts net1 in-neighbors
- **WHEN** `update_DyNAM_choice_mixed_cycle()` is called with `history = "sequential"`,
  `netUpdate = 2`, `replace = 1`
- **THEN** `res$changes` contains the receiver-inSender entries valid for sequential order

#### Scenario: mixed_cycle sequential — adding net1 tie produces no new paths
- **WHEN** `update_DyNAM_choice_mixed_cycle()` is called with `history = "sequential"`,
  `netUpdate = 1`, `replace = 1`
- **THEN** `res$changes` is NULL

#### Scenario: mixed_common_sender sequential — adding net2 tie counts net1 out-neighbors
- **WHEN** `update_DyNAM_choice_mixed_common_sender()` is called with
  `history = "sequential"`, `netUpdate = 2`, `replace = 1`
- **THEN** `res$changes` contains entries for net1 out-neighbors of sender that form valid
  common-sender pairs

#### Scenario: mixed_common_sender sequential — adding net1 tie produces no new paths
- **WHEN** `update_DyNAM_choice_mixed_common_sender()` is called with
  `history = "sequential"`, `netUpdate = 1`, `replace = 1`
- **THEN** `res$changes` is NULL

#### Scenario: mixed_common_receiver sequential — adding net2 tie counts net1 in-neighbors
- **WHEN** `update_DyNAM_choice_mixed_common_receiver()` is called with
  `history = "sequential"`, `netUpdate = 2`, `replace = 1`
- **THEN** `res$changes` contains entries for net1 in-neighbors of receiver that form valid
  common-receiver pairs

#### Scenario: mixed_common_receiver sequential — adding net1 tie produces no new paths
- **WHEN** `update_DyNAM_choice_mixed_common_receiver()` is called with
  `history = "sequential"`, `netUpdate = 1`, `replace = 1`
- **THEN** `res$changes` is NULL

#### Scenario: Sequential removal always clears affected entries
- **WHEN** any `mixed_*` update function is called with `history = "sequential"` and
  `replace = 0` (removing a tie)
- **THEN** `res$changes` is populated with decrements for all affected entries regardless
  of which network (`netUpdate`) is being modified
