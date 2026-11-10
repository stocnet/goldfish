## MODIFIED Requirements

### Requirement: Layer sides via declared sender/receiver mode sets only
A layer's sides SHALL be defined by `info$sender`/`info$receiver` as **per-layer sets of
`nodes$mode` values** (each node carries exactly one mode; a layer's declaration may name
several, and different layers MAY declare different pairs): identical sets make the layer
one-mode over that subset of nodes; disjoint sets make it two-mode with `from`/`to`
remapped through a mode map (global id + label ⇄ (side, local id)) onto the engine's local
index spaces; partially overlapping sets SHALL abort. The declaration SHALL be a
**character vector whose names repeat once per (layer, mode)**
(`c(survey = "employees", survey = "supervisor", report = "employees")`) — the only
per-layer set encoding `manynet` admits, since it type-checks these entries as character.
An **unnamed** character vector SHALL apply to every layer. A **list** of per-layer sets
SHALL abort with a message showing the vector form: `add_info()` does not validate, so a
list would pass where it is written and abort later inside `bind_changes()`, which does.
`is_two_mode` SHALL be resolved once per layer during mapping and carried on the map,
rather than re-derived downstream from node-set names.
Side purity SHALL be validated (error naming the layer and offending nodes). The focal
layer's side pair defines the model's node sets. A layer without the declaration SHALL be
one-mode over ALL nodes — ties are never inspected to infer modes and `nodes$mode` alone
is an ordinary attribute (documented callout for multimodal objects). Attributes SHALL be
served per side from the single nodes tibble (`ego()` reads the sender-side slice,
`alter()` the receiver-side slice); NAs flow into the existing imputation. Composition
SHALL split by the node's side into `active_mode1`/`active_mode2`. On a two-mode layer
`directed` describes no storage behavior: validation SHALL accept either value without
an abort, warning or note (manynet declares every two-mode network undirected), and the
layer SHALL NOT be read as symmetric, whatever `info$directed` declares (see "Layer
symmetry resolved once on the mode map").

#### Scenario: Declared two-mode layer remaps to local indices
- **WHEN** a membership layer declares `sender = "person"`, `receiver = "org"` over a
  nodes tibble with a `mode` column
- **THEN** its events land in an n_person × n_org matrix under per-side local indices and
  effects get `is_two_mode` set as with legacy two-node-set input.

#### Scenario: One object mixes a one-mode and a two-mode layer
- **WHEN** `info$sender` declares `survey = c("employees", "supervisor")` and
  `report = "employees"`, with `info$receiver` declaring
  `survey = c("employees", "supervisor")` and `report = "supervisor"`
- **THEN** `survey` is one-mode over the employee+supervisor nodes while `report` is
  two-mode employees→supervisor, each layer mapping to its own local index spaces.

#### Scenario: The declaration survives the manynet workflow
- **WHEN** per-layer sets are declared as a repeated-name character vector on an object
  assembled with `make_stocnet()` / `add_info()` and then piped through `bind_changes()`
- **THEN** every step validates, and `split()` on the declaration recovers the per-layer
  sets goldfish maps.

#### Scenario: A list declaration is rejected early
- **WHEN** `info$sender` is written as a list of per-layer sets
- **THEN** goldfish aborts naming the field and showing the repeated-name vector form,
  rather than letting the object pass here and abort later inside `bind_changes()`.

#### Scenario: Identical mode sets restrict a one-mode layer
- **WHEN** a friendship layer declares `sender = receiver = c("employees", "supervisors")`
  in a nodes tibble that also contains other modes
- **THEN** the layer is one-mode over exactly the employee+supervisor nodes; ties touching
  any other mode abort as side-impure.

#### Scenario: Partially overlapping mode sets rejected
- **WHEN** a layer declares `sender = "employees"`,
  `receiver = c("employees", "supervisors")`
- **THEN** validation aborts explaining sides must be identical (one-mode subset) or
  disjoint (two-mode), suggesting identical sets plus a `support_constraint`.

#### Scenario: Side impurity rejected
- **WHEN** a tie in a two-mode layer has a `from` node whose mode is not in the sender set
- **THEN** validation aborts naming the layer and the offending node.

#### Scenario: Undeclared layer spans all nodes
- **WHEN** a multimodal object has a calls layer with no `sender`/`receiver` declaration
- **THEN** calls is one-mode over the full node set, with no inference from its ties.

#### Scenario: A two-mode layer declared undirected validates silently
- **WHEN** a layer declares disjoint `sender`/`receiver` sets and
  `directed = c(<layer> = FALSE)`
- **THEN** `validate_goldfish_data()` returns the object without an abort, warning or
  message.

## ADDED Requirements

### Requirement: Layer symmetry resolved once on the mode map
Each layer entry of the mode map SHALL carry a logical `symmetric`, resolved once during
mapping, next to `is_two_mode`: `TRUE` exactly when the layer is one-mode and
`info$directed` declares it `FALSE`; `FALSE` on every two-mode layer and on any layer
whose `directed` is `TRUE` or undeclared. It SHALL be stored as one logical per layer on
the map entry; it SHALL NOT be stored per event, per dyad or per node, and SHALL NOT be
re-derived downstream from `info$directed` or from node-set names.

Every reader of a goldfish data object that decides whether a layer's state is mirrored
SHALL take that decision from the map's `symmetric`, never from `info$directed` directly.
This covers the stocnet data source's starting state fold, the walk's `is_undirected`
object property and second, swapped update call, `network_state_at()`, the DyNES panel
snapshot, dyad mask and flip enumeration, and `prepare_dynes_data()`'s starting fold,
canonical-key ordering and double-report checks. Every such caller of
`materialize_network_state()` SHALL pass `directed = !symmetric`, so the fold mirrors
only a symmetric layer. Readers of legacy `network.goldfish` objects are outside this
requirement.

#### Scenario: A square two-mode layer declared undirected holds no phantom ties
- **WHEN** a two-mode layer with four senders (E..H) and four receivers (A..D) is
  declared `directed = FALSE` and its only tie is G→A
- **THEN** `network_state_at()`, the DyNES panel snapshot and the data source's starting
  state each hold exactly one non-zero cell, G→A, and no cell for E→C or any other
  unreported dyad.

#### Scenario: A non-square two-mode layer declared undirected does not crash
- **WHEN** a two-mode layer with three senders and five receivers is declared
  `directed = FALSE` and carries a `time = NA` history row C→H
- **THEN** `prepare_dynes_data()`, `network_state_at()` and the panel snapshot complete
  without a `subscript out of bounds` error, and the state holds exactly C→H.

#### Scenario: The two-mode result does not depend on the declaration
- **WHEN** the same two-mode layer is built twice, once with `directed = TRUE` and once
  with `directed = FALSE`
- **THEN** every reader listed above returns identical state, and `prepare_dynes_data()`
  returns identical rows and starting levels for both objects.

#### Scenario: A manynet-derived two-mode object is read as not symmetric
- **WHEN** an object converted with `manynet::as_stocnet()` from a two-mode network keeps
  its `directed = FALSE`, named for its layer, and declares the two modes as disjoint
  `sender`/`receiver` sets
- **THEN** the layer validates, its map entry has `symmetric = FALSE`, and no reader
  mirrors its state.

#### Scenario: A one-mode undirected layer is still mirrored
- **WHEN** a one-mode layer is declared `directed = FALSE` and its only tie is A→B
- **THEN** its map entry has `symmetric = TRUE` and every reader's state holds both A→B
  and B→A, as before this change.
