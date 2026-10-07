## MODIFIED Requirements

### Requirement: Shared cache-update helper in utils_effects_closure.R
The package SHALL contain one unexported delta sink in
`R/utils_effects_closure.R` through which every closure member writes its
cache and constructs its `changes`: the two-path members (`trans`, `cycle`),
the star members (`common_sender`, `common_receiver`), the four `mixed_*`
variants, the three-path member (`four`), the undirected dyad member
(`triangle`) and the node members (`node_trans`, `node_triangle`). The sink
SHALL take the touched entries with their increments, write the cache buffer
**in place** at those entries, and return the `changes` rows for the same
entries with the transformer applied. No closure member SHALL subassign into
its cache outside the sink, and no inline `replaceValues <- replace - oldValue
+ ...` block SHALL appear in any member.

#### Scenario: Helper updates cache and changes for non-empty ids
- **WHEN** the sink is called with a non-empty set of touched entries, an
  increment of `1` at each, cache values all equal to 2, and
  `transformer_fn = identity`
- **THEN** the cache at the indexed positions equals 3 and `changes` is a
  matrix with `node1`, `node2`, `replace` columns all non-NULL

#### Scenario: Helper leaves res unchanged for empty ids
- **WHEN** the sink is called with an empty set of touched entries (zero rows)
- **THEN** the cache is unchanged and `changes` is NULL

#### Scenario: the cache buffer is not copied by an update
- **WHEN** any closure member updates on an event whose neighborhoods are
  non-empty, on a layer of 300 or more actors
- **THEN** the cache buffer the walk holds is the same object before and
  after the call (address-identical) and the update allocates no object of
  the buffer's size

## ADDED Requirements

### Requirement: A closure statistic changes by the embeddings of the changed tie
Every closure member SHALL compute the change of its statistic at an event as
the sum, over the positions the changed tie can occupy in the member's path
shape, of the contributions of the embeddings read from the neighborhoods of
the tie's two endpoints in the state before the event is applied. Unweighted,
each embedding contributes `+1` on a creation and `-1` on a dissolution.
The touched cells (or nodes) SHALL be exactly the cells those embeddings land
on; no member SHALL recompute its statistic from a matrix product or a full
scan at an event. The two-path members, the star members, the `mixed_*`
members, `four`, `triangle`, `node_trans` and `node_triangle` SHALL each be
one row set of that enumeration, and the cache after every event SHALL equal
a from-scratch count of the shape on the current state.

#### Scenario: the two-path count is maintained from neighborhoods
- **WHEN** a tie `a → b` is created on a directed layer read by `trans`
- **THEN** the cells `(a, l)` for every out-neighbor `l` of `b` and `(k, b)`
  for every in-neighbor `k` of `a` each gain one, no other cell changes, and
  the cache equals the two-path count `(S %*% S)` of the current adjacency
  `S` at every event of a random fixture

#### Scenario: the three-path count is maintained without a matrix product
- **WHEN** a tie `a → b` is created on a layer read by `four`, one-mode or
  two-mode
- **THEN** the cells touched are those of the three-paths that use `a → b`
  as its `i → k`, `j → k` or `j → l` leg, read from the in- and
  out-neighborhoods of `a` and `b`, the `changes` rows equal what the
  product-based update emitted (order and values) on the parity fixture and
  the two-mode baseline, and no `n1 × n2` product is formed at the event

#### Scenario: a dissolution reverses the creation
- **WHEN** a tie created earlier is dissolved on a layer read by any closure
  member
- **THEN** every cell the creation touched moves back by the same amount,
  clamped at zero, and the cache equals the from-scratch count

### Requirement: The closure cache is a named list written in place
Each closure member's cache SHALL be a named list whose `stat` field holds
the untransformed count at the member's broadcast kind (a dense `n1 × n2`
double matrix for a dyad member, a length-`n1` double vector for a node
member) and, for `history = "consecutive"`, the fields `last_sender`,
`last_receiver` and `last_event_seq` as length-one vectors; no state SHALL be
carried as an attribute on the buffer. The buffer SHALL be materialized fresh
at initialization, held by exactly one binding in the walk, and written in
place through the shared kind-shaped core; the member's initialization SHALL
never return the state matrix, nor one object as both `cache` and `stat`.
The transformer SHALL be applied at emission and never stored in the buffer.

#### Scenario: no initializer aliases its buffer
- **WHEN** every closure member is initialized on an empty, a sparse, a dense
  and a two-mode network
- **THEN** the returned `cache$stat` is address-distinct from the input
  network and from the returned `stat`

#### Scenario: consecutive history rides on list fields
- **WHEN** `trans(history = "consecutive")` is preprocessed on the
  consecutive-history fixture
- **THEN** the statistic equals today's, the cache carries `last_sender`,
  `last_receiver` and `last_event_seq` as fields, and the buffer carries no
  `last_update` attribute

#### Scenario: a simulation replicate owns its buffers
- **WHEN** two replicates of `simulate()` open the walk on a specification
  with a closure member
- **THEN** the caches of the two replicates are address-distinct and each
  replicate's statistics equal a batch preprocessing of its own events

### Requirement: Node closures take the node's role as an argument
`node_trans()` SHALL accept `role = c("source", "broker", "sink")`, the
position of the node in the transitive triangle `i → k → j` with `i → j`
(source `i`, broker `k`, sink `j`), defaulting to `"source"`, which is
today's statistic. The role SHALL be resolved when the term is constructed,
and an out-of-set value SHALL abort at construction naming the effect, the
argument and the allowed values. `node_triangle()` SHALL take no `role`,
its three positions being one on an undirected network.

#### Scenario: the source role is today's node_trans
- **WHEN** `node_trans` and `node_trans(role = "source")` are preprocessed on
  the parity fixture
- **THEN** both statistics equal the pre-change `node_trans` column exactly

#### Scenario: the broker and sink roles count the same triangles at another node
- **WHEN** `node_trans(role = "broker")` and `node_trans(role = "sink")` are
  preprocessed on a random directed fixture
- **THEN** at every event each node's value equals the number of transitive
  triangles in the current state in which it is the broker (resp. the sink),
  and the three roles sum to three times the number of transitive triangles

#### Scenario: an unknown role aborts at construction
- **WHEN** `node_trans(role = "hub")` is written in a formula
- **THEN** construction aborts with a `cli` error naming `node_trans`, `role`
  and the allowed values, before any preprocessing runs

### Requirement: Undirected closures and node_triangle, triangle, degree
`node_triangle()` (rate) SHALL count, per node, the triangles of an
undirected network the node belongs to; `triangle()` (choice, REM,
coordination) SHALL count, per dyad `(i, j)`, the two-paths `i – k – j` of an
undirected network; both SHALL abort at construction on a directed network
argument. `degree()` on an undirected layer SHALL equal the node's degree,
counting each undirected tie once. A closure member, `degree()` or
`node_triangle()` built for an undirected object SHALL evaluate one way per
tie event: it runs its update on the canonical orientation of the event and
emits the complete change set for both orientations, so a mirrored second
evaluation of the same event is a no-op.

#### Scenario: node_triangle counts each triangle once per member
- **WHEN** ties `1–2`, `2–3`, `3–1` are created on an undirected layer read
  by `node_triangle`
- **THEN** nodes 1, 2 and 3 each read 1 after the third tie and 0 before it,
  and a fourth tie `1–4` leaves them unchanged

#### Scenario: triangle is the undirected two-path closure
- **WHEN** ties `1–2` and `2–3` are created on an undirected layer read by
  `triangle` in a choice formula
- **THEN** cells `(1, 3)` and `(3, 1)` read 1 and every other cell 0, equal
  to `trans` preprocessed on the same undirected layer

#### Scenario: degree counts an undirected tie once
- **WHEN** one tie `1–2` is created on an undirected layer read by `degree`
- **THEN** nodes 1 and 2 each read a degree of 1, not 2

#### Scenario: the mirrored evaluation is a no-op
- **WHEN** the walk evaluates a closure member, `degree` or `node_triangle`
  twice for one undirected tie event, once per orientation
- **THEN** the second evaluation returns no changes and leaves the cache as
  the first left it, and the recorded update stream equals the pre-change
  stream on the undirected parity fixtures

#### Scenario: triangle refuses a directed network
- **WHEN** `triangle(calls)` or `node_triangle(calls)` is written with a
  directed `calls` layer
- **THEN** construction aborts with a `cli` error naming the effect and the
  directed layer

### Requirement: Weighted closures combine a path's legs with path_fn
Every closure member SHALL accept `weighted` (logical, default `FALSE`) and
`path_fn` (a function of the path's leg weights, default `prod`). With
`weighted = FALSE` the statistic SHALL be today's count. With
`weighted = TRUE` an embedding SHALL exist only when every leg is present
(weight greater than zero) and SHALL contribute `path_fn` of its leg weights;
the statistic SHALL be the sum of contributions over embeddings, and
`transformer_fn` SHALL apply to that sum. A change of one leg from `w_old` to
`w_new` SHALL move each touched cell by the embedding's new contribution
minus its old one, an embedding that does not exist contributing zero.
`path_fn` SHALL be evaluated once per embedding at the event, never stored,
and a `path_fn` that is not a function SHALL abort at construction.

#### Scenario: unweighted equals today's count under any path_fn
- **WHEN** `trans(net, weighted = FALSE, path_fn = min)` is preprocessed
- **THEN** the statistic equals `trans(net)` exactly

#### Scenario: weighted two-paths follow the combiner
- **WHEN** a dyad `(1, 4)` has brokers 2 and 3 with `w_12 = 3, w_24 = 1,
  w_13 = 2, w_34 = 5` and `trans(net, weighted = TRUE, path_fn = f)` is
  evaluated
- **THEN** the cell reads 13 for `prod`, 3 for `min`, 11 for `sum` and 8 for
  `max`, and after `w_24` moves from 1 to 2 it reads 16, 4, 12 and 8

#### Scenario: a lone leg is not a path
- **WHEN** only `w_12 = 3` exists and `trans(net, weighted = TRUE, path_fn =
  sum)` is evaluated at `(1, 4)`
- **THEN** the cell reads 0

#### Scenario: the weighted cache equals a from-scratch sum
- **WHEN** a random weighted directed fixture is preprocessed with every
  closure member, `weighted = TRUE` and each of `prod`, `min`, `sum`, `max`
- **THEN** after every event each cache equals the from-scratch sum of
  `path_fn` over the shape's embeddings on the current state

### Requirement: The reflexive rule replaces the diagonal exclusions
A closure member's neighborhood reads SHALL exclude the changed tie's own
endpoints exactly when the model excludes self-dyads, and exclude nothing on
that account when self-dyads are allowed (`allow_reflexive`), the rule being
resolved once when the term is constructed. `four`'s distinct-node
constraints (`i ≠ j`, `k ≠ l`) SHALL hold under both settings, being part of
its shape. With self-dyads excluded every member's update stream SHALL be
byte-identical to the pre-change stream.

#### Scenario: self-dyads excluded reproduces today's stream
- **WHEN** the parity fixtures are preprocessed with self-dyads excluded (the
  default)
- **THEN** every closure member's update stream is byte-identical to the
  pre-change stream

#### Scenario: self-dyads allowed count self-loops as legs
- **WHEN** self-dyads are allowed and a self-loop `a → a` exists beside
  `a → b`
- **THEN** `trans` counts the two-path `a → a → b` at `(a, b)` and the cell
  `(a, a)` is maintained like any other, and `four` still counts no
  three-path with `i = j` or `k = l`
