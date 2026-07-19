## ADDED Requirements

### Requirement: Multipartite object with dyadic layers over mode pairs

The package SHALL support a stocnet object carrying any number of `nodes$mode`
values, where each layer is a dyad over a (sender-set, receiver-set) pair of
modes (one-mode when identical, two-mode when disjoint, per the mode-map
identical-or-disjoint rule), and a single model is over exactly one focal layer's
side pair. The engine's two local index spaces (n1×n2) SHALL be unchanged; other
layers MAY declare different mode pairs and enter as exogenous covariates. A
single process spanning three or more modes at once SHALL NOT be modeled — a
documented reserved seam only.

#### Scenario: One object mixes layers over different mode pairs

- **WHEN** a stocnet has modes `{actor, event, org}` with a two-mode focal layer
  `attend` (`actor` → `event`), a one-mode covariate layer `coauthor`
  (`actor` → `actor`), and a two-mode covariate layer `member` (`actor` → `org`)
- **THEN** the model over `attend` runs on an n_actor × n_event index space, and
  `coauthor` / `member` are read as exogenous covariates, each mapped through its
  own mode pair.

#### Scenario: A three-plus-mode single process is rejected

- **WHEN** a specification would require one process over three modes at once
- **THEN** the package aborts noting that a model is over one dyadic side pair and
  points at the reserved multipartite-process seam as future work.

### Requirement: Mode map is the canonical two-mode representation

The mode map SHALL be the one internal two-mode representation consumed
downstream: the single `nodes` tibble plus a `mode` column and per-layer
`info$sender`/`info$receiver` mode sets. Legacy two node-set input (`nodes` +
`nodes2` as distinct objects) SHALL be translated onto the mode map at the
boundary and SHALL NOT reach any downstream branch as two separate node-set
objects; the two node-set input surface SHALL deprecate with the legacy
constructors.

#### Scenario: Two node-set input is translated to one tibble plus modes

- **WHEN** two-mode data is supplied through the legacy two node-set constructors
- **THEN** it is fused into a single `nodes` tibble whose `mode` column
  distinguishes the two source sets, with `info$sender`/`info$receiver` mode sets
  derived per layer, and downstream code sees only the mode-map representation.

### Requirement: Legacy make_data assembles two-mode input into a stocnet

`make_data()` (and the legacy constructor wrappers) SHALL assemble two-mode input
into a `stocnet` rather than a `data.goldfish` environment:
`is_stocnet_assemblable()` SHALL accept two-mode bundles, and the assembler SHALL
build the fused `nodes` tibble with a `mode` column, remap each layer's
`from`/`to` into the fused id space, set `info$sender`/`info$receiver` mode sets
per layer, and route composition/attribute events by mode into
`active_mode1`/`active_mode2`. The result SHALL flow through the same stocnet path
as directly-constructed two-mode input.

#### Scenario: make_data two-mode returns a stocnet, not an environment

- **WHEN** a model is built with `make_network(m, nodes = firms, nodes2 = unis)`,
  a two-mode `make_dependent_events(...)`, and `make_data(...)`
- **THEN** `make_data()` returns a `stocnet` (a list of tibbles) whose focal layer
  is two-mode under the mode map, and no `data.goldfish` environment is produced.

#### Scenario: Two-mode is removed as a legacy-environment producer

- **WHEN** the two-mode assembly is in place
- **THEN** the only remaining producer of a legacy `data.goldfish` environment is
  DyNAMi; the single-object legacy-environment abort remains deferred until DyNAMi
  is also off the environment (this change does not implement the abort).

### Requirement: Effect-validity contract on a two-mode layer

Each effect SHALL carry a validity flag for two-mode layers. On a two-mode
(disjoint-sides) layer the package SHALL accept the effects with a well-defined
two-mode reading — dyadic memory (`inertia`, `tie`), per-side degree
(`indeg`/`outdeg`), four-cycle closure, and attribute effects (`ego`/`alter`/
`same`/`diff`, with `ego` reading the sender-side slice and `alter` the
receiver-side slice of the single `nodes` tibble) — and SHALL reject one-mode-only
effects (reciprocity, one-mode triadic closure such as `trans`/`cycle`, and any
effect assuming a square/symmetric adjacency) with a `cli` error naming the effect
and the layer and listing valid alternatives. `directed` SHALL be treated as
vacuous on a two-mode layer (noted and ignored), and mask symmetrization SHALL NOT
apply.

#### Scenario: A one-mode-only effect on a two-mode layer errors

- **WHEN** a formula uses `recip(attend)` on a two-mode layer `attend`
- **THEN** the package aborts naming `recip` and `attend` and lists the effects
  valid on a two-mode layer.

#### Scenario: A two-mode-valid effect resolves per side

- **WHEN** a formula uses `ego(size) + alter(members)` on a two-mode focal layer
- **THEN** `ego(size)` reads the sender-side slice and `alter(members)` the
  receiver-side slice of the single `nodes` tibble, and estimation proceeds.

### Requirement: Two-mode side pair and node identity on the surface

The specification/estimation surface SHALL resolve the model's row and column node
spaces (`nodes`/`nodes2`) from the focal layer's mode map
(`side1`/`side2`) rather than from two node-set names, and SHALL accept
multipartite objects whose covariate layers declare different mode pairs than the
focal layer. The `node_lookup` (side, local index, global id, label) SHALL carry
onto two-mode preprocessed/estimation results and gather/db exports so
`index_i`/`index_j` resolve to original node identity.

#### Scenario: The model's side pair comes from the focal layer's mode map

- **WHEN** `make_specification(..., layer = "attend", data = x)` runs on a
  multipartite `x` whose `attend` layer is two-mode
- **THEN** the specification's row/column node sets are the focal layer's
  sender-side and receiver-side node spaces from the mode map, and the print shows
  the two-mode side pair.

#### Scenario: Two-mode export resolves index_i/index_j to labels

- **WHEN** a two-mode model's gather/db export is produced
- **THEN** its `index_i` joins the node lookup's `side == 1` rows and `index_j` the
  `side == 2` rows back to the original `nodes` labels.

### Requirement: Flagship two-mode dataset docs and vignette

The package SHALL document the two-mode workflow on `manynet::irps_nuclear`
(Haunss & Hollway 2023): a dedicated precompiled vignette demonstrating the
mnet → stocnet conversion (nodes with `mode`, a two-mode layer with disjoint
sender/receiver mode sets, ±1 increments as creation/dissolution flavors) and a
paper-inspired two-mode DyNAM, plus a multipartite section in the
`goldfish_data` construction reference. goldfish SHALL NOT ship its own copy of
the data (manynet is in Imports); the two-mode coefficient baseline SHALL run on
a frozen subset stored under `tests/`, insulated from upstream data revisions.

#### Scenario: The two-mode vignette loads and models cleanly

- **WHEN** the vignette loads `data(irps_nuclear, package = "manynet")`,
  converts it to a stocnet, and estimates a model on its focal two-mode layer
- **THEN** the converted object validates as a plain stocnet, the model
  estimates, and no deprecation warning is emitted.

### Requirement: Two-mode coefficient equivalence with the legacy path

A two-mode model built as a mode-map stocnet SHALL produce the same coefficients
(to within 1e-6, both engines) as the same model built through the legacy two
node-set constructors (now assembling to stocnet), and a mixed one/two-mode-layer
object SHALL estimate consistently. Frozen one-mode DyNAM/REM baselines SHALL
remain PASS (not SKIP) under `NOT_CRAN=true`.

#### Scenario: Stocnet and legacy two-mode paths agree

- **WHEN** the same two-mode model is estimated via the mode-map stocnet path and
  via the legacy two node-set constructors
- **THEN** the coefficients agree to within 1e-6 for both the `default` and
  `default_c` engines.
