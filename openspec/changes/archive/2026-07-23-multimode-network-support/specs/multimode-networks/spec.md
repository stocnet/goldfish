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

### Requirement: Effect validity derived per argument from the mode map

The package SHALL validate effect formulas per argument position against the
mode map: two-modeness resolves from each network argument's **own** layer
(never as a blanket from the focal layer), the user-fed `is_two_mode` argument
is validated against it — a disagreement SHALL raise a `cli` warning naming the
effect, the declared value, and the layer's actual mode pair, with the mode
map's reading used — and effects without an `is_two_mode` formal SHALL receive
the same parse-time validation. Conformability SHALL be decided by **mode-set
identity** (never by dimension equality), including the chain rule for
mixed-effect argument lists. Statistics that are structurally constant for a
type variant (a type-resolved read of a side the argument cannot have, e.g.
`indeg(type = "ego")` on a same-pair two-mode argument) SHALL be rejected with
a `cli` error naming the effect, the type value, and the layer, listing valid
alternatives.

On a two-mode focal layer the package SHALL accept `inertia`/`tie`,
`indeg(type = "alter")`, `outdeg(type = "ego")`, four-cycle closure (`four`),
`ego` (sender-side slice) and `alter` (receiver-side slice) of the single
`nodes` tibble, `same`/`diff`/`sim`/`ego_alter_interaction` subject to
attribute definedness on both sides, `tertius(type = "alter")` and
`tertius_diff` subject to definedness on the aggregated side, mixed-family
effects whose argument chains conform by mode sets, and `global`; and SHALL
reject `recip` (a reverse-pair argument is a documented reserved seam),
`trans`/`cycle`/`node_trans`/`triangle` (square arguments over identical
sides), `common_sender`/`common_receiver` (their two-mode reading requires a
one-mode focal — see below), degenerate type variants, and non-conforming
mixed chains. `common_sender`/`common_receiver` with a two-mode covariate on a
**one-mode focal** layer (shared-affiliation projection) SHALL be supported.
`directed` SHALL be treated as vacuous on a two-mode layer (noted and
ignored), and mask symmetrization SHALL NOT apply.

Each attribute read SHALL require the attribute to be not entirely missing on
the mode slice that position reads; a violation aborts naming the attribute,
the mode(s), and the effect. The gate SHALL be "no values at all on that
slice", not "any missing value" — partial missingness remains imputation's
concern — and SHALL be evaluated and reported **per mode**, so that a side
declared over several modes reports which of its modes carries no values.

#### Scenario: A side spanning several modes reports the empty mode

- **WHEN** a focal layer's receiver side is declared over modes `event` and
  `org`, and `capacity` has values for `event` nodes and is `NA` for every
  `org` node
- **THEN** the abort names `capacity`, the effect, and `org` specifically —
  not the receiver side as a whole.

#### Scenario: Declared is_two_mode disagrees with the mode map
- **WHEN** a formula supplies `indeg(net, is_two_mode = FALSE)` and `net` is a
  two-mode layer under the object's mode map
- **THEN** a `cli` warning names the effect, the declared value, and the
  layer's mode pair, and the mode map's reading is used.

#### Scenario: A one-mode-only effect on a two-mode layer errors

- **WHEN** a formula uses `recip(attend)` on a two-mode layer `attend`
- **THEN** the package aborts naming `recip` and `attend` and lists the effects
  valid on a two-mode layer.

#### Scenario: A two-mode-valid effect resolves per side

- **WHEN** a formula uses `ego(size) + alter(members)` on a two-mode focal layer
- **THEN** `ego(size)` reads the sender-side slice and `alter(members)` the
  receiver-side slice of the single `nodes` tibble, and estimation proceeds.

#### Scenario: A degenerate type variant is rejected; a conforming covariate is not

- **WHEN** a rate formula uses `indeg(attend, type = "ego")` on the two-mode
  focal layer `attend`, and separately `indeg(coauthor)` where `coauthor` is a
  one-mode layer over the sender mode
- **THEN** the first aborts naming `indeg`, the type value, and `attend` as
  structurally zero (senders never receive on `attend`) and lists valid
  alternatives, while the second is accepted.

#### Scenario: An attribute missing for a required mode errors

- **WHEN** a formula uses `sim(experience)` on a two-mode focal layer whose
  receiver-mode nodes are all `NA` on `experience`
- **THEN** the package aborts naming `experience`, the receiver mode, and
  `sim`, before estimation.

#### Scenario: Shared-partner effects require a one-mode focal

- **WHEN** `common_receiver(affiliations)` is used with a two-mode
  `affiliations` covariate on a one-mode focal layer, and the same effect is
  used on a two-mode focal layer
- **THEN** the one-mode-focal model estimates (shared-affiliation counts), and
  the two-mode-focal formula aborts naming the effect and the focal layer.

#### Scenario: Equal-sized distinct modes do not conform

- **WHEN** a mixed effect's argument chain requires mode-set conformability and
  the supplied layers' adjacent mode sets are distinct modes that happen to
  have the same number of nodes
- **THEN** the package aborts on the mode-set mismatch rather than accepting the
  dimension match.

### Requirement: Nodal state is keyed by the node space it lives on

Nodal attribute state SHALL be keyed by the **mode set** it is read on —
canonicalized so that two layers declaring the same side resolve to the same
key and share one view — rather than by a fixed pair of node-set identifiers,
and the number of nodal node spaces SHALL NOT be capped at two. Every nodal
attribute event stream SHALL be remapped into the local index space of each
view it updates, as composition and network streams already are; a nodal event
SHALL NOT reach the state walk carrying a global node id.

Attribute positions SHALL resolve per effect: the dyad-position effects
(`ego`, `alter`, `same`, `diff`, `sim`, `ego_alter_interaction`) against the
focal layer's sender and receiver sides, and the neighbor-aggregate effects
(`tertius`, `tertius_diff`) against **their own network argument's** sender
side. Where an effect's two positions resolve to the same node space, it SHALL
receive a single attribute vector and behave exactly as a one-mode model does
today.

#### Scenario: A time-varying covariate updates the right node on either side

- **WHEN** a two-mode focal layer `attend` (`actor` → `event`) has a model
  including `alter(size)`, and `size` changes for an `event`-mode node
- **THEN** the statistic updates that event's column, rather than aborting or
  writing at the node's global position in a side-local vector.

#### Scenario: Layers sharing a side share one view

- **WHEN** an object has `attend` (`actor` → `event`) and `member`
  (`actor` → `org`), and a model reads `ego(size)`
- **THEN** the `actor` node space resolves to a single keyed view used by both
  layers, so the attribute is stored once and a change to it is applied once.

#### Scenario: A neighbor-aggregate effect reads its own argument's side

- **WHEN** the focal layer is `actor` → `event` and `tertius(w, z)` is used with
  a covariate layer `w` running `org` → `event`
- **THEN** `z` is read on the `org` mode — the sender side of `w` — and not on
  the focal layer's `actor` side.

#### Scenario: A two-mode rate model estimates

- **WHEN** a DyNAM rate model is specified over a two-mode focal layer whose
  network is non-empty
- **THEN** the model's receiver-side size is the focal layer's receiver side and
  estimation proceeds, rather than collapsing the receiver side onto the sender
  side.

### Requirement: Attribute imputation respects mode slices

Missing-value imputation for nodal attributes SHALL compute within each mode
slice of the side the reading effect resolves to — never pooled across the
modes of the fused `nodes` tibble — and the imputation warning SHALL name the
attribute and the mode(s) imputed. This SHALL hold on layers whose side spans
several modes (an undeclared one-mode layer over a multi-mode object) and for
cross-side attribute reads.

#### Scenario: Undeclared one-mode layer does not pool imputation across modes

- **WHEN** a three-mode object has an undeclared one-mode layer, and a numeric
  attribute has `NA` for a node of mode `employee` while other modes carry
  systematically different values
- **THEN** the imputed value is the mean over `employee`-mode nodes only, and
  the warning names the attribute and the mode.

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
