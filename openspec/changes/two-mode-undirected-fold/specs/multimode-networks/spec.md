## MODIFIED Requirements

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
`directed` SHALL be accepted on a two-mode layer, either value, without an
abort, warning or note, and the layer SHALL NOT be read as symmetric whatever
it declares (see the `single-data-object` requirement "Layer symmetry resolved
once on the mode map"), so mask symmetrization SHALL NOT apply.

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
