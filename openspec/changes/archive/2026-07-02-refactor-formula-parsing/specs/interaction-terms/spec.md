## ADDED Requirements

### Requirement: Interaction terms parsed from : and *

The formula parser SHALL recognize R-standard interaction operators: `a:b` denotes the
interaction term only, and `a*b` SHALL expand to `a + b + a:b`. Each operand MAY be a bare
effect or an effect call with its own arguments (e.g. `global(price):outdeg(net, type = "ego")`).

#### Scenario: interaction-only term with the colon operator
- **WHEN** a formula contains `global(price):outdeg(net, type = "ego")`
- **THEN** the parser produces a single interaction term object referencing the two resolved
  operand terms, and does NOT add either operand as a separate main effect.

#### Scenario: star operator expands to main effects plus interaction
- **WHEN** a formula contains `same(dept)*recip`
- **THEN** the parser produces three terms equivalent to `same(dept) + recip + same(dept):recip`.

#### Scenario: unsupported formula constructs are rejected
- **WHEN** a formula uses a construct goldfish does not model (e.g. `I(...)` or a `|` part
  separator)
- **THEN** the parser aborts with a consistent `cli` error rather than silently admitting a
  term it cannot dispatch.
- **AND** `offset(...)` is NOT rejected — it is unwrapped and tagged as a fixed-coefficient
  term (see the `offset-fixed-terms` capability).

### Requirement: Interaction statistic is the product of its operands

An interaction term's statistic SHALL equal the elementwise product of its two operand
effects' per-dyad statistics. The interaction term's `stat_kind` and broadcast
classification SHALL be the **union of the operands' variation axes**: each broadcast kind
denotes the grid axis the statistic varies on (`global` = neither, `ego` = senders,
`alter` = receivers, point = both), and the product varies on an axis iff either operand
does. `global` is the identity (the product adopts the other operand's kind) and point is
absorbing.

#### Scenario: product statistic
- **WHEN** an interaction term `A:B` is preprocessed
- **THEN** for every (sender, receiver) the term's value equals `stat_A * stat_B` for that
  dyad.

#### Scenario: global operand is the identity for classification
- **WHEN** an interaction multiplies a `global` operand by an `alter` (or degree
  `type="alter"`) operand
- **THEN** the product is classified as `alter` (varies across receivers/alternatives); and
  `global` interacted with an `ego` operand is classified as `ego`.

#### Scenario: different single axes union to point
- **WHEN** an interaction multiplies an `ego` operand by an `alter` operand
- **THEN** the product varies across both senders and receivers and is classified as point
  (dyadic), not as either operand's single-axis kind.

#### Scenario: rate models project onto the sender axis
- **WHEN** an interaction is classified in a sender-indexed (rate) model
- **THEN** it is `global` only if both operands are `global`, otherwise a point update.
