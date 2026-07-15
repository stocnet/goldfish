# interaction-terms Specification

## Purpose
TBD - created by archiving change refactor-formula-parsing. Update Purpose after archive.
## Requirements
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

### Requirement: Interaction statistic in sender-indexed models

An interaction term's statistic in a sender-indexed (DyNAM `rate` / `rate_ordered`) model SHALL
equal the per-sender elementwise product of its operand effects' per-sender statistics. The
interaction's broadcast classification SHALL follow the collapsed rate lattice: the product is
`global` iff **every** operand is `global`, otherwise it varies per sender (point). Operands SHALL
be retained in the design; an operand that is not itself a requested main effect is kept but held
out of estimation (fixed coefficient), as in the dyad case.

#### Scenario: per-sender product statistic
- **WHEN** an interaction term `A:B` is preprocessed in a DyNAM `rate` model
- **THEN** for every sender the term's value equals `stat_A * stat_B` for that sender.

#### Scenario: product of two sender covariates
- **WHEN** a rate formula contains `ego(age):ego(sex)`
- **THEN** the interaction column is the per-sender product of the two actor covariates and is
  estimated, while the bare operands follow the formula's `:`/`*` semantics.

#### Scenario: global operand is the identity on the sender axis
- **WHEN** a rate interaction multiplies a `global` operand by a per-sender (`ego`) operand
- **THEN** the product varies per sender (classified point), not global.

#### Scenario: all-global product stays global
- **WHEN** every operand of a rate interaction is `global`
- **THEN** the product is classified `global` (a per-time scalar).

### Requirement: rate interaction operands must vary on the sender axis

An operand of a sender-indexed interaction SHALL vary on the sender axis: `ego`-perspective
effects, degree `type = "ego"`, and `global` are permitted; `alter`-perspective and dyadic
operands SHALL be rejected with a single consistent `cli` error, because a sender-indexed model
has no receiver axis for them to vary on. `global` SHALL be permitted as an **operand** even in
`rate_ordered`, where it is disallowed as a bare main effect, because interacting it with a
sender-varying operand restores identified per-sender variation.

#### Scenario: alter operand rejected in a rate interaction
- **WHEN** a rate formula contains `alter(x):ego(y)`
- **THEN** preprocessing aborts with an error identifying the `alter` operand as invalid for a
  sender-indexed model.

#### Scenario: global operand permitted in a rate_ordered interaction
- **WHEN** a `rate_ordered` formula contains `global(policy):ego(seniority)`
- **THEN** the interaction is computed and estimated even though a bare `global()` main effect is
  rejected in `rate_ordered`.

### Requirement: DyNAMi interactions remain unsupported

Interaction terms in DyNAMi models SHALL continue to abort with a clear "not yet supported"
message; DyNAMi's interaction support arrives with the dedicated DyNAMi engine change.

#### Scenario: DyNAMi interaction is rejected
- **WHEN** an interaction formula is passed to `estimate_dynami()`
- **THEN** it aborts with a message directing the user to the supported model families.

