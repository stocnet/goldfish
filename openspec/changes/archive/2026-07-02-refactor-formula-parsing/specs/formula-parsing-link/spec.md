## RENAMED Requirements

- FROM: `### Requirement: Interaction effects reserved as a future extension point`
- TO: `### Requirement: Interaction terms parsed and carried through the plan`

## MODIFIED Requirements

### Requirement: Interaction terms parsed and carried through the plan

Interaction terms are no longer a reserved seam: the parser SHALL resolve each `:`/`*`
interaction into a term object that references its two operand terms, and the
plan/update-plan derivation SHALL carry that structure (operand references and the derived
broadcast/`stat_kind` classification) through to the recipe loops so the interaction
statistic can be computed as the product of its operands. The previously reserved
interaction-rendering form (`effect/obj·obj2`) SHALL be produced for interaction terms in the
compact term string and coefficient names.

#### Scenario: interaction structure reaches the recipe loops
- **WHEN** a formula with an interaction term is preprocessed
- **THEN** the compiled plan exposes the interaction's operand references and classification
  so the recipe loop computes the product statistic, rather than aborting as an unsupported
  effect.

#### Scenario: interaction rendering in names
- **WHEN** an interaction term appears in a fitted model
- **THEN** its compact term string and `coef()`/`vcov()` name use the reserved interaction
  rendering form.

## ADDED Requirements

### Requirement: Per-term perspective validity carried downstream

The parsed-formula structure SHALL carry, for each term, its perspective (`type`) and enough
context for a single validity check per `(model, sub_model)` so that invalid term or
interaction combinations are rejected with one consistent error before preprocessing runs.

#### Scenario: ego perspective recorded for a choice term
- **WHEN** a choice formula term specifies `type = "ego"`
- **THEN** the parsed structure records that perspective and the downstream validity check
  accepts it for the choice sub-model.

#### Scenario: disallowed global main effect rejected consistently
- **WHEN** a bare `global()` main effect is used in a choice sub-model
- **THEN** the validity check aborts with the established consistent `cli` error, while the
  interaction form of `global()` is permitted.

### Requirement: Specification structures are built upfront, not inside preprocessing

The compiled structures the recipe loops consume SHALL be built once as the outcome of parsing
(an upfront specification mapping), not rebuilt inside `preprocess()`. The mapping SHALL produce
a `parsed_terms` structure, a separate **effects template** (the per-effect call templates,
split out of the update plan), and an update **plan** of registries only. `preprocess()` SHALL
consume those and build only the data-derived state container and event schedule.

#### Scenario: preprocess consumes a prebuilt plan and template
- **WHEN** a model is preprocessed
- **THEN** the effects template and the update plan are taken from the upfront specification
  mapping, and `preprocess()` builds only the state container and schedule from the data.

#### Scenario: preprocessing inputs are subsumed by the mapping
- **WHEN** preprocessing is invoked from the specification mapping
- **THEN** it does not require separate `effects`, `windowParameters`, or events/objects link
  arguments — the effects template, the plan, and the per-term window in the parsed terms supply
  them.

### Requirement: The specification mapping reads metadata and creates no data

The specification mapping SHALL consult only the data objects' metadata (event-stream names,
node sets, directedness, dimensions, dynamic attributes) and SHALL NOT fetch event tables,
sanitize events to ids, fabricate windowed events or networks, or assign anything into the
caller's environment. Those data operations SHALL happen at state creation, when the data is
available, writing into the state container rather than the user's environment. The mapping
SHALL instead carry a windowed-stream recipe (the derived stream/network names and window
parameters) for state creation to realize.

#### Scenario: mapping produces no tables and mutates nothing
- **WHEN** the specification mapping is built for a formula with a windowed effect
- **THEN** its output contains no event or network tables, the caller's environment is
  unchanged, and the windowed dissolve events and windowed network are created only later, in
  the state container, during preprocessing.

### Requirement: Derived inputs are recorded as promises and realized at state creation

The mapping SHALL record each derived input (e.g. a time-windowed network an effect reads
instead of the raw object) as a promise in a `derivations` registry — `derived_name`, kind,
source object, source-stream names, parameters, and the effects that reference it — and SHALL
rewire those effects' object references to the derived name without creating any data. State
creation SHALL realize each promise by dispatching on its kind into the state container. The
registry SHALL be extensible to further kinds (the disabled `ignore_repetitions` view is the next
intended consumer and is not realized by this change).

#### Scenario: windowed input realized from a promise
- **WHEN** a formula has a windowed effect
- **THEN** the mapping records a `window` derivation promise and points the effect at the derived
  network name, and state creation builds the derived network and its windowed event streams into
  the state container.

### Requirement: Print and naming metadata is owned by the specification mapping

The specification mapping SHALL produce the effect print/naming metadata
(`effect_description`) once as the single source of truth, covering interaction terms and each
effect's role and estimated status. Context-specific short names (console, database, export)
SHALL be rendered from that metadata on demand, so coefficient names, gathered-column names,
and database column names are consistent.

#### Scenario: one naming source across surfaces
- **WHEN** a model is printed, gathered, and exported to a database
- **THEN** the effect names in each surface are rendered from the same `effect_description`
  metadata produced by the specification mapping, differing only by context-specific length.

#### Scenario: effects template is separate from the plan
- **WHEN** the specification mapping is built
- **THEN** the per-effect call templates are returned as a distinct effects-template object,
  and the update plan contains registries only (no templates).

### Requirement: Plan carries interaction links and materialized-state needs

The update plan SHALL record, for each effect, its role (main, operand, or interaction) and
whether it is estimated; for each interaction, the ordered set of operand effect ids
(supporting interactions of any order); the reverse operand→interaction map used to route
operand updates to their interactions; and which effects require a materialized current value
(`stat_state`) during the loop.

#### Scenario: n-ary interaction operands recorded
- **WHEN** a formula contains an interaction of three or more effects
- **THEN** the plan records all operand effect ids for that interaction and routes a change in
  any operand to recompute the interaction.

#### Scenario: operand kept but not estimated
- **WHEN** an effect appears only inside an interaction and not as a main effect
- **THEN** it is recorded with `estimate = FALSE`, retained in the preprocessed object for
  downstream analysis, and excluded from the estimated coefficient set.

### Requirement: Plan carries a formula link for the multivariate seam

The plan SHALL carry a `formula_effects` link of rows `(fid, lid, gid)` — formula id, local
effect index, unique effect id. For a single formula `fid` SHALL be 1; the structure SHALL allow
multiple formulas to share a `gid` (a cross-process effect computed once) for future multivariate
preprocessing.

#### Scenario: single-formula formula link
- **WHEN** a single-formula model is compiled
- **THEN** every effect has `fid = 1` and a local index `lid`, with a unique `gid`.
