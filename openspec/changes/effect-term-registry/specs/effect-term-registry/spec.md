## ADDED Requirements

### Requirement: Declarative term registry with a complete term definition
The package SHALL maintain an internal registry mapping each effect's canonical
name to a single `term_def` object that fully defines the term. Each `term_def`
SHALL carry, at minimum, the following groups of elements:

- **Identity & taxonomy**: `name` (canonical), `family` (hierarchical group),
  `label`, `description`.
- **Presentation metadata**: `short` (compact summary name), `abbrev`
  (coef/print abbreviation), `aliases` (additional lookup/search names).
- **Endogenous/exogenous pairing**: `kind` (`endogenous`/`exogenous`), `twin`
  (the paired term name), and for endogenous terms an `object_default` rule.
- **Recipes**: `init` (initializer reference), `update` (per-event update
  reference), and `variants` (the `(model, sub_model)` combinations supported).
- **Validity constraints**: required object kind(s), allowed directionality,
  allowed mode (one-/two-mode), allowed `models`/`sub_models`, interaction
  validity, and reflexive-diagonal meaningfulness.
- **Argument schema**: per accepted argument, its name, type/allowed values,
  default, validation rule, the error to raise on misuse, and an encode hook.

Lookup `get_term_def(name)` SHALL resolve a term by its canonical name or any of
its `aliases`.

#### Scenario: Registry entry carries every defined element
- **WHEN** `get_term_def("inertia")` is called
- **THEN** the returned `term_def` exposes non-empty identity, presentation,
  pairing, recipe, validity, and argument-schema groups

#### Scenario: Lookup resolves canonical name and aliases
- **WHEN** a term is registered with `name = "trans"` and an alias
- **THEN** `get_term_def()` returns the same `term_def` for the canonical name
  and for the alias

#### Scenario: Unknown name reports suggestions
- **WHEN** `get_term_def()` is called with a name that is neither a canonical
  name nor an alias
- **THEN** an error is raised that lists close registered names as suggestions

### Requirement: Endogenous/exogenous pairing is declared as data
The registry SHALL express the endogenous/exogenous relationship between paired
terms (for example `inertia`↔`tie`, `trans`↔`closure_ff`) through the `kind`,
`twin`, and `object_default` fields rather than one initializer calling another.
The endogenous member SHALL default its object to the dependent network layer;
the exogenous member SHALL require an explicit object.

#### Scenario: Endogenous term paired to its exogenous twin
- **WHEN** the `inertia` and `tie` entries are inspected
- **THEN** `inertia.kind` is endogenous with `twin = "tie"` and an
  `object_default` to the dependent network, and `tie.kind` is exogenous with
  `twin = "inertia"`

### Requirement: Presentation metadata is sourced from the registry
The display and export surfaces SHALL source an effect's short name,
abbreviation, and family from its `term_def` rather than from a separate
hard-coded map. This covers the compact term-string builder, `GetDetailPrint()`,
and the `coef()`/`vcov()`/`tidy()`/export name surfaces. The previously
hard-coded `.goldfishEffectShort` map SHALL be removed once the registry supplies
the same values.

#### Scenario: Builder reads short names from the registry
- **WHEN** a fitted model is summarized with compact term strings
- **THEN** each effect's short form equals its `term_def$short` (and equals the
  value the prior hard-coded map produced for existing effects)

#### Scenario: Hard-coded abbreviation map is gone
- **WHEN** the source tree is searched for `.goldfishEffectShort`
- **THEN** no definition remains; the abbreviations live in registry entries

### Requirement: Public effect registration API
The package SHALL export and document a stable public API to register and
inspect effects: `register_term()` to add a `term_def`, plus accessors to
retrieve, list, and search registered terms. `register_term()` SHALL validate
the supplied definition and reject a malformed `term_def` with a clear error
identifying the offending field. The package's own effects SHALL register
through this same API.

#### Scenario: Custom effect registered and usable
- **WHEN** a user calls `register_term()` with a well-formed custom `term_def`
- **THEN** the term is retrievable via the accessors and usable in a formula

#### Scenario: Malformed definition rejected
- **WHEN** `register_term()` is called with a `term_def` missing a required
  field or with an invalid validity/argument declaration
- **THEN** registration fails with an error naming the offending field and no
  partial entry is added

#### Scenario: Listing and search
- **WHEN** the listing/search accessors are queried by family or alias
- **THEN** they return the matching registered terms

### Requirement: Registry covers all model families
The registry SHALL contain one entry for every effect currently implemented
across the DyNAM (rate/choice/choice_coordination), REM (rate/choice/
rate_ordered), and DyNAMi (rate/choice) families, so that effect resolution
never falls back to a string-built lookup.

#### Scenario: Every implemented effect has an entry
- **WHEN** the set of implemented `init_*`/`update_*` effects is enumerated
- **THEN** each one has a corresponding registry entry whose `variants` include
  its `(model, sub_model)` combinations
