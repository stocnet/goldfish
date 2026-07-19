# dynami-data-boundary Specification

## ADDED Requirements

### Requirement: DyNAMi data assembles to a stocnet
`make_data()` SHALL return a validated stocnet for DyNAMi components (a groups
node set from `make_groups_interaction()`, interaction/composition event
streams as object components, dependent events) — one `nodes` tibble with a `mode`
column distinguishing actors from groups, the interaction structure as a
two-mode layer with disjoint `info$sender`/`info$receiver` mode sets — and
SHALL NOT return a `data.goldfish` environment.

#### Scenario: Constructor-built DyNAMi data is a stocnet
- **WHEN** DyNAMi data is built through `make_groups_interaction()` and
  `make_data()`
- **THEN** the result is a stamped stocnet whose actors×groups layer carries
  disjoint mode sets, and no environment is created at the public surface.

### Requirement: DyNAMi estimation accepts the stocnet object
`estimate_dynami()` AND `make_specification()` SHALL accept a stocnet (raw or
stamped) as `data` for DyNAMi models, resolving the actors×groups layer
through the mode map; internally the estimation MAY materialize the legacy
environment shape for the `preprocessInteraction` monolith, but that bridge
SHALL never be exposed to or accepted from the user.

#### Scenario: Specification path works for DyNAMi
- **WHEN** `make_specification(..., model = "DyNAMi", data = <stocnet>)` is
  built and passed to `estimate_dynami()`
- **THEN** the model estimates identically to the direct-formula call.

#### Scenario: Stocnet input estimates a DyNAMi model
- **WHEN** `estimate_dynami(..., data = <stocnet>)` is called on the
  actors×groups object
- **THEN** the model estimates and reproduces the legacy constructor-path
  coefficients to within 1e-6 on the frozen DyNAMi baselines.

#### Scenario: Bridge equivalence with the constructor path
- **WHEN** the same DyNAMi fixture is built via the legacy constructors and via
  the stocnet boundary
- **THEN** the environment components the monolith reads are exactly equal on
  both paths.

### Requirement: Group availability is derived, not supplied
The public surface SHALL NOT take an `opportunities` list: the choice set —
the groups available when an actor receives a join opportunity — SHALL be
derived from the object's composition/interaction state at event time through
the availability (support-constraint) machinery, combinable with any user
`support_constraint`; the internal bridge SHALL materialize the monolith's
`opportunities` list from that derived availability.

#### Scenario: Derived availability equals a supplied list
- **WHEN** a fixture that legacy code drove with an explicit `opportunities`
  list is estimated through the stocnet boundary without one
- **THEN** the bridge-materialized list equals the constructor-supplied list
  and the coefficients match to 1e-6.
