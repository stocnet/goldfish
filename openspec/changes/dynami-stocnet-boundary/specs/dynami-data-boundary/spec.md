# dynami-data-boundary Specification

## ADDED Requirements

### Requirement: DyNAMi data assembles to a stocnet
`make_groups_interaction()` SHALL return the assembled multipartite stocnet
directly (**BREAKING**: the previous 5-component list, including the
`opportunities` component, is retired): one `nodes` tibble with `mode`
distinguishing actors from groups; a focal two-mode `interactions` layer with
disjoint `info$sender`/`info$receiver` mode sets holding the dependent AND
exogenous join/leave events, the construction's `order` attributes carried as
the reserved `order` column, dependent joins stamped `flavor = "join"`,
dependent leaves `flavor = "leave"`, and exogenous rows `flavor = NA`
(state-only); and the one-mode past-interaction covariate layer. The
records→events transformation (seeded randomization, group assignment, fake
intermediary singletons, dependent/exogenous split) SHALL be unchanged.
`make_data()` with DyNAMi components SHALL likewise assemble to a stocnet and
SHALL NOT return a `data.goldfish` environment.

#### Scenario: Constructor-built DyNAMi data is a stocnet
- **WHEN** DyNAMi data is built through `make_groups_interaction()`
- **THEN** the result is a stamped multipartite stocnet whose actors×groups
  layer carries disjoint mode sets, join/leave flavors on dependent rows,
  `NA` flavor on exogenous rows, and the `order` column preserving the
  construction's total event order; no environment is created at the public
  surface.

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

### Requirement: Group availability is a derived occupancy constraint
The public surface SHALL NOT take an `opportunities` list: the DyNAMi choice
specification SHALL auto-derive the occupancy constraint
`~ indeg(<focal layer>) >= 1` (existing support-constraint grammar — a group
is in the choice set iff occupied at the decision point in event order,
including the joiner's own intermediary singleton, reproducing the
established estimation exactly), AND-combined with any user
`support_constraint`. The estimation SHALL feed the derived per-event
availability through the existing internal `opportunitiesList` channel; no
engine change. The dead `setopportunities_interaction()` SHALL be removed.

#### Scenario: Derived availability equals the stored list
- **WHEN** a fixture that legacy code drove with an explicit `opportunities`
  list is estimated through the stocnet boundary without one
- **THEN** the per-event derived availability equals the constructor-stored
  list (own singleton included) and the coefficients match to 1e-6.

#### Scenario: User constraint composes with the derived one
- **WHEN** a DyNAMi choice specification also supplies a user
  `support_constraint`
- **THEN** the effective risk set is the AND of the derived occupancy
  constraint and the user constraint.
