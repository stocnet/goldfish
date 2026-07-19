# dynami-data-boundary Specification

## ADDED Requirements

### Requirement: DyNAMi data assembles to a stocnet
`make_data()` SHALL return a validated stocnet for DyNAMi components (a groups
node set from `make_groups_interaction()`, interaction/composition event
streams, dependent events) — one `nodes` tibble with a `mode`
column distinguishing actors from groups, the interaction structure as a
two-mode layer with disjoint `info$sender`/`info$receiver` mode sets — and
SHALL NOT return a `data.goldfish` environment.

#### Scenario: Constructor-built DyNAMi data is a stocnet
- **WHEN** DyNAMi data is built through `make_groups_interaction()` and
  `make_data()`
- **THEN** the result is a stamped stocnet whose actors×groups layer carries
  disjoint mode sets, and no environment is created at the public surface.

### Requirement: DyNAMi estimation accepts the stocnet object
`estimate_dynami()` SHALL accept a stocnet (raw or stamped) as `data`,
resolving the actors×groups layer through the mode map; internally it MAY
materialize the legacy environment shape for the `preprocessInteraction`
monolith, but that bridge SHALL never be exposed to or accepted from the user.

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
