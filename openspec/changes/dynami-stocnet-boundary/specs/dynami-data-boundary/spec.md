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

### Requirement: DyNAMi rate specification is flavor-keyed
For `model = "DyNAMi"`, `make_specification()` SHALL accept `rate` as a
flavor-keyed list (`join`/`leave` keys validated against the focal layer's
flavor values) expressing the joining and leaving rate models in the existing
flavored grammar. The boundary SHALL desugar the keyed list into the legacy
per-effect `joining = 1/-1` single-formula encoding consumed by the untouched
monolith — per-flavor `~ 1` intercepts mapped onto the legacy intercept
encoding, an effect under both keys becoming two terms — and coefficient
names SHALL render flavor labels. The `joining` flag SHALL NOT appear on the
new surface. `choice` SHALL be a plain one-sided formula denoting the joining
choice; a flavor-keyed `choice` SHALL be rejected with an error explaining
that the leaving choice is deterministic.

#### Scenario: Keyed rate equals the legacy flag formula
- **WHEN** the same DyNAMi rate model is specified as
  `rate = list(join ~ ..., leave ~ ...)` and as the legacy single formula
  with `joining = 1/-1` flags
- **THEN** both estimate identical coefficients to within 1e-6, with
  flavor-labeled names on the keyed path.

#### Scenario: Effect present in both rates
- **WHEN** an effect appears under both the `join` and the `leave` key
- **THEN** it becomes two statistics (one per rate model), matching the
  legacy double-entry idiom.

#### Scenario: Keyed choice is rejected
- **WHEN** `make_specification(model = "DyNAMi", choice = list(join ~ ...))`
  is called
- **THEN** construction aborts with an error explaining that `choice` is the
  joining choice and the leaving choice is deterministic.

### Requirement: Group availability is a derived support constraint
The public surface SHALL NOT take an `opportunities` list: the DyNAMi choice
specification SHALL auto-derive the constraint `~ indeg(<focal layer>) >= 1`
(existing support-constraint grammar — a second-mode node is in the choice set
iff it is occupied at the decision point in event order). This is Hoffman et al.
Eq. 8's denominator over the present second-mode nodes, which INCLUDES the
joiner's own singleton: an isolate may choose to stay isolated, so its own
singleton is a valid, sometimes-observed choice (excluding it would assign zero
probability to those events). The constraint SHALL be folded into the dense
maintained-availability object the estimation kernel reads (the `active_dyad`
point encoding) and SHALL NOT be fed through the internal `opportunitiesList`
channel. The dead `setopportunities_interaction()` SHALL be removed.

#### Scenario: Derived availability equals the stored opportunity list
- **WHEN** a fixture that legacy code drove with an explicit `opportunities`
  list is estimated through the stocnet boundary without one
- **THEN** the per-event derived availability equals the constructor-stored
  occupied-groups list (own singleton included).

#### Scenario: Choice reproduces the established baselines
- **WHEN** the DyNAMi choice model estimates under the derived constraint
- **THEN** coefficients match the frozen choice baselines (the established
  goldfish 1.7.0 values, own singleton included) to 1e-6, and the rate
  baselines PASS unchanged.

#### Scenario: User constraint composes with the derived one
- **WHEN** a DyNAMi choice specification also supplies a user
  `support_constraint`
- **THEN** the effective risk set is the AND of the derived constraint and
  the user constraint.
