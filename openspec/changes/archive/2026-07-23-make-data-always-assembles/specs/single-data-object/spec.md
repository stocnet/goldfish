# single-data-object Delta Specification

## ADDED Requirements

### Requirement: make_data() assembles a valid bundle or aborts, never an environment
`make_data()` SHALL NOT return a legacy `data.goldfish` environment for a bundle
of constructor objects: it SHALL assemble the bundle to a `stocnet`, or abort
with a `cli` error when the bundle is not assemblable. When the bundle is not
assemblable because a layer records a node-set name that resolves to no node
table in the bundle (the constructors record a node set by deparsing their
`nodes = ` argument, so `make_network(nodes = fx$actors, ...)` records the string
`"fx$actors"`), the error SHALL name the unresolved node-set reference and
instruct the user to bind that node set to a plain, resolvable name (e.g.
`actors <- fx$actors; make_network(nodes = actors, ...)`). The predicate that
decides assemblability (`is_stocnet_assemblable()`) SHALL make the unresolved
reference available to the caller so the message can name it. Node-set names
SHALL NOT be resolved by content matching. This removes the silent
environment fallback for user input; the only remaining `data.goldfish`
environments are objects saved before the 2.0.0 flip.

#### Scenario: Unresolvable node-set name aborts with guidance
- **WHEN** a covariate layer is built with `nodes = fx$actors` (recording the
  deparsed name `"fx$actors"`) and the bundle is passed to `make_data()`
- **THEN** `make_data()` aborts with an error naming the `"fx$actors"` reference
  and telling the user to bind the node set to a plain name so it resolves — it
  does NOT return a `data.goldfish` environment.

#### Scenario: Well-formed bundle still assembles
- **WHEN** the same bundle binds its covariate network's node set to a plain,
  resolvable name
- **THEN** `make_data()` assembles it to a `stocnet` and estimation proceeds.

#### Scenario: Subset-dependent flavoring is unchanged
- **WHEN** a dependent-events object that is a strict subset of its
  `default_network`'s event stream is passed to `make_data()`
- **THEN** it assembles to a `stocnet` whose focal layer is flavored by the
  dependent object's name (matched rows modeled, the rest state-only), exactly as
  before this change — the subset path is untouched.

## MODIFIED Requirements

### Requirement: Legacy environment input rejected with a conversion path
A legacy `data.goldfish` environment passed as `data` SHALL be rejected at every
public estimation surface (such an environment is obtainable only from objects
saved before the 2.0.0 flip): `make_specification()` / `estimate_dynam()` /
`estimate_rem()` / `estimate_dynami()` SHALL abort on `is.environment(data)` with
a `cli` error naming the migration — rebuild the object with `make_data()` /
`make_groups_interaction()`, both of which return a `stocnet`. The guard SHALL
sit at the public entrypoints, not in the internal `estimate_wrapper()`, which
still legitimately receives the DyNAM-i boundary bridge environment built after
the guard. This is safe because `make_data()` no longer returns an environment
for any input (see "make_data() assembles a valid bundle or aborts, never an
environment"): no public path mints one.

`as_goldfish()` conversion of a saved environment to a `stocnet` remains
**deferred** to its own change; until it lands, the migration is to rebuild via
the constructors, and the abort message says so. `as_goldfish()` SHALL continue
to abort on an environment argument.

#### Scenario: Saved environment aborts at estimation
- **WHEN** a `data.goldfish` environment restored from an `.rds` is passed to
  `estimate_dynam(..., data = old_env)`
- **THEN** the call aborts with an error telling the user to rebuild the object
  with `make_data()` / `make_groups_interaction()`.

#### Scenario: DyNAMi surface also aborts on an environment
- **WHEN** an environment is passed to `estimate_dynami(..., data = old_env)`
- **THEN** the call aborts with the same migration error — the stocnet object,
  not an environment, is the only accepted public input.

#### Scenario: Internal DyNAMi bridge environment is not rejected
- **WHEN** `estimate_dynami()` is called with a valid actors x groups `stocnet`
  and the boundary converts it to the internal bridge environment
- **THEN** estimation proceeds — the public guard ran on the stocnet, before the
  bridge environment exists, so the internal environment is never guarded.

#### Scenario: as_goldfish still defers environment conversion
- **WHEN** `as_goldfish(old_env)` is called on a legacy environment
- **THEN** it aborts (conversion is not yet available), pointing the user to
  rebuild via the constructors.
