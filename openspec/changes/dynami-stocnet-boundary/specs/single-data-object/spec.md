# single-data-object Delta Specification

## MODIFIED Requirements

### Requirement: Legacy environment input rejected with a conversion path
A legacy `data.goldfish` environment passed as `data` SHALL abort on every
estimation surface (obtainable only from objects saved before the 1.9.0 flip):
`make_specification()` / `estimate_*()` — including `estimate_dynami()` — raise
a `cli` error naming `as_goldfish()` as the migration. The DyNAMi deferral
exception recorded when `refactor-single-data-object` was archived is removed:
no model family requires an environment at the public surface any longer.
`as_goldfish()` SHALL accept such an environment and convert it: the contained
`nodes.goldfish` / `network.goldfish` / `dependent.goldfish` / global objects
and their `attr(x, "events")` streams assemble into the equivalent stocnet,
which is then validated and stamped. Legacy detection SHALL be
`is.environment(data)`; the stamped list SHALL carry class `data.goldfish`
ahead of the stocnet classes, and `print.data.goldfish()` SHALL render the
list shape.

#### Scenario: Saved environment aborts at estimation
- **WHEN** a `data.goldfish` environment restored from an `.rds` is passed to
  `estimate_dynam(..., data = old_env)`
- **THEN** the call aborts with an error telling the user to run
  `as_goldfish(old_env)` once and pass the result.

#### Scenario: DyNAMi surface also aborts on an environment
- **WHEN** an environment is passed to `estimate_dynami(..., data = old_env)`
- **THEN** the call aborts with the same `as_goldfish()` migration error — the
  stocnet boundary, not the environment, is the only accepted input.

#### Scenario: as_goldfish converts a saved environment
- **WHEN** `as_goldfish(old_env)` is called on that environment
- **THEN** it returns a stamped stocnet whose estimation reproduces the legacy
  coefficients.
