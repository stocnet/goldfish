## ADDED Requirements

### Requirement: DyNAMi preprocessing runs through the shared recipe architecture

DyNAMi rate and choice preprocessing SHALL be produced by the shared recipe
architecture — the state container, the merged event schedule, and the compiled
update plan consumed through a recipe loop — rather than the monolithic
`preprocessInteraction` loop. The conversion SHALL reproduce the existing DyNAMi
coefficients to within the 1e-6 baseline floor, and SHALL consume the shared
`spec_map` via `preprocess(spec_map, data)` (established by
`refactor-formula-parsing`).

#### Scenario: DyNAMi rate preprocessing via the recipe loop

- **WHEN** a DyNAMi rate model is preprocessed
- **THEN** it runs through the shared recipe loop consuming `spec_map` and the
  realized state container, and reproduces the frozen DyNAMi rate baseline to
  1e-6.

#### Scenario: DyNAMi choice preprocessing via the recipe loop

- **WHEN** a DyNAMi choice model is preprocessed
- **THEN** it runs through the shared recipe loop, the DyNAMi-specific event
  handling (order correction, windowed-interaction tagging, `subType`) is applied
  within the recipe path, and it reproduces the frozen DyNAMi choice baseline to
  1e-6.
