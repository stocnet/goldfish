# global-effects-rate Specification

## Purpose
Make the `global()` effect valid in DyNAM-rate formulas via `init_` / `update_DyNAM_rate.global`, broadcasting a global covariate across the sender set.

## Requirements

### Requirement: global() effect works in DyNAM-rate formulas
The `global(df$col)` effect SHALL be valid in DyNAM-rate model formulas. `init_DyNAM_rate.global` SHALL accept a scalar `attribute` (the value of `df$col`) and return `list(stat = rep(attribute, n1))` with no cache. `update_DyNAM_rate_global` SHALL return 2-column changes (`node1`, `replace`) covering all n1 actors when the scalar value changes, consistent with DyNAM-rate change format conventions.

#### Scenario: Initialization with scalar value
- **WHEN** `init_DyNAM_rate.global` is called with `attribute = 0.5` (scalar) and `n1 = 4`
- **THEN** the returned `stat` is `c(0.5, 0.5, 0.5, 0.5)` and there is no `cache` element

#### Scenario: Update broadcasts to all actors — 2-column format
- **WHEN** `update_DyNAM_rate_global` is called with `replace = 1.0` and `n1 = 4`
- **THEN** `changes` has 4 rows with columns `node1` and `replace` only (no `node2` column), all `replace` values equal `1.0`

#### Scenario: No update when value unchanged
- **WHEN** `update_DyNAM_rate_global` is called with `attribute = 1.0` and `replace = 1.0`
- **THEN** `changes` is `NULL`

#### Scenario: Full DyNAM-rate preprocessing with global effect
- **WHEN** a formula `depEvents ~ global(seasons$winter)` is preprocessed with DyNAM-rate
- **THEN** preprocessing completes without error and the stat column has all actor rows sharing the same value equal to `seasons$winter`

---

### Requirement: global() effect works in REM formulas via init_REM_choice.global
The `global(df$col)` effect SHALL be valid in REM model formulas. `init_REM_choice.global` SHALL call `init_DyNAM_rate.global` and expand the length-n1 stat vector to an n1×n2 stat matrix (diagonal zero for one-mode). `update_REM_choice_global` SHALL call `update_DyNAM_rate_global` and apply `to_ego()` to produce 3-column dyadic changes (`node1`, `node2`, `replace`).

#### Scenario: Initialization in REM produces n1×n2 matrix
- **WHEN** `init_REM_choice.global` is called with `attribute = 2.0`, `n1 = 3`, `n2 = 3`
- **THEN** the returned `stat` is a 3×3 matrix with off-diagonal entries equal to `2.0` and diagonal equal to `0`

#### Scenario: REM update produces 3-column dyadic changes
- **WHEN** `update_REM_choice_global` is called with `replace = 1.0`, `n1 = 3`, `n2 = 3`, `is_two_mode = FALSE`
- **THEN** `changes` has 3*(3-1) = 6 rows with columns `node1`, `node2`, `replace`

#### Scenario: Full REM preprocessing with global effect
- **WHEN** a formula `depEvents ~ global(seasons$winter)` is preprocessed with REM
- **THEN** preprocessing completes without error and the stat matrix reflects the current global value for all actor pairs

---

### Requirement: global() effect documentation added to goldfishEffects vignette
The `vignettes/goldfishEffects.Rmd` SHALL contain a section documenting the `global()` effect under the DyNAM-rate attribute effects subsection, covering: formula syntax, mathematical definition (scalar broadcast), initialization behavior, and update behavior.

#### Scenario: Vignette section present
- **WHEN** `vignettes/goldfishEffects.Rmd` is inspected
- **THEN** it contains a section for `global()` with formula usage and a description of the scalar broadcast semantics
