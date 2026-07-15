# closure-effects-dry Specification

## Purpose
Factor the shared two-path cache-update logic behind a single internal helper (`apply_two_path_update()` in `utils_effects_closure.R`) so the closure effect functions stay DRY and behave identically across variants.

## Requirements

### Requirement: Shared cache-update helper in utils_effects_closure.R
The package SHALL contain an unexported function `apply_two_path_update(res, ids, replace,
oldValue, transformer_fn)` in `R/utils_effects_closure.R`. All eight closure-effect update
functions (`trans`, `cycle`, `common_sender`, `common_receiver`, and the four `mixed_*`
variants) SHALL delegate their cache-write and `changes`-construction block to this helper.
No inline `replaceValues <- replace - oldValue + ...` block SHALL appear in any of these
eight functions after the refactor.

#### Scenario: Helper updates cache and changes for non-empty ids
- **WHEN** `apply_two_path_update()` is called with a non-empty `ids` matrix, `replace = 1`,
  `oldValue = 0`, `res$cache` values all equal to 2, and `transformer_fn = identity`
- **THEN** `res$cache` at the indexed positions equals 3 and `res$changes` is a matrix with
  `node1`, `node2`, `replace` columns all non-NULL

#### Scenario: Helper leaves res unchanged for empty ids
- **WHEN** `apply_two_path_update()` is called with an empty `ids` (zero rows)
- **THEN** `res$cache` is unchanged and `res$changes` is NULL

### Requirement: Cache values are clamped to zero, never filtered
The update logic SHALL use `pmax(0L, replace - oldValue + cache[ids])` to compute new cache
values. When one or more entries would become negative, they SHALL be clamped to 0 and
included in `res$changes` with `replace = 0`. The old row-filtering pattern (`if (any(...<0))
{ posValues <- ...; ids <- ids[posValues,...] }`) SHALL NOT appear anywhere in the codebase
for these eight functions.

#### Scenario: All-negative replaceValues → cache clamped, changes populated
- **WHEN** an update is computed where every `replace - oldValue + cache[ids]` entry is
  negative (e.g., delta = -1 and every cache entry is already 0)
- **THEN** `res$cache` at the affected positions is 0 (not the unmodified previous value)
  and `res$changes` is non-NULL with `replace = 0` for those entries

#### Scenario: Mixed positive and negative replaceValues → negative entries clamped
- **WHEN** some entries would be negative and others positive
- **THEN** positive entries are stored as-is and negative entries are stored as 0 in the cache;
  `res$changes` contains all affected entries

#### Scenario: All-positive replaceValues → unchanged from pre-pmax behavior
- **WHEN** all `replace - oldValue + cache[ids]` values are non-negative
- **THEN** `res$cache` and `res$changes` are identical to what the old inline block produced
