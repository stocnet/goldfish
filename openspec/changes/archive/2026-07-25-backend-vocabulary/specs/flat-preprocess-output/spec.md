# flat-preprocess-output (delta)

Wording-only: the compute backends a user selects are named by their `backend`
value. "Engine-native `initialStats`" keeps its phrasing — it describes the
storage form the implementation consumes, not a user choice.

## MODIFIED Requirements

### Requirement: Coefficient reproducibility across refactor
The refactored pipeline SHALL produce ML coefficient estimates identical to the pre-refactor pipeline to within 1e-6 absolute tolerance for all existing test datasets (Social_Evolution, Fisheries) and both compute backends (`backend = "r"`, `backend = "cpp"`).

#### Scenario: DyNAM-rate coefficients are unchanged
- **WHEN** `estimate_dynam()` is run with `backend = "r"` and `backend = "cpp"` on Social_Evolution data before and after the refactor
- **THEN** all coefficients agree to within 1e-6

#### Scenario: DyNAM-choice coefficients are unchanged
- **WHEN** `estimate_dynam()` with subModel="choice" is run before and after the refactor
- **THEN** all coefficients agree to within 1e-6

#### Scenario: Full test suite passes
- **WHEN** `devtools::test()` is run after all chunks are complete
- **THEN** all tests pass with 0 failures

### Requirement: Multi-flavor preprocessing yields per-flavor flat-buffer objects

Preprocessing a multi-flavor specification SHALL return one `preprocessed.goldfish`
object per modeled flavor, each satisfying this capability's single-model contract
(`stat_mat_update`/`stat_mat_pointer`, broadcast buffers, unified event fields,
engine-native `initialStats`, availability C-format, intercept scalars) so that either
compute backend consumes it without restructuring. Within a flavor's object,
`is_dependent` SHALL mark that flavor's events as dependent; on timed rate sub-models
(DyNAM-rate, REM) events of other flavors (modeled or not) SHALL appear as
right-censored entries, and on ordered/choice sub-models they SHALL appear only through
their process-state statistic updates. The intercept scalars (`n_dep_events`,
`total_time`, `avg_active_actors`) SHALL be per-flavor, with `avg_active_actors`
computed over that flavor's post-constraint (derived + user mask) active set. The
objects SHALL be delivered in the fid-indexed, process_map-carrying list defined by
the `flavored-processes` capability.

#### Scenario: per-flavor objects are backend-ready
- **WHEN** a two-flavor DyNAM-rate specification is preprocessed
- **THEN** two `preprocessed.goldfish` objects are returned, and each estimates on both
  the `r` and `cpp` backends without restructuring.

#### Scenario: is_dependent partitions by flavor on timed models
- **WHEN** the event sequence holds 30 creation and 20 dissolution events under a timed
  two-flavor model
- **THEN** the creation object has 30 dependent entries with the 20 dissolutions
  right-censored, and the dissolution object the reverse.
