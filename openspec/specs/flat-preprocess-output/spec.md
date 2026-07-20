# flat-preprocess-output Specification

## Purpose
Define the flat-buffer `preprocessed.goldfish` output: combined `stat_mat_update` / `stat_mat_pointer` point updates, `stat_mat_broadcast` fan-out, unified event fields, and engine-native initial stats, consumed by the R (`default`) and C++ (`default_c`) estimation engines without restructuring.
## Requirements
### Requirement: Combined flat update matrix in preprocessed output
The `preprocessed.goldfish` object SHALL contain `stat_mat_update` (a 4 × K integer/numeric matrix with rows `node1`, `node2`, `effect`, `replace` — all 0-indexed) covering **both** dependent and right-censored events in event-sequence order, holding only cell-specific (point) updates. `stat_mat_pointer` (numeric vector, length = number of stored events) SHALL record the end-column index in `stat_mat_update` after each stored event's updates are written. The object SHALL additionally contain `stat_mat_broadcast` (a 4 × M matrix with rows `kind`, `fixed`, `effect`, `replace`) and `stat_mat_broadcast_pointer` (length = number of stored events) holding the constant-value fan-out updates that were previously materialised as duplicate `stat_mat_update` columns; both SHALL be empty (0 columns) for models with no broadcast-eligible effects. The existing `is_dependent` integer vector (1L = dependent, 0L = right-censored) distinguishes event types. The `stats_change` nested list SHALL NOT be present in the returned object. The `preprocessed.goldfish` format `version` SHALL be bumped so that objects preprocessed by earlier versions are rejected by the `preprocessing_init` version check.

#### Scenario: Combined flat matrix is built during preprocessing
- **WHEN** `preprocess()` is called on any valid model formula
- **THEN** the returned object contains `stat_mat_update`, `stat_mat_pointer`, `stat_mat_broadcast`, `stat_mat_broadcast_pointer`, and `is_dependent`, and does not contain `stats_change`

#### Scenario: Pointer alignment covers both dependent and RC events
- **WHEN** `preprocess()` runs on a model with 2 dependent events and 1 RC event (in that order), each producing 1 point update
- **THEN** `stat_mat_pointer` equals `c(1, 2, 3)` and `ncol(stat_mat_update)` equals 3

#### Scenario: Fan-out effects do not bloat the point buffer
- **WHEN** `preprocess()` runs on a dyad model with an `alter()` effect where a node attribute change fans out to all senders of one alter
- **THEN** that fan-out adds one column to `stat_mat_broadcast` and zero columns to `stat_mat_update` for that effect at that event

#### Scenario: Buffer grows without error for large models
- **WHEN** the total number of point updates across all stored events exceeds the initial buffer allocation
- **THEN** `preprocess()` completes successfully and `ncol(stat_mat_update)` equals the total number of point updates written

#### Scenario: No broadcast buffer for models without fan-out effects
- **WHEN** `preprocess()` runs on a model whose effects are all cell-specific dyad effects (e.g., `inertia`, `recip`, `trans`)
- **THEN** `stat_mat_broadcast` has 0 columns and the point `stat_mat_update` is unchanged from the pre-encoding behaviour

### Requirement: Unified event fields retained
The `preprocessed.goldfish` object SHALL retain the unified event fields already produced by the monolithic loop: `is_dependent`, `event_time`, `event_sender`, `event_receiver`, `intervals`, `event_pos`, `active_mode1_init`, `active_mode1_changes`, `active_mode2_init`, `active_mode2_changes`, `startTime`, and `endTime`. Recipe methods SHALL produce these fields with the same semantics as the pre-refactor monolith.

#### Scenario: Unified fields present in recipe output
- **WHEN** any recipe method returns a `preprocessed.goldfish` object
- **THEN** all unified event fields listed above are present and `length(event_pos)` equals the number of stored events

### Requirement: Global-attribute events handled in rate recipe loops
Recipe methods for rate models — `preprocess.dynam_rate_spec()` (sender-indexed), `preprocess.rem_rate_spec()` and `preprocess.rem_rate_ordered_spec()` (dyad-indexed) — SHALL process global-attribute events (event streams with neither `node` nor `sender`/`receiver` columns): updating the linked `global.goldfish` object column named by `attr(events, "replace")`, and emitting right-censored statistic updates (previous value immediately before the change event) mirroring the node-attribute right-censoring behaviour of the pre-refactor monolith.

#### Scenario: Global change event updates all actors' statistics
- **WHEN** `preprocess()` runs a DyNAM-rate model with a `global(seasons$winter)` effect and one linked event changing `winter` from 0 to 1 at time t
- **THEN** the flat update buffer contains, after time t, one replace update per actor with value 1, matching the pre-refactor output of the same model

#### Scenario: Global events produce no sender/receiver recording
- **WHEN** a global-attribute event is processed by a rate recipe
- **THEN** no `event_sender` / `event_receiver` entry is recorded for it and the event does not count toward `n_dep_events`

#### Scenario: Global coefficient baselines reproduced
- **WHEN** the DyNAM-rate and REM models in `tests/testthat/_baselines/global_v1/` are re-estimated through the recipe pipeline
- **THEN** coefficients agree with the stored baselines to within 1e-6 (baseline regenerable only with documented justification per design D18)

### Requirement: initialStats retained in engine-native form
`initialStats` SHALL be stored in the form each estimation engine uses natively. For `sender_spec` models (rate, rate_ordered), `initialStats` SHALL be `n1 × nEffects` (2D). For `dyad_spec` models (choice, choice_coord, REM), `initialStats` SHALL be `n1 × n2 × nEffects` (3D). The C++ interface SHALL flatten the 3D form to `n1*n2 × nEffects` at call time; preprocessing recipes SHALL NOT pre-flatten it.

#### Scenario: Sender model produces 2D initialStats
- **WHEN** `preprocess()` is called for a DyNAM-rate submodel with `n1=10`, `nEffects=2`
- **THEN** `length(dim(preprocessedObject$initialStats))` equals 2 and `dim(preprocessedObject$initialStats)` equals `c(10L, 2L)`

#### Scenario: Dyad model produces 3D initialStats
- **WHEN** `preprocess()` is called for a DyNAM-choice submodel with `n1=10`, `n2=10`, `nEffects=3`
- **THEN** `length(dim(preprocessedObject$initialStats))` equals 3 and `dim(preprocessedObject$initialStats)` equals `c(10L, 10L, 3L)`

### Requirement: Intercept scalars stored in preprocessed output
The `preprocessed.goldfish` object SHALL contain `n_dep_events` (integer count of dependent events), `total_time` (numeric sum of all inter-event intervals), and `avg_active_actors` (numeric time-weighted mean number of active actors per dependent event). These SHALL be computed once during the preprocessing event loop and SHALL NOT be recomputed at estimation time. When a `support_constraint` is active, `avg_active_actors` SHALL count the **post-constraint** active set (rate: senders with at least one allowed receiver; dyadic models: allowed (sender, receiver) pairs), time-weighted over the sub-intervals created by mask flips.

#### Scenario: Intercept scalars are present and non-negative
- **WHEN** `preprocess()` completes successfully
- **THEN** `preprocessedObject$n_dep_events` is a positive integer, `preprocessedObject$total_time` is a positive numeric, and `preprocessedObject$avg_active_actors` is a positive numeric

#### Scenario: Intercept formula produces correct baseline
- **WHEN** `preprocessedObject$n_dep_events`, `total_time`, and `avg_active_actors` are used in `log(n_dep_events / total_time / avg_active_actors)`
- **THEN** the result equals the intercept estimate previously computed by `estimation_core.R` or `cpp_interface.R` for the same dataset to within floating-point tolerance

#### Scenario: Constrained models count the post-constraint set
- **WHEN** `preprocess()` runs a rate model with a `support_constraint` that gates out some senders over part of the sequence
- **THEN** `avg_active_actors` equals the time-weighted mean of the constrained sender counts (matching a hand-computed fixture value), not the unconstrained presence counts

### Requirement: Presence updates stored in C format
The `preprocessed.goldfish` object SHALL store availability as the per-loop
objects in C format: the sender loop stores `active_sender_init` plus
`active_sender_update` (a 2 x K' integer matrix in C format) and
`active_sender_update_pointer` (an integer vector aligned to stored events); the
dyad loop stores the `active_dyad` fields (see the `active_dyad` requirement) —
each precomputed during preprocessing with presence, gates, and support already
folded. Separate `presence1`/`presence2`, `active_mode1_*`/`active_mode2_*`, or
`active1`/`active2` objects SHALL NOT be stored. The change list MAY be retained
for display purposes.

#### Scenario: Availability C-format buffers are present when they change
- **WHEN** `preprocess()` is called on a sender-loop model with node-level
  composition changes or an availability restriction
- **THEN** `preprocessedObject$active_sender_update` and
  `preprocessedObject$active_sender_update_pointer` are non-NULL integer
  matrices/vectors, and no separate `presence1_update` object is carried

#### Scenario: No availability updates when nothing changes
- **WHEN** `preprocess()` is called on a sender-loop model with no composition
  change and no availability restriction
- **THEN** `preprocessedObject$active_sender_update` is NULL

### Requirement: ignore_repetitions rejected with a clear error
Any formula containing `ignore_repetitions = TRUE` SHALL cause `estimate_dynam()`, `estimate_rem()`, or `estimate_dynami()` to call `cli::cli_abort()` before preprocessing begins. No `ignoreRep_masks` field SHALL exist in any `preprocessed.goldfish` object. The feature is disabled pending a correct reimplementation (see design Open Questions).

#### Scenario: ignore_repetitions triggers immediate error
- **WHEN** `estimate_dynam(inertia(net) + indeg(net, ignore_repetitions = TRUE), data, sub_model = "choice")` is called
- **THEN** a `cli::cli_abort()` error is raised before any preprocessing occurs

### Requirement: Estimation routines are self-contained
After `preprocess()` returns, neither `estimation_core.R` nor `cpp_interface.R` SHALL call `get()` on any R environment object or accept `defaultNetworkName` / `prepEnvir` parameters for the purposes of ignoreRep or actor-count derivation. All data required for estimation SHALL be present in the `preprocessed.goldfish` object.

#### Scenario: Estimation completes without environment lookups
- **WHEN** `estimate_dynam()` is called after preprocessing and the `prepEnvir` environment is removed
- **THEN** estimation completes successfully and produces the same coefficients as before the refactor

#### Scenario: No get() calls in estimation source after refactor
- **WHEN** `grep -n "get(" R/estimation_core.R R/cpp_interface.R` is run after Chunk C is complete
- **THEN** the command returns zero matches

### Requirement: Coefficient reproducibility across refactor
The refactored pipeline SHALL produce ML coefficient estimates identical to the pre-refactor pipeline to within 1e-6 absolute tolerance for all existing test datasets (Social_Evolution, Fisheries) and both engines (default R, default_c C++).

#### Scenario: DyNAM-rate coefficients are unchanged
- **WHEN** `estimate_dynam()` is run with `engine="default"` and `engine="default_c"` on Social_Evolution data before and after the refactor
- **THEN** all coefficients agree to within 1e-6

#### Scenario: DyNAM-choice coefficients are unchanged
- **WHEN** `estimate_dynam()` with subModel="choice" is run before and after the refactor
- **THEN** all coefficients agree to within 1e-6

#### Scenario: Full test suite passes
- **WHEN** `devtools::test()` is run after all chunks are complete
- **THEN** all tests pass with 0 failures

### Requirement: Effective dyadic availability carried as an encoding-aware flat statistic (active_dyad)
The `preprocessed.goldfish` object of a dyad-loop model SHALL carry `active_dyad`
— the effective per-event availability with the per-family operands already
folded (receiver presence and alter/ego/scalar support for choice; both presences
and all support atoms for REM; opportunity) — as a separate, encoding-aware flat
statistic: `active_dyad_init` plus a flat update buffer with a per-event pointer
and an encoding field, mirroring the `stat_mat_*` fields. The stored encoding
SHALL be the minimal one (scalar / ego / alter / outer / point); a dense
`n1 × n2` init and `(node1, node2, replace)` point flips SHALL appear ONLY at the
point encoding (a point-kind atom or an opportunity list). The object SHALL NOT
be a per-event list of dense matrices, SHALL NOT be a slice or column of
`stat_mat_init`/`initialStats`, and no separate raw `support` mask, `active1`/
`active2`, or `presence*` object SHALL be carried alongside it.

#### Scenario: point constraint stored as active_dyad init plus buffer
- **WHEN** `preprocess()` runs on a model with a point-kind `support_constraint` or an
  opportunity list
- **THEN** the object carries `active_dyad_init` plus a flat `(node1, node2, replace)`
  buffer and pointer (the `stat_mat_*` shape) at the point encoding, not a list of
  per-event matrices and no separate `support` object.

#### Scenario: broadcast or separable availability stored without densification
- **WHEN** `preprocess()` runs on a dyad-loop model whose folded availability is
  ego-, alter-, scalar-, or outer-kind (no point atom, no opportunity list)
- **THEN** `active_dyad` is stored at that encoding (vectors or outer factors), no
  dense `n1 × n2` value is materialized, and the field is still named
  `active_dyad`.

#### Scenario: static availability stores a trivial init
- **WHEN** `preprocess()` runs on a model with no `support_constraint`, no
  opportunity list, and no composition change
- **THEN** `active_dyad` carries a constant init with an empty update buffer, and
  estimation output is bit-identical to the pre-change engine.

### Requirement: Multi-flavor preprocessing yields per-flavor flat-buffer objects

Preprocessing a multi-flavor specification SHALL return one `preprocessed.goldfish`
object per modeled flavor, each satisfying this capability's single-model contract
(`stat_mat_update`/`stat_mat_pointer`, broadcast buffers, unified event fields,
engine-native `initialStats`, availability C-format, intercept scalars) so that either
estimation engine consumes it without restructuring. Within a flavor's object,
`is_dependent` SHALL mark that flavor's events as dependent; on timed rate sub-models
(DyNAM-rate, REM) events of other flavors (modeled or not) SHALL appear as
right-censored entries, and on ordered/choice sub-models they SHALL appear only through
their process-state statistic updates. The intercept scalars (`n_dep_events`,
`total_time`, `avg_active_actors`) SHALL be per-flavor, with `avg_active_actors`
computed over that flavor's post-constraint (derived + user mask) active set. The
objects SHALL be delivered in the fid-indexed, process_map-carrying list defined by
the `flavored-processes` capability.

#### Scenario: per-flavor objects are engine-ready
- **WHEN** a two-flavor DyNAM-rate specification is preprocessed
- **THEN** two `preprocessed.goldfish` objects are returned, and each estimates on both
  the `default` and `default_c` engines without restructuring.

#### Scenario: is_dependent partitions by flavor on timed models
- **WHEN** the event sequence holds 30 creation and 20 dissolution events under a timed
  two-flavor model
- **THEN** the creation object has 30 dependent entries with the 20 dissolutions
  right-censored, and the dissolution object the reverse.

#### Scenario: per-flavor intercept scalars
- **WHEN** the two objects' scalars are inspected
- **THEN** each flavor's `n_dep_events` counts only its own dependent events and its
  `avg_active_actors` is time-weighted over its own combined mask.

