# flat-preprocess-output (delta)

## MODIFIED Requirements

### Requirement: Combined flat update matrix in preprocessed output
The `preprocessed.goldfish` object SHALL contain `stat_mat_update` (a 4 × K integer/numeric matrix with rows `node1`, `node2`, `effect`, `replace` — all 0-indexed) covering **both** dependent and right-censored events in event-sequence order, holding only cell-specific (point) updates. `stat_mat_pointer` (numeric vector, length = number of stored events) SHALL record the end-column index in `stat_mat_update` after each stored event's updates are written. The object SHALL additionally contain `stat_mat_broadcast` (a 4 × M matrix with rows `kind`, `fixed`, `effect`, `replace`) and `stat_mat_broadcast_pointer` (length = number of stored events) holding the constant-value fan-out updates that were previously materialised as duplicate `stat_mat_update` columns; both SHALL be empty (0 columns) for models with no broadcast-eligible effects. The existing `is_dependent` integer vector (1L = dependent, 0L = right-censored) distinguishes event types. The `stats_change` nested list SHALL NOT be present in the returned object. The `preprocessed.goldfish` format `version` SHALL be bumped so that objects preprocessed by earlier versions are rejected by the `preprocessed` version check.

#### Scenario: Combined flat matrix is built during preprocessing
- **WHEN** `preprocess()` is called on any valid model formula
- **THEN** the returned object contains `stat_mat_update`, `stat_mat_pointer`, `stat_mat_broadcast`, `stat_mat_broadcast_pointer`, and `is_dependent`, and does not contain `stats_change`

#### Scenario: Pointer alignment covers both dependent and RC events
- **WHEN** `preprocess()` runs on a model with 2 dependent events and 1 RC event (in that order), each producing 1 point update
- **THEN** `stat_mat_pointer` equals `c(1, 2, 3)` and `ncol(stat_mat_update)` equals 3

#### Scenario: Fan-out effects do not bloat the point buffer
- **WHEN** `preprocess()` runs on a dyad model with an `alter()` effect where a node attribute change fans out to all senders of one alter
- **THEN** that fan-out adds one column to `stat_mat_broadcast` and zero columns to `stat_mat_update` for that effect at that event
