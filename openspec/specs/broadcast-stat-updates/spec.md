# broadcast-stat-updates Specification

## Purpose
Encode constant-value fan-out (broadcast) statistic updates compactly during preprocessing — one broadcast row per update instead of materialized duplicate columns — and replay them at estimation time.
## Requirements
### Requirement: Broadcast encoding of constant-value fan-out updates
Preprocessing SHALL encode each constant-value fan-out statistic update as a
single entry in a `stat_mat_broadcast` buffer instead of one duplicate column
per affected cell in `stat_mat_update`. The buffer SHALL be a 4 × M matrix with
rows `kind`, `fixed`, `effect`, `replace`, paired with a
`stat_mat_broadcast_pointer` (length = number of stored events) recording the
end-column index after each stored event. `kind` SHALL be `1` (broadcast over
`node1` / senders, holding alter index `fixed`), `2` (broadcast over `node2` /
alters, holding ego index `fixed`), or `3` (broadcast to all actors, `fixed`
ignored). `effect` and `fixed` SHALL be 0-indexed. Sender-indexed models SHALL
emit only `kind = 3`.

#### Scenario: Fan-out emitted as one broadcast entry
- **WHEN** an `alter()` attribute change affects all `n1` senders' view of one
  alter during a stored event
- **THEN** `stat_mat_broadcast` gains exactly one column (`kind = 1`, `fixed` =
  the alter, `replace` = the new value) and `stat_mat_update` gains no columns
  for that effect at that event

#### Scenario: Global change in a rate model
- **WHEN** a `global()` value changes in a DyNAM-rate model during a stored event
- **THEN** `stat_mat_broadcast` gains one column with `kind = 3` and `replace`
  the new global value

### Requirement: Broadcast decode in the R estimation engine
The `r` backend SHALL reconstruct each event's statistics by applying the
point slice of `stat_mat_update` and then the broadcast slice of
`stat_mat_broadcast`, producing a statistics array bit-identical to the
eager-expansion result. For a 2D (sender) array, `kind = 3` SHALL set the whole
effect column. For a 3D (dyad) array, `kind = 1` SHALL set the held alter
column, `kind = 2` the held ego row, and `kind = 3` the whole effect slice.

#### Scenario: the r backend reproduces eager-expansion statistics
- **WHEN** a dyad model with `alter()` and `ego()` effects is estimated with
  `backend = "r"` using the broadcast encoding
- **THEN** the per-event statistics array equals the array produced by the
  pre-encoding eager expansion at every event

### Requirement: Broadcast decode in the C++ estimation engines
The C++ backends — `backend = "cpp"` and `backend = "gather"` — SHALL apply the
broadcast buffer per event through a shared decode routine, producing
coefficients and log-likelihoods identical (to 1e-6) to the `r` backend on the
same model.

#### Scenario: the C++ backends match the r backend
- **WHEN** the same broadcast-encoded model is estimated with `backend = "r"`,
  `backend = "cpp"`, and `backend = "gather"`
- **THEN** all three return coefficients and `logLik` agreeing to within 1e-6

### Requirement: Reflexive-diagonal and two-mode handling preserved
Broadcast decode SHALL reproduce the focal-cell exclusion that
`to_ego()` / `to_alter()` apply today: for one-mode (`twomode_or_reflexive =
FALSE`) dyad models, a `kind = 1` broadcast holding alter `J` SHALL NOT write
cell `(J, J)` and a `kind = 2` broadcast holding ego `I` SHALL NOT write cell
`(I, I)`; for two-mode or reflexive-allowed models, the full row/column SHALL be
written.

#### Scenario: One-mode broadcast skips the diagonal
- **WHEN** a one-mode dyad model applies a `kind = 1` broadcast holding alter `J`
- **THEN** every cell `(i, J)` with `i != J` is set and cell `(J, J)` is left
  unchanged

#### Scenario: Two-mode broadcast fills the whole slice
- **WHEN** a two-mode dyad model applies a `kind = 2` broadcast holding ego `I`
- **THEN** every cell `(I, j)` is set with no diagonal exclusion

### Requirement: Effect eligibility for broadcast encoding
Preprocessing SHALL encode as broadcasts only effects whose fan-out value is
constant across the broadcast dimension: `alter()`, `ego()`, the degree effects
(`indeg` / `outdeg` / `degree`) in their alter/ego projection, and `global()`.
Effects whose fan-out value depends on both endpoints (`same`, `diff`, `sim`,
`ego_alter_interaction`, tertius variants) and genuinely cell-specific dyad
effects (`inertia`, `recip`, `trans`, `cycle`, `tie`, closure effects) SHALL
continue to emit point updates in `stat_mat_update`. The classification SHALL be
derived from each effect's `stat_kind` in the update plan.

#### Scenario: Constant fan-out effect is encoded
- **WHEN** a model includes `alter(attr)` and a node attribute changes
- **THEN** the change is recorded in `stat_mat_broadcast`, not as duplicate
  `stat_mat_update` columns

#### Scenario: Value-varying effect stays a point update
- **WHEN** a model includes `same(attr)` and a node attribute changes
- **THEN** the affected cells are recorded as point updates in `stat_mat_update`
  and `stat_mat_broadcast` gains no entry for that effect

### Requirement: One stat_kind per effect column
The update plan SHALL guarantee that every effect column is written exclusively
by point updates or exclusively by broadcast updates, never both, so the two
buffers never contend for the same cell. A model whose effect would mix kinds
SHALL fail loudly at plan-build time rather than produce incorrect statistics.

#### Scenario: Mixed-kind effect rejected at plan build
- **WHEN** the update plan is built for an effect that would emit both point and
  broadcast updates for the same effect column
- **THEN** plan construction raises an error instead of returning a plan

### Requirement: Coefficient reproduction and reduced buffer size
The broadcast encoding SHALL reproduce, for every model that uses
broadcast-eligible effects, the pre-encoding coefficients and log-likelihood to
within 1e-6 on both engines, while `ncol(stat_mat_update)` is strictly smaller
than the pre-encoding value and overall estimation wall time is approximately
unchanged.

#### Scenario: Buffer shrinks with reproduced coefficients
- **WHEN** a dyad model dominated by `alter()` / `ego()` effects is preprocessed
  with the broadcast encoding
- **THEN** `ncol(stat_mat_update)` is smaller than the eager-expansion column
  count and the estimated coefficients match the eager-expansion baseline to
  within 1e-6

### Requirement: Shared flat-update and broadcast-apply core reused by mask and active sets
The flat-update application and broadcast-value fan-out application SHALL be
provided by shared functions (one per representation layer: the R backend, the
R gather, and the C++ estimators) that are consumed by statistics, the support
mask, and the `active_sender`/`active_dyad` availability stats alike. The mask and
availability objects SHALL NOT carry their own copies of the flat-update or
broadcast-apply logic.
Extracting these shared functions SHALL NOT change the numerical behavior of the
statistic path.

#### Scenario: statistics behavior unchanged after extraction
- **WHEN** the shared apply functions are extracted and statistics are routed through
  them
- **THEN** unconstrained coefficient and C++ golden baselines are reproduced exactly
  (PASS not SKIP), byte-identical to before the extraction.

#### Scenario: mask applies via the shared functions
- **WHEN** the support mask's flat/broadcast update buffer is applied during
  preprocessing or estimation
- **THEN** it is applied by the same shared functions the statistic buffers use, on
  every backend (`r`, `gather`, `cpp`).

