## ADDED Requirements

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
computed over that flavor's post-constraint (derived + user mask) active set.

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
