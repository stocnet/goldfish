## MODIFIED Requirements

### Requirement: avg_active_entity declared by the recipe constructor
The rate-intercept denominator SHALL be stored as `avg_active_entity` (renaming
`avg_active_actors`), computed during preprocessing from the maintained
availability object as the time-weighted average `exposure_entity_time /
total_time`, where `exposure_entity_time`, stored beside it, is the integral
over the window of the entity count, each count weighted by the time it was in
force, with the reduction declared by the recipe constructor: the sender loop
counts `active_sender` (active senders after the support gate); the REM dyad
loop counts TRUE cells of `active_dyad` (active dyads). Every DyNAM and REM
recipe, the choice family included, SHALL compute `n_dep_events`,
`total_time`, `exposure_entity_time` and `avg_active_entity`, a fitted result
SHALL carry them, and estimation SHALL NOT recompute them (the
`constrained_avg_active_actors` / `mask_to_sender_gate`
estimation-time recombination is removed).

#### Scenario: DyNAM-rate counts active senders
- **WHEN** a DyNAM-rate model with an intercept and a `support_constraint` is
  preprocessed
- **THEN** `avg_active_entity` is the time-weighted count of TRUE entries of the
  maintained `active_sender` (post-constraint), `exposure_entity_time /
  total_time`, with no estimation-time recombination.

#### Scenario: REM counts active dyads
- **WHEN** a REM (timed, with intercept) is preprocessed
- **THEN** `avg_active_entity` is the time-weighted TRUE-count of the maintained
  `active_dyad` (via the encoding accessor, e.g. factor-count product at the
  outer encoding), `exposure_entity_time / total_time`.

#### Scenario: a composition change is weighted by the time it is in force
- **WHEN** a timed DyNAM rate is preprocessed on nodes whose presence changes
  inside the window
- **THEN** `exposure_entity_time` sums each interval's length times the active
  senders during it, and `avg_active_entity` differs from the per-event average
  whenever the intervals are unequal.

#### Scenario: a choice model carries the scalars
- **WHEN** a DyNAM choice model is estimated
- **THEN** its preprocessed object and its fit carry `n_dep_events`,
  `total_time`, `exposure_entity_time` and `avg_active_entity`, computed as for
  a timed rate over the same senders.
