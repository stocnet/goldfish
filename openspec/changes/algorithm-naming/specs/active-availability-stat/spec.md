# active-availability-stat (delta)

## MODIFIED Requirements

### Requirement: One homogenized availability name across all sites
Every site SHALL reference the `active_sender`/`active_dyad` objects (and their
`_init`/`_update`/`_update_pointer`/encoding fields) by these names — every
site that today references `presence1`/`presence2` or
`active_mode1_*`/`active_mode2_*` — the preprocessed object, the default-engine
`compute_step`, the R gather routines, the C++ estimators, and the output
writers. The object keeps its name at every encoding; consumers switch on the
encoding field, never on the name. Because this changes the
`preprocessed.goldfish` structure, the preprocessed format version SHALL be
bumped so stale objects supplied through the estimators' `preprocessed =`
argument (formerly `preprocessing_init =`) are rejected with the existing
outdated-format error.

#### Scenario: consistent naming end to end
- **WHEN** any engine or writer reads availability
- **THEN** it reads `active_sender`/`active_dyad` (not `presence1`/`presence2`,
  `active_mode1_*`/`active_mode2_*`, or `active1`/`active2`).

#### Scenario: stale preprocessed object rejected
- **WHEN** a preprocessed object produced before this change is passed to
  estimation via `preprocessed =`
- **THEN** it is rejected with the outdated-preprocessing-format error, prompting
  recomputation.
