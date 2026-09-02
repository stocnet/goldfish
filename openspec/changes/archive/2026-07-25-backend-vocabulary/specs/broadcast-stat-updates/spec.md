# broadcast-stat-updates (delta)

Wording-only: user-selectable backends named by their `backend` value. The
shared-core requirement keeps its representation-layer phrasing (an
implementation structure, not a user choice) but names the backends a reader
would select.

## MODIFIED Requirements

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
