## MODIFIED Requirements

### Requirement: Shared flat-update and broadcast-apply core reused by mask and active sets
The flat-update application and broadcast-value fan-out application SHALL be
provided by shared functions (one per representation layer: the R backend, the
R gather, and the C++ estimators) that are consumed by statistics, interaction
operands, constraint atoms, the support mask, and the `active_sender`/`active_dyad`
availability stats alike. Those shared functions SHALL be expressed over a value held
at a broadcast kind, with the target kind a parameter rather than a hard-coded point
kind, so that projecting a value or a set of entries from one kind to another, writing
entries in place, and emitting crossings are each defined exactly once. The operand,
mask and availability objects SHALL NOT carry their own copies of the flat-update,
broadcast-apply, projection or expansion logic.
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

#### Scenario: interaction operands apply via the shared functions
- **WHEN** an interaction operand's live value is updated during preprocessing
- **THEN** the write goes through the same shared projection and in-place write the mask
  and the availability objects use, with no operand-specific expansion routine.

#### Scenario: projection to a non-point kind is available
- **WHEN** a value or a set of changed entries held at one broadcast kind must be read at
  a wider kind that is not point
- **THEN** the shared projection produces it directly, without materializing the dense
  n1 x n2 form as an intermediate.
