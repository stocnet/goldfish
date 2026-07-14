## ADDED Requirements

### Requirement: No per-event dense duplication in the dyad-model gather representation
The gather representation SHALL NOT, for dyad-indexed models (REM
timed/ordered, DyNAM-coordination), store one dense statistics row per present
dyad per event when those rows duplicate unchanged or repeated values:
unchanged rows SHALL be stored once and referenced, and coordination SHALL
store d = n(n−1)/2 triangle dyad slots (both directions mapped to one slot;
one or two stored rows per slot per the symmetry audit). The gather pass
SHALL NOT perform a full O(n²·p) snapshot copy per event. The compression
SHALL change only what a row reference points into: the per-row
`index_i`/`index_j` identity and the kernel's index-based read contract
established by the likelihood-computation capability SHALL be preserved
unchanged.

#### Scenario: repeated rows stored once
- **WHEN** a REM model whose statistics leave most dyads at a common value is
  gathered
- **THEN** the stored representation grows with the number of DISTINCT rows and
  per-event changes, not with events × dyads, and the measured size on the
  baseline fixtures is recorded against the pre-change `stat_all_events` size.

#### Scenario: coordination stores one triangle slot per dyad
- **WHEN** a one-mode DyNAM-coordination model is gathered
- **THEN** the representation holds d = n(n−1)/2 dyad slots (one or two
  stored directed rows per slot per the symmetry audit), and expanding a
  slot's references reproduces the same index-identified rows the
  pre-compression gather emitted.

### Requirement: Observed-event statistics kept exact per event
The gather representation SHALL keep, for every dependent event, a pointer to
the observed dyad's exact statistics row (by dictionary id or explicit
storage); the observed contribution to the likelihood and score SHALL be read
from that row, never reconstructed from aggregated quantities. Right-censored
intervals (timed REM), which have no observed dyad, MAY be represented by
aggregates alone.

#### Scenario: observed row survives compression
- **WHEN** any dependent event is evaluated from the compressed representation
- **THEN** the statistics vector entering `score += s_obs` and
  `logL += x_obs` equals the observed dyad's row in the pre-change dense
  gather, exactly.

### Requirement: Numerical equivalence with the pre-change gather engine
The rework SHALL satisfy: (a) the gathered candidate multiset and observed
rows agree with the pre-change gather output on fixture models before the
dense path is deleted; (b) per-event logL/score/information from the new
representation agree with the `default` engine within 1e-10 on the fixture
models; (c) the existing cross-engine coefficient tests pass unchanged.

#### Scenario: gather-output equivalence before deletion
- **WHEN** the new gather pass runs on the fixture models
- **THEN** expanding its representation reproduces the pre-change per-event
  candidate rows (as a multiset) and selected/observed indices, and only after
  this passes is the dense gather path removed.

#### Scenario: estimation equivalence
- **WHEN** the same REM/coordination model is estimated via the new gather
  representation and via the `default` engine
- **THEN** per-event contributions agree within 1e-10 and converged
  coefficients within the existing cross-engine tolerance.

### Requirement: gather_model_data() keeps its documented information content
The exported `gather_model_data()` SHALL keep its documented output,
materialized on demand from the internal representation, and SHALL reproduce
the pre-compression output as SAME INFORMATION: an identical index-keyed row
multiset per event — the same (`index_i`, `index_j`, statistics) tuples —
with identical `selected` and `n_candidates` (deterministic re-indexing, no
floating-point arithmetic in the expansion; row order within an event is not
part of the contract, the index columns are).

#### Scenario: expansion reproduces the index-keyed multiset
- **WHEN** `gather_model_data()` is called on a fixture model before and after
  the format change
- **THEN** for every event the returned (`index_i`, `index_j`, statistics)
  row multisets are identical, and `selected`/`n_candidates`/`timespan`
  match exactly.
