# process-state-evaluators Specification

## Purpose
TBD - created by archiving change refactor-likelihood-compute. Update Purpose after archive.
## Requirements
### Requirement: Estimation helpers factored behind process-state evaluator signatures
The rewritten likelihood helpers SHALL isolate the per-event probability/rate
computation behind named internal helpers taking a materialized process state
(dense statistics state plus active/presence information, a minimal delta
from the preprocessing output) and a parameter vector, rather than inlining
that computation into the estimator loops. Per sub-model the evaluators
SHALL return: DyNAM-choice — the probability of the sender choosing each
active receiver; DyNAM-rate and REM — the rate/hazard for each actor (rate)
or dyad (REM); DyNAM-rate-ordered — the probability of each actor being the
next sender. Exclusions (absent actors, reflexive dyads, risk-set
restrictions) SHALL enter as exact zeros, identically to estimation.

#### Scenario: choice probabilities for a state
- **WHEN** the internal choice evaluator is called with a materialized state,
  a sender, and parameters
- **THEN** it returns a probability vector over receivers that is zero for
  inactive/excluded receivers and sums to one over the active set.

#### Scenario: rates for a state
- **WHEN** the internal rate (or REM) evaluator is called with a materialized
  state and parameters
- **THEN** it returns the per-actor (per-dyad) hazard vector with excluded
  entries exactly zero, on the absolute scale used by the timed likelihood.

### Requirement: State materializer over preprocessed update streams
An internal state materializer SHALL reconstruct the dense process state at a
given event index by replaying the preprocessed initial state and update
streams (reusing the shared flat/broadcast update helpers), without modifying
preprocessing output formats. The materialized state SHALL expose the
active/presence sets alongside the statistics state, so that risk-set
membership and dyad identity at any event index are recoverable from the
DEFAULT preprocessing output alone — the dense state is addressed by
sanitized indices by construction, and the evaluator returns are labeled
with the same index vocabulary used by the gather/export long formats —
giving residuals and diagnostics the same join keys on every engine path.

#### Scenario: materialized state matches the estimation path
- **WHEN** the state is materialized at event k and passed to the evaluators
  with the fitted parameters
- **THEN** the returned probabilities/rates agree within 1e-10 with the
  per-event quantities the estimation path computes at event k (probability
  matrix / interval log-likelihood inputs).

#### Scenario: risk set and dyad identity recoverable at any event index
- **WHEN** the state is materialized at event k from a default-path
  `goldfishStat` object
- **THEN** the active sender/dyad sets at event k are available explicitly
  (matching a from-scratch replay of the availability buffers), every entry
  of the statistics state and of the evaluator returns is identified by its
  sanitized index pair (decodable to node labels), and excluded alternatives
  are exact zeros in the evaluator output — with no change to the
  preprocessing output format.

### Requirement: Evaluators remain internal until a simulation API stabilizes them
The process-state evaluators and state materializer SHALL NOT be exported:
they are internal (`goldfish:::`) building blocks for a future `simulate()`
method (recipe-style preprocessing with sampling steps) and for DyNES data
augmentation (which additionally requires the per-layer panel metadata of the
single-data-object change). No event-generation loop SHALL be added by this
change.

#### Scenario: no exported surface
- **WHEN** the package NAMESPACE is built after this change
- **THEN** no process-state evaluator or materializer is exported and no
  user-facing documentation commits to their signatures.

