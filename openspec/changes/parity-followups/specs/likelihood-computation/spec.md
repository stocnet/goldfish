# likelihood-computation (delta)

## ADDED Requirements

### Requirement: The stored per-event score SHALL be computed from the event's own quantities
Every backend SHALL compute a stored per-event score row directly from that
event's observed statistic and contribution-weighted mean —
`X_obs − c · w' X` — rather than by differencing an accumulating total before
and after the event. The two forms are algebraically identical, but the
difference form subtracts two partial sums that grow with the sequence, so its
relative precision degrades for later events, which are exactly the events a
sequence-level diagnostic examines. The estimator's own accumulation of the
score SHALL be unchanged: this requirement governs the stored diagnostic, not
the quantity the optimizer follows, and no coefficient may move.

#### Scenario: stored scores agree across backends
- **WHEN** the same model is estimated on each supported backend at one fixed
  parameter vector with per-event scores requested
- **THEN** the stored score matrices agree to 1e-10

#### Scenario: the aggregate identity holds against an independent total
- **WHEN** the column sums of the stored per-event score matrix are compared to
  the fit's final score vector
- **THEN** they agree to 1e-10, the rows having been computed independently of
  the accumulated total rather than derived from it

#### Scenario: coefficients are unaffected
- **WHEN** a model whose coefficients are covered by the frozen baselines is
  estimated with and without per-event scores requested
- **THEN** the coefficients match the frozen baseline in both cases

#### Scenario: precision does not degrade along the sequence
- **WHEN** stored per-event scores are compared against scores recomputed
  independently at the same parameter vector, for events early and late in a
  long sequence
- **THEN** the agreement for late events is no worse than for early ones
