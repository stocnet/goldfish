# likelihood-computation (delta)

## ADDED Requirements

### Requirement: The stored per-event score SHALL come from one shared implementation
Every backend SHALL produce its stored per-event score row through the shared
reduction rather than through a private copy of the arithmetic, so a change to
the definition is made once per language rather than once per kernel. The
shared reduction exists as a compiled implementation and an R implementation,
pinned against each other by a direct test on constructed inputs, because the
compiled backends and the R reference backend cannot share one binary. The
estimator's own accumulation of the score SHALL be unchanged, and no
coefficient may move: this requirement governs where the stored diagnostic is
computed, not the quantity the optimizer follows.

Consolidating the event-loop engines onto the shared reduction replaces each
kernel's own arithmetic — in most of them a before/after difference of the
running derivative, in one an open-coded direct evaluation — with the same
quantity computed once, which shifts stored values by a small amount. That
shift is an accepted consequence of having one implementation, **not** a
precision claim: the difference form's conditioning penalty was measured across
the model families at 1.2e-14 to 9.2e-13 relative, one hundred to eight
thousand times tighter than the cross-backend tolerance, so it is real in
mechanism and negligible in size.

Consolidation SHALL NOT be expected to tighten cross-backend agreement.
Agreement between a compiled backend and the R reference is limited by their
being two implementations over different linear-algebra paths, not by which
form each uses internally, so it stays at its floating-point floor either way.

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
