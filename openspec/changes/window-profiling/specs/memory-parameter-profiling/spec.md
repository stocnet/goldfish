# memory-parameter-profiling

## ADDED Requirements

### Requirement: Candidate grids default to observed-lag quantiles

The profile engine SHALL default the candidate set for a time-window
parameter to quantiles of the observed lag distribution (evaluation
time minus past event time) augmented with the data-resolution and
maximum-lag endpoints, SHALL accept a user-supplied candidate vector
instead, and SHALL record the realized grid on the profile object.
The documentation SHALL state why: the profile log-likelihood is a
step function whose breakpoints are observed lags, so candidates
between breakpoints cannot change the fit.

#### Scenario: Default grid from lags

- **WHEN** a profile is requested without candidates
- **THEN** the candidate set is the documented lag-quantile grid with
  both endpoints included, and the profile object records it

### Requirement: All candidates are evaluated in a single data traversal

The profile engine SHALL evaluate the statistics for every candidate
window in one traversal of the event data, using per-key sorted
event-time arrays and a monotone pointer sweep in which the statistics
at a candidate differ from the previous candidate only by the events
whose age falls in the annulus between them. The engine SHALL NOT
materialize per-candidate derived networks or expiry pseudo-event
streams, and its cost SHALL scale as O(E log E + E·C) statistic work
for E events and C candidates rather than C full preprocessing runs.

#### Scenario: No per-candidate build plans

- **WHEN** a profile over C candidate windows runs
- **THEN** no derived network objects or expiry event streams are
  created for the candidates, and the per-candidate statistics agree
  with a per-candidate eager computation on a test fixture

### Requirement: The selected candidate is refit on the eager path

The profile engine SHALL be a search surface only: its per-candidate
states are not fitted models and are exempt from the process-state
replay contract. The selected candidate SHALL be refit through the
ordinary eager window machinery (derived network plus expiry
pseudo-events on the schedule), the returned fit SHALL be
indistinguishable from a fit specified at that window directly, and
the sweep's statistics at the selected candidate SHALL agree
bit-identically with the eager path's — verified by test.

#### Scenario: Winner refit restores every downstream guarantee

- **WHEN** a profile selects ω̂ and returns the refit model
- **THEN** the returned fit was produced by the eager path, supports
  residuals/diagnostics/replay exactly as a directly specified fit,
  and its statistics match the sweep's values at ω̂

### Requirement: Two-window profiles enforce the ordering constraint

A short/long two-window profile SHALL sweep a constrained grid with
ω_short < ω_long strictly (equality makes the two windowed statistics
exactly collinear), warm-starting along both axes, and SHALL offer the
disjoint-band reparameterization ([0, ω_s], (ω_s, ω_l]) as an
equivalent, better-conditioned presentation.

#### Scenario: Equal windows excluded

- **WHEN** a two-window grid is constructed
- **THEN** no candidate pair has ω_short = ω_long, and the constraint
  is stated in the profile output

### Requirement: Integer and rank-kernel grids ride the same surface

The profiling surface SHALL cover the event-index parameters of the
recency family: an integer grid for `k`/`last_k` in which one buffer
walk yields the statistics for all candidates at once, and a
geometric-ρ grid in which all candidates share one rank walk and each
candidate is the transform ρ^r of the shared rank stream. Grid
defaults, warm starts, selection, and inference SHALL be common across
parameter types.

#### Scenario: One walk, all k

- **WHEN** an integer-k profile with k_max candidates runs
- **THEN** the event data is walked once and the per-candidate
  statistics for every k ≤ k_max are produced from that single walk

#### Scenario: Shared rank walk for rho

- **WHEN** a geometric-ρ profile over m candidates runs
- **THEN** ranks are computed once and each candidate's statistics are
  obtained by transforming the shared rank stream

### Requirement: Warm starts along the grid with recorded fallbacks

Fits along the candidate grid SHALL warm-start from the neighboring
candidate's estimates, SHALL fall back to a cold start on
non-convergence, and the profile object SHALL record which candidates
required the fallback. Before reporting, the fit at the selected
candidate SHALL be confirmed by a cold restart.

#### Scenario: Fallback recorded

- **WHEN** a warm-started fit at some candidate fails to converge and
  the cold start succeeds
- **THEN** the profile completes and the profile object marks that
  candidate as cold-started
