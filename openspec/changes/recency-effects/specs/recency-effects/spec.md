# recency-effects

## ADDED Requirements

### Requirement: Scope-named recency effects with kernel and truncation arguments

The package SHALL provide the recency effect family as separate named
effects, one per scope: `recency_send` (recency rank of j among the
sender's past targets), `recency_receive` (recency rank of j among the
sender's past sources), `recency_dyad` (recency of the (i, j) dyad in
the event stream), and `recency_global` (aggregates over the last k
events of the stream). Each effect SHALL accept a `kernel` argument and
a truncation argument `k` (default `Inf`), and SHALL be registered with
its own validity metadata.

#### Scenario: Sender-scope recency statistic

- **WHEN** `recency_send(net)` is used with the default kernel and
  sender i has contacted partners j3, j1, j5 in order of increasing
  recency (j5 most recent)
- **THEN** the statistic evaluated at t⁻ equals 1 for j5, 1/2 for j1,
  1/3 for j3, and 0 for any actor i has never contacted

#### Scenario: Effects are separately registered

- **WHEN** the effect registry is queried for the recency family
- **THEN** each scope-named effect resolves to its own entry with its
  own validity declarations, not to a shared entry branched on an
  argument

### Requirement: Kernel semantics and defaults

The `kernel` argument SHALL be an enumerated character value with
`"inverse"` (1/r) as default, and SHALL additionally accept
`"indicator"` (1 for r ≤ k, else 0), `"geometric"` (ρ^r with a fixed
decay argument), `"rank"` (the raw recency rank r for r ≤ k, else 0),
and `"bands"` (the estimable rank-band basis). A finite `k` SHALL
truncate the selected kernel to ranks r ≤ k with statistic 0 beyond k;
`k = Inf` SHALL compute the exact untruncated statistic. Truncated
variants SHALL NOT be separate kernels.

#### Scenario: Truncation composes with the kernel

- **WHEN** `recency_send(net, kernel = "inverse", k = 100)` is used
- **THEN** the statistic is 1/r for recency ranks r ≤ 100 and exactly 0
  for r > 100 and for never-contacted actors

#### Scenario: Raw-rank kernel reproduces the applied covariate shape

- **WHEN** `kernel = "rank"` is used with finite k
- **THEN** the statistic equals the recency rank r for r ≤ k and 0
  otherwise, matching a `dense_rank()`-by-recency pre-computation
  truncated at k

### Requirement: Recency ordering is over distinct keys and ignores dissolutions

The recency ordering SHALL be over distinct keys (partners, dyads, or
events per the effect's scope) ordered by most recent contact, with
rank 1 the most recent. A tie withdrawal or deletion event SHALL leave
the recency ordering unchanged, and an event whose key already holds
rank 1 SHALL leave the ordering unchanged. Keys never contacted SHALL
have no rank and statistic value 0.

#### Scenario: Repetition does not inflate rank depth

- **WHEN** sender i contacts the same partner j five times in a row
- **THEN** j holds rank 1 and every other contacted partner keeps its
  rank; the ordering length equals the number of distinct partners

#### Scenario: Dissolution is not a contact

- **WHEN** a tie-dissolution event removes the (i, j) tie
- **THEN** j's position in i's recency ordering is unchanged

### Requirement: Statistics are read at t⁻

Recency statistics SHALL be computed from the state at t⁻ — the buffer
is read before the current event is pushed — following the
predictability discipline stated in the coordination-mechanisms
capability (statistics computed from the state before the event so
intensities remain predictable). An event SHALL NOT contribute to the
statistic evaluated for itself.

#### Scenario: Current event excluded from its own statistic

- **WHEN** the statistic for the event (i, j, t) is evaluated
- **THEN** the recency ordering used reflects only events strictly
  before t (subject to the tied-time contract below)

### Requirement: Tie blocks freeze the ordering and update once

Statistic reads within a block of events sharing one timestamp SHALL
use the recency ordering as of the block's start, and the ordering
SHALL be updated once at block end, so the computed statistics are
invariant to the within-block order. The definition of the block
boundary and any user-supplied within-block order SHALL follow the
tied-event-times capability once it lands; until then, behavior on
tied timestamps follows data order and SHALL be documented as such.

#### Scenario: Within-block permutation is inert

- **WHEN** two dependent events share a timestamp and their storage
  order is swapped
- **THEN** every recency statistic computed for either event is
  unchanged

### Requirement: Truncation error bound is reported

For finite `k` the fitted-model summary SHALL report the truncation
bound on the log-intensity perturbation, |θ̂|·sup_{r>k} κ(r) (for the
inverse kernel, |θ̂|/(k+1)), and the documentation SHALL describe the
refit-at-k-and-2k stability diagnostic.

#### Scenario: Bound shown for a truncated fit

- **WHEN** a model with `recency_send(net, k = 100)` is summarized
- **THEN** the output includes the truncation bound computed from the
  fitted coefficient

### Requirement: Rank-band basis expands to per-band statistics

`kernel = "bands"` SHALL expand the term into one statistic per rank
band (default log-spaced bands, overridable via a boundary-vector
argument), with generated terms carrying decoder metadata that groups
them under the parent term for coefficient reporting. The fitted band
coefficients SHALL be interpretable as the estimated memory kernel.

#### Scenario: Bands expand with parent grouping

- **WHEN** `recency_send(net, kernel = "bands")` is parsed with default
  bands
- **THEN** one statistic per band enters the model and `coef()`/tidy
  output groups the band coefficients under the parent recency term

### Requirement: Buffer state is replay-reconstructible

The recency buffer state at any event index SHALL be reconstructible by
replaying the update stream from the initial state, with no state
outside the declared cache fields, so process-state evaluation and
residual computation can recover the statistic at any point of the
event sequence.

#### Scenario: Replay reproduces the buffer

- **WHEN** the update stream is replayed from the initial state up to
  event m
- **THEN** the reconstructed ordering and statistics equal those
  observed during the original forward pass at event m
