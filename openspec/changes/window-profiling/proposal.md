# Window Profiling — Memory-Parameter Selection Without Replays

## Why

Window lengths (and their event-index twins: `last_k`, the recency
truncation `k`, and the geometric decay `ρ`) are covariate-construction
parameters — changing them changes the estimand, so they must be
selected, not absorbed into a coefficient. Today selection means one
full pipeline run per candidate: every candidate window materializes
its own derived network and expiry pseudo-event stream through the
build-plan machinery and pays a full preprocess, so a C-candidate
search costs C replays of the most expensive stage. The structural
facts in `.plan/sp/window_profiling_spec.md` make that waste
avoidable: the profile log-likelihood is a step function that changes
only when ω crosses an observed lag, the exhaustive candidate set is
the distinct observed lags (a practical grid is their quantiles), and
a monotone two-pointer sweep over per-key sorted event times evaluates
every candidate in one data traversal. Selection also needs honest
inference: under the null the window is unidentified (the Davies
problem), so the naive sup-over-candidates LR test is anticonservative.

## What Changes

- **New profile engine** (search-side, distinct from estimation):
  per-key sorted event-time arrays built in one pass, a two-pointer
  annulus sweep that updates statistics between adjacent candidates by
  only the events whose age falls in the annulus (ω_j, ω_{j+1}], and
  warm-started fits along the grid — total cost O(E log E + E·C)
  statistic work instead of C full preprocesses. **The profile engine
  is exempt from the process-state replay contract** (ADR-0028): its
  per-candidate states are search intermediates, never fitted objects;
  the winning candidate is refit through the ordinary eager build-plan
  path, and sweep-vs-refit agreement at the winner is the correctness
  gate.
- **New grid defaults**: candidate windows from observed-lag quantiles
  plus the data-resolution and max-lag endpoints; user-suppliable
  grids; the two-window variant (short < long) as a constrained 2-D
  sweep with the disjoint-band reparameterization available.
- **New event-index grids on the same surface**: the integer `last_k`/
  recency-`k` grid (one buffer walk yields all candidates at once) and
  the geometric-`ρ` grid (one rank walk shared by all candidates, each
  a transform ρ^r) — the profiling surface for `recency-effects`'
  kernels, resolving its geometric-ρ open question.
- **New selection inference**: sup-LR with a Davies-type bound as the
  cheap default (uses the profile curve already computed), and a
  parametric-bootstrap calibration (simulate from the fitted baseline,
  recompute the sup over the same grid per replicate) as the honest
  option — gated on `process-simulation` providing `simulate()`.
  Profile-likelihood intervals reported with the step-function caveat.
- **Not changing**: the estimation path. Final fits at a chosen window
  keep the eager materialized expiry stream and everything downstream
  of it (residuals, diagnostics, replay, DyNES) unchanged. No
  user-facing change to how `window =` is written in formulas.

## Capabilities

### New Capabilities

- `memory-parameter-profiling`: the profile engine — lag-quantile grid
  defaults, single-traversal annulus sweep, constrained 2-D
  (short < long) sweep, integer-k and geometric-ρ grids, warm starts,
  the replay-contract exemption, and the sweep-vs-refit agreement
  gate at the selected candidate.
- `memory-selection-inference`: the profile outputs and tests — the
  step-function profile curve, approximate profile intervals, sup-LR
  with the Davies bound, and the parametric-bootstrap calibration.

### Modified Capabilities

None: the estimation-side window capabilities
(`window-list-network-effects`, `window-list-attribute-effects`,
`observation-window`) are untouched — the profile engine sits beside
them, and the fit it hands back goes through them unchanged.

## Impact

- **Code**: new profile-engine module (per-key sorted arrays, pointer
  sweep, FIFO/hash-keyed multiset helpers — R-first with
  fastmap/collections-style structures; Rcpp port only after the R
  oracle exists), a `profile_memory()`-style exported surface (name to
  be confirmed against the estimation-naming conventions), inference
  helpers, plot/print methods for the profile object.
- **Sequencing**: post-2.0.0 (not on the release spine). The
  time-window (ω) legs need only existing windowed effects; the
  `k`/`last_k`/`ρ` legs are gated on `recency-effects`; the bootstrap
  leg is gated on `process-simulation`. Independent of
  `effect-term-registry` for ω, but the k/ρ legs inherit its registry
  vocabulary through `recency-effects`.
- **Relation to DyNES**: the lazy sweep structures are also the
  substrate a future incremental-augmentation path would want —
  eager expiry streams are derived data invalidated by any sequence
  edit, which is why `dynes-augmentation` currently re-preprocesses
  per drawn sequence. This change builds the structures for search
  only; lifting them into an online lazy engine is explicitly out of
  scope (revisited only if DyNES demand materializes).
- **Validation**: sweep statistics at each candidate must equal the
  eager path's statistics at that candidate on test fixtures
  (bit-identical), the profile curve must be reproduced by brute-force
  per-candidate refits on a small fixture, and bootstrap calibration
  must be exercised on a simulated null.
