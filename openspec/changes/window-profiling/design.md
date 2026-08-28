# Design — window-profiling

## Context

The eager window architecture (derived network + expiry pseudo-events
merged into the schedule) is what estimation, residuals, replay, and
the recipe loop assume; `process-state-evaluators` requires state at
any event index to be recoverable by replaying the preprocessed update
stream. That architecture prices a candidate search at one full
build-plan + preprocess per candidate. The source document
(`.plan/sp/window_profiling_spec.md`) separates two jobs: an *online
engine* for estimation at a chosen window (the negation-event scheme,
kept) and a *profile engine* for search over candidates (per-key
sorted event-time arrays, monotone two-pointer annulus sweep,
FIFO/hash-keyed multisets). The profile log-likelihood is a step
function whose breakpoints are the observed lags, so the exhaustive
grid is finite and known before any fit. `recency-effects` (ADR-0026)
adds the event-index memory family whose parameters (`k`, `last_k`,
geometric ρ) want the same selection machinery, and left geometric-ρ
profiling as an open question this change resolves. Decisions cite
ADR-0028.

## Goals / Non-Goals

**Goals**

- Evaluate all candidate windows in one data traversal; fit along the
  grid with warm starts; select and refit eagerly.
- One profiling surface for ω (continuous, lag grid), (ω_s, ω_l)
  (constrained 2-D), integer `k`/`last_k`, and geometric ρ.
- Honest inference for the selection (Davies sup-LR bound; parametric
  bootstrap).

**Non-Goals**

- No change to the online estimation engine: the eager negation-event
  scheme stays; no lazy expiry in fitted models.
- No free window per effect (multiplies the Davies problem and the
  compute; effect-specific heterogeneity is tested post hoc by
  profiling one effect's window with the shared ones fixed).
- No Rcpp in the first pass — the R implementation is the test oracle
  the port would be gated on.
- No GAM smooth event-age diagnostic (Juozaitienė–Wit ϕ) in v1 — noted
  in docs as the exploratory companion; may become its own change.

## Decisions

### D1: Two engines, two contracts — the profile engine is exempt from replay (ADR-0028)

The replay contract exists for *fitted models*: residuals,
diagnostics, `simulate()`, and DyNES consume a preprocessed object and
must be able to reconstruct its state at any event index. A profile
sweep's per-candidate states are search intermediates: nothing
downstream ever addresses them, only the profile curve and the argmax
leave the engine. Therefore the sweep runs on lazy structures
(sorted-array pointers, FIFO queues, hash-keyed multisets) that never
touch the schedule, and the **selected candidate is refit through the
ordinary eager path**, restoring every downstream guarantee. The
correctness gate is agreement: the sweep's statistics at the selected
candidate must equal the eager path's bit-identically. Alternatives
rejected: making the sweep replay-compliant (C materialized expiry
streams is exactly the cost being removed); making the online engine
lazy too (breaks every consumer of schedule-visible state for a
benefit that only exists during search — revisit only if DyNES
incremental augmentation creates real demand, since eager expiry
streams are derived data that sequence edits invalidate).

### D2: Grid = observed-lag quantiles, endpoints pinned, user-overridable

The profile is a step function with breakpoints at observed lags, so
candidates between breakpoints are wasted fits. Default grid: lag
quantiles (2.5%, 5%, …, 97.5%) plus the data-resolution and max-lag
endpoints; `candidates =` overrides. The grid is computed once from
the event stream and recorded on the profile object (the bootstrap
must reuse the *same* grid — the sup is over a fixed set).

### D3: Two-window variant is a constrained 2-D sweep

ω_s < ω_l enforced on the grid (equality is exact collinearity);
sweep ω_l incrementally within each ω_s with warm starts along both
axes. The disjoint-band reparameterization ([0, ω_s], (ω_s, ω_l]) is
offered for conditioning and interpretability; same span, same
engine.

### D4: Event-index grids ride the same surface, and are cheaper

- Integer `k` (recency truncation and `last_k`): one buffer walk
  yields the statistics at *every* k ≤ k_max simultaneously (the walk
  emits ranks; each candidate is a prefix). The grid is tiny
  (K = 1…8 covers the participation-shift literature; powers of two
  beyond that) — no annulus machinery needed, only the shared
  fit-along-grid, warm-start, and inference layers.
- Geometric ρ: the rank walk is shared across all candidates; each
  candidate statistic is the transform ρ^r of the same rank stream.
  Profiling ρ is therefore a per-candidate reweighting of one
  precomputed rank basis — the same "one traversal, C evaluations"
  shape as the annulus sweep. This resolves `recency-effects`' open
  question: geometric ρ ships fixed-only there, and this change is
  the machinery that profiles it.
- Composition ("last K within ω") profiles one axis holding the other
  fixed; the joint grid is supported but documented as expensive and
  rarely identified.

### D5: Inference — Davies bound default, bootstrap honest, intervals caveated

Under H₀ the window is unidentified, so the sup-LR over the grid is
anticonservative against χ². Default report: sup-LR with a
Davies-type upper bound on the p-value (cheap — uses the profile
curve already computed). Honest option: parametric bootstrap —
simulate from the fitted *baseline* (unwindowed) model, recompute the
sup over the same grid per replicate, empirical p-value;
embarrassingly parallel and priced by the sweep engine. The bootstrap
leg is gated on `process-simulation` providing `simulate()`; until it
lands, the Davies bound is the only calibrated test and the docs say
so. Profile-likelihood intervals ({ω: pl(ω̂) − pl(ω) ≤ χ²₁(0.95)/2})
are reported with the explicit caveat that ω is not a regular
parameter (step-function likelihood); the bootstrap gives coverage
when it matters.

### D6: Warm starts along the grid are the default fit strategy

Adjacent candidates differ by an annulus of events, so coefficients
move little; each fit starts from its neighbor's estimates. The 2-D
sweep warm-starts along both axes. A cold-start fallback triggers on
non-convergence, and the profile object records which fits needed it.

### D7: R-first with the structures named, Rcpp gated behind the oracle

R implementation on fastmap/collections-style structures (hash-keyed
multisets with decrement-and-test-for-zero for indicator/threshold
statistics; plain FIFO — constant window means insertion-order expiry,
no priority queue). The Rcpp port (flat hash map, 64-bit packed dyad
keys, contiguous arrays with head indices) is a separate task group
strictly behind the R oracle, activated only if profiling shows the R
engine binding on real data. Statistics linear in counts use the
decrement-only running sum; max/min-type statistics defeat lazy
deletion and are documented as unsupported by the sweep (they fall
back to per-candidate recompute).

### D8: One exported surface, name aligned with the estimation-naming pass

A single generic — working name `profile_memory(fit, parameter =
c("window", "short_long", "last_k", "k", "rho"), candidates = NULL,
test = c("davies", "bootstrap"), ...)` — returning a
`goldfishProfile` object (class named per `class-naming-scheme`'s
camelCase `goldfish<Thing>` rule, ADR-0031; updated 2026-08-19 from the
earlier `_goldfish`-suffix spelling) with print/plot methods (profile
curve, selected value, interval, test). The name is flagged for the
`algorithm-naming`/estimation-naming review before export; `profile_`
follows the `stats::profile` precedent rather than `diagnose_*`
(selection, not diagnosis).

## Risks / Trade-offs

- [Sweep and eager path drift apart numerically] → the bit-identical
  agreement gate at the selected candidate is a test, not a hope; any
  future change to window semantics must keep both paths or fail CI.
- [Davies bound too conservative on fine grids] → report the bound
  alongside the raw sup and the grid size; docs recommend the
  bootstrap for borderline calls.
- [Warm starts drag a bad optimum along the grid] → cold-start
  fallback on non-convergence plus a randomized cold restart at the
  argmax before reporting.
- [Bootstrap blocked on process-simulation] → Davies leg ships
  independently; the bootstrap task group is gated and the profile
  object is designed to accept the test result later.
- [Users read ω̂ as a regular MLE with a regular CI] → the step-caveat
  is printed with the interval, not buried in docs.
- [Scope creep toward a lazy online engine] → explicit non-goal;
  D1 records the only condition for revisiting (DyNES incremental
  augmentation demand).

## Migration Plan

No migration: additive surface. Internal ordering: ω engine + Davies
on existing windowed effects first (no dependencies), then the 2-D
variant, then the k/ρ legs when `recency-effects` lands, bootstrap
when `process-simulation` lands.

## Open Questions

- Exported name (`profile_memory` vs a `profile_window`/`profile_rank`
  split) — settle in the estimation-naming review.
- Default k_max for the integer grid (8 per the literature, or scale
  with observed degree?).
- Whether the disjoint-band reparameterization should be the *default*
  presentation of the two-window variant rather than an option.
- Case-control risk-set sampling inside the sweep (the source document
  says it composes) — v1 or later.
