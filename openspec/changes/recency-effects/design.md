# Design — recency-effects

## Context

goldfish has no recency effects (working catalogue gap #2); the nearest
mechanism is the time-domain `window =`, which is eager and
materialized — `parse_time_windows()` derives a zero-filled network copy
and injects dissolution pseudo-events into the ordinary event schedule,
which the recipe loop, residuals, and the C++ replay all assume exist.
The source document (`.plan/sp/k_rank_effects.md`) defines the
event-index-domain memory family: statistics κ(r_ij(t)) of the rank of
a key in a recency-ordered list per scope, computable incrementally.
Two working reference implementations exist locally:
`.plan/recency/functionsEffects.R` (a choice-coordination
`recencyInertia` init/update pair maintaining a rank matrix with
localized updates) and `.plan/recency/04DyNAMSBM.R` (truncated raw rank
via `dense_rank()` pre-computation fed as a time-varying covariate).

Constraints from the rest of the programme: `effect-term-registry` is
Layer-1-only (its D13) and reserves schema surface for successors (its
D18); the living spec already uses "rank" to mean *alternative rank
within a risk set* (model-evaluation-pass, diagnostic-primitives,
backend-primitive-parity); tied event times have no representable order
in the data model until `tied-event-times` settles its D3;
`parametric-rates` D8 aborts Weibull fits on zero waiting times;
`coordination-tie-consistency` documents that discrete-valued
statistics multiply exactly-tied alternatives under a comparison that
currently splits across backends; `process-state-evaluators` requires
state at any event index to be recoverable by replaying the update
stream. Decisions below cite ADR-0026 (decision record).

## Goals / Non-Goals

**Goals**

- Ship the literature recency effects with parity targets
  (relevent/remstats) and the estimable rank-band generalization.
- Make truncation a first-class kernel with a reported error bound.
- Provide the `last_k =` event-count restriction as the event-index
  twin of `window =`, on the same build-plan machinery.
- Leave the rank-buffer state replayable and registry-declared.

**Non-Goals**

- No lazy expiry or schedule-invisible state (contradicts the window
  architecture and the replay contract).
- No participation-shift effects (they ride the same buffer later; the
  buffer design must not preclude them).
- No changes to existing effects' numerics; no touching frozen
  baselines beyond minting new ones for new statistics.
- No time-domain decay work (that is `effect-statistic-extensions`
  D22 territory).

## Decisions

### D1: Standalone change, implemented after Layers 1+2 (ADR-0026)

`recency-effects` is its own change, a sibling of the Layer-3
`effect-statistic-extensions` successor, implemented after
`effect-term-registry` (registry + `cache_spec` machinery to declare
into) and `effect-naming-scheme` (the naming surface it must be born
compliant with), post-2.0.0. Alternatives rejected: amending
`effect-term-registry` (contradicts its settled D13 — Layer 1 is
numerics-frozen and baseline-gated; these are new statistics needing
new baselines) and folding into `effect-statistic-extensions` (the
rank-buffer infrastructure and parity harness are a coherent reviewable
unit on their own; the extensions change is already large).

### D2: User-facing vocabulary avoids bare "rank"; scope-named effects

The spec token "rank" is owned by the risk-set-alternative meaning
(backend parity requires exact rank agreement across backends). The
family is named **recency**: `recency_send()`, `recency_receive()`,
`recency_dyad()`, `recency_global()` — separate named effects per
scope, following the `indeg`/`outdeg`, `common_sender`/
`common_receiver` precedent, each separately registrable with its own
validity metadata (avoids the D23-style argument-dependent validity
branching a single `recency(scope =)` would need). Internal
documentation may say "rank buffer"; spec prose says "recency rank"
when the ordering position is meant, never bare "rank". Names comply
with Layer-2 (D19) conventions from birth.

### D3: Kernel set and defaults

`kernel` is an enum (noun, character values, default first):
`"inverse"` (1/r, Butts; default), `"indicator"` (1{r ≤ k}),
`"geometric"` (ρ^r, fixed ρ argument), `"rank"` (raw truncated rank
r·1{r ≤ k}, the applied-reference shape — value 0 beyond k), and
`"bands"` (estimable basis, D9). `k = Inf` by default: the exact
untruncated statistic on the move-to-front path; any finite `k`
truncates the chosen kernel (so "truncated inverse" is
`kernel = "inverse", k = 100`, not a separate kernel). Rationale: the
error of truncation is |θ|·sup_{r>k} κ(r) — for 1/r at most
|θ|/(k+1) — so k is an accuracy/model choice the user makes explicitly;
defaulting to exactness follows the source document's own
choose-k-from-the-bound logic. The fitted summary reports the bound for
finite k, and the documented diagnostic is refit at k and 2k — the
two-point special case of the k-grid profiling that `window-profiling`
provides in general.

### D4: Two surfaces with distinct semantics — kernel effects vs `last_k =`

The kernel effects return κ(recency rank of the evaluated dyad/key):
they *weight the evaluated alternative by its recency*. The `last_k =`
modifier instead *restricts the network the statistic is computed on*:
`inertia(net, last_k = 50)` computes ordinary inertia (dummy or
weighted, transformers intact) on the network built from only the last
50 events in scope — no rank transform anywhere in its output. The two
must not be conflated in docs or spec: `recency_send(net)` ≠
`inertia(net, last_k = k)` for any kernel. `recency_*` effects do not
accept `last_k` (their `k` already truncates the ordering);
`window =` and `last_k =` on the same term compose by intersection
(events both recent-in-time and recent-in-count).

### D5: Rank buffer — MTF list with ring-buffer fast path, registry-declared cache

Per-scope state is a move-to-front structure over distinct keys
(partners for sender/receiver scopes, dyads for dyad scope, events for
global scope): O(1) update on an event, full risk-set evaluation by one
walk assigning ranks (O(deg), O(min(deg, k)) truncated), localized
cache updates (only keys ranked above the moved key's former position
shift, by +1). For finite k the buffer is a k-slot ring with a
membership hash — implicit eviction, O(k) memory per scope — and this
is the same structure a future participation-shift/last-K family needs,
so it is shared, not private to recency. The reference implementation's
rank-matrix cache (`.plan/recency/functionsEffects.R`) is the
R-language seed: it demonstrates the localized update against a dense
cache and pins two semantics adopted here — **tie withdrawals and
deletions leave the recency ordering unchanged** (a dissolution is not
a contact), and an event that leaves the key at rank 1 is a no-op.
Cache fields are declared through the registry's `cache_spec` (the
reserved-surface edit in `effect-term-registry` D18 carries the field
names), and the state is reconstructible by replaying the update stream
from the initial state — no hidden side state — satisfying
`process-state-evaluators`. Implementation is R-first; a C++ port
happens only when the effect joins the C++ engine surface, gated by the
backend-parity discipline.

### D6: `last_k =` is eager, on the window build-plan machinery

The k+1-th oldest contributing event's contribution expires
deterministically the moment a new event enters scope, so `last_k =`
materializes exactly like `window =`: a derived network copy plus a
stream of expiry pseudo-events merged into the schedule — count-driven
instead of clock-driven. Lazy expiry is rejected: every downstream
consumer (recipe loop, residuals, C++ replay) assumes schedule-visible
state changes, and `process-state-evaluators` forbids
non-replayable state. The build-plan (`expands_to`) route from
`effect-term-registry` D16 is the interception point, which also gives
`refactor-single-data-object` a single place to see the derived object.
Panel layers reject `last_k` exactly as they reject `window` (expiry
would reset every panel tie).

### D7: Tie blocks — contract stated, order mechanism consumed from `tied-event-times`

The contract: within a block of tied event times, all statistics read
the ordering frozen at the block's start (t⁻), and the buffer pushes
happen once at block end, making the block order-invariant
(freeze-then-update). But the data model today has no representable
order among tied events — that is precisely `tied-event-times` D3, and
its resolution (secondary key vs order column vs pre-sort hook) is
open. This change therefore *declares a consumer dependency*: the
freeze-then-update requirement is specified now, and the block boundary
definition binds to whatever `tied-event-times` ships. Until it lands,
behavior on tied times is data-order-dependent like the rest of the
package and is documented as such. The fractional index-advance rule
for dependent events affects the event *counter* consumed by
`recency_global`/`last_k` counting, not the recency *ordering*; the
interaction is pinned by test, not prose.

### D8: t⁻ discipline is inherited, not restated

The canonical predictability wording lives in `two-sided-coordination`'s
coordination-mechanisms spec ("Every statistic SHALL be computed from
the state at t⁻ …") and its design D8. This change's spec states the
requirement once for the recency family and cites that formulation
rather than inventing a variant; the homogenization edit adds the
cross-reference in both directions. Concretely: the buffer is read
before the current event is pushed, so an event never predicts itself.

### D9: Rank-band basis rides the expansion machinery

`kernel = "bands"` expands one formula term into one statistic per
band via the same build-plan/expansion route as categorical expansion
(`effect-term-registry` D16/D20): generated terms named
`<effect>_band_<lo>_<hi>`, decoder metadata keeps the parent term for
`coef()`/`tidy()` grouping. Default bands are log-spaced —
{1}, {2–3}, {4–7}, {8–15}, {16–31}, {32+} — overridable via a
`bands =` argument (boundary vector). The fitted γ̂ curve is the memory
kernel; fixed kernels are its shapes (1/r a smooth decrease, indicator
a step, geometric log-linear), so a fixed-kernel fit's specification
check is the LR against the bands fit. Collinearity on sparse
histories is real: the man page says so, and
`identifiability-diagnostics` (null-space attribution) is the
diagnostic consumer — a band never occupied is a structural zero
column it will name.

### D10: Compensator-exactness claim is scoped

Every statistic in the family is a step function changing only at
events, so the compensator is exact — without quadrature — under Cox
partial likelihood and exponential waiting times. Under
`parametric-rates`' transformed exposures the claim holds only if the
exposure transform is integrated per constant segment (to check
against its D5 accumulator during that change's implementation, not
asserted here), and Weibull aborts on tied waiting times regardless
(its D8), so the tied-time regime of D7 cannot reach a Weibull fit at
all. The spec states exactness for Cox/exponential and defers the
transformed-exposure statement to `parametric-rates`.

### D11: Coordination slice lands last, behind its gatekeepers

Discrete kernels (indicator, truncated anything) make large blocks of
exactly-equal alternatives the *normal* case — the trigger condition
`coordination-tie-consistency` documents for backend rank divergence.
The coordination-side recency effects (seeded by the reference
implementation, which is itself a choice-coordination pair) therefore
implement only after `coordination-tie-consistency` lands, and target
the surface as reshaped by `two-sided-coordination`
(`estimate_dynamu()`; rank statistics flow through the directed p_ij
unchanged, per that change's derivations). DyNAM choice, rate, and REM
do not wait on either.

### D12: Validation = parity harness + reference-implementation acceptance

Three legs. (1) A parity harness in the style of
`.plan/residuals_comparison.qmd`: goldfish `recency_send`/
`recency_receive`/`recency_dyad` against remstats `rrankSend`/
`rrankReceive`/`recencyContinue` and relevent `RSndSnd`/`RRecSnd` on
shared data, with an explicit convention-mapping table (t⁻ reads, tie
handling, first-contact value) — differences must be explained by the
table, not tolerated. (2) The reference implementations as acceptance
cases: bit-identical statistics to `functionsEffects.R`'s cache pair on
its own example data (k = Inf, inverse kernel), and reproduction of the
`04DyNAMSBM.R` truncated raw-rank covariate stream
(`kernel = "rank"`, finite k). (3) New frozen baselines minted for the
new statistics under the baselines rule; ordinary testthat fixtures
cover buffer edge cases (empty history, single partner, eviction at k,
localized-update correctness against a full recompute).

## Risks / Trade-offs

- [Bands collinear on sparse histories] → default bands few and
  log-spaced; man-page warning; `identifiability-diagnostics` names
  empty bands; LR-vs-fixed-kernel as the recommended workflow.
- [`1/r` kernels degrade the gather dictionary] → note added to
  `gather-rem-coordination-format` (all-distinct rows), indicator/band
  kernels documented as the dictionary-friendly choices for REM-scale
  data.
- [Tie-order mechanism slips] → D7 keeps this change shippable without
  it (documented data-order dependence), with the contract ready to
  bind when `tied-event-times` lands.
- [Registry schedule slips (54 open tasks)] → this change stays
  proposal-complete; no implementation before its dependencies, per
  D1. The reserved-vocabulary edit costs `effect-term-registry`
  nothing at runtime.
- [Ring buffer vs MTF divergence] → the finite-k path must equal the
  truncated Inf-path statistic by construction; a property test pins
  ring-buffer output against the MTF walk truncated at k.
- [Scope creep toward participation shifts] → buffer API designed for
  shared use but P-shifts stay out of scope (Non-Goals); only the
  structure is shared.

## Migration Plan

No user-visible migration: all surfaces are new. Ordering within the
change: buffer + `recency_send`/`recency_receive` on DyNAM choice
first (parity targets richest), then REM and `recency_dyad`, then
rate/`recency_global`, then `last_k =`, then bands, coordination last
(D11). Homogenization edits to the four active changes happen at
proposal time (they are text edits to un-archived artifacts and must
precede those changes' archives).

## Open Questions

- Exact default band boundaries (log-spaced proposal above) — settle
  against real data during implementation.
- ~~Whether `"geometric"`'s ρ is ever profiled~~ Resolved 2026-08-19:
  fixed-only in this change; profiling ρ is `window-profiling`'s job
  (its shared-rank-walk grid — ranks computed once, each candidate a
  ρ^r transform — plus the Davies/bootstrap selection inference).
- The `recency_global` aggregate menu for the rate side (rank of last
  activity, distinct partners in last k, share of last k with property
  P) — which subset ships v1.
- Which effects accept `last_k =` in v1 beyond `inertia`/`recip`
  (closure effects need a defined restricted-network semantics for
  two-path counting).
