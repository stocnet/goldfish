# Recency Effects — Event-Index-Domain Memory

## Why

Recency effects — Butts's inverse-rank statistic and its kin — are the
largest effect-family gap goldfish has against relevent and remstats
(gap #2 in the working catalogue, `.plan/sp/effect_overview.md` §13):
the package's only answer to "do recent partners matter?" is the
time-domain `window =`. Applied work has had to hand-roll them, either
as an O(history)-per-event effect pair or as pre-computed time-varying
covariates truncated at an arbitrary K (reference implementations in
`.plan/recency/`). The formal family in `.plan/sp/k_rank_effects.md`
shows the whole event-index-domain memory axis — rank kernels over
recency-ordered contact lists — is computable exactly and cheaply with
incremental buffers, and that truncation is a kernel choice with a
provable error bound, not a hack.

## What Changes

- **New recency kernel-effect family** (separate named effects, one per
  scope): `recency_send` (rank of j among i's recent targets;
  relevent `RSndSnd` / remstats `rrankSend`), `recency_receive` (rank of
  j among i's recent sources; `RRecSnd` / `rrankReceive`),
  `recency_dyad` (recency of the (i,j) dyad in the event stream;
  `recencyContinue`), and `recency_global` (node-scoped last-K
  event-stream aggregates, usable on the rate side). Each takes
  `kernel` — inverse rank `1/r`, truncated inverse `1/r·1{r ≤ k}`,
  last-k indicator `1{r ≤ k}`, geometric `ρ^r`, raw truncated rank
  `r·1{r ≤ k}` (the applied-reference shape) — and `k` (default `Inf`,
  the exact untruncated path).
- **New estimable rank-band basis**: `kernel = "bands"` emits one
  statistic per rank band (log-spaced dummies), so the fitted
  coefficient curve is the memory kernel itself; nests the fixed
  kernels as shapes and serves as their specification check.
- **New rank-buffer infrastructure**: per-scope move-to-front list of
  distinct keys (O(1) event update, O(deg) risk-set walk) with a ring
  buffer fast path for finite `k` (O(k) memory, implicit eviction);
  localized cache updates (only ranks above the moved key shift).
  State is reconstructible by replaying the update stream from the
  initial state, per the `process-state-evaluators` contract.
- **New `last_k =` restriction modifier** on history-based effects
  (`inertia`, `recip`, …): the effect computes its usual statistic
  (dummy or weighted, transformers intact) on the network built from
  only the last k events in scope — the event-index twin of
  `window =`, implemented on the same build-plan machinery (derived
  network plus deterministic expiry pseudo-events; the k+1-th oldest
  tie expires when a new one arrives). No lazy expiry: the schedule
  sees every state change, as all downstream consumers assume.
- **Model coverage**: DyNAM choice, DyNAM rate, REM, and DyNAM
  choice-coordination (the coordination slice tracks
  `two-sided-coordination`'s move to `estimate_dynamu()` and lands
  behind `coordination-tie-consistency`; see design).
- **Homogenization edits to active changes** (carried by this change's
  tasks, applied in those changes' artifacts): reserve
  `kernel`/`k`/`last_k` argument names and the rank-buffer
  `cache_spec` fields in `effect-term-registry`'s D18 schema surface;
  a consumer note in `tied-event-times` (rank-buffer update order
  consumes whatever tie-order mechanism its D3 settles); inherit the
  canonical t⁻ predictability wording from `two-sided-coordination`'s
  coordination-mechanisms spec instead of restating it; a note in
  `gather-rem-coordination-format` that indicator/band kernels are
  dictionary-friendly while `1/r` produces all-distinct rows.
- **Not breaking**: no existing effect changes numbers; every statistic
  here is new and needs new baselines (cannot ride the frozen
  `global_v1` set).

## Capabilities

### New Capabilities

- `recency-effects`: the kernel-effect family — scope-named effects,
  kernel and `k` argument semantics, t⁻ read discipline, rank-buffer
  computation and its replay contract, rank-band basis expansion, and
  the truncation error bound reported to the user.
- `last-k-restriction`: the `last_k =` modifier — event-count-restricted
  derived networks, their expiry pseudo-event stream, composition with
  `window =`, and which effects accept it.

### Modified Capabilities

None in the living spec: all statistics are new, and the
homogenization edits touch other *active changes'* artifacts (their
designs/specs pre-archive), not `openspec/specs/**`. If
`effect-term-registry` archives before this change implements, its
registry capabilities absorb the reserved-vocabulary edit there.

## Impact

- **Code**: new effect init/update pairs in the
  `R/functions_effects_*` families (registered via the effect
  registry), rank-buffer container (R first; C++ port when the
  backend-parity surface demands it), `last_k` build-plan fulfillment
  next to the window machinery in `R/formula_parser.R` /
  `R/preprocess_builders.R`, registry entries with validity metadata,
  man pages and the pkgdown effect gallery.
- **Sequencing**: proposal now; implementation **after
  `effect-term-registry` (Layer 1) and `effect-naming-scheme`
  (Layer 2)**, post-2.0.0 — this is Layer-3-family work (new
  statistics, new baselines) and targets the registry API, not the
  string-built S3 lookup. Tie-block update semantics declare a
  dependency on `tied-event-times` (its D3 order mechanism);
  coordination-side effects depend on `coordination-tie-consistency`
  and `two-sided-coordination`.
- **Validation**: parity harness against remstats
  (`rrankSend`/`rrankReceive`/`recencyContinue`) and relevent
  (`RSndSnd`/`RRecSnd`) with documented convention mapping; the
  reference implementations in `.plan/recency/functionsEffects.R`
  (choice-coordination inverse-rank cache pair) and
  `.plan/recency/04DyNAMSBM.R` (truncated raw-rank covariate route)
  reproduced as acceptance cases; new frozen baselines minted for the
  new statistics per the baselines rule.
- **Docs**: effect gallery/reference additions; a vignette section
  contrasting time-domain (`window =`) and event-index-domain
  (`last_k =`, kernels) memory.
