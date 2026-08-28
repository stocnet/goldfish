## Why

The `gather` backend materializes, for REM and DyNAM-coordination, **every
present dyad's statistics row for every event** (`gather_sender_receiver_model_r`,
`R/cpp_interface.R:1054`: `stat_mat[idx, ]` stacked into `stat_all_events`) —
O(n_events × n²) × p doubles with no deduplication, even though (a) consecutive
events change only the few cells in the flat-update buffer, (b) sparse-network
statistics make most dyad rows identical within an event, (c) coordination ships
the full directed grid *including the never-used diagonal* (`gather_` forces
`twomode_or_reflexive = TRUE` for DyNAM-MM, `R/cpp_interface.R:783`) where only
n(n−1)/2 distinct dyads exist — the diagonal drop itself now lands earlier,
with the index-based emit in `refactor-likelihood-compute` (its D13); the
remaining coordination redundancy here is the two directed rows per kept dyad
and the across-event duplication — and (d) the gather pass itself pays an
O(n²·p) copy per event. This defeats the engine's purpose (compact precomputation
that avoids per-iteration update replay) exactly for the two model families where
per-event work is n²-sized — the same families that dominate estimation time.
This change rethinks the gather format for the dyad models; it was parked as an
out-of-scope opportunity in `refactor-likelihood-compute`.

## What Changes

- **Rework the gather representation for REM (timed + ordered) and
  DyNAM-coordination** from per-event dense row stacks to a compressed
  representation; the design sketches the candidate encodings and the
  considerations for choosing among them (unique-row dictionary + per-event
  multiplicity counts; triangle/dyad indexing for coordination; delta/CSR
  hybrids), with an investigation-and-measurement phase BEFORE the format
  decision is locked.
- **REM is the aggregation win**: its per-event likelihood terms (normalizer,
  score, Fisher) are sums over the *multiset* of candidate rows, so unique rows
  + counts are sufficient statistics — per-iteration cost drops from
  O(total_rows × p) to O(unique_rows × p) with per-event O(u_e) accumulation.
  Note (2026-08-19, from `recency-effects`): kernel choice there decides how
  compressible its rows are — last-k indicator and rank-band kernels give few
  distinct values with huge multiplicities (near-ideal dictionary input),
  while the inverse-rank `1/r` kernel makes every contacted partner's row
  distinct and degrades the dictionary; the format decision should not assume
  all future statistics are indicator-like.
- **Coordination is the storage win**: the pairwise weights p(i→j)·p(j→i) need
  per-dyad identity, so computation stays per-dyad (that redesign lives in
  `refactor-likelihood-compute` D9, which also lands the index-based ragged
  emit — per-row `index_i`/`index_j`, per-sender offsets, dyad pairing — the
  diagonal drop, and the lifting of the constrained-coordination redirect);
  here the gains are triangle single-slot storage, symmetric-statistics dedup
  (when the effect set is symmetric), and row-dictionary storage — changes to
  what the index layer points into, never to which rows exist or how the
  kernel reads them.
- **Scope boundary settled 2026-07-10**: everything needed for estimation to
  WORK (ragged emit, redirect lift, index metadata) moved to
  `refactor-likelihood-compute`; this change is pure storage compression plus
  the open semantic discussion (undirected-dependent REM, coordination
  effect-set symmetry) and SHALL NOT start implementation before that
  discussion settles the format.
- **Estimation consumers updated** (`compute_multinomial_selection`,
  `compute_coordination_selection`) to read the new representation; the
  `cpp` and `r` backends are untouched.
- **The exported gather stack keeps its documented output shape** —
  expanded/materialized from the internal representation on demand, so user
  code is unaffected. **BREAKING** (internal only): the gather intermediate
  structures change.

## Capabilities

### New Capabilities
- `gather-dyad-format`: the internal gather representation for dyad-indexed
  models (REM, DyNAM-coordination): compressed storage contract (no per-event
  dense duplication of unchanged rows, no diagonal rows for one-mode
  coordination), numerical-equivalence floor against the pre-change engine,
  and the compatibility contract for the exported gather stack.

### Modified Capabilities
<!-- none yet: no existing spec covers the gather internals; if the
     investigation reveals `preprocess-output-writers` requirements are
     touched, a delta will be added then -->

## Impact

- **Code**: `R/cpp_interface.R` (`gather_sender_receiver_model_r`, `gather_`),
  `src/compute_multinomial_selection.cpp`,
  `src/compute_coordination_selection.cpp`, `R/preprocess_export.R`
  (gather-stack expansion path). cpp-reviewer on every `src/` diff.
- **Memory/time**: gather preprocessing and gather estimation for REM/
  coordination; unchanged for choice/rate (already n-sized per event).
- **Tests**: equivalence vs the current gather backend at tight tolerance;
  frozen baselines PASS (the gather backend is not part of the baseline grid —
  verified: `baselines_backends = c("r", "cpp")`, with the frozen `.rds` keys
  keeping their legacy `default` / `default_c` names — so cross-backend
  agreement tests carry the floor here).
- **Sequencing**: after `refactor-likelihood-compute` — plain ordering,
  no task-level coupling remains (its D9/D13 fix the index-based compute
  contract and the ragged emit this change's storage sits behind; its former
  pairing with task 2.4 here was dissolved 2026-07-10 by moving the emit half
  there) — and after `backend-parity` (added 2026-07-25), which rewrites the
  same three gather kernels first: it threads `index_i` / `index_j` into
  their signatures, adopts the shared stable softmax, and adds the per-event
  reduction accumulators, all of which this change's new storage format must
  feed rather than fork. `support-constraint-as-stat` is ARCHIVED (2026-07-10), so the
  availability representation in the gather loop is final. Implementation
  additionally gated on the §1 discussion phase (undirected-dependent REM
  semantics, symmetry audit) — this case has no real usage yet and the
  current code does not consider it properly, so the format must be settled
  by discussion before building.
