## Context

The gather pipeline replays the flat/broadcast update buffers once
(`gather_sender_receiver_model_r`) and snapshots, per event, the statistics
rows of all present dyads into one stacked matrix consumed per Newton
iteration by `compute_multinomial_selection` (REM) /
`compute_coordination_selection` (DyNAM-MM):

```
today (REM/MM):                     sizes (n actors, E events, p effects)
  stat_all_events   E·n² × p dbl    ← dense snapshot per event
  n_candidates(1/2) E int
  selected(_actor*) E int
gather cost:  O(E · n² · p) copy    (stat_mat[idx, ] per event)
estim. cost:  O(E · n² · p) / iter  (every row re-dotted every iteration)
```

Three structural redundancies:
1. **Across events**: consecutive events differ by the few cells in the update
   slice; the snapshot re-stores everything.
2. **Within events**: sparse-network statistics make most dyad rows identical
   (typically the all-zeros/backgound row dominates).
3. **Coordination-specific**: the full directed grid ships including the
   diagonal (`gather_` forces `twomode_or_reflexive = TRUE` for DyNAM-MM), and
   when the effect set is symmetric (s_ij = s_ji), the two directions are
   duplicates of each other.

Constraints: `gather_model_data()` is **exported API** (`NAMESPACE:105`) —
its documented contract is already ragged ("up to" events × actors rows,
per-event `n_candidates`), and after `refactor-likelihood-compute` D13 its
rows carry `index_i`/`index_j`, so the compatibility bar is SAME INFORMATION
(the index-keyed row multiset), not byte identity; the gather backend is NOT
in the frozen baseline grid
(`baselines_backends = c("r", "cpp")`; the frozen `.rds` keys keep their
legacy `default` / `default_c` names), so the equivalence floor
is cross-backend agreement tests, not the frozen files;
`refactor-likelihood-compute` D9/D13 fix the index-based, ragged-safe compute
contract this storage sits behind; `support-constraint-as-stat` (ARCHIVED
2026-07-10) landed the availability consumption in the gather loop — final.

## Goals / Non-Goals

**Goals:**
- Remove the per-event dense duplication for REM and coordination gather
  (memory AND gather-pass time), with a measured decision among the candidate
  encodings.
- Cut per-iteration estimation cost for gather REM via sufficient-statistics
  aggregation (unique rows × counts).
- Preserve `gather_model_data()`'s user-facing shape via on-demand expansion.
- Numerical equivalence with the current gather engine at ~1e-10 on fixtures.

**Non-Goals:**
- No change to the `r` / `cpp` backends or to choice/rate gather
  (already n-sized per event).
- No change to the coordination *estimator algebra* — that is
  `refactor-likelihood-compute` D9; this change only feeds it a better layout.
- No user-facing API change.

## Decisions

### D1 — Investigation before format lock (measure, then choose)
The format decision is gated on measurements taken on the two baseline
datasets plus one synthetic large-n fixture: (a) unique-row counts per event
and globally (how compressible is the row set really), (b) update-slice sizes
(how sparse the across-event deltas are), (c) share of gather time in the
end-to-end estimation, (d) coordination effect-set symmetry in practice.
Rationale: the candidate encodings (D2) have different break-even points —
e.g. the dictionary only wins if u ≪ E·n², which depends on statistic
sparsity; locking the format on paper risks building the wrong one.
*Alternative rejected:* pick the dictionary format now — cheap to validate
first, expensive to unbuild.

### D2 — Candidate encodings (the sketch to choose from)
The design space, from least to most aggressive:

1. **Triangle single-slot storage (coordination)** — the diagonal drop and
   the `twomode_or_reflexive = TRUE` forcing removal land earlier with the
   index-based emit (`refactor-likelihood-compute` D13); here the win is
   storing d = n(n−1)/2 dyad SLOTS with both directions index-mapped to one
   slot (the goldfish_latent Stan `index_pos` pattern). When the effect set
   is symmetric, one stored row per dyad; when not, two rows per dyad.
   Prerequisite: a per-model symmetry audit of the coordination effect
   bindings (are asymmetric effects — ego/alter kinds — reachable in
   DyNAM-MM at all?).
2. **Unique-row dictionary + per-event (row_id, count) multiset (REM)** —
   store `unique_rows (u × p)` once plus, per event, CSR-style
   `(row_id, count)` pairs and the observed row's id. Sufficient for the REM
   likelihood: normalizer = Σ count·e_u, score = Σ count·e_u·s_u, Fisher =
   Σ count·e_u·s_u s_uᵀ — computed once per iteration over u rows, then
   per-event O(u_e) scalar accumulation. Maintained incrementally during the
   gather pass with a row hash (update → decrement old id, hash new, increment),
   which also removes the O(n²·p) per-event snapshot copy from the gather
   pass itself.
3. **Delta/base hybrid** — store a base snapshot + per-event deltas and replay
   inside the estimator: rejected as a primary candidate because it converges
   to what the `cpp` backend already is (replay per iteration), erasing the
   gather backend's reason to exist; kept in the sketch only as the fallback
   if (b) shows the dictionary doesn't compress.

Coordination does NOT get the multiset aggregation: the pairwise weights
p(i→j)·p(j→i) and deviations D_d = s_ij + s_ji − E_i − E_j need per-dyad
identity (E_i is sender-specific), so its computation stays per-dyad
(rewritten in `refactor-likelihood-compute` D9); its gains here are storage:
diagonal drop, triangle, symmetry dedup, and optionally the row dictionary as
pure storage with per-dyad row_ids.

**The ragged/support-mask path is NOT this change's job (moved 2026-07-10).**
The index-based ragged emit (per-row `index_i`/`index_j`, per-sender CSR
offsets, dyad-pairing indices), the diagonal drop, and the lifting of the
constrained-coordination redirect onto the `cpp` backend all land in
`refactor-likelihood-compute` (its D9/D13, tasks 5.7–5.9) — the earlier split
(compute half there, emit half here as task 2.4) left a consumer with no
producer and was dissolved. By the time this change starts, the kernel
already consumes an index-identified dyad list, ragged or full. This change's
storage encodings therefore only redefine WHAT a row reference points into
(dictionary id, triangle slot, deduped symmetric row); the index layer and
the kernel's read contract are fixed inputs here, not design freedom.

### D3 — Observed-event fidelity is per-event, not aggregated
Whatever the encoding, the observed dyad's own statistics row enters the
likelihood exactly (score += s_obs, logL += x_obs): the format SHALL keep a
per-event pointer to the observed row's exact values (dictionary id or
explicit row), never reconstructing it from aggregates. Right-censored
intervals (timed REM) carry aggregates only — they have no observed dyad —
so the multiset suffices there.

### D4 — gather_model_data() expands on demand, verified as same information
The exported `gather_model_data()` keeps its documented output by
materializing from the internal representation (dictionary lookup / triangle
expansion) at call time. The compatibility bar is **self-describing
equivalence, not byte identity** (amended 2026-07-10): the expansion SHALL
reproduce the same index-keyed row multiset — identical
(`index_i`, `index_j`, statistics) tuples per event, identical
`selected`/`n_candidates` — as the pre-compression output. Byte identity was
the original bar, but it protected an unwritten positional row order that the
`index_i`/`index_j` columns (landed by `refactor-likelihood-compute` D13)
make irrelevant, and the multiset comparison is the more meaningful test
(order is free, content is pinned; still deterministic re-indexing, no float
arithmetic). *Alternative rejected:* changing the exported information
content — user code and vignettes depend on it, and the expansion is cheap
relative to a user-side model fit.

### D5 — Equivalence floor: cross-engine, not frozen files
The gather engine is outside the frozen-baseline grid, so the floor is:
per-event logL/score/information agreement with the `default` engine at 1e-10
on the fixture models (reusing the harness pattern from
`refactor-likelihood-compute` D4), plus coefficient agreement in the existing
`test-cpp_interface.R` cross-engine tests. Gather-pass output equivalence
(same candidate multiset, same observed rows) is asserted directly against
the current gather output before the old path is deleted.

### D6 — Sequencing: after refactor-likelihood-compute; discussion-gated
Implement after `refactor-likelihood-compute` (its D9/D13 fix the index-based
compute contract and the ragged emit this storage sits behind; building the
format against the old estimator means wiring it twice).
`support-constraint-as-stat` is ARCHIVED (2026-07-10): the
availability/`active_dyad` representation the multiset maintenance consumes
is final. No task-level coupling to any other change remains — this is plain
ordering. Additionally, implementation is gated on the §1 discussion phase:
the dyad-model gather cases this change exists for (notably
undirected-dependent REM) have no real usage yet and the current code does
not handle them properly, so the format is settled by discussion + the D1
measurements before building. The measurement phase (D1) can start any time.

## Risks / Trade-offs

- **The dictionary may not compress for dense-statistics models** (e.g. many
  continuous covariate effects make rows unique) → D1 measures first; the
  fallback is candidate (1) alone (diagonal/triangle wins are unconditional
  for coordination) and keeping dense rows for REM.
- **Hash-maintenance correctness during gather** (decrement/increment on every
  cell update) → gather-output equivalence test against the current stacked
  rows before deletion (D5).
- **Symmetry assumption for coordination dedup** → gated on the effect-binding
  audit (D1d); if asymmetric effects are reachable, ship the two-directed-rows
  triangle variant instead (still no diagonal).
- **`gather_model_data()` drift** → byte-identical expansion test (D4).
- **In-flight changes touching the same files** → D6 sequencing;
  `support-constraint-as-stat` landed (2026-07-10); this change stays in
  proposal state until `refactor-likelihood-compute` lands and the §1
  discussion settles the format.

## Migration Plan

1. Measurement phase (D1) — no code change; record findings in `progress.md`
   and lock the format choice as a design amendment.
2. Coordination storage: triangle single-slot indexing (+ symmetry dedup if
   the audit allows) behind the fixed index contract — the diagonal drop and
   ragged emit already landed with `refactor-likelihood-compute` — row-ref
   mapping updated, equivalence green.
3. REM dictionary + multiset gather and estimator consumption, equivalence
   green, per-iteration timing recorded.
4. `gather_model_data()` expansion path + byte-identical test.
5. Delete the dense gather path; cross-engine floor green; record
   before/after memory + timing.

Rollback: each phase is independently revertible; the dense path is deleted
only in the final step.

## Open Questions

- **Undirected-DEPENDENT REM semantics** (the general discussion this change
  is gated on): an undirected dependent network in REM is a case with no real
  usage yet that the code does not consider properly. Open points: (a) the
  risk set — should the mask be weakly symmetrized as coordination's is
  (`support[i,j] ∩ support[j,i]`, both directions FALSE if either is)?
  `fold_active_dyad_support_rem` does NOT symmetrize today, so if
  undirected-dependent REM is reachable, a constraint on it is currently
  mis-folded — decide symmetrize vs. guard-and-abort; (b) should the gather
  emit one row per undirected dyad or both directions (ties into the
  undirected-as-DEPENDENT vs undirected-as-EXPLANATORY storage/update
  discussion parked in `refactor-likelihood-compute`'s Non-Goals — the
  preprocessing double-evaluation of undirected updates); (c) whether the
  triangle/dedup storage should treat this case as coordination-like
  (symmetric slots) or REM-like (directed multiset).
- Are asymmetric (ego/alter-kind) effects reachable in DyNAM-MM's effect
  bindings, or is the coordination effect set structurally symmetric? (Decides
  one-row vs two-rows-per-dyad triangle; D1d audits this.)
- Should the unique-row dictionary be global (across events) or per-block
  (bounded memory for the id arrays on very long sequences)? Measure first.
- Does the multiset representation subsume the right-censored interval rows in
  timed REM cleanly (aggregates only), or do interval boundaries need their
  own pointer structure beyond the current `is_dependent` split?
- Whether choice/rate gather (n rows per event) eventually adopts the same
  dictionary for uniformity — out of scope here, note for later.
