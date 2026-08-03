## 1. Measurement + audit phase (design D1 — gates the format choice)

- [ ] 1.1 Measure on both baseline datasets + one synthetic large-n fixture:
      unique-row counts (per event and global), update-slice sizes per event,
      `stat_all_events` memory, and the gather pass's share of end-to-end
      estimation time for REM and coordination; record in `progress.md`
- [ ] 1.2 Audit the DyNAM-MM effect bindings for symmetry: is any asymmetric
      (ego/alter-kind) effect reachable in coordination, or is s_ij = s_ji
      structural? Record the answer (decides one vs two rows per dyad slot)
- [ ] 1.3 Lock the format choice as a design.md amendment (D2 candidate per
      family, with the measured justification); update specs/tasks if the
      fallback (no REM dictionary) is chosen
- [ ] 1.4 Settle the undirected-DEPENDENT REM semantics as a design amendment
      (design D6 gate; see Open Questions): risk-set symmetrization (the REM
      fold does not symmetrize today — decide symmetrize like coordination
      vs. guard-and-abort), one row per undirected dyad vs. both directions,
      and how the triangle/dedup storage treats the case; record the decision
      with the undirected-as-dependent vs. undirected-as-explanatory
      discussion it belongs to
- [ ] 1.5 Confirm the sequencing precondition (design D6):
      refactor-likelihood-compute landed (index-based ragged emit + rewritten
      coordination kernel + `index_i`/`index_j` on the long formats);
      support-constraint-as-stat is already archived (2026-07-10) — the
      availability representation is final

## 2. Coordination storage (design D2.1)

- [ ] 2.1 Store coordination dyads as d = n(n−1)/2 triangle SLOTS (both
      directions map to one slot; one or two stored rows per the 1.2 audit) —
      the diagonal drop, ragged emit, and `twomode_or_reflexive` forcing
      removal already landed with refactor-likelihood-compute (D13); this is
      storage only, behind the fixed `index_i`/`index_j` contract
- [ ] 2.2 Update the coordination kernel's row-reference mapping to the
      triangle slots (what a row reference points into changes; the kernel's
      index-based read contract from refactor-likelihood-compute does not);
      cpp-reviewer; force recompile
- [ ] 2.3 Tests: gather-output equivalence vs the pre-compression path (same
      index-keyed row multiset + selected indices, constrained and
      unconstrained); estimation equivalence vs `default` at 1e-10; memory
      before/after recorded

## 3. REM dictionary + multiset (design D2.2)

- [ ] 3.1 Implement the unique-row dictionary with incremental hash maintenance
      in the gather pass (update → decrement old id, hash new row, increment);
      per-event CSR-style `(row_id, count)` arrays + observed row id; no
      per-event snapshot copy
- [ ] 3.2 Update `compute_multinomial_selection` (REM timed + ordered paths) to
      the aggregated form: per-iteration `exp(unique_rows %*% β)` once, then
      per-event O(u_e) accumulation of normalizer/score/Fisher; observed row
      read exactly (design D3); right-censored intervals from aggregates;
      cpp-reviewer; force recompile
- [ ] 3.3 Tests: gather-output equivalence (expanded multiset == dense rows);
      estimation equivalence vs `default` at 1e-10; per-iteration timing +
      memory before/after recorded

## 4. Exported API + cleanup

- [ ] 4.1 Gather-stack expansion path from the internal
      representation; same-information test against the pre-compression
      output — identical index-keyed row multiset per event, identical
      `selected`/`n_candidates`/`index_i`/`index_j` (design D4, amended:
      multiset equivalence, not byte identity)
- [ ] 4.2 Delete the dense gather path once 2.3/3.3/4.1 are green; cross-engine
      coefficient tests pass; `NOT_CRAN=true devtools::test()` green
- [ ] 4.3 Milestone: bump `DESCRIPTION` + `NEWS.md` (gather memory/time for
      REM/coordination; internal format change, exported API unchanged);
      spawn the spec-conformance agent; `lintr::lint_package()` clean on
      touched files
