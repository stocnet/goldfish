**Every gap here is behaviorally neutral, so every detector must be verified by
making it fail.** Write the test, revert the fix locally, watch it fail, restore
the fix, and record what the failure said in `progress.md`. A test that asserts
the wrong thing passes in both worlds and looks exactly like one that works —
the predecessor shipped two of those.

`NOT_CRAN=true` with the frozen 1e-6 coefficient baselines and the C++ goldens
PASS not SKIP at every commit. The interaction path is covered by them, so they
are a real detector here.

## 1. The sender branch joins the shared write

- [ ] 1.1 Detector first: `tracemem` reports zero duplications of the
      sender-branch operand buffer across a multi-event walk, with a
      deliberate-copy control in the same test. **Capture `type = "output"`**:
      `tracemem` reports on stdout, and a test capturing `"message"` observes
      nothing and passes regardless. Confirm it fails against the current tree.
- [ ] 1.2 Route `R/model_preprocess.R:872` and `R/preprocess_joint.R:993`
      through `write_entries()`. The buffers are materialized fresh at seeding
      and live in one environment binding, which is the aliasing precondition
      the in-place write needs; state that reasoning in the comment, in code
      terms, not as a pointer to a decision record.
- [ ] 1.3 Verification: `NOT_CRAN=true`, baselines PASS not SKIP. The operand
      values and the product column are byte-identical.

## 2. One collapse, consumed everywhere (design D2)

- [ ] 2.1 Detector first: an alter-kind operand's write passes exactly one
      entry, not n1 copies of it. Confirm it fails today, where the parity toy
      fixture passes four entries with one unique value.
- [ ] 2.2 Extract the collapse both paths need — an effect's
      `(node1, node2, replace)` block to a collapsed delta at kind `k` — and
      route the dyad operand path and the constraint-atom path through it. Both
      already call `project_entries()` with `from == to`.
- [ ] 2.3 Route `broadcast_entries_from_updates()`'s grouping through the same
      helper and **keep its constant-value abort exactly as strong**. The
      validation is broadcast-encoding business, not kind-shaped writing, and
      folding it in would give every caller an abort it cannot trigger.
- [ ] 2.4 Tests: the existing broadcast-encoding tests stay byte-identical, and
      the mixed-fan-out abort still fires with its current message.
- [ ] 2.5 Verification: `NOT_CRAN=true`, baselines PASS not SKIP.

## 3. Close

- [ ] 3.1 Record in `progress.md`, for each of the three detectors, the failure
      it produced against the unfixed tree. A detector whose failure was never
      observed is not yet a detector.
- [ ] 3.2 Confirm the `interaction-terms` requirement is now met on both kernel
      shapes, and that its new sender-kernel scenario is the one that would have
      caught the regression this change repairs.
- [ ] 3.3 `bash .plan/opsx-spec-placement-check.sh shared-core-operand-conformance`
      and `openspec validate shared-core-operand-conformance --strict` clean.
- [ ] 3.4 Final verification: full `NOT_CRAN=true` suite green, baselines and
      goldens PASS not SKIP. No `NEWS.d/` fragment; nothing user-visible moved.
