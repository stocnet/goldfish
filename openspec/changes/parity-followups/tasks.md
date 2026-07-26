# Tasks — parity-followups

Disciplines (openspec/config.yaml): one focused conventional commit per task,
tests green at every commit, `devtools::document()` inline whenever roxygen /
exports / signatures change, `air format` only the R files a task touched before
`lintr` runs on them, `NOT_CRAN=true` with the frozen baselines PASS (not SKIP)
before each commit, r-lib skills (r-package-development, testing-r-packages,
cli, lifecycle) invoked before the work they cover. Every task touching
`src/*.cpp` / `src/*.h` invokes `cpp-recompile` BEFORE testing.

**Sequencing constraint:** this change depends on `backend-parity` being
**archived first** — its deltas rename a requirement in `optimizer-selection`
and add the parity requirements this change's specs sit beside. Task 0.1 is the
gate on that.

Section 1 (guards) is independent and comes first. Section 2 is additive.
Section 3 is priority-deferred (task 0.4). Sections 4 and 5 are independent of
all of them.

## 0. Decisions taken before this change starts (2026-07-26)

These were settled in an exploration session after `backend-parity` archived.
They are recorded here because two of them **removed** work this change
originally proposed, and an implementer reading only the task list would
otherwise redo it.

- [ ] 0.1 Confirm `backend-parity` is archived and its deltas are folded into
      `openspec/specs/**`, then re-run the spec-delta placement check on this
      change (D9): every `## MODIFIED`/`## REMOVED` header resolves in the
      living spec, and `## RENAMED` blocks are honored rather than flagged.
      Both deltas here deliberately use `## ADDED`, so a failure means the
      living spec is not where it is assumed to be.
- [ ] 0.2 Verify the node-lookup completeness D3 depends on before anything is
      written against it: `node_lookup` resolves indices on all four risk-set
      geometries, carries **both** sides for a two-mode model, and maps `local`
      back to the original `global` row on a node-subset model. Record what it
      does NOT cover — D3 is written assuming it is sufficient, and if it is
      not, the gap is a task here rather than a surprise later.
- [x] 0.3 **`margins` is out of scope — decided, do not reshape it.** The
      original D1 reshaped margins into a role-keyed schema. `residuals-gof`
      owns that object (`diagnostic-primitives` :: "margins primitive content",
      assigned by `backend-parity` D6) and its task 1.10 is already opening it
      to attach labels. Reshaping from here would break consumers twice and
      split one concept across two capabilities — which `openspec validate`
      cannot catch, since they are different capabilities. **Resolution:**
      residuals-gof 1.10 adds actor labels plus a uniform accessor over the
      existing storage; this change contributes only the axis (D2) that makes
      an index interpretable. Non-breaking, one owner. See residuals-gof
      task 0.1.
- [x] 0.4 **The score fold keeps its narrower justification — measured, not
      asserted.** D4 originally claimed the before/after-difference form of
      `event_scores` was materially worse-conditioned. Measured across the
      families: ‖running total‖/‖increment‖ of 53 to 4125, i.e. 1.2e-14 to
      9.2e-13 relative loss — 100x to 8000x inside the 1e-10 tolerance, and
      still ~1e-11 extrapolated to a 57k-event sequence. The mechanism is real,
      the magnitude is not. **Resolution:** the fold stays on the
      one-implementation argument alone, the value shift is documented as an
      accepted consequence rather than sold as a precision fix, and section 3
      is priority-deferred to whenever those kernels are next opened. Do not
      re-propose it on conditioning grounds.

## 1. Guards and hygiene (independent, no numerics)

- [ ] 1.1 Checksum guard on the frozen baselines (D6): a test asserting the
      digests of `coefficient_baselines_v1.rds` and
      `global_v1/coefficient_baselines_global.rds` via base R's `tools::md5sum`
      (no new dependency). **`coefficient_baselines_v2.rds` is deliberately
      excluded** — its gather column is expected to be regenerated when gather
      numerics legitimately change, and pinning it would turn a supported
      operation into a failure. Comment must say so, or someone will "fix" the
      omission.
- [ ] 1.2 `parallel` into `Suggests` (D8) with `requireNamespace()` guards at
      the two call sites in `helper-baselines.R`, falling back to the serial
      path `baselines_cores()` already returns on Windows. Verify the fallback
      by forcing it, not by reading it.
- [ ] 1.3 Move the baseline build loop into `helper-baselines.R` (D7) and
      reduce the three generator scripts to thin drivers passing their own
      policies (v1/v2 carry-forward, `global_v1`'s grid). The point is that a
      rename now breaks a test immediately instead of rotting in a script that
      nothing runs — verify by renaming a helper symbol locally and confirming
      the suite fails, then reverting.
- [ ] 1.4 Promote the spec-delta placement check to a documented archive step
      (D9): a small script in the repo plus a line in `CLAUDE.md` beside
      `bash .plan/opsx-archive-track.sh`. It MUST understand `## RENAMED`
      blocks — a naive version flags every legitimate rename, which is how it
      would get ignored. Prove it on the known-good `backend-parity` archive
      and on a deliberately mis-placed block.

## 2. The diagnostic object contract (spec: diagnostic-object-contract)

Scope note: margins are NOT here (task 0.3). This section is the index-semantics
contract only.

- [ ] 2.1 Record the risk-set axis on the fit as documented, exported surface
      (D2), so no consumer needs `goldfish:::risk_set_axis()`. One write in the
      results assembly, an accessor, roxygen `@return` documenting it, and
      `devtools::document()` inline. Tests: the two same-length-different-axis
      fits are distinguishable; the axis names the geometry without the consumer
      inspecting `model` / `sub_model`; it is readable without `:::`.
- [ ] 2.2 Specify `node_lookup` as the documented index resolver (D3) and close
      whatever 0.2 found missing. Tests: the three resolution scenarios,
      including both sides on a two-mode fit; and that per-event components
      carry no per-event labels — the storage property the decision rests on,
      and the one that would silently regress if someone "helpfully" added
      `names()`.

## 3. Per-event score: one implementation (spec: likelihood-computation)

Priority-deferred per task 0.4 — worth doing when these kernels are next open,
not worth opening them for. Sections 1, 4 and 5 do not depend on it.

- [ ] 3.1 Move the six `*_default.cpp` engines' `event_scores` block onto
      `event_score_row()`, replacing six private copies of the arithmetic with
      the shared reduction. `derivative` itself is untouched — only the stored
      diagnostic changes. `cpp-recompile` before testing; the frozen 1e-6
      baselines are the gate and must not move, since no coefficient depends on
      this block.
- [ ] 3.2 Re-verify the aggregate identity, which changes character rather than
      value: column sums of the stored scores equalling the final score used to
      hold by construction (the rows *were* the differences that built the
      total) and now holds because independently computed rows agree with it.
      Assert at 1e-10 and say in the test comment why it is no longer a
      tautology.
- [ ] 3.3 Re-run the three-way parity suite and record the movement. Expect it
      to shrink: `r` and `gather` already use the direct form, so this brings
      the third backend to them. If any comparison instead worsens, stop and
      record it — that would contradict the decision's premise.

## 4. The summation-order invariant (design D5)

- [ ] 4.1 In-process guard that the reduction inputs are the likelihood inputs
      (D5): `total_rate` equals the sum of the rate vector the margins reduction
      consumed, at **0 tolerance**, because both come from the same doubles in
      the same run — no cross-platform comparison is involved, which is what
      makes this portable where a stored-vector comparison is not. It fails if
      someone rebuilds the vector by a different route, e.g. as a GEMV.
- [ ] 4.2 If 4.1 proves fragile in CI, degrade to documentation and **record
      that outcome** in progress.md rather than dropping it silently. The
      decision states this fallback up front so taking it is not a retreat.

## 5. The two open measurements (design D10)

- [ ] 5.1 Bisect the ~2.4e-07 relative drift of `fish_rem` / `fish_dynam_rate`
      against the baselines frozen at `b890cd0`. It is identical on `r` and
      `cpp`, so a backend cause is already ruled out and the search is over
      shared preprocessing or convergence-path history; the fisheries fixture is
      the one with composition change. Time-boxed to identifying the commit:
      an understood cause gets recorded, an unexplained one gets parked **with
      the evidence**, and a genuine regression becomes its own change.
- [ ] 5.2 **Spike, not a decision** — coordination rank ties (D10). The
      evidence: at the MLE, social-evolution event 266 carries a ~7000-wide
      block of dyads within 1e-12 of the observed probability, of which ~57
      round across the boundary differently between `r`'s `stable_softmax`
      log-normalizer and `cpp`'s `log_sum_exp`, giving ranks 2155 vs 2098. So
      on near-degenerate data the rank a user gets depends on the backend.
      What is NOT yet known, and is what this spike must establish before any
      rule is chosen: how often real coordination data reaches that
      degeneracy (is event 266 pathological or typical of the tail?), whether
      a midrank over the tied block would change any diagnostic a user would
      actually read, and what the other tie-sensitive primitives (recall@k in
      `residuals-gof`) do with such a block. Only then choose between a
      deterministic rule — midrank is backend-independent by construction and
      is what a statistician expects of a rank — and documenting the
      fragility where a user reading `observed_rank` will see it. Report the
      findings; if a rule is adopted it is specced and applied on all three
      backends, which is its own task.

## 6. Closure

- [ ] 6.1 NEWS entry: the margin reshape as **BREAKING** with the before/after
      shape, the axis and node-lookup contract as what replaces reaching into
      internals, and the score conditioning as a stored-value change that moves
      no coefficient. DESCRIPTION version bump.
- [ ] 6.2 Full `NOT_CRAN=true` suite green with the frozen baselines PASS (not
      SKIP), `openspec validate` green, and the D9 placement check green on this
      change's own deltas before archive.
