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

The cheap, independent hardening (section 1) is sequenced before the numerics
(sections 2–3) so the guards exist before anything starts changing values.

## 0. Ground the design against the code

- [ ] 0.1 Confirm `backend-parity` is archived and its deltas are folded into
      `openspec/specs/**`, then re-run the spec-delta placement check on this
      change (D9): every `## MODIFIED`/`## REMOVED` header resolves in the
      living spec, and `## RENAMED` blocks are honored rather than flagged.
      This change deliberately uses `## ADDED` in both deltas, so a failure
      here means the living spec is not where it is assumed to be.
- [ ] 0.2 Verify the node-lookup completeness D3 depends on before anything is
      written against it: `node_lookup` resolves indices on all four risk-set
      geometries, carries **both** sides for a two-mode model, and maps `local`
      back to the original `global` row on a node-subset model. Record what it
      does NOT cover — D3's requirement is written assuming it is sufficient,
      and if it is not, the gap is a task here rather than a surprise later.
- [ ] 0.3 Re-measure the two facts D1 rests on, since they are the whole
      argument for the reshape: the per-family margin component names, and that
      coordination totals `2n` while every other family totals `n`. Record any
      drift in progress.md before the reshape starts.

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

- [ ] 2.1 Record the risk-set axis on the fit as documented, exported surface
      (D2), so no consumer needs `goldfish:::risk_set_axis()`. One write in the
      results assembly, an accessor, roxygen `@return` documenting it, and
      `devtools::document()` inline. Tests: the two same-length-different-axis
      fits are distinguishable; the axis is readable without `:::`.
- [ ] 2.2 Reshape `margins` to the role-keyed schema (D1) in both assembly
      sites (`cpp_interface.R`, `estimation_core.R`) — `axis`, `roles`, and one
      element per role — with coordination declaring `participant`. **BREAKING**:
      the `*_sender` / `*_receiver` component names go away. Tests: the
      single-consumer scenario (one loop reads every family); per-family role
      declarations; exact-time roles carry `expected_probability` and
      multinomial ones do not.
- [ ] 2.3 Declare the credits-per-observation attribute (D1) and assert the
      totals it explains: coordination sums to twice the dependent-event count,
      every other family to once. This is the assertion that would have caught a
      normalization error silently dividing by the wrong factor.
- [ ] 2.4 Update every in-repo consumer of the old margin names — the parity
      suite, the diagnostic-primitives tests, and any helper — to the new
      schema. `NOT_CRAN=true` green is the gate; the frozen baselines are
      untouched by a naming change and must stay PASS.
- [ ] 2.5 Specify `node_lookup` as the documented index resolver (D3) and close
      whatever 0.2 found missing. Tests: the three resolution scenarios,
      including both sides on a two-mode fit; and that per-event components
      carry no per-event labels (the storage property the decision rests on).

## 3. Per-event score conditioning (spec: likelihood-computation)

- [ ] 3.1 Move the six `*_default.cpp` engines' `event_scores` block onto
      `event_score_row()` (D4), computing the row directly instead of
      differencing the running derivative. `derivative` itself is untouched —
      only the stored diagnostic changes. `cpp-recompile` before testing; the
      frozen 1e-6 baselines are the gate and must not move, since no coefficient
      depends on this block.
- [ ] 3.2 Re-verify the aggregate identity, which changes character rather than
      value: column sums of the stored scores equalling the final score used to
      hold by construction (the rows *were* the differences that built the
      total) and now holds because independently computed rows agree with it.
      Assert it at 1e-10 and say in the test comment why it is no longer a
      tautology.
- [ ] 3.3 Re-run the three-way parity suite and record the movement (D4
      predicts cross-backend agreement gets *easier*, since `r` and `gather`
      already use the direct form). If any comparison instead worsens, stop and
      record it — that would contradict the decision's premise.
- [ ] 3.4 Late-sequence precision check: stored scores versus independently
      recomputed scores, early events against late ones on a long fixture. This
      is the requirement's teeth — the whole justification is that the old form
      degraded along the sequence, and an unmeasured claim of improvement is
      worth nothing.

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
- [ ] 5.2 Decide coordination's rank tie policy (D10): either a deterministic
      rule (midrank over a tied block is backend-independent by construction and
      is what a statistician would expect) or documented inherent fragility.
      The evidence is social-evolution event 266 at the MLE — a ~7000-wide block
      within 1e-12 of the observed probability, of which ~57 round across the
      boundary differently between `r`'s `stable_softmax` log-normalizer and
      `cpp`'s `log_sum_exp`, giving ranks 2155 vs 2098. If a rule is adopted it
      is specced and applied on all three backends; if not, the fragility is
      documented where a user reading `observed_rank` will see it.

## 6. Closure

- [ ] 6.1 NEWS entry: the margin reshape as **BREAKING** with the before/after
      shape, the axis and node-lookup contract as what replaces reaching into
      internals, and the score conditioning as a stored-value change that moves
      no coefficient. DESCRIPTION version bump.
- [ ] 6.2 Full `NOT_CRAN=true` suite green with the frozen baselines PASS (not
      SKIP), `openspec validate` green, and the D9 placement check green on this
      change's own deltas before archive.
