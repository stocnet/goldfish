# Design — parity-followups

## Context

`backend-parity` landed twelve commits and ended with `NOT_CRAN=true` at
4445 passing, 0 failing, 0 skipped. Along the way it verified eight problems it
then deliberately left alone, each for a reason recorded at the time. This
change is the cleanup those reasons implied.

**Key code facts, verified before writing this design** (the same discipline
`backend-parity` task 0.1 used, because two of them changed what this change
needs to do):

- `result.goldfish` **already carries `node_lookup`**, a data frame of
  `(side, local, global, label)`. Labels are therefore already resolvable, and
  attaching `names()` / `dimnames()` to per-event components would duplicate
  them once per event — on a 57k-event, 159-actor REM that is the same
  $O(n|R|)$ blow-up `residuals-gof` D2 exists to avoid. **The gap is
  documentation and completeness, not storage.**
- The risk-set axis **is already recoverable** from the stored `model_spec`, but
  only through `goldfish:::risk_set_axis()` — an unexported accessor. In-package
  consumers can use it; a user writing a diagnostic cannot, and `:::` on one's
  own package in user code is a smell that invites the accessor to be copied.
- `margins` component names genuinely vary by geometry, measured on `dataTest`:
  `observed, expected, expected_probability` on DyNAM-rate;
  `observed, expected` on DyNAM-choice and on coordination;
  `observed_sender, expected_sender, observed_receiver, expected_receiver,
  expected_probability_sender, expected_probability_receiver` on REM. A
  consumer must branch on family to read them, and a length-5 vector means
  senders on rate and receivers on choice with nothing on the object saying so.
  **This is `residuals-gof`'s to solve** (D1 as revised): it owns the capability
  and is already opening the object. What this change contributes is the axis
  that makes the second half of that sentence answerable.
- The frozen-baseline PreToolUse hook matches `Edit|Write|MultiEdit` on
  `.tool_input.file_path`. Shell writes bypass it — necessarily, since the
  sanctioned generator writes through `Rscript` + `saveRDS`, which the
  `regen-baselines` skill documents. So the guard cannot be closed at the tool
  layer without breaking the one legitimate writer.
- No test references either baseline generator. Both were broken (they called
  `baselines_engines`, renamed by `backend-parity` task 1.2) and nothing caught
  it, because nothing runs them.
- `helper-baselines.R` calls `parallel::detectCores()` and `parallel::mclapply()`
  while `parallel` appears in neither `Imports` nor `Suggests`.

## Goals / Non-Goals

**Goals:**

- A consumer can learn what a per-event index means from the fit itself,
  through documented and exported surface rather than through `:::`.
- The per-event reductions have one implementation rather than six private
  copies, the same argument that carried the ranks and margins folds.
- Every control that currently depends on someone remembering something —
  the summation order, the frozen files, the generators — depends instead on
  something that fails loudly.
- The two measurements left unexplained get explained while they are cheap to
  reproduce.

**Non-Goals:**

- Changing any coefficient. The score-form change (D4) touches a stored
  diagnostic that feeds no estimate; the frozen 1e-6 floor stays the gate.
- The `diagnostics` vocabulary, defaults and storage guardrail — owned by
  `residuals-gof`'s `diagnostic-primitives`.
- The `(backend, primitive)` capability table — `backend-parity` owns it and it
  is uniformly supported; nothing here changes availability.
- Duplicating actor labels onto per-event components (see D3).
- **The shape, labelling or accessor of `margins`** — `residuals-gof` owns that
  object and is already opening it (D1 as revised).
- Re-freezing any baseline. v2's r/cpp columns are v1's numbers carried forward
  bit-identically and stay that way.

## Decisions

### D1 (revised 2026-07-26) — margins keep their storage; uniformity comes from an accessor, owned by residuals-gof

The original decision reshaped `margins` into a role-keyed schema. Two things
killed it.

**Ownership.** `residuals-gof` already carries the `margins primitive content`
requirement in `diagnostic-primitives` — the capability `backend-parity` D6
assigned it — and its task 1.10 is already scheduled to open that object to
attach the `"probability"` / `"expected_count"` labels. Reshaping from here
would restructure margins twice, in two changes, breaking consumers twice, and
would split one concept across two capabilities: precisely the hazard D6 exists
to prevent. `openspec validate` would not catch it, because the two live in
different capabilities; only the runtime object would, and it can satisfy one
shape.

**Necessity.** The uniformity a consumer actually needs is deliverable without
touching storage. With actor labels (which residuals-gof's requirement already
mandates and 1.10 already adds) and the exported axis of D2, a per-family
accessor returning a normalized view gives every consumer one shape to program
against, additively and without a breaking change. The residual cost is that a
consumer bypassing the accessor still meets two shapes — real, but small
against a break plus cross-change coordination.

**Decision.** This change does not touch `margins`. `residuals-gof` task 1.10
owns labels plus a uniform accessor; this change contributes only the axis
(D2) that makes an index interpretable. The `diagnostic-object-contract`
capability shrinks to the index-semantics contract, which is coherent alone and
genuinely not `residuals-gof`'s.

Rejected: reshaping inside `residuals-gof` 1.10 so consumers break exactly once
— defensible, and the better option if a stored-shape break is wanted at all,
but it buys uniformity for non-accessor users at the cost of a break that the
accessor makes unnecessary. Rejected: keeping the reshape here (the ownership
split above).

### D2 — the index axis is documented fit surface, not an unexported accessor

The axis is already stored (inside `model_spec`); this promotes it to a
documented component and an exported reader, so no consumer needs
`goldfish:::risk_set_axis()`. It answers "what does position `i` of this
per-event component mean" — sender, receiver-given-sender, dyad, or unordered
pair — for probabilities, ranks and margins alike, which is what lets
`residuals-gof`'s accessor (D1 as revised) normalize margins without this
change touching them.

The reason this is worth a requirement rather than a docs line: `residuals-gof`
is about to build `diagnose_*()`, `residuals()` and `predict()` on exactly this
question, and `backend-parity` D10 already established the precedent that a fit
must be able to describe how it was produced (it records `backend` for the same
reason). An index whose meaning is only recoverable through `:::` is not a
contract.

Rejected: exporting `risk_set_axis()` alone — it takes a spec, not a fit, so
every consumer would still reach into `fit$model_spec`.

### D3 — labels resolve through `node_lookup`; per-event components stay unlabelled

The tempting fix is `names()` on every per-event vector and `dimnames()` on
every grid. It is wrong here: `node_lookup` already maps
`(side, local, global, label)`, so per-event labels would be a second copy of
the same table repeated once per event — for a 57k-event REM, tens of thousands
of copies of a 159-row mapping, against a primitive whose storage guardrail
already warns about size.

So: `node_lookup` is specified as *the* way to resolve any per-event index, and
this change verifies it is complete for every geometry rather than assuming it —
one-mode dyad models must carry both sides, two-mode models must distinguish
them, and a node-subset model must resolve `local` to the original `global` row.
Task 0.2 grounds that before anything depends on it.

Rejected: labelling per-event components (storage); rejected: a `labels =
TRUE` opt-in (a second shape for the same primitive, which is the disease).

### D4 (revised 2026-07-26) — the score fold is a consistency cleanup; the precision argument did not survive measurement

The original decision moved the six engines onto `event_score_row()` on the
grounds that their before/after difference of the running derivative is
worse-conditioned than a direct evaluation, badly so for late events in a long
sequence. The mechanism is real. The magnitude was asserted, not measured, and
measuring it removes the argument:

```
  family                  ‖running total‖ / ‖increment‖   implied relative loss
  DyNAM choice                        4125                      9.2e-13
  DyNAM rate_ordered                   184                      4.1e-14
  REM_ordered                          174                      3.9e-14
  DyNAM rate                           132                      2.9e-14
  REM                                   53                      1.2e-14
```

That is 100× to 8000× tighter than the 1e-10 cross-backend tolerance, and
extrapolating the worst case to a 57k-event sequence still lands near 1e-11. A
**breaking change to stored values** to buy 1e-13 is not a trade worth making,
and specifying a precision benefit the numbers do not support would be worse
than not specifying it.

**Decision.** The fold stays, on its remaining honest justification — one
implementation of the reduction instead of six private copies, which is the
same argument that carried the ranks and margins folds — and the small value
shift is documented as an accepted consequence rather than sold as an
improvement. Its priority drops accordingly: it is worth doing when those
kernels are next opened, not worth opening them for.

Rejected: dropping the fold entirely (the one-implementation benefit is real
and the six copies are exactly what the shared header exists to remove).
Rejected: keeping the conditioning justification (the measurement contradicts
it, and a spec asserting it would be false).

### D5 — the summation-order invariant gets a test where one is portable, and honesty where it is not

`DyNAM_rate_default` stores the doubles its loop already computed rather than
rebuilding the rate vector with a GEMV, because per-row `dot()` and a
matrix-vector product sum in different orders and can differ in the last bit,
which on a frozen coefficient is a movement nobody intended. The technique is
right; nothing currently fails if it is "simplified" back.

The honest difficulty: the natural guard — assert bitwise equality against a
stored vector — is not portable, because different BLAS implementations
legitimately differ. So the guard is layered:

1. a comment at the site stating the invariant and why (already written);
2. a test asserting the *reduction inputs* equal the *likelihood inputs* within
   the same process — that `total_rate` equals the sum of the rate vector the
   margins reduction consumed, exactly (0 tolerance), since both come from the
   same doubles in the same run and no cross-platform comparison is involved;
3. if (2) proves fragile in CI, it degrades to documentation and that outcome is
   **recorded**, not silently dropped.

Point (2) is the load-bearing one: it fails if someone rebuilds the vector by a
different route, because the rebuilt values would no longer be the ones the
likelihood summed, while staying immune to platform BLAS differences.

Rejected: tightening the baseline tolerance for the rate cells — it would fail
on a different machine for a reason unrelated to the invariant.

### D6 — the frozen baselines get a content guard, because the tool guard cannot be closed

The PreToolUse hook matches `Edit|Write|MultiEdit`; the sanctioned generator
writes through `Rscript`. Closing the shell path would break the one legitimate
writer, so the guard belongs at the content layer instead: a test asserting the
checksums of `coefficient_baselines_v1.rds` and
`global_v1/coefficient_baselines_global.rds`, which fails no matter which tool
modified them.

This is not hypothetical. During `backend-parity` I edited two scripts in that
directory through a shell one-liner before noticing the hook applied there; the
edits were benign, but nothing in the repo would have told me otherwise.

`coefficient_baselines_v2.rds` is deliberately **not** checksummed: its gather
column is expected to be regenerated when gather numerics legitimately change,
and pinning it would turn a supported operation into a test failure. v1 and
`global_v1` are the frozen floor and are the ones that must never move.

Rejected: making the hook match `Bash` too — it would have to parse arbitrary
shell to find the write, and would block the generator the `regen-baselines`
skill exists to run.

### D7 — the generators become thin drivers over tested code

Both generators rotted because they are the only place their logic lives and
nothing executes them. Rather than add a test that runs a full grid refit
(minutes, and duplicating what the baseline tests already assert), the shared
build loop moves into `helper-baselines.R` — which every baseline test loads, so
a rename like `baselines_engines` → `baselines_backends` breaks a test
immediately instead of lying dormant in a script.

The scripts keep their distinct policies (v1/v2 carry-forward, `global_v1`'s own
grid) as arguments, and shrink to a few lines each. Rot becomes structurally
impossible rather than something a future sweep has to remember.

### D8 — `parallel` goes to Suggests, and its use stays guarded

It is used only by test helpers, so `Suggests` is the correct field —
`Imports` would make every user install it to run a model. The call sites gain
a `requireNamespace()` guard falling back to the serial path, which
`baselines_cores()` already has a branch for (it returns 1 on Windows), so the
fallback is exercised rather than theoretical.

### D9 — the spec-delta placement check becomes a documented archive step

`openspec validate` checks SHALL wording and scenario structure but not `##`
section placement, so a `## MODIFIED` block naming a requirement absent from the
living spec validates cleanly and then silently lands as an ADD at archive,
leaving the old wording in place. That is not a hypothetical either: it found a
real instance in `revise-gather-output`, and a false positive of a naive version
of the check found none in `backend-parity` — the check must understand
`## RENAMED` blocks, or it cries wolf on every legitimate rename.

It joins `bash .plan/opsx-archive-track.sh` as an explicit archive step in
`CLAUDE.md`, so it runs when it matters rather than when someone remembers.

### D10 — the two open measurements get answered, or explicitly parked

**The v1 drift.** `fish_rem` and `fish_dynam_rate` differ from baselines frozen
at `b890cd0` by 2.4e-07 and 1.5e-08 relative — within the 1e-6 gate, and
*identical on `r` and `cpp`*, which rules out a backend cause and points at a
shared preprocessing or convergence-path change somewhere in the intervening
history. It is worth bisecting once: either it is an understood consequence of a
landed change (record it and move on) or it is a small unnoticed regression, and
the fisheries fixture is the one with composition change, which is where such a
thing would hide.

**Coordination rank ties.** At the MLE, social-evolution event 266 carries a
~7000-wide block of dyads whose probabilities sit within 1e-12 of the observed
one; `r`'s `stable_softmax` log-normalizer and `cpp`'s `log_sum_exp` round ~57
of them across the boundary differently, giving ranks 2155 vs 2098.
`backend-parity` handled this by choosing a fixture without such a block, which
is right for a parity test and does not answer the user-facing question: on that
data, the rank a user gets depends on the backend. Either ranks get a
deterministic tie rule (e.g. midrank over the tied block, which is what a
statistician would expect and is backend-independent by construction), or the
fragility is documented as inherent. This change decides which; it does not
assume the answer.

## Risks / Trade-offs

- [D1 renames margin components, breaking any consumer reading
  `expected_sender`] → it is a 2.0.0 breaking item with a NEWS entry, and the
  in-repo consumers are this package's own tests plus `residuals-gof`, which is
  unarchived and can be written against the new shape directly.
- [D4 changes stored `event_scores` values, and something may compare them to
  stored expectations] → the aggregate identity and cross-backend parity are the
  gates, and both should get *easier*; any snapshot of raw score values is
  re-accepted deliberately with the diff inspected, never blanket-accepted.
- [D4 touches all six frozen-baseline kernels again] → it edits only the
  `event_scores` block, which feeds no estimate; the frozen 1e-6 baselines are
  the gate on every commit, and `cpp-recompile` before testing so no baseline is
  compared against a stale `.so`.
- [D5's in-process guard may still prove brittle] → its fallback is stated in
  the decision rather than discovered later, and the outcome is recorded either
  way.
- [D10's bisect could be open-ended] → it is time-boxed to identifying the
  commit; explaining a *found* cause can become its own change, and parking it
  with the evidence recorded is an acceptable outcome.
- [This change depends on `backend-parity` archiving first] → its deltas rename
  a requirement this change's spec sits beside; the D9 pre-flight is exactly the
  check that catches the mistake if the order slips.

## Open Questions

Both are D10's, and both are questions this change answers rather than carries:
what causes the 2.4e-07 drift against `b890cd0`, and whether coordination ranks
adopt a deterministic tie rule. Neither blocks the other seven items.
