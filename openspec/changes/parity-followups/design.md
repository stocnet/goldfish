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

- A consumer reads per-event diagnostics without knowing the model family:
  one component vocabulary, one documented way to learn what an index means.
- The stored per-event score is the better-conditioned of two algebraically
  equal forms.
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
- Re-freezing any baseline. v2's r/cpp columns are v1's numbers carried forward
  bit-identically and stay that way.

## Decisions

### D1 — `margins` has one shape, keyed by role, with the roles declared

The two-vocabulary split is the whole problem, so the fix is a single schema
that every geometry fills in:

```
fit$margins = list(
  axis  = "dyad",                    # the risk-set axis, verbatim
  roles = c("sender", "receiver"),   # which role slots are populated
  sender   = list(observed =, expected =, expected_probability =),
  receiver = list(observed =, expected =, expected_probability =)
)
```

A consumer writes `for (role in m$roles) m[[role]]$expected` and never branches
on family. Single-sided families declare one role; exact-time families carry
`expected_probability` and multinomial ones do not, which is already true and
now visible rather than inferred from which names happen to exist.

Coordination declares `roles = "participant"`, not `"sender"`. Its realized risk
set is the unordered pair list and a pair credits **both** members into one
actor set, so its vectors total `2n` where every other family totals `n`. Naming
that slot `sender` would be a lie a consumer would silently divide by two the
wrong way; naming it `participant` makes the fourth geometry legible. The
doubling is additionally recorded as `attr(m, "events_per_observation") = 2L`
so a calibration routine can normalize without special-casing.

Rejected: keeping both vocabularies and documenting the branch — it pushes a
family lookup into every consumer, which is the cost this change exists to
remove. Rejected: a tidy one-row-per-(actor, role, scale) data frame — the
friendliest surface, but margins are also read by the parity tests and by
`residuals-gof` in hot paths, and a frame per fit is a heavier object than the
vectors it wraps; the frame belongs in an `augment`-style accessor built *on*
this, not in the stored component.

### D2 — the index axis is documented fit surface, not an unexported accessor

The axis is already stored (inside `model_spec`); this promotes it to a
documented component and an exported reader, so no consumer needs
`goldfish:::risk_set_axis()`. `fit$margins$axis` (D1) carries it for margins,
and the same value answers "what does position `i` of this per-event
probability vector mean" — sender, receiver-given-sender, dyad, or unordered
pair.

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

### D4 — the stored score is computed directly, and that changes values on purpose

The six `*_default.cpp` engines derive `event_scores` as
`derivative_after − derivative_before`: a difference of two accumulating
partial sums. `event_score_row()` computes the same quantity as
`X_obs − c·w'X` from the event's own terms. Algebraically identical; the
difference form loses relative precision as the running derivative grows, so
late events in a long sequence are the worst-conditioned — exactly the events a
sequence-level diagnostic cares about.

`backend-parity` D7 required the engine fold to change nothing it computed, so
this could not go in there; smuggling a numerical improvement into a refactor
commit would also have made any baseline movement unattributable. Here it is the
point of the change and is specced as such.

Two consequences to hold onto. First, the aggregate identity — column sums of
`event_scores` equal the final score — must still hold, and it becomes a
*stronger* statement: currently it holds by construction (the increments are
literally the differences that built the total), afterwards it holds because the
independently computed rows agree with the accumulated total, which is a real
check rather than a tautology. Second, cross-backend parity must be re-verified:
`gather` and `r` already use the direct form, so this should *reduce*
disagreement, and the parity suite's 1e-10 becomes easier to meet, not harder.

Rejected: changing `derivative` itself to accumulate the direct rows — that
would move coefficients, and the estimator's accumulation is not what is
mis-conditioned.

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
