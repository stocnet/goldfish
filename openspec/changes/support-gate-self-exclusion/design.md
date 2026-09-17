## Context

See `proposal.md` (Why) for the defect and where it surfaced. Three facts
from the tree shape the approach.

1. **The gate is computed twice, and both copies agree today.** The batch
   side folds the gate into `active_sender` once per rate unit
   (`finish_output()` in `preprocess_joint.R` calls
   `fold_active_sender_support()` only when a constraint exists). It keeps a
   per-sender `count` seeded by `initial_receiver_count()` and moved by
   `apply_receiver_count_flips()` (mask flips) and
   `apply_receiver_presence_flips()` / `receiver_reach()` (receiver
   crossings). The live side recomputes it from scratch per evaluation with
   `sender_gate_from_mask()` (`walk_risk_set()`, `simulation_fid_state()`),
   as do the preprocessing checks in `model_estimate.R`. The spec's
   "the rate gate equals the reduction of the shared mask" scenario pins the
   two copies to each other, which is why the diagonal leak is in both.
2. **The engines already exclude self-dyads, but the rate engine's flag says
   otherwise.** Choice and REM drop `(i, i)` through `twomode_or_reflexive`.
   On a rate engine, though, `walk_handle.R` sets
   `engine$twomode_or_reflexive <- engine$is_sender || is_two_mode`, which
   is always `TRUE`, because the rate's own statistics have no receiver
   axis. The gate therefore cannot reuse that flag. `allowReflexive` is
   `FALSE` on every DyNAM path, so for the gate "excludes self-ties" is
   exactly "one-mode": `!isTRUE(model_spec$is_two_mode)`, equivalently
   `identical(nodes, nodes2)`.
3. **ADR-0063 (vault, accepted) already decided where the diagonal lives.**
   A value stored at a broadcast kind carries the broadcast only. The
   diagonal rule is re-applied at the reading site through a
   `drop_diagonal` argument, and "a reduction reads each node's value from a
   cell whose other index is not that node". The panel completion count
   (`support_grid_at()`) already follows it for this gate.

## Goals / Non-Goals

**Goals:**

- The counter and the from-scratch reduction exclude the self-dyad on a
  one-mode layer, at every mask kind, with no dense grid for a separable
  mask.
- The live walk, the simulation driver and the preprocessing checks read
  the same corrected gate as the batch fold, boolean-exact.
- The size of the estimation change is measured and recorded before the fix.

**Non-Goals:**

- The unconstrained rate gate. Without a constraint, `active_sender` is
  presence alone and no fold runs. A sender who is the only present node is
  a separate, pre-existing edge case, and it would touch the frozen
  baselines' path.
- The stored mask values and the constraint grammar (no irreflexivity atom).
- Choice, REM, coordination and DyNAM-i risk sets, which already exclude the
  self-dyad structurally.
- Any change to `simulate()`'s driver logic. The regression test checks
  that the corrected gate removes the abort; it adds no guard of its own.

## Decisions

### D1 — The exclusion is applied by the reduction, not stored in the mask or the constraint

The sender gate's reduction excludes `(i, i)` when the layer is one-mode.
Nothing is written to the mask.

*Alternatives.* (a) An irreflexivity atom AND-ed into every constraint (and
into every derived flavor formula). It forces separable masks to point
kind, fixes only the formulas that carry it, and attaches a model property
to a value, which ADR-0063 rejected. (b) Zeroing the diagonal of the stored
mask. An ego or alter mask cannot store a diagonal exception at its own
kind, and a point mask derived from `tie(L) == 0` would need a diagonal
write after every flip. ADR-0063 rejected this too ("a value at kind k plus
exceptions"). (c) Patching only the simulation driver, which would make
the simulator disagree with the likelihood it is validated against.

### D2 — The flag is required, named `drop_diagonal`, and read from the model spec

`sender_gate_from_mask()` and the fold take `drop_diagonal` with **no
default**. Every caller derives it from the unit's model spec as
`!isTRUE(model_spec$is_two_mode)`, never from
`engine$twomode_or_reflexive` (always `TRUE` on a rate engine; see
Context 2). The name matches ADR-0063's argument on `project_value()` and
`read_value_at_cells()`.

*Alternative rejected:* a `FALSE` default, as ADR-0063's reads have. Its
accepted cost was that a forgetting caller is silently wrong. Here there are
five known callers and a silent default is exactly the defect being fixed,
so a missing argument errors instead.

### D3 — Counter algebra per kind: every reach skips the sender whose index equals the receiver's

With `a` the receiver presence, `P = sum(a)`, and `d = drop_diagonal`:

```
kind      initial count_i                  mask flip                     receiver j crosses by delta
global g  g * (P - d * a_i)                count = v * (P - d * a)       count += delta * g, except i = j
ego e_i   e_i * (P - d * a_i)              count_i = v * (P - d * a_i)   count += delta * e, except i = j
alter c_j sum_j c_j a_j - d * c_i a_i      entry j: +/- a_j to all i,    count += delta * c_j, except i = j
                                           except i = j
point s   rowSums(s & a) - d * s_ii a_i    entry (i, j): +/- a_j to i,   count += delta * s[, j], except i = j
                                           none when i = j
```

A single rule covers every row: when `d`, the entry or reach that names
receiver `j` does not touch sender `j`. That keeps each update O(n1) at
worst, as today, and it works because DyNAM one-mode layers have
`n1 == n2`, so index `j` names the same node on both axes. The from-scratch
form in `sender_gate_from_mask()` is the same table's "initial" column, used
per event.

*Alternative rejected:* a correction pass after the existing update (compute
the uncorrected count, then subtract the diagonal term per event). It reads
`s_ii` for every sender at every event, which on a point mask means a
diagonal extraction per event: O(n1) reads where the rule above does O(1)
extra work per flip.

### D4 — Detector first: the measurement fixture and the oracle exist before the fix

Before any production edit, the tests add:

- A small one-mode flavored fixture (four actors) whose history gives one
  actor a tie to every other actor, so its creation row allows only its
  self-dyad for a stretch of events, while other actors create and
  dissolve. Estimation on it is recorded before the fix (coefficients,
  `n_candidates`, `avg_active_entity`).
- An independent oracle in the test file: the gate from a dense grid,
  `rowSums(grid & present & off_diagonal) > 0`, never calling the package's
  reduction. The per-kind counter tests compare the fold and
  `sender_gate_from_mask()` against it at every event, for ego, alter,
  point and global masks, one-mode and two-mode.
- The simulation regression: a free-running run on a small one-mode
  flavored fixture that saturates a creation sender, asserting that no drawn
  event has a missing receiver. It must fail with the current tree.

The before/after estimate difference goes into `progress.md` and the NEWS
fragment ("fixed estimates of ... when ...").

### D5 — The relational completion pin follows without its own edit

`avg_active_entity` is recomputed from the folded gate inside
`fold_active_sender_support()`, so the relational pin of a completed rate
(`intercept-only-rate`) picks up the corrected risk set. It then agrees with
the panel path, which already excluded self-loops. No change to
`complete_generative_spec.R`; a test asserts the two paths give the same
`|R|` on a fixture both can read.

## Risks / Trade-offs

- [An existing flavored or constrained test hard-codes a number that moves]
  → The measurement task lists every test whose expectation changes before
  the fix is written. A moved number is accepted only when the fixture
  reaches the self-only case, and the diff is reviewed rather than
  re-snapshotted blindly.
- [A caller passes the rate engine's `twomode_or_reflexive`] → D2 makes the
  flag required and names its source. A test on a one-mode rate walk
  asserts the gate excludes the self-only sender; it would fail on the
  always-`TRUE` flag.
- [A frozen baseline moves] → None carries a support constraint or flavors.
  The fold does not run on those paths, so the six baseline files must stay
  PASS, not SKIP, under `NOT_CRAN=true`.
- [Undirected layers] → The mask is symmetric and so is the rule. `(i, i)`
  is excluded once; nothing else changes. A test covers an undirected
  one-mode flavored layer.
- [The alter-kind closed form relies on `n1 == n2`] → A one-mode layer
  guarantees it. On a two-mode layer `d` is `FALSE` and the index is never
  compared across axes.

## Migration Plan

- A behavior fix for estimation, not an API change: no lifecycle step. A
  `NEWS.d/support-gate-self-exclusion.md` fragment states which models'
  estimates can change and when.
- Branch (corrected 2026-09-17 at task 0.1, Alvaro): the change is
  implemented directly on `feature_simulation`, one commit per task. The
  integration branch cannot host it: `develop` still has the older gate (a
  per-event dense `rowSums(support[[e]] & ...)` list), with no per-kind
  counter, no `sender_gate_from_mask()`, no live walk gate and no
  `simulate()`, all of which arrived in `feature_simulation`'s commits. Per
  ADR-0040 the branch writes a `NEWS.d` fragment only; the merge into
  `develop` folds NEWS and archives the change.
- Order: this change before `constraint-objects-on-shared-walk`, whose
  derived-mask oracle must be written against the corrected rule.
- Rollback: reverting the change's commits restores today's gate; the stored
  masks and the preprocessed format are unchanged.
- A preprocessed object saved before the fix still carries the old folded
  `active_sender` and is accepted by `preprocessed =`, since the format did
  not change. The NEWS fragment tells users to re-preprocess constrained or
  flavored one-mode rate models; no format bump, since the old object is
  only wrong in the case this change fixes.
