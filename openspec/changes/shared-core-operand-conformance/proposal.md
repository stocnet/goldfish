---
depends-on: support-mask-sparse-updates
---

## Why

**Three places still implement the shared core's ideas separately, and one of
them is a requirement the predecessor wrote and then did not meet on half its
surface.**

`support-mask-sparse-updates` extracted the kind-shaped vocabulary in
`R/broadcast_kind.R` and routed the dyad interaction branch, the constraint
atoms and the support mask through it. Its `interaction-terms` delta says an
operand's write SHALL go through the shared maintain-at-kind routines rather
than an operand-specific path, on every kernel shape. A post-landing conformance
review found three gaps.

**The sender branch never joined.** `R/model_preprocess.R:872` and
`R/preprocess_joint.R:993` still read

```r
ov <- get(as.character(gid), envir = op_state)
ov[updates[, "node1"]] <- updates[, "replace"]
assign(as.character(gid), ov, envir = op_state)
```

`get()` binds the buffer to a second name, so the subassignment copies the whole
operand vector every event. `write_entries()` exists and takes the in-place C++
writer for a double buffer. This one is embarrassing in a specific way: it was
reported as done during the predecessor's group 3, was applied to the working
tree, and was then lost to a `git checkout` during an unrelated investigation.
It has never been in a commit. Nothing caught it because routing a write through
a wrapper is behaviorally neutral, so no test could fail.

**The dyad operand path skips the collapse.** A broadcast effect reports its
delta on the grid's terms, so an alter-kind operand delta arrives n1 rows long
with one unique entry. The constraint-atom path collapses it; the operand path
does not, so the same entry is written n1 times. Correctness is unaffected — the
last write wins either way — which is exactly why it needs a detector rather
than trust.

**A third implementation of that collapse exists.**
`broadcast_entries_from_updates()` groups a broadcast effect's updates by its
fixed index and validates constant fan-out. The grouping is the collapse; the
validation is its own thing and stays.

The through-line is that all three are the same idea implemented more than once,
or proven in one place and not applied in another. That is the divergence the
shared core exists to close, and closing it is cheap now and expensive once a
fourth consumer copies one of these.

## What Changes

- **The sender branch of the operand path goes through `write_entries()`**, in
  both the recipe loop and the merged walk, so the buffer is written in place
  instead of copied per event.
- **The dyad operand delta is collapsed at its kind before the write**, through
  one helper both the operand and the atom paths call, since both already reach
  `project_entries()` with `from == to`.
- **`broadcast_entries_from_updates()` consumes that same collapse** and keeps
  its constant-value validation, which is not weakened.
- **The detectors are written first and are live.** A `tracemem` assertion with
  a deliberate-copy control in the same test, because the predecessor recorded
  the trap: `tracemem` reports on stdout, so a test capturing `type = "message"`
  passes while observing nothing.

## Capabilities

### New Capabilities
<!-- none -->

### Modified Capabilities
- `interaction-terms`: the requirement's words already say "on every kernel
  shape". It gains the scenario for the SENDER kernel it never had. Its
  scenarios pin the dyad branch only, which is why a sender-branch regression
  could sit in the tree unnoticed: the requirement was asserted for both and
  testable for one.

**Written against `support-mask-sparse-updates`'s post-delta wording**
(`depends-on`). That change ADDS the requirement this one modifies, so the
spec-placement pre-flight reports it misplaced until the predecessor folds. Per
`openspec/config.yaml` that is deferral rather than a defect; the check that
counts runs at fold time, in dependency order.

## Impact

- **Depends on** `support-mask-sparse-updates` for `R/broadcast_kind.R` and for
  the post-delta wording of `interaction-terms`.
- **Code**: `R/model_preprocess.R`, `R/preprocess_joint.R`,
  `R/preprocess_builders.R`.
- **No `src/` change expected**, and this time the claim is cheap to keep: the
  in-place writer it needs already exists and already covers a double vector.
  If that proves wrong, `cpp-recompile` gates it.
- **Baselines**: the interaction path is covered by the frozen 1e-6 coefficient
  baselines, so they are a real detector here. Every task reports them PASS not
  SKIP; the product statistic and its update stream are byte-identical.
- **Decision record**: no ADR expected. ADR-0061 already decided that
  kind-shaped state is maintained in place and emitted as changes; this is
  conformance to it, not a new choice.
