## Context

The shared kind-shaped core landed in `R/broadcast_kind.R`. Four consumers were
meant to reach it: statistics, interaction operands, constraint atoms and the
support mask. Three do. The interaction operand path reaches it on its dyad
branch and not on its sender branch, and the collapse step is implemented three
times.

Nothing here is a correctness gap. That is the interesting part, and it is why
the design is mostly about detectors.

| gap | what breaks if unfixed | why no test caught it |
| --- | --- | --- |
| sender write copies | an operand vector copied per event | routing a write through a wrapper is behaviorally neutral |
| dyad delta not collapsed | one entry written n1 times | last write wins, so the value is right |
| third collapse | drift between three implementations | all three are correct today |

A defect that cannot fail a test is the kind that returns. One of these already
did: the sender-branch routing was written, lost to a `git checkout` during an
unrelated investigation, reported as done, and never committed. The suite stayed
green throughout.

## Goals / Non-Goals

**Goals:**
- The operand write goes through the shared routine on both kernel shapes.
- The collapse is defined once and consumed by every path that needs it.
- Each gap gets a detector that could actually fail, verified by making it fail.

**Non-Goals:**
- The interaction product's emission kind. That is `merged-walk-effect-reuse`
  group 0, deliberately, because it changes what the walk emits and that
  change's group 1 measures the walk.
- Touching the statistics path's own applies. They already consume the core.
- Anything about the constraint. Its remaining gaps have their own successors.

## Decisions

### D1 — Every detector is verified by making it fail

For each gap, the test is written, then the fix is reverted locally and the test
observed failing, then the fix is restored. The observation is recorded in
`progress.md` with what the failure said.

This is not ceremony. All three gaps are behaviorally neutral, so a test that
asserts the wrong thing passes in both worlds and looks exactly like a test that
works. The predecessor shipped two such tests: a `tracemem` assertion capturing
the wrong stream, and a counter of a function nobody calls.

*Rejected:* trusting that the assertion is well-formed because it passes. That
is the failure mode, not the evidence.

### D2 — The collapse is one function, and the validation is not part of it

`broadcast_entries_from_updates()` does two things: it groups an effect's
updates by the index its kind holds fixed, and it aborts if the fan-out values
are not constant. The first is the collapse. The second is a real check that
belongs to broadcast ENCODING, not to kind-shaped writing, and it stays where it
is at full strength.

Folding both into one shared helper would make every caller of the collapse
inherit an abort about broadcast encoding that most of them cannot trigger.

### D3 — The sender kernel's kind codes mean something different, and the shared write does not care

Under the sender kernel `classify_broadcast_kind()` returns 3 for `global` and 0
otherwise, where 0 means per-sender rather than per-dyad. So the sender branch's
kind vocabulary is not the dyad branch's, and translating between them is not
what this change is doing.

What is shared is narrower and safe: the WRITE. `write_entries()` takes a buffer,
entries and values, and neither knows nor needs the kernel's kind semantics. The
sender branch already computes its own entries correctly; it simply writes them
with a subassignment that copies.

*Rejected:* unifying the two kernels' kind codes here. That is effect-metadata
work and belongs to `effect-term-registry`.

## Risks / Trade-offs

- [A behaviorally-neutral change with no detector] → D1 makes the detector's
  failure an observed fact rather than an assumption.
- [The in-place write aliases something] → the operand buffers are materialized
  fresh at seeding from `initial_stats` slices and live in one environment
  binding, which is the same precondition the dyad branch already meets and the
  same one ADR-0059 states for the state matrix.
- [The collapse changes emission order] → the dirty-cell accumulation and the
  product emission are untouched; only the operand's own write is collapsed. The
  frozen baselines are the check.

## Migration Plan

Internal. One commit per task, `NOT_CRAN=true` with the frozen baselines PASS not
SKIP at each. No `NEWS.d/` fragment: nothing user-visible moves.

## Open Questions

- Does the sender branch's `dirty_inter` accumulation want the same collapse?
  It appends `updates[, "node1"]` and dedups with `unique()` at emission, so it
  is already collapsed in effect, just later. Worth confirming rather than
  assuming.
