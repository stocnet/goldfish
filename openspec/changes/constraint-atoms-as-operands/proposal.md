---
depends-on: support-mask-sparse-updates
---

## Why

**A living-spec requirement says constraint atoms are non-estimated operands of
the compiled plan. They are a private sub-plan with their own walk, and the tag
the requirement names is written in one place and read in none.**

`support-constraint` has required this since `support-constraint-as-stat`:

> Each effect atom in a `support_constraint` SHALL be parsed as an operand and
> seeded/updated through the existing operand / `stat_state` registries …
> carrying `role = "constraint"` in `plan$effects`.

`augment_constraints()` does set `role <- "constraint"`, but on the SUB-PLAN's
effects table, not on `plan$effects`. Grep the package: the string is written in
one place and consumed nowhere. The atoms live in a sibling structure with their
own state container, their own event schedule and their own event loop, built by
`build_atom_maintainer()`.

This was group 7 of `support-mask-sparse-updates`, descoped after group 6
measured. That decision was right and it is worth restating honestly rather than
quietly reversing: **the performance case is now small.** Group 6 gave a process
one atom pool shared by its sub-models, so a rate-plus-choice layer builds one
maintainer instead of two. What remains is about 0.13 s on a 1500-event
constrained model, against 0.29 s of total constrained overhead.

So the case for doing it is no longer speed. It is these:

- **A constrained model advances two event schedules over one event stream.**
  The merged single-clock walk shares its state with everything except the
  constraint, so the constraint is the one thing that substrate cannot amortise
  — and amortising shared work is what that substrate is for.
- **Two maintenance paths for one idea.** `build_atom_maintainer()` duplicates
  the main walk's state container, schedule and routing loop. Every fix to one
  has to be considered for the other. The diagonal defect fixed during the
  predecessor is exactly the kind of thing that hides in a second
  implementation, and the post-landing review found three more gaps that exist
  only because a second path exists.
- **Wiring that already exists is unused.** Windowed constraint atoms register
  their derivations in `plan$derivations` today, so their expiry streams would
  ride the shared schedule with no constraint-specific branch.
- **A requirement that is unmet is a requirement nobody is holding.** It has
  been unmet through two changes that both cited it.

## What Changes

- **Constraint atoms are compiled into `plan$effects` with
  `role = "constraint"`**, excluded from `initialStats` and from the output
  statistic columns while their state stays live across the event loop. The tag
  the requirement names becomes a tag something reads.
- **`build_atom_maintainer()`'s private state container, schedule and event loop
  retire.** The atoms ride the main walk.
- **The two-layer DAG stays enforced.** An atom whose inputs read the
  availability mask is rejected at parse time. That check exists in the parser
  today and must keep holding once atoms are ordinary plan effects, which is the
  one genuinely new obligation the move creates rather than inherits.

## Capabilities

### New Capabilities
<!-- none -->

### Modified Capabilities
<!-- none expected: `support-constraint`'s "Constraint atoms are non-estimated
     operands" requirement is met as written, not reworded. A delta is written
     only if implementation finds the requirement itself wrong, and then it
     says so. -->

## Impact

- **Depends on** `support-mask-sparse-updates` for the atom maintenance this
  moves and for the shared kind-shaped core the atoms already use.
- **Code**: `R/support_mask_maintain.R` (the private walk goes),
  `R/constraint_parser.R` (`augment_constraints()` writes into the main plan),
  `R/formula_parser.R` (`compile_support_constraint()` emits operands),
  `R/model_preprocess.R` and `R/preprocess_joint.R` (the walks gain the atoms).
- **It touches the unconstrained path's plan construction**, which is what the
  frozen baselines guard. Atoms enter `plan$effects` only when a constraint is
  present, so an unconstrained model's effect list is unchanged — but that is a
  claim to assert in a test, not to rely on.
- **It is the only proposed change that touches `R/support_mask_maintain.R`.**
  The post-landing review's dead-code and stale-header items are being cleaned
  on the predecessor's branch before it archives, precisely so they do not wait
  on this change.
- **Baselines**: `test-preprocess_parity.R` and `test-support_mask_maintain.R`
  plus byte-identity of `active_sender` / `active_dyad` are the detectors the
  frozen baselines cannot be, since no baseline model carries a constraint.
- **Decision record**: an ADR is likely owed on where the two-layer DAG check
  lives once atoms are ordinary effects, since that is the one invariant the
  move could quietly lose.
