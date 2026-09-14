---
depends-on: constraint-atoms-as-operands
---

## Why

**A support constraint that reads an object no formula reads is the last
thing the merged walk does not carry, and the estimators have never been
checked against a known truth on flavors, constraints or composition
change.**

`constraint-atoms-as-operands` put constraint atoms on the merged walk's
shared state and left its task 3.2 unchecked: a constraint whose atoms read
a *changing* object that no effect term reads is not in the shared registry
(`build_shared_objects()` unions only `plan$objects`, and
`plan_with_constraint_atoms()` appends effect rows but never the atoms'
objects), so its event stream is never fetched by `build_joint_schedule()`.
The batch walk hides this by falling back to `build_atom_maintainer()`, a
private state container, schedule and event loop replayed *after* the walk.
The stepping handle has no "after": `process-simulation` task 2.0a made it
refuse such a constraint (`walk_assert_covered_constraints()`,
`goldfish_walk_unsupported`) rather than serve a mask frozen at its seed,
and recorded the gap. Two implementations of one idea remain, one of them
the fallback the predecessor set out to delete, and `simulate()` (task 2.1)
would inherit the refusal. The living `multi-process-walk` spec is silent on
which objects the registry must carry; the covered/uncovered distinction
lives only in the predecessor's unarchived delta.

On the testing side, `test-classical_equivalence.R` validates goldfish's
*statistics* against `coxph`/`clogit`/`glm` on a real data set, but nothing
validates the *estimator* on a sequence whose generating process is known.
Every simulator in the suite runs on a unit clock (`time = seq_len(n)`); none
simulates exact-time waiting times, flavors, a support constraint or nodes
entering and leaving. ADR-0006, ADR-0010 and ADR-0019 each name "composition
change plus support constraints" as the case the closed-form shortcuts get
wrong, and the frozen baselines carry no constraint, so they are a floor,
not a detector. `.plan/datasets/Simulation.R` holds the only prior art
(exact-time rate plus multinomial choice with `glm` and `mlogit` twins) but
is a research scratch file: no seed, no assertion, pre-1.7.0 API, a `trans`
slice that is really receiver in-degree, and swapped rate updates in its
continuous-time block.

## What Changes

- **Constraint-only objects ride the shared walk.** Objects read only by
  constraint atoms join the shared registry after the units' objects (so no
  existing oid moves), are realized on the shared source, and their event
  streams are appended to the joint schedule after the unit streams, deduped
  by stream key. An engine whose local id is `NA` for such an object skips
  the row: no statistic is folded and no right-censoring row is emitted.
  Unconstrained and already-covered models preprocess byte-identically.
- **The private atom walk is retired.** `build_atom_maintainer()`'s state
  container, schedule and event loop, `recorder_covers_constraint()`, the
  fallback branch of `recorder_atoms_factory()` and the default
  `atoms_factory =` argument go; `walk_assert_covered_constraints()` and its
  snapshot go with them. Detector-first: byte-identity references are
  captured on the new uncovered fixture before the threading, and the
  deletion happens only once every reference matches.
- **The mask timing rule is stated once**: a support mask reads its atoms
  strictly before the event's stamp on every path. The batch already does;
  the handle defers atom moves stamped at the current clock until the clock
  passes them, so batch and replay agree even when an atom-object event
  shares a dependent event's stamp.
- **Simulation cross-validation tests.** A seeded, self-contained
  data-generating harness in the test helpers, lifted from the loop in
  `.plan/datasets/Simulation.R` with its defects corrected, simulates
  exact-time rate and multinomial choice sequences and builds by hand the
  oracle frames (one row per actor at risk per interval, one row per allowed
  receiver per event) with the same risk-set rule the process used. Five
  fixtures: unconstrained baseline; mutually exclusive creation/dissolution
  flavors with derived masks; a constraint on an object no formula reads;
  time-varying node composition; and one modeled flavor of a two-flavor
  layer. Each fixture asserts, at the classical 1e-6 discipline, that
  goldfish's fit equals a Poisson-with-offset `glm` (rate) and a
  `mlogit`/`clogit` conditional logit (choice) on the hand-built frame; that
  `compute_statistics(output = "data.frame")` enumerates exactly the
  hand-built rows; that a flavored container, its standalone single-flavor
  specs and a de-flavored data object (each flavor its own layer with a
  hand-written mask) agree; and that the handle replays the batch on every
  constrained fixture with a non-empty exclusion. DyNAM-coordination is
  excluded: it has no conditional-logit twin.
- **Not changed**: the walk extent. Whether constraint-only rows count as
  real events for the observation tail is decided in
  `observation-tail-right-censoring`; until then they are exempt, like
  window-derived rows, which keeps every current number.

## Capabilities

### New Capabilities

- `simulation-cross-validation`: the estimator-versus-oracle contract on
  simulated sequences — the harness, the hand-built frames, the fixtures,
  the parity assertions and the exclusions.

### Modified Capabilities

- `support-constraint`: "Constraint atoms are non-estimated operands" —
  every constraint is covered by the shared walk; the uncovered fallback
  scenario is replaced by a threading scenario, and the mask timing rule is
  stated.
- `multi-process-walk`: "One merged single-clock walk serves all fids" —
  the shared registry and schedule carry constraint-only objects; "A
  stepping walk handle supports evaluation and injection" — the handle
  maintains every support constraint, with batch-vs-replay equality on a
  constrained fixture.

## Impact

- **Code**: `R/preprocess_joint.R` (`build_merged_blocks()` ordering,
  `build_shared_objects()`, `build_shared_state()`, `build_joint_schedule()`,
  `run_merged_walk()`'s covariate branch, `build_walk_recorders()`,
  `recorder_atoms_factory()`, `realize_pending_masks()`),
  `R/support_mask_maintain.R` (`build_atom_maintainer()` reduced to its
  seed/template part or removed, `preprocess_pooled_support_masks()`
  signature), `R/walk_handle.R` (`walk_assert_covered_constraints()` removed;
  deferred atom moves at a tied stamp), `R/constraint_parser.R` (object
  names exposed to the registry). Tests: `helper-simulate-dgp.R` (new),
  `test-simulation_cross_validation.R` (new), `test-walk_handle.R`,
  `test-constraint_atoms_operands.R`, `test-support_mask_sparse.R` (the
  `mask_call_counts` comment), `helper-constraint-atoms.R` (new reference
  family), `_snaps/walk_handle.md`.
- **Numbers**: none move for unconstrained or covered models (byte-identity
  asserted); the uncovered case changes from a private replay to the shared
  walk with the same strictly-before rule, so its masks are identical by
  construction and a reference captured before the threading proves it.
- **Dependencies**: none added. `survival` and `mlogit` are already in
  Suggests; `stats::glm` runs live.
- **Sequencing**: `depends-on: constraint-atoms-as-operands` (unarchived on
  this branch; its delta scoped the walked-once scenario to covered
  constraints). Closes the gap recorded in `process-simulation` task 2.0;
  its task 2.1 does not wait on this. The walk-extent cell is owned by
  `observation-tail-right-censoring`. Branch `feature_simulation`; NEWS
  fragment, no Version bump.
- **ADRs**: ADR-0073 (constraint-only objects on the shared walk, private
  walk retired, strictly-before rule everywhere), ADR-0068 (DAG check stays
  in the parser), ADR-0069 (real tail extends the window; the extent
  question), ADR-0054 (retire a reserved function when its consumer lands),
  ADR-0006/0010/0019 (the composition-plus-constraint case).
