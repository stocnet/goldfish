---
depends-on: constraint-objects-on-shared-walk
---

## Why

**The availability the engines read is one folded object per axis, but the
objects the preprocessed output stores and the arguments the engines take
still describe the two-object world the fold replaced.** `support-constraint-as-stat`
(2026-07-10, living spec `active-availability-stat`) decided that preprocessing
emits exactly one availability object per recipe loop with presence, gates,
support and opportunity list already intersected, and that raw presences and a
raw support object are neither stored nor passed. `constraint-objects-on-shared-walk`
task 4.5b made that fold an effect maintained in the walk (ADR-0106). The
likelihood side was never brought to the same statement:

- Every kernel signature carries both axes. The DyNAM-rate kernels receive and
  update `active_dyad` and never read it; the REM, REM-ordered and coordination
  kernels receive a raw `active_sender` and AND it into a point grid that
  already contains sender presence, while the R backend ignores it there
  (`folded_full`). Only the choice kernel takes one object.
- The preprocessed object keeps the raw operands beside the fold: the
  `active_sender_changes` / `active_dyad_changes` lists, and raw presence
  copies stashed on `support_mask` (`sender_presence_init`,
  `receiver_presence_*`) for a validation that `constraint-objects-on-shared-walk`
  task 4.6 retired. Nothing reads the stashes; the R backend reads the raw
  change lists for an unfolded axis and filters a data frame by time on every
  event, where the C++ and gather backends walk the pointer buffer.
- A vector-kind constraint on REM or coordination (`~ alter(attr) > 0`,
  `~ ego(attr) > 0`) is folded on a dense point grid although the outer
  encoding, two factor vectors, represents it exactly. On the 1899-actor
  CollegeMsg data that is a 13.8 MB grid plus a column per presence change
  where two vectors of 7.6 KB and one entry per change would do.
- The simulation driver's deferred-fid path rebuilds a dense grid from
  presence and support on every call, re-intersecting what the walk already
  folded (`R/simulate_driver.R`, the `support_to_grid()` branch).

Two truths of one fact, one of them unread, is the defect `support-constraint-as-stat`
D4 named when it rejected "store `active1`, `active2` and a `support` residual
and intersect at consumption". Readers meet two names, an encoding switch and a
dead argument in every engine. Now is the time because the fold is in the walk,
the effect is the single producer, and the next changes on the likelihood
(estimation-side cost, the estimation contract matrix) should start from
signatures that say what the loop reads.

## What Changes

- **One availability object per loop in every engine signature.** The sender
  loop (DyNAM rate, rate-ordered) takes `active_sender_*` only. The dyad loop
  (DyNAM choice, coordination, REM, REM-ordered) takes `active_dyad_*` at its
  encoding; at the outer encoding the sender factor rides as the object's own
  second factor, and at the point encoding no sender factor is passed. The R
  backend, the gather expansion and the six C++ kernels change together; the
  Rcpp bindings are regenerated.
- **The raw operands leave the preprocessed object.** The `active_*_changes`
  lists and the `support_mask` presence stashes are not stored. Composition
  changes reach every backend through the `active_*_update` /
  `_update_pointer` buffers the writers already produce; the R backend's
  per-event data-frame filter goes with the lists. The mask stream itself
  (`support_mask$initial`, `update`, `update_pointer`, kinds) stays as the
  constraint's own derived value, inspectable and read by no engine.
- **REM and coordination keep vector-kind constraints at the outer encoding.**
  An alter-kind mask folds into the receiver factor, an ego-kind mask into the
  sender factor, a global mask into the receiver factor; coordination's
  symmetrisation of a vector mask is `m[i] & m[j]`, which is the outer
  product of the same vector. Only a point-kind atom or an opportunity list
  forces the grid, as the choice loop already does.
- **The handle and the simulation driver read the effect at its kind.**
  `walk_risk_set()` returns the effect's factors for an outer-encoded REM;
  the simulation driver's deferred-fid path reads the maintained availability
  instead of composing presence with a grid.
- **The ego-kind choice constraint stays validation-only**, stated in the
  spec: it cannot change a choice likelihood (the event's sender is given), so
  it is not folded into the receiver object and the observed-event check is
  its only enforcement. The living scenario that has it fold "as row flips"
  is corrected to match.
- **Not changed**: numbers. Every backend already reads the folded object, so
  no coefficient, log-likelihood, exposure or exported row moves; the frozen
  1e-6 baselines and the C++ goldens stay PASS. **Not changed**: the
  preprocessed format version. Development versions are free until 2.0.0
  (Alvaro, 2026-10-01); a stale object carrying the old fields is still read.
  **Not in scope**: the per-event `allowed(n1 * n2)` allocation in the REM
  kernels and the R backend's list-held grid copy, which belong to the
  estimation-side cost change; the living-spec contradictions the archive
  survey found beyond the scenarios this change touches, which belong to a
  spec-hygiene change.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `active-availability-stat`: "Availability output keyed by recipe loop" (the
  raw change lists and presence stashes join the SHALL NOT list; the engine
  argument shape is stated); "active_dyad encodings with accessor-mediated
  consumption" (REM and coordination vector-kind constraints stay at outer);
  "Per-family folding declared by the recipe constructor" (the ego-kind
  choice constraint is validation-only, not row flips; the REM/coordination
  folding of a vector mask into a factor); "One homogenized availability name
  across all sites" (each engine takes only its loop's object).
- `support-constraint`: "Symmetric mask for coordination and undirected REM"
  (the symmetric fold of a vector-kind mask is the outer product of the
  vector, no grid).

## Impact

- **Code**: `src/DyNAM_rate_default.cpp`, `src/DyNAM_rate_ordered_default.cpp`
  (drop the `active_dyad_*` arguments), `src/REM_default.cpp`,
  `src/REM_ordered_default.cpp`, `src/DyNAM_MM_default.cpp` (sender factor
  used only at the outer encoding, empty at point), `src/RcppExports.cpp` and
  `R/RcppExports.R` regenerated (cpp-recompile skill); `R/cpp_interface.R`
  (`make_engine_evaluator()`, the per-kernel dispatch, the gather loops);
  `R/estimation_core.R` (`estimate_int()`'s `compChange*` path replaced by
  the pointer-buffer walk the folded path already uses; `folded_full`
  simplified); `R/availability_effect.R` (`availability_finish()` stops
  stashing; the `dyad` variant gains an outer form), `R/preprocess_joint.R`
  (`build_engine_availability()` picks outer for a vector-kind REM or
  coordination constraint), `R/preprocess_writers.R` (the writers stop
  emitting `active_*_changes`; `active_dyad_encoding_decide()` serves REM and
  coordination too), `R/dynami_bridge.R`, `R/methods_display.R` (field
  list), `R/walk_handle.R` (`walk_risk_set()` outer form),
  `R/simulate_driver.R` (deferred-fid availability from the effect).
- **Per-event code touched**: the six kernels' argument lists and update
  walks (`apply_availability_updates()` in `src/availability_updates.h` is
  reused unchanged), the R backend's per-event availability step (the
  `compChange*` data-frame filter is deleted; the folded pointer walk through
  `.availability_event_cols()` serves every axis), and the simulation
  driver's deferred-fid read. No new preprocessed output, no new walk
  consumer, no new buffer format: the outer-encoded REM availability is
  `new_availability_effect()`'s existing factor form written through
  `write_entries()` / `set_entries()` and the existing writers.
- **Tests**: `helper-constraint-atoms.R` (the byte-identity field list loses
  `active_*_changes` and the stashes; references re-saved with every kept
  field `identical()`), `helper-parity-fixtures.R` and the parity goldens
  (same), `test-support_mask_sparse.R` and `test-support_mask_maintain.R`
  (readers of the stashes, if any, move to the mask stream), new tests: a
  vector-kind REM and coordination constraint at outer with r = cpp = gather
  equality and a cell-equality check against the point fold, a stale-object
  read, the simulation driver's deferred fid on a constrained REM.
- **Numbers**: none move. Byte-identity holds for every field that stays;
  the point-fold references of REM and coordination with a vector-kind
  constraint move to the outer form with cell equality asserted before the
  re-save.
- **Dependencies**: none added. `depends-on: constraint-objects-on-shared-walk`
  (the effect this change reshapes; its deltas are written against the
  predecessor's wording). Branch `feature_simulation`; NEWS.d fragment, no
  Version bump (ADR-0040).
- **ADRs**: ADR-0109 (one availability object per loop; vector-kind REM and
  coordination constraints at outer kind; the ego-kind choice constraint is
  validation-only), ADR-0106 (availability as an effect), ADR-0061
  (kind-shaped state), ADR-0067 (the shared availability apply is the pointer
  walk), ADR-0054 (retire a reserved function when its consumer lands).
