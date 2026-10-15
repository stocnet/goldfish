---
status: landed (feature/dynes, awaiting fold)
depends-on: constraint-objects-on-shared-walk
---

## Why

When a support constraint reads its own focal layer and no formula term
reads that layer, the merged schedule carries the layer's event stream
twice, so every observed event is written into the shared state twice. A
constraint that reads the layer's weights then sees doubled values. On data
that satisfies `outdeg(calls, type = "ego", weighted = TRUE) < 2`, a plain
`estimate_dynam()` fit aborts with "empty risk set at event 2"; adding a
term that reads `calls`, which removes the duplicate, makes the same fit
succeed. The DyNES augmenters inherit the defect: the batch log-likelihood
of such a candidate is `-Inf`, and the `augment_seq_random()` legality gate,
which applies each event once, accepts it.

The defect sits between two landed commits.

- `2822625b` (2026-09-16) added a fallback loop to `build_joint_schedule()`
  that registers the stream of a focal layer no term planned. The loop does
  not record what it added.
- `b1b21033` (2026-09-29, `constraint-objects-on-shared-walk`) then
  appended the streams of objects only a constraint reads, skipping any
  stream or object already seen. The focal object had been added but never
  marked seen, so the constraint loop added its stream again.

The constraint loop is meant for objects that only a constraint reads; the
focal object is not one.

## What Changes

- **Every added stream is recorded where it is added.** In
  `build_joint_schedule()`, `add_part()`, which every loop calls, records the
  key of each state-update stream, and every loop skips a stream whose key is
  already recorded. The focal-layer loop's streams are therefore seen by the
  support-constraint loop, which never adds a stream already on the
  schedule. Skipping is by stream key only, no longer by object. The
  remaining rows keep their relative order. A stream that a support
  constraint adds after the dropped one has its stream index lowered by one.
- **Every reader then applies each observed event once.** That covers the
  batch preprocessing behind every estimator, the stepping walk handle, the
  rate-query replay, and the DyNES fused candidate pass, which until now
  agreed with the doubled batch. The `augment_seq_random()` legality
  checker, which never doubled, now agrees with them.
- **Rate-query positions count each event once.** A candidate's
  realizations drop to one per observed event, so `after = k` addresses the
  same event as `mcmc_ordinal()` on affected specifications.
- **A schedule guard.** No source event stream key appears twice among the
  joint schedule's state-update streams. `add_part()` checks each key as it
  records it, before the walk, and aborts with an internal error. The key
  is the stream's identity: on the stocnet path it is the layer's name (or
  a view-qualified attribute key), so two objects never share one. It is
  not per row content, because two distinct streams may carry identical
  rows.

No user-facing API changes. Fits on affected specifications change: a
weighted read of the focal layer now sees the observed value, and an abort
caused by a doubled weight no longer fires. Specifications where a term
reads the focal layer, or where the constraint reads it only through tie
presence or tie counts, give the same numbers as before. Doubling a tie
leaves both its presence and its count unchanged.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `multi-process-walk`: adds a requirement that each source event stream is
  on the joint schedule once, so each observed event is applied to the
  shared state once, whichever loop of the schedule builder registers its
  object.

## Impact

- **Per-event code — yes, by removal.** The schedule is the walk's per-event
  input, and an affected specification drops one full stream, its rows
  stepped, applied and right-censoring once per event. Nothing is added to
  the walk. The fix reuses the builder's own `stream_seen` bookkeeping
  (`R/preprocess_joint.R`, `build_joint_schedule()`), the shared registry
  (`build_shared_objects()`) and the schedule format unchanged. The guard
  runs once per added stream while the schedule is built, at O(number of
  streams) per schedule, and nothing per event.
- **R:** `R/preprocess_joint.R` (`build_joint_schedule()`, its `add_part()`
  closure, and a stream-key assertion helper `add_part()` calls). No change in
  `R/walk_handle.R`, `R/rate_queries.R` or the augmenters: they read the
  schedule.
- **C++:** none.
- **Tests:** a red/green pair on a plain `estimate_dynam()` fixture with an
  exact-threshold control, a per-object stream-count check (single-process,
  two-object and cross-process fixtures), a unit test of the stream-key
  assertion (including distinct keys on one object), a permanent check that
  a term on the focal layer keeps the planned streams, the DyNES agreement between the legality checker, the
  fused pass and the batch log-likelihood, and realization positions. The
  frozen coefficient baselines use no support constraint, and the frozen
  constraint-atom captures (`helper-constraint-atoms.R`) carry no
  duplicate (scanned: at most one stream per object in every family), so
  neither should move.
- **Sequencing:** on `feature/dynes`, after
  `dynes-augmenter-walk-alignment` closes its conformance follow-ups (its
  task 8.8), so that this change's reruns of the seeded augmenter tests see
  the final v1 augmenters, including any change to the sim walk's
  preprocessing control. Before `dynes-augmentation` 6.2.6 onward, so those
  tasks run on a schedule that applies each event once. Folds after
  `constraint-objects-on-shared-walk` and after
  `dynes-augmenter-walk-alignment`. The
  fix belongs to shared walk code that `process-simulation` and
  `constraint-objects-on-shared-walk` (upstream `feature_simulation`) also
  edit, so it should also be offered upstream.
