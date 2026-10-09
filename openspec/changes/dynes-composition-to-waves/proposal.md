---
status: landed (feature/dynes, awaiting fold)
---

## Why

The sim augmenter selects an interval's next event in proportion to rates read
at the walk state after the previous event. A composition change inside a
panel interval makes those rates zero for an actor before it joins (or after it
leaves), and that zero breaks the draw in two ways:

- **Strand.** If every event left to place belongs to an actor that has not
  joined yet, the step's total weight is zero and the attempt strands. It
  strands on every redraw, the restart ceiling aborts the fit. An actor that
  joins at the stamp of its own first observed event (composition derived from
  first activity) always hits this, with or without a support constraint,
  because the driver's breakpoint clip is strict and the last clock call reads
  the composition before the stamp.
- **Zero proposal mass.** If other events are pending, the joining actor's event
  can never be selected while its weight reads zero. Every ordering that places
  another event after it has proposal density zero while the target gives it
  positive density, so the importance-sampling estimate is biased, and more
  draws do not shrink the bias.

A **leave** inside an interval causes the mirror: a flip whose waiting time is
censored at the leave redraws at rate zero and strands. It strands even with no
user support constraint. That breaks the `dynes-augmenter-walk-alignment`
premise that an uncoupled draw cannot strand (one walk per draw), and the
restart it triggers puts a theta-dependent completion normalizer into
`log_proposal` without the coupling warning.

The exact remedy is to bound each actor's events by its true join and leave
times. That is deferred to `dynes-augmenter-driver-v2`. This change is the v1
approximation, and it must land before `dynes-augmenter-walk-alignment`
records its pre-change references and before the `dynes-augmentation` 6.2
recovery sweep.

## What Changes

- **Composition snapped to the panel waves for involved actors.** At the
  DyNES estimation entry, once the wave grid is known and before the
  specification is completed, an actor that appears in interval
  `k = (w_{k-1}, w_k]` as the sender or receiver of an observed event of a
  modeled relational process, or as the `from`/`to` of a flip (a panel change
  the interval must draw), is made present over the whole interval:
  - a **join** in `(w_{k-1}, w_k]`, including one exactly at `w_k`, takes
    effect at `w_{k-1}`, the start of the interval;
  - a **leave** in `[w_{k-1}, w_k)` takes effect at `w_k`, the next panel wave;
  - several changes of one actor in one interval (a leave and a rejoin, or
    longer alternations) collapse to "present over the interval, then its
    real state at `w_k`". The actor's presence path is rebuilt interval by
    interval, so the result never depends on row order, and an actor involved
    in two consecutive intervals stays present across the wave between them.

  This holds for senders and receivers alike. An actor not involved in the
  interval keeps its real presence times: nothing is drawn for it, so the
  proposal never reads its weight, and the target's compensator stays as the
  data has it. A join at `w_{k-1}` is in force for relational events stamped
  exactly at `w_{k-1}` (interval `k-1`). That one-interval-early effect is
  accepted in v1 and stated in the warning.
- **A warning.** One cli warning per fit, with a `goldfish_*` condition class,
  names the moved actors and the original and new times. It says that the
  rewrite changes the model (the moved actor's rate enters the compensator over
  the whole interval) and that the real time is ignored.
- **One composition for the target and every proposal.** The rewrite runs once,
  before completion, on the data that the likelihood, completion (a pinned
  relational rate reads presence) and all three augmenters read. The
  augmenters cannot do it themselves, because the target and the completion
  must read the same composition. Composition lives in the `active` rows of
  `data$changes`, which `bind_candidate_data()` does not compare today. Its
  guard is extended to the composition, compared in a normalized form (only
  the `active` rows; numeric times, global node ids, logical values, sorted;
  `NULL` and zero rows equal). A candidate carrying a second composition then
  aborts with an explained error instead of being walked, and an encoding
  difference alone never aborts.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `sequence-augmentation`: adds the requirement that, for actors involved in
  an interval, presence changes strictly inside the interval are moved to the
  interval's waves, with a warning. The capability is introduced by the
  active `dynes-augmentation` change, so the requirement is written as
  `## ADDED` and this change archives after `dynes-augmentation`.
- `rate-queries`: adds the requirement that binding a candidate also compares
  its normalized composition with the specification's. The capability is
  introduced by the `rate-queries` change, which is complete but not yet
  archived. The requirement is a separate `## ADDED` one, so the existing
  "rebound without recompletion" requirement keeps its wording, and this
  change archives after `rate-queries`.

depends-on: dynes-augmentation, rate-queries

## Impact

- **Per-event code — no.** The rewrite runs once per fit, before completion.
  It adds no preprocessed output, no walk and no buffer. It edits the `active`
  rows of `data$changes` (never `data$nodes`) before the walk's composition
  buffers (`presence1_changes` / `presence2_changes`) are built from them.
- **R:** `R/estimate_dynes.R` (the call, after `panel_wave_grid()` and before
  `complete_generative_spec()`); a helper that computes each interval's
  involved actors (modeled relational events from `data$ties`, flips from
  `diff_panel_waves()`) and rewrites the composition on the joint
  specification's data and on each process specification's data, as
  `bind_candidate_data()` rebinds ties; `R/rate_queries.R`
  (`bind_candidate_data()` also guards the normalized composition).
- **User-facing:** a new warning, and fitted coefficients that change on data
  with an in-interval join or leave of an involved actor. Data without one
  fits byte-identically, with no warning. On such data, a `compute_lik_seq()`
  call that mixes the fit's specification or sequences with the original ones
  now aborts with an explained error, where today it silently scores the
  candidate's composition. NEWS.d fragment.
- **Tests:** fixtures with a join at an event's stamp; a join exactly at a
  wave with the actor's flip in the interval ending there; a join before an
  event in the same interval; a join of a flip's receiver (and of a two-mode
  receiver on the second node set); a leave before a flip's end; a leave
  exactly at the interval start; a leave and rejoin in one interval; an actor
  involved in two consecutive intervals; and an uninvolved actor that keeps
  its times. Also: no-op data returned identical with no warning; the diffs
  taken before the rewrite equal the recipe's; every augmenter's candidate
  passes the extended guard; the guard passes on encoding-only differences
  (`NULL` versus zero-row `changes`, numeric versus POSIXct times, label
  versus index nodes, a differing non-`active` attribute row) and aborts on a
  real composition difference, including the two mixed `compute_lik_seq()`
  pairings; snapshots of the warning and of the guard's abort (pinned cli
  context); a sim draw that strands today and completes after.
- **Sequencing:** before `dynes-augmenter-walk-alignment` (its task 1.1 records
  references after this lands) and before the `dynes-augmentation` 6.2 sweep.
  Superseded by the true bounds in `dynes-augmenter-driver-v2`.

## Resolved Questions

Both were settled in `design.md`:

- **Ordering (D1).** The rewrite runs before completion. Involvement reads the
  wave diffs directly, which do not read presence, so it does not wait for
  the recipe. Running it after completion would pin a completed relational
  rate on the original composition.
- **Presence changes at a wave (D3).** Intervals are right-closed and a
  change is in force at its stamp. So a join exactly at `w_k` belongs to
  interval `k` (it takes effect at `w_{k-1}` when the actor is involved), and a
  leave exactly at `w_{k-1}` belongs to interval `k` (it takes effect at `w_k`).
