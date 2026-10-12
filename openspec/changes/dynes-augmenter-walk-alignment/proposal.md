## Why

The `upstream/feature_simulation` merge (f663b266) gave the walk handle and the
simulation driver the batch walk's stamp semantics (`walk_advance(inclusive =
FALSE)`, draw-then-inject per stamp in the time-anchored driver) and removed the
last walk-handle refusal the DyNES augmenters were written around. The
augmenters predate both, so in three places they now read the walk differently
from the likelihood they feed, and in three others they pay for walks and rate
evaluations they never use. The parameter-recovery sweep (`dynes-augmentation`
task 6.2) is about to multiply every per-draw and per-step cost by the replicate
count, so both should be settled before it runs.

## What Changes

- **Legality replay at the batch stamp semantics.** The support-legality replay
  shared by `augment_seq_random()` and `augment_seq_mcmc()` checks an event
  against the state strictly before its stamp, as the batch walk reads a support
  mask, and injects the events of a stamp only after all of them are checked.
  Today it advances inclusively, so an exogenous row sharing the stamp is
  applied before the check, and a tied observed event is checked against the
  one injected before it.
- **Rate-query replay on the handle's own advance.**
  `walk_replay_to()` reimplements `walk_advance()`'s exogenous branch for one
  row and reaches into handle internals to do it. The copy already lacks a
  replay-skip check that upstream added. That gap is latent, since no
  rate-query walk registers replay rows, but it would diverge the moment one
  did. The query contract (`after = k`, the state right after candidate event
  `k`'s realization) is kept. The copy retires in favor of the shared stamp
  helper over earlier stamps, `walk_advance(inclusive = FALSE)` at the
  query's stamp, and the realizations at that stamp through `k`. One stamp
  helper serves this replay, the legality replay, the sim prefix and the fused
  MCMC pass.
- **Tied observed relational events in the sim augmenter.** After the first of
  two tied events is placed, the second is weighted against a state that
  already includes the first and the stamp's exogenous rows. If a flip is
  selected at that stamp instead, the draw hangs (a zero-width truncated
  draw). Later events of a tied stamp become forced steps, and no flip is
  offered between them. Every event of the stamp, the first included, has its
  support checked once, at the walk state strictly before the stamp.
- **An observed event at a wave time.** It competes with the pending flips
  today. If it is selected first, the flips left in its interval get a
  zero-width truncation window and the draw hangs. Such an event is placed
  last in its interval, with probability 1.

All the stamp items apply one rule: a dependent event is an actor's decision,
made before the moment it is acted on, so nothing stamped at that moment
informs it.
- **The sim augmenter's forced prefix evaluates no rates.** The observed events
  and earlier intervals' flips that each interval attempt replays are applied
  the way the time-anchored driver applies observed stamps (advance strictly
  before, inject), not as full driver steps that evaluate every rate fid and
  discard the result. The driver still evaluates once at the walk's opening
  state, before the step contract gives the augmenter control, so at most one
  rate round per attempt precedes the interval, whatever the prefix length.
  Removing that round is `dynes-augmenter-driver-v2`. For a specification with
  no user support constraint, once `dynes-composition-to-waves` has removed
  in-interval composition changes for involved actors, a draw cannot strand.
  One walk then carries the whole draw and there is no prefix to replay.
- **One rate evaluation per fid per step in the sim augmenter.** The candidate
  weights, the relational weight and the waiting time read one cached rate
  vector per fid per step instead of re-evaluating it per candidate pair; the
  per-step data-frame row reads in the same path become reads of
  precomputed columns.
- **Two walks per MCMC step, not three** (for a specification with a user
  support constraint; an unconstrained one has no legality walk and already
  opens two, measured in task 1.1). The legality check and the reverse
  rate queries both walk the proposed sequence over the same interval; they
  run in one pass, inside the proposal evaluator. The evaluator holds theta and
  the `control_prep` the likelihood uses. Today the legality walk always opens
  with the default `control_prep`, so a non-default one (a `start_time`)
  makes the gate walk a different configuration from the target it gates. The
  standalone legality checker of `augment_seq_random()` moves to the
  evaluator's `control_prep` as well.
- **Stale record of the removed refusal.** The `dynes-augmentation` design
  still says one `goldfish_walk_unsupported` refusal remains, and its
  characterization test asserts the refusal's absence, which can no longer
  fail. The design passages are corrected and the test is replaced by one that
  asserts what the merge enabled: a constraint on an object no formula term
  reads moves the augmenter's live mask.

No user-facing API changes. The drawn sequences, their log proposal densities
and the fitted coefficients are unchanged wherever no observed event shares a
stamp with an exogenous row or with another event, and none sits at a wave
time. The legality change alters the accept/reject decision only at such ties,
and only towards the batch walk's answer.

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `sequence-augmentation`: adds requirements on the augmenters' walk semantics
  (legality read strictly before the stamp) and on their walk and evaluation
  counts (no rate evaluation over a replayed prefix, one walk per draw when
  nothing can strand, at most two walks per MCMC step). The capability is not
  yet in the living spec — it is introduced by the active `dynes-augmentation`
  change — so its requirements are written as `## ADDED` and this change
  **archives after `dynes-augmentation`**.

## Impact

- **Per-event code — yes.** Every item runs once per augmenter step (the sim
  draw's steps, the legality replay's events, the MCMC move's walk). Nothing
  adds a preprocessed output, a buffer format, or a walk: the changes remove a
  walk (MCMC), remove evaluations (sim prefix, per-pair re-evaluation), and
  move a replay onto the handle's own primitives. Reused shared functions:
  `walk_advance(inclusive = FALSE)`, `walk_inject()`, `walk_next_breakpoint()`
  and `walk_risk_set()` (`R/walk_handle.R`), the time-anchored driver's
  draw-then-inject discipline (`R/simulate_anchored.R`), `evaluate_fid()` /
  `fid_evaluation_inputs()` and the deferred choice's `support_row()` read
  (`R/simulate_driver.R`), and the step contract of `set_simulation_steps()`
  (no new plug point).
- **R:** `R/augment_seq_random.R` (legality checker), `R/augment_seq_sim.R`
  (prefix, single walk, rate cache, column reads), `R/augment_seq_mcmc.R`
  (fused candidate pass), `R/rate_queries.R` (`walk_replay_to()`),
  `R/preprocess_joint.R` (the ordering comment gains its rationale),
  `R/evaluator_dynes.R` (`make_proposal_evaluator()`'s closure gains an
  optional `legality` argument that runs the fused pass; calls without it
  behave as today).
- **C++:** none.
- **Tests:** count-based cost guards (walk opens, rate evaluations), seeded
  draw-identity regressions for the sim augmenter, stamp-tie legality
  fixtures, and the replacement for the vacuous refusal test in
  `tests/testthat/test-augmenters_dynes.R`.
- **Docs:** `openspec/changes/dynes-augmentation/design.md` (two passages).
- **Sequencing:** after `dynes-augmentation` tasks 3.x–5.x (landed) and after
  the prerequisite `dynes-composition-to-waves` (task 1.1 records its
  references once that lands), before the `dynes-augmentation` 6.2 sweep;
  archives after `dynes-augmentation`. Follow-up: `dynes-augmenter-driver-v2`
  (zero-evaluation prefix, rate vectors handed to the clock, true composition
  bounds).
