## Context

The DyNES augmenters drive the `multi-process-walk` handle from outside:

```
 augment_seq_random --+                     +--> walk_open / walk_advance / walk_inject
                      +--> legality replay -+
 augment_seq_mcmc ----+                     +--> walk_risk_set (live support)
        |
        +--> make_proposal_evaluator --> query_rate_at_position --> walk_replay_to
        |                                                           (own exogenous step; D2 retires it)
        +--> evaluate_candidate (full re-preprocess; unchanged here)

 augment_seq_sim ----> simulate_replicate (driver) with supplied clock/mark/accept
                       one driver run per interval attempt, prefix replayed as steps
```

The `upstream/feature_simulation` merge (f663b266) changed three things under
them:

1. `walk_advance(handle, t, inclusive = FALSE)` stops strictly before `t`
   while moving the composition up to `t` — the state the batch preprocessing
   evaluates an observed event at (commit 58b806da).
2. The time-anchored driver (`R/simulate_anchored.R`) draws every event of a
   stamp over the state strictly before it and injects them together afterward
   (commit 83a414fc), so no event at `t` informs another draw at `t`, as the
   batch reads a support mask strictly before its stamp. It applies observed
   stamps with no rate evaluation between them.
3. `walk_assert_covered_constraints()` is gone (commit b1b21033): a constraint
   on a changing object no formula term reads now rides the shared walk. The
   live risk set is the availability effect's own buffer, written in place by
   the next step (`walk_risk_set()`).

Checked and unaffected: `walk_event_supported()` indexes the new availability
buffers correctly (point/dyad variants hold an `n1 x n2` logical matrix; the
factors variant splits sender and receiver factors across `active_sender` /
`active_dyad`), and it reads them before the walk moves. The sim augmenter's
choice reads already go through `evaluate_fid(sender =)` and so inherit the
deferred choice's no-grid `support_row()` read.

**The stamp rule every decision below applies.** At a shared stamp, a
dependent event reads the state strictly before the stamp, with the node
composition at the stamp included. Nothing else stamped at the same time
informs it: not an exogenous row, not a flip, not another dependent event.
The reason is the actor's agency. A dependent event is an actor's decision,
and the thought process behind it precedes the moment the actor acts.
Something that happens at that same moment cannot have entered the decision,
so it must not be informative to it. The batch schedule already encodes the
rule: `order(time, !dependent, stream_index)` puts every dependent row of a
stamp first (`R/preprocess_joint.R`, the merged schedule builder). A
realization of a dependent event is a focal-layer row, which is not a
dependent row and so sorts after all of the stamp's dependent rows.

Measured so far (`dynes-augmentation` progress journal): one legality walk
opens in about 50 ms on the 4-actor fixture; a coupled specification spends
about 10 s per coupled interval in the legality pilot.

## Goals / Non-Goals

**Goals:**

- The augmenters read live support at the stamp semantics the likelihood
  (batch walk) uses.
- No rate is evaluated that no density or decision reads.
- No walk is opened that another walk over the same sequence could serve.
- Every gain is pinned by a count-based guard that fails on today's code.

**Non-Goals:**

- Incremental re-preprocessing of a candidate sequence (the full re-preprocess
  per scored sequence stays the `dynes-augmentation` baseline).
- Walk cloning or snapshotting (would let a coupled interval restart without a
  prefix replay; not built, and not needed once the prefix is cheap).
- Changing the `set_simulation_steps()` contract or the driver's public
  behavior.
- The excursion MCMC moves, alternatives sampling, or any C++ change.

## Decisions

### D1. The legality replay reads support strictly before the stamp and injects a stamp together

`new_legality_checker()`'s replay groups the candidate's modeled rows by stamp.
For each stamp it calls `walk_advance(handle, t, inclusive = FALSE)`, checks
every in-interval event of the stamp with `walk_event_supported()`, and only
then injects the stamp's events in their row order. This is the time-anchored
driver's loop with a check where the driver has a draw.

Drawn flips never tie anything (continuous draws strictly inside an interval),
so the decision changes only for observed relational events that share a stamp
with an exogenous row or with each other. In those cases the current replay
can reject a sequence the likelihood scores as legal, or accept one it scores
as illegal.

*Alternative considered:* keep the inclusive advance and document the
divergence. Rejected: legality is an accept/reject gate on the target's support,
so a gate that disagrees with the target biases the chain.

The replay is the stamp helper of D8 with `walk_event_supported()` as its
check hook.

The same question applies to `sim_relational_weight()` under coupling, where
the driver injects an observed event before evaluating the next tied one. D3
resolves it for the prefix, and D7 resolves it for in-interval ties without a
driver change.

### D2. `walk_replay_to()` on the stamp helper; the row-level copy retires

`walk_replay_exogenous_row()` copies the exogenous branch of `walk_advance()`
to apply exactly one row. It reaches into handle internals (`exo_cursor`,
`exo_rows`, `merged_build_event_args`, `walk_apply_object_event`), and it is a
second hand-written copy of a walk step, which D8 removes. Upstream has since
added replay-skip handling (`replay_row_applies()`) to that branch, and the
copy did not follow. That drift is **latent**, not live: only the simulation
drivers call `walk_replay()`, and the rate-query walk is opened through
`walk_open()` alone, so its `replay_rows` is always empty and no rate query
can observe the missing check. It would diverge the moment a rate-query walk
registered replay rows.

**The query contract is kept.** `query_rate_at_position()` addresses a query by
realization, not by dependent row: `after = k` is the state right after
candidate event `k`'s realization, with no exogenous row at its stamp applied,
and two tied events are two positions. In practice a proposal only queries
after the last realization of a tied group, because a continuous draw cannot
land between two tied events. The contract nonetheless allows the intermediate
position, and the rewrite keeps it. Tightening it to stamp positions was
considered and rejected: it changes a contract the `rate-queries` change owns,
for a case no caller uses.

The state at `after = k`, with realization `k` at stamp `s`:

```
rows < s | comp(s) | dep(s) dep(s) | real(k1) real(k2) exo(s)...
                                      ^after=k1  ^after=k2
```

`walk_advance()` does not apply focal-layer rows (`exo_rows` leaves them out),
so `walk_replay_to()` for `after = k` becomes: the D8 helper over the
candidate's stamps before `s` with no check hook, then
`walk_advance(handle, s, inclusive = FALSE)` (the composition at `s` comes in,
no exogenous row at `s`), then the realizations at `s` through `k` in schedule
order. The row-level exogenous step and `walk_replay_exogenous_row()` retire.

Here a *realization* is every non-dependent focal-layer row of the candidate,
including the rows of a flavor no process models, in schedule order. Those rows
write the focal network, and today's `walk_replay_to()` injects them through
its focal branch. At an earlier stamp `s'`, the order of the focal injections
and the exogenous rows at `s'` is immaterial: nothing is evaluated between
them, and in the rate-query walk the exogenous rows write only non-focal
objects.

One behavior moves at ties. Non-dependent rows at a stamp are ordered by
`stream_index`, so today an exogenous row at `s` whose stream sorts before the
focal stream *is* applied at `after = k`, despite the docstring. The rewrite
never applies it, which is what the docstring promises.

A test pins the ordering at the schedule builder (a dependent event tied with
a covariate change and a composition change) so that a later reordering of the
schedule fails there and not silently in the rate queries.

### D3. The sim augmenter's forced prefix is applied as anchored injections, in the clock step

Today each interval attempt is one `simulate_replicate()` run whose
`n_events` includes the forced prefix: every observed event and every
earlier-interval flip up to the interval start. Each of those is a full driver
iteration. At each one, `rate_values()` evaluates every rate fid, the clock
returns the forced event, `mark_evaluation()` runs, and the event is
injected. The evaluations are discarded, because the forced events add nothing
to `log_q`.

Instead, the first `sim_clock()` call of an attempt applies the prefix itself
with the handle the step receives, using the anchored discipline (the D8
helper, no check hook): for each forced stamp,
`walk_advance(handle, t, inclusive = FALSE)` and then inject the stamp's
events. It then advances the handle through the gap,
`walk_advance(handle, wave_start, inclusive = FALSE)`, and returns a
`"breakpoint"` wait to `wave_start` as it does now. Without that last advance,
every exogenous breakpoint between the last forced stamp and `wave_start`
would cost the driver one more full rate round. The driver's `t` then lags the
handle's `current_time`, which is safe because both only move forward and the
driver's next advance is to `wave_start`. The attempt's `n_events` drops to
the interval's own relational events and flips. No driver change is needed:
the augmenter is already an external driver of the handle, and the steps
receive the handle.

**One round remains.** The driver evaluates every rate fid (`rate_values()`)
at the walk's opening state *before* its first `clock()` call, so the
clock-step route cannot reach zero evaluations before the interval opens. The
requirement is therefore at most one rate round per attempt before the
interval opens, independent of the prefix length. The quadratic-in-K term
still disappears. Reaching zero needs the `prefix` argument on
`simulate_replicate()` below, which is the scope of
`dynes-augmenter-driver-v2`.

*Alternatives considered:*

- `walk_replay()` the prefix rows. Rejected: the driver owns the single
  `walk_replay()` registration (it registers unmodeled flavors first and the
  registration is once-only). Also, the earlier flips are not schedule rows of
  the walk's data, where every flip is stamped at the first wave.
- `walk_schedule()` the prefix. Rejected: a driver-scheduled event goes
  *after* an observed exogenous row sharing its stamp. Today's forced replay
  places it *before* (the driver's `t + wait > breakpoint` test is strict).
  Scheduling would therefore change the state at ties.
- A new internal `prefix` argument on `simulate_replicate()`. It is the only
  route to zero evaluations before the interval, and it touches shared driver
  code that `process-simulation` is still changing. Deferred to
  `dynes-augmenter-driver-v2`; it remains this change's fallback if the
  clock-step route proves fragile.

**RNG identity.** The forced steps consume no random numbers today (the clock
returns a deterministic wait, and the mark copies the forced row). Applying
them outside the driver therefore leaves the random stream untouched. For a
fixed seed, the drawn times and `log_proposal` must be identical before and
after. This is the regression test.

**One walk per draw when nothing can strand.** Without a user support
constraint (`plan$coupled` is FALSE), and with the prerequisite
`dynes-composition-to-waves` applied, no step can strand. The derived flavor
constraint reads only the flip's own dyad, whose chain order the draw keeps.
No actor that takes part in an interval joins or leaves inside it, so no rate
drops to zero under a pending event. (Without the prerequisite, a leave
inside an interval censors a flip's wait at the leave, the pair redraws at
rate zero, and the attempt strands and restarts, even uncoupled.) So no
interval is ever redrawn, and nothing needs replaying. If an uncoupled draw
strands anyway, the single-walk path raises an internal error instead of
restarting, so a bypassed precondition fails loudly rather than mixing a
silent completion normalizer into `log_proposal`. The error carries the class
`goldfish_dynes_uncoupled_strand`, so tests match it and it is never mistaken
for the coupled path's `goldfish_dynes_restart_ceiling`.

The precondition is enforced in one place: `estimate_dynes()` calls
`snap_composition_to_waves()` before completion, and the augmenters receive the
snapped specification. The augmenters are internal and do not snap. A direct
`augment_seq_*()` call on an unsnapped specification, as the tests make,
bypasses the precondition, and that is the path the error exists for.
`test-composition_waves.R`'s "a sim draw strands on the real composition" is
such a call. It expects the restart ceiling today and the uncoupled-strand
error once this lands. The draw runs all
intervals on one driver run: `n_events` is the total of relational events and
flips, and the attempt state advances `k` when interval `k` has placed
everything. A coupled specification keeps the per-interval attempt (with D3's
cheap prefix), because a restart must rebuild the state at the interval start
and the handle only moves forward.

### D4. One rate vector per fid per step, and column reads in the step path

`sim_flip_options()` calls `sim_fid_values()` inside a `vapply` over
candidate pairs, and each call evaluates the whole rate vector of the pair's fid
to read one sender's entry. `sim_relational_weight()` and `sim_flip_wait()`
evaluate it again. The driver has evaluated the same vectors already
(`rate_values()`), but `clock()` receives only their totals.

A per-step cache on the attempt state, keyed by fid (and by sender for a
choice row), holds the values read at one walk state. **It is invalidated
whenever the walk moves, not when the clock is entered.** Two things move the
handle:

- the driver, between iterations, so the cache is cleared at every
  `sim_clock()` entry;
- the clock itself: the D3 prefix and gap advance, and D7's last-call
  advance to the stamp. Every such move goes through one augmenter move
  wrapper (the D8 helper plus the single advance), and that wrapper clears the
  cache.

The second case is not hypothetical. One clock call can select a relational
event (filling the cache at `S(t)`), find that it is the last call before the
event's stamp, advance to the stamp, and then check support. A cache cleared
only at entry would answer that check from `S(t)`: the evaluation count would
be the same, so no count guard would notice, but the legality answer would be
wrong.

What can lie between `S(t)` and the state strictly before the stamp within
that one call is narrow: **only the node composition at the stamp.** An
exogenous row strictly before the stamp is a breakpoint, so the driver clips
the step there, and the next clock call enters with a cleared cache and reads
nothing before its own advance (a pending relational event needs no read).
An exogenous row at the stamp is not applied, since the advance stops
strictly before it. A selection call is the last call only when
`walk_next_breakpoint(handle) >= stamp`. The composition at the stamp is such
a breakpoint, and the exclusive advance brings it in. So the stale read the
move wrapper's clear prevents is a support or rate that a join or leave at
the stamp changes.

The driver runs `mark_evaluation()` before its next `walk_advance()`
(`R/simulate_driver.R`), so `sim_mark()` reads the same state as the clock's
last move, and may read the cache.

A test pins the invalidation. On a coupled fixture where the receiver of a
relational event leaves at the event's stamp, with no other breakpoint
between the selection and the stamp, the last-call check reads the
post-advance value and strands the attempt. With a cache that is cleared
only at entry, the draw completes instead.

*Alternative considered:* hand the driver's rate vectors to the clock, so the
cache seeds from them and the driver's round is not repeated. Deferred to
`dynes-augmenter-driver-v2`, together with the `prefix` argument of D3, since
both change the shared driver. Until then a sim step pays the driver's round
plus one read per distinct fid.

*Alternative considered:* the last-call check bypasses the cache. Rejected:
it trades one rule ("cleared when the walk moves") for a standing exception.

The same path reads data-frame rows per step: `state$forced[n, ]`,
`unique(heads[, cols])`, `pairs[i, ]`, and the `heads` subset in `sim_mark()`.
These become integer-indexed reads of columns built once in `sim_plan()` /
`new_sim_attempt()` (`.claude/rules/walk-cost.md` rule 4). The chain heads
become an integer vector of event ids over the plan's flip columns.

### D5. The MCMC legality check and reverse rate queries share one walk

A step opens a walk on the current sequence for the forward rate queries. It
then opens one on the proposed sequence for legality, which replays through the
end of interval `k`, and another on the proposed sequence for the reverse rate
queries, whose positions all lie at or before interval `k`'s end. The last two
read the same sequence over the same prefix.

A single candidate pass opens one walk on the proposed sequence and visits its
modeled rows in order. It checks each in-interval stamp under D1, stops at each
query position (`after = k`, D2) to read `pair_intensity()`, and returns
`list(legal, rates)`. At the first illegal stamp it returns early with no
rates, and no log-likelihood is scored, as today. Queries before interval `k`
may already have been read by then; they are discarded, not returned.

**The pass lives in the proposal evaluator.** `make_proposal_evaluator()`'s
closure gains an optional `legality = list(interval = k, waves)` argument.
With it, the evaluator opens one walk with its own `control_prep`, runs the
pass, and returns `list(legal, rates, loglik)`, with `loglik` scored only for
a legal candidate. Without it, calls behave exactly as today. The evaluator is
the only place that holds what the pass needs:

| | Legality walk today | Reverse-query walk (evaluator) |
|---|---|---|
| Opened by | `open_augmenter_walk(context, data)` | `query_rate_at_position(..., control_prep)` |
| `control_prep` | always `set_preprocessing()` | the evaluator's, the one the likelihood uses |
| Has theta | no | yes (`theta_by_fid`) |

So today, with a non-default `control_prep` (a `start_time`), the legality
gate walks a different configuration from the likelihood it gates, which is
D1's "a gate that disagrees with the target biases the chain" again. The
standalone checker of `augment_seq_random()` therefore also opens its walk
with the evaluator's `control_prep`.

*Alternative considered:* give the checker theta and `control_prep` at
`mcmc_attach()` and let it answer the queries. Rejected: it duplicates
`query_rate_at_position()`'s position logic, a second copy D8 exists to
prevent.

The forward-query walk stays: it reads the current sequence, whose handle
cannot be reused because walks only move forward. A step therefore opens two
walks, not three, plus one preprocess for the log-likelihood.

The pass's replay is the D8 helper with `walk_event_supported()` as the check
hook, stopping at each query position as D2's `walk_replay_to()` does. Its row
positions are computed once per candidate, not per query.

**What the pass replays, and what it checks.** The pass replays the handle's
realizations (`walk_realizations()`), so its query positions and its rates
are those of `query_rate_at_position()`. The checks do not come from those
rows. A realization is a write into the focal network, and the merged
schedule gives it no flavor: the flavor rides on the dependent row, which
routes the event to its process. `walk_event_supported()` called on a
realization of a flavored layer would therefore find no process for flavor
`NA` and answer "supported", whatever the constraint. The pass takes each
stamp's checks from the candidate's in-interval ties instead
(`replay_columns()`, the flavor included). A cursor hands them to the hook
the first time the replay reaches their stamp, and an internal abort fires
if a check is left over. On `legal_spec()` a creation placed before the
dissolution it depends on is rejected. Checked against the realizations,
it would be accepted.

### D6. The removed refusal's record and test

- The two `dynes-augmentation/design.md` passages ("One refusal remains",
  around line 826, and the "Residual gap" paragraph, around line 2346) are
  rewritten to say the refusal was lifted by `constraint-objects-on-shared-walk`
  (b1b21033) at the f663b266 merge.
- The test "the walk opens on a flavored, constrained, windowed DyNES
  fixture" asserts the absence of a condition class that `walk_open()` no
  longer raises. It is replaced by a test that builds a DyNES fixture whose
  support constraint reads a network no formula term reads, changes that
  network mid-interval, and asserts that the augmenter's
  `walk_event_supported()` answer flips at the change.

### D7. Tied observed relational events in the sim augmenter are forced, and checked before the stamp

Today, after the first of two tied observed relational events is placed, the
driver's next iteration (`wait = 0`) runs `walk_advance(handle, t)`
inclusively. The stamp's exogenous rows are applied, the first event is
already injected, and `sim_next_step()` then weights the second event against
that state. This breaks the stamp rule. The same path can also hang:
`sim_next_step()` may select a flip at `t = stamp`, and `sim_flip_wait()` then
takes the anchor at the second event's time, so `width = 0`. The truncated
draw returns `wait = 0`, and its `repeat` loop never meets its `wait > 0` exit.
This happens with or without coupling.

At a stamp that holds an observed relational event, nothing else can be
placed: flips are continuous draws strictly inside an interval and never land
on a stamp. Every tied event after the first is therefore forced:

- The first event of a tied stamp is selected as today, weighted against the
  flip candidates.
- Each later event of the stamp (a *sibling*) is a forced step with
  `wait = 0`. It is not selected, adds no term to `log_q`, and no flip
  candidate is considered while a sibling is pending. This also removes the
  `width = 0` path.
- **Support is checked once, on the last call before the stamp, for every
  event of the stamp, the first included.** While the first event is pending,
  the driver may clip its wait at breakpoints and call the clock again. Each
  call sees a later state, and only the last one sees the state strictly
  before the stamp. The clock knows which call that is: the driver does not
  clip once `walk_next_breakpoint(handle) >= stamp`. On that call the clock
  advances through the move wrapper (D4), `walk_advance(handle, stamp,
  inclusive = FALSE)`. That brings in the composition at the stamp, as the
  batch walk does, and applies nothing else, since no breakpoint lies in
  between. The clock then checks, under coupling, the support of every
  relational event at the stamp. One unsupported event strands the attempt.
  Earlier calls only carry the step forward.

  Checking on every call, as first drafted, would strand on a support that a
  later exogenous row before the stamp restores. That would reject sequences
  the batch walk and D1's legality replay score as legal, and the spurious
  strands would bias the completion normalizer in `log_proposal`. Checking
  only the siblings would leave the first event unchecked after its
  selection, so it could be placed without support. The single last-call check
  makes the sim augmenter and the legality gate agree.

Because the last call moves the walk to the stamp before the driver's clock
gets there, the clock's wait must never land short of the stamp: the driver
adds it as `t + (s - t)`, which can round one unit below `s` (and ties to even
can make `s` itself unreachable), so the wait is the smallest that lands at or
just past `s`, and a sibling whose clock already stands there waits 0.

No driver change is needed. The tied siblings come from the plan's relational
columns (built once, D4), so finding them is an integer comparison on the
next entries of the time column.

**An observed event at a wave time is placed last.** Intervals are
`(w_{k-1}, w_k]`, so an observed relational event at `w_k` belongs to interval
`k`. Today it competes with the pending flips by weight. If it is selected
while flips remain, the walk sits at `w_k`, the next flip's anchor is
`min(wave_end, ...) = w_k`, `width = 0`, and the truncated draw's `repeat`
never exits, with or without coupling. Such an event is not weighted: it is
selected only once no flip of the interval is left, with probability 1 and no
`log_q` term. It is the last event of the segment ending at `w_k`
(equivalently, it is placed before anything of the next interval).

**Zero selection weights from composition.** An event's selection weight reads
its sender's rate at the state after the previous event. If the sender joins
inside the interval, that weight is zero before the join. Every ordering that
places another pending event after it then has proposal density zero while the
target's is positive, and the draw strands when nothing else is left. The
prerequisite `dynes-composition-to-waves` moves such joins (and leaves) to the
interval's waves, so v1 never reads such a zero. True join and leave bounds are
`dynes-augmenter-driver-v2`.

**Snapped composition sits on the waves, so it ties with wave-time events.** A
join that the snap moves to `w_{k-1}` is in force at that stamp. An observed
event stamped at `w_{k-1}` belongs to interval `k-1`, so it sees the joiner
one interval early: in its receiver set and in its sender's rate denominator.
`dynes-composition-to-waves` accepts this side effect and warns about it. This
change keeps it. The last-call advance above reads the composition at the
stamp, and so does the batch walk, so the sim augmenter and the likelihood
agree. It is not a stamp-rule violation to correct. Before the snap, a
composition row at an event's stamp was a corner case. Now it is the normal
case at the waves, which is where the wave-time rule's events sit.

*Alternative considered:* an anchored-stamp mode in `simulate_replicate()` that
draws every event of a stamp over the pre-stamp state, as
`R/simulate_anchored.R` does. Rejected for this change: it touches shared
driver code, and the forced-sibling form reaches the same state with the step
contract as it is.

### D8. One stamp helper serves every replay

The legality replay (D1), the rate-query replay (D2), the sim prefix (D3) and
the fused MCMC pass (D5) all apply the same discipline: for each stamp in
order, advance strictly before it, then inject the stamp's realizations. They
share one internal helper with an optional check hook that runs on every
event of a stamp before any of them is injected. The legality replay and the
fused pass pass `walk_event_supported()`; the prefix and the rate-query replay
pass none. Writing the discipline once is the point: the
`walk_replay_exogenous_row()` drift came from a second hand-written copy of a
walk step.

The helper holds the stamp's events in one buffer of stamp size, built once
per replay (as the anchored driver's `pending_*` buffers), and reads the
candidate's rows through integer indices into columns computed once per
candidate.

The realizations the helper injects are every non-dependent focal-layer row
of the candidate, including the rows of a flavor no process models, in
schedule order (D2). It can stop partway through a stamp's realizations,
which the rate-query position `after = k` needs.

In the sim augmenter the helper and the single advances D3 and D7 make from
inside the clock form the augmenter's one move wrapper. It is the only code in
the augmenter that moves the handle, and it invalidates the rate cache (D4).

## Storage and per-event cost

No preprocessed output, buffer format or walk is added. What changes per step:

| Site | Read | Today, per step | After |
|---|---|---|---|
| Sim forced prefix event | none (inject only) | one `rate_values()` over all F rate fids + `mark_evaluation()` | zero evaluations; one driver round per attempt at the opening state (D3) |
| Sim candidate weights | sender entry of the fid's rate vector | one full rate evaluation per candidate pair (P per step) | one per distinct fid per step |
| Sim relational weight / wait | sender (or dyad) entry | one more full evaluation each | cache hit |
| Sim choice row | sender's row via `evaluate_fid(sender =)` | unchanged (`support_row()`, no grid) | unchanged |
| Sim step data frames | row of forced / heads / pairs | `data.frame` row extraction, `unique()` over columns | integer index into prebuilt columns |
| Sim tied sibling (D7) | sender entry (support check under coupling) | full selection: candidate weights + relational weight | no selection; one support read per event of the stamp, on the last call, under coupling |
| Sim event at a wave time (D7) | none | weighted selection | no selection, no weight read |
| Legality replay event | cell or row of the availability buffer, as stored | unchanged | unchanged; inject deferred to stamp end |
| Rate-query replay row (D2) | none (advance/inject only) | one R-level row step per schedule row | one advance per stamp + focal injections |
| MCMC step | — | 3 walk opens + 1 preprocess | 2 walk opens + 1 preprocess |

The cache holds at most one rate vector (length `n1`, or `n1 * n2` for a REM
fid) per rate fid, plus one choice row (length `n2`) per sender read, all for
one step. It replaces evaluations that each allocate the same vectors today.
So the per-step allocation falls, and nothing new is allocated beyond the
values the step reads.

**Cost prediction (stated before implementation).** Let `K` be the number of
intervals, `N_k` the events before interval `k` (observed plus earlier flips),
`n_k` the events in interval `k`, `F` the rate fids and `P` the candidate pairs
per step.

- Sim draw, rate-vector evaluations:
  today about `sum_k (N_k + n_k) * F + sum_k n_k * (P + 2)`;
  after D3 + D4 about `A * F + sum_k n_k * F + sum_k n_k * F'` (where
  `F' <= F` are the distinct fids among the step's candidates and `A` the
  attempts: one per interval attempt when coupled, one per draw when
  uncoupled). The first term is the driver's round at the opening state, and
  the second the driver's round per step, read only for its total. Both go in
  `dynes-augmenter-driver-v2`.
  The quadratic-in-K prefix term disappears. On the 6.2 recovery fixture this
  is predicted to cut sim draw time by at least half for `K >= 3`, with the
  remainder dominated by `walk_open()`.
- Uncoupled sim draw: walk opens per draw fall from `K'` (intervals with
  events) to 1.
- MCMC step on a specification with a user support constraint: walk opens
  fall from 3 to 2. At about 50 ms per open on the 4-actor fixture, this
  saves at least 50 ms per step; the step's share is predicted to fall by
  about a third, net of the unchanged preprocess. A specification with no
  user support constraint has no legality checker, so its step already opens
  2 walks (forward and reverse rate queries) and D5 changes nothing there.
  Task 1.1 measured this: 2.0 opens per step uncoupled, 2.67 coupled (3 on a
  step that reaches the reverse evaluator, 2 on a step the legality check
  rejects).
- Legality replay: no change in walk count. The deferral of injections to the
  stamp end costs one buffer of stamp size, built once per replay (as the
  anchored driver's `pending_*` buffers).
- Preprocessing walk time, preprocessed object size, and estimation fit time
  are unchanged (`tests/testthat/_budgets/walk_cost.csv` and
  `estimation_cost.csv` must not move).

Every walk opened on a candidate passes through `bind_candidate_data()`. Since
`dynes-composition-to-waves`, that function builds the candidate's
composition normal form, O(C log C) for C composition rows, and does not
memoize it. Only the specification's form is memoized. Task 1.1 records after
that change, so the "before" values include this cost. D5's 3 to 2 opens also
save one bind per MCMC step. On the 6.2 recovery fixture, which has no
composition change, this is negligible. On composition data the saving is
slightly larger than the walk-open figure above. An O(1) pointer shortcut in
the bind is out of scope here.

**Measured "before" values (task 1.1, at `b0c14308`).** Recorded on a
12-actor, three-interval state-panel fixture (friendship panel plus calls),
with the coupled variant carrying `support_constraint = ~tie(friendship)` on
calls. The values are in `progress.md` ("Cost record") and in
`tests/testthat/_references/walk_alignment_v1/`. Three of them shape how the
groups are judged:

- The uncoupled sim draw opens 3 walks (one per interval with events) and
  never restarts. The coupled one restarts on every seed tried: 8, 120, 422,
  121 and 498 walk opens, 1.3 to 51 s, 41 MB to 1.9 GB per draw. The restart
  tail dominates its mean, so the sim comparison for the coupled fixture is
  per seed (draws are seeded and deterministic) and on the median, and counts
  per walk open are reported beside counts per draw. D3's single walk does
  not change the coupled path, so a coupled restart count that is unchanged
  is expected, not a regression.
- `evaluate_fid()` per walk open is about 370 uncoupled and 330 to 690
  coupled before D3/D4. That is the figure the "one evaluation per fid per
  step" gain moves, and it is less sensitive to the restart tail than the
  per-draw total.
- The MCMC 3 to 2 gain applies to the constrained (coupled) fixture only,
  as stated above.

The guards are **counts** (walk opens, `evaluate_fid()` calls), not timings.
Counts are deterministic and do not need a large fixture to separate from
noise. The closing measurement reports seconds and allocations on the 6.2
recovery fixture against the pre-change values. "The 6.2 recovery fixture" is
the state-panel toy shape of the recovery study (12 actors, a modeled
flavored friendship panel and a calls event stream), with three intervals
`c(0, 5, 10, 15)`. It is generated by
`tests/testthat/_references/walk_alignment_v1/generate_walk_alignment_references.R`.
Its coupled variant needs a support constraint: the study's own "coupled"
cells read friendship in the rate and choice but carry none, so they never
run a legality walk.

## Risks / Trade-offs

- [Doing the prefix in the clock step stretches the step contract: a clock
  that advances and injects] → The augmenter is already documented as an
  external driver of the handle, and the clock already receives `handle`. The
  internal `prefix` argument on `simulate_replicate()` is the fallback (D3).
- [D1 changes accept/reject decisions at ties, so a seeded test pinned on
  today's behavior can move] → Only fixtures with a stamp tie can move. Every
  existing seeded augmenter test is run before and after, and any that moves
  is inspected for a tie before its expectation is updated.
- [The single-walk sim draw (D3) changes the order of random draws across
  intervals compared with per-interval runs] → None today, because the forced
  steps draw nothing. The regression test asserts seeded identity on an
  uncoupled fixture to prove it.
- [D7 changes `log_q` and the step sequence at tied stamps, so a seeded sim
  test over tied observed events can move] → As for D1: run every seeded
  `augment_seq_sim()` test before and after, and inspect any that moves for a
  tie before updating its expectation. 1.1's reference fixtures have no ties
  and must not move.
- [A later change reorders the merged schedule at a stamp, which D2 relies
  on] → The ordering is pinned by a test at the schedule builder.
- [The single-walk sim draw and the absence of zero selection weights rely on
  `dynes-composition-to-waves`] → It lands first. An uncoupled strand raises an
  internal error, never a silent restart. Task 1.1's fixtures carry no
  in-interval composition change, so its references stay comparable when v2
  replaces the snapping with true bounds.
- [The clock moves the handle ahead of the driver's `t` (D3 gap advance, D7
  last-call advance)] → Both only move forward, and the driver's next advance
  is never earlier than the handle's time. The rate cache is invalidated by the
  one move wrapper (D4).
- [The wave-time rule changes the selection and the random stream on fixtures
  with an observed event at a wave time] → Task 1.1's references avoid such
  events. Any seeded expectation that moves is checked for one before it is
  updated, as for stamp ties.
- [Data with an actor absent at its own observed event at `w_k`] → The snap
  keeps the real presence at a closing wave unless the next interval involves
  the actor. So such data stays as it is, and the snap does not repair it. An
  uncoupled draw places the event unweighted (the wave-time rule) and does
  not strand. Under coupling, the last-call check reads presence
  (`walk_event_supported()` reads the risk set's presence), fails on every
  attempt, and ends at `goldfish_dynes_restart_ceiling`. That abort blames
  coupling for what is a data error. The message fix belongs to
  `dynes-augmenter-driver-v2`, as its handling of residual composition stalls.
  This change adds no check.
- [The fused MCMC pass couples the legality checker and the rate-query code
  that the `abmcem`/`rate-queries` work owns] → The pass composes the existing
  `walk_event_supported()` and `pair_intensity()` without changing either.
  `make_proposal_evaluator()`'s contract (log-likelihood with optional rate
  queries) is kept.

## Migration Plan

Internal only, with no API surface. Each decision lands as its own red/green
pairs. Rollback is per commit.

## Open Questions

None open. Resolved (2026-10-08):

- **Batch ordering at a shared stamp (gated D2):** an exogenous row cannot
  precede a dependent row stamped at the same time. The merged schedule sorts
  by `order(time, !dependent, stream_index)`, so every dependent row of a
  stamp comes first. D2 is confirmed, in its stamp-rule form (no tied event
  is injected before another is read).
- **In-interval relational ties under coupling (D1's tail):** resolved by D7
  without a driver change. A tied sibling is forced, and every event of the
  stamp is checked once, on the last clock call before the stamp.

Resolved (2026-10-08, design review):

- **Prefix evaluations (D3):** the clock-step route is kept, with at most one
  rate round per attempt before the interval opens. Zero goes to
  `dynes-augmenter-driver-v2`.
- **Support-check timing (D7):** one last-call check for all events of a tied
  stamp, at the state strictly before it, composition at the stamp included.
- **Composition inside an interval:** joins move to `w_{k-1}` and leaves to
  `w_k` for actors involved in the interval (as sender or receiver of an
  observed relational event or a flip), with a warning, in the prerequisite
  change `dynes-composition-to-waves`. True bounds go to v2.
- **Events at a wave time (D7):** placed last in their interval with
  probability 1.
- **Rate cache (D4):** invalidated by the move wrapper.
- **Rate-query contract (D2):** `after = k` is kept. Task 3.2a's premise (a
  live replay-skip drift) does not hold; the drift is latent.
- **Fused pass (D5):** in the evaluator, with the evaluator's `control_prep`
  used by every legality walk.
- **Driver rate vectors to the clock:** deferred to v2 with the `prefix`
  argument.
- **Order:** `dynes-composition-to-waves`, then this change, then the
  `dynes-augmentation` 6.2 sweep.
