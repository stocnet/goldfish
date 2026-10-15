## Context

`build_joint_schedule()` (`R/preprocess_joint.R`) builds the merged walk's
schedule in three loops. They share two "already added" sets, `stream_seen`
(stream keys) and `targets_seen` (registry ids):

```
 1. units      each unit's planned streams        writes stream_seen, targets_seen
 2. focal      a focal layer no term planned       writes NEITHER
 3. constraint streams of objects only a           skips what stream_seen /
               constraint reads (b1b21033)         targets_seen hold
```

When a support constraint reads its own focal layer and no term reads it,
loop 1 skips the layer, loop 2 adds it, and loop 3 finds it in neither set
and adds it again. Each observed event of the layer becomes one dependent
row and **two** state-update rows on the same object. Observed on the
`stamp_tie_spec()` fixture with `~ outdeg(calls, type = "ego") < 1`:
streams 4 and 5 both target object 2 (`calls`).

Both loops use the same stream key for the layer. Traced on 2026-10-14,
loop 2's `focal_stream_fetch_plan()` key and loop 3's constraint
`events_objects_link` key are both `calls` on the stocnet path, and both
`call_network` on a legacy envir fixture (`make_network()` /
`link_events()`), where the plain fit aborts with "empty risk set at
event 3". Loop 2 only fails to record the key.

Every reader of the schedule inherits the duplicate:

| Reader | Effect today |
|---|---|
| batch walk (`run_merged_walk()`, every estimator through `preprocess_one_unit()` / `preprocess_joint()`) | increments applied twice; a weighted read sees 2 |
| stepping handle (`walk_open()`, `walk_advance()`) | same schedule, same doubling for exogenous rows |
| rate-query replay and the DyNES candidate pass (`walk_realizations()`) | two realizations per event: injected twice, positions doubled |
| `augment_seq_random()` legality checker (`replay_columns()` from the ties) | applies once, so it disagrees with the likelihood |

The atom stores that maintain a constraint are advanced per **object**
(`advance_recorders_for_event()`, keyed by `props$key[oid]`), not per
stream, so loop 2's stream alone keeps the constraint maintained once loop
3's copy is gone.

Measured (scratch scripts, not kept):

- **Plain fit.** `estimate_dynam()` with `choice = ~ tie(friendship)` and
  `support_constraint = ~ outdeg(calls, type = "ego", weighted = TRUE) < 2`,
  where N2 calls at t = 2 and again at t = 3, aborts with "empty risk set
  at event 2". With `+ inertia` (a term reads `calls`, so there is no
  duplicate) the fit completes. With `< 3` it completes too.
- **DyNES.** On the same constraint, the batch log-likelihood of a
  candidate is `-Inf`, the fused candidate pass says illegal, and the
  `augment_seq_random()` checker says legal.
- **Frozen captures.** No frozen constraint-atom capture
  (`helper-constraint-atoms.R`, every family) carries a duplicate. No
  frozen coefficient baseline uses a support constraint.

## Goals / Non-Goals

**Goals:**

- Each source event stream is on the joint schedule once, so each observed
  event is applied once by every reader.
- Specifications without the pattern keep a byte-identical schedule.
- The builder aborts on any future loop that re-adds a stream key.

**Non-Goals:**

- Reworking the three-loop builder into one pass, or changing where
  constraint-only objects sit in the registry (`constraint-objects-on-shared-walk`
  owns that).
- Any change to the augmenters, the rate queries or the walk handle; they
  are corrected by reading the corrected schedule.
- The observation-window extent rule for constraint-only rows (left open by
  `constraint-objects-on-shared-walk`).

## Decisions

### D1. Every loop skips by stream key, and `add_part()` records the key

`add_part()` takes the stream key of each state-update stream and appends it
to `stream_seen` itself, so no loop can add a stream without recording it
(the guard of D2 sits in the same place). Loop 2 then skips a focal stream
whose key is already in `stream_seen`, and loop 3's `fresh` test becomes
`!(eol$events[rows] %in% stream_seen)`. `targets_seen` stops being a skip
criterion. Skipping by object assumed that an object on the schedule always
has all its streams there. That holds for focal layers, because
`focal_stream_fetch_plan()` mirrors the planner entry for entry, but nothing
states it, and if it ever failed, a per-object skip would drop a legitimate
stream without any error. The key skip and the per-object count check of
1.2 together cover the same ground without the assumption. On every
specification without the pattern, the two rules skip the same streams,
because on the stocnet path a layer has one stream, keyed by its name (D2).

Loop 3 also keeps `!duplicated(keys)` beside the key test. It is defensive:
a constraint link has one row per object, so no case reaches it, and without
it a repeated key would hit D2's guard rather than duplicate silently.

The duplicate was always loop 3's, so dropping it changes no row
or stream index from loops 1 and 2. Loop 3 can add other streams after the
duplicate: a second constraint, or a constraint-only object that comes
after the focal layer in the same constraint's link. Their stream indices
fall by one. The remaining rows keep their relative order, because the
ordering compares stream indices, never their values.

The same fix covers a constraint of one process that reads another
process's focal layer when no term reads it. Loop 2 adds that layer because
it is the other unit's focal layer, and loop 3 used to add it again.

*Alternatives considered:*

- *Exclude focal layers in loop 3 by name.* This works today, but it leaves
  loop 2's bookkeeping wrong for the next loop someone adds. Fixing the
  bookkeeping at the source is the general fix.
- *Have loop 2 append to `stream_seen` and `targets_seen` itself.* This
  fixes this bug, but every future loop would still have to remember to
  append. Recording inside `add_part()`, which every loop already calls,
  removes that duty.
- *Keep the per-object skip (`targets_seen`) and state its invariant.*
  Rejected: it keeps an unstated "all of an object's streams or none"
  assumption on the skip path, where a violation drops a stream silently.
- *Deduplicate the finished schedule by row content.* Rejected: two
  distinct linked streams may legitimately carry equal rows, and
  deduplicating would hide the next builder bug instead of failing on it.

### D2. The guard is per source stream key, checked by the builder

The invariant is "no source stream key twice among the state-update
streams". The key identifies the stream: on the stocnet path, which every
estimator now runs on (`make_data()` converts legacy objects to it), a
network layer's key is the layer's own name (`ds_object_streams()`), and an
attribute's key is view-qualified. Two objects therefore never share a key.
Verified 2026-10-15: two networks linked to the same events frame get keys
`net_a` and `net_b`, and both are applied in full. It is not "no two
streams with identical rows", for the reason D1 rejects deduplicating by
content.

Today a network layer has at most one stream, so "one occurrence per key"
and "one stream per object" accept the same schedules. The key rule is kept
because it costs nothing more and stays correct if an object ever carries
several streams again. No fixture can build such an object from current
data (see Risks), so the schedule-level tests do not try to.

The builder enforces it inside `add_part()` (D1). When a state-update
stream is added, a small helper checks its key against `stream_seen` and
aborts on a repeat with `cli::cli_abort(.internal = TRUE, class =
"goldfish_duplicate_schedule_stream")`, naming the key and the object.
Dependent rows are never recorded, so they are exempt: a process's dependent
stream shares its key with the focal layer's state-update stream, and in a
joint specification with another process's planned stream of that layer.
The check runs once per added stream, before the walk, and costs O(number
of streams) per schedule.

The fixed builder can never trigger the helper, so the helper has a unit
test of its own: a repeated key aborts, and distinct keys pass, including
two distinct keys on one object. What the builder produces is checked from
outside: for each object, the number of distinct non-dependent
`stream_index` values on the schedule equals the object's number of source
streams. That check is the RED test (1.2), and it needs no access to the
keys.

*Alternative considered:* a test-only check that no two streams of one
object carry identical rows. Rejected: it contradicts D1's reason for
rejecting content deduplication, and it would only run on test fixtures.

### D3. The pair is red/green on the plain estimator, with controls

The failing test is the plain `estimate_dynam()` fit above: a 4-actor
fixture whose data satisfies the weighted constraint. It asserts two
things: the fit completes, and the schedule carries exactly one
state-update row per `calls` event. A comparison against the `+ inertia`
fit is not used, because the formulas differ. Three controls keep the fix
honest:

- an unweighted `outdeg(calls, type = "ego") < 1` on the same data still
  aborts with the empty-risk-set error, so the constraint still bites once
  its own stream is gone;
- an exact threshold: the same weighted `< 2` constraint on data where one
  sender calls three times aborts at that sender's third call (weighted
  outdegree 2), not at the second. This pins the weight to the observed
  count, which a fit that merely completes does not. The empty-risk-set
  error carries no condition class or event field, so the test matches the
  event number in its message. Adding structure to that user-facing
  condition is out of scope;
- the existing `test-augment_legality.R` cases on `outdeg(calls) < 1` stay
  as they are (the illegal "event after another's stamp" included).

### D4. The DyNES readers are checked for agreement, not changed

A test on the `stamp_tie_spec()` fixture with the weighted constraint
asserts that the batch log-likelihood is finite, the fused candidate pass
(`make_proposal_evaluator(legality =)`) and the `augment_seq_random()`
checker both say legal, and `walk_realizations()` has one entry per focal
timed tie of the candidate.

`walk_realizations()` takes every non-dependent row of a focal layer
(`R/rate_queries.R`), so a realization is a state-update row, while
`mcmc_ordinal()` counts events. The two agree because each focal layer's
stream is on the schedule once and a layer has one stream. Before the fix
the duplicate gave every event two realizations, so `after = 2` addressed
the copy of the first event instead of the second. No reader changes, and
the spec states which rows the realizations are.

### D5. The fix lands on `feature/dynes` and travels upstream by cherry-pick

The fix lands here as a standalone RED/GREEN pair, as the labeled-node fix
did, and is offered to `feature_simulation` afterwards. Upstream
(`283c7252`) already carries `2822625b` and `b1b21033`, and its
`build_joint_schedule()` loops are byte-identical to this branch's, so the
fix applies without conflict. That holds only if the commits that travel
read nothing DyNES-only. The guard (1.2), the red test (2.1a) and the fix
(2.1b) therefore live in test files present on both branches and use only
shared fixtures (`helper-constraint-atoms.R`, small inline data). The DyNES
agreement test (2.2a), including the `stamp_tie_spec()` case, stays on this
branch.

*Alternative considered:* land on `feature_simulation` first and receive it
by merge. Rejected: the DyNES readers that expose the defect live here, and
the remaining `dynes-augmentation` 6.2 tasks wait on the fix.

## Storage and per-event cost

No preprocessed output, buffer, walk or read is added. The change removes
work:

| Site | Today, per focal event (affected spec) | After |
|---|---|---|
| Schedule rows | 1 dependent + 2 state-update | 1 dependent + 1 state-update |
| Shared-state writes | 2 (`write_entries()`) | 1 |
| Atom-store advances | 2 | 1 |
| Rate-query realizations | 2 | 1 |

On an unaffected specification, the schedule, the walk and the
preprocessed output are byte-identical. The design checks this once, not
with a permanent capture. 1.1 saves the schedule of the `+ inertia`
variant (a term reads `calls`), and 2.4 asserts it is `identical()` after
the fix. The saved files go in the local-only `.plan/focal-stream-dedup/`,
next to their generator script, so the FD1 and FD3 sessions can both use
them. The stream-key assertion runs once per added stream while the
schedule is built, not per event, so it adds nothing to the walk cost.

**Cost prediction (stated before implementation).** On an affected
specification the walk steps one row fewer per focal event. Walk time and
allocation fall by roughly that row's share: about a third of the schedule
rows of a constraint-dominated walk. The preprocessed object shrinks only
if the duplicate rows wrote right-censoring rows (see Risks). On every
unaffected specification nothing moves: `tests/testthat/_budgets/walk_cost.csv`
and `estimation_cost.csv` stay as they are, since the budget fixture's
constrained walk (`~ !tie(msg)` with terms on `msg`) has no duplicate. The
measurement is on a synthetic affected fixture large enough to separate a
per-event row from noise (200 actors, 5000 calls, `choice = ~
tie(friendship)`, a weighted focal constraint loose enough to hold on the
data), before and after.

## Risks / Trade-offs

- [The duplicate rows, being constraint-only rows, may have written
  right-censoring rows at the dependent event's own stamp for timed rate
  fids whose mask reads the layer] → If so, they were zero-width and carried
  no exposure, so removing them changes the preprocessed output of an
  affected timed-rate specification but not its likelihood. A task compares
  the preprocessed output before and after on an affected timed-rate
  fixture. Any difference beyond zero-width censoring rows at a dependent
  stamp stops the work for a design review.
- [An affected user fit changes its numbers] → This is the intended fix: a
  weighted read now sees the observed value. Tie-presence and tie-count
  reads are unchanged, since doubling a tie changes neither. The NEWS
  fragment names the affected pattern.
- [Shared code that `constraint-objects-on-shared-walk` and
  `process-simulation` also edit] → The diff is a few lines inside loop 2,
  plus the stream-key assertion. The assertion makes any later builder loop
  that re-adds a stream fail loudly, upstream included.
  The fix is offered upstream (`feature_simulation`) as well, so the trunk
  does not carry it twice.
- [No fixture can build an object with two streams] → `make_data()` keeps
  only the first events frame of a network linked twice with
  `link_events()`: observed 2026-10-15, `net_a` linked to `ev1` (t = 1, 3)
  and `ev2` (t = 2, 4) reaches `ties` with t = 1, 3 only. This defect is
  older than this change and out of its scope; it is filed in
  `.scratch/make-data-multi-link-loss/`. Here it means a positive control
  on "two streams on one object" would run on one stream and pass
  trivially, so that case is tested on the guard helper only (D2).

## Migration Plan

Internal; no API. One red/green pair plus the guard. Rollback is per commit.

## Open Questions

None. Where the fix lands was settled on 2026-10-14 (D5). The guard's form
(D2), the exact-threshold control (D3) and the re-index note (D1) were
settled in a grilling session the same day. On 2026-10-15 an explore
session moved the bookkeeping and the guard into `add_part()`, made every
skip key-only (D1), grounded the key identity in the layer-name key (D2),
and dropped the two-streams positive controls (Risks).
