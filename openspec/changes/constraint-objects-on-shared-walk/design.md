## Context

Measured on `feature_simulation` at 1.9.31 after `process-simulation` task
2.0 (commits `9a56835`, `8641656`, `a21b20c`), with the elements the session
that landed it handed over.

**The gap, precisely.** `build_merged_blocks()` calls
`build_shared_objects(units)` before `build_joint_support_constraints()`;
the registry is the union of `unit$spec_map$plan$objects`, which
`plan_with_constraint_atoms()` never extends (it appends effect rows tagged
`role = "constraint"`, the atoms' `objects`/`routing` staying on the compiled
sub-plan). `build_joint_schedule()` fetches each unit's `fetch_plan` and one
stream per `events_objects_link` row; a constraint's own `fetch_plan` is never
fetched. Coverage is one predicate, `recorder_covers_constraint(sub_plan,
shared_object_keys)`: every dynamic atom object key (an object named in the
sub-plan's `events_objects_link`) must be in `merged$objects$key`. Static
atoms (`allowedNet`) are seed-only and always covered; a derived flavor mask
reads the focal layer, which is always in the registry, so the "flavored
derived mask" listed as uncovered in the predecessor's progress notes is
stale — the flavored mutually exclusive fixture replays exactly on the
handle.

Batch: `run_merged_walk()` builds recorders for covered constraints only;
`realize_pending_masks()` → `recorder_atoms_factory()` falls back to
`build_atom_maintainer()` for an uncovered one — its own `fetch_events()`,
state container, schedule and cursor, advanced after the walk. Handle:
`walk_open()` calls `walk_assert_covered_constraints()` just before
`build_walk_recorders()` and aborts (`goldfish_walk_unsupported`, snapshot in
`_snaps/walk_handle.md`). The reproducer is `walk_constrained_data()` with
`choice ~ inertia` (does not read `emails`) and `support_constraint = ~
!tie(emails)`; `choice ~ inertia + tie(emails)` on the same data is covered
and replays exactly. Flipping one formula term flips the case.

**Mask timing.** `preprocess_support_mask()`'s contract: `advance(t)` applies
atom events strictly before `t` (a lagged risk set); `walk_mask_streams()`
and `constraint_replay_atoms()` keep the same `< t` test. The handle has no
comparison: `walk_apply_object_event()` advances the recorders and folds the
deltas at once, and `walk_advance(t)` applies every row with time `<= t`. The
two differ only when an atom-object event shares a dependent event's stamp
and precedes it in schedule order. Every replay fixture uses distinct
stamps; a free-running simulation never produces the tie;
`times = "observed"` can.

**Extent.** `resolve_walk_extent()` reads real events as every row except
`merged$window_derived`. Adding constraint streams would move the resolved
end of a constrained exact-time fit with a trailing constraint event unless
those rows are exempted. `observation-tail-right-censoring` D1 phrases the
invariant as real versus window-derived and its D2 (ADR-0069) says a real
exogenous tail extends the window; whether a constraint-only row is "real"
is a cell of that change's matrix, unanswered.

**Testing substrate.** `test-classical_equivalence.R` is the pattern: a
hand-walked design matrix, `survival` numbers frozen under
`_references/classical_v1/` with a measured-delta table (worst 2.9e-08),
`stats::glm` live, gate 1e-6, and a numeric demonstration that
`choice_coordination` has no conditional-logit twin.
`compute_statistics(output = "data.frame")` exports rows already filtered by
presence and mask with `event, chosen, sender, receiver, index_i, index_j,
timespan, is_dependent` — `index_j` is the real receiver, `is_dependent =
FALSE` marks a censored row. `helper-flavored-fixtures.R` has the
state-consistent alternating creation/dissolution stream and the
"container equals standalone" test at 1e-6. `helper-constraint-atoms.R`
captures byte-identity references (mask stream plus folded
`active_sender`/`active_dyad`) for four families, none of them the uncovered
case. Composition change is a `present`/`active` node stream compiled by
`C_convert_composition_change()`; absent nodes produce no exported rows;
`outdeg` still counts ties to absent receivers, which the rate row-reduction
does not. A single-flavor spec keeps every focal row in the network stream
and models only the keyed flavor's rows, so the other flavor's events are
right-censoring boundaries on a timed rate.

Constraints: the frozen 1e-6 baselines and C++ goldens do not move; the
branch writes a `NEWS.d/` fragment (ADR-0040); ADR-0068 keeps the DAG check
in the parser; ADR-0054 governs retirements; ADR-0044's first-argument
asymmetry is untouched.

## Goals / Non-Goals

**Goals:**
- Every support constraint is maintained by the one shared walk, batch and
  handle alike; the private atom walk is gone.
- One stated mask timing rule that both paths satisfy, with a fixture that
  reaches the tie.
- A seeded simulation harness whose oracle frames use the process's own
  risk-set rule, and five fixtures proving goldfish's estimator equals
  `glm`/`mlogit`/`clogit` on flavors, constraints, composition change and a
  single modeled flavor.
- The handle replays the batch on every constrained fixture.

**Non-Goals:**
- Deciding the walk extent for constraint-only rows (owned by
  `observation-tail-right-censoring`).
- A `simulate()` generic (`process-simulation` 2.1); the harness is plain R
  in the test helpers.
- Oracles for `choice_coordination` or DyNAM-i; parametric waiting-time
  distributions.
- Parameter recovery against the true θ as an assertion (finite samples);
  the assertions are goldfish-versus-oracle on one frame, and frame
  identity.
- Migrating `.plan/datasets/Simulation.R`; it is a design source.

## Decisions

### D1 — Constraint-only objects join the registry after the unit objects, and their streams the schedule after the unit streams

`build_merged_blocks()` compiles the support constraints first, then
`build_shared_objects()` appends each constraint sub-plan's `objects` to the
registry *after* every unit's objects, so existing oids never move and an
unconstrained or covered model builds a byte-identical registry. Each unit's
`shared_to_local` is `NA` for such an object, so no engine folds statistics
from it. `build_shared_state()` builds the container over the extended
registry; constraint atoms are realized on `shared_src` (windowed atoms
already register their derivations there). `build_joint_schedule()` appends
the constraint sub-plans' streams after all unit streams, deduped by stream
key, so `stream_index` ordering and every existing tie are preserved; in
`run_merged_walk()`'s covariate branch an engine with a `NA` local id skips
the row, emitting no right-censoring row and not advancing `i_total`. The
coverage predicate then holds for every constraint by construction.
*Rejected:* registering constraint objects in `plan$objects` at parse time
— moves oids inside the unit and breaks the baselines the predecessor's D3
guards; a second registry for constraint objects — a second registry is the
private walk under another name.

*Revised 2026-09-29 (Alvaro, ADR-0103):* a constraint-only object's event
closes the open interval of every exact-time rate whose mask reads it,
with a right-censored row, as every non-dependent event does. It still
folds no statistic: the engine's local id stays `NA`, so no column moves;
the boundary is emitted because the event changes that rate's risk set.
Measured before the decision (8 actors, 120 events, 21 `emails` events
gating three senders under `~ !tie(emails)`): today's private walk stores
687 rate rows with no boundary, the process defines 834, and the estimates
miss the process's likelihood by 6.5e-3; with a rate term reading the same
layer (`outdeg(emails)`) goldfish already splits the intervals exactly as
the process does. Sequencing keeps the detector: task 2.2 threads with no
boundary, byte-identical to the task 0.2 references, and task 2.3a then
adds the boundary and re-captures the one reference it moves
(`uncovered_rate`), asserting the difference is exactly the new censored
rows. (*Corrected the same day:* this first named the `rate` family too,
but its constraint reads its focal layer, whose events are dependent events
and already close the interval, so it does not move.) The presence half of
ADR-0103 is `process-simulation` task 2.11c, applied at the trunk merge
because it moves the fisheries rate and REM baselines.

### D2 — The private atom walk is retired, detector-first

Following the predecessor's D4 (the private walk goes last): first capture
byte-identity references for the uncovered case — a fifth family in
`helper-constraint-atoms.R` on the `walk_constrained_data()` shape, mask
stream plus folded availability — with the fallback still authoritative;
then thread (D1) and assert every reference, old and new, matches; then
delete `build_atom_maintainer()`'s state container, schedule and event loop
(its seed/template part survives only if `build_constraint_atom_store()`
still calls it), `recorder_covers_constraint()`,
`constraint_atom_object_keys()` where unused, the fallback branch of
`recorder_atoms_factory()`, and the default `atoms_factory =
build_atom_maintainer` argument of `preprocess_pooled_support_masks()`.
`mask_call_counts()` keeps counting one atom pool per layer.
`preprocess_support_mask()` (single-constraint wrapper) has no production
caller found; it is an ADR-0054 candidate checked against the tests before
deletion, in the retirement task. On the handle,
`walk_assert_covered_constraints()` and its snapshot go, replaced by a
replay test on the uncovered fixture with `n_excluded > 0`.
*Rejected:* keeping the fallback "just in case" — two implementations of
one idea is the defect the predecessor named.

### D3 — A support mask reads its atoms strictly before the event's stamp, on every path

The batch contract stands: `advance(t)` applies atom events strictly before
`t`, a lagged risk set, documented in `preprocess_support_mask()` and
encoded in every byte-identity reference and the classical parity route.
The handle conforms through its driver: a driver evaluating an observed
event at its stamp steps strictly before it (`walk_advance(t, inclusive =
FALSE)`, the revision below), so `walk_evaluate()` at `t` sees the mask over
atoms strictly before `t` exactly as the batch snapshot does. (First
planned as a pending set of atom moves inside the handle, folded into the
live mask once the clock passed the stamp; built and withdrawn, see the
revision.) A new fixture puts an
`emails` event at a call's own stamp, ordered before it, and asserts
batch-vs-replay equality there — the tie the current fixtures avoid.
*Rejected:* aligning the mask with statistics (in force when scheduled
before the event) — one clock rule for all three state kinds is cleaner,
but it changes the documented contract, moves every constrained fit with a
tied stamp, and re-mints the byte-identity references; recorded as the
alternative to revisit in ADR-0073's open questions, not taken here.

*Revised 2026-09-29 (Alvaro, ADR-0105):* the rule stands, the mechanism
does not. Deferring the atom moves inside the handle was built and fixed the
tie for an anchored run and a replay, but it broke free-running simulation
(seven tests): after an event injected at `t`, or an exogenous breakpoint at
`t`, the free-running driver evaluates for the next event, strictly after
`t`, where the moves at `t` must count, and the handle's clock reads `t` in
both cases. Deferring only the touched set would not do either, since the
live mask recomputes from the atom buffer. Instead the handle keeps one clock
rule (the state at its clock includes every row stamped up to it), and a
driver that evaluates an observed event at its stamp steps strictly before
the stamp: `walk_advance(t, inclusive = FALSE)` applies the rows stamped
before `t` and the composition up to and including `t`, and the rows sharing
the stamp are applied after the event is injected, which is the order the
batch schedule gives them. The time-anchored driver steps this way; the
free-running driver is unchanged.

### D4 — Constraint-only rows are exempt from the walk extent until the censoring change decides

`resolve_walk_extent()` treats constraint-only stream rows like
`window_derived` rows, so threading them changes no resolved end and every
current number holds. This is an interim rule, not a decision: whether such
a row is a real exogenous event (ADR-0069 read literally) is the cell
`observation-tail-right-censoring`'s matrix must carry, and its D1/D2 own
the answer. The exemption is implemented as one predicate with a comment
naming the open cell, so flipping it is one line and one test.

### D5 — One seeded harness, hand-built frames, the process's own risk-set rule

`tests/testthat/helper-simulate-dgp.R` provides
`simulate_relational_sequence()` returning the nodes, the event stream (with
`flavor`, `time`, and any composition or exogenous events) and two oracle
frames built inside the loop before each event is applied: a rate frame
with one row per actor at risk per interval (`outcome`, `dt`, statistics)
and a choice frame with one row per allowed receiver per dependent event
(`chosen`, `event`, `receiver`, statistics). Waiting times are `rexp()`
draws from the total rate over actors at risk; the sender is drawn
proportional to its rate; the receiver from the multinomial over the allowed
set. The frames apply the same rule the process used: under a dyadic
constraint a sender with no allowed present receiver contributes no rate
row (the row-reduction), an absent node contributes no row on either axis,
and every non-dependent event — an exogenous constraint-object event, a
composition change, an unmodeled-flavor event — closes the open interval
with a censored rate row (`outcome = 0`, its `dt`) and opens a new one.
(*2026-09-29, ADR-0103:* this is the rule goldfish adopts, not only the
oracle's. For a constraint-only object it lands here, task 2.3a; for a
composition change in `process-simulation` task 2.11c, at the trunk merge,
so F4's rate arm moves there.)
Effects are limited to those whose update rule the DGP mirrors exactly on a
0/1 state matrix: rate `1 + indeg + outdeg` (present-tie counts), choice
`recip + trans` (`trans` as the two-path count `(S %*% S)[i, j]`, never
receiver in-degree; `inertia` is degenerate under a mutually exclusive
creation mask). Seeds through `withr::local_seed()`; no `rbind` growth —
frames are preallocated at `n_events × n_actors` and trimmed.
*Rejected:* sourcing `.plan/datasets/Simulation.R` — not runnable, no seed,
mislabeled `trans`, swapped rate updates; simulating through the walk
handle — the oracle must not share goldfish's state code.

### D6 — Five fixtures, three arms each, one tolerance

| fixture | rate arms | choice arms | extra arm |
|---|---|---|---|
| F1 baseline (DyNAM, also REM rate) | `estimate_dynam(rate)` ↔ `glm(poisson, offset(log dt))`; `estimate_rem` ↔ dyadic `glm` | `estimate_dynam(choice)` ↔ `mlogit`/`clogit` | — |
| F2 creation/dissolution flavors, derived masks | per flavor, gated senders dropped | per flavor over the allowed set | container = standalone single-flavor specs = de-flavored layers |
| F3 constraint on an object no formula reads | `~ !tie(emails)`, `choice ~ recip` | same | handle replay, `n_excluded > 0` |
| F4 composition change on F1 | absent nodes: no rows; presence flips: censored rows (rate arm moved to `process-simulation` 2.11c, 2026-09-29) | absent receivers excluded | handle replay with `walk_advance_presence()` |
| F5 one flavor modeled on F2's data | dissolution events: censored rows | creation only | equals F2's creation blocks |

Arms: (a) goldfish fit versus the oracle on the hand-built frame, gate 1e-6
(independently optimized, the classical discipline; the log-likelihood
offset `sum(log dt)` asserted as in the classical test); (b) frame identity
— `compute_statistics(output = "data.frame")` rows equal the hand-built
rows on `(event, index_i, index_j)` and on every statistic column, which
proves the risk-set filtering and the statistics before any optimizer runs;
(c) the fixture's extra arm. `survival` and `mlogit` arms
`skip_if_not_installed()`; `glm` arms never skip. The "de-flavored" arm
builds a data object where each flavor is its own layer and writes the
mask by hand (`~ !tie(creation_layer)`), so the flavored machinery is
checked against the plain-layer machinery too.
*Rejected:* asserting recovery of the true θ — a finite-sample statement
that fails honestly; freezing the oracle numbers — `glm` ships with R and
`survival`/`mlogit` are live behind a skip, as the classical README argues.

### D7 — Coordination is excluded by evidence, not by omission

`choice_coordination` has no conditional-logit twin (the two log-sum-exp
corrections are nonlinear in θ and indexed by the dyad's own endpoints;
`_references/classical_v1/README.md` shows 0.2849 versus 0.0575). The
capability spec records the exclusion and the reason, and the coordination
fixtures keep their existing byte-identity references as their gate.

### D8 — The handle replay is the fourth arm on every constrained fixture

`walk_replay_against_batch()` (drives the schedule; `walk_advance()` at
exogenous rows, `walk_inject()` at focal rows, `walk_advance_presence()`
before evaluating; compares `walk_evaluate()` to
`materialize_process_state()` + `evaluate_process_state()`) runs on F2, F3,
F4 and F5 with `expect_gt(n_excluded, 0)`, so the substrate half and the
testing half of this change meet on the same fixtures.

### D9 — The batch walk composes masks live, as the handle does (added 2026-09-27)

Today the batch walk records the constraint atoms as it goes
(`advance_recorders_for_event()`), but composes them into masks only
afterwards: `realize_pending_masks()` replays the recorded atoms at each
fid's requested snapshot times (a choice's own events, a rate's rows), and
`fold_active_sender_support()` / `fold_active_dyad_support()` /
`build_active_dyad_point()` then fold each mask into the stored
availability. Nothing in the walk needs that order. The statistics are
computed over every dyad whatever the mask; the mask only filters the risk
set. The post-walk order is inherited from the recipe loops, which finalized
their output before a mask existed. Pooling one atom stream across fids
with a union of snapshot times was the reason to keep it once the walk
merged. The stepping handle already does it live:
`walk_build_live_masks()` feeds the batch's own `build_mask_maintainer()`
the atom entries each object event moved, and `walk_live_support()` reads
the current mask.

The batch walk uses that same construction. At each object event it collects
the atom moves, and under D3's rule a move stamped at the current clock waits
until the clock passes it. At each stored row it reads every constrained
fid's current mask and folds that fid's availability incrementally: the
rate's sender gate through per-sender allowed-receiver counts, which change
only on a flipped cell or a presence change, and the choice's `active_dyad`
as net flips of the event sender's row. What is stored does not change; the
flip streams are the same objects, emitted as the walk goes. Three things
follow:
- a constrained process's exposure is a running sum on the same walk (no
  row-time list, no second realization);
- the observed dyad can be checked against the mask at its own event rather
  than after the whole walk;
- one mask path serves batch and handle, so they cannot drift.

Detector-first, as D2: capture the folded availability and mask streams of
every constrained reference family (rate, choice, rate plus choice, REM,
flavored derived masks, coordination) before the switch, and delete
`realize_pending_masks()`'s replay and the three post-walk folds only once
all match byte-identically. *Rejected:* keep the post-walk realization and
request extra snapshot times for the exposure. It works, but it keeps a
second composition path beside the handle's and a row-time list per
constrained consumer, the very duplication this change retires.

*Settled 2026-09-28 (Alvaro) — the observed event is checked in the walk.*
At each dependent event, the walk checks the event against the live mask
and the active status the risk set is built from. It aborts there, with
today's message and condition, when the observed dyad is excluded, the
risk set is empty, or (rate) the observed sender is gated out. With the
mask live and the sender gate a maintained count, the check reads one mask
cell, one presence entry and one count per dependent event. So a bad
event stops preprocessing where it happens instead of after the whole walk.
The verdicts that describe the whole sequence can only be reached at the
end, so they stay warnings there: a receiver never an allowed candidate, a
sender never at risk, forced choices. The exception is time. If task 4.7
measures the in-walk check adding walk time that the end-of-walk check
avoids, the abort stays at the end, and the timing is recorded as the
reason.

*Amended 2026-09-29 (Alvaro) — one availability tracker per fid.* The
live masks landed first (`926a8d9`): the batch walk builds them with the
handle's `walk_build_live_masks()`, writes the atom moves strictly before
each stored row, and each fid collapses its flips into its row, so
`realize_pending_masks()` is not called and every reference matches. The
availability folds then showed where the memory goes, and it is not the
masks. `mask_cursor()` hands each reader a fresh copy of the mask per event
by design, so the observed-event validation copies the whole mask on every
event to read one row or one cell (about 13 GB on the generated fixture's
joint rate plus choice, 1500 actors, before and after the live masks), and
the post-walk folds build size-`n` vectors on every stored row (40-63 KB a
row). Everything downstream needs little per row: the rate's stored gate
changes only where a sender's count of allowed present receivers crosses
zero; the choice needs the event sender's row; the observed-event check
needs one mask cell, one presence entry and one count; the exposure
(`process-simulation` 2.11b) needs the number of entities at risk times the
interval; the handle's `walk_risk_set()` needs the same gate live.

So each constrained fid carries an availability tracker, maintained during
the walk from the two flip streams the walk already has, the live mask's
flips and the presence changes. It holds the presence vectors and the
per-sender allowed-receiver counts, updated in place and only where a flip
lands; the gate (rate) or the event sender's row read by linear index on
the stored mask (choice, rule 2), never a grid; and the count of entities at
risk as a scalar. At each stored row it writes only the availability
entries that changed, in the buffers the folds produced; at each dependent
event it answers the observed-event check in constant time; it accumulates
the counters the whole-sequence warnings need (receivers ever allowed,
senders ever at risk, forced choices). One tracker type with a variant per
encoding (rate gate; choice alter, outer, point; REM and coordination)
replaces `fold_active_sender_support()`, `fold_active_dyad_support()` and
`build_active_dyad_point()`, and is what the handle reads, so batch and
simulation share it. Presence is applied with the writer's own rule (a
composition change lands on the next stored row) until `process-simulation`
2.11c makes presence changes rows of their own (ADR-0103); the references
guard the rule. *Rejected:* fixing `mask_cursor()`'s copy alone. It removes
the 13 GB, but keeps a second, post-walk path beside the handle's and gives
2.11b no running count.

*Revised the same day (Alvaro) — availability is maintained as an effect.*
Not a new tracker type: availability is kept the way an effect is, which is
also what `active-availability-stat` already requires (one availability
object per loop, stored at its minimal encoding, `avg_active_entity`
computed during the loop from the maintained object) and what the cost
rules require of a new consumer (the shared writers and `R/broadcast_kind.R`,
no walk or buffer format of its own). Per fid, the stat is `active_sender`
(ego kind) or `active_dyad` (alter, outer or point), held in memory and
updated in place through `write_entries()` / the C++ `set_entries()`; the
cache holds the per-sender count of allowed present receivers and the count
of entities at risk. Its inputs are the live mask's flips and the presence
changes; its outputs are changes written by the existing writers (initial
value, update buffer, pointer). The handle reads the same stat and cache
through `walk_risk_set()`, so batch and simulation share one path. The
effect is maintained for every exact-time rate fid, constrained or not
(unconstrained, the stat is presence alone), because the exposure
(`exposure_entity_time`, `avg_active_entity`) now reads its running count
here rather than `risk_set_exposure()`'s post-walk loop.

The choice's point stream changes. Today it is lazy: the initial grid
carries the first event's receiver presence in every row but the first
sender's, and each event emits only the event sender's row, against the
last time that row was emitted. As an effect the grid is the true
availability for every row from the start, and every cell that flips is
emitted when it flips; a receiver presence change writes its whole column.
Each dependent event's sender row, the only part the likelihood reads, is
unchanged, and that is asserted before the moved references are refrozen
(Alvaro approved refreezing on the branch: the point-encoded choice
families of `constraint_atoms_refs.rds` and the point-choice entries of the
parity goldens). The stream grows where composition changes meet a
constraint; 4.7 records the sizes. The REM and coordination fold already
emits every flipped cell and keeps its stream. When the folds and the
validation go, `mask_cursor()` and `mask_flips()` lose their last readers
and are deleted (ADR-0054).

## Storage and per-event cost (added 2026-09-29)

Required by the per-event cost rules (`.claude/rules/walk-cost.md`),
which postdate this design. `n1`, `n2` are the sender and receiver counts,
`E` the dependent events, `C` the rows of the streams only constraints read.

**D1 (constraint-only objects on the shared walk).** New stored object:
none. The registry gains one row per constraint-only object and the joint
schedule `C` rows, both built once before the walk. The state container
gains the object itself, which the private walk already held in its own
container, so memory moves rather than grows: one copy of the object
instead of two (the shared state plus the private walk's). Per event: a
constraint-only row applies its write in place through the shared writers
and advances the atom store's changed cells, exactly as a covered
constraint's row does today; nothing of size `n1`, `n2` or `n1 x n2` is
allocated. Reads: the atom store reads the changed cells only. Prediction
before implementation: for the uncovered case, walk time and allocation
fall by the private walk's cost (its own `fetch_events()`, state
container and event loop over the `C` rows); for unconstrained and covered
models nothing moves, since the registry and schedule are byte-identical.
The preprocessed object is byte-identical until task 2.3a.

**Task 2.3a (constraint-only rows close the rate interval).** The
exact-time rate's stored rows gain one right-censored row per `C` event,
held in the existing flat stores (statistic update buffer and pointer,
`event_time`, `is_dependent`), so the object grows by `C` pointer entries
and the stored rows those events change; no new object, no grid. Per
event: the same row emission any covariate row triggers. Prediction: rate
object size grows in proportion to `C / (E + C)`; walk time moves by the
same proportion at most.

**D3 (the tied stamp on the handle; revised 2026-10-03 to the mechanism
that landed, ADR-0105).** New stored object: none, and no pending state.
The handle keeps one clock rule; a driver evaluating an observed event at
its stamp calls `walk_advance(t, inclusive = FALSE)`, which stops the
schedule cursor before the rows sharing `t` (one extra comparison per
row) and applies them after the event is injected, through the same atom
store and writers as any other row. Nothing is allocated per event beyond
what those rows already change. (The pending set of held atom moves this
paragraph first described was built and withdrawn: it broke free-running
simulation, and deferring only the touched set would have read post-tie
atoms through the live mask's recompute.)

**D9 (live masks in the batch walk).** Stored objects: unchanged. The
mask stream and the folded `active_sender` / `active_dyad` keep their
current shape (initial value, flat update buffer, per-event pointer, at
their broadcast kind); only when they are written moves, from after the
walk to during it. Per event: the live mask maintainer updates the cells
the atom moves touched; the rate's sender gate is a per-sender count of
allowed present receivers, changed only at a flipped cell or a presence
change; the choice's `active_dyad` is the net flips of the event sender's
row, read by linear index on the stored vector (rule 2), never through
the grid. The observed-event check reads one mask cell, one presence
entry and one count. Nothing of size `n1 x n2` is allocated per event.
Prediction: object sizes byte-identical; walk time of a constrained
choice falls by the post-walk realization's cost (the `realize_pending_masks()`
replay and the three folds); the in-walk check adds `O(1)` per dependent
event.

**Guards.** Each group changing per-event code opens with a cost guard
on a generated fixture large enough that one grid dominates the noise
(at least 1000 actors: one double grid is 8 MB), recorded failing on the
unfixed code, and closes with a measurement of walk time, allocation and
object size against the values before the group.

## Risks / Trade-offs

- [Threading touches `build_shared_objects()`/`build_joint_schedule()`, the
  hot substrate the baselines guard] → registry appended after unit objects
  and streams after unit streams, byte-identity asserted on the unconstrained
  and covered references before and after; baselines PASS at every commit.
- [The uncovered case's numbers could move between private replay and
  shared walk] → same strictly-before rule on both, and a reference captured
  before the threading is the detector.
- [A tied stamp makes the handle's live mask lead the batch] → the
  anchored driver's strict stepping (D3 revision, ADR-0105) plus a fixture
  that reaches the tie.
- [The oracle frame silently uses goldfish's rule instead of the process's]
  → the frames are built inside the DGP loop from the DGP's own state; arm
  (b) then compares them to goldfish's export, so a disagreement is visible
  as a frame difference before any coefficient is compared.
- [`mlogit` optimizer noise widens the gap] → the classical discipline's
  measured worst case is 2.9e-08 at 1e-6; if `mlogit` exceeds it on a
  fixture, `clogit` is the second choice oracle and the delta is recorded
  in the capability's README.
- [Composition change and the rate intercept] → goldfish's exposure sums
  over active senders and the `glm` frame has rows only for active senders,
  so the intercepts agree; `avg_active_entity` is not used by either arm.
- [`observation-tail-right-censoring` later flips D4] → one predicate, one
  test; the fixtures end with a dependent event so the interim rule and the
  flipped rule give the same numbers on them.

## Migration Plan

Commit per task on `feature_simulation`; `NOT_CRAN=true` green with
baselines PASS at each. Order: references for the uncovered case (D2 step
one) → harness and F1 (D5, D6) so the oracle path exists before the
substrate moves → threading (D1) with byte-identity → handle conformance and
tie fixture (D3) → retirement (D2 step three) → live masks in the batch
walk (D9) → fixtures F2–F5 with replay
arms (D6, D8) → close. `status: landed (feature_simulation, awaiting fold)`
at the end; no archive on the branch.

## Open Questions

- Whether a constraint-only stream row is a real exogenous event for the
  walk extent (D4; `observation-tail-right-censoring` D1/D2 own it).
- Whether the mask rule should later align with statistics and presence at
  a tied stamp (D3's rejected alternative; ADR-0073 open question).
- ~~Whether the observed-dyad check moves into the walk or stays after
  preprocessing~~ Settled 2026-09-28: in the walk, aborting at the event,
  unless 4.7 measures a walk-time cost the end check avoids (D9).
- Whether `preprocess_support_mask()` has a test-only caller worth keeping
  as a helper under ADR-0054's oracle rule, or goes.
