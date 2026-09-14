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
The handle conforms: atom moves collected from an object event stamped at
the current clock are held in a pending set and folded into the live mask
only when the clock advances past that stamp (or on the next `walk_advance`
beyond it), so `walk_evaluate()` at `t` sees the mask over atoms strictly
before `t` exactly as the batch snapshot does, while statistics and
presence keep their in-force-at-the-stamp behavior. A new fixture puts an
`emails` event at a call's own stamp, ordered before it, and asserts
batch-vs-replay equality there — the tie the current fixtures avoid.
*Rejected:* aligning the mask with statistics (in force when scheduled
before the event) — one clock rule for all three state kinds is cleaner,
but it changes the documented contract, moves every constrained fit with a
tied stamp, and re-mints the byte-identity references; recorded as the
alternative to revisit in ADR-0073's open questions, not taken here.

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
| F4 composition change on F1 | absent nodes: no rows; presence flips: censored rows | absent receivers excluded | handle replay with `walk_advance_presence()` |
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

## Risks / Trade-offs

- [Threading touches `build_shared_objects()`/`build_joint_schedule()`, the
  hot substrate the baselines guard] → registry appended after unit objects
  and streams after unit streams, byte-identity asserted on the unconstrained
  and covered references before and after; baselines PASS at every commit.
- [The uncovered case's numbers could move between private replay and
  shared walk] → same strictly-before rule on both, and a reference captured
  before the threading is the detector.
- [A tied stamp makes the handle's live mask lead the batch] → D3's deferred
  moves plus a fixture that reaches the tie.
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
tie fixture (D3) → retirement (D2 step three) → fixtures F2–F5 with replay
arms (D6, D8) → close. `status: landed (feature_simulation, awaiting fold)`
at the end; no archive on the branch.

## Open Questions

- Whether a constraint-only stream row is a real exogenous event for the
  walk extent (D4; `observation-tail-right-censoring` D1/D2 own it).
- Whether the mask rule should later align with statistics and presence at
  a tied stamp (D3's rejected alternative; ADR-0073 open question).
- Whether `preprocess_support_mask()` has a test-only caller worth keeping
  as a helper under ADR-0054's oracle rule, or goes.
