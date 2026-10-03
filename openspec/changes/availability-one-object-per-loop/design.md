## Context

Measured on `feature_simulation` at `7b56821`, after `constraint-objects-on-shared-walk`
group 4 (the availability effect, ADR-0106). See `proposal.md` for the
motivation; this section records only the shapes the design starts from.

**Producer.** `build_engine_availability()` (`R/preprocess_joint.R`) picks one
effect variant per fid: `gate` (constrained sender loop), `presence` /
`presence_dyad` (unconstrained exact-time, count only), `alter` / `outer` /
`point` (choice, via `active_dyad_encoding_decide()`), and `dyad` for any
constrained REM or coordination fid, a dense point grid whatever the mask's
kind. `availability_finish()` (`R/availability_effect.R:714-758`) writes the
stream onto the output and, for the dyad and choice variants, stashes the raw
presences on `support_mask`. The writers (`R/preprocess_writers.R:860-957`)
emit both the `active_*_changes` lists and the `active_*_update` buffers built
from them by `C_convert_composition_change()`.

**Consumers.** `make_engine_evaluator()` (`R/cpp_interface.R:44-308`) passes
both axes to every kernel but the choice one, whose signature already takes
`active_dyad_*` alone (`R/RcppExports.R`). The rate kernels update
`active_dyad` through `apply_availability_updates()` and never read it. The
REM, REM-ordered and MM kernels AND `active_sender(i)` into the allowed set
at every encoding; at point the grid already carries sender presence. The R
engine (`R/estimation_core.R:1295-1340`) walks the pointer buffer for a folded
axis and, for an unfolded axis, filters the `compChange*` data frame built
from the raw lists by time on every event; `folded_full` (`:1359`) skips the
sender reduction for a point-encoded REM or coordination. The gather loops
(`:1394-1510`, `:1600-1640`) walk both buffers and read the row by encoding.
The handle's `walk_risk_set()` (`R/walk_handle.R:1285-1325`) returns the
effect's value plus raw `presence1`, labelling an outer-encoded value
`"alter"`; the simulation driver's deferred-fid path
(`R/simulate_driver.R:~367`) composes `matrix(present2, n1, n2, byrow =
TRUE) & support_to_grid(...)` per call.

**Accessors.** `active_dyad_row()`, `active_dyad_cell()`,
`active_dyad_count()` (`R/preprocess_writers.R:803-850`) already take the
sender factor as an argument for the outer encoding; the kinds are the
broadcast lattice of `R/broadcast_kind.R` (3 global, 2 ego, 1 alter, 0
point) and `write_entries()` / `set_entries()` are the in-place writers.

**References.** `helper-constraint-atoms.R` compares a fixed field list
byte-for-byte, including `active_*_changes` and `support_mask`; the parity
goldens (`helper-parity-fixtures.R`) compare the preprocessed object minus
decoration; the C++ goldens and the frozen 1e-6 baselines compare numbers.

Constraints: the frozen baselines and goldens do not move; a branch writes a
`NEWS.d/` fragment and bumps nothing (ADR-0040); no preprocessed format bump
(development versions are free until 2.0.0; Alvaro, 2026-10-01); ADR-0054
governs retirements; the per-event cost rules (`.claude/rules/walk-cost.md`)
cover the likelihood and the simulation step.

## Goals / Non-Goals

**Goals:**
- Each engine's interface names exactly the object its loop reads.
- The preprocessed object stores availability once, as buffers, with no raw
  operand beside it; the mask stream stays as the constraint's own value.
- REM and coordination store a vector-kind constraint at the outer encoding.
- The handle and the simulation driver read the effect at its kind.
- Every number identical; byte-identity for every kept field.

**Non-Goals:**
- The per-event `allowed(n1 * n2)` allocation in the REM, REM-ordered and MM
  kernels and the R backend's list-held grid copy (estimation-side cost
  change). This change may not make either worse.
- The living-spec contradictions outside the five requirements touched
  (spec-hygiene change).
- Folding the ego-kind choice constraint; the spec is aligned to the code's
  validation-only behavior instead (D4).
- The DyNAM-i bridge's own availability (`build_active_dyad_point()` stays).
- Renaming the C++ argument identifiers beyond what D1 needs.

## Decisions

### D1 — Each kernel takes its loop's object; the sender factor is the outer encoding's second factor, absent at point

The sender-loop kernels (`estimate_DyNAM_rate`, `estimate_DyNAM_rate_ordered`)
lose the three `active_dyad_*` arguments; `make_engine_evaluator()` and the
rate gather stop passing them. The dyad-loop kernels keep two buffers because
the outer encoding *is* two vectors: `active_dyad_*` (receiver factor, or the
grid at point) and `active_dyad_sender_*` (sender factor, zero-length at
point). `estimate_DyNAM_choice` is already in this form with no sender
factor, since the choice loop never folds one (D4). The kernels' allowed-set
loop reads `active_dyad_sender(i)` only when `!active_dyad_is_point`; at
point the grid cell alone decides. The R engine drops `folded_full`'s
special case: a point-encoded dyad object means no sender reduction and the
grid as the mask; an outer-encoded one means the two factors; the sender
reduction of the statistics array happens only for the sender loop. The
gather loops read `present1_ids` from the sender factor at outer and from the
grid's rows at point.
*Rejected:* keeping both axes everywhere and documenting the dead one — the
defect the proposal names. *Rejected:* a single dyadic grid for every loop —
puts a 13.8 MB grid under every vector (D2, D13 of
`support-constraint-as-stat`), moves every baseline.

### D2 — Composition changes are stored once, as buffers; the R engine walks them

The writers stop emitting `active_sender_changes` / `active_dyad_changes`; the
`active_*_update` / `_update_pointer` buffers they already build are the only
form. `estimate_int()` builds no `compChange*` frame; `compute_step.default`
applies each axis's pointer slice through `.availability_event_cols()` as the
folded branch does today, so the `active_*_folded` flags lose their only
reader in the engine and go (the fold is a fact of the object, not a mode of
the engine). `methods_display.R`'s field list and `dynami_bridge.R`'s
assignments follow. The `support_mask` presence stashes
(`sender_presence_init`, `receiver_presence_init`, `receiver_presence_update`,
`receiver_presence_update_pointer`) are not written; `availability_finish()`
writes the stream and the encoding only. The mask stream (`initial`,
`update`, `update_pointer`, `mask_kind`, `stored_kind`, `symmetric`) stays:
it is the constraint's own kind-shaped value, the thing
`prep$support_mask` points users at, and what the maintainer tests read; no
engine reads it, and the estimation guard keeps testing its presence.
*Rejected:* deleting the mask stream too — removes the only inspectable form
of the constraint and the oracle the maintainer tests compare against, for a
field that costs one vector per kind plus flips.

### D3 — REM and coordination take the outer encoding for a vector-kind mask

`build_engine_availability()` routes a constrained REM or coordination fid
through `active_dyad_encoding_decide()` like the choice loop: a point-kind
atom or an opportunity list gives the `dyad` (point) variant; otherwise the
`outer` variant with both factors live. The effect's outer step folds the
mask at its kind into the factor of its own axis: alter kind (1) into the
receiver factor `p2[at] & mask[at]`, ego kind (2) into the sender factor
`p1[at] & mask[at]`, global kind (3) into the receiver factor as a scalar
broadcast. Coordination's symmetrisation of a vector mask is the outer
product of the same vector, so both factors receive it (the walk's
`symmetric` flag selects that); for a one-mode coordination model `n1 == n2`
and the two presences are one vector. The accessors already compute the
outer cell as `f1[i] & f2[j]`; the C++ kernels already handle the outer
encoding for an unconstrained REM, so only the producer and the variant
selection change. The handle's `walk_risk_set()` returns `encoding =
"outer"` with the effect's two factors for this variant, instead of raw
presence labelled `"alter"`.
*Rejected:* keeping the grid "because the kernel allocates one anyway" — the
kernel's per-event `allowed` vector is a separate defect (Non-Goal), and the
stored object is what the budget sizes.

### D4 — The ego-kind choice constraint is validation-only, and the spec says so

`constraint-availability-encoding` left the ego-kind choice constraint
unfolded (outer encoding, both factors raw presence) because a condition on
the sender cannot change a likelihood that conditions on the observed sender;
the living scenario "ego constraint in choice zeroes rows" still says it
folds as row flips, which would force the point encoding for a constraint
with no effect on the numbers. This change aligns the spec to the code and
makes the handle's outer-for-choice read explicit: `walk_risk_set()` returns
the receiver factor with `encoding = "alter"` for a choice fid, as it does
now, since the drawing sender's row is presence alone.
*Rejected:* folding the ego atom as row flips — a grid for a no-op.

### D5 — The simulation driver reads the effect

The deferred-fid path in `R/simulate_driver.R` returns the fid's maintained
availability from the engine's effect (`availability_step()` to the current
cursor, then its value and encoding), as `walk_risk_set()` does, instead of
composing presence with `support_to_grid()`. At point the value is the grid
the effect already holds (no copy, rule 3); at outer it is two vectors.
*Rejected:* leaving the per-call grid because simulation is "not the hot
path" — it is a per-event allocation of size `n1 x n2` in the simulation
step, which the cost rules name.

*As landed (amended 2026-10-03, conformance follow-up):* the wording above
assumed a deferred fid owns an availability effect to step. It does not. A
fid is deferred when its whole family carries no effect, so it has no
engine and no effect of its own, and its sibling's effect is the other
loop's object (the sender gate when the sibling is the rate). The deferred
path is DyNAM-only: a REM process is a single rate fid with no sibling to
defer to, so the "constrained REM" case this decision and tasks 4.2/4.3
name does not occur. What `simulation_fid_state()` does instead is read the
sibling's live mask, which survives the deferral because a process's rate
and choice share one support constraint. A deferred choice gets the
drawing sender's row of that mask at the mask's stored kind
(`support_row()`, a linear-index read, rule 2), ANDed with receiver
presence and returned at the alter encoding. A deferred rate gets the
sender gate from the same mask (`sender_gate_from_mask()`) ANDed with
sender presence. Without a sender (reading shapes only) the receivers are
presence. The cost intent holds: no grid is composed per call, and the 4.2
guard measures one call at 0.23 MB (point mask) and 0.01 MB (alter mask)
against 34.55 / 34.33 MB before, at 1500 actors. The handle's
`walk_risk_set()` is the path that reads the maintained effect, as
D3/D4 describe.

### D6 — Detector-first, references re-saved only for fields that move

Before any edit, the byte-identity references and parity goldens are
re-captured with the field list of today, and a second capture restricted to
the fields this change keeps is asserted `identical()` after each group. The
only entries whose kept fields move are the point-fold REM and coordination
references with a vector-kind constraint, if any exist (today's families use
`tie(allowedNet)`, point kind, so none is expected to move); a new reference
family covers the outer-encoded REM and coordination constraint, captured
after cell equality against the point fold is asserted. The C++ goldens and
the frozen baselines are the detector for D1 and D2: every kernel line runs
on them and no number may move.

## Storage and per-event cost

`n1`, `n2` are the sender and receiver counts, `E` the stored rows, `C` the
composition changes, `F` the mask flips.

**D1.** New stored object: none. The dyad kernels' sender factor at outer is
the vector the output already stores; at point it is zero-length, so the
kernel's argument list shrinks by `n1` doubles per fit in that case and by
`n1` doubles plus the unread pointer walk per fit for the rate kernels.
Per event: the same pointer walk through `apply_availability_updates()`;
one fewer `active_sender(i)` read per allowed cell at point. Prediction: fit
time per backend unchanged within noise on an optimized build; no allocation
moves.

**D2.** Removed stored objects: the `active_*_changes` lists (`C` list
elements of three scalars each, about 1 KB per change) and the stashes (one
or two vectors of `n1` / `n2` logicals plus a copy of the receiver update
buffer). The preprocessed object shrinks by those; prediction on the
CollegeMsg budget fixture (5000 events, 1899 actors): object size down by
under 0.1 MB unconstrained, by about `n2 * 4` bytes plus the stashed buffer
constrained; allocation down by the same plus the per-change list building.
Per event in the R engine: the folded pointer walk replaces a data-frame
filter that allocated a logical of length `C` and a subset frame per event
on every Newton iteration (rule 4); prediction: R-backend fit time on a
composition-change fixture down, measured on the Fisheries baseline fixture
(62 changes).

**D3.** For a vector-kind REM or coordination constraint the stored object
moves from a grid (`n1 * n2` logicals, 13.8 MB at 1899 actors) plus `F`
point flips plus a column of `n1` entries per receiver presence change, to
two vectors (`n1 + n2` logicals, 15 KB) plus one entry per flip or change.
Per event in the walk: the effect writes the touched factor entries in place
through `write_entries()`, nothing of size `n1`, `n2` or `n1 x n2`
allocated. Per event in the kernels: unchanged, the outer path exists. Reads:
the row for sender `i` at outer is the receiver factor gated by `f1[i]`,
read as the stored vector; a cell is two scalar reads; the count is the
product of two sums maintained by the effect's cache. Prediction on a
1500-actor generated fixture (the `constraint_cost_data()` shape with an
alter-kind atom on REM): object size down by one grid, walk allocation down
by the grid and the columns, walk time down by the grid's initialization.

**D5.** Per call of the deferred-fid path: the grid composed today (`n1 *
n2` logicals plus `support_to_grid()`'s projection) is replaced by a read of
the effect's value; at point no copy (the buffer is returned), at outer two
vectors. Prediction: simulation step allocation on a constrained REM down by
one grid per step. As landed (see D5): the read is one row of the sibling's
live mask at its kind plus presence, a vector of `n2` logicals per call for
a deferred choice and of `n1` for a deferred rate; measured on a
constrained DyNAM spec, since no REM fid is deferred.

**Guards.** Each group opens with a guard recorded failing on the unfixed
code: D2, the R engine's per-event allocation on a composition-change fixture
is bounded independent of `C`; D3, the preprocessed object of a vector-kind
REM constraint is under a small multiple of `n1 + n2` logicals and no
`n1 x n2` object is allocated during the walk; D5, one deferred-fid
evaluation on a constrained REM allocates less than a grid (as landed, on a
constrained DyNAM spec with its choice deferred; see D5). D1 has no
allocation to guard; its detector is the goldens.

## Risks / Trade-offs

- [A kernel argument removal silently shifts a positional argument] →
  `compileAttributes()` regenerates both binding files (cpp-recompile skill)
  and the C++ goldens and frozen baselines run every kernel; a shift moves
  numbers or aborts.
- [The R engine's unfolded path hid a timing difference (`<= t` filter
  versus pointer slice)] → the pointer buffer is built by
  `C_convert_composition_change()` from the same list the filter read, with
  the writer's rule (a change lands on the next stored row); the baselines
  with composition change (Fisheries, r backend) are the detector, and a
  unit test compares both forms over a sequence with a change stamped at a
  dependent event's time before the filter is deleted.
- [An outer-encoded coordination fold diverges from the symmetrised grid
  when `n1 != n2`] → coordination is one-mode; the variant selection falls
  back to point when the node sets differ, asserted by a test.
- [A global-kind mask at outer flips a whole factor] → it rides as a scalar
  broadcast on the receiver factor, exactly as the alter case with every
  entry affected; the cell-equality test includes a global atom.
- [Users relying on `prep$active_*_changes`] → internal fields of a
  development version; the NEWS.d fragment names the removal.
- [The handle's `"alter"` label for an outer-encoded value was load-bearing
  somewhere] → the replay tests on every constrained fixture
  (`walk_replay_against_batch()`) run before and after D3.

## Migration Plan

Commit per task on `feature_simulation`; `NOT_CRAN=true` green with baselines
and goldens PASS at each commit. Order: references and guards (D6) → kernels
and interface (D1, cpp-recompile per kernel) → object and R engine (D2) →
outer REM and coordination (D3) with the spec's cell-equality tests → handle
and simulation driver (D4, D5) → close. `status: landed (feature_simulation,
awaiting fold)` at the end; no archive on the branch.

## Open Questions

- Whether the estimation guard should test an explicit `constrained` flag on
  the output instead of `!is.null(prep$support_mask)`; harmless either way,
  decided when the mask stream's readers are next touched.
