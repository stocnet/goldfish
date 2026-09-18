## Context

`support-constraint-as-stat` (archived 2026-07-10) set the target
representation: the mask is a broadcastable stat, an initial value plus a flat
update buffer at an inferred broadcast kind, maintained by the same functions
statistics use. Its consumer half shipped. `active_sender` / `active_dyad` are
flat encoding-aware objects, and no per-event matrix list reaches the engines.

Its producer half did not. Three scenarios in the living spec fail against the
current code: "no per-event dense list is materialized" (a point-kind constraint
stores one dense matrix per event), "constraint atom produces no estimated
column" through `role = "constraint"` (`plan$effects` carries a `role` column
holding only `"main"`), and "mask updates reuse the statistic apply routines".
A fourth, requiring the DyNAM-rate gate to be "maintained incrementally as a
per-sender allowed-receiver counter ... never recomputed by row reduction", is
contradicted by `fold_active_sender_support()`.

So this is a conformance change first and a design change second. The archived
design says why the gap opened, without meaning to.
Its D1 reads: *"The incremental maintenance already computed by the recipe pass
(atom Δ → re-eval tree at touched cells → flip) is unchanged; only its **sink**
changes."* That premise is false. `eval_constraint_mask()` calls
`assemble_support_mask()` on the **full** atom matrices and rebuilds the entire
tree at every snapshot. There is no touched-cell maintenance to re-sink. The
producer half was scoped as a plumbing change and is actually a maintenance
change, which is a plausible reason it stayed deferred.

Measured state today, CollegeMsg, 1899 actors:

| | |
| --- | ---: |
| rate loop, 10k events, unconstrained | 0.349 s |
| rate loop, 10k events, constrained | 362 s |
| `support_mask$support`, 3000 events | 21.90 MB |
| share of the preprocessed object | 98% |
| entries stored / entries that ever change (`~ indeg(msg) < 20`) | 5,697,000 / 4 |

Profile of the constrained rate loop: `apply_atom_event` 64.7 percent,
`eval_constraint_mask` 16.0, `fold_active_sender_support` 18.8. Allocation is
44.3 GB for a 600-actor, 1498-event call, about eleven matrix-sized allocations
per event, because `call_atom_template()` binds the state matrix and the atom
walk then writes it with `<<-`, and the atom matrices are copied the same way.

Three consumers share the pattern. Constraint atoms are dense. Interaction
operands are dense on the dyad branch and correctly kind-shaped on the sender
branch (`ov[node1] <- replace`, no expansion). The mask's *output* moved to its
kind in `preprocess-one-walk` task 0.5a; its *production* did not.

Constraints are also maintained per family rather than per process: a
constrained DyNAM model builds two atom maintainers and evaluates the mask 880
times on both substrates, measured.

## Goals / Non-Goals

**Goals:**
- One shared maintain-at-kind core, used by statistics operands, constraint
  atoms and the support mask, with no third implementation of the same idea.
- The support mask produced as an initial value plus a flat update stream at its
  axis-union kind, never as a per-event snapshot.
- The mask recomputed only on entries whose atoms changed.
- One mask per `(layer, flavor)` process, with sender-indexed sub-models deriving
  their gate from it rather than maintaining a second one.
- Time-varying availability collapsed into the maintained object.
- The constraint walked by the main walk rather than by a private second walk.
- Measured baselines recorded before any code moves, and the contrast reported
  against them.

**Non-Goals:**
- Any change to what a constrained model *computes*. Availability objects must be
  byte-identical, and unconstrained models bit-identical.
- Porting effect bodies to C++ (ADR-0060 puts that elsewhere).
- The merged-walk-versus-recipe-loops question (`preprocess-one-walk` owns it),
  though this change is a precondition for reading its constrained cell.
- Changing the estimation-side consumption established by
  `support-constraint-as-stat`.
- `src/` changes. None are expected; see Risks.

## Decisions

### D1 — One kind-shaped state vocabulary, extracted before it is reused

The shared core is a small set of functions over a value held at a broadcast
kind, and every one of them generalizes something that already exists in exactly
one place:

| function | generalizes | today |
| --- | --- | --- |
| `kind_length(kind, n1, n2)` | the implicit sizes | scattered |
| `project_value(v, from, to, n1, n2)` | `support_to_grid()` | projects only to point |
| `reduce_value(v, from, to)` | `support_from_grid()` | reduces only from point |
| `project_entries(e, from, to, n1, n2)` | `expand_operand_update()` | expands only to point cells |
| `write_entries(buf, entries, values)` | `set_matrix_cells()` | doubles only |
| `emit_crossings(prev, new, entries)` | `crossings_from_vectors()` | whole-vector diff |

`support_to_grid()` and `expand_operand_update()` are the same function with
`to = 0` hard-coded. Making `to` a parameter is the whole extraction.

*Rejected:* writing mask-specific maintenance beside the operand maintenance.
That is the drift the predecessor's D3 existed to prevent, and it would make this
the third parallel implementation rather than the first shared one.

**Amended 2026-09-10, during implementation (ADR-0063).** The table above assumes
a dyad statistic's dense value IS its broadcast. It is not: a one-mode dyad
statistic zeroes its own diagonal (`init_DyNAM_choice.alter()` and its siblings
end with `if (!is_two_mode) diag(stats) <- 0`), so `ego(a)` is row-constant
everywhere except at `[i, i]`. A value stored at a kind therefore carries the
broadcast and nothing else, and the diagonal is a rule re-applied wherever the
value becomes dense — a `drop_diagonal` argument on `project_value()` and
`read_value_at_cells()`, threaded from whether the two node sets are the same.
The reduction needs no flag: reading each node's value from a cell whose other
index is not that node is correct in both modes.

This is not a refinement. `support_from_grid()` reduced by `grid[, 1L]` /
`grid[1L, ]`, which is the diagonal entry for node 1, so every shipped
constrained model with a separable mask was excluding actor 1 from every event's
risk set. Fixed in its own commit before the operand work resumed. Two more
functions than the table names, both tiny: `donor_index()` and
`off_diagonal_cell()`. One more than the design anticipated on the read side:
`read_value_at_cells()`, which is what `project_entries()` implies and what the
product recompute needs to read an operand without densifying it.

### D2 — The core is proven on interaction operands first, then applied to the constraint

Phase order is operands, then atoms, then the mask. Three reasons. The sender
branch of the operand path is already a working reference for the target shape,
so the core is extracted against a known-good example rather than invented. The
interaction tests already exist, so the extraction is guarded from the first
commit. And the operand path has real incremental maintenance today
(`dirty_inter` plus a recompute on touched cells only), which the constraint path
does not, so the harder half is attempted only once the core is settled.

*Rejected:* mask first, on the grounds that it is the motivating problem and the
larger win. It is, but it is also the half with no working incremental
maintenance to copy, and starting there means designing the core and the hardest
consumer at the same time.

### D3 — Atoms are maintained at `atom_kinds[gid]`, in place, and never expanded

`atom_kinds[gid]` is already computed by `compile_support_constraint()` and today
its only use is to tell `expand_operand_update()` how to blow a kind-shaped delta
up into dense cells. Storing the atom at its kind removes both the expansion and
the per-event copy of the atom matrix. An alter-kind atom is a length-n2 vector;
a scalar atom is one number.

This subsumes the `<<-` state write in `apply_atom_event()`, which is ADR-0057's
copy-on-modify pattern surviving in the one file group 0 of
`preprocess-one-walk` deliberately left alone. It comes out under ADR-0059's
invariant, with the same aliasing precondition and the same identity tests.

### D4 — The mask is recomputed only where an atom changed, and that is exact

`assemble_support_mask()` evaluates the constraint tree **elementwise**, so mask
entry `e` depends only on entry `e` of each atom. A change to atom `k` at entries
`E` can therefore affect only the mask entries that `E` projects onto under
`project_entries(E, atom_kind[k], mask_kind, n1, n2)`. Nothing else can move.

This is the same locality licence the interaction second hop already relies on
when it recomputes a product only at `dirty_inter` cells. The argument is not new;
it is newly applied to the same shape of problem.

Mechanically, per event: each atom emits its kind-shaped delta, the deltas project
into the mask's kind space, the tree is evaluated at those entries only with each
atom read through its own projection, and entries that flip are appended to the
mask's update buffer.

### D5 — Mixed-kind evaluation projects each atom into the mask's kind

The mask's kind is the axis-union of its atoms' kinds, by the interaction-operand
rule the predecessor's D2 fixed. The union is an upper bound in the lattice
`scalar < {ego, alter} < point`, so every atom projects *up* into the mask's kind
and none projects down.

```
atom kinds        mask kind      projection needed
scalar, alter  →  alter          scalar recycles; alter is identity
ego, alter     →  point          each fills its axis
point, alter   →  point          alter fills across rows
scalar         →  scalar         identity
```

`project_value()` and `project_entries()` carry these four cases and nothing
else. Today `support_to_grid()` handles the "to point" column only, which is why
a separable constraint still materializes a grid.

*Rejected:* holding every atom at point kind so evaluation is uniform. That is
what happens today, and it is the cost being removed.

### D6 — One mask per `(layer, flavor)`; a sender-indexed sub-model reduces it

The constraint is a property of the process, not of the sub-model. A
specification carrying both a rate and a choice for one flavor maintains **one**
dyad mask; the rate derives its gate as "sender `i` is at risk iff it has at
least one available receiver".

This is the semantics already shipped and already recorded in the predecessor's
D12. What changes is that the reduction reads a shared maintained mask rather
than a second mask maintained for the rate family. The reduction is maintained
incrementally by a per-row available-receiver counter: a mask cell flip adjusts
one counter, and a sender crossing is emitted only when a counter crosses zero.
That keeps the sender output at the size of its own information, which is why
`~ indeg(msg) < 20` produced four crossings and not three thousand snapshots.

A constraint whose atoms are all sender-axis needs no dyad mask at all; its mask
kind is ego or scalar and the gate is the mask.

The one-time `cli_inform()` about the row reduction, and its honest caveat that
`~ outdeg(net) > 0` matches `~ tie(net)` only under static receiver composition,
are unchanged.

### D7 — The maintained availability collapses time-varying composition

Composition changes arrive on the same schedule as everything else. The
maintained availability object folds presence as those events occur, so the
stored object is the effective risk set over time and no consumer intersects a
support mask with a presence vector afterwards.

The predecessor's D11 established the per-family folding rules and this change
does not alter them. What changes is that the fold is incremental: it consumes
the mask's flip stream rather than reconstructing from a snapshot sequence, so
`rowSums(grid & rep(active, each = n1))` per event, which the profile puts at
18.8 percent, disappears with the grid it reads.

### D8 — The atoms become operands of the main plan and the private walk goes

`build_atom_maintainer()` builds a state container, an event schedule and an
event loop that duplicate the main walk's. Atoms are effects; they belong in
`plan$effects` with an operand role, exactly as interaction operands are, with
the mask as their second-hop product.

Consequences that fall out rather than being designed: the constraint is walked
once instead of once per family, so `preprocess_pooled_support_masks()` has
nothing left to pool and retires; windowed constraint atoms
(`preprocess-one-walk` task 0.5b) already register derivations in
`plan$derivations`, so their expiry streams ride the shared schedule with no
constraint-specific branch; and on the merged single-clock walk the constraint
finally rides the shared walk, which is the one place that substrate currently
loses.

*Rejected:* keeping the private walk and only fixing its storage. That leaves the
measured 2-maintainers-per-model in place, so a constrained model is still walked
twice on both substrates.

The predecessor kept the pass separate so the unconstrained path could not move
and the frozen baselines could not shift. That argument is weaker now: atoms are
added to the plan only when a constraint is present, so an unconstrained model's
effect list is unchanged, and `test-preprocess_parity.R` plus
`test-support_mask_maintain.R` are detectors the predecessor did not have.

### D9 — The snapshot list leaves the preprocessed object

Its only remaining reader is `validate_support_constraint()`, which is already a
sequential per-event walk and can advance the flat stream instead. The folds are
the other two readers and they are being rewritten anyway.

Dropping the field is an internal breaking change to `goldfishStat`. It is
gated on the same `prep_version` mechanism the object already carries.

### D10 — Baselines are recorded before any code moves, on the full family grid

Time, peak memory and stored-object size for DyNAM rate, DyNAM choice, REM and
DyNAM choice-coordination, each constrained and unconstrained, at two sizes. The
grid matters because the families differ in what they pay: the rate loop is
driver-dominated, the choice loop is effect-dominated, REM is single-family, and
coordination symmetrises the mask and so cannot store it separably.

The change reports its result as a contrast against that table. This is
ADR-0057's lesson applied prospectively: a ratio measured against a defect is not
evidence, so the defect is measured first.

### D11 — Test-driven, with the fixtures written before the core is extracted

Every fixture must fail, or assert something no current test asserts, before the
code it guards moves. Four are named in the proposal's terms:

1. **One layer, rate and choice.** One mask serves both sub-models, and the rate
   gate equals the row reduction of the choice mask computed from scratch.
2. **Creation and deletion flavors with a state constraint.** The derived
   complementary pair, plus an additional user constraint on an exogenous
   time-varying tie layer, so the mask moves for two independent reasons.
3. **Mixed-kind atoms.** A constraint combining a scalar, an ego-axis and an
   alter-axis atom, asserting the mask's kind is the axis-union and that storage
   is at that kind rather than dense.
4. **Composition change under constraint.** Nodes entering and leaving while a
   constraint is active, asserting the maintained availability collapses both.

Byte-identity of `active_sender` / `active_dyad` against the current
implementation is the acceptance test throughout, since those objects are what
estimation consumes and they are not changing shape.

## Risks / Trade-offs

- [The mask stream and the snapshot list disagree] → byte-identity of the folded
  availability objects is asserted per fixture before the snapshot list is
  dropped, so the two representations are compared while both exist.
- [Mixed-kind elementwise evaluation is new code] → it is the one genuinely new
  piece. It is confined to `project_value()` / `project_entries()` with four
  cases each, and D11 fixture 3 exists specifically to cover it.
- [Coordination symmetrisation destroys separability] → `m & t(m)` of a
  row-constant mask is an outer product, found in `preprocess-one-walk` task
  0.5a. A symmetrised mask is maintained at point kind whatever its atoms say,
  and the `stored_kind` field that task added already carries the distinction.
- [Extracting the shared core moves the statistics path] → the functions are
  extracted, not rewritten, and the frozen 1e-6 baselines plus the C++ goldens
  are the floor. D2's ordering exists so the extraction is proven against
  interaction tests before the constraint depends on it.
- [`src/` turns out to be involved] → not expected, because the mask already
  reaches the compiled engines through the availability objects and their shape
  is unchanged. If it is, the `cpp-recompile` skill and the C++ golden baselines
  gate it.
- [The atoms-as-operands move is large] → it is the largest single step, and D2
  puts it last so the core and the storage are settled first. It can be
  descoped to a successor without losing the memory and time wins, at the cost
  of leaving the two-maintainers-per-model duplication in place.

## Migration Plan

Internal only. One commit per task, `NEWS.d/` fragment rather than `NEWS.md`
(ADR-0040). Phases follow D2: baselines and fixtures, then the shared core
against interaction operands, then atoms at their kind, then the mask as a
stream, then the atoms-as-operands fold, then the snapshot list removal.

Each phase is independently revertible. The memory and time wins land in the
third and fourth phases, so a stop after them still delivers most of the value.

## Open Questions

- Does `emit_crossings` want to live beside the availability folding or beside
  the update buffers? Both callers exist; the answer probably falls out of the
  operand extraction in phase two.
- Should the per-row available-receiver counter of D6 be maintained for every
  constrained model, or only when a sender-indexed sub-model is present? Building
  it unconditionally is simpler and costs one integer vector.
- Is `active-availability-stat` genuinely modified, or does its existing wording
  already cover an incrementally maintained fold? To be settled when the delta is
  written.
