## Why

**The living spec already requires this. The implementation never conformed, and
the change that wrote those requirements was archived as done.**

`support-constraint-as-stat` (archived 2026-07-10) established the target and
synced its specs into `openspec/specs/support-constraint/`. Three requirements
there describe exactly the design this change would build, and three of their
scenarios fail against the current code:

| living-spec scenario | today |
| --- | --- |
| "no per-event dense list is materialized" | a point-kind constraint stores one dense matrix per event |
| "constraint atom produces no estimated column" via `role = "constraint"` | `plan$effects` has a `role` column carrying only `"main"` |
| "mask updates reuse the statistic apply routines" | `eval_constraint_mask()` re-evaluates the whole tree on full atoms |

A fourth, under "Mask assembly per model", requires the DyNAM-rate sender gate to
be "maintained incrementally as a per-sender allowed-receiver counter ... never
recomputed by row reduction". `fold_active_sender_support()` recomputes it by
row reduction, per event, on a densified grid.

The predecessor's own design says why its producer half stalled, without meaning
to. Its D1 reads: *"The incremental maintenance already computed by the recipe
pass (atom Δ → re-eval tree at touched cells → flip) is unchanged; only its
**sink** changes."* That premise is false. There is no touched-cell maintenance
to re-sink. The producer half was scoped as a plumbing change and is actually a
maintenance change, so the consumer half shipped and the producer half did not.

What the consumer half did ship is real and is the foundation here:
`active_sender` / `active_dyad` are flat, encoding-aware objects, and no
per-event matrix list reaches the estimation engines.

The cost of the gap, measured on CollegeMsg (1899 actors):

| | |
| --- | ---: |
| rate loop, 10k events, unconstrained | 0.349 s |
| rate loop, 10k events, constrained | 362 s |
| `support_mask$support`, 3000 events | 21.90 MB |
| share of the preprocessed object | 98% |
| entries stored / entries that ever change (`~ indeg(msg) < 20`) | 5,697,000 / 4 |

Twenty-one megabytes to carry four changed bits, and the representation the data
wants already exists one step downstream at 0.0002 MB. Profiling the constrained
rate loop puts all of the cost in the three things conformance removes: atom
maintenance 64.7 percent, the tree rebuilt on full atoms per snapshot 16.0, the
fold's dense row reduction 18.8. Allocation is 44.3 GB for a 600-actor,
1498-event call, because `call_atom_template()` binds the state matrix and the
atom walk writes it with `<<-`, which is ADR-0057's copy-on-modify pattern
surviving in the one file group 0 of `preprocess-one-walk` deliberately skipped.

Two things are genuinely new rather than unmet.

**One mask per process, not per family.** A constrained DyNAM model builds two
atom maintainers and evaluates the mask 880 times, on both substrates, measured.
The constraint belongs to the `(layer, flavor)` process, so one mask should serve
its rate and its choice. The living spec is silent on this because the
predecessor only ever considered one family at a time.

**The same idea is needed in a third place.** Interaction operands are dense on
the dyad branch and correctly kind-shaped on the sender branch, where
`ov[node1] <- replace` writes at kind size with no expansion. The shared-core
requirement names statistics, the mask and the active sets, but not operands.

## What Changes

- **The support mask is produced as sparse updates at its broadcast kind**: an
  initial value plus a flat `(entry, replace)` stream, never a per-event
  snapshot list. This completes `support-constraint-as-stat`'s stated intent on
  the producer side, using the representation its consumer side already
  established.
- **Constraint atoms are maintained at their own kind and updated in place.**
  `atom_kinds[gid]` is already computed and already passed to
  `expand_operand_update()` solely to expand a kind-shaped delta into dense
  cells. Storing at the kind removes the expansion and the per-event copy of the
  atom matrix.
- **The mask is recomputed only where an atom changed.** `assemble_support_mask()`
  evaluates the constraint tree elementwise, so mask entry `e` depends only on
  entry `e` of each atom. A change to one atom entry can affect only the mask
  entries its kind projects onto. This is the same locality the interaction
  second hop already relies on, applied to the same shape of problem.
- **One mask per `(layer, flavor)` process, shared by its sub-models.** The
  dyad-shaped mask is maintained once; a sender-indexed sub-model derives its
  gate from it through the per-sender allowed-receiver counter the living spec
  already requires, rather than maintaining a second mask or recomputing a row
  reduction per event. A constraint whose atoms are all sender-axis needs no dyad
  mask at all.
- **The mask collapses the time-varying availability.** Composition changes
  (`active_1` / `active_2`) are folded into the maintained object as they occur,
  so the stored availability is the effective risk set over time and not a
  support mask that a consumer must later intersect with presence.
- **The three consumers share one set of functions (DRY).** Constraint atoms,
  interaction operands, and the mask are maintained by common
  maintain-at-kind / update-in-place / emit-changes routines rather than three
  parallel implementations. `broadcast-stat-updates` is the home the predecessor
  already named for this.
- **The atom walk folds into the main walk.** Atoms become operands of the
  compiled plan rather than a private sub-plan with its own state and schedule,
  so a constrained model is walked once. This is what makes the merged
  single-clock walk's sharing extend to constraints for the first time.
- **The per-event snapshot list leaves the preprocessed object.** Its only
  remaining reader, `validate_support_constraint()`, walks the flat stream
  instead.
- **Baselines are recorded before any code moves**, as time, peak memory and
  stored-object size, for DyNAM rate, DyNAM choice, REM and DyNAM
  choice-coordination, constrained and unconstrained. The change reports the
  contrast against those figures rather than against recollection.

## Capabilities

Most of this change is **conformance to requirements that already exist**, which
needs implementation and tests rather than spec deltas. Three requirements
change.

### New Capabilities
<!-- none: the constraint-atom and mask representations are already specified -->

### Modified Capabilities
- `support-constraint`: one mask per `(layer, flavor)` process serves every
  sub-model of that process, rather than one mask maintained per sub-model
  family; a sender-indexed sub-model derives its gate from that shared mask
  through the incremental counter the spec already requires.
- `broadcast-stat-updates`: the shared flat-update and broadcast-apply core is
  consumed by interaction operands as well as by statistics, the support mask and
  the availability stats.
- `interaction-terms`: interaction operands are maintained at their declared
  broadcast kind on the dyad branch as they already are on the sender branch,
  through the shared core, rather than expanded to dense cells per event.

## Impact

- **Depends on** `support-constraint-as-stat` (archived 2026-07-10) for the
  consumer-side representation this change produces into, and on
  `preprocess-one-walk` task 0.5a for the mask's axis-union storage and task 0.5b
  for windowed constraint atoms reaching `plan$derivations`.
- **Code**: `R/support_mask_maintain.R` (the private walk is removed, its atom
  maintenance becomes operand maintenance in the main plan),
  `R/support_mask.R` (kind-aware elementwise evaluation across mixed-kind
  atoms), `R/model_preprocess.R` (`fold_active_sender_support()` /
  `fold_active_dyad_support()` consume a stream, `expand_operand_update()` is
  replaced by kind-shaped writes), `R/preprocess_joint.R` (the merged walk's
  operand and finalize paths), `R/formula_parser.R`
  (`compile_support_constraint()` emits operands rather than a sub-plan),
  `R/model_estimate.R` (`validate_support_constraint()` walks the stream).
- **No `src/` change is expected.** The mask already reaches the compiled engines
  through the availability objects; this change alters how those objects are
  produced, not their shape at the boundary. If a `src/` change proves necessary
  it goes through `cpp-recompile` and the C++ golden baselines.
- **Baselines**: unconstrained models must stay bit-identical, and constrained
  models must produce byte-identical availability objects. The frozen
  coefficient and C++ golden baselines (`NOT_CRAN=true`, PASS not SKIP) are the
  floor; `test-preprocess_parity.R` and `test-support_mask_maintain.R` are the
  detectors for what the baselines do not reach.
- **Decision record**: ADR-0061 records the maintain-at-kind rule the four
  consumers share (statistics, interaction operands, constraint atoms, the mask). Related: ADR-0057 (the copy the gate measured), ADR-0059 (the
  in-place write and its aliasing precondition), ADR-0060 (why the loop is not
  the thing to move to C++).
