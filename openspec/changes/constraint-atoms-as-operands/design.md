## Context

Constraint atoms are effects. They are computed from the same objects by the
same `init_*` / `update_*` functions as any other effect, and kept live so the
mask can be evaluated from them. The only thing that distinguishes them is that
nobody estimates a coefficient for them — which is exactly what an interaction
OPERAND is, and `plan$effects` already has a `role` column that says so.

They nevertheless live in a sibling sub-plan with a private walk. The predecessor
made that walk much cheaper and shared it across a process's sub-models; it did
not remove it.

```
today                              target
  main walk                          main walk
    state, schedule, routing           state, schedule, routing
    effects: role = "main"             effects: role = "main"
             role = "operand"                   role = "operand"
             role = "interaction"               role = "interaction"
                                                role = "constraint"  <- atoms
  atom maintainer                      (gone)
    state, schedule, routing
    atoms
```

## Goals / Non-Goals

**Goals:**
- Constraint atoms are plan effects with `role = "constraint"`, excluded from
  estimation and from the output columns, state live across the loop.
- One walk per constrained model.
- The two-layer DAG (atoms → mask, never mask → atoms) stays enforced.
- No number moves, constrained or unconstrained.

**Non-Goals:**
- Performance. The case is duplication, not speed; the remaining duplication is
  worth about 0.13 s on a 1500-event constrained model. Claiming a performance
  win here would be claiming one that was already taken.
- The mask's own maintenance. It is a stream and stays one; only its atoms move.
- Anything about the availability encodings or the validator.

## Decisions

### D1 — The honest reason is duplication, and the proposal says so

It would be easy to write this change as a performance story, because its
ancestor was one. The measurement says otherwise, and a change that overstates
its case gets evaluated on the wrong axis and then descoped again when the
number disappoints.

The case is that one idea has two implementations, that the second one is where
the predecessor's diagonal defect could hide, and that the post-landing review
found three further gaps which exist only because a second path exists.

### D2 — The two-layer DAG check is the one new obligation

Today the parser rejects an atom whose inputs read the availability mask, and it
can do so because atoms are parsed separately. Once atoms are ordinary plan
effects the separation is gone, and the check has to be re-established on a plan
where constraint-role effects and main-role effects sit in one table.

This is the single genuinely new invariant the move creates. Everything else is
relocation. It gets its own detector and, probably, its own ADR.

### D3 — Unconstrained bit-identity is asserted, not assumed

Atoms enter `plan$effects` only when a constraint is present, so an
unconstrained model's effect list should be untouched. That is a reasonable
expectation and the predecessor learned what happens to reasonable expectations
about plan construction: the interaction seeding broke on a diagonal nobody had
written down.

So the change asserts it: an unconstrained model's `plan$effects` is identical
before and after, and the frozen baselines run on every task.

### D4 — The private walk goes last

Compile the atoms into the plan first, with the private walk still present and
still authoritative, and assert the two produce the same atom values at every
event. Then switch the mask to read the plan's atoms. Then delete the walk.

Three steps rather than one, because the intermediate state is testable and the
one-step version is not: if the atom values diverge, the two-implementation
comparison is what tells you where.

## Risks / Trade-offs

- [It touches plan construction, which the baselines guard] → that is the risk,
  and D3 makes it an assertion rather than a hope. The intermediate state in D4
  is what localises a divergence.
- [The DAG check is lost quietly] → D2 gives it a detector before the move.
- [It is the largest remaining piece and may descope again] → possible, and the
  three-step D4 sequence is designed so that stopping after step one still
  leaves a tested equivalence rather than a half-migration.
- [The case is not compelling enough to schedule] → also possible. If so, the
  right outcome is to say so and mark the requirement as knowingly deferred,
  rather than leave it unmet and uncited for a third change running.

## Migration Plan

Internal. One commit per task, `NOT_CRAN=true` with baselines PASS not SKIP at
each. `NEWS.d/` fragment under Internal.

## Open Questions

- Where does the two-layer DAG check live once atoms are plan effects — still in
  the parser, or in plan construction? The answer is probably the ADR this
  change owes.
- Do constraint atoms need a distinct `stat_kind`? They always use the dyad
  kernel today, chosen by the constraint rather than by the estimated sub-model,
  and a sender-indexed model's plan is sender-shaped. That mismatch is the first
  thing implementation will meet.
