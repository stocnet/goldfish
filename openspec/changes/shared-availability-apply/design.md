## Context

`broadcast-stat-updates` requires one shared flat-update and broadcast-apply per
representation layer, consumed by statistics, the support mask and the
availability stats alike. The statistics path has it on all three layers. The
availability path has it on none.

`support-mask-sparse-updates` converged the PREPROCESSING side without setting
out to: `flip_reader()` reads the mask's update buffer and both presence
crossings buffers through one function, because all three are
`(entry, replace)` keyed by a cumulative per-event pointer. That is evidence the
sharing is achievable where the shapes agree, and it is the half of the problem
already done.

The estimation side is where the shapes stop agreeing:

```
  statistics buffer        4 x k    (node1, node2, effect, replace)
  availability, sender     2 x k    (node,  replace)
  availability, dyad point 3 x k    (node1, node2, replace)
  availability, outer      2 x k    per factor vector
```

So the question this change exists to answer is not "should these be shared"
— the requirement already says yes — but "shared as what". A function whose
signature is the union of four shapes is worse than four small functions, and
writing it would satisfy the letter of the requirement while making the code
harder to read. That failure mode is the one to design against.

## Goals / Non-Goals

**Goals:**
- Meet the requirement on every layer, or narrow it for a named layer with the
  evidence that sharing there is genuinely wrong.
- Collapse the six C++ engine copies of the presence-apply loop to one.
- Decide, once and in writing, what the statistics buffer and the availability
  buffer have in common, so the next consumer does not re-litigate it.
- Move no number. The frozen baselines execute this code.

**Non-Goals:**
- The preprocessing side. `flip_reader()` already shares it, and
  `support-mask-sparse-updates` owns that history.
- Changing the availability ENCODINGS (`alter`, `outer`, `point`). What is
  shared is the apply, not the representation the encoding chose.
- The `active_dyad` encoding gaps found in the same review. Those are a separate
  successor about what the folded object means.

## Decisions

### D1 — The common abstraction is a cursor over a pointer-keyed buffer, not a signature over four shapes

Every one of these buffers is the same object at a different width: a matrix of
columns, a cumulative per-event pointer, and a rule for turning one column into
one write. What differs is only the last part.

So the shared thing is the WALK — advance to event `i`, yield this event's
columns — and the per-buffer part is the write. That is what `flip_reader()`
already is on the preprocessing side, and it is why that one converged without
anybody designing it to.

*Rejected:* a single `apply_*()` taking a shape argument and branching inside.
It satisfies the requirement's words and makes every caller pass a constant that
the function immediately switches on, which is four functions wearing one name.

*Rejected:* leaving the R backend alone on the grounds that it is two
subassignments. It is two subassignments that walk a pointer, and the pointer
walk is the part that goes wrong. The write is not what is being shared.

### D2 — C++ first, because that is where the duplication is dangerous

Six engines each carry the loop; the R backend carries it twice. The C++ copies
are the ones where a pointer bug hides, and they are the ones a reader cannot
diff by eye across six files. Doing C++ first also settles the abstraction on
the hardest layer, so the R layers either fall out of it or provide the
counter-evidence for D3.

### D3 — A layer may be exempted, but only with evidence and only in writing

If, after D1, a layer's sharing genuinely reads worse than the duplication it
replaces, that layer is exempted by a spec delta that narrows the requirement
FOR THAT LAYER and records why. What is not acceptable is leaving the
requirement asserting something the code does not do, which is the state this
change was created to end.

The bar for an exemption is a diff a reader can compare, not an assertion.

### D4 — Every task reports the baselines, and a numerical move is this change's bug

The preprocessing work could treat the frozen baselines as a floor, because no
baseline model carried a constraint and the code being changed ran outside them.
That is not true here: the six engines and the R backend apply are executed by
every baseline fit. A coefficient that moves is not a baseline to refreeze
(ADR-0021 permits a refreeze only by prediction); it is a defect in this change.

## Risks / Trade-offs

- [The abstraction is worse than the duplication] → D3 makes that a permitted,
  recorded outcome rather than something discovered halfway and then defended.
- [A C++ refactor moves a number] → the goldens run on every task and the
  engines are the code they cover; `cpp-recompile` on every `src/` edit so they
  never run against a stale object.
- [Scope creep into the availability encodings] → explicitly a non-goal. The
  encodings are `active-availability-stat`'s business and the review's gap 4
  belongs to a different successor.
- [It collides with the preprocessing track] → it does not: no file here is
  touched by `preprocess-one-walk` or `merged-walk-effect-reuse`, which is the
  reason this is its own change rather than a group in one of them.

## Migration Plan

Internal. One commit per task, `NOT_CRAN=true` with baselines PASS not SKIP at
every one. `NEWS.d/` fragment under Internal only if anything user-visible moves,
which nothing should.

## Open Questions

- Does the gather layer's presence apply share the walk, or is its writer
  different enough that D1's cursor does not fit? The gather stack expands
  candidates rather than writing a buffer, so it may be the exemption D3
  anticipates.
- Is there a fourth consumer coming? `active-availability-stat` and the
  simulation walk both read these buffers, and a shared cursor would serve them
  too. Worth knowing before the abstraction is fixed.
