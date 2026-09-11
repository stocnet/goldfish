## Context

`support-mask-sparse-updates` closed the constrained half of the substrate
question: a layer's sub-models now share one atom pool, and the merged walk's
constrained ratio went 1.26 -> 1.04 (Social Evolution) and 1.12 -> 0.99
(CollegeMsg, 1500 events). The unconstrained ratio did not move at all — 1.35
and 1.18 — because nothing outside the constraint path changed.

That is the whole finding. **The merged walk shares exactly one thing today, the
state container, and on an unconstrained model that is not enough to pay for
merging.** The constrained columns show what happens when a second thing is
shared; the unconstrained columns show what happens when nothing is.

The gap's shape is a clue nobody has followed: 1.35 at 439 events, 1.18 at 1500,
0.98 at 10,000. A per-call constant amortised by event count. `preprocess-one-walk`
task 0.1 recorded ratios at several sizes and task 0.9 re-ran them, but neither
decomposed one — the number has always been a ratio of totals.

The deeper question is what a merged walk is for. If it only ever matches two
loops, it is a refactor with no user-visible benefit and the loops may as well
stay. If it can compute a shared quantity ONCE where two loops must compute it
twice, it is strictly better and the case for deleting the loops is a
performance case rather than an architectural preference.

## Goals / Non-Goals

**Goals:**
- Attribute the merged walk's overhead against the loops to per-call setup and
  per-event work, at three event counts, with the parts named.
- Reduce the per-call constant where it is reducible; record what is not, and
  how big it is.
- Inventory what a layer's sub-models recompute independently, and what fraction
  of the walk that is.
- Establish whether a shared node-level quantity broadcast to two kinds is a
  clean seam in goldfish's effect system, or an accident of two implementations
  agreeing. Prototype it if clean; say so and stop if not.
- Leave `preprocess-one-walk`'s group 3 decision readable on the size profile.

**Non-Goals:**
- Deleting the recipe loops. That is `preprocess-one-walk` group 3 and needs
  Alvaro's explicit approval on top of any number.
- Changing what any statistic equals. The frozen baselines are the floor.
- Moving the driver to C++ (ADR-0060 measured the ceiling at about 3x and this
  gap is 18-35 percent).
- Constraint work. `support-mask-sparse-updates` closed that half, and its
  successor owns what is left of it.

## Decisions

### D1 — Measure the decomposition before proposing any fix

The per-call constant is separated from per-event work by running each substrate
at several event counts on one dataset and fitting time against count: the
intercept is the constant, the slope the per-event work. Three counts minimum,
because two cannot show curvature and curvature is itself a finding.

The parts of the constant are then named by instrumenting the phases the merged
walk has and the loops do not — building one engine per unit, the shared
schedule, the per-unit writers and consumer specs — rather than inferred from
the intercept.

*Rejected:* profiling the merged walk and reading the top self-times. That is
exactly what produced the wrong cause for the constrained loop a day earlier:
the largest cost was spread across four callers of one function and appeared on
no line, while the function the profile did name was credited with 64.7 percent
and was worth 47. A profile is a hypothesis generator here, not evidence.

### D2 — The duplication inventory is counted, not estimated

For each `(layer, flavor)` process with more than one sub-model, count the
effect-update calls each engine makes and identify which name the same object
and the same quantity. The output is a table of what is computed twice, how
often, and its share of the walk — not an opinion about how common shared terms
are in practice.

This is the same instrument the constraint work used for atom maintainers, where
counting rather than assuming is what showed the merged walk was gaining nothing.

### D3 — The shared-quantity seam is a question, and phase 3 may answer "no"

The hypothesis: `indeg` in a rate formula and `indeg` in a choice formula are
one node-level in-degree vector at two broadcast kinds — ego (sender axis) for
the rate, alter (receiver axis) for the choice. If so, the merged walk can
maintain the vector once and project it to each sub-model's kind through
`project_value()`, which already does exactly this and already knows (ADR-0063)
that a kind carries the broadcast and not the diagonal.

The reasons it might not be clean, each of which phase 3 must check rather than
assume:

- The rate and choice kernels implement `indeg` as different functions over
  different stat caches. Whether they agree on the node-level quantity, or only
  look like they do, is unverified.
- A dyad-kernel statistic is diagonal-masked and a sender-kernel one is not, so
  the shared part is the pre-mask quantity and each consumer applies its own
  rule. ADR-0063 makes that expressible; it does not make it true.
- Windowed terms, weighted variants and `type =` arguments may make two terms
  that share a NAME not share a quantity. Same name is not the test; same
  computed value is.
- The effect catalogue is large and the seam may exist for a handful of terms.
  A mechanism that serves three effects is not obviously worth its own
  abstraction, and the inventory from D2 is what decides.

*Rejected:* generalizing first and checking coverage later. `effect-term-registry`
is the change that owns a general effect-metadata layer, and inventing a second
one here would be the divergence ADR-0061 exists to close.

### D4 — Stop conditions are named in advance

This change stops, and records why, if: the per-call constant is irreducible and
small enough that the unconstrained ratio cannot reach 1.0; or the duplication
inventory shows shared terms are rare enough that the seam cannot pay; or the
seam is not clean. Any of those is a result, and each leaves
`preprocess-one-walk` group 3 a better-informed decision than it has now.

A research change that cannot fail is not research. Naming the exits before
measuring is what keeps the answer honest when it is inconvenient.

## Risks / Trade-offs

- [The attribution finds nothing actionable] → that is a permitted outcome (D4),
  and it still converts group 3's decision from one cell to a size profile.
- [A shared-quantity maintainer changes a statistic's value] → the frozen 1e-6
  baselines and the C++ goldens gate every phase; the prototype is guarded from
  the first commit.
- [It overlaps `effect-term-registry`] → that change owns effect metadata as a
  layer. This one owns a measurement and, at most, a narrow prototype. If the
  seam turns out to want registry support, this change hands it over rather than
  building a parallel one.
- [Optimising for a ratio rather than for users] → the sizes are chosen to span
  what goldfish models actually are (439 events to 10,000), which is why the
  439-event cell is in the table at all: it is the one the current gate rule
  does not read.

## Migration Plan

Internal. One commit per task, `NEWS.d/` fragment only if behavior changes
(ADR-0040). Phase 1 (measure) and phase 2 (the constant) are independently
useful and independently revertible. Phase 3 (the seam) starts only if phases 1
and 2 say it can pay.

## Open Questions

- Does the effect catalogue have enough shared rate/choice quantities for a
  shared maintainer to matter, or is `indeg` close to the whole list?
- Is the per-call constant mostly compiling the plan, building the engines, or
  building the writers? None of the three has been measured separately.
- Should this land before or after `preprocess-one-walk` group 3? The gate rule
  as written passes on the 10k cell today, so group 3 could proceed without
  this — the question is whether it should.
