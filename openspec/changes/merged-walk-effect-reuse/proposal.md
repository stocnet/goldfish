---
depends-on: preprocess-one-walk
---

## Why

**The merged single-clock walk is slower than the two recipe loops it replaces,
whenever there is no constraint to share.** Measured on 2026-09-10 at the close
of `support-mask-sparse-updates`, medians of repeated timed runs after a
discarded warm-up, merged against `compute_statistics()` per family:

| dataset | arm | merged s | loops s | merged/loops |
| --- | --- | ---: | ---: | ---: |
| Social Evolution, 439 events | unconstrained | 0.081 | 0.060 | 1.35 |
| Social Evolution, 439 events | constrained | 0.113 | 0.109 | 1.04 |
| CollegeMsg, 1500 events | unconstrained | 0.262 | 0.222 | 1.18 |
| CollegeMsg, 1500 events | constrained | 0.491 | 0.498 | 0.99 |
| CollegeMsg, 10k events | unconstrained | 4.922 | 5.002 | 0.98 |
| CollegeMsg, 10k events | constrained | 5.656 | 6.057 | 0.93 |

Two things are visible and neither is yet explained.

**The gap is size-dependent**: 1.35 at 439 events, 1.18 at 1500, 0.98 at 10,000.
That shape says a per-call constant the merged walk pays and the loops do not,
amortised away by event count. What that constant IS has never been measured —
`preprocess-one-walk` task 0.1 recorded the ratio, not its decomposition.

**The gap closes only where there is something to share.** The constrained
columns are 1.04 and 0.99 because `support-mask-sparse-updates` made a layer's
sub-models share one atom pool. Sharing state is what a merged walk is FOR, and
today it shares exactly one thing: the state container. The unconstrained
columns are the honest verdict on everything else.

This matters beyond tidiness. `preprocess-one-walk` group 3 deletes the recipe
loops, gated on a 1.10x rule read on the 10k cell — which passes at 0.98. But
the 439- and 1500-event cells do not, and those are the sizes most goldfish
models actually are. Deleting the loops while the merged walk is 35 percent
slower on a typical dataset would be a regression users feel, delivered under a
number that technically cleared.

**The idea worth testing is that the merged walk should be strictly faster, not
merely level.** A layer's rate and its choice routinely name the same quantity.
`indeg` in a rate formula is a node's in-degree broadcast along the sender axis;
`indeg` in a choice formula is the same in-degree broadcast along the receiver
axis. Two recipe loops MUST compute it twice, because they share nothing. A
merged walk need not — it can maintain the node-level quantity once and hand it
to each sub-model at that sub-model's own broadcast kind. `broadcast-stat-updates`
and ADR-0061 already give the vocabulary for exactly that: a value held at a kind,
projected to a wider one, written in place and emitted as changes.

Whether goldfish's effects can be decomposed that way is an open question, not
an assumption. The rate and choice kernels implement `indeg` as separate
functions (`init_DyNAM_rate.indeg`, `init_DyNAM_choice.indeg`) over separate
stat caches, and each merged engine maintains its own. Whether the shared part
is a clean seam or an accident of two implementations agreeing is the first
thing this change has to find out, and it is why this change is scoped as
research first and implementation second.

**Addendum 2026-09-11 (session goldfish-8b).** The constant has now been
decomposed and mostly removed, ahead of this change's own groups 1 and 2,
because `preprocess-one-walk` task 0.10b asked the single-family question
first. The overhead belongs to the RATE family alone: on CollegeMsg subsets
the rate-only merged/batch ratio ran 2.33 / 1.40 / 1.46 / 1.58 at 500 / 1000
/ 5000 / 10,000 events while choice-only and REM sat at 1.04 to 1.10, so it
is a cost per call and per event visible only against the cheapest kernel,
which is also why the two-family cells above converge to parity. Two causes:
setup materialized the n1 x n2 adjacency matrix eight times per merged call
against the loops' five (an existence check and an NA check that discarded
the grid, a per-unit state container the walk never reads), and the merged
covariate branch resolved its routing, update positions and broadcast kinds
from the plan per event. Commit `53add23` (preprocess-one-walk task 0.11)
removed the setup waste and inlined two helper calls; the routing table
(ADR-0066, this change's group 2) removed the per-event lookups. Rate-only
at 10k: 1.58 -> 1.17 -> 1.09; the other single-family cells 0.98 to 1.05.
Numbers and method in `.plan/sp/preprocess_single_family_2026-09-11.md`.
What is left is about 4.5 ms per call (a third data source and a second
event fetch in the schedule builder, reusable) and about 2.7 µs per event
(the per-engine step call, the price of one loop hosting several engines).
Groups 1 and 2 are closed on that record; group 3, the shared-quantity seam,
is the research this change still owns.

## What Changes

- **The interaction product is maintained at its own broadcast kind.** Its
  operands became kind-shaped in `support-mask-sparse-updates`; the thing built
  from them did not, so an alter-by-alter product still emits one point update
  per sender where a plain alter effect emits a single broadcast entry. This
  lands FIRST, because it changes what the walk emits and the measurement below
  would otherwise be taken against a baseline the next group invalidates.
- **The merged walk's overhead against the loops is decomposed and attributed**,
  at three event counts, into per-call setup and per-event work. The ratio is a
  symptom; this change produces the cause, which no measurement has yet.
  *Done 2026-09-11, four event counts, see the addendum above.*
- **The per-call constant is reduced where it is reducible**, guided by that
  attribution rather than by guesswork. What is irreducible is recorded as such,
  with its size, so the gate decision reads a floor rather than a hope.
  *Done 2026-09-11: `53add23` and the routing table (ADR-0066).*
- **An inventory of what a layer's sub-models recompute independently.** Which
  effects appear in more than one formula of one process, how often, and what
  fraction of the walk's work that duplication is.
- **The shared-quantity seam is either found or ruled out.** For the effects
  where a rate and a choice term are the same underlying node-level quantity at
  two broadcast kinds, a prototype maintains it once and projects it to each
  consumer's kind through the existing shared core. If the seam is not clean the
  change says so and stops, with the reason recorded.
- **The `preprocess-one-walk` gate is re-read on the decomposed numbers**, so
  its group 3 decision is taken on the size profile rather than on one cell.

## Capabilities

Research first. Whether any requirement changes depends on what the measurement
finds, which is the point.

### New Capabilities
<!-- none -->

### Modified Capabilities
- `multi-process-walk`: effect deduplication reaches ACROSS statistic blocks for
  the pre-broadcast node-level quantity two blocks share, where today it is
  within-block only and explicitly forbidden across dispatch families. The
  living spec's existing rule is the right one for a computed statistic — a rate
  `indeg` and a choice `indeg` are not the same column — and this carves out the
  one level below it, the quantity both columns are broadcast FROM. Sharing is
  established by the value two effects compute, never by the name they share.

**The delta is written for the target state and may be trimmed.** Design D4
names the conditions under which phase 3 does not happen; task 4.2 trims this
delta to what landed, exactly as `preprocess-one-walk` task 4.2 does on a no-go.

**Written against `preprocess-one-walk`'s post-delta wording** (`depends-on`),
since that change already modifies this requirement and has not archived. The
spec-placement pre-flight is EXPECTED to defer until it folds.

## Impact

- **Absorbs gap 7 of the post-landing conformance review** (the interaction
  product's emission kind), folded here rather than into the shared-core
  successor because both are broadcast-kind locality in the same pipeline and
  touch the same sites, and because splitting them would have this change
  measure a walk the other was about to change.
- **Depends on** `support-mask-sparse-updates` for the shared kind-shaped core
  (`project_value()`, `project_entries()`, `write_entries()`, `emit_crossings()`,
  `read_value_at_entries()`) this would consume, and for the measurement
  baseline it starts from.
- **Blocks nothing, and unblocks a decision**: `preprocess-one-walk` group 3
  (delete the recipe loops) is gated on the ratio and on explicit approval.
  Approval on a 0.98 that becomes 1.35 at typical sizes is a decision made on
  half the evidence.
- **Code, if phase 3 proceeds**: `R/preprocess_joint.R` (the engine, its stat
  cache and routing), `R/preprocess_builders.R` (effect templates and broadcast
  classification), `R/broadcast_kind.R` (the shared core it consumes).
- **No `src/` change expected.** ADR-0060 already measured the driver at roughly
  a third of a cheap effect's cost and the callback floor at 5.5 microseconds,
  so the ceiling on moving this loop to C++ is about 3x; that is not where this
  gap is, and the same reasoning applies until the attribution says otherwise.
- **Baselines**: every phase asserts the frozen 1e-6 coefficient and C++ golden
  baselines PASS not SKIP. A shared-quantity maintainer changes how a statistic
  is computed, not what it equals, so the baselines are the floor throughout.
- **Decision record**: related ADR-0057 (the gate measured a copy, not an
  architecture — the error this change exists to avoid repeating), ADR-0060 (why
  the loop is not the thing to move to C++), ADR-0061 (kind-shaped state), and
  ADR-0063 (a kind carries the broadcast, not the diagonal — which constrains
  any projection this change adds).
