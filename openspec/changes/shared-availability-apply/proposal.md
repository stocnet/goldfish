---
depends-on: support-mask-sparse-updates
---

## Why

**A requirement in the living spec names three representation layers and says
the availability stats consume the shared apply on all of them. They consume it
on none.**

`broadcast-stat-updates` has required this since the capability was written,
before `support-mask-sparse-updates` touched it:

> The flat-update application and broadcast-value fan-out application SHALL be
> provided by shared functions (one per representation layer: the R backend, the
> R gather, and the C++ estimators) that are consumed by statistics, the support
> mask, and the `active_sender`/`active_dyad` availability stats alike. The mask
> and availability objects SHALL NOT carry their own copies of the flat-update or
> broadcast-apply logic.

What exists:

| layer | statistics apply | availability apply |
| --- | --- | --- |
| R backend | `apply_flat_update()`, `apply_broadcast_update()` | inline subassignment, twice, `R/estimation_core.R:1292` and `:1314` |
| R gather | `.gather_stat_cells()` plus a direct write | `.gather_apply_presence()`, `.gather_apply_presence_point()` |
| C++ estimators | `src/flat_updates.h`, `src/broadcast_updates.h` | an inline while-loop copied into six engine files |

The six are `DyNAM_choice_default.cpp`, `DyNAM_rate_default.cpp`,
`DyNAM_rate_ordered_default.cpp`, `DyNAM_MM_default.cpp`, `REM_default.cpp` and
`REM_ordered_default.cpp`. Each walks its own pointer into the availability
buffer and writes its own presence vector, with the point and broadcast
encodings branched inline.

**Six copies of one loop is six places a pointer bug can hide**, and the loop is
not incidental: it decides which actors are in the risk set at each event, so a
bug there moves the likelihood rather than crashing. The statistics path reached
this conclusion already — that is why `flat_updates.h` exists — and the
availability path was left behind.

**This is not a gap `support-mask-sparse-updates` created.** That change widened
the requirement's consumer list to include interaction operands and constraint
atoms, and it converged the PREPROCESSING side: `flip_reader()` now reads the
mask's update buffer and both presence buffers through one function. The
estimation side it declared a non-goal and did not touch. The requirement was
already unmet before it, and remains unmet after.

**The honest objection, which the design has to answer rather than dodge.** The
availability buffers are a different shape from the statistics buffers: two rows
`(entry, replace)` or three `(node1, node2, replace)`, against four
`(node1, node2, effect, replace)`. A literally shared function therefore needs a
common abstraction, not a rename. Whether that abstraction is worth its weight
is a real question on the R backend, where the duplication is two
subassignments. It is much less of a question in C++, where it is six copies.

## What Changes

- **One shared availability apply per representation layer**, consumed by
  `active_sender` and `active_dyad` alike, so the requirement is met as written
  rather than narrowed.
- **The abstraction is chosen from the buffer shapes**, not assumed. The design
  settles what the statistics buffer and the availability buffer genuinely have
  in common before any code moves, and says so in terms a reader can check.
- **The six C++ engine copies collapse to one header**, on the terms
  `set_entries()` already established in `src/state_write.cpp`: a narrow
  addition, routed through `cpp-recompile`, with the interface delta recorded.
- **The R backend and the R gather converge or are explained.** If a layer's
  duplication is genuinely two subassignments over incompatible shapes, the
  change says so with the evidence rather than forcing a shared function that
  reads worse than the thing it replaces.

## Capabilities

### New Capabilities
<!-- none: the requirement exists and is unmet -->

### Modified Capabilities
- `broadcast-stat-updates`: the requirement's WORDS do not change. It gains the
  scenarios it never had.

**That absence is why the gap survived.** The requirement names the availability
stats in its prose, and every other consumer it names has a scenario pinning it
— "mask applies via the shared functions", "interaction operands apply via the
shared functions", "statistics behavior unchanged after extraction". There has
never been one for availability, so nothing could fail while the code did not
comply. A requirement asserted only in prose is a requirement nobody can break.

The delta adds three: that an availability buffer is walked by the shared
function on every backend, that the compiled estimators reach one implementation
rather than one per engine, and that a deliberately exempted layer is recorded
as an exception rather than silently contradicting the prose.

**Written against `support-mask-sparse-updates`'s post-delta wording**
(`depends-on`), since that change modifies this requirement and has not
archived.

## Impact

- **Independent of the preprocessing track.** This is estimation-side, where
  `preprocess-one-walk` and `merged-walk-effect-reuse` do not reach. It can run
  in parallel with either.
- **Code**: `src/` (the six engines plus a new shared header),
  `R/estimation_core.R` (the two inline applies), `R/cpp_interface.R` (the
  gather presence helpers).
- **`src/` changes are expected here**, unlike the change that surfaced this.
  Every one goes through `cpp-recompile` so `RcppExports.*` is regenerated and
  the goldens never run against a stale object.
- **Baselines are a real detector, not a floor.** Unlike the preprocessing work,
  this moves code the frozen 1e-6 coefficient baselines and the C++ goldens
  execute directly. Any numerical move is a bug in this change, and every task
  reports them PASS not SKIP.
- **Decision record**: an ADR is owed on what "one shared apply per layer" means
  when the buffers differ in shape, since that is the question the requirement
  has never answered and the next consumer will ask again. Related ADR-0061
  (kind-shaped state maintained in place and emitted as changes), whose
  vocabulary is the preprocessing-side answer to the same question.
