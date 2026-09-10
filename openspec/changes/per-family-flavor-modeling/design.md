## Context

Verified on `develop` at 1.9.31 (2026-09-09) with the flavored test fixture
(layer `calls`, flavors `creation` and `dissolution`), a rate for both and a
choice for `creation` only:

```
estimate_dynam(half, sub_model = "rate")      ERROR: `rate` and `choice` must key the same flavor set.
compute_statistics(half, sub_model = "rate")  same
preprocess_joint(half)                        ERROR: Sub-model "choice" is missing for some modeled flavor.
preprocess_flavored(half)                     same
```

The user-facing abort lives in `abort_on_completion_gaps()`, called by
`estimate_from_specification()` before the family is selected. The internal
one lives in `plan_flavor_union()`, which both `preprocess_flavored()` and the
joint `process_family_plan()` call, and which refuses when any modeled flavor
lacks the family. The joint `process_map` builder, by contrast, already emits
one row per present `(flavor, family)` and `engine_owning_fid()` already
treats an event of a flavor no fid owns as a boundary for the unit's rates.
So the merged walk's routing is ready for the asymmetric case; only its
planner is not.

Constraints: the frozen 1e-6 baselines do not move (every currently
estimable specification is unaffected); the generative path is untouched
(`completion_gaps`, `complete_generative_spec()`, `walk_open()`'s assert);
DyNAM-i has no flavor-keyed choice by construction and is unaffected.

## Goals / Non-Goals

**Goals:**
- A flavor named in one family's list is estimated in that family and
  unmodeled in the other, with the existing unmodeled-flavor semantics.
- One specification expresses the model the user wrote; no second
  specification and no completion on the estimation path.
- The living spec says one thing about mismatched key sets.

**Non-Goals:**
- Any change to `simulate()`, `estimate_dynes()`, the augmenters or the
  completion transform.
- ADR-0002's "every sub-model as blocks" and the `sub_model` retirement; only
  the selector spelling is shared and it is decided there first.
- Printer homogenization (ADR-0051), beyond not assuming symmetric families.

## Decisions

### D1 — The planners take the family's flavor subset; the walk routes the rest as unmodeled

`plan_flavor_union(spec, family)` unions the bundles of the flavors that carry
the family and records, on the union, which modeled flavors of the layer are
absent from it. The walk treats an absent flavor exactly as it treats a flavor
present in the data but keyed nowhere: on a timed rate engine its events are
right-censoring boundaries for every modeled flavor's output; on a choice
engine they are state-only. On the merged walk this is already what
`engine_owning_fid()` does for an unowned flavor, so after `preprocess-one-walk`
the change is confined to the planner; on the recipe loops it would also touch
the consumer routing, which is why this change is sequenced after that one.
*Rejected:* completing the absent family with a zero-parameter default for
estimation — ADR-0011 aborts on an effect-free sub-model, for a reason distinct
per family.

### D2 — The estimator abort goes; the gap record stays

`abort_on_completion_gaps()` is removed from `estimate_from_specification()`.
`completion_gaps` stays on the specification because the generative consumers
read it. A flavored fit reports, per family, which flavors it modeled, and the
estimation entry emits one `cli_inform` when the two families model different
flavor sets, so the asymmetry is visible without being an error.

### D3 — Asymmetric containers are the same container

`estimate_flavored()` already iterates `process_map` rows; with a gap the rows
simply cover different flavors per family. `render_process_label()` renders
from the map and needs no change; `print.goldfishFlavFit` stops assuming each
flavor has both families and groups by flavor with whatever families exist.
`coef()`, `vcov()`, `tidy()`, `glance()` and the `test_*` fan-outs read the map
and are unaffected. *Rejected:* a distinct class for a one-family or
asymmetric container (the model-spec-descriptor rule: a class earns its place
by dispatching a different implementation, and none does).

### D4 — Selector spelling is ADR-0002's

Whether a caller narrows the fit to one family through `sub_model` on the
flavored path or through a `blocks =` selector is the same question as
ADR-0002's, and is decided there before task 2.1 here. Until then the
flavored path keeps estimating every family present, which is what it does
today.

### D5 — The living spec is corrected in three places, no more

`model-specification`'s "must key the same flavor set" clause and its
rejection scenario describe an abort `make-multivariate-spec` already
removed at construction; `multivariate-specification`'s completion
requirement says the error is re-imposed at estimation. Both are reworded
to ADR-0056's table. `flavored-processes`' unmodeled-flavor requirement is
extended to "unmodeled in a family" and its estimation requirement admits
asymmetric containers. The "Single-pass preprocessing emits per-flavor
outputs" requirement is deliberately not touched here because
`preprocess-one-walk` modifies it; the fid-per-present-sub-model contract it
states already covers the asymmetric case.

## Risks / Trade-offs

- [A coefficient moves for a symmetric specification] → the union planner's
  symmetric case is byte-identical by construction; the baselines are the
  detector (ADR-0021).
- [A user expected the abort as a typo guard] → the `cli_inform` on
  asymmetric families names both flavor sets; an unknown flavor key still
  aborts at construction.
- [Overlap with `preprocess-one-walk` on the same files] → sequenced after
  it, and this change's spec deltas avoid the requirement that change
  modifies.

## Migration Plan

Internal behavior change with a NEWS entry under Improvements: a
half-specified flavored specification now estimates. No deprecation. Feature
branch, NEWS.d fragment, trunk folds (ADR-0040).

## Open Questions

- The selector spelling (D4), pending the developers' API discussion
  ADR-0002 records.
- Whether the `cli_inform` on asymmetric families should be silenced when
  the user passes `sub_model` explicitly for one family.
