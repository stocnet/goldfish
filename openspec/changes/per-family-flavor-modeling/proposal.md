## Why

A flavored specification names flavors in a `rate` list and a `choice` list.
When a flavor appears in one list only, the event-stream estimators
(`estimate_dynam()`, `estimate_rem()`, `estimate_dynami()`, and
`compute_statistics()` through them) abort before selecting a family,
stating that the two lists must key the same flavor set, and behind that
message both preprocessing planners abort internally because they assume
every modeled flavor carries every family. Yet a plain rate-only formula has
always been estimable, and a flavored specification is that interface one
flavor at a time: naming `dissolution` in `rate` and not in `choice` means
"model its timing, not its partner". ADR-0056 (2026-09-09, closing the
ADR-0053 → 0055 chain) settles it: estimation estimates the family a gap
names and treats the flavor as unmodeled in the other family, with the
semantics the `flavored-processes` spec already gives an unmodeled flavor
(state update, right-censoring on timed engines, no likelihood term). The
generative consumers keep completing a gap with a warning, and a flavor named
in neither list keeps its current meaning everywhere. This change implements
the estimation side.

## What Changes

- **The estimators no longer abort on a completion gap.** For each family,
  the flavors named in its list are modeled; any other flavor of the layer is
  unmodeled in that family. `compute_statistics()` inherits it.
- **The planners walk per-family flavor subsets.** `plan_flavor_union()` and
  `process_family_plan()` union the flavors that carry the family instead of
  requiring all of them; the flavored walk (or, after `preprocess-one-walk`,
  the merged walk's per-family engine) routes an absent flavor's events as
  unmodeled: state-only on choice engines, right-censoring on timed rate
  engines.
- **Asymmetric containers.** A flavored fit may model different flavor sets
  per family; the `process_map` already has one row per present
  `(flavor, family)`, and the printers and labels name the modeled flavors
  per family rather than assuming symmetry.
- **The living spec stops contradicting itself.** `model-specification`
  still records a construction-time abort on mismatched key sets that
  `make-multivariate-spec` removed; `multivariate-specification` says the
  same-flavor-set error is re-imposed at estimation. Both are reworded.
- **The vignette's `half-specified-estimate` chunk** becomes a successful
  choice fit over both flavors, with new prose.
- Nothing changes for `simulate()`, `estimate_dynes()`, the augmenters, or
  `walk_open()`: `completion_gaps` stays on the specification and the
  generative path completes as before.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `flavored-processes`: "Unmodeled flavors update state only" extends to a
  flavor unmodeled in one family; "Per-flavor estimation returns a sectioned
  multi-process result" admits a container whose families model different
  flavor sets.
- `model-specification`: the same-flavor-set constraint on flavor-keyed
  lists is removed and its rejection scenario replaced.
- `multivariate-specification`: the completion requirement no longer says the
  same-flavor-set error is re-imposed at estimation.

## Impact

- **Code**: `R/model_estimate.R` (`abort_on_completion_gaps()` and its call
  in `estimate_from_specification()`), `R/preprocess_flavored.R`
  (`plan_flavor_union()`, `build_flavor_union()`, consumer routing for an
  absent flavor), `R/preprocess_joint.R` (`process_family_plan()`,
  `engine_owning_fid()` already treats an unowned flavor as a boundary),
  `R/estimate_flavored.R` and the printers (`render_process_label()`,
  `print.goldfishFlavFit`), the vignette source.
- **Numbers**: none move for any specification that estimates today; the
  new case is tested against the full specification's block to 1e-6.
- **Sequencing**: after `preprocess-one-walk`, so the planner change is made
  once on the merged walk rather than on the recipe loops and again after.
  The selector surface (`sub_model` on the flavored path or ADR-0002's
  `blocks =`) is decided with ADR-0002 before task 2.1; until then
  `estimate_flavored()` keeps estimating every family present.
- **ADRs**: ADR-0056 (decision), ADR-0011 (why the estimator never
  completes), ADR-0002 (block selector), ADR-0051 (printers).
