# imputation-contract

> **Skeleton.** Created 2026-07-21 out of `multimode-network-support` D9 to hold
> the imputation *policy* surface, which is cross-cutting and pre-existing and
> should not block 2.0.0. Not yet scoped for `/opsx:apply` — `design.md`,
> `specs/`, and `tasks.md` still to be written.
>
> **Depends on** `multimode-network-support` task 3.9, which lands the single
> resolver, mode-category stratification, per-category definedness, and the
> schedule-construction abort. This change starts from that resolver.

## Why

Imputation is a semantic contract that no capability owns. Its rules are
duplicated across four sites in three files and have drifted apart:

| shape | at the start of the window | during the walk |
|---|---|---|
| dyad (network) | `NA → 0` | `NA → 0` — consistent |
| global | length-1 `mean()` → `NaN` | `NA → 0` — **disagree** |
| node, numeric | mean of observed | mean of others — consistent |
| node, categorical | most common value | **`mean()` → `NA`** — broken |

Three of eight cells are wrong or undefined, and none of that is multimode's
doing. `multimode-network-support` 3.9 fixes the categorical cell because it
cannot route walk-time through a typed resolver and leave the type branch out,
but it deliberately stops there: making two-mode imputation correct is a
different job from deciding what imputation should promise.

Two further problems have no home:

- **Deliberate missingness is destroyed before anything can honor it.**
  `ds_impute_missing()` runs at preprocessing entry (`model_preprocess.R:452`),
  *before* effect init, so an attribute that is missing by design — `party` for
  the non-politicians in `irps_nuclear` — is imputed to the most common party
  before any summarizer sees it. Mode-category stratification does **not** fix
  this: those actors are missing *within* the actor stratum, so they are still
  imputed from it. The D6 vignette works around it with sentinel recodes, which
  is a dataset-shaped fix for a package-shaped problem.
- **The tertius cache sites impute a statistic, not an attribute.**
  `update_DyNAM_rate_tertius`'s `impute_changes` branch
  (`functions_effects_DyNAM_choice.R:3019`, `:3217`, `:3230`) replaces missing
  cache entries with the cross-neighborhood mean, so a node with an undefined
  summary inherits the average of every other node. Whether that is imputation
  at all, or a summarizer contract, is unsettled.

## What Changes

- **The shape × time table becomes a documented public contract** rather than an
  implementation detail rediscovered per site, including the `global` cell that
  currently disagrees with itself.
- **A first-class route for deliberate missingness** at the `ds_impute_missing()`
  seam: a per-attribute opt-out, a sentinel-aware contract, or a declared
  "missing is a category" type. Whichever is chosen must let a summarizer see the
  missingness that the current ordering destroys.
- **A decision on the tertius cache sites** — imputation, summarizer contract, or
  neither — and consistency with whatever the attribute-level answer is.
- **Last-observation-carried-forward considered as an alternative estimator** for
  time-varying attributes, where "the mean of everyone else right now" is a
  weaker answer than "what this node last was". A choice, not a default change.

## Impact

- Affected: `R/data_source.R` (`ds_impute_missing`, `impute_attribute`),
  `R/model_preprocess.R` (walk sites, schedule construction),
  `R/functions_effects_DyNAM_choice.R` (tertius cache).
- Capability: extends `nodal-imputation`, added by `multimode-network-support`.
- Downstream: `irps_nuclear` (D6) may drop its sentinel recodes if the
  deliberate-missingness route lands first; coordinate with
  `multimode-network-support` tasks 5.1/5.3 before either ships.
- Frozen 1e-6 baselines: any change to a default imputation rule moves
  coefficients on data with missing values. Establish which baselines carry
  missingness before touching a default.

## Open Questions

- Does the deliberate-missingness route change a default, or only add an opt-in?
  A default change needs a deprecation path and new baselines.
- Is "missing is a category" better expressed at conversion time (the D6 sentinel
  approach, kept) than in the engine?
- Does LOCF belong here at all, or is it a modeling choice for a future
  time-varying-covariate change?
