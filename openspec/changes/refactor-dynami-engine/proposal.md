> **Scope note (2026-07-19, release plan):** the *data boundary* — DyNAMi
> accepting the single stocnet data object at the public surface, `make_data()`
> assembling DyNAMi components to stocnet, and the legacy-environment abort —
> was extracted to the `dynami-stocnet-boundary` change (in the 2.0.0 release).
> This change is the **post-release engine conversion only**: it consumes the
> stocnet boundary that will already exist, and additionally retires the
> internal stocnet→environment bridge that change introduces, along with the
> `data_source_envir`/`is_legacy` seam, when the monolith goes.

> **Class note (2026-09-05):** three internal classes exist only to carry
> DyNAM-i events through the monolith this change retires —
> `interaction.network.updates`, `interaction.groups.updates` and
> `windowed.interaction.network.updates`, stamped in `R/make_data_group.R` and
> read in exactly one place, `preprocessInteraction`
> (`R/model_preprocess_group.R:155,161,167`). `class-naming-scheme` renames
> them to `goldfishInterNet`, `goldfishInterGrp` and `goldfishInterWindow`
> under its uniform rule, expecting them to lose their only consumer here.
> When the monolith goes, check whether they still have one: if not, remove
> them rather than carrying three classes nothing reads.

## Why

The DyNAM/REM preprocessing path was converted to the recipe architecture
(state container + merged schedule + compiled update plan, run through
`run_sender_recipe_loop` / `run_dyad_recipe_loop`). DyNAMi was deliberately left
on the **monolithic** `preprocessInteraction` loop
(`R/model_preprocess_group.R`) plus its `cleanInteractionEvents` pre-step
(`R/make_data_group.R`).

After `refactor-formula-parsing` lands, DyNAMi is **isolated** in its own
preprocessing front-end (task 2.3e): `estimate_dynami` routes to a DyNAMi-only
front-end that keeps the current parse-time windowing (`assign()`),
`cleanInteractionEvents`, and the `preprocessInteraction` monolith, while the
shared DyNAM/REM path (recipe `spec_map` + data-boundary realizer) carries no
DyNAMi `if`-branches. DyNAMi does **not** consume the shared `spec_map`/realizer
yet. This change both (a) converts the monolith to the recipe loop **and**
(b) unifies DyNAMi onto the shared `spec_map` + state-creation realizer — the
unification `refactor-formula-parsing` deliberately left out, done here in one
move on the new foundation rather than half-migrating a loop about to be
rewritten.

## What Changes

- Replace the monolithic `preprocessInteraction` loop with a recipe-style loop
  that consumes the shared `spec_map` + realized state container and produces the
  same preprocessed outputs (`stat_state` / compact updates as appropriate to the
  DyNAMi rate and choice sub-models).
- Fold the DyNAMi-specific event handling (`cleanInteractionEvents` order
  correction + windowed-interaction class tagging; post-event update order;
  `subType` normalisation) into the recipe loop or its plan, removing the
  separate monolithic path.
- Unify DyNAMi onto the shared `spec_map` + state-creation data-boundary
  realizer (`preprocess(spec_map, data)`): retire the isolated DyNAMi front-end
  from `refactor-formula-parsing` task 2.3e (its parse-time windowing `assign()`)
  in favour of the shared realizer, so DyNAM/REM and DyNAMi share one mechanism.
- Add the `make_specification()` / spec-object estimate path for DyNAMi
  (DyNAM/REM gained it in `refactor-formula-parsing`) — unless
  `dynami-stocnet-boundary` already landed it (its grounding decides; see its
  design open questions).
- Retire the internal stocnet→environment bridge introduced by
  `dynami-stocnet-boundary` and delete the `data_source_envir`/`is_legacy`
  seam — the last environment consumers go with the monolith.
- Retire `R/model_preprocess_group.R`'s monolith and the DyNAMi branches in
  `cleanInteractionEvents` once the recipe loop reproduces them at the 1e-6
  baseline floor.

## Capabilities

### Modified Capabilities

- `dynami-preprocessing`: DyNAMi rate/choice preprocessing runs through the
  shared recipe architecture (state container, merged schedule, compiled plan),
  not the monolithic `preprocessInteraction` loop, while reproducing the existing
  coefficients to 1e-6.

## Impact

- `R/model_preprocess_group.R` (monolith retired), `R/make_data_group.R`
  (`cleanInteractionEvents` folded in), `R/model_preprocess.R`
  (`preprocess.dynami_*_spec` methods), `R/model_estimate.R` (`estimate_dynami`
  spec-object path).
- Depends on `refactor-formula-parsing` (the recipe `preprocess(spec_map, data)`
  signature + the DyNAM/REM data-boundary realizer, and the **isolated** DyNAMi
  front-end from task 2.3e) being merged first. DyNAMi's unification onto the
  shared realizer happens **in this change**, not the prior one.
- Frozen coefficient/golden baselines for DyNAMi are the regression floor.
