> Follow `openspec/config.yaml` disciplines: commit-per-task, run
> `devtools::document()` inline when roxygen/signatures change, test with
> `NOT_CRAN=true` (frozen coefficient baselines PASS not SKIP), bump `NEWS.md` at
> the milestone. No OpenSpec refs (decision IDs, task numbers, change/spec names)
> in code comments — inline the reasoning itself.
>
> **Sequencing (design D8):** start this change only after
> `multimode-network-support` is archived — it freezes the two-mode baselines
> this change proves the stamp against, and this change reframes the two-mode
> vignette that one ships.

## 0. Grounding and reference

- [x] 0.1 Reproduce the crash on a **focal-less** hand-built `stocnet`: a
      two-mode object with `info$focal` unset aborts in `check_effect_sides()`
      (`ds_layer_map(src, src$focal)`) and then in `ds_side_ids()`
      (`src$mode_map$layers[[src$focal]]`) with `get1index`. Confirm a **one-mode**
      focal-less object hits the same path (the bug is not two-mode-specific).
      Add both as helper fixtures (or extend an existing helper). Record the two
      crash sites.
- [x] 0.2 Verify the frozen coefficient baselines PASS under `NOT_CRAN=true`
      before any change (the untouched-path reference).

## 1. The fix (design D1, D4, D5)

- [x] 1.1 Stamp the resolved dependent as the working object's focal at
      estimation entry (design D1): after `dep_name` is resolved in
      `model_estimate.R`, set the working copy's `info$focal` to `dep_name` so
      every downstream `new_data_source(data = work_data)` resolves the modeled
      layer as focal. Confirm design D2 empirically — both
      `estimate_dynam(formula)` and `estimate_dynam(spec)` reach the stamp, and
      the `make_specification()` *build* path does not crash on an unset focal.
      The focal-less two-mode and one-mode fixtures from 0.1 now estimate.
- [x] 1.2 Guard the focal-name lookups (design D4): `ds_layer_map()` returns
      `NULL` for a zero-length / `NA` name instead of erroring in `[[`, so any
      other unresolved-name path degrades to the existing "not a layer, skip"
      branch rather than a `get1index` abort. Pin with a unit test on
      `ds_layer_map()`.
- [x] 1.3 Confirm (or add) a clear error when **no** dependent is resolvable —
      empty LHS, no `layer`, no `info$focal` (design D5): estimation aborts
      naming the missing declaration, never with an indexing error. Test it.

## 2. Tests

- [x] 2.1 Cover the contract end to end: (a) focal-less two-mode object
      estimates and resolves sides against the modeled layer; (b) focal-less
      one-mode object estimates; (c) an object whose `info$focal` names a
      **different** layer than the modeled one resolves side/mode lookups against
      the **modeled** layer (design D3) — including a two-mode side-validity check
      keyed to the modeled layer's mode pair; (d) the no-dependent clear-error
      case. Assert the frozen baselines are untouched by construction (the stamp
      is a no-op when `info$focal` already equals the modeled layer).

## 3. Docs — reframe to focal-less-first, `focal` as optional override (design D6, D7)

- [x] 3.1 Note in `?goldfish_data` / `add_info()` that `focal` is an optional
      default — the formula LHS or `make_specification`'s `layer` names the
      dependent, and the modeled layer drives side resolution. Run
      `devtools::document()` (check `packageVersion("roxygen2")` against
      `Config/roxygen2/version` first); confirm no unrelated man/ drift.
- [x] 3.2 Reframe the vignettes to lead with the focal-less pattern (formula/
      `layer` names the dependent) and mention `focal` once as an optional
      override: `teaching1.Rmd`, `teaching2.Rmd`, `two-mode.Rmd` — edit the
      `.orig` precompiled sources and re-knit via `vignettes/precompile.R`. For
      `two-mode.Rmd`, handle the `support (focal)` print annotation per design D7
      (drop it with the narration, or keep `focal` set and label it the optional
      override). Re-knit clean.
- [x] 3.3 Reframe the roxygen `@examples` that set `focal =` as required
      metadata — `goldfish_data`, `as_goldfish`, `fisheries_treaties`,
      `social_evolution_stocnet` — to the optional-override framing; leave the
      `focal` *argument* of `network_state_at()` / `nodes_state_at()` untouched
      (a function parameter, not `add_info` metadata). `devtools::document()`;
      confirm no unrelated man/ drift.

## 4. Milestone

- [x] 4.1 Full `NOT_CRAN=true` suite green, frozen baselines PASS **not** SKIP;
      `lintr::lint_package()` clean on touched files; `devtools::document()`;
      `openspec validate formula-drives-focal --strict`.
- [x] 4.2 `NEWS.md` bullet: a hand-built `stocnet` with no `info$focal` now
      estimates when the formula/`layer` names the dependent (previously crashed
      with an internal indexing error); the modeled layer drives side resolution.
