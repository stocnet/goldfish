> Follow `openspec/config.yaml` disciplines: commit-per-task, run
> `devtools::document()` inline when roxygen/exports/signatures change, test with
> `NOT_CRAN=true` (frozen one-mode baselines PASS not SKIP), bump `DESCRIPTION` +
> `NEWS.md` at the milestone. Depends on `refactor-single-data-object` (mode map
> D7, `node_lookup`, validator side rules) being in place.

## 0. Pre-work: grounding and fixtures

- [ ] 0.1 Map the current two-mode surface: where `is_two_mode` is threaded
      through the DyNAM rate/choice/coordination + REM effect families
      (`R/functions_effects_*`, `R/formula_parser.R`), where the mode map resolves
      `side1`/`side2` (`R/mode_map.R`, `R/data_source.R`), and where
      `is_stocnet_assemblable()` / the assembler live (`R/legacy_wrappers.R`).
      Write findings to the change `progress.md`.
- [ ] 0.2 Build hand-made two-mode + multipartite (3-mode, mixed one/two-mode
      layers) stocnet fixtures and a legacy two node-set fixture, extending
      `tests/testthat/helper-stocnet-fixtures.R`; assert both validate.
- [ ] 0.3 Verify one-mode frozen baselines PASS under `NOT_CRAN=true` before any
      change (the untouched-path reference).

## 1. Legacy two-mode -> stocnet assembly (design D3)

- [ ] 1.1 Drop the `two_mode` early-return in `is_stocnet_assemblable()` and add
      a two-mode branch to the assembler: fuse the two node sets into one `nodes`
      tibble with a `mode` column (distinct value per source set, labels
      preserved), remap each layer's `from`/`to` into the fused id space, and set
      `info$sender`/`info$receiver` mode sets per layer (identical for one-mode
      layers, disjoint for two-mode).
- [ ] 1.2 Route composition/attribute events by node mode into the per-side
      `active_mode1`/`active_mode2` streams the mask assembly consumes.
- [ ] 1.3 `make_data()` two-mode input returns a `stocnet` (never a
      `data.goldfish` environment); the one-mode path and the DyNAMi env fallback
      are unchanged. Unit tests over the fixtures.
- [ ] 1.4 Verify: `NOT_CRAN=true` green, one-mode baselines PASS; commit.

## 2. Canonical mode-map representation; two node-set translation (design D2)

- [ ] 2.1 Confirm no downstream branch consumes two node-set objects on the
      stocnet path (composition, identity, effect dispatch all resolve through the
      mode map); remove any residual two-node-set-name comparison in favor of the
      mode map's `is_two_mode` flag.
- [ ] 2.2 Lifecycle: the two node-set *input surface* (`nodes2` argument on the
      constructors) deprecates with the constructors, its message pointing at the
      mode-set declaration; `devtools::document()`.
- [ ] 2.3 Verify + commit.

## 3. Effect-validity contract per two-mode layer (design D4)

- [ ] 3.1 Make the mode map the source of truth over the user-fed `is_two_mode`
      effect argument: derive two-modeness from the focal/effect layer's mode
      map, validate the declared argument against it, and raise a `cli` warning
      (effect, declared value, actual mode pair) on disagreement — the ~69
      effect surfaces keep their signature.
- [ ] 3.2 Revise the init-method gates into the D4 taxonomy with consistent
      `cli` errors naming the effect + layer and listing valid alternatives:
      valid = dyadic memory (`inertia`, `tie`), per-side degree
      (`indeg`/`outdeg`), four-cycle closure, attribute effects
      (`ego`/`alter`/`same`/`diff`), `common_sender`/`common_receiver`
      (shared-partner reading), and `mixed_trans`-family effects when
      dimensions conform (validated against hand-computed counts); invalid =
      `recip`, `trans`, `cycle`, `node_trans`, square/symmetric-adjacency
      effects, non-conforming mixed effects.
- [ ] 3.3 `ego` reads the sender-side slice and `alter` the receiver-side slice of
      the single `nodes` tibble on a two-mode layer; `directed` noted-and-ignored,
      mask symmetrization skipped.
- [ ] 3.4 Table-driven test over the effect families (each effect × one-mode /
      two-mode → accepted / rejected). Verify + commit.

## 4. Estimation surface and node identity (design D5)

- [ ] 4.1 `make_specification()` / `estimate_dynam()` / `estimate_rem()` resolve
      the model's `nodes`/`nodes2` from the focal layer's mode map
      (`side1`/`side2`), accept multipartite objects (covariate layers with
      different mode pairs), and the spec print shows the two-mode side pair.
- [ ] 4.2 `node_lookup` (side, local, global, label) carries onto two-mode
      preprocessed/estimation results and gather/db exports; `index_i`/`index_j`
      join back to `nodes` labels per side.
- [ ] 4.3 End-to-end two-mode model on a fixture (choice_coordination + REM) runs
      and the export lookup joins to labels. Verify + commit.

## 5. Flagship dataset, docs, vignette (design D6 — `manynet::irps_nuclear`)

*Requires `flavored-processes` landed: the vignette models the ±1 claim
increments with the flavor-keyed formula syntax.*

- [ ] 5.1 Write the `irps_nuclear` → stocnet conversion (mnet/tbl_graph with
      `type` mode marker + `time`/`increment` edge attributes → nodes tibble with
      `mode`, a two-mode `claims` layer with disjoint sender/receiver mode sets,
      `increment` ±1 mapped to creation/dissolution flavors); freeze a small
      subset of the converted object under `tests/testthat/` for the two-mode
      baseline (goldfish ships NO `.rda` copy — manynet is in Imports).
- [ ] 5.2 Add a multipartite section to `?goldfish_data` (mode sets, the mnet →
      stocnet conversion pattern, pointer to the vignette);
      `devtools::document()`.
- [ ] 5.3 New dedicated precompiled vignette on `manynet::irps_nuclear`
      (Haunss & Hollway 2023, doi:10.1017/nws.2022.31): load from manynet, the
      live conversion from 5.1, the two-mode effect-validity discussion (what a
      two-mode layer rejects and why), and a paper-inspired DyNAM with
      flavor-keyed creation/dissolution formulas; wire into
      `vignettes/precompile.R` and re-knit clean.

## 6. Coefficient equivalence baselines (design D8)

- [ ] 6.1 Equivalence tests: a two-mode model as a mode-map stocnet vs the legacy
      two node-set constructors (now assembling to stocnet) agree to 1e-6 on both
      engines; a mixed one/two-mode-layer object estimates consistently.
- [ ] 6.2 Add the two-mode baselines to the frozen set under the
      `NOT_CRAN=true` / `skip_on_cran()` regime; one-mode baselines still PASS.

## 7. Milestone

- [ ] 7.1 Full `NOT_CRAN=true` suite green (one-mode + new two-mode baselines PASS
      not SKIP); `lintr::lint_package()` clean on touched files; `devtools::document()`;
      `openspec validate multimode-network-support --strict`.
- [ ] 7.2 Bump `DESCRIPTION` + `NEWS.md` describing multipartite DyNAM/REM support:
      the canonical mode-map representation, legacy two-mode assembly, the
      effect-validity contract, node identity on two-mode exports, and the new
      `irps_nuclear` vignette.
