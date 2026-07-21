> Follow `openspec/config.yaml` disciplines: commit-per-task, run
> `devtools::document()` inline when roxygen/exports/signatures change, test with
> `NOT_CRAN=true` (frozen one-mode baselines PASS not SKIP), bump `DESCRIPTION` +
> `NEWS.md` at the milestone. Depends on `refactor-single-data-object` (mode map
> D7, `node_lookup`, validator side rules) being in place.

## 0. Pre-work: grounding and fixtures

- [x] 0.1 Map the current two-mode surface: where `is_two_mode` is threaded
      through the DyNAM rate/choice/coordination + REM effect families
      (`R/functions_effects_*`, `R/formula_parser.R`), where the mode map resolves
      `side1`/`side2` (`R/mode_map.R`, `R/data_source.R`), and where
      `is_stocnet_assemblable()` / the assembler live (`R/legacy_wrappers.R`).
      Write findings to the change `progress.md`.
- [x] 0.2 Build hand-made two-mode + multipartite (3-mode, mixed one/two-mode
      layers) stocnet fixtures and a legacy two node-set fixture, extending
      `tests/testthat/helper-stocnet-fixtures.R`; assert both validate.
- [x] 0.3 Verify one-mode frozen baselines PASS under `NOT_CRAN=true` before any
      change (the untouched-path reference).

## 1. Legacy two-mode -> stocnet assembly (design D3)

- [x] 1.1 Drop the `two_mode` early-return in `is_stocnet_assemblable()` and add
      a two-mode branch to the assembler: fuse the two node sets into one `nodes`
      tibble with a `mode` column (distinct value per source set, labels
      preserved), remap each layer's `from`/`to` into the fused id space, and set
      `info$sender`/`info$receiver` mode sets per layer (identical for one-mode
      layers, disjoint for two-mode).
- [x] 1.2 Route composition/attribute events by node mode into the per-side
      `active_mode1`/`active_mode2` streams the mask assembly consumes.
- [x] 1.3 `make_data()` two-mode input returns a `stocnet` (never a
      `data.goldfish` environment); the one-mode path and the DyNAMi env fallback
      are unchanged. Unit tests over the fixtures.
- [x] 1.4 Verify: `NOT_CRAN=true` green, one-mode baselines PASS; commit.

## 2. Canonical mode-map representation; two node-set translation (design D2)

- [x] 2.1 Confirm no downstream branch consumes two node-set objects on the
      stocnet path (composition, identity, effect dispatch all resolve through the
      mode map); remove any residual two-node-set-name comparison in favor of the
      mode map's `is_two_mode` flag.
- [x] 2.2 Lifecycle: the two node-set *input surface* (`nodes2` argument on the
      constructors) deprecates with the constructors, its message pointing at the
      mode-set declaration; `devtools::document()`.
- [x] 2.3 Verify + commit.

## 3. Effect validity derived per argument (design D4, rewritten 2026-07-21)

- [x] 3.1 Make the mode map the source of truth over the user-fed `is_two_mode`
      effect argument, resolved **per network argument's own layer** (never a
      blanket from the focal layer): validate the declared argument against it,
      raise a `cli` warning (effect, declared value, actual mode pair) on
      disagreement with the mode map's reading winning (flip the current
      user-wins precedence at `formula_parser.R:761-774`), and extend the
      parse-time gate to effects **without** the `is_two_mode` formal (`tie`,
      `inertia`, rate `degree`/`triangle`) — the ~69 effect surfaces keep their
      signature.
- [x] 3.2 Revise the validity gates into the D4 signature rules with consistent
      `cli` errors naming the effect + layer (+ type value where relevant) and
      listing valid alternatives. Corrected taxonomy on a two-mode focal:
      valid = `inertia`/`tie`, `indeg(type = "alter")`, `outdeg(type = "ego")`,
      `four`, `ego`/`alter`/`same`/`diff`/`sim`/`ego_alter_interaction`,
      `tertius(type = "alter")`/`tertius_diff`, conforming mixed chains,
      `global`; degenerate (reject naming the type) = `indeg(type = "ego")`,
      `outdeg(type = "alter")`, `tertius(type = "ego")` on same-pair args;
      invalid = `recip`, `trans`, `cycle`, `node_trans`, `triangle`,
      `common_sender`/`common_receiver` on a two-mode focal (their two-mode
      covariate reading on a one-mode focal stays supported), non-conforming
      mixed chains. Specifics: conformability by **mode sets** not dimensions;
      add the missing DyNAM-choice `indeg`/`outdeg` type gates (REM has them);
      lift the `init_DyNAM_rate.outdeg` and `.ego` over-rejections; fix the
      three message defects
      (`ego_alter_interaction` naming `diff`; REM `indeg`/`outdeg` missing
      spaces).
- [x] 3.3 Fixtures for the two reproduced two-mode defects (design D13). Both
      regressions need fixture support that does not exist yet, and both were
      reproduced by editing the fixture inline — land that as a fixture, not as
      throwaway code: `make_stocnet_fixture_multipartite()` has **no `changes`
      slot** (the attribute-event repro needs a nodal change on a receiver-side
      node) and its `attend` layer has **no `time = NA` row** (the rate repro
      needs a non-empty initial network — an empty one returns early and hides
      the crash). Extend `helper-stocnet-fixtures.R` and assert the fixtures
      validate. Green on its own; consumed by 3.5 and 4.1.
      *The failing regression tests themselves stay inside 3.5 and 4.1 and land
      in the same commit as their fix — a red test committed here would leave
      the suite red across every commit until its phase, which the
      commit-per-task discipline forbids.*
- [x] 3.4 Nodal state keyed by mode set (design D13). `build_object_keys()` /
      `build_state_container()` stop comparing `entry$nodeset` to the
      `nodes`/`nodes2` synthetic keys and stop hard-wiring exactly two nodal
      buckets: one view per **referenced mode set**, canonicalized (sorted,
      `+`-joined), so two layers declaring the same side share one view. The
      resolver rewrites each attribute reference to its position's mode-set key.
      One-mode focal layers must still collapse both positions of
      `same`/`diff`/`sim` onto one key (arity 1, existing code path, frozen
      baselines unchanged) — assert that explicitly.
- [x] 3.5 Split nodal attribute event streams per referenced view, mirroring
      `split_composition()` (design D13): `split_stocnet_streams()` currently
      keeps `node = node_global` for every non-`active` variable, so the walk's
      `state[[component]][[key]][event_args$node]` indexes a side-local vector
      with a global id. Regression-test the reproduced crash first
      (`attend ~ alter(size)` with a change on a receiver-side node →
      `missing value where TRUE/FALSE needed`), then the sender-side silent
      case (a layer whose `side1` is not `1:n1`).
- [ ] 3.6 `ego` reads the sender-side view and `alter` the receiver-side view on
      a two-mode layer; `same`/`diff`/`sim`/`ego_alter_interaction` take two
      positions (list) when the two views differ and one (vector) when they do
      not. `is_two_mode` is **injected per attribute position** from the mode map
      (design D12 as revised) — the init is told, it does not re-derive.
      Attribute definedness per read mode: `!all(is.na(slice))` evaluated and
      reported per mode (design D4/R3), aborting with attribute + mode + effect.
      Delete the four dead attribute-effect two-mode stops
      (`same`/`diff`/`sim`/`ego_alter_interaction`) here, with the two-sided read
      that replaces them, so their unit tests are rewritten once against real
      behavior; fix `ego_alter_interaction`'s message naming `diff`.
      `directed` noted-and-ignored, mask symmetrization skipped.
      **Term-string stability check**: `GetDetailPrint()` builds
      `effect_description` from `objects_effects_link`'s resolved reference
      names, with column count `max(objects_effects_link)`, so an effect that
      gains a second attribute reference gains an `Object 2` column. Assert the
      printed/exported term string for `same(z)` is identical on one-mode and
      two-mode data — the user wrote one operand and must see one. Verify
      whether the `<mode-set-key>$` prefix reaches the rendered string (not
      confirmed during exploration) and strip it if so.
- [ ] 3.7 `tertius` / `tertius_diff` resolve their attribute position against
      **their network argument's own sender side**, not the focal S1 (design
      D13). Cover the case that motivates it: focal `actor -> event` with a
      covariate `w: org -> event`, where `tertius(w, z)` type-checks and must
      read `z` on the `org` mode.
- [ ] 3.8 Table-driven test over the effect families (each effect × type
      variant × argument mode pair → accepted / rejected), including the
      hand-computed counts for `four`, the mixed chains, and the
      one-mode-focal `common_*` projection case. Verify + commit.
- [ ] 3.9 Per-mode imputation (design D9). Spike first: reproduce pooled-mean
      contamination on an undeclared one-mode layer in a multi-mode fixture;
      check whether mean-centering pools the same way; size the cross-side
      (`same`/`diff`/`sim`) case and the deliberate-stratum-missingness case
      (imputation at `model_preprocess.R:452` destroys NAs before effects can
      honor them — weigh a per-attribute opt-out or sentinel-aware contract at
      the `ds_impute_missing()` seam). Then implement: `impute_attribute()`
      computes within each mode slice of the read side, warning names the
      attribute and mode(s). Verify + commit.

## 4. Estimation surface and node identity (design D5)

- [ ] 4.1 `make_specification()` / `estimate_dynam()` / `estimate_rem()` resolve
      the model's `nodes`/`nodes2` from the focal layer's mode map
      (`side1`/`side2`), accept multipartite objects (covariate layers with
      different mode pairs), and the spec print shows the two-mode side pair.
      Includes the reproduced two-mode **rate** crash (design D5 amendment):
      `new_model_spec()` short-circuits sender specs with
      `constructor(nodes = nodes, ...)`, dropping `nodes2`, so
      `dynam_rate_spec(nodes2 = nodes)` collapses `n2` to `n1` and a two-mode
      rate model dies with `'x' is too short` on the first non-empty network.
      Regression-test it.
- [ ] 4.2 `node_lookup` (side, local, global, label) carries onto two-mode
      preprocessed/estimation results and gather/db exports; `index_i`/`index_j`
      join back to `nodes` labels per side.
- [ ] 4.3 Retire the surviving node-set **name** comparisons in favor of
      mode-map queries (design D5 amendment): `model_estimate.R:1349`
      (`preprocessing_init` re-entry), `utils.R:332` (`ReduceBroadcastFlat`,
      where it drives broadcast diagonal exclusion), `model_spec.R:285`. Each
      works today only because `ds_side_names()` manufactures its two literals to
      satisfy them. Pin the broadcast case with a two-mode expansion test.
- [ ] 4.4 User-facing side naming: `print.specification.goldfish`
      (`methods_display.R:423`) prints the synthetic keys verbatim
      (`nodes_side1 -> nodes_side2`); show the real mode pair via
      `ds_layer_mode_pair()`. Sweep the preprocessed-object field docs
      (`methods_display.R:859-860, 895-896`) describing `nodes`/`nodes2` as node
      sets, and the `preprocess_builders.R:78-82` error that names
      `nodes_side1`/`nodes_side2` to the user. Snapshot-test the print.
- [ ] 4.5 End-to-end two-mode model on a fixture (choice_coordination + REM +
      a **rate** model, and a time-varying nodal covariate on each side) runs
      and the export lookup joins to labels. Verify + commit.

## 5. Flagship dataset, docs, vignette (design D6 — `manynet::irps_nuclear`)

*No `flavored-processes` dependency (revised 2026-07-21): the ±1 increments
convert to separate support/contestation layers, and periods use the
`start_time`/`end_time` observation window.*

- [ ] 5.1 Write the `irps_nuclear` → stocnet conversion per the D6 contract
      (revised 2026-07-21): nodes tibble with `mode` from `type` (ignore
      `present` — it duplicates the mode), derived `individual` attribute,
      `party` as numeric codes; ties split by increment sign into a focal
      `support` layer and a `contestation` covariate layer (−1 flipped to +1);
      per-claim activation changes (`active` TRUE at first event time minus a
      one-second epsilon on the numeric time axis — claims choosable at their
      own introduction — never FALSE; actors always active); sentinel recodes
      for deliberate missingness (`party` NA → 0, `power` NA → 0) so
      pre-preprocessing imputation never fires; dependent = the 545
      `default & increment == 1` events (104/161/202/78 per period — P1 matches
      the paper exactly, the 24-event gap is all in P2; stated approximation,
      see `.plan/irps_nuclear_author_questions.md`); `period2/3/4` replace-only
      global attributes at the paper's boundary dates. Freeze a small
      subset of the converted object under `tests/testthat/` for the two-mode
      baseline (goldfish ships NO `.rda` copy — manynet is in Imports).
- [ ] 5.2 Add a multipartite section to `?goldfish_data` (mode sets, the mnet →
      stocnet conversion pattern, pointer to the vignette);
      `devtools::document()`.
- [ ] 5.3 New dedicated precompiled vignette on `manynet::irps_nuclear`
      (Haunss & Hollway 2023, doi:10.1017/nws.2022.31): load from manynet, the
      live conversion from 5.1, the two-mode effect-validity discussion (what a
      two-mode layer rejects and why), and the paper's models per the D6
      contract — rate M1–M4 (`outdeg(support)` + `ego(power)` + `ego(office)` +
      `ego(individual)`), choice M5–M10 (`indeg` on support + contestation,
      `four`, tertius max-power, `tertius_diff` mean-`govt`, tertius Shannon
      over sentinel-recoded `party` with never-NA summarizers), periods via
      fully-interacted `global(period2/3/4)` dummies (rate: mains +
      interactions; choice: interactions only — mains drop out of the
      conditional logit), cross-checked against four per-period fits via
      `control_preprocessing$start_time`/`end_time` (coefficients must agree —
      the decoupling equivalence), plus the Fig 1 events-per-day and
      per-period coefficient plots (period-difference tests from the joint
      vcov); wire into `vignettes/precompile.R` and re-knit clean. Explain the
      deliberate-missingness story (`party` for non-politicians is correctly
      missing — sentinel category, excluded from the diversity index, never
      imputed).
- [ ] 5.4 Effects-vignette errata (design D4 finding 5): correct the blanket
      "cannot be used for two-mode networks" claims in
      `vignettes/goldfishEffects.Rmd` — wrong for `four`, `same`, `diff`,
      `sim`, `mixed_trans`; rephrase `common_sender`/`common_receiver` as the
      one-mode-focal + two-mode-covariate projection; type-qualify the
      `indeg`/`outdeg` statements — and align any roxygen echoing them.

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
