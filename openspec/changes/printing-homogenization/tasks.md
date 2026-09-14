## 0. Gates and snapshot baseline

- [ ] 0.1 Confirm the branch is `feature_simulation` with a clean tree and
      the `NOT_CRAN=true` suite green (not-cran-test; baselines PASS not
      SKIP). Record in `progress.md` the state of
      `per-family-flavor-modeling` (must not have started task 2.2) and
      `identifiability-diagnostics` (task 5.2).
- [ ] 0.2 Hoist one `local_cli_context()` into `tests/testthat/helper-cli.R`
      (`cli.width = 80`, `cli.num_colors = 1`, `cli.unicode = FALSE`,
      `snet_verbosity = "quiet"`) and delete the per-file copies
      (r-lib:testing-r-packages). Existing snapshots must not move.
- [ ] 0.3 Snapshot-first: record the current output of
      `print.goldfishBaseFit`, `print.goldfishSummFit` (`compact = TRUE` and
      `FALSE`), `summary()` on a flavored container, `print.goldfishFlavFit`,
      `print.goldfishSpec` (flavored) and `print.goldfishJointSpec` under the
      helper, in a new `test-printing_baseline.R`, so every later diff is
      reviewed against a recorded before.
- [ ] 0.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP.

## 1. One process order (D2)

- [ ] 1.1 Retire `preprocess_flavored()` (ADR-0054; added 2026-09-15): with
      the family-major re-key gone it is a class stamp over
      `preprocess_joint()`, so `estimate_flavored()` calls `preprocess_joint()`
      directly and stamps `goldfishFlavPrep` itself; the container's
      `process_map` is the planner's map minus `coupled`, outputs keyed by
      the planner's fids; the stale header comment (two walks per family)
      goes with the function; `test-preprocess_flavored.R` re-pointed. Test: fids 1–4 of the
      two-flavor fixture are creation-rate, creation-choice,
      dissolution-rate, dissolution-choice; every block's coefficients equal
      the standalone fits within 1e-6.
- [ ] 1.2 Retire `flavored_row_order()`: `flavored_processes()`,
      `flavored_component_labels()`, `coef.goldfishFlavFit`,
      `print.goldfishFlavFit`, `test_gof`/`test_time`/`test_parameter` and
      `margin_table` walk `process_map` in fid order. Update the six test
      files that hard-code a fid. Test: `coef(fit)` positions equal
      `coef_layout(fit)$index`; `glance(fit)` rows are in fid order with
      identity columns (pins D5).
- [ ] 1.3 Flat `coef()`/`vcov()` on the container (D12): `coef.goldfishFlavFit`
      concatenates the blocks in fid order under `f<fid>_<short>` names,
      `vcov.goldfishFlavFit` assembles the block-diagonal matrix with the
      same dimnames; `process =` replaces `flavor =` outright (a dev-only
      argument, never released: no lifecycle shim, per the pre-2.0.0
      deprecation scope) and returns one block under bare names;
      `complete = TRUE` honored. Move the
      list-name pin in `test-estimate_flavored.R:135`; tests: names, order,
      block equality with the standalone fits, off-diagonal zeros,
      `confint.default`/`lmtest::waldtest` run on the container.
- [ ] 1.4 The fit stores its dependent layer name (D16): both engines set
      `layer` at assembly from `spec$focal` or the formula path's dependent
      name; `@return` roxygen updated; `devtools::document()`. Test on the
      spec and formula entry points of `estimate_dynam()`/`estimate_rem()`.
- [ ] 1.5 `coef_layout()` on every object (D13): `coef_layout.goldfishFit`
      returns one block labeled `<layer> › <family>` (the
      `goldfish_generic_refused` site replaced), `coef_layout.goldfishSpec` on
      single and flavored specifications; the frame gains `coef_name`,
      `term`, `export` and the effect-detail columns from `term_table()`'s
      builder, and a `pattern =` argument. Remove `model_terms()` outright
      (`R/model_terms.R`, its man page, `test-model_terms.R` folded into
      `test-coef_layout.R`), re-point every roxygen `[model_terms()]`
      reference and the unrecognized-term abort to `coef_layout()`. Update
      `test-coef_layout.R`'s refusal test to a one-block test and delete its
      error snapshot; `test-fit_class_reachability.R` if it enumerates the
      refusal.
- [ ] 1.6 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; the `estimate_flavored` and `coef_layout` snapshots re-recorded
      and reviewed; `devtools::document()` for the changed signatures and
      the removed export.

## 2. The shared renderer (D1, D3)

- [ ] 2.1 Implement `render_process_sections()` in `R/methods_display.R`:
      header rule + summary line, layer sections (only when more than one
      layer), flavor and family sections, process label with fid as data,
      content closure, footer (r-lib:cli). Unit-test with a synthetic
      process list under the pinned context.
- [ ] 2.2 Route `print.goldfishSpec` (flavored branch) through the renderer
      with the formula `cli_dl` as content; retire `print_flavor_processes()`.
      Re-record and review the `make_specification` and
      `specification_stocnet` snapshots.
- [ ] 2.3 Route `print.goldfishJointSpec` through the renderer: per-layer
      Dependent block from the same block renderer as the plain spec,
      formulas plus `separable`/`coupled`/`completed` annotation as content,
      the `estimate_dynes()` footer kept; retire `print_joint_flavor()`.
      Re-record and review the `make_joint_specification` snapshots; add a
      two-layer snapshot.
- [ ] 2.4 Route `print.goldfishFlavFit` through the renderer with the
      `print.default()` estimate block as content and the joint
      log-likelihood footer. Re-record and review the `estimate_flavored`
      snapshots; add a snapshot proving each section label is a
      `coef(fit)` name and its fid a `coef_layout(fit)` fid.
- [ ] 2.5 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; `devtools::document()` if roxygen changed.

## 3. The container summary (D4, D5)

- [ ] 3.1 `summary.goldfishFlavFit` stamps `goldfishSummFlavFit` on the
      list (unwrap unchanged); `print.goldfishSummFlavFit` renders through
      the renderer with `printCoefmat()` plus the per-process convergence
      line as content and the joint log-likelihood, free-parameter count
      and AIC once in the footer, worded over the separable processes, no
      BIC/AICc (D15). Extend `test-class_naming.R`'s
      enumeration if the new class needs a fixture; `test-flavored_summaries.R`
      keeps passing unchanged. Snapshot under the pinned context.
- [ ] 3.2 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; `devtools::document()` for the new method's roxygen.

## 4. The fit printers move to cli (D8)

- [ ] 4.1 `print.goldfishBaseFit`: rule header with the class, call as
      `{.code}`, coefficient vector via `print.default()`. Re-record the
      task-0.3 baseline and review the diff.
- [ ] 4.2 `print.goldfishSummFit`: header, call, convergence report, score
      and step norms, parameter and fixed counts, log-likelihood,
      AIC/AICc/BIC, `model`/`sub_model` provenance and both legends via
      `cli_text`/`cli_dl`/`cli_bullets`; `printCoefmat()` table and the
      compact-width probe kept, width from `cli::console_width()`. Both
      `compact` values snapshotted; diff reviewed line by line.
- [ ] 4.3 Install the package, re-knit `README.Rmd` and the `teaching1`,
      `teaching2`, `two-mode`, `dynami-example` vignette sources
      (`vignettes/rebuild-all.R`); confirm the rendered output shows the
      new printers and `tidy(tieModel, conf.int = TRUE)` still knits.
- [ ] 4.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP.

## 5. Console convention and conversion (D6, D7, D10, D11)

- [ ] 5.1 Add the internal `console_verbosity()` helper reading
      `getOption("snet_verbosity", "quiet")` and the gated `inform`/`alert`
      call pattern; `.onAttach` banner via
      `cli::cli_inform(class = "packageStartupMessage")`. Tests: quiet
      default prints nothing informational, `"normal"` prints alerts,
      warnings unaffected, `suppressPackageStartupMessages()` still works.
- [ ] 5.2 Convert the 21 verbose-progress `cat()` sites in
      `estimation_core.R`, `cpp_interface.R`, `model_estimate.R`,
      `model_preprocess.R`, `model_preprocess_group.R`, `preprocess_joint.R`
      and `make_data_group.R` to `cli_progress_step()`/`cli_alert_info()`;
      explicit `progress = TRUE` still shows them. Snapshot one estimator
      run with `progress = TRUE` under the pinned context.
- [ ] 5.3 Convert base conditions to cli, by hand, one file per commit:
      `set_opt.R` (18), `formula_parser.R` (10), `estimation_core.R` (7)
      and `cpp_interface.R` (5) — argument in `{.arg}`, value in `{.val}`,
      remedy as an `"i"` bullet, `call = NULL` where `call. = FALSE` was;
      typos fixed. Each converted message snapshotted or matched with
      `expect_error(class = )`/regexp where a test already existed.
- [ ] 5.4 Same for `model_estimate.R` (5 + 1 warning), `model_preprocess.R`
      (4 + 1 rethrow), `model_preprocess_group.R` (2), `preprocess_joint.R`
      (2 warnings), `data_source.R` (1), `functions_effects_DyNAM_choice.R`
      (7) and `functions_effects_DyNAMi_choice.R` (1). Fix the `.call =
      FALSE` misspelling at `class_checks.R:342` without converting that
      file.
- [ ] 5.5 Align the one `cli_h1`/`cli_h3` header in
      `print_data_goldfish_list()` with the convention (headers allowed
      there; rule header added) and the `print.goldfishStat`,
      `print.goldfishAlgoNewton`, `print.goldfishPrepCtrl` headers to the
      rule idiom; batch `print.goldfishParams`'s per-coefficient
      `cli_bullets()` into one call. Snapshots reviewed.
- [ ] 5.6 Verification: `grep -n 'stop(\|warning(\|cat(' R/` outside
      `class_checks.R`/`make_data.R`/deprecated printers reports only
      commented or non-user-facing sites (list them in `progress.md`);
      `NOT_CRAN=true` suite green; baselines PASS not SKIP;
      `devtools::document()` if roxygen changed.

## 6. Parameters (D14)

- [ ] 6.1 A `goldfishSpec` (single or flavored) carries its `process_map`
      from construction via `build_joint_process_map(list(spec))`;
      `set_parameters()` accepts it, keyed by the rendered labels
      (`calls › rate` on a single process). Tests on both kinds; the joint
      path unchanged.
- [ ] 6.2 `set_parameters(spec, fit)` accepts any fitted result and
      `set_parameters(spec, coef(fit))` accepts the flat vector matched by
      `f<fid>_<short>` (bare names on a single-process spec); the
      round-trip test on all three specification kinds.
- [ ] 6.3 `initial_parameters` on `estimate_dynam()`, `estimate_rem()`,
      `estimate_dynami()` and the flavored path accepts a `goldfishParams`
      (pinned free slots are the warm start; `NA` slots start at zero as
      today); the numeric vector and the flavor-keyed list build one
      internally through one helper. Tests: identical estimates from the
      three input forms; a `goldfishParams` built over a different
      specification aborts naming the mismatch.
- [ ] 6.4 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; `devtools::document()`.

## 7. Model methods (D9)

- [ ] 7.1 `nobs.goldfishBaseFit` returning `n_events` (the container reads
      its summed `logLik` attribute through the same method); correct the
      `logLik.goldfishFlavFit` comment that says a rate process counts its
      right-censored rows. Tests that `nobs()`, `logLik()`'s `nobs`
      attribute and `glance()$nobs` agree on both classes, and that on a
      fit with a censored tail `length(residuals(fit))` is `n_events + 1`
      while `nobs(fit)` is `n_events`.
- [ ] 7.2 `confint.goldfishBaseFit` (Wald, `level =`, rows named as the flat
      `coef()`; `process =` narrows to a block) with no container override;
      `tidy.goldfishBaseFit(conf.int = TRUE)` reaches it. Tests on both
      classes, including equality with the standalone fits' intervals on a
      separable container; `test-fit_class_reachability.R` stays green.
- [ ] 7.3 Tests pinning `AIC()`, `BIC()`, `lmtest::lrtest()` and
      `lmtest::waldtest()` on a single-process fit and on a flavored
      container (lmtest in Suggests; `skip_if_not_installed`), asserting
      the `logLik` `df`/`nobs` contract, that a separable container's
      `AIC()` equals the sum of its processes' (and its `BIC()` does not),
      and the `lrtest` statistic against a hand computation.
- [ ] 7.4 `test_parameter()` gains `type = c("score", "wald", "lr")` and
      `null = NULL` (ADR-0072): `"wald"` on the full fit from the flat
      `coef()`/`vcov()` over `effects =` at offset-or-zero; `"lr"` against
      `null` with the class, layer, `n_events` and `coef_layout()`-name
      subset guards; the container method reports one statistic per
      differing process and the total, `process =` narrowing;
      `goldfishParamTest` gains a `type` field, its cli print names the
      type; no `anova` method. Roxygen records the RSiena parallel
      (`score.Test` ↔ `"score"`, `Wald.RSiena`/`Multipar.RSiena` ↔
      `"wald"`, none ↔ `"lr"`) and drops the "Wald deferred" sentence.
      Tests: `"wald"` equals `lmtest::waldtest()`, `"lr"` equals
      `lmtest::lrtest()` on a nested pair; each guard aborts with its
      named reason; print snapshots per type; the existing score tests
      unchanged.
- [ ] 7.5 Verification: `NOT_CRAN=true` suite green; baselines PASS not
      SKIP; `devtools::document()` run; NAMESPACE diff reviewed.

## 8. Close

- [ ] 8.1 Re-check the seven spec deltas against the living spec:
      `bash .plan/opsx-spec-placement-check.sh printing-homogenization`
      exit 0; `openspec validate --strict --change printing-homogenization`
      clean.
- [ ] 8.2 `NEWS.d/printing-homogenization--shared-renderer.md` and
      `NEWS.d/printing-homogenization--cli-console.md`: the renderer and
      classed container summary, the **BREAKING** flavored-container fid
      order and the **BREAKING** flat container `coef()`/`vcov()` with the
      `process =` selector, the cli conversion and verbosity option,
      `coef_layout()` on every object with `pattern =` and `model_terms()`
      removed, `set_parameters()` on every specification, the `layer`
      field, `nobs()`, `confint()`, `test_parameter(type = )`. No `NEWS.md` or
      Version edit on the branch.
- [ ] 8.3 Vault: ADR-0051, ADR-0070, ADR-0071 and ADR-0072 `spec:`/open
      questions current (all minted 2026-09-14; ADR-0037 superseded);
      ADR-0050 open question ticked; dashboard fragment in `_dashboard-inbox/`; session note
      closed.
- [ ] 8.4 Final verification: full `NOT_CRAN=true` suite green; baselines
      PASS not SKIP; `devtools::check()` shows no new WARNING or NOTE;
      `status: landed (feature_simulation, awaiting fold)` in
      `proposal.md`.
