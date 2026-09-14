## Why

**goldfish prints the same thing three ways, reports errors in two idioms,
and documents model methods it never tests.** ADR-0051 (accepted 2026-09-11,
Option B) measured the multi-process surfaces at `cli.width = 78`: a
flavored `goldfishSpec`, a `goldfishJointSpec` and a `goldfishFlavFit` name
one process (`calls › creation › rate`) in three layouts, only the joint
spec prints the fid every data method is keyed by, the joint spec drops the
Dependent block the plain spec shows, and `coef_layout()` orders processes
by fid while `coef()`, `vcov()`, `summary()` and the diagnostics order them
flavor-major — so joining the two by position silently mispairs
coefficients. `summary()` on a container prints as a bare R list: four
`$`-headed single-fit summaries with four AIC/BIC pairs and a synthesized
`Call:` nobody typed, while `print()` on the same object reports one model
and one joint log-likelihood.

Around that core, the console surface is split down the middle.
`print.goldfishSummFit` — the only fit output the README and four vignettes
show — and `print.goldfishBaseFit` are base-R `cat()` renderers with no
snapshot coverage, next to thirteen cli print methods; two header idioms
coexist (`cli_rule(left = "{.cls …}")` in nine places, `cli_h1`/`cli_h3` in
one). 135 base `stop()` and 13 `warning()` calls remain beside 402
`cli_abort()` calls, 60 of them outside the deprecated data-class path, and
21 `progress =`/`verbose =` paths still `cat()` their status. ADR-0042
deferred the manynet `snet_*()` messaging question to "a proposal and an ADR,
not an import" — this is that proposal. On the model-method side,
`lmtest::lrtest()` is documented as supported and never tested,
`tidy(conf.int = TRUE)` reaches `stats:::confint.default` because no
`confint()` method exists, `nobs()` falls through to `nobs.default` reading
`residuals()` instead of the event count `logLik()` carries, and the Wald
form of `test_parameter()` has been deferred since 2026-07-19 while nested
comparison had no goldfish surface at all.

Why now: `per-family-flavor-modeling` is about to rewrite
`print.goldfishFlavFit` for asymmetric containers, `identifiability-diagnostics`
is about to add content to `summary()`, and `abmcem`/`gof-dynes` will add the
DyNES fit class. Each would build on the divergent printers and re-record
snapshots that this change re-records again. One renderer first, then the
three changes render through it.

## What Changes

- **One shared multi-process renderer.** A single internal renderer takes a
  set of processes keyed by fid — each with a layer, a flavor, a family and a
  caller-supplied content block — and renders header, per-process sections
  and footer. `print.goldfishSpec` (flavored), `print.goldfishJointSpec`,
  `print.goldfishFlavFit` and the container summary all call it;
  `print_joint_flavor()` and `print_flavor_processes()` are retired. The
  rendered process label is the `render_process_label()` string every data
  method uses, and the fid is shown on every surface.
- **One process order.** The flavored container adopts the joint planner's
  flavor-major fid order instead of re-keying it family-major;
  `flavored_row_order()` becomes the identity and is retired. `coef()`,
  `vcov()`, `summary()`, `tidy()`, `glance()`, `coef_layout()`,
  `goldfishParams` and every diagnostic fan-out then agree by position and by
  key. **BREAKING** for code that indexed a flavored container's fids by
  number: fid 2 was the second flavor's rate and is now the first flavor's
  choice.
- **Every fit has a flat `coef()` and `vcov()`** (ADR-0071). The flavored
  container stops returning lists: `coef()` is one named vector in fid
  order and `vcov()` one matrix, block-diagonal while the processes are
  separable and dense once a coupled fit supplies cross-process covariance,
  as RSiena does for co-evolution. Names on a multi-process fit are
  `f<fid>_<short>`; a single-process fit keeps bare short names;
  `coef(fit, process = )` returns one block under bare names (the `flavor =`
  selector generalized). `confint.default`, `lmtest::waldtest()` and every
  other R-base consumer then work on a container with no goldfish method.
  **BREAKING** for code that read the container's `coef()` as a list.
- **`coef_layout()` on every specification and fit, and it absorbs
  `model_terms()`.** It dispatches on a single-process or flavored
  `goldfishSpec`, a `goldfishFit` (one block labeled from the layer name the
  fit now stores; the refuse verdict is withdrawn), a `goldfishParams`, a
  joint spec and a container; it carries the process identity, every term
  spelling, the effect details, `coef_name`, and the values where the object
  has them, with a `pattern =` filter. `model_terms()` is removed outright
  (dev-only, never released) and every pointer to it moves.
- **`set_parameters()` on every specification; `goldfishParams` everywhere.**
  A single-process or flavored `goldfishSpec` carries its `process_map`
  from construction, so `set_parameters()` accepts it; the from-result form
  accepts any fit and a flat `coef()` by name; `initial_parameters` on every
  estimator accepts a `goldfishParams`, with the numeric vector and the
  flavor-keyed list kept as conveniences. Fixing stays in the formula.
- **The joint spec regains a Dependent block per layer**, rendered by the
  same renderer as the plain spec's.
- **`summary()` on a container returns a classed list** (`goldfishSummFlavFit`)
  whose print renders the joint header and footer once and each process's
  coefficient table as its content block; the list shape and the
  `flavor =` unwrap are unchanged. The footer prints the joint
  log-likelihood, parameter count and AIC once (AIC is additive over
  separable processes; BIC and AICc are not, so they are left to the
  generics). `glance()` on a container stays one row per process and is
  pinned by test; it is not given a pooled row.
- **`print.goldfishSummFit` and `print.goldfishBaseFit` move to cli.** Header,
  call, convergence, log-likelihood, information criteria and legend render
  via cli semantic elements with data interpolated; the coefficient table
  stays on `stats::printCoefmat()`, since cli formats prose, not columns.
  Snapshots are recorded for both before they move.
- **One console-output convention, written down.** A `console-output`
  capability records the house style: `cli_rule(left = "{.cls …}")` as the
  object header, `cli_h*` only inside the data print, one shared
  `local_cli_context()` test helper, no base `stop()`/`warning()`/`cat()` in
  user-facing paths outside the deprecated data-class code, and informational
  output (progress, info, success) gated by the stocnet
  `options(snet_verbosity = )` levels read through `getOption()`, never
  imported from manynet. Errors and warnings are never gated.
- **Base conditions convert to cli**: the 60 `stop()`/`warning()` sites
  outside `class_checks.R`/`make_data.R` become `cli_abort()`/`cli_warn()`
  with the argument, value and remedy interpolated; the 21 verbose-progress
  `cat()` sites become `cli_progress_step()`/`cli_alert_info()` under the
  verbosity gate. Three typos and one `.call = FALSE` misspelling are fixed
  in passing, not frozen into snapshots.
- **Model-method gaps close.** `nobs()` and `confint()` methods on the fit
  parent only, since the flat shape makes container overrides unnecessary;
  tests pin that `AIC()`, `BIC()`, `lmtest::lrtest()` and
  `lmtest::waldtest()` work on both classes; `test_parameter()` gains
  `type = c("score", "wald", "lr")` and `null =` (ADR-0072, superseding
  ADR-0037's separate `test_nested()` generic): the Wald type on the full
  fit from the flat `coef()`/`vcov()`, the likelihood-ratio type against a
  restricted `null` fit with nestedness and same-data guards, and on a
  container one statistic per differing process. `anova()` stays
  unregistered. Verdicts for the new generics are recorded in
  `fit-class-hierarchy` before the code is written.
- **Not changed**: no `print.stocnet` — manynet owns it and goldfish only
  coerces to it; no import of manynet's `snet_*()` helpers; no
  `goldfish_verbosity` option; the deprecated `print.nodes.goldfish`,
  `print.network.goldfish`, `print.dependent.goldfish` and
  `print.data.goldfish` keep their base-R bodies until the path is deleted;
  the frozen coefficient baselines do not move.

## Capabilities

### New Capabilities

- `console-output`: the package-wide console convention — cli at the call
  site with data interpolated, the object-header idiom, the stocnet
  verbosity gate for informational output, no base conditions or raw `cat()`
  on live user-facing paths, and one pinned cli context for snapshot tests.
- `multi-process-rendering`: the shared renderer's content contract — what
  every multi-process surface shows in common (fid, label, layer, flavor,
  family, in the one canonical order), what each caller supplies, and how
  the container summary and the fit statistics print once.

### Modified Capabilities

- `diagnostic-tests`: the `test_parameter` requirement is renamed and
  extended to the score, Wald and likelihood-ratio types; the Wald deferral
  and the lmtest pointer are replaced.
- `fit-class-hierarchy`: the verdict table gains `nobs` and `confint` rows
  and records `test_parameter`'s three types, `coef`/`vcov` are stated flat on every class, the
  single-process `coef_layout` refusal is withdrawn, and the container's
  `summary()` is a classed list.
- `flavored-processes`: the sectioned multi-process result orders its
  processes flavor-major in fid order, rendered through the shared renderer,
  with the family-major re-keying removed; `coef()`/`vcov()` are flat with a
  `process =` block selector.
- `model-specification`: the flavored specification print renders its
  processes through the shared renderer with fids shown; the plain
  single-process print is unchanged.
- `multivariate-specification`: the joint specification print shows a
  Dependent block per layer and its process sections come from the shared
  renderer; the `coef_layout` requirement extends to every specification
  and fit, states the flat naming rule, and opens `set_parameters()` and
  `goldfishParams` to every specification kind and every consumer.
- `compact-term-strings`: the compact summary print renders its prose via
  cli while the coefficient table stays on `printCoefmat()`; the exported
  term lookup is `coef_layout(pattern = )`, `model_terms()` is gone.

## Impact

- **Code**: `R/methods_display.R` (all print methods; `summary.goldfishFlavFit`;
  `print_joint_flavor`, `print_flavor_processes` retired),
  `R/preprocess_flavored.R` (the family-major re-key block removed),
  `R/methods_postestimate.R` (`flavored_row_order()` retired, flat
  `coef`/`vcov`, `nobs`, `confint`), `R/joint_parameters.R` (`coef_layout`
  on every object, `coef_name` column, `set_parameters()` on any spec),
  `R/make_specification.R` (`process_map` at construction),
  `R/model_terms.R` (removed), both engines (`layer` on the fit),
  `R/test_gof.R`/`R/test_time.R`/`R/test_parameter.R`/`R/diagnostic_tables.R`
  (drop the row-order call), `R/test_parameter.R` (types),
  `R/set_opt.R`, `R/formula_parser.R`, `R/estimation_core.R`,
  `R/cpp_interface.R`, `R/model_estimate.R`, `R/model_preprocess.R`,
  `R/model_preprocess_group.R`, `R/preprocess_joint.R`, `R/data_source.R`,
  the DyNAM/DyNAMi choice effect files (base conditions), `R/zzz.R`
  (`cli_inform` startup, no theme), `tests/testthat/helper-cli.R` (new),
  every printing snapshot under `tests/testthat/_snaps/`.
- **Docs**: `README.Rmd` and the `teaching1`, `teaching2`, `two-mode`,
  `dynami-example` vignette sources show `summary()` output and are re-knit
  (`vignettes/rebuild-all.R`); `pkgdown-update` task 28 inherits the result.
- **Numbers**: none move. Fid renumbering on the flavored container
  permutes which block is which and the flat `coef()` reshapes how the
  blocks are returned; every block's coefficients are unchanged and the
  frozen 1e-6 baselines must PASS.
- **Dependencies**: none added. `lmtest` stays in Suggests for the `lrtest`
  test. The manynet floor is untouched (ADR-0047); `snet_verbosity` is read
  by name through `getOption()`.
- **Sequencing**: lands before `per-family-flavor-modeling` (its task 2.2
  becomes "render the asymmetric container through the shared renderer")
  and before `identifiability-diagnostics` task 5.2 and `abmcem` task 5.3,
  whose printers render through it. `effect-term-registry` task 1.5's
  `stop()` inventory shrinks to the deprecated path. Branch:
  `feature_simulation`; a `NEWS.d/` fragment, no Version bump.
- **ADRs**: ADR-0051 (renderer; its body is reconciled to Option B and its
  first four open questions are answered by this change), ADR-0070 (own cli
  calls under the stocnet verbosity gate, closing ADR-0042's deferral),
  ADR-0071 (flat `coef()`/`vcov()` on every fit, one `coef_layout()`, one
  `goldfishParams` input), ADR-0072 (nested comparison as
  `test_parameter()` types; supersedes ADR-0037),
  ADR-0050 (`glance()` container shape, confirmed per process), ADR-0037
  (`test_nested()` ships), ADR-0052 (verdicts live in the capability spec),
  ADR-0046 (no new stubs on the retired class), ADR-0016 (labels renamed
  only where false, no wording sweep), ADR-0013/0007 (message versus abort
  asymmetry kept).
