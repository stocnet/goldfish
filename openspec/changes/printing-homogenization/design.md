## Context

Measured on `feature_simulation` at 1.9.31 (2026-09-14), after
`fit-class-hierarchy`, `class-naming-scheme` and `model-spec-descriptor`
folded and `preprocess-one-walk` made the merged walk the only walk.

**Three multi-process printers, one vocabulary in the data.** ADR-0051's
table still holds: `print.goldfishSpec` (flavored) delegates to
`print_flavor_processes()` (`cli_dl`, flavor → family, no fid, Dependent
block shown); `print.goldfishJointSpec` delegates to `print_joint_flavor()`
(`cli_bullets`, layer → flavor → family, `[fid n, separable]`, Dependent
block dropped); `print.goldfishFlavFit` renders flavor → family headers with
`print.default()` estimate blocks, no fid, no standard errors. Every data
method — `coef()`, `vcov()`, `tidy()`, `model_terms()`, the `test_*` and
`diagnose_*` fan-outs — names a process with `render_process_label()`
(`calls › creation › rate`). None of the three printers prints that string.

**The ordering split is a deliberate re-key, not an accident.** The joint
planner's `build_joint_process_map()` already assigns fids
specification-major, then flavor, then family, so a process's rate and
choice rows are contiguous — flavor-major. `preprocess_flavored()` then
re-keys the merged outputs "under the family-major fid scheme the flavored
container has always exposed" (its own comment), so a flavored container's
fids 1..4 are rate, rate, choice, choice. `flavored_row_order()` in
`R/methods_postestimate.R` exists only to walk that family-major map
flavor-major again for `coef()`, `vcov()`, `summary()`, the diagnostics and
`print()`. `coef_layout()` and `goldfishParams` walk the map in fid order.
Removing the re-key makes the map flavor-major, `flavored_row_order()` the
identity, and every surface agree.

**The container summary is a bare list.** `summary.goldfishFlavFit` returns
`flavored_component_apply(object, flavor, summary, "summary")`, a named list
of `goldfishSummFit` objects; the comment says printing the list "reaches
the existing print method per process rather than needing a summary class
of its own". The result is R's `$`-headed list printing wrapping four base-R
summaries. `tests/testthat/test-flavored_summaries.R` pins only the list
type, the names and the `flavor =` unwrap, all of which a classed list keeps.

**Two console idioms.** Thirteen print methods render via cli with
`cli_rule(left = "{.cls goldfishX}")` as the header in nine of them and
`cli_h1`/`cli_h3` only in `print_data_goldfish_list()`. Ten render via
base R: `print.goldfishBaseFit` and `print.goldfishSummFit` (the only fit
output the README and four vignettes show — 69 `cat()` calls in
`methods_display.R`, zero snapshots), the four deprecated-path classes,
`print.data.goldfish`, `print.goldfishStat`, `print.goldfishAlgoNewton`,
`print.goldfishPrepCtrl`. Conditions: 402 `cli_abort()` beside 135 base
`stop()` (46 in `class_checks.R`, 29 in `make_data.R`, 18 in `set_opt.R`,
10 in `formula_parser.R`, 7 each in `estimation_core.R` and the DyNAM choice
effects, 5 each in `cpp_interface.R` and `model_estimate.R`, 4 in
`model_preprocess.R`, 2 in `model_preprocess_group.R`, 1 each in
`data_source.R` and the DyNAMi choice effects) and 13 `warning()`. No
`message()`. 21 `progress =`/`verbose =` paths `cat()` their status. No cli
theme, no `.onLoad` cli option, no wrapper helper: 34 per-situation
`abort_*`/`warn_*`/`inform_*` helpers each call cli directly. Twelve
`goldfish_*` condition classes exist where tests match by class.
`local_cli_context()` is redefined per test file (`cli.width = 80`,
`cli.num_colors = 1`, sometimes `cli.unicode = FALSE`) in some thirty files,
and `local_reproducible_output()` in twenty more.

**manynet's helpers are exported now.** manynet 2.3.2 exports
`snet_abort`, `snet_warn`, `snet_info`, `snet_minor_info`, `snet_success`,
`snet_prompt`, `snet_unavailable` and four `snet_progress_*`; autograph
imports two and namespace-qualifies the rest, with manynet in `Depends` and
no cli of its own. Each is `cli::cli_*(paste(...), .envir = parent.frame())`
— `paste()` collapses a named bullet vector into one string, there is no
condition class and no `call` argument, and everything except abort, warn
and prompt is silenced unless `options(snet_verbosity = )` is `"normal"` or
`"verbose"`; the documented default is `"quiet"`, and manynet's `.onAttach`
raises it to `"verbose"` for interactive sessions only. The theme that
colors `{.mnet}`/`{.auto}` spans is unexported and set by manynet's
`.onAttach`. ADR-0042 deferred adoption to a proposal and an ADR; the
2026-07-19 `refactor-single-data-object` decision to keep goldfish's own
cli calls rested on the helpers being unexported, which is no longer true.

**Model methods.** `logLik.goldfishBaseFit` carries `df = n_params` and
`nobs = n_events`; the container sums both. That is what makes `AIC()`,
`BIC()` and `lmtest::lrtest()` work — and nothing tests it. No `nobs()`,
`confint()`, `anova()` (ADR-0037 keeps it unregistered) or `test_nested()`
method exists; `tidy.goldfishBaseFit(conf.int = TRUE)` calls
`stats::confint()` and lands in `confint.default`.

Constraints: the frozen 1e-6 baselines and C++ goldens do not move; the
branch writes a `NEWS.d/` fragment and no Version bump (ADR-0040);
verdicts for new fit generics are recorded in the `fit-class-hierarchy`
capability, not a table (ADR-0052); the retired `result.goldfish` class
gains no stub (ADR-0046); printed labels are renamed only where false, not
swept (ADR-0016); goldfish does not diverge from RSiena's first-argument
asymmetry on the `test_*` family (ADR-0044); estimator names are not
renamed (ADR-0043).

## Goals / Non-Goals

**Goals:**
- One renderer for every multi-process surface, so a spec, a joint spec, a
  fitted container and its summary differ in content and never in layout,
  label or order.
- One process order — flavor-major, in fid order — on every surface that
  lists processes, so position and key agree.
- Every user-facing print method on a live path renders via cli with data
  interpolated, under one written convention and one snapshot helper.
- Every live user-facing condition is a cli condition; informational
  output honors the stocnet verbosity option.
- `nobs()`, `confint()`, `AIC()`, `BIC()`, `lmtest::lrtest()`,
  `lmtest::waldtest()` and `test_parameter()`'s three types work on both fit
  classes and are pinned by tests, with verdicts recorded before
  implementation.
- One shape for coefficients on every fit — flat `coef()`/`vcov()`, one
  `coef_layout()`, one `goldfishParams` input — so the R stats base sees one
  model and goldfish's structure lives in the layout, as RSiena does for
  co-evolution.

**Non-Goals:**
- Printing `stocnet` objects — manynet owns `print.stocnet` and
  `print.mnet`; goldfish coerces to them and adds nothing.
- Importing `snet_*()` from manynet, or setting a `cli.theme`.
- Converting the deprecated data-class path (`class_checks.R`,
  `make_data.R`, `print.nodes/network/dependent/data.goldfish`): ADR-0031
  keeps it exempt and it goes at 2.0.0.
- A pooled `glance()` row; an `anova()` method; the DyNES verdicts for
  `nobs`/`confint`/`test_nested` (deferred to `abmcem` with `logLik`'s).
- Any wording sweep of existing cli messages, or renaming the `test_*`
  generics' first arguments.
- Changes to `print.goldfishStat`, `print.goldfishAlgoNewton` and
  `print.goldfishPrepCtrl` beyond the header idiom.
- The shape clause on the fit-class contract (ADR-0051's fifth open
  question) — see Open Questions.

## Decisions

### D1 — One renderer, driven by the object's shape (ADR-0051, Option B)

An internal `render_process_sections(x)` takes the four things every
multi-process surface has — a header line, an ordered list of processes,
each carrying `fid`, `layer`, `flavor`, `family`, its rendered label and a
content block, and a footer — and renders the `cli_rule(left = "{.cls …}")`
header, one section per layer (only when more than one layer is present),
one section per flavor beneath it, one per family beneath that, and the
footer. The section line for a process is the `render_process_label()`
string with the fid appended as data — `{.strong calls › creation › rate}
[fid 1]` — so the printout spells the name `coef()` uses and the key every
data surface is indexed by. A caller supplies its content block as a
zero-argument closure the renderer calls inside the section: the flavored
spec's is its formulas (`Rate`/`Choice`/`Derived`/`Support` as a `cli_dl`,
formula deparsed as data); the joint spec's is the same list plus the
`separable`/`coupled`/`completed` annotation the `multivariate-specification`
capability requires; the fit's is its estimate table on `print.default()`;
the summary's is `stats::printCoefmat()`. Numeric tables stay on base
printers because cli formats prose, not columns.
*Rejected:* three renderers sharing a vocabulary and a pinning test
(ADR-0051 Option C/D) — a convention is what diverged; a renderer
parameterized by content type (`type = c("spec", "joint", "fit")`) — that is
three renderers wearing one name, and the content closure keeps the
renderer ignorant of what it renders.

### D2 — The flavored container adopts the planner's flavor-major fid order

The re-key block in `preprocess_flavored()` that renumbers the merged
outputs family-major is removed; the container exposes the joint planner's
map, minus the `coupled` column, whose fids are already
specification-major, flavor-major, family-minor. `flavored_row_order()` is
then the identity over the map and is retired with its six callers walking
`process_map` directly (ADR-0054: retired in the task that removes the last
caller). `coef_layout_frame()` already walks the map in fid order and needs
no change; `goldfishParams` and `goldfishJointSpec` were already in this
order. A flavored fit's fid 2 was the second flavor's rate and becomes the
first flavor's choice — the one breaking consequence, named in the NEWS
fragment; six test files hard-code a fid and are updated with it.
*Rejected:* renumbering the joint planner family-major to match the
container — moves the vocabulary `goldfishParams`, `set_parameters()` and
`simulate()` already share (ADR-0051's own caution); keeping both orders
and documenting the split — leaves position and key disagreeing, which is
the defect measured.

### D3 — The joint spec regains a Dependent block per layer

Dropping it was never argued; the joint spec holds every member's
`dependent` and the renderer's layer section is where a layer's events,
time span, node sets and network belong. The per-layer block is the plain
spec's Dependent block rendered by the same code, so the two cannot drift.
A join has no single dependent process, so there is no join-level
Dependent block above the layers.

### D4 — The container summary is a classed list rendered by the same renderer

`summary.goldfishFlavFit` keeps returning the named list of `goldfishSummFit`
objects and the `flavor =` unwrap, and stamps the list `goldfishSummFlavFit`
(the D17 short vocabulary: `Summ` + `Flav` + `Fit`). `print.goldfishSummFlavFit`
calls the renderer with the joint header (model, layer, flavor count, one
call), the per-process `printCoefmat()` table plus that process's own
convergence line as the content block, and the joint log-likelihood,
parameter count, AIC and BIC once in the footer. Per-process AIC/BIC pairs
are not printed; they remain on the components. Option B needs a class
here because a list without one cannot dispatch — the class is the
renderer's entry point, not a fourth layout.
*Rejected:* leaving R's list printing — four models with four AICs beside a
`print()` that says one model; a data.frame-shaped summary — the non-tidy
container convention requires the list.

### D5 — `glance()` on a container stays one row per process

The fit statistics are per process and a pooled row would have to invent a
quantity; the joint value lives on `logLik()` and in the summary footer
(ADR-0050's open question, answered as it was left). The existing
`glance.goldfishFlavFit` is pinned by a test on the flavor-major order and
the identity columns; no `by_process =` argument is added.

### D6 — Own cli calls, gated by the stocnet verbosity option (ADR-0070)

goldfish keeps calling `cli::cli_abort()`, `cli_warn()`, `cli_inform()`,
`cli_alert_*()` and `cli_progress_*()` at the call site, with named
bullets and condition classes where a test or a caller matches on them.
Informational output — `cli_alert_info`/`cli_alert_success` and the new
progress steps — is emitted only when `getOption("snet_verbosity",
"quiet")` is `"normal"` or `"verbose"` (progress and minor info at
`"verbose"` only), read by name through a single internal
`console_verbosity()` helper; errors, warnings and the object print
methods are never gated. The existing per-call `progress =` and
`verbose =` arguments keep their meaning — an explicit `progress = TRUE`
shows progress whatever the option, because a user who typed it asked for
it — and render through `cli_progress_step()`. `.onAttach` emits its
banner through `cli::cli_inform(class = "packageStartupMessage")` so
`suppressPackageStartupMessages()` still works. No `cli.theme` is set:
manynet's theme applies when manynet is attached and clobbering it would
recolor a sibling's spans.
*Rejected:* importing `snet_*()` — `paste(...)` collapses the named-bullet
form 402 aborts rely on, adds no `class =`, and reaches the theme only
through `:::`; a `goldfish_verbosity` option — two knobs for one stack; no
gate — the stocnet contract is that `quiet` reports nothing but errors,
warnings and prompts, and goldfish's output should obey the option its
siblings honor.

### D7 — Conversion scope and wording

The 60 `stop()`/`warning()` sites outside `class_checks.R` and
`make_data.R` convert to `cli_abort()`/`cli_warn()` with the argument in
`{.arg}`, the offending value in `{.val}` or `{.cls}`, and the remedy as an
`"i"` bullet where one exists; `call. = FALSE` sites pass `call = NULL`.
Meaning is preserved; wording is corrected only where false or misspelled
("geater", "attibute", "nose set", the `.call = FALSE` argument silently
pasted into a message at `class_checks.R:342` — outside scope but fixed
since it is a bug, not a wording). No new condition classes are minted
speculatively; a class is added only where a test or caller needs to match
it, keeping `goldfish_<snake_case>` per the `class-naming` capability. The
21 verbose `cat()` sites become `cli_progress_step()` (per phase) or
`cli_alert_info()` (per fact) under D6. ADR-0013/0007's asymmetry — an
inert argument aborts at construction, an identity is announced once as a
message — is kept where the sites embody it.
*Rejected:* converting the deprecated path too — 75 sites of code
scheduled for deletion; a mechanical `sed` — the memory rule against
script-editing message text stands, each site is rewritten by hand.

### D8 — The fit printers move to cli, snapshot first

`print.goldfishBaseFit` and `print.goldfishSummFit` are snapshotted as they
print today, under the pinned context, before a line changes; the
conversion then re-records and the diff is reviewed line by line
(`class-naming-scheme` D11: snapshots are reviewed, not accepted). The
header becomes `cli_rule(left = "{.cls goldfishFit}")` with the call as a
`{.code}` line; convergence, score and step norms, parameter count with
fixed count, log-likelihood, AIC/AICc/BIC and the `model`/`sub_model`
provenance render as `cli_text`/`cli_dl` with values interpolated; the
compact legend renders as a `cli_bullets` block instead of
`writeLines(strwrap())`; the coefficient table stays on
`stats::printCoefmat()` and the width probe that fits compact term strings
to the console stays, reading `cli::console_width()`. The `compact =`
argument and both legends are unchanged. README and the four vignette
sources re-knit in the same task group so the docs never show a printer
that no longer exists.

### D9 — Model methods: verdicts first, then one restriction test with three types (ADR-0072)

With D12 in force, `nobs` and `confint` are parent-borne `inherit` and need
no container method: `nobs.goldfishBaseFit` returns the dependent-event
count `logLik()` carries — the method is needed because `nobs.default` reads
`length(residuals())`, which on a fit whose window runs past the last event
is `n_events + 1` (the censored remainder row) — and on the container
`logLik()`'s summed `nobs` is what `nobs()` reads; the comment in
`logLik.goldfishFlavFit` claiming a rate process counts its right-censored
rows is corrected, since the attribute is `n_events` on every process.
`confint.goldfishBaseFit` computes Wald intervals from the flat
`coef()`/`vcov()`, so on a separable container it equals the per-process
intervals and on a coupled fit it uses the joint covariance;
`tidy(conf.int = TRUE)` reaches it on both classes.

Nested comparison does not get its own generic. RSiena has no two-fit entry
point: a restriction `θ_T = θ_T⁰` is tested by `score.Test()` on the
restricted fit and by `Wald.RSiena()`/`Multipar.RSiena()` on the full fit,
and goldfish's `test_parameter()` is already the score column of that
table. It gains `type = c("score", "wald", "lr")` and `null = NULL`:
`"score"` unchanged (restricted fit, `offset()` terms); `"wald"` on the full
fit as the quadratic form of the flat `coef()`/`vcov()` over the terms
`effects =` names, at their offset value or zero; `"lr"` against a `null`
restricted fit with the class, layer, `n_events` and `coef_layout()`-name
subset guards, reporting `2(ℓ_full − ℓ_null)` on the free-parameter
difference, and on a container one statistic per process whose terms differ
plus the total, `process =` narrowing to one. One return class,
`goldfishParamTest`, gains a `type` field so its print and autograph's plot
dispatch stay single. The generic keeps no parent default, so the DyNES
method (`gof-dynes`, bootstrap-adjusted `"lr"`) decides each type itself and
an exact-likelihood `"lr"` can never reach it by inheritance — ADR-0037's
safety argument one level down. `anova()` stays unregistered. `AIC()`,
`BIC()`, `lmtest::lrtest()` and `lmtest::waldtest()` get tests, no methods:
the `logLik` attributes and the flat `coef()`/`vcov()` are the contract,
and the `"lr"`/`"wald"` statistics are pinned equal to lmtest's. The DyNES
cells are deferred to `abmcem` beside `logLik`'s.
*Rejected:* `test_nested()` as its own generic (ADR-0037, superseded by
ADR-0072) — a second name and print for one null; an `anova()` method
(ADR-0037's survey stands); a restriction matrix for the Wald type — no
caller yet, and `effects =` at offset-or-zero is what RSiena's `Multipar`
covers; a data fingerprint for the same-data guard — goldfish has none, and
`n_events` plus the layer name is the guard `lrtest` itself lacks; a
container `confint` returning a list — the list shape is what D12 removes.

### D10 — One pinned cli context, hoisted

`tests/testthat/helper-cli.R` defines the single `local_cli_context()`
(`cli.width = 80`, `cli.num_colors = 1`, `cli.unicode = FALSE`,
`snet_verbosity = "quiet"`) and the per-file copies are deleted; new
snapshots use it. `local_reproducible_output()` call sites are left alone
unless the file is otherwise touched.

### D11 — The console convention is a capability, not a comment

The `console-output` capability records the header idiom, the verbosity
gate, the no-base-conditions rule for live paths, and the pinned-context
rule so the next printer is held to it by a spec requirement rather than
by whoever remembers ADR-0051. `diagnostic-plot-classes` already says "no
raw `cat()` markup" for its own classes; the new capability says it for
the package.

### D12 — Every fit has a flat `coef()` and `vcov()`; the fid names the block (ADR-0071)

`coef.goldfishFlavFit` returns one named numeric vector in fid order and
`vcov.goldfishFlavFit` one matrix — block-diagonal for separable processes,
with zeros stating that each process was estimated as if the others were
exogenous, and dense once a coupled or DyNES fit supplies cross-process
covariance. That replaces the list-shaped return the non-tidy container
convention gave them. Names on any multi-process fit are `f<fid>_<short>`
(`f1_Intercept`, `f1_odeg`, `f2_inertia`): syntactic, no spaces or
punctuation beyond the underscore the short names already use, sorted in
fid order, and derivable from the printout because every rendered section
carries its fid. A single-process fit keeps the bare short names, so the
vignettes' `b[["Intercept"]]` arithmetic is untouched. `coef(fit, process
= )` (and `vcov(fit, process = )`) returns one block with bare names — the
existing `flavor =` selector renamed outright to a process label, with no
lifecycle shim since the container has never been released — so the same
arithmetic works on a container block. `complete = TRUE` includes fixed
slots on both classes. This is what makes `confint.default`, `AIC()`,
`BIC()`, `lmtest::lrtest()`, `lmtest::waldtest()` and `car::vif` work on a
container with no goldfish method, and it is RSiena's shape for
co-evolution: one `theta`, one `covtheta`, the effects frame as layout.
*Rejected:* keeping the list on the flavored container while the joint
contract is flat — two shapes for one concept, and a list cannot hold
cross-process covariance; composite label names (`calls › creation ›
rate: odeg`) — non-syntactic and long, and the same information sits on
`coef_layout()` and in the print; bare short names with no prefix — they
collide across fids; prefixing single-process fits too — consistent, but
breaks every vignette for no gain since one block cannot collide. ADR-0002
lists the container's `coef()` as satisfied and does not bind its shape.

### D13 — `coef_layout()` is the one coefficient table: layout, term lookup and values

`coef_layout()` dispatches on a single-process `goldfishSpec`, a flavored
`goldfishSpec`, a `goldfishJointSpec`, a `goldfishParams`, a `goldfishFit`
and a `goldfishFlavFit`; the single-process fit returns one block, so the
recorded `refuse` verdict for `coef_layout` on `goldfishFit` is withdrawn
(the refuse mechanism keeps no live cell until the DyNES `logLik` cell). It
absorbs `model_terms()`, which is removed outright (a dev-only surface, never
released): `term_table()`'s own comment says "one row per coefficient, every
spelling side by side", the same row unit as the layout, and nothing in `R/`
calls either generic — both were user lookups plus the from-result round
trip. One table therefore carries the process identity (`fid`, `process`,
`sub_model`, `flavor`), the three spellings (`term` compact at infinite
width, `name` = the `coef()` label, `export`), the new `coef_name` (D12),
the effect-detail columns the non-compact summary prints, and `fixed`,
`value`, `se`, `index` where the object has them (`NA` on a raw spec). A
`pattern` argument filters rows case-insensitively across every spelling and
detail column, as `model_terms()` did. Every roxygen pointer that sent users
to `model_terms()` for `effect =`/`effects =` arguments, and the
unrecognized-term abort, now name `coef_layout()`.
*Rejected:* keeping both names as two views of one frame — the duplication
ADR-0045 asked to inspect, with two documented functions returning one
table minus columns; keeping `model_terms()` as the survivor — "terms" reads
oddly on a `goldfishParams`, and the `multivariate-specification` contract
is already written around `coef_layout()`.

### D14 — `set_parameters()` on every specification; `goldfishParams` accepted by every consumer

`set_parameters()` accepts a `goldfishJointSpec`, a flavored `goldfishSpec`
and a single-process `goldfishSpec`: the map it needs is
`build_joint_process_map(list(spec))`, which `preprocess_joint()` already
builds for a single spec, so a specification carries its `process_map`
from construction. The from-result form accepts any fitted result, and a
flat `coef()` vector is accepted by name now that D12 makes the names
collision-free. Every consumer accepts a `goldfishParams`: `estimate_dynes()`
(required), `simulate()` on any specification kind (the process-simulation
change reads it), and `initial_parameters` on `estimate_dynam()`,
`estimate_rem()`, `estimate_dynami()` and the flavored path, where a numeric
vector or the flavor-keyed nested list remains accepted as a convenience
that builds one. A value at a free slot is a warm start (a pinned free);
`NA` is free with no start; `complete` means every free slot carries a
value, which is what a walk with no estimate behind it needs. Fixing a
coefficient stays in the specification — `offset(term, coef = )` or
`set_algorithm_newton(offset_coef = )` — never in the parameter object:
what is fixed defines the model, the object carries numbers.
*Rejected:* a `fixed =` argument on `set_parameters()` — it would let a
parameter object change the model it parameterizes, and ADR-0007's
construction-time abort for an inert argument assumes the formula owns
that; keeping the flavor-keyed list as the flavored path's only input — a
third format for one concept.

### D15 — A separable container prints its log-likelihood, parameter count and AIC; not BIC or AICc

Over separable processes AIC is exactly additive (`Σ(−2ℓ_i + 2k_i) = −2Σℓ +
2Σk`), so the container's AIC is the sum of its processes' and means what
each of them means. BIC is not additive — its penalty is `k·log(n)` and
`log(Σn_i)` is not `log(n_i)` — and AICc is nonlinear in `n` and `k`; the
container's summed `nobs` also mixes counts of different kinds (a rate
process counts censored rows). So `logLik()` on the container keeps its
`df` and `nobs` (the generic `AIC()`/`BIC()` work and are tested), the
renderer's footer prints the joint log-likelihood, the free-parameter
count and AIC with the wording "over k separable processes", and BIC and
AICc are not printed for a container; `glance()` rows carry all three per
process, where a user comparing one flavor's models looks. The joint value
is kept rather than refused because it is the general case: a coupled or
DyNES likelihood does not factorize, and the container's `logLik()` is then
the only value there is.
*Rejected:* refusing `logLik()`/`AIC()` on separable containers — valid
numbers, and nested containers differing in one process give the right LR
statistic and df; printing four AIC/BIC pairs — the shape ADR-0051 measured
as "four models where print says one".

### D16 — A fit stores the name of its dependent layer

Both engines assemble the fit with `model`, `sub_model`, `n_events` and
`n_intervals` but no layer name; the spec path knows it (`spec$focal`) and
the formula path knows the dependent object's name (`work_data$info$focal`).
The fit gains a `layer` field set at assembly from whichever the entry point
has, so `coef_layout.goldfishFit` renders `<layer> › <family>` with the same
`render_process_label()` the container uses, the flat naming has one
source, and the fit printer can state what was modeled. The `@return`
roxygen of `estimate_dynam()` lists the field.
*Rejected:* deriving the label from `x$call` at print time — the call may
name a variable, not a layer.

## Risks / Trade-offs

- [Fid renumbering on flavored containers breaks a user's `fit$results[["2"]]`
  or `coef_layout()` join by fid] → named as **BREAKING** in the NEWS
  fragment; the package has never released a flavored fit; every internal
  consumer reads the map by `(flavor, family)` and is tested against the
  standalone fits to 1e-6.
- [Snapshot churn across every printer masks a real regression] →
  snapshot-first for the base-R printers, and every re-recorded snapshot is
  diffed and reviewed in its task, never accepted wholesale.
- [`per-family-flavor-modeling` rebases its task 2.2 twice] → this change
  lands first; its task becomes a renderer call on an asymmetric map, which
  D1's layer/flavor/family walk already handles (a flavor with one family
  renders one family section).
- [New informational alerts are invisible at the default `quiet` level] →
  intended (the stocnet contract); explicit `progress = TRUE` still shows
  progress, and nothing that was an error or warning changes level.
- [Vignettes knit against the *installed* goldfish, so a re-knit before
  install shows the old printers] → the re-knit task installs first
  (`vignettes/rebuild-all.R` convention).
- [`identifiability-diagnostics` adds summary content after this lands] →
  it adds a footer line through the renderer's footer, one place.
- [The conversion touches `estimation_core.R`/`cpp_interface.R` hot paths]
  → conditions only; no statistic or coefficient computation is edited, and
  the baselines PASS at every commit.
- [Flat `coef()` on the container breaks code that read it as a list
  (`coef(fit)[["calls › creation › rate"]]`)] → named **BREAKING** with the
  fid renumbering; the replacement is `coef(fit, process = "calls › creation
  › rate")`, and the container has never been released.
- [Withdrawing the single-process `coef_layout()` refusal leaves the refuse
  mechanism with no live cell] → `test-fit_class_reachability.R` still
  enforces reach, and `abmcem`'s `logLik` cell is the next live refusal;
  recorded as an open question on ADR-0071.
- [`set_parameters()` on a single-process spec and `initial_parameters`
  accepting `goldfishParams` widen the single estimators' input surface] →
  numeric inputs stay first-class; the object is an additional accepted
  type, validated by the same value gate the joint path uses.

## Migration Plan

Commit per task on `feature_simulation`; `NOT_CRAN=true` green with
baselines PASS at each. Task order: order, flat shape and layer (D2, D12, D13, D16) →
renderer (D1, D3) → container summary (D4, D5, D15) → fit printers and
docs (D8) → console convention and conversion (D6, D7, D10, D11) →
parameters (D14) → model methods (D9) → close.
No archive on the branch; `status: landed (feature_simulation, awaiting
fold)` at the end. Rollback is per commit.

## Open Questions

- Whether the fit-class contract gains a **shape** clause beside its
  verdict (ADR-0051's fifth open question). This change is the printing
  sighting the question came from, not a third; the renderer's content
  contract is where presentation now lives, so the question is whether
  that suffices. Left open in ADR-0051 for the next container method that
  is not a fan-out.
- Whether the `"lr"` type's nestedness guard can be automated fully from
  `coef_layout()` when an effect's arguments differ (carried from
  ADR-0037 into ADR-0072); the first implementation requires the null's
  terms to be a subset by `name` and documents that.
- Whether the Wald type later accepts a contrast matrix, as
  `Wald.RSiena()` does (ADR-0072's open question).
- Whether `coef(fit, process = )` also accepts a fid integer, as the term
  matcher accepts a position (ADR-0071's open question); the first
  implementation accepts the rendered label only.
