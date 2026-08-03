## Why

The `residuals-gof` diagnostic surface shipped in 1.9.23, and applying it end to
end on a realistic workflow (`.plan/sp/diagnostic_apply.qmd`) found that it
breaks on two shapes the package itself produces and documents. A fit with a
`start_time` — the remedy `diagnose_onset()` recommends — cannot be passed to
`augment()`, `diagnose_outliers()` or `diagnose_changepoints()` at all. A
flavored fit — the shape `teaching2.Rmd.orig` and the Fisheries Treaties example
use — has no diagnostic surface beyond five methods, and three of the gaps are
silent `NULL`s rather than errors.

Both are user-visible defects in the surface 2.0.0 is shipping, and the flavored
gaps are already **non-conformant with a requirement in the living spec**
(`diagnostic-tests/spec.md:356-384`). Investigating them surfaced four further
correctness bugs in the observation window and the formula parser, including one
that silently zeroes an effect: `trans(history = "consecutive")` accumulates
nothing across a `start_time` burn-in, which is exactly the combination
`set_preprocessing()` advertises `start_time` for.

## What Changes

### Phase 1 — the observation window

- Fix `stocnet_dependent_events()` to filter to the resolved estimation window,
  so `fit$dependent_events` holds the events the fit actually modeled. Today it
  holds every event of the focal layer, so `augment()` aborts on any windowed
  fit and any pairing of `dependent_events` with a per-interval vector is
  mis-paired.
- **BREAKING (numerically):** fix the `event_order` counter so it does not drift
  across the burn-in. A pre-`start_time` dependent row increments only the total
  counter, so the order difference gains 2 per network change instead of 1, and
  `compute_update_two_path_consecutive()`'s `lastEventOrder == event_order - 1L`
  test never fires. Any `history = "consecutive"` model fitted with a
  `start_time` currently burns in as zeros; fixing it moves those coefficients.
- Make the legacy `preprocess_monolith()` stop traversing at `end_time` as the
  recipe loops already do, rather than draining every remaining pointer and
  discarding. **BREAKING (numerically)** for DyNAMi fits with an `end_time`.
- Record the resolved sender/receiver on the final right-censored row instead of
  carrying stale metadata from the out-of-window event that triggered the stop.
- Correct `set_preprocessing()`'s documentation, which states the opposite of
  what the code does ("won't stop at this time and will continue processing
  events after this time").

### Phase 2 — parser and guards

- Normalize the degenerate `terms()` shape in `get_rhs_names()`. When the RHS has
  no non-offset term, `attr(., "factors")` is a zero-length vector rather than a
  0-row matrix, so `nrow()` raises `invalid 'length' argument`. This affects
  `~ 1`, `~ offset(x)`, `~ 1 + offset(x)` and multi-offset formulas alike.
- Add a free-parameter floor with a cli abort: a model whose free parameter count
  is zero cannot be estimated. An intercept-only model stays estimable where the
  intercept is a baseline rate (`rate`, exact-time REM) and aborts where a lone
  intercept cancels in the normalization (`choice`, `choice_coordination`).
- `residuals(type = "cox_snell")`: correct the abort message, which asserts "a
  multinomial likelihood has none" for `choice_coordination`, whose normalizer is
  `"coordination"`. Settle whether DyNAMi `rate` — which passes the guard today,
  untested — is intended.
- `"conditional_scores"` requested on a multinomial family emits a cli **message**
  naming the identity (`event_scores` already *are* the conditional rows) and the
  `set_algorithm_newton()` adjustment, instead of the current silence.
- `evaluate_model(return = "conditional_scores")` on a multinomial fit follows the
  same policy as its sibling `return = "exposure"`, which aborts rather than
  handing back nothing under a name that was asked for.
- `risk_set_axis()` on a flavored container aborts naming the missing component
  and the remedy, instead of returning `NULL` silently.

### Phase 3 — interval and event accounting

- **BREAKING (numerically):** compute BIC and AICc from the number of dependent
  events rather than the number of likelihood intervals. `logLik()` reports
  `nobs` as `object$n_events`, a field that despite its name holds the interval
  count, so a windowed effect — which opens a right-censored interval per event —
  roughly doubles it. Measured on the `social_evolution` rate fixture: an
  unwindowed model reports `nobs = 439`, the same model plus one windowed term
  reports `nobs = 876` on the same 439 events, moving the BIC penalty by 2.76 at
  four parameters and 3.45 at five. Any `AIC()` / `BIC()` comparison of a
  windowed model against an unwindowed one is currently uneven, and biased
  against the window. AIC itself is unaffected, having no `n`.
- Store both counts honestly: `n_events` becomes the dependent event count and a
  new `n_intervals` carries what `n_events` holds today. Six consumers read the
  field as though it meant events and every one is wrong — BIC, AICc, `glance()`,
  `logLik(avgPerEvent = TRUE)` (off by the censoring ratio, an argument named
  per-event dividing by intervals), and the counts `margin_table()` and
  `test_parameter()` render as "events". Three others already recompute the right
  number for themselves, which is the evidence that the field rather than the
  readers is the defect. Every consumer then reads `n_events`, including the
  Grambsch-Therneau scaling: the residual it scales is the conditional score,
  which is undefined on an interval realizing no alternative, so its constant is
  the event count too.
- **BREAKING:** make every per-interval residual type return one value per
  dependent event, by accumulating the intervals between consecutive events
  rather than reporting the pieces. This is the same correction the Cox–Snell
  residual needs — the time-rescaling quantity is the compensator between
  events, and window closures split it — applied uniformly, so `residuals()`
  has one length on every sub-model.
- **BREAKING (numerically):** read `test_gof()`'s cumulative score process on the
  accumulated basis. This is a correctness fix rather than a smoothing: the
  standardizing constant is an outer-product variance estimate, valid only for
  uncorrelated increments, and the pieces of one event's span are positively
  correlated — a dependent interval and the closure its windowed effect opens are
  two views of the same tie. Measured scale ratios of 1.03–1.17 on a windowed
  rate fit show the per-interval constant understating the variance, which
  inflates the standardized path and makes the test **anti-conservative** on
  every fit carrying right-censored intervals. Sub-models with no censored
  intervals are unaffected.
- Make `augment()` return one row per dependent event, matching `residuals()`,
  with a column giving how many likelihood intervals were accumulated into each
  span. The `NA` rows on `.fitted` / `.resid` disappear with the intervals that
  produced them.
- Retire `include_censored` on `diagnose_outliers()` and
  `diagnose_changepoints()`. It exists to suppress the alternation of dependent
  and right-censored intervals, and aggregation removes the alternation itself.
  Dropping the censored rows discards their contribution — measured at 372 of
  score mass on a windowed rate fit, against a total that should be zero — where
  accumulating attributes it. Follow the r-lib:lifecycle skill.

### Phase 4 — flavored diagnostics

- Add the missing `diagnose_outliers()`, `diagnose_changepoints()` and
  `diagnose_onset()` flavored methods. These are required by a shipped
  requirement and are absent, so this is conformance work.
- `augment()` on a container row-binds per process with `flavor` and `family`
  columns appended, following the shape `margin_table()` established.
- `residuals()`, `fitted()`, `predict()` and `evaluate_model()` on a container
  return a list named by process label, and gain `flavor =` to return the
  ordinary single-fit shape for one process.
- Replace the five open-coded per-method flavor/family appends with one shared
  helper, and use `flavored_row_order()` everywhere so `model_terms()` and
  `margin_table()` stop disagreeing with the `test_*` family on row order.
- Record explicitly that `flavor` and `family` are **not** defining columns:
  making them defining would demote a subsetted table to a plain tibble and
  autograph would stop dispatching. Today that is an accident of
  `new_diagnostic_list()` having no `defining` parameter at all.

### Phase 5 — reading a large model

- `margin_table()` gains a `dispersion` column: the variance of an actor's
  stratified waiting-time residuals, one under the model, `NA` where the family
  defines no waiting time. The existing columns are the count and the sum of
  those residuals, so the table reports level only; an actor whose events are
  correctly counted but clustered in time is calibrated on every column it
  carries today.
- `residuals(type = "cox_snell", level = "actor")` returns those stratified
  residuals, each actor's final span marked censored — which is what makes the
  set a survival object rather than a re-drawing of the unstratified Q-Q.
- Per-term diagnostics stay readable on a model with many terms, with **no
  interactive step**: a ranked screening table on the returned object, the
  existing effect selection, and page-wise rendering whose page count is known
  before anything is drawn. Fits are produced in batch on a cluster, so a route
  that assumes someone looks at a grid before choosing what to plot does not
  exist for the case that needs it most.
- A plot that draws only its highest-ranked terms reports how many it omitted.

### Phase 6 — decisions and milestone

- Settle ADR-0010 (a caller-supplied time transform must be a function, not a
  vector, because each process of a flavored fit has its own clock), the
  `diagnose_*` sub-question of ADR-0008, and the flatness-flag placement
  sub-question of ADR-0003.
- Bump `DESCRIPTION` and `NEWS.md` at each phase milestone.

**Explicitly out of scope:** the vectorized `start_time` initialization. A fast
path is sound only for `history = "pooled"` effects with no window, and windowed
effects would need state over `[start_time - window, start_time)` plus the
still-pending expiry rows rather than a point state. It is deferred to its own
change with its own benchmark evidence.

## Capabilities

### New Capabilities

- `observation-window`: the semantics of the resolved `[start_time, end_time)`
  estimation window — how pre-`start_time` events fold into the initial
  statistics, that per-event ordering state stays continuous across that fold,
  where traversal stops, which sub-models close the trailing interval, and the
  contract that a fit's recorded dependent events are the ones it modeled.

### Modified Capabilities

- `residual-methods`: flavored container dispatch gains a named-list return and a
  `flavor =` selector; `augment()` row-binds with identity columns; the
  `cox_snell` abort names the correct likelihood family per normalizer; every
  per-interval residual type returns one value per dependent event.
- `diagnostic-plot-classes`: `include_censored` is retired from the two
  describers, which read a per-event series with no alternation to suppress. The
  per-term diagnostics gain a batch-usable screening/selection/pagination
  contract.
- `diagnostic-primitives`: `margin_table()` gains the `dispersion` column.
- `diagnostic-primitives`: requesting `"conditional_scores"` off the exact-time
  families informs rather than staying silent, and `evaluate_model()` treats it
  as it treats `"exposure"`.
- `diagnostic-tests`: the flavored mapping requirement gains a row-order rule and
  states that the identity columns are not defining columns.
- `diagnostic-object-contract`: a diagnostic reaching for a component a flavored
  container does not carry aborts naming it, rather than returning `NULL`; and
  the fit names its two counts — likelihood intervals and dependent events — so
  the information criteria and the Schoenfeld scaling each read the one they
  mean.
- `offset-fixed-terms`: a model needs at least one free parameter, and an
  intercept counts as one only where the sub-model's normalization identifies it.
- `preprocessing-controls`: `start_time` / `end_time` documentation corrected and
  pointed at the `observation-window` semantics.

## Impact

**R code.** `R/legacy_wrappers.R` (`stocnet_dependent_events`),
`R/model_estimate.R` (the call site, diagnostics forwarding),
`R/model_preprocess.R` (`event_order`, the monolith's traversal stop, final-row
metadata), `R/formula_parser.R` (`get_rhs_names`, `parse_formula`),
`R/formula_validate.R` (free-parameter floor), `R/methods_residuals.R`
(`cox_snell` message, flavored dispatch), `R/methods_predict.R`,
`R/methods_display.R` (`augment`), `R/class_diagnostics.R` (three flavored
methods), `R/diagnostic_tables.R` (shared helper, `new_diagnostic_list`),
`R/model_evaluate.R`, `R/model_spec.R` (`risk_set_axis`), `R/set_opt.R` (docs and
the informing message).

**No C++ changes**, so the `cpp-recompile` skill is not needed.

**Reported values beyond coefficients.** The BIC/AICc correction changes a
*reported* number on every windowed rate and REM fit without changing any
coefficient, and the residual aggregation changes the *length* of what
`residuals()` returns on those fits. Neither is caught by a coefficient
baseline, so both need their own regression tests. The applied walkthrough
`.plan/sp/diagnostic_apply.qmd` and the shipped diagnostics vignette both
consume per-interval lengths and will need revisiting.

**Baselines.** Two fixes move coefficients by design — the `event_order` drift
for `history = "consecutive"` with a `start_time`, and the monolith's traversal
stop for DyNAMi with an `end_time`. The frozen 1e-6 baselines in
`tests/testthat/_baselines/` must be checked for exposure to either combination
before any implementation task lands; if one moves, it was pinning wrong numbers,
and regenerating it is a deliberate, separately-recorded step.

**autograph.** No change required. Its plot methods already facet on
`flavor`/`family` when the columns are present, which is why they are appended
rather than prepended and why they must not become defining columns.

**Decision record.** ADR-0010 moves to accepted with an inverted option
preference; ADR-0003 and ADR-0008 gain a settled sub-question each and stay
proposed. One new ADR revisits the `conditional_scores` silence.
