## Context

Two shapes the package produces and documents have no working diagnostic
surface. A `start_time` fit — the remedy `diagnose_onset()` recommends in its own
`@section Remedies` — aborts in `augment()` and the two describers built on it. A
flavored fit — how `teaching2.Rmd.orig` and the Fisheries Treaties example are
written — carries only nine methods, three of the gaps failing as silent `NULL`.

Both were found by applying the shipped surface end to end rather than by
testing it, which is also how the four adjacent bugs surfaced. The relevant
current state, verified against the working tree:

- `stocnet_dependent_events()` (`R/legacy_wrappers.R:534-551`) selects every tie
  row of the focal layer with `!is.na(time)` and applies no window filter. The
  result is stored at `R/model_estimate.R:2641` under a comment calling it "the
  modeled dependent events".
- The burn-in is a full sequential replay; its only specialization is a two-line
  branch redirecting updates into `initial_stats`
  (`R/model_preprocess.R:915-917`, `:1757-1762`).
- `event_order` is passed as `i_total_events - i_dependent_events`
  (`R/model_preprocess.R:853`). In-window a dependent row bumps both counters;
  pre-`start_time` it bumps only the total, because the routing at `:718`/`:739`
  leaves pre-start dependent rows falling through both branches.
- The recipe loops `break` at `end_time` (`:982`, `:1826`);
  `preprocess_monolith()` does not, draining pointers to exhaustion.
- `attr(terms(y ~ 1), "factors")` is `integer(0)`, not a 0-row matrix, so
  `nrow()` at `R/formula_parser.R:1225` raises `invalid 'length' argument`.
- The `cox_snell` guard keys off the risk-set `normalizer`
  (`R/methods_residuals.R:276-280`), which is correct; only its message is wrong.
- `diagnostic-tests/spec.md:356-384` already requires every `test_*` **and**
  `diagnose_*` to map over a flavored fit's processes, with `diagnose_outliers()`
  as a named scenario. `residual-methods/spec.md:85-97` says the residual family
  "SHALL NOT require the flavored container". The two are in tension and this
  change resolves it.

## Goals / Non-Goals

**Goals:**

- A fit's recorded dependent events are the events it modeled, so every consumer
  pairs correctly with the per-interval vectors.
- Per-event ordering state is continuous across the burn-in fold, so an
  order-dependent effect means the same thing with and without a `start_time`.
- One traversal-stop rule across both preprocessing paths, and documentation that
  matches it.
- A degenerate formula produces a cli abort naming the problem, never an internal
  `nrow()` failure.
- Every diagnostic reaches a flavored fit, in a shape the user can predict, with
  the single-fit shape one argument away.
- Absence that is an identity is announced once; absence that is a gap aborts.

**Non-Goals:**

- The vectorized `start_time` initialization. Deferred with its reasons recorded
  in D3.
- Changing which sub-models close the trailing interval. That is a property of
  the likelihood, not a defect (D5).
- The cross-package generic-ownership question of ADR-0008; only its `diagnose_*`
  sub-question is settled here.
- Any C++ change. Every fix is in R.

## Decisions

### D1 — The window filter belongs at the source, not at `augment()`

`augment()` is where the failure is *observed*, so patching it there is the
tempting fix, and it is the wrong layer: it would leave `fit$dependent_events`
mis-paired for every other consumer while making the loud symptom disappear. The
filter goes inside `stocnet_dependent_events()`, which needs the resolved window
passed in from the call site at `R/model_estimate.R:2641`.

The window must be the **resolved** one, not the user's raw argument — the
resolution at `R/model_preprocess.R:425-448` substitutes `events_min`/`events_max`
when an argument is `NULL`, and computes the span over non-window streams only.
The resolved bounds are already stored on the fit as `start_time` / `end_time`.

The boundary convention is read off the loop rather than assumed. The walk admits
an event once `next_event_time >= startTime` (`:704-706`), and stops at
`next_event_time > endTime` (`:698`), so the window is `[start_time, end_time]`
on the closing side for the streams that store a final row. The acceptance test
is not a row count but row identity: the filtered `time` column must equal
`event_time[!right_censored_events]` exactly, since equal counts would pass even
if the wrong rows were kept.

*Alternative rejected:* recomputing the modeled events from `event_time` inside
each consumer. It duplicates the rule per call site and loses the sender/receiver
columns that only the tie table carries.

### D2 — `event_order` counts events, not routed events

The drift exists because `event_order` is derived as a difference of two counters
whose increments diverge exactly when a dependent row is not routed. The fix
keeps the derivation but makes the pre-start dependent row increment
`i_dependent_events` as an in-window one does, so the difference advances by one
per network change on both sides of the fold.

This is preferable to special-casing the burn-in inside the effect functions:
`compute_update_two_path_consecutive()` asks a question about adjacency in the
event stream, and the burn-in *is* part of that stream. An effect should not have
to know whether the observation window has opened.

The test that pins it is a two-way equality: a model fitted with `start_time = t`
and one fitted on data truncated so that `t` is the first event must agree on the
consecutive-closure statistic wherever the histories coincide.

### D3 — The burn-in stays a sequential replay

Recorded here because the alternative is attractive and the reasons it fails are
not obvious. A vectorized fold from a materialized state at `start_time` is sound
only when an effect's cache is a pure function of that state. Measured against the
code:

- `history = "pooled"` qualifies, and the closed form already exists —
  `cache <- unname(network %*% network)` at
  `R/functions_effects_DyNAM_choice.R:1040-1049`.
- `history = "sequential"` does not: on a creation only `inSender` is updated and
  `outReceiver` deliberately is not (`:881-908`), so the count depends on tie
  arrival order and is invisible in the final adjacency. Deletions decrement, so
  it is not even monotone in the creations.
- `history = "consecutive"` does not: it tests immediate adjacency in the stream
  and has no closed form at all.
- **Windowed effects do not, for a reason that is easy to miss.** Window expiry is
  not a runtime promise — `create_windowed_events()`
  (`R/formula_parser.R:869-885`) materializes dissolve pseudo-events at parse
  time. A skip would therefore have to synthesize the still-pending expiries over
  `[start_time - window, start_time)`, i.e. materialize the derived layer across
  an interval rather than at a point.

So the eligible set is "pooled and unwindowed", the fallback is per-formula rather
than per-effect (one ineligible effect forces the whole walk), and the payoff is
narrower than it first appears. `R/materialize_state.R:13-17` already names itself
as the seam for this work. Deferred to its own change, where it can carry a
benchmark and a bitwise-equality proof against the replay.

### D4 — One traversal-stop rule, and the docs follow the code

`preprocess_monolith()` changes to `break` as the recipe loops do.

**Measured 2026-08-04, and this decision's premise was wrong on two counts.**
The monolith is *not* the DyNAMi path, and the change is not numeric.

Every concrete specification class — `dynam_*`, `rem_*` and `dynami_*` alike —
now has its own `preprocess()` method, so the `preprocess.model_spec()` fallback
that reaches `preprocess_monolith()` has no live caller. The DyNAMi path is
`preprocess_interaction()` in `R/model_preprocess_group.R`. The `break` is
therefore correct and consistent, and reaches nothing today.

The DyNAMi path has a different defect, and a worse one.
`preprocess_interaction()` refuses a window outright
(`R/model_preprocess_group.R:100-107`, "DyNAMi doesn't support setting the
endTime parameter"), but `preprocess.dynami_rate_spec()` and its siblings absorb
`startTime`/`endTime` into `...` and never forward them, so the guard is
unreachable and the argument is **silently ignored**. Measured on the RFID
fixture: an `end_time` at the median of the interaction span produces
preprocessed output identical to no `end_time` at all, with the fit reporting
the last event as its end time and storing rows past the requested boundary. No
error, no warning, no message.

That is the same family as D5 — an `end_time` that does nothing — reached by a
different road, and it is **deferred to its own change** rather than fixed here.
The remedy is small (forward the two arguments so the existing `stop()` fires,
turning the silent no-op into an abort), but it changes the behavior of a
released estimator for anyone passing the argument today, and it belongs with
the DyNAMi boundary work rather than inside a preprocessing-semantics change.
Until then the `start_time`/`end_time` documentation says DyNAMi ignores them,
so the documented contract stays true.

`set_opt.R:703-704` currently documents the discarded behavior ("won't stop at
this time and will continue processing events after this time"). The
documentation is corrected to the code, not the reverse.

The final right-censored row also stops carrying the sender/receiver of the
out-of-window event that triggered the stop (`R/model_preprocess.R:741-750`).
That metadata is stale rather than wrong for the likelihood, but it appears in
`augment()` output, so a reader would take it for a real event.

### D5 — The trailing interval is closed where the likelihood has a compensator

**Measured, and worse than it first appears.** An `end_time` beyond the last
event in the schedule is currently a *complete no-op*. The loop creates the
closing interval only in the branch it enters on encountering an event with
`next_event_time > endTime` (`R/model_preprocess.R:698-701`); when the schedule
exhausts first, that branch never runs and no trailing row is emitted. On the
`social_evolution` rate fixture, `end_time` at +1, +6 and +30 days past the last
call all produce byte-identical results — same 439 intervals, same
`logLik = -6048.530`, same `Intercept = -14.19247` — as setting no `end_time` at
all. Thirty days of exposure time silently leave the likelihood, biasing the
baseline rate high.

So the fix is not documentation. The walk closes the window when it runs out of
events, not only when it steps past the boundary.

**Which sub-models store the row is still a likelihood property.** `rate` and
exact-time REM have a compensator, so the trailing exposure is a genuine
likelihood contribution and must be stored. The multinomial families have none —
a censored row there would contribute exactly zero while changing interval
counts, `augment()` row counts and every per-interval diagnostic — so they
continue to store nothing.

That asymmetry breaks the shipped scenario "the clock spans the observation
window" (`diagnostic-object-contract`), which reads as universal and is currently
false for *both* families whenever `end_time` exceeds the last event. It becomes
normalizer-aware: the accumulated intervals reach `end_time` on the censoring
sub-models, and reach the last dependent event on the multinomial ones, where the
documentation states the difference and why.

*Alternative rejected:* aborting on an `end_time` past the last event. A window
in which nothing happened at the end is real information about the rate, not a
user error.

### D6 — Parse the degenerate shape, then judge it

Two defects are entangled in the same crash and are fixed at two layers.

The **shape** is normalized in `get_rhs_names()` immediately after the `factors`
attribute is read, so a formula with no non-offset term parses instead of failing
on `nrow()`. Lines 1236-1252 already tolerate zero terms; only the `row_to_rhs`
construction needs it.

The **verdict** is a parameter floor. The existing "A model without effects
cannot be estimated" check (`R/formula_parser.R:55-58`) is not reused: it is
unreachable behind the crash, it runs before `parse_intercept()`, and it counts
offset terms as effects, so it would pass every case in question. It is replaced
by a cli abort naming the reason.

**Revised 2026-08-04, on two counts, after implementation measured what the
original placement and threshold cost.**

*The floor is a **parameter** floor, not a free-parameter one.* An all-fixed
model is not a degenerate fit: it is a likelihood evaluated at given values, and
goldfish relies on it — `pse_reference()` takes the process-state evaluators'
1e-10 reference from exactly such a fit, and `test-fixed_spec.R` asserts that an
all-fixed model yields a contract covering every coefficient. An offset-only
formula *is* an all-fixed model, so aborting it while accepting
`fixed_parameters = c(1, 2, 3)` would give fixedness two meanings depending on
where it came from — the second-source-of-truth problem D7 exists to avoid,
reintroduced one field over. Only a formula yielding **no coefficient at all**
aborts.

*The floor runs before preprocessing, not in `assemble_fixed_parameters()`.* The
original placement was chosen so the fixed set would already be known; the
narrowed floor does not need the fixed set, only the parameter count and the
normalizer, both of which are known at parse time. It has to move anyway,
because the abort must fire before the preprocessing builders do — see D23, which
records why the crash they raise is not fixed here.

### D7 — An intercept-only model aborts everywhere, for two different reasons

**Revised 2026-08-04; see ADR-0011, which carries the full reasoning and the
map of what supporting it would take.**

As originally written, `~ 1` was a legitimate baseline on `rate` and exact-time
REM and aborted only on the multinomial families. The first half is true of the
statistics and false of the code: making it estimate takes eight zero-effect
tolerances through the recipe builders, which is out of scope here (D23). So
`~ 1` aborts on every sub-model — but the two aborts carry **different
messages**, because they are different facts:

```
choice / coordination   the intercept identifies nothing        about the likelihood
rate / exact-time REM   goldfish carries no effect-free model   about goldfish
```

On the multinomial families a constant statistic cancels in the normalization,
so the intercept carries no information; that holds regardless of implementation.
On the exact-time families the model is perfectly well-defined — it estimates to
`Intercept = -2.5123` on `dataTest`, reconciling with the fixture's exposure —
and we are declining to fit it. Telling a rate user their intercept "identifies
nothing" would be false and would cost them an afternoon looking for a
statistical mistake that is not there.

The predicate separating them is the risk-set `normalizer` already carried on
`model_spec`, which is the same field `is_exact_time_fit()` reads. Deriving it
from `sub_model` strings a second time would be a second source of truth for one
fact.

**Measured 2026-08-04, and the descriptor earns its keep on the ordinal
sub-models.** An ordinal likelihood normalizes over the risk set, so a constant
intercept cancels there exactly as it does in a multinomial choice — an
intercept-only ordinal model is impossible, not merely unsupported. Both
`rate_ordered` variants are stamped `multinomial` and so already take the
identification branch:

| model · sub_model | normalizer | branch |
|---|---|---|
| DyNAM · rate, REM · rate | poisson | unsupported |
| DyNAM · rate_ordered, REM · rate_ordered | multinomial | identifies nothing |
| DyNAM · choice | multinomial | identifies nothing |
| DyNAM · choice_coordination | coordination | identifies nothing |

The single-source-of-truth argument above is the reason to read the descriptor;
this is the payoff. A name-based predicate would plausibly have been written
`sub_model %in% c("choice", "choice_coordination")` and would have put **both**
ordinal sub-models on the wrong message.

### D23 — The zero-effect crash chain is out of scope, and is recorded rather than fixed

`~ 1` does not fail in one place. Normalizing the `terms()` shape makes it
*parse*; making it *estimate* took eight sites, seven of them the same root
cause — `unlist()`, `ifelse()` and `rownames()` returning `NULL` where a
zero-length vector was assumed:

| # | site | what breaks with zero effects |
|---|---|---|
| 1 | `formula_parser.R` `get_rhs_names()` | `attr(terms(), "factors")` is `integer(0)`, no `nrow()` |
| 2 | `utils.R` `get_data_objects()` | `ifelse()` over an empty condition yields logical; `strsplit()` rejects it |
| 3 | `preprocess_builders.R` | `rownames(NULL)` against `character(0)` |
| 4 | `preprocess_builders.R` | scalar `stat_kind` in a zero-row `data.frame()` |
| 5 | `preprocess_builders.R` `augment_interactions()` | `unlist(list())` is `NULL`, so `!is_main` errors |
| 6-7 | `utils.R` `GetDetailPrint()` | `max()` of nothing; `matrix(NULL, ...)` |
| 8 | `model_preprocess.R` `run_dyad_recipe_loop()` | `array(NULL, ...)` seeding the dyad statistics |

The eight were found by successive crashes, which is evidence about the eight
and not about the pipeline: nothing here establishes there is no ninth. The
honest acceptance test for supporting effect-free models is not "does `~ 1` fit"
but "does every recipe builder have an exercised zero-effect path", and that is
a change of its own with its own coverage story — not something to land in the
phase billed as this change's small one.

So the tolerances are **reverted rather than kept as defensive code**. Keeping
them behind an abort would leave them unreachable and untested while reading to
the next maintainer as "zero effects is handled", which is exactly the belief
that would make the ninth site a surprise. The table above is the record, and it
is duplicated in ADR-0011 because this change will be archived.

### D24 — A diagnostic undefined without free parameters aborts; the rest keep working

Measured on an all-fixed choice fit, the diagnostic surface is **already almost
right**, which narrows this to a gap rather than a policy:

| behavior | surfaces |
|---|---|
| already aborts cleanly | `residuals()` for `dfbeta`, `dfbetas`, `cooks`, `scaled_schoenfeld` ("the information matrix of this fit cannot be inverted"); `test_gof()` and `test_time()` ("no free coefficient to test"); `diagnose_onset()` |
| defined, and keeps working | `logLik()`, `AIC()`, `BIC()`, `glance()`, `fitted()`, `augment()`, `margin_table()`, `residuals()` for `score` and `deviance`, `diagnose_outliers()` |
| **gap** | `vcov()` reaches `solve()` and fails with `'a' is 0-diml` |
| **gap** | `print(summary())` emits three `max(nchar(...))` "no non-missing arguments" warnings over an empty coefficient table |

The boundary is whether the quantity is a function of the free parameters, not
whether it is "advanced": the log-likelihood needs none, the inverse information
needs at least one. Only the two gaps are in scope; the surfaces that already
abort are left exactly as they are.

**Measured 2026-08-04 with the statistics supplied, and the answer is that all
three work.** The first probe's aborts were an artifact of the fit carrying no
preprocessed statistics, not of the fixedness. Handed them through
`preprocessed =`, `evaluate_model()` returns its log-likelihood and information,
`predict()` returns its per-event list, and `test_parameter()` — the interesting
one — returns a full score test:

```
Score test of 2 coefficients held at an imposed value, over 12 events.
Joint: LM = 8.752 on 2 degrees of freedom
  inrt   imposed  0.5   score 0.359   statistic 0.0612
  rec    imposed -0.2   score 2.84    statistic 3.94
```

So `test_parameter()` is not merely defined on an all-fixed fit, it is the
diagnostic that fit exists for: every coefficient held at an imposed value is
the pure case of the question it asks. That is the strongest argument yet that
an all-fixed model is an evaluation rather than a degenerate fit — the package
already ships a first-class diagnostic whose subject is exactly this shape.
None of the three needs an abort.

### D8 — Absence that is an identity is announced once

The living spec requires silence when `"conditional_scores"` is requested off the
exact-time families, on the grounds that their `event_scores` rows *are* the
conditional rows, "so the absence reads as an identity rather than as a gap"
(`diagnostic-primitives/spec.md:198-205`). The fact is right; the inference is
not. Silence does not make a user read the absence as an identity — it makes them
not read it at all, and they keep passing an argument that does nothing.

A cli **message**, not a warning: the condition is informational, it fires per
estimate call and so would be noisy in a loop, and a warning would trip
`options(warn = 2)` and surface in R CMD check for any example that requests the
full primitive set. The message names the identity and the
`set_algorithm_newton()` adjustment.

This revisits a settled requirement, so it carries a MODIFIED delta and a new
ADR rather than being folded in silently. Recorded as **ADR-0013**
(`decisions/ADR-0013-an-identity-is-announced-once.md`), which sets out why the
spec's reasoning contains a hidden step — it moves from "the absence *is* an
identity" to "the user will read the absence as an identity", and nothing
supports the second.

*Alternative rejected:* messaging only when `"conditional_scores"` was the sole
requested primitive. More precise, but the rule is subtle to document, hard to
test, and rewards the case a user is least likely to hit.

### D9 — `evaluate_model()` treats the two undefined returns alike

`return = "exposure"` on a multinomial fit aborts, with the reason stated in
`abort_if_exposure_undefined()` (`R/model_evaluate.R:261-285`): handing back
nothing under a name that was asked for would be a lie. `return =
"conditional_scores"` on the same fit returns `NULL`. These are the same case and
get the same policy — abort — which also keeps `evaluate_model()` distinct from
the estimation-time primitive request, where the same name is a no-op with a
message. The asymmetry is deliberate: asking the evaluator *for* a value is a
demand, requesting a primitive is a preference.

### D10 — Flavored dispatch splits on whether the return is tidy

The two spec requirements are in tension because they were written for different
return shapes, and the resolution follows the shape rather than picking a winner.

- **Tidy returns** (`augment()`, and the `test_*` / `margin_table` family that
  already does this) row-bind per process with `flavor` and `family` **appended**,
  so term columns stay positionally stable between a single-process fit and a
  multi-process one, and so autograph facets on the columns rather than needing a
  flavored plot method.
- **Non-tidy returns** (`residuals()`, `fitted()`, `predict()`,
  `evaluate_model()`) cannot carry a column. On a container they return a list
  named by process label, and gain `flavor =` to return the ordinary single-fit
  shape for one process. The list is the honest default — it neither invents a
  shape nor pretends the container is one fit — and `flavor =` keeps the familiar
  shape one argument away.

`residual-methods/spec.md:85-97`'s "SHALL NOT require the flavored container" is
preserved in substance: the per-process semantics are unchanged and each fid's
result still answers exactly as a standalone fit would. What changes is that the
container is no longer a dead end.

*Alternative rejected:* requiring `flavor =` and aborting without it. It
guarantees one output shape, but makes the common "look at everything" case an
explicit `lapply` over an internal list the user has to know about.

### D11 — The identity columns are not defining columns

`flavor` and `family` stay non-defining, and this is now a decision rather than
an accident of `new_diagnostic_list()` having no `defining` parameter.

Making them defining has real appeal — a table that has silently collapsed
several processes into one is misleading. It is rejected because the demotion
rule (`diagnostic-plot-classes/spec.md:277-282`) strips the class when a defining
column is lost, and autograph dispatches on class. A user subsetting columns for
a table would silently lose their plot method. The failure mode we would be
adding is worse than the one we would be catching.

### D12 — Reaching for a missing component aborts

`risk_set_axis()` returns `NULL` on a container because it is a plain function
reading `model_spec`, which a container does not carry. That violates
`diagnostic-object-contract/spec.md:99-103`, which requires aborting and naming
the missing component and the remedy. It aborts, naming the `$results` route.

### D13 — The `cox_snell` guard is right; its message is not

The guard keys off the risk-set `normalizer`, which is the correct predicate and
stays. The message asserts "a multinomial likelihood has none", which is false for
`choice_coordination` (`normalizer = "coordination"`, the mutual `getLikelihoodMM`
product). The message becomes normalizer-aware.

DyNAMi `rate` is stamped `normalizer = "poisson"` and so passes the guard today
with no test behind it. It shares the DyNAM-rate event contribution
(`R/estimation_core.R:907-908`), so the arithmetic is the same family; the task is
to add the test and let it pass, or to block it with a reason. The decision is
deferred to the measurement, not to taste.

**Measured 2026-08-04 on the RFID fixture: the guard passes, and the residuals
are arithmetically correct.**

```
residuals == intervals * total_rate     exact
sum(residuals) = 234.0000001            the dependent-event count, 234
                                        -- the time intercept's score equation
all finite, all >= 0
```

So the guard is right to admit DyNAMi `rate`, and the test is added rather than
a block. The measurement surfaced something the task did not ask about, though,
and it is worth recording before someone reads a Q-Q plot of these:

```
127 of 234 intervals have dt == 0       54% tied event times
  -> 127 residuals are exactly 0
the remaining 107 have mean 2.19        a unit exponential would be 1
```

The compensator identity survives ties — the zeros and the inflated remainder
cancel exactly — but the *distributional* reading does not, and that reading is
the whole purpose of the type: the Q-Q plot against a unit exponential is what
makes it a goodness-of-fit check. A user plotting these would see a point mass
at zero and read gross misspecification, when the cause is timestamp resolution.

**Not a DyNAMi defect, and not fixed here.** Any exact-time fit with tied times
behaves this way; DyNAMi is where it is worst, group joins and leaves being
recorded at coarse timestamps. It belongs to the post-release `tied-event-times`
change, whose proposal motivates itself on `Fisheries_Treaties_6070` at **17%**
ties — this fixture is at **54%** and is the sharper case for it to cite.

### D14 — Two fixes move numbers, and that is verified before it is accepted

D2, D4 and D5 change coefficients for specific combinations. The policy is:
establish the blast radius **first** — which frozen baselines in
`tests/testthat/_baselines/` use `history = "consecutive"` with a `start_time`,
DyNAMi with an `end_time`, or a censoring sub-model with an `end_time` past the
last event — before any of the three fixes is written.

**Measured 2026-08-04, and the answer was none of them.** No test estimates with
`history = "consecutive"` at all — it appears once, inside a name-formatting
call. No test combines DyNAMi with an `end_time`. Every test that sets an
`end_time` sets it *inside* the event stream, so the loop always meets an
out-of-window event and takes the branch that already works; the defect lives
past the stream's end, where nothing goes. And `baselines_model_grid()` carries
no window, no `start_time`, no `end_time`, no `consecutive` and no DyNAMi, with
no `control_prep` in the builder — so the frozen coefficient baselines cannot
move under any of the three.

Two consequences follow, and the second reverses a step this decision had
planned.

The reassuring one: nothing needs regenerating, and the fixes can land without
touching the 1e-6 floor.

The uncomfortable one: **zero coverage is why these bugs were reachable**, and it
means the characterization-test step this policy called for has nothing to
characterize. Pinning current behavior would assert the buggy values — a
`consecutive` statistic of zeros, an `end_time` that does nothing — for the one
commit before the fix inverts them, putting wrong expectations in the suite to
document a bug that `progress.md` and the commit message already document. That
step is therefore dropped, and its burden moves onto the regression tests, which
gain two obligations they did not have: each must be **written against the
unfixed code and confirmed to fail**, since with no before-baseline a passing
test proves nothing; and each fix adds a control for the ordinary case its own
absence of coverage left unguarded — `consecutive` estimated without a
`start_time`, and `end_time` set inside the stream. A baseline that moves was pinning wrong numbers, and
regenerating it is a deliberate step recorded with its reason, never a silent
side effect of a task. If neither combination is covered, that is itself worth
recording: it means the bugs were reachable precisely because nothing pinned them.

### D16 — One field lies, and almost every reader believes it

`n_events` on a fitted object is set to `length(is_dependent)`
(`R/cpp_interface.R:95`) — the number of likelihood **intervals**. The name says
events. On the multinomial families the two coincide, which is why it survived;
on a rate or REM fit they diverge.

**Measured 2026-08-04 (task 7.1), and the trigger is broader than a windowed
effect.** A right-censored row is written for *any* non-dependent event falling
inside the window with a positive interval, on a sub-model that keeps a
right-censoring consumer. Three sources, of very different size:

| source | on `social_evolution`, 439 events |
|---|---|
| windowed effect (dissolve pseudo-events) | 876 intervals — roughly a factor of two |
| **exogenous event stream** (e.g. `indeg(friendship)`) | 441 intervals |
| estimation-window boundary row | +1 |

The exogenous case matters more than its size suggests, and it is the one an
earlier reading of this decision missed: a two-interval discrepancy looks like
nothing, and silently shifts BIC and AICc on a fit no one would think to check.
It also widens the documentation blast radius from one vignette fit to four —
`teaching1` carries `indeg(friendship)` from `mod01Rate` onward.

```
fit$n_events  =  876          the interval count, under an events name
truth         =  876 intervals, 439 dependent events, 437 right-censored
```

The readers split in two directions, and an earlier draft of this decision saw
only one of them. **Some readers wanted the interval count and were reading
`n_events` correctly under its old meaning** — they break on the rename and must
move to `n_intervals`, which is the opposite repair from the one below. Found by
the suite (task 7.2): `nrow(event_scores)`, `length(total_rate)` and the
per-event probability totals are all per *interval*, as is the whole R-backend
evaluation loop, whose `nEvents` sizes every buffer and drives
`for (i in seq_len(nEvents))` — changing that one would silently evaluate 439 of
441 intervals.

So the rename is not "one field, six wrong readers". It is one field carrying
two meanings, and every reader has to be asked which it wanted.

Six consumers read the field as though it meant events, and every one of them is
wrong:

| reader | reads | should read |
|---|---|---|
| `logLik()` `nobs` → **BIC** | 876 | 439 |
| **AICc** denominator (`R/methods_display.R:152`) | 876 | 439 |
| `glance()` `nobs` | 876 | 439 |
| `logLik(avgPerEvent = TRUE)` | −6.459 | −12.889 |
| `margin_table()` print, "N events" | 876 | 439 |
| flavored `margin_table()` context (`R/diagnostic_tables.R:535-537`) | 876 | 439 |

**Corrected 2026-08-04 (task 7.1), after verifying each site rather than
trusting this table.** `test_parameter()` is **not** a wrong reader — it already
recomputes at `R/test_parameter.R:166` and `:415`, and belongs in the list
below rather than here. The flavored `margin_table()` context, a second site in
the same file reading `x$results[[key]]$n_events` per process, **is** one and
was missing. The count of wrong readers is unchanged at six; the membership is
not.

`logLik(avgPerEvent = TRUE)` is the clearest: an argument named per-event
dividing by intervals, off by exactly the censoring ratio.

The other consumers already read the right number, each by recomputing
`sum(!x$right_censored_events)` for itself — the Grambsch-Therneau scaling in
`scaled_schoenfeld_rows()`, and the printed contexts of `diagnose_onset()`,
`test_gof()`, `test_time()` and (per the correction above) `test_parameter()`. That the workaround was invented independently
three times is the evidence that the field, not the readers, is the defect.

**Both consumers want the event count**, which is the correction to an earlier
draft of this decision. It had claimed the Grambsch-Therneau constant wanted the
interval count, on the grounds that every interval contributes a likelihood term.
That is wrong, and the reason is worth recording because it is not obvious: the
residual GT scales is the **conditional** score
`s_k = x_obs(k) − sum_j pi_j x_j`, which carries no exposure term, does not sum
to zero, and is undefined on a right-censored interval — `pi_j` needs a realized
alternative. It is the Cox partial-likelihood residual, where inter-event times
never enter the score, and GT's constant there is the number of events. The
implementation already had this right; only the decision was wrong.

So the fix is a rename plus a redirect, not a per-consumer negotiation:

```
n_events     ->  the number of dependent events        (was: intervals)
n_intervals  ->  the number of likelihood intervals    (new; length(right_censored_events))
```

Every method then reads `n_events`, the three ad-hoc recomputations collapse onto
the stored field, and `n_intervals` remains available for the printed contexts
that legitimately report both.

BIC and AICc taking the event count is also the standard survival convention —
information accrues with events, not with exposure records — so the choice is not
merely a goldfish convenience. The comparability argument and the statistical one
agree.

*Alternative rejected:* leaving BIC as it is and documenting the caveat. A
reported number comparable only within a subset of models is a trap, and the
package's own teaching vignette walks into it: `AIC(mod03Rate, mod04Rate)`
compares exactly an unwindowed model against a windowed one.

### D17 — A residual is per event, not per interval

Every per-interval residual type accumulates the intervals between consecutive
dependent events and returns one value per event. The Cox–Snell case forced the
question — the time-rescaling quantity is the compensator *between events*, and
window closures split it — but the correction generalizes, and applying it only
there would leave `residuals()` with one length for one type and another for the
rest.

Aggregation is total-preserving, and measured on a windowed rate fit, 876
intervals over 439 events:

```
score column sums, which are zero at the maximum
  all intervals      0.000119     the score equation
  dependent only        372       what dropping the censored rows costs
  aggregated         0.000119     identical to all-intervals
```

**Corrected 2026-08-04: that table settles a different question than it looks
like it settles, and an earlier draft leaned on it too hard.** It compares
accumulating against *dropping* the censored rows, and on that comparison it is
decisive — 372 against 0.000119 is the whole case for not dropping them. It says
nothing about accumulating against *keeping* the per-interval series, because
both preserve every total. `residuals-gof.md` §2.2 makes the point exactly:

> The total identity `sum_k r_k = n` holds under **both** readings, because the
> sum telescopes — which is why no existing test discriminates between them.

So total-preservation is not the reason to accumulate. The reason is that the
quantity the theory defines *is* the span quantity, and the per-interval series
holds its pieces:

| type | what the theory defines | what a per-interval row holds |
|---|---|---|
| `cox_snell` | `r_k = int_{t_{k-1}}^{t_k} lambda`, unit exponential | one addend of that integral |
| `deviance` | `D_k = -2 log f_k`, `f_k` carrying the **waiting-time density** over the span | one addend of `log f_k` |
| `score` (via `test_gof`) | increments at distinct events, which are martingale differences and so uncorrelated | pieces within a span, which are positively correlated |

The discriminating measurement is the **mean**, not the total. On a 30-minute
windowed rate fit of the calls data:

```
cox_snell mean, dependent rows (the pieces)   0.7127
cox_snell mean, accumulated (the span)        1.0000     <- Exp(1)
cox_snell sum, either reading                 439.00     <- telescopes
```

Exp(1) has mean 1. The pieces do not have it and the span does, exactly.

Not every type aggregates, and the distinction follows from where each is
defined:

| type | today | under this decision |
|---|---|---|
| `deviance`, `score`, `cox_snell` | per interval | accumulated between events |
| `schoenfeld`, `scaled_schoenfeld` | `NA` on censored intervals | already per event; drop the `NA` rows |
| `dfbeta`, `dfbetas` | per interval | accumulate the score, then transform — the map is linear, so it commutes |
| `cooks` | per interval | compute **from** the accumulated score, never accumulate the scalar: the quadratic form does not commute, and the question is the influence of the whole event |
| `response`, `martingale` | per event / per actor | unchanged |

The trailing span is the one genuinely open edge: exposure with no event to
attach to, because Cox-Snell groups intervals by **waiting time** — the span
from event `k-1` to event `k` — so anything after the last event belongs to no
event at all.

**Settled 2026-08-04 (Alvaro): a censored final observation.** `residuals()`
returns `n + 1` values there, the last flagged as censored. That is the standard
survival treatment and the honest shape for a Q-Q plot, which already
understands censoring. The cost is accepted knowingly: this type's length is no
longer `n_events`, so D17's headline holds "one value per dependent event, plus
a censored remainder where the window outlives the last event" rather than
unconditionally.

The two alternatives were **measured and rejected**, not weighed on taste:

| option | consequence on the `social_evolution` windowed fit |
|---|---|
| attach to the last event | its residual goes `0.0335` -> `1.36`, a 40x inflation showing as an outlier in exactly the plot the type exists for, and meaning exposure *after* the event rather than before it |
| drop it | the accumulated total falls from `439.0000` — precisely the event count, which is the compensator identity — to `437.67` |

**When it arises is also corrected.** This decision assumed the span appears only
when `end_time` runs past the last event. Measured, it appears whenever any
non-dependent event follows the last dependent one, and the sources separate
cleanly:

```
window only, no exogenous     876 intervals / 439 events    trailing = 0
exogenous only, no window     441 / 439                     trailing = 1
both                          880 / 439                     trailing = 3
neither                       439 / 439                     trailing = 0
```

**Windowed effects never produce a trailing row**, which was worth checking
rather than assuming: their dissolve pseudo-events are bounded by the
observation window, so all 437 of them fall before the last event. Only
exogenous streams and `end_time` reach past it. The `both` row is the window
rule working rather than leaking — `end_time` resolves over the *non-window*
streams, so adding friendship extends the window to the last friendship event,
admitting call-window dissolves that were previously outside it.

### D18 — `include_censored` retires because its problem is gone

The argument exists to suppress the alternation of dependent and right-censored
intervals, which makes a segmented series describe the censoring pattern rather
than the fit. Under D17 the series has no alternation to suppress: it is one
value per event.

The existing requirement is careful and its reasoning survives — it is the
*mechanism* that is superseded. Worth recording that the default was a decent
approximation of the right answer rather than a wrong one: on the same fixture
the dependent-only log-likelihood series has median 25.6 and IQR 7.7 against the
accumulated series' 26.1 and 8.0. It discarded the censored contribution where
accumulation attributes it, and for the log-likelihood series that difference is
small. For the score series it is not, which is why the argument was already
scoped away from `test_gof()` and `diagnose_onset()`.

Two simplifications follow and are the reason to prefer this over keeping the
argument:

- The returned object no longer needs the "one row per interval under either
  setting, candidacy read from the `NA` pattern on `.resid`" construction. One
  row per event, all rows candidates.
- `diagnose_onset()` stops being a special case. It currently has to explain why
  it reads all intervals while reporting on a dependent-event axis; under D17
  those are the same axis.

Retirement follows the lifecycle skill — the argument is deprecated with a
warning rather than removed, since it ships in 1.9.23.

### D19 — Accumulation corrects `test_gof()`, it does not merely smooth it

Measured, on a windowed rate fit with 876 intervals over 439 events, reproducing
`gof_processes()` on both bases:

| effect | T interval | T accumulated | scale ratio | p interval | p accumulated |
|---|---|---|---|---|---|
| Intercept | 1.2856 | 1.2694 | 1.03 | 0.0734 | 0.0797 |
| indeg | 0.3532 | 0.3383 | 1.09 | 0.9996 | 0.9998 |
| outdeg | 0.9997 | 0.9745 | 1.05 | 0.2704 | 0.2983 |
| indeg [30m] | 0.7593 | 0.7020 | 1.17 | 0.6116 | 0.7079 |
| outdeg [30m] | 0.5599 | 0.5352 | 1.09 | 0.9126 | 0.9369 |

Every statistic falls and every p-value rises, and two mechanisms push the same
way. The supremum is taken over a coarser grid, so it can only shrink; and the
standardizing scale — the root of the summed squared per-row contributions —
*grows* on every effect.

That growing scale is the substantive finding. `scale` is an outer-product
variance estimate, and it is only a valid one for **uncorrelated** increments. A
ratio above one says the within-span pieces are positively correlated, which they
must be: a dependent event and the window closure it opens are two views of the
same tie, so their score contributions point the same way. The per-interval
denominator therefore **understates** the variance of the cumulative process,
which inflates the standardized path.

So `test_gof()` is currently **anti-conservative on any fit carrying
right-censored intervals** — it treats correlated pieces of one event's
contribution as independent martingale increments. Accumulation restores the
unit the theory assumes: score contributions at distinct events are martingale
differences and are uncorrelated, spans between events are not. The 3–17%
variance understatement here moved no conclusion, but it is the direction that
over-rejects, and a borderline effect would flip the wrong way.

The scope is narrow and worth stating: only the sub-models that store
right-censored intervals are affected. A choice, ordinal or coordination fit has
none, so its per-interval and accumulated bases coincide and its statistic is
unchanged.

The code comment at `gof_processes()` asserting that the statistic "does not
depend on whether intervals or events are counted" is correct about the `n` in
`sqrt(n · J_d)` cancelling, and does not speak to aggregation. It is worth
amending so the two claims are not confused.

*Consequence for the reference:* the analytic Kolmogorov band assumes
proportional information accrual, and the accumulated process is closer to that
than a path that alternates event and closure steps. This is a second, weaker
argument in the same direction; the variance argument is the one that carries.

### D21 — The margin reports level; a shape column reports what it cannot

`margin_table()` gives each actor an observed count and an expected count. Those
are exactly the count and the sum of that actor's stratified waiting-time
residuals, so the whole table is a **first-moment** view. An actor whose events
are correctly counted but clustered in time is calibrated on every column it
carries.

```
  well-timed   r = (1.0, 1.0, 1.0, 1.0)     sum 4, n 4    margin ok, shape ok
  bursty       r = (0.01, 0.02, 0.01, 3.96) sum 4, n 4    margin ok, shape wrong
```

So `margin_table()` gains a `dispersion` column — the variance of the actor's
stratified residuals, one under the model — following the convention
`expected_count` already established: always present, `NA` where the family
defines no waiting time. It is the cheapest possible expression of the idea,
because it adds a column to a table users already read rather than a second
object they must learn to join.

The per-actor curves this replaces were the obvious design and are the wrong one.
Measured on the calls fixture: 84 actors, but only **34 ever send**, the median
sender has **5 events**, and 13 senders with 10 or more events carry 84% of the
data. A Kaplan-Meier curve per actor is therefore 34 curves of which about 13
could support a reading — unreadable as a plot and misleading as a promise. One
number per actor, read against its own event count, says the same thing without
inviting a distributional interpretation the data cannot bear.

The plot that follows is a scatter of level against shape — `observed − expected`
on one axis, `dispersion` on the other, sized by event count — where the two
diagnostics become two axes and each quadrant is a distinct misfit. That belongs
to autograph and is not specified here beyond the column it consumes.

### D22 — Screening is data, not a step; pagination is for batch

A model with many terms breaks a one-panel-per-term figure, and the obvious
remedy — look at the grid, then select — assumes a human between the fit and the
figure. Fits go to a cluster, so that assumption fails exactly where the problem
is worst.

The decision is therefore to make each route work with no interactive step at
all:

```
   fit on HPC ──▶ object ──┬─▶ ranked table       already in $effects; order it
                           │   (survives having no screen)
                           ├─▶ effects = "..."    already exists
                           └─▶ plot(page = k)     new; n_pages known up front
                                                  loop writes every page to file
```

**Screening is a property of the returned object, not an interaction.** The
per-term statistic is already in `$effects`; what it needs is a defined order so
a script can take the front of it without anyone looking. This is the route that
degrades best: a table remains legible at fifty terms long after a grid stops
being.

**Pagination is enumerable or it is useless in batch.** The page count must be
derivable without rendering, so a loop can write every page; a method that only
knows it is on the last page once it gets there cannot be scripted. `ggforce` is
already in autograph's Imports and `facet_wrap_paginate()` is the idiom, so this
costs a dependency nobody has to add.

**A reduced figure must say so.** `plot.result.goldfish` already ranks and keeps
four. Silently drawing four of fifty-six is the same failure as the Hampel window
reporting no outliers because it could not see any — the output looks like an
answer. Whatever is dropped is named.

*Alternative rejected:* an interactive `devAskNewPage()` walk, which is what base
`plot.cox.zph` does. It is the wrong shape for the case that motivated this.

### D15 — Three ADR sub-questions settle here

- **ADR-0010** settles fully, and inverts its own recorded preference. It favored
  a numeric vector (Option B) over a function (Option C) for a caller-supplied
  time transform. The flavored `test_time()` method establishes that each process
  has its own preprocessed object and its own interval count, so a length-`n`
  vector provably cannot serve a container while a function can. C-first, and the
  feature stays unplanned.
- **ADR-0008**'s `diagnose_*` sub-question settles: adding three flavored
  `diagnose_*` methods enlarges the generic surface it calls "the same situation
  one release earlier and therefore the cheaper one to get right first". The
  cross-package ownership question stays open.
- **ADR-0003**'s flatness-flag placement settles as an output-shape question this
  change is already answering for flavored tables. Its constraint is honored: no
  test added here asserts a p-value on the flavored fixture, only
  container-versus-standalone equality.

**ADR-0005 is deliberately not touched.** It carries an explicit note that its
remaining scoping question should not be resolved inside an implementation
session.

### D20 — `augment()` follows, and reports what it accumulated

`augment()` returns one row per **dependent event**, matching `residuals()`, and
carries a column giving how many likelihood intervals were accumulated into that
event's span. The alternative — leaving it per interval — breaks the join the two
describers depend on, and a `level =` argument on both would double the surface
every consumer has to reason about for a distinction that has one right answer.

The span count is not decoration. It is the only place the interval structure
remains visible once every other surface is per event, and it is what lets a
reader see that a fit with windows accumulated roughly two intervals per event
while an unwindowed one accumulated one. It also makes the two counts of D16
reconstructable from the augmented table alone.

Consequences that follow rather than needing their own decisions: the describers
stop reading candidacy from the `NA` pattern, and the augmented table becomes
joinable with the dependent-events table of D1 by position, which it currently is
not on a windowed fit.

**Corrected 2026-08-04 (task 7.7): the two halves of this decision conflicted
once the trailing span was settled.** "One row per dependent event" and "aligns
row-for-row with `residuals()`" stopped agreeing when D17 settled the trailing
span as a **censored final observation** — `residuals()` returns `n + 1` values
where a span outlives the last event, so a strictly `n`-row table would not
align.

The alignment requirement wins, and not on taste: letting the two disagree would
reproduce exactly the mis-pairing D1 opens this change by fixing — a table whose
rows do not correspond to the per-event vectors beside it. So `augment()` carries
the censored remainder as one final row, flagged in the existing
`right_censored_event` column, which becomes meaningful again rather than
vestigial: at most one row is ever `TRUE`.

It follows that `.fitted` and `.resid` do **not** lose their `NA` rows entirely,
as this decision first said. They lose the interleaved ones — one per censored
interval, which is what made the table unreadable — and keep exactly one where a
remainder exists, for the row that realizes no outcome. Measured:

```
windowed only     439 rows   439 events   876 intervals   sum(n_intervals)=876   0 censored rows
with exogenous    440 rows   439 events   441 intervals   sum(n_intervals)=441   1 censored row
```

`.fitted` is now the span's density contribution and `.resid` its deviance —
which is to say exactly the literature's `f_k` and `D_k`, defined over the
waiting time rather than over one stored interval.

## Open Questions

**Does the intercept-only abort belong on `rate_ordered`?** Its normalizer is not
Poisson, so D7's identification wording does not apply as written, but an ordinal
likelihood with only an intercept has nothing to rank either. Unmeasured; carried
in ADR-0011.

**~~What happens to the trailing span?~~ Settled 2026-08-04:** a censored final
observation; see D17, which carries the measurements that rejected the two
alternatives and the confirmation that windowed effects do not create the span
at all.

**~~Is the `cox_snell` guard right for an ordinal REM?~~ Answered 2026-08-04:
there is no ordinal REM stamped Poisson, and the question had a false premise.**
It read the message `estimate_rem()` emits on an intercept-free rate formula as
a statement that the fit *is* ordinal. It is not: the text says "a time
intercept has been added" and points at `rate_ordered` for the ordinal
alternative, and the code adds the intercept
(`R/model_estimate.R:1755-1763`), setting `has_intercept <- TRUE`. Measured:

```
estimate_rem(~ inertia, sub_model = "rate")          coef: Intercept, inrt
                                                     normalizer: poisson
                                                     is_exact_time_fit: TRUE

estimate_rem(~ inertia, sub_model = "rate_ordered")  coef: inrt
                                                     normalizer: multinomial
                                                     is_exact_time_fit: FALSE
```

So the fit really is exact-time with a baseline hazard, the Poisson stamp is
correct, and `cox_snell` is right to compute on it. The only route to an ordinal
likelihood is `sub_model = "rate_ordered"`, which is stamped `multinomial` and
which the guard already excludes. **Task 5.2 loses this half** and keeps only
its DyNAMi `rate` question.

## Risks / Trade-offs

**Closing the trailing interval changes every rate fit that sets an `end_time`**
→ the change is a correction, not a regression: those fits were estimated with
part of their exposure missing. The blast-radius check of D14 covers this case
too, and the NEWS entry states the direction (baseline rates fall).

**A frozen 1e-6 baseline moves and the regression floor is weakened** → D14 makes
the blast-radius check a prerequisite task rather than a discovery during
implementation. Regeneration is separately committed with its reason, so the diff
shows a decision rather than a drift.

**The `event_order` fix changes an effect's meaning without users noticing** →
`history = "consecutive"` under a `start_time` currently yields all zeros, so
anyone relying on it has a coefficient estimated from a degenerate statistic.
A NEWS entry states the combination and the direction, and the two-way equality
test pins it.

**Making the monolith break changes DyNAMi results** → DyNAMi's own tests are the
guard; if none covers `end_time`, one is added before the change, so the fix is
measured rather than assumed harmless.

**The flavored list return is a third output shape to learn** → mitigated by
`flavor =` giving the familiar shape, by the list being named with the same
process labels `print()` already shows, and by the abort messages naming the
argument. It is documented once in `?diagnostic-requirements` rather than per
method.

**Aborting `~ 1` on choice sub-models rejects something that used to "work"** →
it did not work; it crashed with an internal error. The abort is strictly more
informative.

**Scope.** Four phases in one change is large, and the phases are separable if it
proves too much: phase 1 is the release blocker, phase 3 is the spec-conformance
work, phases 2 and 4 are small. The phase milestones are the natural split points
if the change has to be cut.
