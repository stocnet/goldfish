# Design — residuals-gof

## Context

`.plan/sp/residuals-gof.md` is the statistical reference for this change: it
derives every residual formula (deviance, Cox–Snell, Schoenfeld/scaled,
score, martingale, response), the Boschi–Wit cumulative-score bridge test,
the score/LM test, and the per-submodel conditional-residual argument for
DyNAM, and it records the survey of relevent, remstimate, mlogit, and the
Boschi–Wit GOF implementation. Its §0 (added 2026-07-24 after an
agent-verified literature pass; bibliography in `.plan/sp/residuals-gof.bib`)
is the decision layer: margins validity derivations and the authoritative
margins table (REM stores both sender and receiver margins; per-flavor
expected-count formulas; coordination sums to 2n), the scaled-Schoenfeld
scaling correction, the information-clock transform for the bridge tests,
and the verified package landscape (no package ships analytic actor
margins; residuals in the wild are per-event, per-effect, or
simulation-based). Key code facts it establishes:

- All four `default_c` engines (`DyNAM_choice`, `DyNAM_rate`,
  `DyNAM_MM`, `REM`, plus the `_ordered` variants) already compute
  `intervalLogL` and, behind `return_event_scores`, the per-event score
  rows (`D.row(obs) - g` for multinomial engines; full score increments for
  exact-time rate/REM). These are byproducts of the final Newton–Raphson
  iteration — storing them costs no extra compute.
- ~~No `default_c` engine returns a probability matrix (`pMatrix` falls back
  to a "not implemented" string in `cpp_interface.R`); ranks, recall, and
  margins therefore require in-pass computation.~~ **Superseded by
  `backend-parity` (D16/D23):** all nine kernels now return per-event
  probabilities natively, actor-indexed over the whole node set and zero off
  the risk set, and the `"not implemented"` fallback is gone. The conclusion
  still holds and for a better reason: ranks, recall and margins remain in-pass
  computations because reducing them from a stored probability matrix would
  materialize the $O(n|R|)$ object D2 exists to avoid, not because the matrix
  is unavailable.
- `preprocessing_only = TRUE` / `preprocessed =` already exist as the
  preprocessed-object producer/consumer surface.
- `augment.result.goldfish` exists (events + `intervalLogL`);
  `diagnose_outliers()`/`diagnose_changepoints()` return class
  `diagnostic.goldfish`.
- autograph (separate repo, stocnet umbrella) already ships
  `plot.outliers.goldfish` and `plot.changepoints.goldfish` (expecting
  those classes and an `outlier` column with `"YES"` strings — a contract
  mismatch with goldfish's logical column), plus `plot.sienaGOF`,
  `plot.ag_gof`, and the RSiena → autograph pattern this change follows.

Settled with the user (2026-07-17): test_* surface only (no `gof()`
generic); `test_time()` implements both trend and window methods;
goldfish adopts autograph's `outliers.goldfish`/`changepoints.goldfish`
classes; in-sample `predict()` ships now.

**These code facts predate the dispatch/estimation churn.** They were
established 2026-07-17; since then `refactor-single-data-object` archived,
`multimode` (two-mode fits) landed, and `spec-driven-dispatch` (risk-set
descriptor onto the model spec, legacy-vocabulary retirement) is still in
flight — all of which reshape exactly the `modelTypeCall` dispatch,
`make_specification()` fit shape, and engine surface this change routes
through. Task 0.1 re-verifies each fact and refreshes the anchors before any
edit; the dispatch/spec portion re-runs against the landed surface once
`spec-driven-dispatch` archives.

## Goals / Non-Goals

**Goals:**

- Phase 1: diagnostic primitives (`diagnostics =`), in-pass C++ ranks and
  margins, `return_preprocessed`/`preprocessed` plumbing, `evaluate_model()`,
  `residuals()`/`fitted()`/`predict()`/`augment()` methods,
  `diagnose_*` class alignment.
- Phase 2: `test_gof()`, `test_parameter()`, `test_time()`; autograph plot
  methods on `feature/goldfish-diag`; coverage/power verification.
- cli-rendered print methods on every new user-facing object; plotting
  exclusively in autograph.

**Non-Goals:**

- `simulate()`, simulation-based GOF, auxiliary-statistic multiplier
  bootstrap, forecasting `predict()`, DHARMa-style PIT residuals — phase 3,
  after DyNES lands.
- Random-effects models and their GOF (goldfish_latent / TMB territory).
- Time-varying or non-linear effect estimation (only their *detection* via
  `test_time()`).
- A `gof()` S3 generic (would mask `ergm::gof` under the stocnet umbrella).
- Hausman–McFadden IIA helper (documented in the reference; can ride on
  `test_parameter()` infrastructure later without new seams).

## Decisions

### D1 — `diagnostics =` names stored primitives, replacing the three return flags

`set_algorithm_newton(diagnostics = c("loglik", "scores", "ranks", "margins",
"probabilities"))`; `TRUE` ≡ `c("loglik", "scores")`, `"all"` = everything,
`FALSE`/`character(0)` = none. Default `c("loglik", "scores")` (preserves
today's `return_interval_loglik = TRUE` behavior and adds the free scores).
`return_interval_loglik` and `return_probabilities` are soft-deprecated
(lifecycle) with one-to-one mapping — both shipped publicly (CRAN 1.6.x as
camelCase `estimationInit` entries, v1.7.0 as arguments). `return_event_scores`
never shipped (introduced on the development branch after the v1.7.0 tag), so
it is removed outright at 2.0.0 with no lifecycle ceremony — the
deprecation-scope audit lives in `backend-parity` design D11; task 1.9 undoes
the uniform three-flag deprecation task 1.2 had implemented. Rationale: residual
*types* are transformations of stored *primitives*; naming primitives makes
the storage cost legible and avoids false economies (deviance and outcome
probability are the same stored vector). Alternative rejected: tiered
keywords ("basic"/"full") — hides the one decision that matters at scale
(probabilities in or out). Guardrail: requesting `"probabilities"` emits a
cli warning with the estimated size (`n_events × |riskset| × 8` bytes)
before running; no silent thresholds.

### D2 — Ranks and margins are computed in-pass in C++; the probability matrix is never the primary interface

Each engine gains flags to accumulate, inside the event loop:
`observed_rank` (rank of the observed alternative among the risk-set
weights, $O(|R|)$ per event, integer vector) and margins (per-actor
observed vs expected counts as defined in `.plan/sp/residuals-gof.md` §0.2,
the authoritative table): receiver margins $\sum_k \hat p(r|s_k)$ for
choice; **both sender and receiver margins for REM** (exact-time via the
in-/out-degree compensators $\sum_k \Delta t_k \sum_{\cdot}
\hat\lambda_{sr}$ including right-censored intervals — NOT probability
sums, which are the ordinal formula; ordinal via coarsened-softmax sums);
sender margins $\sum_k \Delta t_k \hat\lambda_s$ (exact-time, incl.
right-censored intervals) or probability sums (ordinal) for rate;
per-actor pair margins for coordination (each event credits both members;
totals sum to $2n$, not $n$). Margins accumulate over the same realized
risk set as estimation (`twomode_or_reflexive`, support constraints).
Calibration identities split by flavor: multinomial sums are algebraic
(machine tolerance, any $\theta$); exact-time sums equal $n$ only at the
MLE via the intercept score equation (convergence tolerance; goldfish
force-adds the intercept for exact-time models). Margins are documented as
calibration descriptives, never per-actor tests (plug-in, negatively
correlated across actors; formal route = `test_parameter()` with an
activity/popularity candidate effect). Defaults unchanged: margins are
opt-in; REM keeps per-event/sequence-level diagnostics as the default
surface. Recall@k derives from ranks in R. Rationale: the full probability matrix is
$O(n|R|)$ (≈ 11 GB for a 57k-event, 159-actor REM) while every consumer
needs only these summaries; returning small vectors keeps the C++→R
boundary flat. `"probabilities"` stays available for the choice model and
small risk sets behind the size guardrail; with `backend-parity` landed,
every backend returns it natively (the silent redirect onto the R backend is
removed), so availability is governed by the guardrail alone, not by which
backend ran. cpp-recompile discipline applies to every `src/` edit.

### D3 — `evaluate_model()` is the single shared evaluator

`evaluate_model(x, at = coef(x), return = c("loglik", "score",
"information", "interval_loglik", "event_scores", "ranks", "recall",
"margins", "probabilities"), preprocessed = NULL, backend = <the fit's
recorded backend>)`, dispatched per model/submodel inside the Rcpp entry points
(`estimate_()` for the `cpp` backend, `compute_()` for `gather`) keyed on
the `spec` object — the R-side `modelTypeCall` routing no longer exists
(retired by `spec-driven-dispatch`; see task 0.1 findings). The existing
single-pass closure `evaluate_default_c(pars, need_scores)` in
`cpp_interface.R` (already fed to `estimate_via_maxlik()`) is the substrate
to generalize into `evaluate_model()`. One no-iteration engine pass at `at`.
Consumers:
`residuals()` on-demand types, `test_parameter()` (full model at the
constrained estimate), `test_time(method = "windows")`, `predict()`, and
later DyNES ascent-based Monte Carlo. The backend MUST default to the one
used for estimation, read from the fit's `backend` component
(`backend-parity` D10 records it on every `result.goldfish`; a pre-2.0.0 fit
without the component means unknown — fall back to the default backend, never
error on the absence): diagnostics must be numerically consistent with the
fit they diagnose (1e-6 baseline discipline; backends differ in
accumulation order).

**Reflexive/two-mode flag consistency.** `evaluate_model()` and the new
in-pass C++ quantities (ranks/margins) reconstruct per-event statistics, so
their broadcast fan-out and risk-set iteration MUST key on the unified
`twomode_or_reflexive = allowReflexive || is_two_mode`, exactly as the
estimation engines do — never `is_two_mode` alone. Reading `is_two_mode` by
itself would drop the reflexive diagonal cell for a self-tie-allowing one-mode
model, diverging from the risk set and from the fit. (The pre-existing R-engine
broadcast site that read `is_two_mode` alone was corrected separately; it was
dormant because `allowReflexive` is not yet threaded from the estimation entry,
but the new evaluator must not reintroduce the asymmetry.)

### D4 — coxph storage/recompute split

Store by default what the last iteration produced (`loglik`, `scores`);
recompute everything else on demand through `evaluate_model()`, mirroring
`residuals.coxph`. On-demand types need the statistics replay:
`estimate_*()` gains `return_preprocessed = FALSE` (attaches the
`preprocessed.goldfish` to the fit); consumers take `preprocessed =`
(named after the class, the same name the estimators take it under;
`stats_data` rejected — collides mentally with remstats). When a replay is needed and
unavailable, `cli_abort` names both routes (re-estimate with
`return_preprocessed = TRUE`, or supply
`preprocessed = compute_statistics(...)` — the consolidated producer from
revise-gather-output; `estimate_*(..., preprocessing_only = TRUE)` remains
its equivalent until the naming pass supersedes it).

### D5 — residuals()/fitted()/predict()/augment() semantics

`augment` wiring rides this decision (2026-07-25 audit finding): the
method is currently a bare `export(augment.result.goldfish)` with no
S3 registration and no re-exported generic — unlike `tidy`/`glance`.
Since this change reworks the method anyway, it lands wired like its
siblings (registration + `generics::augment` re-export; bare export
removed under the dev-line-only rule); pkgdown indexing of the topic is
owned by the pkgdown-update change.

`residuals(object, type = c("deviance", "schoenfeld", "scaled_schoenfeld",
"score", "cox_snell", "response", "martingale", "dfbeta", "dfbetas"),
preprocessed = NULL)` — survival::coxph type vocabulary; deviance default
($-2 \cdot$ `intervalLogL`; continuity with `diagnose_*`). All DyNAM
residuals are conditional per submodel (the exact score residuals of the
factorized likelihood — no dyad-rate transformation; see the reference
doc §5). `fitted(type = "outcome")` = `exp(intervalLogL)` (stored, free);
`type = "probabilities"` via evaluator. `predict(type = c("probabilities",
"ranks"), events = NULL)` = in-sample next-event prediction at observed
decision points via the evaluator; documented as NOT forecasting and NOT
marginal effects (endogenous statistics — reference doc §3.3). `augment()`
gains `.fitted` ($\hat p_k$) and `.resid` (deviance) columns following
broom naming; existing columns unchanged. Alternative rejected: exposing
mlogit-style marginal effects — history-conditioned sensitivities would be
misread as process-level effects.

### D6 — test_* surface, no `gof()` generic

`test_gof()`, `test_parameter()`, `test_time()` — matching the
manynet/migraph `test_*` convention under stocnet and avoiding masking
`ergm::gof`. Dispatch: methods on `result.goldfish` (single submodel) and
on the specification-based fit (both submodels; per-block tests + joint
omnibus). No two-fitted-objects helper function; the spec fit is the
multi-block surface.

### D7 — test_gof(): Boschi–Wit bridge, fully analytic

Per effect $d$: standardized cumulative score process
$\widehat W_d(u) = \hat J_d^{-1/2} n^{-1/2} \sum_{k \le \lfloor nu \rfloor}
s_{kd}$ from stored `event_scores`; $T_d = \sup_u |\widehat W_d|$ with
Kolmogorov p-value $p(t) = 2\sum_{j\ge1} (-1)^{j-1} e^{-2j^2t^2}$;
$\hat J_d = I[\hat\theta]_{dd}/n$ — the **average per-event** observed
information, not the total (the $n^{-1/2}$ normalization requires the
per-event scale; the empirical variance of centered contributions is the
fallback estimator of the same quantity). Omnibus per block and joint:
Cauchy combination
$T_o = \frac{1}{L}\sum_l \tan(\pi(0.5 - P_l))$,
$p = \frac12 - \arctan(t_o)/\pi$ (valid under arbitrary dependence).
All-FLE goldfish models need no penalty centering, no multivariate-block
simulation, no model simulation. Offset (fixed-coefficient) terms are
excluded — their score processes are not bridges; a cli error points to
`test_parameter()`. Verification includes the exact bridge property
(process returns to 0 at $u = 1$, free effects, no offsets).

**Clock choice (added 2026-07-24).** The event-index normalization
$u_k = k/n$ inherits Boschi–Wit's proportional-information-accrual
assumption (their covariance $\min(t,u) \cdot J$ presumes constant
increment variance), which cold-start endogenous statistics violate — the
same phenomenon D13 diagnoses. `test_gof(clock = c("event",
"information"))`: `"information"` places increment $k$ at
$u_k = \hat I_d(k)/\hat I_d(n)$ from OPG cumulative sums of stored scores
(zero passes) — the martingale time change restoring the bridge limit
under non-uniform accrual. `diagnose_onset()`'s information-accrual curve
is exactly the clock map (one diagnostic, no second variant); its docs and
the diagnostics vignette present the workflow: flat onset segment →
rerun with the information clock. Derivation:
`.plan/sp/residuals-gof.md` §0.3(b), §7.4.

*(Revised by D32, 2026-07-30 — two corrections. The standardization is
the empirical per-event variance of the centered score contributions
(the OPG scale both reference implementations use), with `I_dd/n`
demoted to the documented asymptotic equivalent — the reverse of the
order stated above. And the clock does not change the statistic, which
is clock-invariant by construction: it selects the reference
distribution — analytic Kolmogorov on the event clock, the
Lin–Wei–Ying multiplier simulation on the observed information grid on
the information clock. The "restores the bridge limit" sentence above
survives in that form: what the time change restores is the accuracy of
the null law, expressed through the on-grid reference.)*

### D8 — test_parameter(): score/LM test, plus Wald for combinations

Score test of a candidate effect block: estimate the constrained model,
run `evaluate_model()` on the full model (candidate statistics included)
at $\tilde\theta = (\hat\theta_1, 0)$, form
$\mathrm{LM} = U^\top I^{-1} U \sim \chi^2_q$ (efficient-score form
$U_2^\top [I^{-1}]_{22} U_2$). This is RSiena's score-type test
(Schweinberger 2012) computed analytically. The Wald form for linear
combinations (`Multipar.RSiena` analog; restriction matrix on an
unconstrained fit via `vcov()`) is **deferred post-release** (2026-07-19
decision — task 4.4 descoped): phase 2 ships the score/LM test only.
LR/Wald of nested fitted pairs stay with
`lmtest::lrtest()`/`waldtest()` (generics already satisfied) — documented,
not reimplemented.

### D9 — test_time(): trend (default) and periods methods

`method = "trend"`: zph-style — scaled Schoenfeld residuals
$\hat s^*_k = \hat\theta + \bar V^{-1} \hat s_k = \hat\theta + n\,
I(\hat\theta)^{-1} \hat s_k$ with $\bar V = I(\hat\theta)/n$ (corrected
2026-07-24 — an earlier draft wrote $\hat\theta + n \bar V^{-1} \hat s_k$,
an extra factor $n$; $n$ is the diagnosed submodel's own event count,
never shared across submodels) against a time transform
(`transform = c("identity", "rank", "km")`), per-effect
zero-slope score test + global test, from stored primitives only.

`method = "periods"` (named to avoid colliding with windowed effects):
sienaTimeTest analog — score test (D8 machinery) of period-dummy × effect
interactions $h^*_{j,d} = h_d \cdot \mathbb{1}\{t \in P_j\}$. Because the
period indicator depends on time only, it is constant across the risk set
and factors out of the model expectation, so the interaction's Schoenfeld
residual is the stored score row masked by period membership:
$s^*_k = \mathbb{1}\{t_k \in P_j\}\, s_{kd}$. Consequences: **no
preprocessing pass and no new statistics** — the block scores are
period-wise partial sums of `event_scores` (with $\sum_j U_j = 0$ at the
MLE, so the first period is the reference and $J-1$ blocks are tested,
matching sienaTimeTest's base period). The `periods =` argument accepts an
integer $J$ (split into $J$ periods with approximately equal *event*
counts — the default form, keeping per-period information balanced when
the event rate varies), a numeric vector of cut times (right-open
intervals on the event-time axis), or a length-$n$ grouping vector/factor
(exogenous regimes). Information: `information = c("expected", "opg")` —
the default accumulates the exact per-event Fisher contributions
period-wise inside one evaluator pass ($J$ matrices of $p \times p$; the
per-event contributions are never stored), because OPG-based LM tests
over-reject in finite samples (Davidson–MacKinnon); `"opg"` uses
$\hat I^*_j = \sum_{k \in P_j} s_k s_k^\top$ from stored scores alone
(zero passes — for choice-type submodels the OPG term is conditionally
unbiased for the exact contribution; for exact-time rate/REM only the
martingale expectation identity holds) and serves as the free screening
mode across many effects × period schemes. The one-step per-period deltas
$\Delta_j = I^{*-1} U_j$ are reported as the interpretable readout. Both
methods return one object class with the per-effect table and the
plot-ready residual/process data. Testing a *windowed statistic* (e.g.
`inertia(net, window = 300)`, a memory/decay hypothesis) is a different
test that does need preprocessing and routes through `test_parameter()`.

### D10 — goldfish emits plot-data; autograph plots (branch feature/goldfish-diag)

Every test/residual object carries the data its plot needs (process paths,
smooths' inputs, per-effect tables) and prints via cli (semantic elements,
data interpolation, pluralization; snapshot tests pin a reproducible cli
context). No ggplot2 code lands in goldfish. autograph, on
`feature/goldfish-diag` off `develop`, gains `plot.test_gof.goldfish`,
`plot.test_time.goldfish` (and `test_parameter` if a plot is meaningful),
following its RSiena/ergm/MoNAn dispatch-on-class pattern with no goldfish
dependency. `diagnose_outliers()`/`diagnose_changepoints()` change their
return classes to `outliers.goldfish`/`changepoints.goldfish` (BREAKING for
anyone dispatching on `diagnostic.goldfish`; NEWS entry) and autograph's
`plot.outliers.goldfish` is fixed to test the logical `outlier` column
(currently expects `"YES"`). Class names for new objects follow the
existing `<thing>.goldfish` suffix convention so autograph dispatch stays
uniform across the stocnet umbrella.

### D11 — Documentation via inheritance

`residuals.result.goldfish` is the canonical page for residual-type
definitions; `evaluate_model` for `preprocessed`/`at`/`return`;
`test_gof` for the omnibus/Cauchy description. Other methods use
`@inheritParams`/`@inherit`. `devtools::document()` runs inside any task
touching roxygen; man pages checked for resolved inheritance.

### D12 — Effect selection by compact term string; term-wise diagnose_*

Effect-selecting arguments (`effect =` / `effects =`) match against the
**compact term strings** (the shared builder behind print/`tidy()`/
`gather_model_data()` — e.g. `inertia/callNetwork [W,300s]`), which are
unique per term by construction; integer positions are the fallback.
Matching semantics follow function semantics: an exact compact-string
match always wins; a bare family name (`"inertia"`) matching several terms
**selects the whole family in the vectorized `test_*` functions** but
**errors in the single-series `diagnose_*` functions**, with the cli error
listing the matching compact strings (doubling as discoverability). The
compact-term-strings spec gains "selection" as a consuming surface
(delta in this change).

`diagnose_changepoints()` and `diagnose_outliers()` gain `effect =`:
term-wise diagnosis on the stored score columns. Changepoints run on the
effect's scaled Schoenfeld series (regime shifts in $\theta_d(t)$ — the
structural-break literature's empirical fluctuation processes,
`strucchange::gefp`, are the same object); outliers rank events by
$|\text{dfbeta}_{kd}|$ (influence localization: which events drag
coefficient $d$). Default (no `effect =`) keeps the current
`intervalLogL` behavior. Post-selection caveat documented: changepoints
*detected* on a score series and then *tested* with
`test_time(method = "periods")` on the same data inflate size — the
supported workflow is exploration here, a priori periods (or the other
submodel) for confirmation.

### D13 — diagnose_onset(): the cold-start / left-censored-history diagnostic

Motivation: for early events the endogenous statistics are constant across
the risk set (no accumulated history), so the model predicts at the
per-event null benchmark — the frequentist face of the PSIS-LOO
`influence_pareto_k` signal goldfish.latent observed on first
observations. Empirically, `diagnose_changepoints()` on `intervalLogL`
does **not** surface this phase (short segment vs the PELT penalty, the
`minseglen` default, a plateau *at* rather than away from the null, and —
decisively — the endogenous score contributions are exactly zero at cold
start, a series the changepoint detector never sees). *(D24 adds a fourth
cause, measured 2026-07-29: on a rate or REM fit the segmented series pools
the dependent log densities with the right-censored timing terms, and on a
windowed fit that is half the series — PELT then reports 427 changepoints in
875 intervals, one per window closure, against 50 in 439 on the dependent
intervals alone. On such a fit it is the dominant cause.)* Hence a dedicated
diagnostic, `diagnose_onset()` (named for the process onset; the docs give
the exact term, left-censoring of the endogenous statistics; `burnin`/
`warmup` rejected for MCMC connotations beside goldfish.latent), computing
from stored primitives only:

1. **leave-initial-segment-out parameter path**
   $\hat\theta_{-[1:m]} \approx \hat\theta - I^{-1} \sum_{k \le m} s_k$
   for all $m$ at once (cumulative score sums through the stored
   information matrix — zero passes): flat = onset harmless;
   drift-then-stabilize = influential up to the stabilization index;
2. **information-accrual curve**
   $\mathcal{I}(u) = \sum_{k \le nu} \operatorname{tr}(I_k) /
   \operatorname{tr}(I)$, OPG from stored scores: the initial flat segment
   shows when the data begins identifying the endogenous parameters.

*(Revised 2026-07-29.)* The exact-information variant of (2) is **not** a
second mode of this diagnostic to be built here. It needs $\operatorname{tr}
(I_k)$ per interval, which no primitive carries — `evaluate_model(return =
"information")` and the fit's `final_information_matrix` are both the total
$p \times p$ matrix, and D9 declined to store per-event ones — so it is a
kernel accumulation. Task 4.6's `test_time(information = "expected")` needs
the identical accumulation indexed by period rather than by interval, and is
the only estimation-kernel visit scheduled before 2.0.0. **They are one
feature under two names and ship together**, in 4.6, so the kernels are opened
once for two consumers; cutting section 4 then leaves both functions on their
OPG default rather than one of them half-built. Nothing is lost by waiting:
this object is descriptive (no p-values), and the OPG calibration weakness D9
records is a property of *tests*, which a cumulative share is not.

Output is descriptive, not inferential: the object carries both curves
(coefficients labeled by compact term strings) and a stabilization
summary; the docs present the remedies — warm-starting the networks by
linking pre-observation events (the goldfish idiom) or excluding initial
events from the dependent set (relevent's `conditioned.obs` precedent) —
and cross-reference the deviance-trace reading (early plateau at
$D_{\text{null},k}$ = uninformative, not surprising). Companion residual
type `"cooks"` ($s_k^\top I^{-1} s_k$, the scalar one-step
self-influence) joins dfbeta; both are stored-primitive types (scores +
information matrix), not evaluator types.

**Documented caveat (canonical residuals page, explicit section):** all
influence measures here — dfbeta, dfbetas, cooks, the onset path — and
equally PSIS-LOO in goldfish.latent, are **likelihood deletion, not
history deletion**: removing an event's likelihood term keeps its effect
inside every subsequent endogenous statistic. Counterfactual removal
(the event never happened) would change the downstream statistic stream
and requires a replay per deletion; none of these measures estimate that.
For the onset segment the distinction is benign (conditioning on early
history is the standard REM move), but the caveat must be stated where
the residual types are defined.

### D14 — Dedicated diagnostics vignette; teaching vignettes stay short (2026-07-24)

A new long-form vignette (`vignettes/diagnostics.Rmd.orig`, precompiled
like the others) is the canonical prose documentation of the diagnostics
layer: the residual-type map (which primitive feeds which type, per
submodel and flavor), the margins **calibration-descriptive** reading —
observed-vs-expected actor maps as a screen for unmodeled heterogeneity
(the Juozaitienė–Wit "ghost effects" motivation), explicitly not
per-actor tests, with `test_parameter()` as the formal route — the
`test_*` family with the clock-choice workflow (`diagnose_onset()` accrual
curve → `clock = "information"`), and where each identity holds
(algebraic vs at-the-MLE). The teaching vignettes gain only **short**
diagnostics sections (fit → a couple of residual calls → one test) with a
pointer to the diagnostics vignette; depth lives in one place.
Literature grounding: `.plan/sp/residuals-gof.md` §0.4 and
`.plan/sp/residuals-gof.bib` — no surveyed package ships analytic actor
margins (relevent/remstimate are per-event/per-effect; degree calibration
exists only as simulation workflows), so the vignette must carry the
justification, not assume it.

The vignette also carries a **REM-vs-DyNAM model comparison section**
(2026-07-25), descriptive only and built purely from what the fits already
store — no new function surface. Its basis is the exact-time likelihood
algebra in backend-parity's design appendix (items 8–10): both models factor
into a categorical "which dyad" part and an `Exp(T)` timing part, and
DyNAM's dyad probability composes exactly as
`p_rate(sender) × p_choice(receiver | sender)`, so the stored conditional
components put the two parameterizations on one categorical scale. The
section illustrates, with figure code: (a) the per-event conditional
difference `d_e = conditional^REM − (conditional^rate + loglik^choice)` and
its cumulative trace over the event sequence; (b) the total decomposition
table — full loglik difference split into categorical vs timing parts, with
AIC/BIC on the full likelihoods; (c) calibration side-by-side — REM
sender/receiver margins against DyNAM's composed probability-scale margins;
(d) pacing — Cox–Snell Q-Q panels from each model's `total_rate`; (e)
`predict()`-based who-is-next agreement on a small fixture. Requirements:
both fits on the same dependent events and consistent risk sets (dyads =
senders × choice sets); the timing and categorical parts are reported side
by side, never pooled. Formal non-nested inference (the Vuong statistic,
derivation recorded in backend-parity's appendix item 11) is deliberately
NOT implemented here — the vignette names it as the future formal route.

**Revision (2026-07-28, from the marked-point-process literature pass —
findings in `.plan/sp/residuals-gof.md` §0.5 and §0.4 sweep):**

- **MPP framing**: the vignette presents the margins as coarsened-mark
  marked-point-process residuals — one factorization
  $\lambda^*(t,\kappa) = \lambda^*(t) f^*(\kappa|t)$, with the
  probability margin as the mark side and the expected-count margin as
  the ground/compensator side (the which/when split *is* the mark/ground
  split); cite Daley–Vere-Jones Prop. 7.3.III / Jacobsen 2006, and
  Vu et al. 2017 for the separability lineage.
- **Novelty claim narrowed**: Perry & Wolfe (2013, §5.4) ship analytic
  *dyad-level* Breslow-based martingale/Pearson residuals — the claim is
  "no package computes analytic **actor-level** margins" (re-verified
  2026-07-28 across relevent, remverse released+dev, remulate, rem,
  amen, dream, redeem, amorem), with Perry & Wolfe cited as the
  dyad-level precedent.
- **Terminology box** distinguishing the two "martingale residual"
  senses: Boschi–Wit-style cumulative score processes (per-effect,
  event-indexed, the `test_gof()` object) vs per-unit end-of-window
  martingale residuals (per-dyad/actor, the `residuals(type =
  "martingale")` / margins object); note the `survival`
  `collapse = id` recurrent-events precedent for the actor-level view.
- **amorem comparison**: position `margin_table()` and `test_gof()`
  against `amorem` 1.0.0 (the packaged Boschi–Wit reference
  implementation: per-observation NCC martingale residuals,
  `gof_univariate/multivariate/global/auxiliary`) — goldfish runs the
  same bridge tests on exact full-risk-set scores rather than
  one-control-per-case sampling, and adds the actor margins amorem does
  not have.
- **Per-actor aggregation section** (D19): top-x% recall per actor from
  stored ranks, per-actor deviance sums, aggregated-residual ggplots
  with reading hints (spotting actors that behave differently), teaching
  the sender-vs-receiver attribution choice — explicitly no function.
- **Citation hygiene**: Boschi–Wit is cited as the published version
  (*Statistics and Computing* 36(1):4, 2026), Lakdawala et al. as
  *J. Computational Social Science* 8:92 (2025), Juozaitienė & Wit
  "It's about time" as JRSS-A 188(4):1246–1262 (2025).

### D15 — Rank ties: strict-greater with a 1e-12 tolerance, phase 1 (user, 2026-07-27)

The rank tie rule is `observed_rank = 1 + count(rate > observed_rate + tol)`
with `tol = 1e-12`, applied identically on all three backends and to every
rank-sensitive primitive (`observed_rank`, top-k recall). The record R.2
requires: the tolerance is the load-bearing part — it collapses
float-split blocks identically across backends (0 cross-backend
disagreements at ≥1e-12 on the coordination fixture, versus 106/439 for
tolerance-free strict `>`); the backends agree on probabilities to
~5.6e-13 and genuinely distinct levels are ~17× apart, so 1e-12 sits well
above the noise and below any real structure. Strict-greater over midrank:
it is the simpler contract to state and test, needs no fractional term
(whose block-membership sensitivity made midrank *worse* at tolerance 0,
159/439), and the diagnostics consuming ranks (recall, rank histograms)
read "how many alternatives beat the observed one", for which the count
form is the direct answer. R.1's survey still runs as due diligence and
documents this choice against `remstimate`/`relevent`/base `rank()`
practice.

Sequencing (user, 2026-07-27): R.1–R.5 are **phase 1, completed before
section 2**, because the tie rule is part of the primitive contract that
`evaluate_model()`/`augment()` consume — they are release-critical and not
subject to the phase-2 cut line. `coordination-tie-consistency` (fixing
*why* equal dyads get unequal floats) is deferred **post-release**; when
it lands, it may tighten `tol` toward ~1e-15 as its own change. Landing
the tolerance rule first is accepted knowingly — the coordination
float-split stays masked at the rank layer until then, and the defect
remains documented in that change.

**R.1 survey, run 2026-07-28 against the installed packages** (method: the
package namespaces and argument lists, not recollection). There is no
in-family precedent to inherit — no surveyed relational-event package
reports a rank at all, so none has a tie rule:

- **base `rank()`** — six `ties.method`s (`average`/`first`/`last`/
  `random`/`max`/`min`), all on *exact* equality: no tolerance concept
  anywhere. goldfish's rule is `"min"` (1 + count strictly greater) with a
  tolerance added, which base R has no spelling for.
- **`survival::coxph(ties = c("efron", "breslow", "exact"))`** — a
  *partial-likelihood* correction for tied event **times**, an argument of
  the model fitter, not of any diagnostic. It answers "how is the
  likelihood approximated when several events share a time", a different
  question from "which alternatives count as equally likely in a reported
  rank". Not a precedent; conflating the two was the trap R.1 names.
- **`remstimate` 3.0.0** — `diagnostics()` returns standardized
  (Schoenfeld-type) residuals, smoothing weights and rates; the namespace
  contains no object matching `rank|tie`. No rank diagnostic exists, so
  there is no rule to follow.
- **`relevent` 1.2-1** — likewise no `rank|tie` object in the namespace;
  its gof work is a single `.Call` into C.

**R.3 implementation note (2026-07-28): the tolerance is RELATIVE, not
additive on the raw weight.** The kernels do not all rank the same vector
— the six `*_default.cpp` engines rank raw `exp(linear predictor)` weights
(`weights`, `rates`, `dyad_weights`, `e`), the gather kernels rank
normalized probabilities, and the r backend's coordination path ranks
log-symmetric values. These differ by a positive per-event constant (or a
log), so only a relative comparison is invariant to which one a kernel
holds — an absolute `+ 1e-12` would mean different things in each and
could not deliver the cross-backend identity the rule exists for. The
implemented rule is `w_j > w_obs * (1 + 1e-12)`, equivalently
`log w_j > log w_obs + 1e-12` on the log scale, which is D15's rule with
its scale made explicit.

**Measured before/after (2026-07-28, `NOT_CRAN=true`, all six baseline
families, cpp vs r and cpp vs gather at a shared parameter vector):**

```
                        before (strict >)        after (relative 1e-12)
  se_dynam_choice_coord  1 of 439 disagree        0
  every other family     0                        0
```

The 107/439 figure recorded above no longer reproduces on HEAD: the r
coordination path now ranks in log space (its own fix for underflowed
dyads), which had already collapsed all but one of those events. The one
survivor was the pathological event 266 — cpp rank 2098 vs r rank 2155,
a 57-place gap inside one block — and the tolerance closes it. The rule
is still the right contract to state: what changed is how much
disagreement it currently absorbs, not whether the blocks are exactly
tied.

### D16 — `margin_table()`: the exported margins accessor (user, 2026-07-28)

The uniform accessor task 1.10 owes is the exported function
`margin_table()`, returning one tibble-based schema for every family so
`residuals()` / `diagnose_*()` / `predict()` never branch:

```
actor | role | observed | expected_probability | expected_count
```

- **Name**: `margin_table` — one vocabulary from kernel (`diagnostics ∋
  "margins"`, `fit$margins`) to user; the sense is base R's
  `margin.table()`/`marginSums()`, and the docs lead with
  observed-vs-expected to preempt the Stata/`margins`-package
  marginal-effects misread. The function stays **strictly margins**; future
  per-actor joins (ranks, deviance, exposure) are a separate surface
  (D19), so the name never becomes a misnomer. Rejected:
  `calibration_table` (breaks the margins vocabulary; drags in
  forecasting-calibration expectations), `actor_table`/`node_*`
  (pre-commits to a kitchen-sink table; imports an umbrella vocabulary
  goldfish doesn't otherwise use), `diagnose_margins` (the prefix
  promises a verdict the margins requirement explicitly refuses).
- **Role vocabulary**: `sender` / `receiver` / `endpoint`. `endpoint`
  replaces the earlier `partner` sketch for choice_coordination —
  application-agnostic, and it is the graph-theoretic word the handshake
  argument is about (sum over endpoints = 2|E|, which is why the
  coordination column total is 2n). Rows are always actors; REM
  contributes two rows per actor (sender and receiver), rate/choice one
  (sender / receiver), coordination one (endpoint).
- **Two expected columns, never scale rows**: `observed` does not vary
  with scale; `expected_count` is `NA` on multinomial families and the
  documentation states this `NA` means "not defined for this model
  class", never "not computed". Both columns always present (schema
  stability). Consequence stated in docs: multinomial families have a
  calibration ratio (`observed / expected_probability`) but no
  martingale residual (`observed - expected_count`).
- **Two-mode fits**: labels come per side from the model's
  `node_lookup`; the mode slice is carried in the metadata `context`
  (D18), not as extra columns.
- **Flavored fits**: `margin_table()` gains a
  `flavored_result.goldfish` method — rbind of per-fid tables with
  `flavor` and `family` columns from `process_map`; its print method
  reflects the multiple flavors estimated. Per-event methods
  (`residuals`, `augment`) on the flavored class stay out of scope; the
  documented route is `fit$results[[fid]]` until a combined per-event
  identity convention is decided (overlaps with DyNES pool formats).
- The wrong comment at `R/cpp_interface.R:600` (coordination listed among
  the two-sided sub-models; only REM/REM_ordered are two-sided, the
  MM/coordination kernels allocate a single margins pair over
  `n_actors_1`) is corrected in the same task.

### D17 — Diagnostic classes: snake_case constructor-name classes on tibbles, no hierarchy, one internal constructor (user, 2026-07-28)

Every diagnostic data object is built by one internal constructor,

```
new_diagnostic_table(df, class, context, params)
```

which returns a **tibble** with `class` prepended —
`c("<constructor_name>", "tbl_df", "tbl", "data.frame")` — and the D18
metadata attached. Rules:

- **Class = exported constructor's name**, snake_case, no `.goldfish`
  suffix: `margin_table`, `diagnose_outliers`, `diagnose_changepoints`.
  This is the RSiena/autograph invariant (`selectionTable()` returns
  class `selectionTable`; autograph binds `plot.<class>`): zero future
  naming decisions, `inherits(x, "margin_table")` self-documents, and it
  removes the S3 ambiguity of dotted classes (`plot.outliers.goldfish`).
  Supersedes task 3.1's earlier flat `outliers.goldfish` /
  `changepoints.goldfish` classes — those names never shipped from
  goldfish (today both functions return `diagnostic.goldfish`), and
  autograph's bindings are updated by task 3.4 anyway, so the rename is
  free now and costs a second autograph round-trip later.
- **No class hierarchy.** A parent class's only non-speculative payoff
  was an autograph fallback plot; uniformity comes from the constructor
  and the D18 contract instead. If autograph ever wants the fallback, a
  trailing marker element can be appended later — a non-breaking change.
- **Tibble base is safe** (experiment, 2026-07-28): `[`, `filter`,
  `mutate`, `arrange`, and `rbind` all preserve a prepended class and
  attributes via `dplyr_reconstruct()`; only `group_by() |> summarise()`
  drops them — correctly, a summary is no longer a `margin_table`. The
  earlier "subsetting drops classes" concern is unfounded; tibbles keep
  the diagnostics coherent with the `tidy()`/`glance()`/`augment()`
  output family.
- **Scope**: only this change's diagnostic classes. The fit classes
  (`result.goldfish`, `flavored_result.goldfish`) and the data classes
  (`nodes/network/dependent/global.goldfish`, the `data.goldfish`
  marker) are untouched — verified 2026-07-28: no new data class exists
  anywhere; `as_goldfish()` stamps `data.goldfish` as an optional
  provenance/print **marker** and estimation validates the stocnet on
  the fly, unconditionally. A broader class-rename pass, if ever, is its
  own change; not opened now.

### D18 — Diagnostic metadata contract (user, 2026-07-28)

Every diagnostic table carries attributes sufficient for `print`
(goldfish, cli) and `plot` (autograph) without reaching back into the
fit:

- `diagnostic` (chr): producer name (`"margin_table"`,
  `"diagnose_outliers"`, …).
- `context` (list): model, sub_model, flavor/fid (or `NA`), backend,
  `n_events` plus per-side totals for margins, node-set name(s)/mode
  labels on two-mode fits, and — for `margin_table` — which expected
  columns are defined for this family, so autograph chooses
  calibration-ratio vs martingale-map rendering without NA-sniffing.
- `params` (list): producer arguments that shape interpretation
  (method/threshold/window for the diagnose family; requested scales).
- `version` (chr): goldfish version that produced the object (autograph
  precooked fixtures age).

The goldfish/autograph division of labor is the bayesplot
`mcmc_*_data()` pattern: goldfish functions are the data producers,
autograph owns the geometry via `plot.<class>` methods reading exactly
this contract. The contract is documented once in a dedicated roxygen
topic; `print.diagnostic.goldfish`'s column sniffing disappears with the
per-class print methods.

### D19 — Per-actor extensions: exposure via the evaluator; aggregations vignette-only for now (user, 2026-07-28)

- **`exposure` and `n_opportunities` become `evaluate_model()` return
  quantities, computed on demand in phase 2** — not stored primitives.
  *(The "not stored" half is superseded by D23: they are storable under the
  `"availability"` primitive as well, and are counted per actor membership
  rather than per risk-set position. The rest of this bullet stands.)*
  They accumulate inside the same masked risk-set loop as the margins
  (`exposure_time[j] += Δt` per interval with `j` at risk, all intervals,
  exact-time; `n_opportunities[j] += 1` per dependent event with `j` in
  the realized choice/risk set, every family), which is the load-bearing
  point: the kernel's per-interval membership *is* the realized
  availability — composition changes, state-derived flavor masks, and
  support constraints included — so no R-side replay of masks is ever
  attempted. The two variants deliberately mirror the two margins scales
  (`exposure_time` ↔ `expected_count`, ground side over all intervals;
  `n_opportunities` ↔ `probability`, mark side over dependent events).
- **Per-actor rank/recall/deviance aggregations ship as a vignette
  section, not a function** (D14 revision): per-actor top-x% recall from
  stored `observed_rank`, per-actor deviance sums from
  `interval_log_lik`, aggregated-residual ggplots with reading hints on
  spotting actors that behave differently — all R-side group-bys over
  stored per-event vectors joined with `augment()`. The open attribution
  question (grouping by sender answers "how predictable are s's
  choices"; by receiver, "how surprising are arrivals at r") is *taught*
  in the vignette rather than frozen into an API. A future
  `node_summary(fit, include = ...)` join surface is name-reserved, not
  designed.

### D20 — Exact-time Schoenfeld rows are a primitive, `conditional_scores` (user, 2026-07-29)

**The problem, found while implementing 2.2.** The Schoenfeld residual is
`r_k = X_obs − Σ_j p_j X_j`. The stored score row of an exact-time
sub-model is `s_k = X_obs − Δt_k T_k · Σ_j p_j X_j`, so the two differ by
the exposure scale on the mean term. Recovering `r_k` from `s_k` needs the
*p-vector* `m_k = Σ_j p_j X_j` (or `X_obs`), and the fit carries neither:
one vector equation, two vector unknowns. `total_rate` and `Δt` give only
the scalar `c_k = Δt_k T_k`, which converts between the forms once you
already hold one of the vectors — it does not supply one. (On a
right-censored interval `s_k = −c_k m_k`, so `m_k` *is* recoverable there,
which is precisely where Schoenfeld is undefined.) So the exact-time
Schoenfeld rows cannot be derived from the fit, and `scaled_schoenfeld`
(task 2.3), `test_time(method = "trend")` (4.5) and the `survival::cox.zph`
cross-check (2.6) all inherit that.

**The decision.** The kernels emit the rows directly, under the name
`conditional_scores`, as a new opt-in name in the `diagnostics` vocabulary.
The framing that makes this a completion rather than an addition:

```
  exact-time log-likelihood = conditional ("which")  +  timing ("when")
                stored as:     conditional_logl          (already exists)
  exact-time score row      = conditional score      +  exposure term
                stored as:     conditional_scores        (this decision)
```

The Schoenfeld residual of an exact-time model **is** the score of its
conditional (partial) likelihood — which is why the multinomial families
need nothing: their likelihood is already conditional, and their stored
`event_scores` already are the Schoenfeld rows.

- **Implementation is one more call to an existing function.** The
  multinomial kernels already call
  `event_score_row(X, probabilities, 1.0, obs, dependent)`; the exact-time
  ones call it with `c = Δt` (or `Δt·T` on the gather stack). The new
  component is the same call at unit scale, in three kernels —
  `DyNAM_rate_default.cpp`, `REM_default.cpp`,
  `compute_poisson_selection.cpp` — plus the one-line R mirror
  (`event_score_row()` in `estimation_core.R`). The normalized
  `probabilities` vector is already in scope in all four, computed for the
  probability-scale margins and the conditional log-likelihood. Not nine
  kernels: the multinomial ones are already correct by construction.
- **Opt-in, not default-on.** It is an n × p matrix, the same size as
  `event_scores`. The default stays `c("loglik", "scores")`; `"all"` means
  all six. The storage-footprint guardrail (task 1.3) sizes it like
  `scores`.
- **One flag, two consumers** (user, 2026-07-29). The same kernel flag
  serves estimation-time storage and `evaluate_model(return =
  "conditional_scores")`, because both reach the kernels through the one
  closure the 2026-07-28 refactor extracted. So a fit that did **not**
  store the primitive can still produce the rows on demand from the
  preprocessed statistics — attached via `return_preprocessed = TRUE` or
  handed in — and a fit that did store them pays no pass. That is what
  makes exact-time `schoenfeld` / `scaled_schoenfeld` the first residual
  types to span both tiers: stored if present, recomputed under the
  replay rules otherwise, and only aborting when neither route is
  available (in which case the error names both).
- **Silently ignored off exact-time.** Requesting it on a multinomial
  family stores nothing and warns nothing, exactly as `total_rate` and
  `conditional_logl` are simply absent there. The documentation states the
  reason (the score rows already are the conditional ones), so the silence
  is informative rather than mysterious.
- **The archived `backend-primitive-parity` requirement "All five
  primitives are available on all three backends" is deliberately NOT
  amended.** Nothing it asserts becomes false — those five remain available
  on all three — and the sixth carries its own parity claim in this
  change's new requirement. Reopening an archived capability to renumber a
  title is cost without content.
- Rejected: evaluating the same preprocessed object under the **ordinal
  spec** (the ordinal score row is algebraically the same object, since the
  conditional component *is* the ordinal likelihood — already test-verified
  for the log-likelihood). It costs a second full pass and a runtime spec
  swap whose equivalence a reader must re-derive, to avoid a change in
  kernels that are being opened anyway for D19. Rejected: an R-side replay
  of the statistics — O(n·|R|·p) and a second implementation of what the
  kernel already knows.

### D21 — The fit carries the interval clock (user, 2026-07-29)

The fitted object gains `intervals` (the per-interval Δt), `start_time` and
`end_time`, joining the `event_time` and `right_censored_events` it already
carries. Three consequences, in order of importance:

1. **`cox_snell` stops needing a replay object.** It becomes
   `intervals * total_rate` — an exact product of two stored vectors, where
   the fit-only fallback identity
   (`conditional_logl + log(total_rate) − interval_log_lik`, verified to
   1.2e-10 with compensators summing to the dependent-event count) is a
   difference of logs. See D22.
2. **Diagnostics can be read on the observed clock.** `augment()` (2.5) and
   every autograph panel (section 5) can place a deviance trace, a
   Cox–Snell Q-Q or an onset curve on real time rather than event index,
   and `start_time`/`end_time` give the observation window the axis needs.
3. **`n` stops being ambiguous.** `scaled_schoenfeld`'s Grambsch–Therneau
   constant is the event count *of the sub-model being diagnosed*, which is
   `sum(!right_censored_events)` — the dependent events, not the intervals.
   A fit that carries both makes that unambiguous rather than conventional.

**`is_dependent` is deliberately NOT added**: the fit already carries the
same fact as `right_censored_events` (its negation), and one fact under two
keys is what `format_version.R` argues against for the two epoch counters.
The documentation cross-references the two spellings — the preprocessed
object's `is_dependent`, the fit's `right_censored_events` — instead.

**`fit_version` does NOT move — the epoch names a *released* layout**
(user, 2026-07-29). Checked against the history before deciding: the last
release is `v1.7.0` (2025-06-23), and the epoch was introduced on
2026-07-26 at DESCRIPTION 1.9.15, thirteen months later. So **no released
goldfish has ever written a `fit_version`** — every fit in the wild is
epoch 1 ("no record"), epoch 2 exists only inside this dev line, and 2.0.0
will be the first release to ship an epoch at all.

```
  released   v1.6.x … v1.7.0   →  epoch 1   (no record)
  dev line   1.7.1 … 1.9.x     →  epoch 2, still being assembled
  2.0.0                        →  ships epoch 2, frozen at release
```

The rule that follows: **one bump per release whose layout differs from the
previous release's**, not one per dev-line component addition. Epoch 2 is
"the 2.0.0 layout" and keeps absorbing changes — the interval clock
included — until 2.0.0 ships. Bumping now would burn a number that no user
could ever hold an object stamped with, and would refuse every dev-line fit
for a change that only *adds* components. The same rule governs
`prep_version`.

The cost of not bumping, stated so it is chosen rather than discovered: a
dev-line fit can be *current in stamp but stale in content* — stamped 2,
yet fitted before the clock existed. The epoch cannot catch that, so the
**consumer** must: a diagnostic needing a component that a fit may lack
checks for it and aborts naming what it needs, exactly as
`residual_stored()` already names a missing primitive. That guard is
per-consumer and legible; a global stamp for a dev-line addition is neither.

The components land at the shared assembly in `estimate_wrapper()`, so
DyNAM, REM and DyNAM-i fits get the clock in one place.

### D22 — `cox_snell` is a stored-primitive type, and the kernel session runs before 2.3 (user, 2026-07-29)

`cox_snell` was specced among the recompute types. It is not one: at the
fitted estimate it is `Δt · total_rate`, both of which the `"loglik"`
primitive stores on an exact-time fit (D21 makes Δt exact rather than
recovered). It moves to the stored-primitive tier, and only an evaluation
at θ ≠ θ̂ routes through `evaluate_model()`. The residual-methods spec is
corrected accordingly.

**Sequencing (user, 2026-07-29):** the kernel work — `exposure` and
`n_opportunities` (D19, the open remainder of task 2.1) and
`conditional_scores` (D20) — runs as one session **between 2.2 and 2.3**,
as tasks K.1–K.4. One flag-plumbing pass, one `cpp-recompile`, one baseline
run, rather than opening the kernels twice; and 2.3 then has the rows it
needs for `schoenfeld` / `scaled_schoenfeld` on exact-time fits instead of
landing partial.

### D23 — Availability gets both doors too, and is per **actor**, not per risk-set position (user, 2026-07-29)

**Revises D19's storage half.** D19 made `exposure` and `n_opportunities`
"on-demand evaluator quantities, never stored estimation primitives". They
become storable as well, under one new `diagnostics` name,
`"availability"`, while remaining evaluator quantities — the same both-doors
shape D20 gives `conditional_scores`, and for the same reason: the kernel
flag exists either way, and both routes reach the kernels through the one
closure, so storing costs a vocabulary entry rather than an
implementation.

Three arguments carried it, none of which D19 had weighed:

- **They are `|A|`-sized, not `n × |R|`-sized.** Two vectors over the actor
  set — on the standard fixture, 84 doubles each. The storage guardrail
  that governs `probabilities` and `scores` has nothing to say about them.
- **They do not depend on θ.** A stored availability vector can never
  disagree with the fit it rides on, because re-estimating does not move it.
  That is the opposite of the staleness risk that makes per-event
  primitives worth gating.
- **The per-actor aggregation workflow needs them beside the margins.**
  D19's own vignette section joins per-actor observed/expected counts
  (stored) with a denominator (not stored). Splitting a two-column table
  across "on the fit" and "needs the replay object" is a seam the user
  meets immediately and for no reason.

**Vocabulary shape, now stated as a rule.** `diagnostics` names *what to
compute* and is coarse; `evaluate_model(return =)` names *what to hand back*
and is fine. This is already true of `"loglik"` (one primitive, three
components, three evaluator names) and of `"margins"`. So:

```
  diagnostics = "availability"      →  fit$exposure, fit$n_opportunities
  evaluate_model(return = c("exposure", "n_opportunities"))
```

`exposure` is exact-time only and is simply absent on multinomial families,
silently, exactly as `total_rate` and `conditional_scores` are; requesting
it from the **evaluator** still aborts naming `n_opportunities`, because
there the caller asked for a named return value and getting nothing back
would be a lie.

**The definition, which the drafted reduction gets wrong.** The accumulation
is per **actor membership**, not per risk-set position:

```
  exposure[j]        += Δt   once per interval in which j is at risk at all
  n_opportunities[j] += 1    once per dependent event whose risk set contains j
```

On the actor-oriented families (rate, choice) there is one position per
actor and the two readings coincide. On the dyadic families they do not: a
REM risk set over 84 actors holds 6 972 dyads, of which 166 contain any
given actor, so a position-counting walk reports an "exposure" 166 × the
window length. The specs already say membership in their own words ("the
number of dependent events whose realized risk/choice set **contains** the
actor"), and membership is what makes `observed[j] / exposure[j]` an event
rate per unit time at risk — the quantity the per-actor tables want.
Position-counting would mirror the margins' dyad sums instead, but margins
are on the probability scale where the dyad sum is the point; availability
is a denominator.

Consequence for K.1: the drafted `accumulate_availability()` in
`.plan/availability_reduction.patch` walks positions and scatters through
the side index, which is reading (ii). The dyadic kernels need a per-event
actor mask — set a bit per at-risk actor while walking the risk set, then
add `Δt` / `1` once per set bit — an extra `|A|` pass per event against a
risk-set walk that is already `|A|²`. The single-sided families can keep
the position walk, since there position *is* actor.

**Availability has exactly the shape the margins have** (user, 2026-07-29).
Membership is per side, not per actor-in-the-abstract: a dyad `(i, j)` at
risk makes `i` available *as a sender* and `j` available *as a receiver*,
and an actor whose outgoing dyads are all masked while its incoming ones
are open is available on one side only. Collapsing that to a single "was
this actor at risk" vector would also break the denominator it exists to
be — `observed_sender[j] / exposure[j]` is an activity rate only if the
exposure is the sender-side one. So:

```
  single-sided families        rate, choice, coordination
      availability$exposure, availability$n_opportunities
  two-sided families           REM, REM_ordered
      availability$exposure_sender,        …_receiver
      availability$n_opportunities_sender, …_receiver
```

which is the margins' container, the margins' suffix rule and the margins'
labeling helper — no second convention, and coordination stays single-sided
over its endpoint set exactly as its margins do. The primitive name maps to
a component of the same name (`"availability"` → `fit$availability`), as
`"margins"` does; the evaluator's two finer names select the exposure half
or the opportunities half of that container.

`margin_table()` is **not** extended to carry these columns: D16 keeps it
strictly margins, and the per-actor join surface stays the reserved
`node_summary()`, taught in the vignette until it is designed.

### D24 — `diagnose_*` analyze the dependent likelihoods by default (user, 2026-07-29)

**Found while stress-testing the 2.5 `augment()` row-order fix.** On a
windowed rate model — `calls ~ 1 + indeg(calls, window = "15 minutes")` on
`social_evolution` — every event opens a window that closes 15 minutes later,
so the interval sequence alternates almost perfectly:

```
  875 intervals = 439 dependent + 436 right-censored, 1:1 interleaved

  interval_log_lik
      0 ┤ C     C     C     C     C     C        censored:  -Dt*T
        │ ●     ●     ●     ●     ●     ●        n=436, sd 0.84, ~-0.185
    -20 ┤ ○                 ○  ○     ○  ○ ○
        │       ○                                dependent: x_obs - Dt*T
    -50 ┤       ○                                n=439, sd 5.15
        │
    -80 ┤             ○
        └────────────────────────────────────
```

Two distinct findings came out of it.

**(a) The row-order bug 2.5 fixed was worse than its commit message says.**
Measured on this fit, the pre-2.5 `augment()` — which appended the censored
rows after the dependent ones while every per-interval column is in interval
order — mispaired **874 of 875 rows (99.9%)**, starting at row 2 and never
recovering. `diagnose_outliers(method = "Top", threshold = 5)` overlapped the
true five worst intervals on **1 of 5**. The 16-interval figure in the commit
message (13 of 16) understates the ordinary case: any window effect makes the
censored intervals interleave 1:1, which is the worst case for an
append-at-the-end layout.

Also confirmed on this fit: the fix places rows by **interval index and never
sorts on time**. Forcing two intervals to share a timestamp leaves the
alignment intact — which the obvious alternative fix (`arrange(time)`) would
not, and timestamp ties are real (two exogenous updates at one instant, a
window closing exactly as an event fires).

**(b) The series `diagnose_*` segment mixes two different quantities.** A
dependent interval contributes `x_obs - Dt*T`, a log density including the
observed alternative's term; a right-censored one contributes `-Dt*T`, pure
timing. Pooling them distorts every statistic both functions compute:

| on the windowed fit           | mixed series | dependent only |
| ----------------------------- | -----------: | -------------: |
| `median(interval_log_lik)`    |       -9.04  |       -13.73   |
| `IQR(interval_log_lik)`       |       13.63  |         4.83   |
| IQR outliers (threshold 3)    |           3  |            4   |
| **PELT changepoints**         | **427 / 875**| **50 / 439**   |

427 changepoints in 875 intervals is the detector reporting *"a window closed
here"*, once per closure. The IQR threshold is likewise set by the censoring
pattern rather than by the fit.

**The decision.** `diagnose_outliers()` and `diagnose_changepoints()` analyze
the **dependent intervals by default**, with an argument to opt back into the
full series. The returned table keeps **one row per interval** either way — the
censored rows are present, carry their time and their log-likelihood
contribution, and are simply never candidates (`outlier` / `cpt` `FALSE`). That
preserves the one-row-per-interval contract 2.5 established, keeps the real
time axis the autograph panels (section 5) plot against, and makes `nrow()`
independent of the argument.

The mechanism is already in place and was not designed for this: **2.5's
`.resid` is `NA` exactly on the right-censored intervals**, for broom-convention
reasons (an interval that realizes no outcome has no fitted probability and no
deviance for one). That NA *is* the "not a candidate" marker. Reading `.resid`
instead of `interval_log_lik` gives the default for free; the only real work is
mapping changepoint positions in the dependent subseries back to interval
indices for the returned table.

**Scope: the likelihood-value series only.** This does NOT generalize to the
score-based diagnostics, and stating the boundary is the point:

```
  restricted (deviance / interval_log_lik)   |  NOT restricted (score rows)
  ------------------------------------------ | ---------------------------
  diagnose_outliers()   default mode         |  diagnose_onset()      (D13)
  diagnose_changepoints() default mode       |  test_gof()            (4.1)
                                             |  test_time()           (4.5)
```

A censored interval's `event_scores` row is a genuine contribution to the
gradient — the sum over **all** intervals is the score — so excluding it from a
cumulative score process would be wrong. Only the likelihood *value* changes
meaning between the two kinds of interval. The argument nevertheless governs
both `diagnose_*` modes uniformly, including the term-wise `effect =` mode
(D12), so its meaning does not depend on which series was selected; in the
exact-time `scaled_schoenfeld` case the restriction is already automatic, those
rows being `NA` on censored intervals by construction (D20).

**The boundary is a theorem, not a judgment call — measured** (2026-07-29). On
the same windowed fit:

```
  |score| mass          dependent   censored
    Intercept              85.0%      15.0%
    indeg                  51.1%      48.9%     <- half the endogenous signal

  indeg score exactly 0 on 192 of 439 dependent intervals
  indeg score exactly 0 on   0 of 436 censored  intervals

  cumulative indeg score, ALL intervals, final value:    -0.0003
  cumulative indeg score, DEPENDENT only, final value:  142.3
```

A windowed statistic is alive *precisely during the window*, and the window's
lifetime **is** the right-censored interval — the closure is what ends it. So
`indeg` is dead on the dependent intervals whose window has already shut, and
alive on every censored one. Dropping them does not merely lose information: the
full gradient is zero at the maximum by construction, so a cumulative score
process over **all** intervals is a bridge that returns to zero (`-0.0003`),
while over the dependent subset it ends at `142.3`. **A subset of a score series
has no null distribution.** Every score-process diagnostic — the onset path,
`test_gof()`'s Brownian bridge, `test_time()`'s partial sums — rests on that
return, so restricting any of them would manufacture drift that is an artifact
of the truncation and hide the drift that is real. That is the opposite of what
D13 exists to catch.

**Consequently `include_censored` is NOT offered on the score-based
diagnostics** (user, 2026-07-29) — not as a default, and not as an escape
hatch. There is no setting under which the restricted series is the quantity
anyone wants.

**`diagnose_onset()` re-indexes instead** (user, 2026-07-29). The legitimate
wish behind the question is a readable x-axis: "how many *events* of history
did I condition away", not "how many intervals". A window closure adds no
history — no tie, no inertia, nothing endogenous — so counting intervals makes
the axis depend on the censoring pattern. The resolution is a re-indexing, not
a filter: the cumulative sums run over **all** intervals (bridge intact), and
the reported path is **indexed by dependent-event count**, `θ̂_{-[1:m]}`
evaluated at the interval index of the m-th dependent event. Same numbers, an
axis that means what the reader thinks it means, and no argument. This is also
what the stated remedy actually does: warm-starting by linking pre-observation
events, or starting the window later, removes whole intervals — the censored
ones included — so the counterfactual the path approximates is the
all-intervals one.

**Multinomial sub-models are unaffected**: choice, the ordinal rate and REM
sub-models and coordination have no right-censored intervals, so both settings
agree and the new default is safe everywhere.

**This is a behavior change, not only a class rename.** Task 3.1 already carries
a BREAKING NEWS entry for the class rename; the NEWS text must say explicitly
that the numbers move on rate and REM fits, since a user re-running an old
script gets different flagged events rather than a different class.

**Fourth cause for D13.** The design already records three reasons
`diagnose_changepoints()` failed to surface the cold-start phase empirically
(the `minseglen` default, the PELT penalty against a short segment, a plateau
*at* the null). This is a fourth, and on a windowed fit the dominant one: half
the series it segments is not the quantity being diagnosed.

**The argument is `include_censored`, default `FALSE`** (user, 2026-07-29).
The alternatives all collide: `include` is reserved by D19's
`node_summary(fit, include = ...)` for choosing *quantities*; `intervals`
collides with the fit component K.3 landed (`fit$intervals`, the per-interval
elapsed times), which would put two meanings of one word on one help page; and
`series` collides with `effect =`, which already selects *which* series. A
boolean naming exactly what it admits avoids all three and puts the default in
the signature. The shorter `right_censored` spelling goldfish uses elsewhere is
a noun for the fact; `include_censored` is a verb phrase for the choice, and
reads correctly at the call site. A `c("dependent", "all")` character argument
was the alternative if a censored-only third mode were ever wanted; the
pure-timing residual is interesting on its own but is deliberately not offered
now, and a boolean can grow into that argument later without breaking a call.

### D25 — One term vocabulary across every surface that takes a term from a user (user, 2026-07-29)

**Measured, on `depNetwork ~ 1 + indeg + outdeg + indeg(networkExog)`.** The
same four coefficients carry three different labels, and the argument users are
most likely to reach for matches the least guessable one:

```
  surface                            label for  indeg(networkExog)
  --------------------------------   ------------------------------
  print(summary(fit))                indeg/networkExog    <- compact string
  tidy(fit, compact = TRUE)          indeg_networkExog    <- export form
  coef(fit)                          ideg_networ_1        <- minimal-unique
  initial_parameters = c(...)        ideg_networ_1        <- ditto
```

The abort proves it: naming a bad coefficient reports *"Available:
`Intercept`, `ideg_networ`, `odeg`, `ideg_networ_1`"*. Nobody reads
`indeg/networkExog` in a summary and then types `ideg_networ_1`.

Meanwhile the compact-term-strings capability already **requires** that "the
string a user reads in the printed summary is the string that selects the term
in the diagnostics". Implemented as written, D12's `effect =` would take
`"indeg/networkExog"` while `initial_parameters` next to it takes
`ideg_networ_1`. Two vocabularies, adjacent workflows, one package.

**Decision: one resolver, several accepted spellings, one canonical form in
messages.** Every surface that takes a term from a user —
`initial_parameters`, `offset()` reporting, `diagnose_*(effect =)`,
`test_*(effects =)` — resolves through a single internal matcher that accepts
the compact string, the export form, the `coef()` label, and (per task 4.7's
existing integer fallback) a position; ties and unknown names abort with the
candidates rendered in the **compact** form, because that is the one the user
just read. `coef()` keeps its short labels: they exist to print a vector
readably and to subset it, and widening what is *accepted* costs nothing there.

**Correction to a premise** (checked in `formula_validate.R`): an unnamed
`initial_parameters` must be a **full-length vector**, one value per
coefficient — there is no `c("2" = 0.5)` positional selection today. So there
is no existing integer-selection surface to be homogeneous *with*; task 4.7's
integer fallback would be the first, and the docs should mark positions as
fragile (adding a term renumbers every later one).

**On the proposed `look_term(fit, keyword)` — worth having, with three
changes.** What is right: `fit$names` genuinely carries everything needed
(`Object`, the argument columns, `.effect_short`, `.object_short`,
`.term_export`, `.coef_name`), so the table costs nothing to assemble; and
naming the remedy inside the failing message is the right pattern, the one
`residual_stored()` and `fit_component()` already use. The changes:

1. **Name it as a noun, not an imperative.** goldfish's verbs are `make_*`,
   `link_*`, `estimate_*`, `compute_*`, `diagnose_*`, `test_*`,
   `evaluate_*`; its accessors are nouns (`margin_table()`,
   `risk_set_axis()`). `model_terms(fit, pattern = NULL)` fits that and folds
   "list them" and "search them" into one argument, where `look_term` implies
   only the search.
2. **Return a tibble, not printed output.** "The table `print(summary(fit),
   compact = FALSE)` shows, plus the name and the index" is the right
   *content*; it should arrive as data so it can be filtered and joined,
   following D17/D18 (classed tibble, metadata attached) rather than as a
   second printing surface.
3. **It is the second fix, not the first.** The failure is not "I cannot list
   the terms" — `initial_parameters` already prints the available ones. It is
   "the ones listed are unguessable and are not what `summary()` showed me".
   Fix the vocabulary first; the helper then earns its place on models with
   many effects, where listing candidates in an error is unreadable anyway.

**`model_terms()` is exported by this change** (user, 2026-07-29), documented
as a helper with worked examples of when it earns a call: after an
unrecognized-name abort, on a model with enough effects that listing candidates
in an error is unreadable, when two terms differ only in an argument
(`indeg` vs `indeg(networkExog)`), and to find what to pass as
`initial_parameters` / `offset()` / `effect =` without re-reading the formula.

It returns a **tibble** — the effects-details table `print(summary(fit),
compact = FALSE)` renders, plus the term's compact string, its `coef()` label,
its export form, and its coefficient index — filterable by `pattern`. On a
flavored fit it row-binds the per-process tables with `flavor` and `family`
columns appended, exactly as D27 requires of every diagnostic table, so
"which process has this term" is answerable in the same object.

This does **not** collide with `effect-term-registry`, and the boundary is
worth stating because the two sound alike:

```
  registry lookup (effect-term-registry)   "what effects does goldfish provide?"
                                            package-scoped, no fit involved

  model_terms(fit)          (here)         "what terms does THIS fit have,
                                            and what do I call each of them?"
                                            fit-scoped
```

They meet only at the compact-string builder, which the registry's task 5.1
will drive from `term_def` instead of hard-coded rules. That changes *how* the
string is built, not *what* `model_terms()` returns — so exporting it now and
letting the registry land later is additive, not a double definition.

**The multi-spelling matcher is a CORRECTNESS requirement, not ergonomics**
(measured 2026-07-29). The first draft of this decision treated accepting
several spellings as convenience. It is not: the compact string is **not
guaranteed unique**, and it is not even guaranteed to be what the user saw.
Two holes, both measured:

*(a) Truncation — what you read is not what selects.* `print.summary` calls
`compact_term_strings(names, "console", width = avail)` with `avail` derived
from the console width, so the printed string is width-dependent. On two terms
differing only in a window:

```
  width 10000   inertia/calls [1h]   inertia/calls [2h]    <- full, unique
  width    20   inertia/calls [1h]   inertia/calls [2h]
  width    12   inrt/cal [1...       inrt/cal [2...        <- abbreviated
  width     8   inrt/ca...           inrt/ca...            <- IDENTICAL
```

Copying from a narrow console yields a string that either matches nothing or
matches two things. `.compactLegend()` does not close this: it explains the
bracket tokens (`W = weighted`, `wdw = window`) and carries no map from a
truncated rendering back to the full one.

Worse than truncation, found while testing 3.1b: the collision arrives through
**abbreviation**, before any ellipsis appears. On
`~ 1 + indeg + outdeg + indeg(networkExog)` at width 16 the renderer shortens
`indeg` to `ideg` and the object to `networ`, so terms 2 and 4 both read
`ideg/networ` — identical, with no visual marker that anything was shortened
at all. An ellipsis at least tells the reader the string is partial; this does
not.

*(b) Anonymous functions collide.* Named function arguments render by name
(`inertia/calls [sqrt]` vs `[log]`, verified), but `.compactLegend()` carries
the line `fn = user-defined function`, so a non-symbol function collapses to
the bare token. `transformer_fn = \(x) x^2` and `\(x) x^3` on the same effect
and object both render `[t:fn]`. Narrow, but real, and nothing in the builder
prevents it.

By contrast the `coef()` labels **are** unique by construction — that is what
the `_1` in `ideg_networ_1` is doing — and so is the coefficient index.

Three consequences follow, and they are requirements rather than niceties:

1. The matcher MUST accept the `coef()` label and the index, because they are
   the only spellings that can always resolve a term. Without them an
   ambiguous compact string is a **dead end**: the user is told the name is
   ambiguous and has no way to say which one they meant.
2. The ambiguity error MUST **offer a unique spelling** for each candidate —
   its `coef()` label or its index — not merely list the colliding compact
   strings, which by construction all read the same.
3. `model_terms()` is more load-bearing than "a convenience for long models".
   Returning the **full** compact strings independent of console width, beside
   the unique labels, is arguably its primary job. The abort that names it is
   therefore the resolution path, not a courtesy.

Deliberately not attempted: making the compact string unique by construction.
It would mean rendering anonymous functions by a synthesized identity
(`fn#1`, `fn#2`) and forbidding truncation in the summary — the first invents a
label with no meaning outside one fit, the second breaks the display the
string exists for. The string stays optimized for reading; uniqueness lives in
the labels that were designed for it.

### D26 — `test_parameter()` tests offset terms; testing absent effects is deferred (user, 2026-07-29)

Two things called "the score test" were in scope, and they have very different
costs:

```
  FORM A -- terms already in the formula, held at a value
    dep ~ inertia + trans + offset(recip, coef = 0)
    "is the score at the imposed value zero?"
    needs: the fit + ONE evaluate_model() pass
    statistics: already preprocessed, the term is in the formula

  FORM B -- effects NOT in the model
    fit: dep ~ inertia + trans      then  test_parameter(fit, ~ . + recip)
    needs: preprocessing the AUGMENTED formula over the whole sequence,
           then one pass
    cost: a full preprocessing pass. On a large sequence this dominates
          everything else the diagnostics layer does.
```

**Form A is what RSiena actually does.** `includeEffects(eff, recip, fix =
TRUE, test = TRUE)` puts the effect *in* the model held at its value and
accumulates its score; the test asks whether that score is zero. It is not a
separate second use — it is the mechanism, and `offset(term, coef = value)` is
goldfish's spelling of `fix = TRUE`. Phase 2 therefore ships **Form A only**:
`test_parameter(fit)` tests the fit's offset terms at the values the formula
imposed.

Three consequences make this a better default than a reduced one:

- **It subsumes Form B's question at no extra cost, when planned.** "Should
  `recip` be in the model?" is answered by writing `offset(recip, coef = 0)`
  and estimating once — the same constrained MLE Form B would compute, with
  the candidate's statistics preprocessed in the same pass. Form B is only for
  "I already fitted and now wonder", and pays a second full preprocessing for
  the privilege.
- **The evaluator was already built for it.** D3/2.1 record that
  `evaluate_model()` is "unconditional on fixedness ... nothing is zeroed
  out — that is what makes the output usable as the constrained-model input of
  a score test". That is exactly Form A's requirement, and it is needed: the
  fit's own `final_score` has `score[id_fixed] <- 0` applied before the Newton
  step, so the score AT the offset coefficients exists nowhere on the fit. One
  pass at `coef(fit, complete = TRUE)` recovers it.
- **The flavored problem dissolves.** Candidates travel in the per-process
  formula because `offset()` already does (`offset_coef` is rejected outright
  for multi-process specs). No nested candidate argument, no per-process
  formula list — `test_parameter()` on a flavored fit maps over processes and
  reads whatever offsets each one declares. The asymmetry that motivated the
  question disappears.

**On merging preprocessed objects** (asked 2026-07-29): structurally an effect
is a slice of `initial_stats` plus update columns carrying its effect index, so
appending one to an existing preprocessed object is mechanical — interleave the
update buffers by event and rebuild the pointer vector, asserting the two share
an event schedule, intervals, dependence indicator and presence buffers. The
cost it saves is the smaller half: computing a new effect still requires walking
the whole sequence maintaining network and attribute state, and only the
*evaluation* of the other p effects is avoided. Adding one effect to a
three-effect model saves on the order of half the preprocessing, not the
sequence walk. That is real but does not change the order of magnitude, and it
is machinery nothing else in the package needs — which is itself the argument
for deferring Form B rather than building the merge for it.

**Deferred means NOT SUPPORTED, not "supported expensively"** (user asked
2026-07-29). Form B is not implemented in this change: `test_parameter()`
accepts no formula, runs no preprocessing, and aborts — naming the
`offset(term, coef = 0)` idiom — when asked for a term the fit does not have.
Nothing recomputes, because nothing is offered that would need to.

The two forms take **different arguments**, which is what makes the deferral
free of API debt rather than a promise to reinterpret one later:

```
  NOW (Form A)      test_parameter(fit)                     all offset terms
                    test_parameter(fit, effects = "recip/friendship")
                    `effects =` SELECTS AMONG TERMS THE FIT HAS,
                    resolved by D25's matcher -- the same vocabulary
                    `diagnose_*(effect =)` uses.
                    cost: 0 preprocessing passes, 1 evaluation pass.

  LATER (Form B)    test_parameter(fit, add = ~ . + recip)
                    `add =` NAMES TERMS THE FIT DOES NOT HAVE, and must be a
                    formula: an effect is a call, not a name --
                    `indeg(net, window = "15 minutes")` cannot be selected
                    from a list of existing labels.
                    cost: a full preprocessing pass over the sequence, then 1
                    evaluation pass.
```

Two argument names because they are two vocabularies (select vs specify) and
two cost classes. Adding `add =` later extends the signature; it does not
change what `effects =` means.

**Why the fixed score is zeroed at all, and what it costs** (asked
2026-07-29; recorded so the next reader of `score[id_fixed] <- 0` does not
re-derive it). The zeroing does **nothing** to the Newton step: the update
reads `score[id_unfixed]`, so the zeros are never indexed. And the step is the
correct constrained one — maximizing over the free block with the rest held
gives `dtheta_f = I_ff^-1 U_f`, the inverse of the free **submatrix**, which is
exactly what `informationMatrix[id_unfixed, id_unfixed]` computes; not the free
block of the full inverse, which would be wrong. Nothing needs adding to the
derivative or the information either: an offset enters the linear predictor, so
it is already inside every weight, probability and expected statistic, which is
what an offset *is*.

One line, three consumers, and only one of them wants it:

```
  score[id_fixed] <- 0
      |
      +--> the update           IGNORED     (indexes id_unfixed)
      +--> max_abs_score /      ESSENTIAL   U_x is generally != 0 at the
           score_rel_norm                   constrained optimum, so without
                                            zeroing `max|score| < tol` is
                                            never met and the loop never
                                            converges
      +--> final_score          LOSSY       the reported score, with holes
                                            exactly where a score test looks
```

So the fix is not to stop zeroing — it is that one vector serves the
convergence test and the report, and those want different things.

Where this puts goldfish relative to its neighbours is the interesting part:

| package | a fixed term is... | its score |
| --- | --- | --- |
| `glm`, `coxph`, `lme4` (`offset =`) | dropped from the parameter vector | does not exist |
| **goldfish** (`offset(t, coef = v)`) | **kept in the vector, held at v** | **computed, then discarded** |
| **RSiena** (`fix = TRUE, test = TRUE`) | kept in the vector, held | **computed and kept — this IS the score test** |

A glm-style offset has nothing to test, because the coefficient is not a
coefficient. RSiena keeps the term in the vector *precisely so* it can be
tested. goldfish already made RSiena's structural choice and then throws away
the one number that choice buys. That is the whole of the gap below.

**Settled (user, 2026-07-29): `test_parameter()` requires the preprocessed
statistics and one `evaluate_model()` pass, and aborts when it has neither.**
The information matrix on the fit is already the full one (`res$fisher`, never
masked, and `vcov()` re-extracts the free block itself), but the score is not:
`estimate_c_int()` applies `score[id_fixed] <- 0` before the Newton step, and
the reset dance carries the *zeroed* copy through `score.old`, so
`final_score` has holes exactly at the coefficients Form A wants to test.
Recovering them is the one evaluation pass, and that pass reads the
preprocessed object — so a fit estimated without `return_preprocessed = TRUE`
and handed no `preprocessed =` aborts, through the same guiding error naming
both routes that every other replay-needing diagnostic uses.

**Rejected: storing the unmasked score on the fit.** It would have put
`test_parameter()` in `residuals(type = "score")`'s tier — no replay object, no
pass, any fit — and the number is one estimation already had. Three reasons it
was not worth it:

- **One rule beats two.** `residuals(type = "response")`,
  `martingale(level = "dyad")` and exact-time `schoenfeld` without the
  primitive all already route through `resolve_preprocessed()` and abort with
  one guiding error. `test_parameter()` joining that tier means the answer to
  "which diagnostics need the statistics" is a list, not a list with an
  asterisk.
- **It touches the accept/reject reset.** The unmasked copy has to ride the
  same `score.old` dance, or a rejected final step leaves it describing a
  parameter vector the fit does not report. That is a delicate loop carrying
  every frozen coefficient baseline, edited for one diagnostic's convenience.
- **It buys the wrong path.** The friction lands on "I already fitted and now
  wonder", which is exactly the workflow Form B defers anyway. A user writing
  `offset(term, coef = 0)` to test a term is planning ahead by construction,
  and adds `return_preprocessed = TRUE` in the same breath once the
  documentation says so.

**A consequence worth naming: the `preprocessed =` contract stops being
split.** An earlier note here observed that `preprocessed =` would mean
something different for `test_parameter()` than for `residuals()` — the
*augmented* model's statistics rather than the fitted model's. That was true of
Form B only. Under Form A the tested term is in the formula, so the statistics
the test needs are the fit's own, and `preprocessed =` means on
`test_parameter()` exactly what it means everywhere else. Scoping to offsets
collapsed a contract split that would otherwise have needed documenting.

Form B stays a candidate for phase 3 beside the simulation-based GOF, where a
full replay is the ambient cost anyway and a
`compute_statistics(..., add_to = prep)` merge could be justified across
several consumers.

### D27 — Flavored fits: every diagnostic maps over processes and labels the rows (user, 2026-07-29)

A flavored specification is K independent fits — the competing-flavor
likelihood factorizes, which is why `vcov()` returns one component per process
rather than a block-diagonal matrix. Every diagnostic follows from that, and
the pattern already exists: `margin_table()`'s `flavored_result.goldfish`
method row-binds per fid and **appends** `flavor` and `family` columns (D16).

The rule, stated once so each surface does not re-decide it:

```
  returns a TABLE   -> flavored method row-binds per fid, appending
                       `flavor` / `family` columns (margin_table precedent)
                       diagnose_outliers, diagnose_changepoints,
                       diagnose_onset, the test_* per-effect tables

  returns a TEST    -> per process, plus the joint combination the
                       diagnostic-tests capability already specifies for
                       test_gof (Cauchy across blocks, blocks being
                       fid x submodel)

  PLOTTING          -> autograph facets on `flavor` / `family`; no separate
                       flavored plot method, since the appended columns are
                       what a facet needs
```

**The flavored output is ONE row-bound tibble, not a list of tibbles** (user
asked 2026-07-29). `margin_table()` settled this in D16 and nothing here
argues for a second answer: one object plots with a facet on `flavor`, composes
with dplyr directly, and does not make a consumer loop before it can filter.
The processes of a flavored specification model the same focal layer, so their
event columns align and the bind is well defined; row counts differ between a
rate process and its choice counterpart, which is exactly what the appended
columns are for.

The rule applies **per tibble, not per object**. An object that is a single
table (the `diagnose_*` returns) row-binds itself. An object that carries
several tables — `diagnose_onset()` with its parameter path and its accrual
curve, `test_gof()` with its per-effect statistics and its process paths — is
a classed *list* of tibbles under the plot-data contract, and its flavored form
row-binds **each component tibble**, keeping the container shape. So a consumer
never has to ask whether a flavored result nested one level deeper than a
single-process one.

Scalars that summarize a process — a joint omnibus p-value, a process count —
stay in the object's metadata rather than becoming a column repeated down every
row.

`test_gof()` is the only one the specs currently answer; `test_time()`,
`test_parameter()` and the `diagnose_*` family are extended to the same shape
here. Nothing pools across processes except a declared omnibus: the processes
have different effect sets and different event counts, and a pooled residual
or a shared scaling constant would assert a joint model that was never
estimated (the same reasoning that makes `scaled_schoenfeld`'s `n` per
sub-model, D21).

### D28 — The `diagnose_onset()` plot: what it draws, and how far (user, 2026-07-29)

Task 5.4 said "per-coefficient path panel (with stabilization marker) +
information-accrual panel" and left the geometry open. Measured on the
`calls` choice fit (`~ inertia + recip`, 439 events), the open parts are not
cosmetic:

| quantity | value |
|---|---|
| `stabilized_at` | 10 and 31 events, of 439 |
| onset region as a share of the x-axis | **7%** |
| full path y-range ÷ onset-window y-range | **4.0x** (inertia), **8.5x** (recip) |
| accrual at stabilization vs proportional | 0.0482 vs 0.0706 |

The path is a *bridge*: it returns to $\hat\theta$ at the end, so deleting
most of the sequence leaves few events and a wide late excursion. Drawn at
full range, the thing the diagnostic exists to show occupies 7% of the width
at a quarter to an eighth of the vertical resolution — the panel is mostly
the bridge tail, which says nothing about onset.

Settled:

1. **The path panel is windowed on the excursion, per coefficient**
   (settled 2026-07-29 after measurement). For each free coefficient:

   ```
   x_max = if (stabilized_at == 0)  n_events
           else min(n_events, max(ceiling(1.15 * stabilized_at), 10))
   ```

   Each clause was measured, not chosen:

   - **The all-zero case needs no compromise window, because it proves its
     own answer.** `stabilized_at == 0` means the drift never exceeded
     `tolerance` anywhere, so the *entire* path lies within
     $\pm\,\text{tolerance}\cdot\text{SE}$ by construction. Full range is
     therefore provably flat and is the correct picture — the message is
     "nothing drifted, anywhere", and there is no excursion to zoom into.
     (Flat *in SE units*: `tie/contiguity` on the fisheries fixture has a
     raw y-range of 1.0 with a standard error of 31.8. The band is what
     makes it read as flat, which is why (3) puts `std_error` on `path`.)
   - **Headroom is multiplicative, and 1.15 is free.** ggplot2's default
     continuous expansion is 5% either side, so `limits = c(0, 31)` gives a
     panel of $[-1.55, 32.55]$ and a marker at 31 sits 95.5% across — the
     "marker on the edge" worry was unfounded, and nothing is owed to
     cosmetics. What headroom buys is *evidential*: seeing the path stay
     settled. That is expensive and gets expensive fast, because the bridge
     resumes almost immediately after re-entry — on the cold `calls` fit
     w = 39 costs 1.24x, w = 47 1.77x, w = 62 3.10x; on the warm one w = 10
     costs 1.61x, w = 20 2.82x. But ~15% past stabilization is *exactly*
     free on both (31 → 35 and 5 → 7 are both 1.00x), the path still
     lingering inside its band. An absolute `+20` is not the same thing: it
     is 4x the warm fit's whole window and costs 2.8x there.
   - **A proportional floor was rejected.** `max(stabilized_at, n/3)` costs
     4.0x the y-resolution on the cold fit and 5.1x on the warm one, against
     4.9x and 8.0x for drawing everything — it puts back most of what this
     decision removes. It is also the wrong shape for scale: onset length
     tracks how long the endogenous statistics take to fill in, not sequence
     length, so at n = 5000 a third of the sequence is bridge tail. The
     absolute floor of 10 covers the case a proportional one was reaching
     for — every coefficient settling in 1-3 events, where a three-event
     panel shows nothing.

2. **The two axes are free — of each other, and across facets.**
   *Between panels:* the accrual panel is **full-range with the onset window
   shaded**, not truncated to it. Truncating would put the cold-start
   shortfall (0.0482 accrued against 0.0706 proportional) across the whole
   panel, but the diagonal only reads as a reference if the curve can be seen
   meeting it at $(n, 1)$, and the whole sequence is the natural frame for
   "does accrual ever catch up". The shaded region is $[0,
   \max(\texttt{stabilized\_at})]$ — "by here, every coefficient had settled"
   — which stays well defined however the path facets are windowed.
   *Across facets:* each coefficient's facet gets **its own** x range from
   the rule in (1). A window shared across facets reproduces the very
   squashing (1) exists to prevent, one level down: on the fisheries REM
   (`stabilized_at = 4/0/66/2/6`) a shared window of 66 draws three of five
   coefficients in 3-9% of their facet at 3.0x, 4.3x and 4.8x their own
   y-range.
3. **The accrual panel draws the proportional diagonal** $y = x/n$ as its
   reference. Every accrual curve is monotone from 0 to 1; the signal is the
   *departure* from proportionality, and without the diagonal the panel is
   decorative. `context$n_events` supplies it.
4. **A plot method may join the object's own component tibbles.** The
   stabilization marker lives on `summary` while the path lives on `path`, and
   joining them on the term key is the ordinary use of a documented relational
   shape, not the "assembly" the plot-data requirement forbids. This
   generalizes: `test_gof()` has the same per-effect-table-beside-longer-table
   shape, so the rule is stated once rather than per object. (The band needs
   no join — `reference` and `std_error` already ride on `path` — so the two
   are inconsistent in *convenience*, deliberately: repeating a per-term
   constant down a path column is cheap, repeating a scalar summary is noise.)
5. **Fixed coefficients are not drawn.** An `offset()` coefficient is a flat
   line at its imposed value with zero drift by construction; a facet spent on
   it is a facet not spent on a coefficient that moves. `path$fixed` is the
   column that says so.
6. **The x-axis is `dropped_events`, not `dropped_intervals`.** Same reasoning
   as D24: on a rate or REM fit most intervals are window closures, and an
   axis counting those says little about how much history has accumulated.
   Both columns ship; this states which one the plot uses so it is not
   re-decided at implementation time.
7. **Two panels, composed — not paged.** autograph's house answer for "one
   object, two views" is patchwork: `plot.ag_conv` composes a *faceted* trace
   panel beside a density panel (`wrap_plots(..., ncol = 2, widths = c(5,
   1))`), which is structurally what onset needs with the axis shared
   vertically instead. Printing several pages to the device is possible and
   RStudio/Positron would let a user page back through them, but it breaks the
   contract every other autograph method keeps — the method returns a ggplot,
   which is what makes `ggsave(plot(x))` and the `expect_s3_class(p,
   "ggplot")` tests work — so it is not the way to go. An argument
   selecting one of the two is still worth having for the many-coefficient
   case, where the composed figure grows a facet per free term; composed is
   the default, and the argument is the escape hatch rather than the primary
   interface.

**The argument is `view = c("both", "path", "accrual")`, and the name is a
convention, not a one-off (user, 2026-07-30).** The earlier sketch called it
`panel =`, which collides twice: "panel data" is a data type in this
package's statistical neighborhood, and "panel" is ggplot2's own word for a
facet — an argument that windows facets per coefficient (item 2) must not
share a name with them. `scope` was considered and rejected by D24's own
naming rule (a word already meaning something else in the same feature):
the `diagnose_*` print output already says "Computed over the dependent
intervals" for the `include_censored` choice — the scope of the analysis —
so `scope =` would put data-subsetting and figure-selection under one word.
`level` is taken outright by `residuals(level = c("actor", "dyad"))`.
`view` is unused in goldfish and autograph and says the right thing: one
object, several renderings. The generalization: **any autograph plot method
that offers several renderings of one goldfish object selects among them
with `view =`**, composed/complete default first in the vector, one value
per single rendering.

### D29 — Every diagnostic entry point is a generic, and the suite shares the names (user, 2026-07-29)

goldfish is split down the middle today: `margin_table()`, `model_terms()`
and `evaluate_model()` dispatch through `UseMethod()`, while
`diagnose_outliers()`, `diagnose_changepoints()` and `diagnose_onset()` are
plain functions. D6 already commits the `test_*` family to methods on
`result.goldfish` and on the specification fit. The rule, stated once:

> Every user-facing diagnostic entry point is an S3 generic dispatching on
> the fitted object, with a `default` method that aborts naming what it
> received.

Two reasons beyond consistency. D27 requires each diagnostic to map over the
processes of a flavored fit and row-bind the result — which *is* a second
method, so the generic is owed anyway. And a plain function forecloses the
suite question below, where a generic does not.

**The suite question, and it is not the one it first looked like.** Swept
2026-07-29 over the installed stocnet stack plus ergm and survival:

```
  RSiena 1.6.6   test_gof, test_parameter, test_time   <- ALL THREE, as
                                                          S3 GENERICS
  migraph        test_gof                              <- plain fn, deprecated
  manynet        clear      netrics clear      autograph clear
  ergm clear     survival clear
```

`migraph::test_gof(diff_model, diff_models)` is a plain function about
diffusion models, already `.Deprecated()` in favour of `migraph::test_fit` —
a nuisance collision, and on its way out. **RSiena is the finding.** It
publishes all three names as generics —

```
  test_gof(object, ...)      -> test_gof.sienaFit       wraps sienaGOF
  test_parameter(x, ...)     -> test_parameter.sienaFit wraps score.Test
  test_time(x, ...)          -> test_time.sienaFit      wraps sienaTimeTest
```

— and the semantics line up one for one with what this change is building.
D26 already observed that goldfish's `test_parameter()` *is* RSiena's score
test (`fix = TRUE, test = TRUE`), and `sienaTimeTest` is the same
time-heterogeneity question `test_time()` asks. So these are not names to
route around. They are **the same three questions asked of different model
classes**, and a sibling has already published the generics for them. That is
a convergence to join, not a collision to dodge.

The catch is mechanical: R finds a method through the generic it is
registered against. Two packages each defining their own `test_gof` generic
does not give one working surface — whichever is masked, its methods are not
found by the other's generic. So "define our own and hope" is not merely
untidy, it is broken in the both-attached case, which under stocnet is the
ordinary case.

**Measured, not reasoned** (two throwaway packages built and installed,
2026-07-29: `pkga` defines a `test_gof` generic and registers
`test_gof.result.goldfish`; `pkgb` defines its own `test_gof` generic):

```
  pkga alone                      -> "pkga method ran"
  pkgb attached AFTER pkga        -> ERROR: no applicable method for
                                     'test_gof' applied to an object of
                                     class "result.goldfish"
  pkga::test_gof(obj)             -> "pkga method ran"
```

So the hazard is precise: with both attached and RSiena attached **last**,
goldfish's method is invisible to the generic in scope even though goldfish
is attached and the method registered. Attach order decides, and
`goldfish::test_gof()` is the only reliable escape.

**Settled (user, 2026-07-29): goldfish defines the generics and the methods
itself, and carries that hazard, while a home for the shared stocnet
generics is worked out separately.** goldfish cannot fix this unilaterally
in a way that is right for the suite, and the interim is not broken — it is
correct for every user who has goldfish attached without RSiena, which is
the common case.

The routes remain, for whenever that conversation happens:

| where the generic lives | cost |
|---|---|
| import RSiena's | RSiena becomes a hard dependency of a peer, and its `R (>= 4.5.0)` raises goldfish's floor from 4.4.0 |
| move them to manynet | free for goldfish (already `Imports: manynet (>= 2.1.0)`) — but RSiena imports only Matrix/lattice/parallel/MASS/methods/xtable/network, so this asks it to take a dependency it has deliberately not taken |
| delayed registration | `Enhances: RSiena` + `S3method(RSiena::test_gof, result.goldfish)`. **Verified to close the hazard completely and to cost nothing**: both classes dispatch correctly whatever the attach order, and with the sibling package *uninstalled* the package still loads and dispatch still works. `Enhances` is neither Imports nor Suggests, and autograph already declares `Enhances: ergm, RSiena` for exactly this relationship |
| rename ours | abandons the convention D6 chose, and the semantic alignment argues the other way |

**One cheap thing to do now, because it makes that move a two-line change
instead of a breaking rename.** R CMD check enforces S3 generic/method
argument consistency, so registering against RSiena's generics later would
*force* their first-argument names on goldfish's methods:

```
  RSiena:  test_gof(object, ...)   test_parameter(x, ...)   test_time(x, ...)
```

goldfish's own convention is `object` where a stats generic dictates it
(`residuals`, `fitted`, `predict`) and `x` everywhere else (`margin_table`,
`model_terms`, `evaluate_model`, `diagnose_*`). Two of the three already
agree; the whole exposure is **one argument name on one function**. Naming
`test_gof()`'s first argument `object` costs one local inconsistency now and
keeps the door open; naming it `x` closes the door behind a breaking rename.

The same sweep found **no** collision on `diagnose_*`, `margin_table`,
`model_terms` or `evaluate_model` anywhere in the stack, so the exposure is
specific to the `test_*` family, not general.

### D30 — Diagnostic tables: stable schema, flagged-subset printing, defining-column demotion (user, 2026-07-30)

The homogeneity rule for every `test_*` and `diagnose_*` table, stated once:
**the schema never varies with the result; emptiness lives in content, never
in shape.** A no-outlier `diagnose_outliers()` result is the full-length
D24 table with `outlier` all `FALSE` — not a shorter table, not different
columns. This restates D24's one-row-per-interval contract and D16's
always-present-columns rule (`expected_count` is `NA`, never absent) as one
principle the remaining surfaces inherit instead of re-deciding.

**The print shows the header count plus only the flagged rows.** The count
is derived from the logical flag column (`outlier`, `cpt`); zero flagged
means the header reports zero and no rows are listed. Today's
`print_diagnose_table()` prints `as_tibble(x)` — the first ten intervals of
the full series, which are almost never the flagged ones — so the change is
strictly better even before the homogeneity argument.

**The bug that forced the rest of the decision** (reproduced 2026-07-29): a
*column* subset that drops `outlier` keeps the class — D17's experiment
established that tibble's `[` preserves a prepended class, which is the
feature — so `print` reads `x$outlier`, tibble's `$` warns and returns
`NULL`, `sum(NULL, na.rm = TRUE)` is 0, and the header reports
**"0 outliers identified" while two are present**. A silent wrong number,
not just a warning; `print.diagnose_changepoints` reading `x$cpt` has the
same exposure.

**The fix is demotion, not a print guard.** Each diagnostic class declares
its **defining columns** at construction — `new_diagnostic_table()` gains
the declaration, carried in the D18 metadata — and one shared pair of
`[` / `dplyr_reconstruct()` helpers returns a **plain tibble** whenever an
operation yields a table lacking any of them. This is how `grouped_df`
behaves, and D17 already accepted the principle for
`group_by() |> summarise()` ("correct — a summary is no longer a
`margin_table`"); losing the column that defines the object is the same
case. It revises D17's "tibble base is safe" bullet: class preservation
through `[` is both the feature and the hazard, and demotion is what bounds
it. One rule covers what would otherwise be per-class guards:
`diagnose_outliers`/`outlier`, `diagnose_changepoints`/`cpt`,
`margin_table`/`observed`, and the `.series` column the plot methods read.

### D31 — Validation references are classical equivalent fits; other REM packages are harness documentation only (user, 2026-07-30)

Task 2.6 as written made `remstimate::diagnostics()` a NOT_CRAN test, which
means adding remify + remstats + remstimate to Suggests for one test.
Dropped: **no REM package enters the validation chain**. The already-run
harness comparison (`.plan/residuals_comparison.qmd`, coefficient parity
≤ 7e-07) stays as recorded one-time evidence, and the harness gains the
output-mapping note that question (e) was missing —
`standardized_residuals` ↔ `scaled_schoenfeld` (per submodel in
actor-oriented mode), `recall`/`top_pct` ↔ ranks-derived recall,
waiting-time Q-Q ↔ `cox_snell`, margins ↔ nothing (the §0.4 novelty
claim). No test derives from any of it.

**The references are classical fits of the same likelihoods** (user's
correction, 2026-07-30 — the map is broader than "the ordinal REM"):

```
  ordinal REM             coxph   (Cox partial likelihood; distinct event
                                   times, dyads in start/stop form)
  choice                  survival::clogit  (a conditional logit IS a
                                   stratified Cox; one stratum per event)
  ordinal rate            clogit over the active senders
  coordination            clogit over pairs at risk ONLY IF the pair weight
                                   is log-linear in the pair statistics —
                                   verify in the harness before claiming
  exact-time rate / REM   stats::glm poisson with offset log(dt)
                                   (piecewise-exponential equivalence;
                                   fixture needs every dt > 0)
```

Exact-time Schoenfeld needs no reference of its own: D20's identity — the
exact-time Schoenfeld rows *are* the conditional/ordinal score rows —
carries the coxph/clogit validation over. The timing-side distributional
diagnostics (Cox–Snell Exp(1) Q-Q, KS) have no package reference and need
none: the compensator identity (sums to the dependent-event count at the
MLE, already a spec scenario) and the D7 null-coverage simulations are
their validation. That answers "where does the time-to-event test come
from": the glm equivalence validates the likelihood side; identities plus
simulation validate the residual side.

**Shared-θ comparisons remove optimizer noise entirely**: goldfish side via
`evaluate_model(at =)`, survival side via
`coxph(init = , control = coxph.control(iter.max = 0))`, glm side by
evaluating the Poisson likelihood/score at fixed coefficients. This is what
makes the residual-matrix comparison the one that pins the
Grambsch–Therneau convention (it would have caught the ×n error §0.3(a)
fixed on paper), with the per-transform statistic/p-value comparison of
`test_time(method = "trend")` against the `cox.zph` table as the
end-to-end check on top. Note for the harness: on the Cox-expressible
fixtures every likelihood event is a "death", so cox.zph's event count and
D21's dependent-event count coincide by construction — the fixture cannot
distinguish those conventions, which is precisely why the shared-θ matrix
comparison is the load-bearing one.

**Storage rule.** survival-derived numbers enter the repo only as **frozen
reference files minted by the harness**, with provenance recorded beside
them (survival/R versions, date, fixture); the NOT_CRAN tests compare
goldfish against the frozen values at the ordinary 1e-6 baseline
discipline (goldfish-vs-frozen is a reproducibility check, not a
cross-implementation one). The glm-based equivalences run **live**:
`stats` ships with R, so there is no Suggests cost and no provenance
question, and at shared θ the comparison is deterministic.

**Two documented tolerances** (question (d)): shared-θ comparisons are
measured once in the harness and documented one order of magnitude above
the measured agreement; independently-optimized coefficient parity is a
**harness claim, never a test gate** (measured ≤ 7e-07, documented 1e-6).

**Self-contained invariants are preferred wherever they pin the same fact**
(user): Schoenfeld rows sum to zero at the MLE (already spec'd); the mean
of the scaled Schoenfeld rows equals θ̂; and the scale-pinning two-regime
fixture — a synthetic sequence whose true coefficient jumps mid-sequence,
where the within-period means of the full-fit scaled residuals must track
each period's separately-refit coefficient, which any scale-convention
error breaks by exactly its factor.

### D32 — `test_gof(clock =)` selects the reference distribution, not the scale (user, 2026-07-30)

**Resolves the OPEN question below.** Explore session against both
reference implementations, read 2026-07-30: Boschi's own code
(`GOF/00-Functions/functions.R` — `W <- cumsum(Psi.tilde)/sqrt(n.e)`
standardized via `ginv(crossprod(Psi.tilde)/n.e)`) and `amorem` 1.0.0
(`R/gof.R` — `J <- crossprod(G_k)/n`) both standardize by the **OPG of
the per-event contributions**, not an observed-information diagonal, and
both use `u = k/n` only — no information clock exists in either. So the
spec's `I_dd`-primary standardization silently deviated from the test it
cites, and the OPEN table's measured divergence (ratio up to 1.37,
`recip` crossing the 1%/5% boundary) is the size of that deviation:

```
  effect        inertia   recip   trans      (social_evolution calls,
  I_dd            40.62   45.15   23.91       ~ inertia + recip + trans,
  sum s_kd^2      47.43   61.71   22.48       choice, n = 439)
  ratio            1.17    1.37    0.94

  sup|W| via I_dd  1.224   1.727   0.675    -> p ~ 0.10, 0.005, 0.75
  sup|W| via OPG   1.133   1.477   0.696    -> p ~ 0.15, 0.025, 0.72
```

**Why neither OPEN reading survives.** A supremum reads only the set of
path values; a time change relabels their x-positions. A clock can
therefore reach a sup test through exactly three channels: (i) the
x-placement — plot honesty only; (ii) the **reference distribution** of
the discrete sup — a Brownian bridge observed on *this* grid, versus the
continuous/uniform-grid sup the Kolmogorov formula assumes; (iii)
u-weighted functionals (CvM/AD), which integrate `du`. Reading B ("scale
per clock") uses none of these: swapping `I_dd` for `OPG_n` is a
variance-estimator change, orthogonal to time — the p-value difference
it manufactures measures the information-matrix discrepancy (the White
IM-test direction), not accrual, and once the event clock adopts the
reference implementations' OPG scale, B collapses into A. Reading A
makes the clock decorative and leaves the cold-start scenario
unsatisfiable.

**The decision**, three parts:

1. **One scale, the reference's.** `J_d` is the empirical per-event
   variance of the centered score contributions (the per-effect OPG
   scale both reference implementations use); `I_dd/n` is demoted to a
   documented asymptotically-equivalent alternative — inverting the
   spec's previous primary/fallback order. The statistic `T_d` is
   identical under both clocks **by construction, and documented as
   such**.
2. **Event clock** = `u_k = k/n` with the analytic Kolmogorov p-value:
   exactly Boschi–Wit, reference parity preserved.
3. **Information clock** = `u_k = OPG_d(k)/OPG_d(n)` for the axis, and
   the p-value from a **simulated reference on the observed grid**:
   replicated Gaussian increments with variances
   `Δu_k = s_kd²/OPG_d(n)` (equivalently, standard-normal multipliers
   `g_k · |s_kd|`), centered to end at zero, supremum recorded. This is
   Lin–Wei–Ying (1993) multiplier resampling specialized to the fixed
   observed path — the canonical survival-analysis reference when the
   analytic limit's accrual assumption fails — and the analytic
   Kolmogorov formula is its proportional-accrual special case. Zero
   evaluation passes (stored scores only), vectorized over
   replications, replication count exposed as an argument, draws
   through the session RNG so `set.seed()` reproduces them.

**Why this satisfies the cold-start scenario honestly.** Under
concentrated accrual the true null law of the discrete sup is that of a
bridge observed on the information grid — stochastically smaller than
the continuous sup the analytic formula assumes — so event-clock
p-values are conservative (deviate toward 1) while the on-grid reference
stays uniform. Same statistic, different p-values, and the difference is
the accrual correction and nothing else.

**Verification consequence.** With near-uniform accrual the two
references coincide, so the cold-start fixture must be *shown*
concentrated (its own accrual curve) before the coverage separation is
asserted — which is also what keeps D7's clock-choice workflow
(`diagnose_onset()` flat segment → information clock) meaningful: the
accrual curve predicts in advance whether the clocks will visibly
separate. Named but not shipped: a CvM/AD functional, where the clock
enters even asymptotically through `du` — the future lever if the grid
correction proves too weak at realistic n.

### D33 — `test_gof()` ships experimental, and the omnibus is computed but not printed (user, 2026-07-30)

Two consequences of one gap. The per-effect test earned its place with a
measured null-coverage study (300 replications, two clocks, the accrual
concentration verified first). The **omnibus** was implemented beside it and
never simulated at all, and the only worked example — the four-block flavored
fixture — returns a joint p of 0.99999989 that one term of six supplies
entirely, because `tan(pi(0.5 - p))` diverges at *both* ends and a p-value near
1 dominates exactly as a tiny one does. Written up with the measurements in
ADR-0002/ADR-0003 of the decision record and in
`.plan/sp/gof_boschi_implement.md` §5.

**The decision, two parts:**

1. **The combination is computed and carried, but not printed.** `x$omnibus`
   and `attr(x, "context")$joint` stay exactly as they are — the autograph data
   contract is untouched, and the validation study ADR-0003 calls for can run
   against shipped objects rather than reimplementing the combination. The
   print methods report per-effect rows grouped by block and stop there. A
   flavored fit therefore reports its **individual** tests and no combination.
   Un-suppressing later is a print-only change, which is the reason this beats
   removing the component: the alternative breaks the object shape twice, once
   now and once when the evidence arrives.
2. **`test_gof()` carries `lifecycle::badge("experimental")`**, the badge six
   surfaces in the package already use (`as_goldfish()`, `add_flavor()`,
   `make_specification()`, `state_at()`, the `optimizer` argument). It is the
   honest signal for a function whose per-effect surface is validated, whose
   combination surface is not, and whose intercept row (D34) is an extension
   beyond the reference implementation.

Not chosen: dropping the omnibus component outright — it breaks the shape
twice and forces the study to recompute what the package already knows how to
compute. Not chosen either: suppressing only the cross-block joint while
keeping the per-block rows — the measured domination is identical at both
levels, so that line falls where the evidence does not.

**How long the suppression stands.** The validation study is scheduled
**post-2.0.0** (user, 2026-07-30), so this is the shipped state for the whole
2.0.0 line, not a few weeks' caution. That is what the pairing buys: the
release presents no unvalidated model-level verdict, while the quantity stays
on the object so the post-release study runs against shipped fits rather than
a reimplementation. Task 4.8 is **not** the study's home — it is inside the
release and would have to ship before the study exists.

### D34 — The intercept is a tested effect, and its row is the counting-process martingale (user, 2026-07-30)

The time intercept reaches the kernels as a literal column of ones
(`prepare_statslist()` prepends `cbind(1, ...)` for the sender families, a
ones-slice for REM), so no kernel special-cases it and neither does
`test_gof()`: it is column 1 of `event_scores` and enters the default tested
set as any other free coefficient. What that produces is worth stating,
because it is not what the reference implementation tests.

On the exact-time families the score row is `x_obs - Dt * sum_i lambda_i x_i`
on a dependent interval and `-Dt * sum_i lambda_i x_i` on a right-censored one.
With `x = 1` the two branches collapse to

```
  s_k(Intercept) = dN_k - Dt_k * total_rate_k
                 = dN_k - Lambda_k        (the Cox-Snell compensator)
```

verified bit-for-bit against `residuals(fit, type = "cox_snell")`. Its
cumulative sum is therefore `N(t) - Lambda(t)`, the counting-process
martingale, and the score equation `sum_k s_k = 0` is `N(T) = Lambda(T)`. Every
other effect's cumulative score is the same object weighted by that effect's
statistic, so the intercept is the **unweighted member of the family** — the
plain martingale residual rather than a covariate-weighted one.

**The consequences that shape the documentation:**

- **Composition versus volume.** A covariate row asks whether the model is
  right about *who* acts; the intercept row asks whether it is right about *how
  many* events and *when*. They route a user to different remedies — a
  wandering intercept calls for a time-varying baseline, a period split, or a
  corrected presence schedule, not for another choice effect.
- **`rate_ordered` has no such row and cannot.** The ordinal families model
  who, conditional on when; the intercept cancels in the softmax and a constant
  column would give `s_k = 1 - sum_i p_i = 0` identically — the degeneracy
  `test_gof()` already aborts on. goldfish drops it upstream with a warning, so
  the abort is unreachable by that route. On those families the martingale is
  the discrete event-index one, `sum (observed - expected)`, with no
  compensator and no censored intervals.
- **This is outside the reference implementation's scope.** `amorem` 1.0.0 fits
  `one ~ d_stat1 + ... - 1` on case-control *differenced* covariates, so an
  intercept differences to zero and is excluded by the formula as well — the
  same reason a Cox partial likelihood has no baseline. goldfish's parametric
  baseline is an extra assumption relative to Cox, and this row is the test of
  it: a cost that buys a diagnostic.
- **It is the row most likely to need `clock = "information"`.** Its increment
  variance is approximately `Lambda_k`, which swings with interval length, so
  on irregular event times it has the coarsest effective grid of any effect.

Also noted while reading `amorem`: it *forces* the bridge
(`cum_centered <- cum - outer(u, cum[n, ])`), where goldfish uses the raw
cumulative sum and lets the maximum deliver the endpoint. goldfish's
"process ends at zero" scenario is a real check for that reason and would be
vacuous under the other convention; the difference is deliberate and stays.

### D35 — `test_time()` is exact, needs one evaluator pass, and OPG survives only where nothing is tested (user, 2026-07-31)

**What forced this.** Task 4.5 said `test_time(method = "trend")` computes its
zero-slope tests "from stored primitives only", and the frozen `cox_zph`
reference was minted for it to match. The two are incompatible. Modern
`survival::cox.zph` is an **exact score test of the augmented model**
`[X, X·g(t)]` — `test[ii] <- drop(solve(imat, u) %*% u)` — whose information
blocks are `I_12 = Σ_k g_k Cov_k` and `I_22 = Σ_k g_k² Cov_k`, i.e. **per-event
risk-set covariances**. survival makes a dedicated C call (`Czph1`) to get
them. goldfish stores per-event *scores* (first moments) and the *total*
information; per-event second moments are stored nowhere, so the reference is
unreachable from stored primitives.

**Why precomputing is impossible, not merely unattractive.** The weights are
knowable in advance — `g(t)` for `identity`/`rank`/`km` is a function of the
event times alone, no parameters — but the thing being weighted is not.
`Cov_k` is the risk-set covariance under the *fitted* probabilities, and
preprocessing has no parameter vector. The earliest a θ exists is the final
Newton iterate, so "in advance" can only mean *during estimation*, never during
preprocessing. Accumulating at estimation would also serve only `trend`: the
period grouping is a user choice at diagnostic time, so the on-demand path has
to exist regardless, and building both means two code paths for one quantity.
Storing per-event blocks was already refused by the `periods` requirement
("without storing per-event matrices").

**The decision, four parts:**

1. **One route: an on-demand evaluator pass.** `evaluate_model()` becomes the
   only place with access to per-event information, which follows from D3
   naming it the single shared evaluator. `test_time()` therefore **requires
   the model's statistics** — attached by
   `estimate_*(return_preprocessed = TRUE)` or supplied through
   `preprocessed =` — and aborts with the same guiding error the other
   replay-needing diagnostics raise.
2. **No `information =` argument on `test_time()`.** Both methods use expected
   information. An argument with one legal value is vestigial, and the pre-2.0.0
   cost asymmetry says to ship the narrow surface: **omitting a value now is
   free to add later, shipping one now is a deprecation cycle to remove.**
3. **OPG survives only in `diagnose_onset()`**, on a principle worth stating
   once: *OPG is acceptable where nothing is being tested, and not where
   something is.* An accrual share is a description and its calibration
   weakness does not apply — the onset requirement already says so. A p-value is
   a claim, and the two OPG variants that were on offer are not even equally
   defensible: the periods one over-rejects in finite samples, and the trend one
   is the Grambsch–Therneau statistic that survival **retired** in its 2019
   rewrite.
4. **The accumulator takes weights, not a grouping.** Task 4.6 said "one flag
   taking a caller-supplied grouping serves both", which does not work: no
   grouping expresses `g(t)`. Weights generalize — a grouping is a set of
   indicator columns — and the same accumulation serves both methods:

   ```
     periods   augment x_d · 1{k in period j}   weights = J indicator columns
               I_1,(d,j) = Σ_{k∈j} Cov_k        (indicators are idempotent, so
               I off-diagonal in j is 0          J columns, not 2J)

     trend     augment x_d · g(t_k)             weights = [g, g²]
               I_12 = Σ g_k Cov_k
               I_22 = Σ g_k² Cov_k
   ```

**Three facts that make this cheap.** Every kernel *already* materializes
`fisher_current_event`, a p × p per-event block, and discards it after
`fisher += fisher_current_event`; the accumulation is a scalar multiply-add on
a matrix already in hand, with no new risk-set walk. A `if (w == 0) continue`
guard in the inner loop makes the indicator case `O(n·p²)` rather than
`O(n·J·p²)`, so periods needs no specialized path. And because centering is
linear with `Σ Cov_k = I` already known,
`Σ(g_k − ḡ)Cov_k = Σ g_k Cov_k − ḡ·I` — the kernel accumulates uncentered and
`test_time()` centers in R, so the kernel never learns what a transform or a
period is.

**Consequence for ordering**: 4.6 (the kernel visit) now gates 4.5. They land
together.

**Evidence caveat, recorded so it is not over-read.** The Grambsch–Therneau
statistic was reconstructed from memory twice during this investigation, the
first attempt off by ~900x and the second giving 3.6x-44x versus `cox.zph` on a
400-event fixture. Those numbers are weak evidence about GT and strong evidence
only about the operative fact: the reference could not be reproduced from
stored primitives. Not shipping the OPG variants has the side benefit that
nothing depends on getting GT right.

### D36 — `evaluate_model()` gains weighted information, as public API (user, 2026-07-31)

D35 makes `evaluate_model()` the sole door to per-event information, and it is
exported, so the additions are API from 2.0.0:

```
evaluate_model(x, at, return, preprocessed, weights = NULL, ...)

  return gains
    "weighted_information"      requires `weights` (n x m numeric matrix)
                                -> p x p x m array, third dimension named by
                                   colnames(weights)
    "event_information_trace"   no weights
                                -> length-n vector of tr(I_k)
```

`weights` is **public** rather than an internal path with `test_time()` as its
only caller (user, 2026-07-31). The evaluator is already documented as the
shared low-level pass, and a weighted-information return is what lets a user
write a diagnostic goldfish does not ship. The cost is that the contract is
API: the array shape and the meaning of a weight column are fixed at 2.0.0.

Both additions are reversible in the cheap direction — `EVALUATE_QUANTITIES` is
a widen-only vocabulary and `weights` has a `NULL` default, so a later value or
argument breaks nothing.

`"event_information_trace"` is a separate return rather than a weights case
because per-event traces cannot be expressed as a weighted sum of blocks:
asking for one block per event would be the per-event storage this design
refused. It is `n` doubles, and it is what
`diagnose_onset(information = "expected")` reads — the exact counterpart of the
OPG curve's `rowSums(scores^2)`, which is itself the trace of the outer-product
contribution.

**The naming question is settled (user, 2026-07-31), and the premise it was
raised on was wrong.** `event_scores` is not *adjective*-first, it is
*modifier*-first — the same shape as `information_trace`, whose head noun is
`trace`. Read that way the whole vocabulary is uniform (`interval_loglik`,
`conditional_scores`, `total_rate`, `n_opportunities`: modifiers first, head
noun last) and both proposed names already conformed. The real inconsistency
was elsewhere: **granularity was invisible.** `event_scores` announces its
length-`n` indexing and `information_trace` did not, though the two are indexed
identically. So the rule is stated as *modifiers first, head noun last, and the
index set is named whenever the return is per-interval*, and the trace becomes
**`event_information_trace`**. `weighted_information` is unchanged — it is a
`p x p x m` aggregate, indexed by weight column rather than by interval, so it
has no granularity to announce.

## Risks / Trade-offs

- [BREAKING class rename of `diagnose_*` returns] → goldfish and autograph
  are versioned together under stocnet; NEWS entries in both; autograph
  branch merges before/with the goldfish release. (Revised by D17: the new
  classes are `diagnose_outliers`/`diagnose_changepoints` — snake_case,
  constructor-named; the previously sketched alias class is dropped under
  the dev-line-only rule, since the current `diagnostic.goldfish` class
  never shipped to CRAN)
  vector if needed.
- [In-pass C++ additions touch all engines; risk of perturbing baselines]
  → additions are read-only accumulators behind flags default-off;
  NOT_CRAN=true baseline suite must PASS (not SKIP) at every commit;
  cpp-recompile skill after every src/ edit.
- [`diagnostics` deprecation breaks scripts using the old flags] → the two
  publicly-shipped flags (`return_interval_loglik`, `return_probabilities`)
  get lifecycle soft deprecation with mapping, at least one release cycle;
  `return_event_scores` never shipped publicly, so its outright removal can
  break no released script (backend-parity D11).
- [Scaled-Schoenfeld scaling constants differ across references
  (Grambsch–Therneau variants)] → shared-θ cross-check against
  `survival::cox.zph` on the Cox-expressible fixtures via frozen
  references, plus the self-contained two-regime scale invariant (D31;
  the remstimate comparison is harness documentation, not a test).
- [Kolmogorov approximation poor for small n] → document; verification
  includes null-coverage simulation at n ∈ {1000, 5000} mirroring
  Boschi & Wit §4 (NOT_CRAN tests).
- [Memory: `return_preprocessed = TRUE` on large data] → documented cost;
  default FALSE; cli message states the object size when attached.
- [autograph coordination: two repos, one feature] → goldfish tasks only
  emit stable classes/columns (the contract in `diagnostic-plot-classes`
  spec); autograph tasks are additive plot methods on a feature branch;
  goldfish never imports autograph (Suggests at most, examples gated).

## Migration Plan

1. goldfish phase 1 lands behind default-compatible options (`diagnostics`
   default preserves current behavior; new methods are additive except the
   `diagnose_*` class rename).
2. autograph `feature/goldfish-diag` branch developed in parallel; merged
   to autograph `develop` when goldfish phase 2 is complete.
3. The two public return flags removed no earlier than one minor release
   after deprecation; `return_event_scores` is already gone at 2.0.0 (never
   public, no cycle owed).
4. Rollback: every task is one focused commit with green tests; the C++
   accumulators are flag-gated so reverting R surface alone is safe.

## Open Questions

*(both resolved 2026-07-19 explore session)*

- ~~Class names~~ — the suffix convention: `test_gof.goldfish`,
  `test_time.goldfish`, `test_parameter.goldfish` — as D10 already names in
  the autograph plot methods, matching the existing
  `outliers.goldfish`/`changepoints.goldfish` pattern.
- ~~Wald form~~ — trails post-release (task 4.4 descoped); phase 2 ships the
  score/LM test only, with `lmtest::waldtest()` documented for nested pairs.

- ~~OPEN (raised 2026-07-30, task 4.1 paused on it) — what does
  `test_gof(clock =)` actually change?~~ — **resolved by D32 (same
  day)**: neither of the two readings weighed here (one scale / scale
  per clock) survived contact with the reference implementations, which
  both standardize by OPG and have no information clock at all. The
  statistic is clock-invariant by construction; the clock selects the
  **reference distribution** (analytic Kolmogorov vs the Lin–Wei–Ying
  multiplier simulation on the observed information grid). The measured
  scale table and the bridge-endpoint non-discrimination note moved
  into D32.
