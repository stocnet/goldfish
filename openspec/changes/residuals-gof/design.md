# Design — residuals-gof

## Context

`.plan/residuals-gof.md` is the statistical reference for this change: it
derives every residual formula (deviance, Cox–Snell, Schoenfeld/scaled,
score, martingale, response), the Boschi–Wit cumulative-score bridge test,
the score/LM test, and the per-submodel conditional-residual argument for
DyNAM, and it records the survey of relevent, remstimate, mlogit, and the
Boschi–Wit GOF implementation. Its §0 (added 2026-07-24 after an
agent-verified literature pass; bibliography in `.plan/residuals-gof.bib`)
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
observed vs expected counts as defined in `.plan/residuals-gof.md` §0.2,
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
`.plan/residuals-gof.md` §0.3(b), §7.4.

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
start, a series the changepoint detector never sees). Hence a dedicated
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
   \operatorname{tr}(I)$ (OPG from stored scores by default; exact via
   one evaluator pass): the initial flat segment shows when the data
   begins identifying the endogenous parameters.

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
Literature grounding: `.plan/residuals-gof.md` §0.4 and
`.plan/residuals-gof.bib` — no surveyed package ships analytic actor
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

## Risks / Trade-offs

- [BREAKING class rename of `diagnose_*` returns] → goldfish and autograph
  are versioned together under stocnet; NEWS entries in both; autograph
  branch merges before/with the goldfish release; `print` method kept for
  both old and new class names during one release via an alias class
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
  (Grambsch–Therneau variants)] → cross-check against `survival::cox.zph`
  on a REM expressible as a Cox model and against
  `remstimate::diagnostics()` on a shared dataset (verification tasks).
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
