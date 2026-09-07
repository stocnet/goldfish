# residual-methods Specification

## Purpose
TBD - created by archiving change residuals-gof. Update Purpose after archive.
## Requirements
### Requirement: residuals method with coxph-style types
`residuals.goldfishFit(object, type, preprocessed = NULL, ...)` SHALL
support `type = c("deviance", "schoenfeld", "scaled_schoenfeld", "score",
"cox_snell", "response", "martingale", "dfbeta", "dfbetas", "cooks")` with
`"deviance"` as default. Definitions: deviance = `-2 * intervalLogL`
(documented note: for exact-time submodels `intervalLogL` is a
log-density, so deviance values can be negative); schoenfeld = per-event
observed-minus-expected statistic rows — the stored `event_scores` for
multinomial submodels, whose likelihood is already conditional; for
exact-time submodels the stored `conditional_scores` primitive, which is
the observed-minus-risk-set-weighted-mean rows with weights
`lambda / sum lambda` (no exposure term — these do NOT sum to zero at the
MLE, only the score rows do, and the documentation SHALL say so). On an
exact-time fit that did not store that primitive, schoenfeld SHALL be
recomputed through `evaluate_model()` under the replay rules, and SHALL
abort — naming both the primitive and the replay routes — only when no
preprocessed statistics are available either. It SHALL NOT fall back to
returning the exposure-carrying score rows under the Schoenfeld name: the
exposure term cannot be removed from a stored score row, which is one
vector equation in two unknown vectors; scaled_schoenfeld =
`coef(object) + solve(Vbar) %*% s_k` with `Vbar = I / n` the average
per-event observed information at the estimate (equivalently
`coef(object) + n * solve(I) %*% s_k`, Grambsch-Therneau; `n` is the
event count of the submodel being diagnosed — the constant is never
shared across submodels); score = the per-event score increments (equal
to schoenfeld for ordinal/choice submodels, including the exposure term
for exact-time submodels); cox_snell = interevent time times the total
fitted rate (exact-time rate/REM submodels only; requesting it elsewhere
aborts with a cli error whose stated reason names the requesting
sub-model's own likelihood family rather than asserting a single family
for every case that lacks a compensator) — a **stored-primitive** type,
since the fit
carries the interval clock and the `"loglik"` primitive carries
`total_rate`, so at the fitted estimate it SHALL NOT trigger an evaluation
pass and only an evaluation at another parameter vector SHALL; on a fit
predating the interval clock the elapsed time SHALL be recovered from the
stored components through the compensator identity rather than by requiring
a replay object; response = observed indicator minus fitted
probability per alternative, using the conditional multinomial
probabilities `lambda / sum lambda` for exact-time submodels; martingale
= per-actor observed minus expected counts exactly as defined by the
`"margins"` primitive (both sender and receiver margins on REM fits),
with `level = "dyad"` returning the per-dyad observed-minus-expected map
via `evaluate_model()` (never a stored primitive);
dfbeta/dfbetas = `solve(I) %*% s_k` (scaled by standard errors for
dfbetas); cooks = `t(s_k) %*% solve(I) %*% s_k`, the scalar one-step
self-influence (Cook's-distance analog; the frequentist counterpart of a
per-event influence flag such as PSIS-LOO's Pareto k). dfbeta, dfbetas,
and cooks are stored-primitive types (scores plus the stored information
matrix), as are deviance, score, schoenfeld (given the primitive its
family needs) and cox_snell. Types computable from stored primitives SHALL
NOT trigger an evaluation pass; the remaining types SHALL recompute via
`evaluate_model()` under the diagnostic-primitives replay rules. For DyNAM
fits all residuals SHALL be conditional per submodel (rate residuals over
the sender risk set, choice residuals over the receiver risk set given the
observed sender).

Every residual type SHALL return one value per **dependent event**, on every
sub-model, so that `residuals()` has one length and pairs with the fit's
dependent events without further alignment. The types defined per interval —
deviance, score and cox_snell — SHALL be accumulated over the intervals
between consecutive dependent events rather than returned as the pieces. That
accumulation SHALL be exact rather than a restriction to the dependent rows:
a right-censored interval carries a genuine likelihood and score contribution,
so summing it into the span that ends in the next event preserves the totals
the identities rest on, where discarding it does not. In particular the
accumulated score rows SHALL still sum to zero at the maximum, and the
accumulated cox_snell values SHALL be the compensator between consecutive
events, which is the quantity the time-rescaling theorem makes unit
exponential — the per-interval pieces are not.

The types already defined per event SHALL be unchanged: schoenfeld and
scaled_schoenfeld, which realize no observed alternative on a right-censored
interval; response; and martingale, which is per actor. Types derived from the
score SHALL be derived from the accumulated rows — dfbeta and dfbetas by a
linear map, which commutes with the accumulation, and cooks by evaluating its
quadratic form on the accumulated row, which does not commute and SHALL NOT be
obtained by summing per-interval values.

#### Scenario: every type returns one value per event
- **WHEN** each residual type is computed on a windowed rate fit whose
  intervals outnumber its dependent events
- **THEN** each returns one value per dependent event

#### Scenario: accumulation preserves the score identity
- **WHEN** the score residuals of such a fit are accumulated between events
- **THEN** their column sums equal the column sums over all intervals, and are
  zero at the maximum within the convergence tolerance, which the sums over
  the dependent intervals alone are not

#### Scenario: the augmented table matches the residuals
- **WHEN** `augment()` and any residual type are computed on a windowed rate fit
- **THEN** the table has one row per dependent event, the same length the
  residual has, and the two align row for row

#### Scenario: the augmented table reports what it accumulated
- **WHEN** `augment()` is called on a fit whose intervals outnumber its
  dependent events
- **THEN** it carries a column giving the number of likelihood intervals
  accumulated into each event's span, those counts sum to the fit's interval
  count, and on a fit with no right-censored intervals every count is one

#### Scenario: the augmented table has no unrealized rows
- **WHEN** `augment()` is called on a fit carrying right-censored intervals
- **THEN** `.fitted` and `.resid` are present on every row, none being `NA`
  for want of a realized outcome

#### Scenario: waiting-time residuals stratify by actor
- **WHEN** `residuals(type = "cox_snell", level = "actor")` is called on an
  exact-time fit
- **THEN** it returns one set of accumulated compensators per acting actor,
  measured between that actor's own consecutive events, together with the
  censored final span from each actor's last event to the observation window's
  end

#### Scenario: the stratified residuals decompose the margin
- **WHEN** an actor's stratified waiting-time residuals are summed and counted
- **THEN** the sum equals that actor's expected count and the count equals its
  observed count, as `margin_table()` reports them

#### Scenario: the rescaled residuals are the between-event compensator
- **WHEN** cox_snell residuals are computed on a fit whose windowed effect
  splits the spans between events
- **THEN** each value is the compensator accumulated over its whole span, and
  their total equals the number of dependent events

#### Scenario: deviance residuals from stored loglik
- **WHEN** `residuals(fit)` is called on a fit with stored `intervalLogL`
- **THEN** the result equals `-2 * fit$intervalLogL` with no evaluation
  pass.

#### Scenario: schoenfeld residuals sum to zero at the MLE
- **WHEN** `residuals(fit, type = "schoenfeld")` is called on a converged
  choice-model fixture with no offset (fixed-coefficient) terms
- **THEN** the column sums of the free-parameter columns are zero within
  the convergence tolerance. (The identity is multinomial-submodel-only:
  exact-time schoenfeld rows lack the exposure term and do not sum to
  zero even at the MLE; offset columns are excluded because a fixed
  coefficient's score component is not zero at the optimum.)

#### Scenario: cox_snell residuals are unit exponential under the model
- **WHEN** cox_snell residuals are computed on data simulated from a known
  exact-time rate model at the true parameters
- **THEN** a Kolmogorov-Smirnov test against Exp(1) does not reject at the
  1% level on the fixture seed.

#### Scenario: the cox_snell refusal names the right likelihood
- **WHEN** `residuals(type = "cox_snell")` is requested on a
  `choice_coordination` fit
- **THEN** the abort names that fit's own likelihood family rather than
  describing it as multinomial

#### Scenario: unavailable type triggers replay rules
- **WHEN** a recompute-requiring type is requested without stored
  primitives, an attached preprocessed object, or a `preprocessed` argument
- **THEN** the method aborts with the diagnostic-primitives guiding error.

### Requirement: Methods apply per process on flavored fits
Every residual, fitted, predict and augment method SHALL reach a flavored
(multi-process) fit, and SHALL compute each process's result from that process
alone, exactly as for a single-model fit. Methods SHALL NOT pool across
processes. Each fid's result carries its own stored primitives and preprocessed
linkage, and calling a method on that inner result directly SHALL remain
supported and unchanged. The Fisheries Treaties creation/dissolution example is
the reference fit shape.

The return shape on the container SHALL follow whether the method's return is
tidy:

- `augment()`, whose return is a table, SHALL row-bind the per-process results
  and **append** `flavor` and `family` columns, in the shape `margin_table()`
  established, so that term and event columns stay positionally stable between a
  single-process fit and a multi-process one.
- `residuals()`, `fitted()` and `predict()`, whose returns are vectors, matrices
  and lists, SHALL return a list named by process label, since no column can
  carry the identity.

These three SHALL additionally accept `flavor =`, selecting one process and
returning exactly the shape the single-process fit returns, so the familiar
shape is one argument away. Supplying a flavor the fit does not carry SHALL
abort naming the available flavors.

A method reaching a flavored container SHALL NOT return `NULL`.

#### Scenario: per-process residuals on a flavored fit
- **WHEN** `residuals()` is called on one process result of a flavored
  Fisheries Treaties fit (creation or dissolution)
- **THEN** the output equals what a standalone single-process fit of that
  flavor would produce, with the same types available.

#### Scenario: the container returns one entry per process
- **WHEN** `residuals()` is called on a flavored container
- **THEN** it returns a list with one named entry per process, each equal to the
  result of calling the method on that process's own fit

#### Scenario: a flavor selects the single-fit shape
- **WHEN** `residuals(fit, flavor = "signing")` is called on a flavored container
- **THEN** the return is the ordinary single-fit shape for that process, not a
  list, and equals the corresponding entry of the unselected call

#### Scenario: an unknown flavor aborts naming the choices
- **WHEN** a `flavor =` value the fit does not carry is supplied
- **THEN** the call aborts naming the flavors the fit does carry

#### Scenario: augment row-binds with identity columns
- **WHEN** `augment()` is called on a flavored container
- **THEN** the result is one table row-binding the per-process tables, with
  `flavor` and `family` appended after the existing columns

### Requirement: fitted method
The method `fitted.goldfishFit()` SHALL accept
`type = c("outcome", "probabilities")` and
SHALL return, for `"outcome"` (default), `exp(intervalLogL)` — the fitted
probability (density contribution for exact-time submodels) of each
observed event from stored primitives; and for `"probabilities"`, the full
per-event fitted probability vectors via stored primitives or
`evaluate_model()`.

#### Scenario: outcome probabilities are free
- **WHEN** `fitted(fit)` is called on a fit with stored `intervalLogL`
- **THEN** it returns `exp(fit$intervalLogL)` without an evaluation pass.

### Requirement: in-sample predict method
The method `predict.goldfishFit()` SHALL accept
`type = c("probabilities", "ranks")`, an optional `events` index subset,
and `preprocessed`, and SHALL return, at the observed decision points, the
fitted next-event probability vectors or observed ranks via
`evaluate_model()`. The documentation SHALL state that this is in-sample
prediction given the observed history — not forecasting (which requires
`simulate()`) and not marginal effects.

#### Scenario: predicted ranks match stored ranks
- **WHEN** `predict(fit, type = "ranks")` is called on a fit estimated with
  the `"ranks"` primitive
- **THEN** the result equals the stored `observed_rank` vector.

### Requirement: augment gains broom residual columns
`augment.goldfishFit()` SHALL add `.fitted` (fitted outcome
probability) and `.resid` (deviance residual) columns alongside the
existing event columns and `intervalLogL`, following broom naming
conventions. Existing columns SHALL be unchanged. The method SHALL be
wired like `tidy`/`glance`: registered with
`S3method(augment, goldfishFit)` and the `generics::augment` generic
re-exported, replacing the current bare `export(augment.goldfishFit)`
(the bare export is dev-line-only, so it is removed without a stub;
NEWS records the wiring fix).

#### Scenario: augmented tibble carries broom columns
- **WHEN** `augment(fit)` is called on a fit with stored `intervalLogL`
- **THEN** the tibble contains `.fitted = exp(intervalLogL)` and
  `.resid = -2 * intervalLogL` for non-censored rows, with prior columns
  intact.

#### Scenario: augment dispatches like the other broom generics
- **WHEN** `augment(fit)` is called after attaching goldfish
- **THEN** dispatch reaches the method through the re-exported generic,
  and `augment.goldfishFit` is no longer an exported name (matching
  how `tidy`/`glance` are wired).

### Requirement: documented caveats on residual use
The canonical residuals documentation page SHALL contain an explicit
caveats section covering, at minimum: (1) **likelihood deletion vs
history deletion** — dfbeta, dfbetas, cooks, and the onset diagnostic
remove an event's likelihood term while its effect remains inside every
subsequent endogenous statistic; counterfactual removal would require a
statistics replay per deletion and is not what these measures estimate;
(2) **the onset reading of the deviance trace** — early events sitting at
the per-event null benchmark are uninformative (left-censored history),
not surprising, and must not be read as misfit; (3) **the zero-score
property at cold start** — endogenous statistics constant across the risk
set give exactly zero score contributions, so score-based diagnostics
show no influence of onset events on endogenous coefficients while
intercept and exogenous blocks can still absorb them. The section SHALL
cross-reference `diagnose_onset()` and the warm-start / event-exclusion
remedies.

#### Scenario: caveats section present and complete
- **WHEN** the rendered man page for the canonical residuals topic is
  inspected
- **THEN** it contains a caveats section addressing likelihood-vs-history
  deletion, the onset/null-benchmark reading, and the cold-start
  zero-score property, with a cross-reference to `diagnose_onset()`.

### Requirement: validation against classical equivalent fits
The residual definitions SHALL be validated against classical fits of the
same likelihoods, not against other relational-event packages: the ordinal
REM against `survival::coxph` (its likelihood is the Cox partial
likelihood), the choice and ordinal rate sub-models against
`survival::clogit` (a conditional logit is a stratified Cox model), and
the exact-time sub-models against a `stats::glm` Poisson fit with a
log-elapsed-time offset (the piecewise-exponential equivalence, on a
fixture whose inter-event times are all positive). Comparisons SHALL be
made at a shared parameter vector — the evaluation pass on the goldfish
side, a zero-iteration fit on the reference side — so optimizer noise is
excluded and the comparison pins conventions rather than convergence.
Exact-time Schoenfeld rows need no separate reference: they are the
conditional score rows, so the partial-likelihood validation carries over.

survival-derived reference values SHALL enter the repository only as
frozen reference files minted by the one-time comparison harness, with
provenance (package versions, date, fixture) recorded beside them; the
NOT_CRAN tests compare goldfish output against the frozen values. The
`stats::glm` equivalences MAY run live, `stats` shipping with R. Two
tolerances SHALL be documented: the shared-parameter agreement (set one
order of magnitude above the measured value) and the
independently-optimized coefficient parity, which is a harness claim and
never a test gate. Comparisons against other relational-event packages
(`remstimate` and its stack) SHALL live only in the harness as one-time
recorded evidence with an output-mapping note, SHALL NOT be tests, and
SHALL NOT add packages to Suggests.

#### Scenario: frozen agreement with survival at a shared parameter vector
- **WHEN** the scaled Schoenfeld matrix and the per-transform time-trend
  statistics are computed on the Cox-expressible fixtures at the frozen
  reference fit's coefficients
- **THEN** they match the frozen `coxph`/`cox.zph`-derived reference values
  within the documented shared-parameter tolerance, without any
  relational-event package installed.

#### Scenario: exact-time fits match the Poisson-offset equivalence
- **WHEN** an exact-time fixture with strictly positive inter-event times
  is fitted and the equivalent Poisson GLM with a log-elapsed-time offset
  is fitted live
- **THEN** the coefficients agree within the documented tolerance and the
  log-likelihoods agree up to the documented offset constant.

#### Scenario: the scale convention is pinned without external packages
- **WHEN** the scaled Schoenfeld residuals are computed on a two-regime
  fixture whose true coefficient changes mid-sequence
- **THEN** the within-regime means of the scaled residuals track each
  regime's separately-estimated coefficient, so a scale-convention error
  is detected by the invariant itself.

### Requirement: A per-event column is not named for an interval
A column SHALL be named for what its rows hold. Where a table moved from one
row per likelihood interval to one row per dependent event, a column whose name
says `interval` now describes something it does not contain, and SHALL be
renamed rather than left to be read wrongly.

This SHALL NOT be applied as a sweep over the word. The same vocabulary is
correct wherever a quantity really is per interval — the stored
`interval_log_lik` primitive, the `intervals` clock, `n_intervals`, and the
printed contexts that report an interval count *beside* an event count, which
are the one place both numbers are stated honestly.

The renamed columns are `augment()`'s log-likelihood column, which now carries
the span's contribution rather than one interval's; `augment()`'s censoring
flag, which marks the one row that closes **no** waiting time and so is
precisely not an event; and `test_time()`'s row index, which now indexes
events.

#### Scenario: the augmented table names its columns for events
- **WHEN** `augment()` is called on any fit
- **THEN** the per-event log-likelihood is `event_log_lik` and the censoring
  flag is `censored`, a logical that is `TRUE` only on a span closing no
  waiting time

#### Scenario: a term-wise time table indexes events
- **WHEN** `test_time()` returns its per-row table
- **THEN** the row index is `event`, matching the per-event residual rows it
  reports

#### Scenario: the per-interval vocabulary is retained where it is accurate
- **WHEN** a fit's stored components and the printed diagnostic contexts are
  read
- **THEN** `interval_log_lik`, `intervals` and `n_intervals` keep their names,
  and a context reporting both counts still distinguishes intervals from
  dependent events

### Requirement: waiting-time residuals stratify on request
`residuals(type = "cox_snell", level = "actor")` SHALL return the
waiting-time residuals stratified by acting actor: for each actor, the
compensator accumulated between that actor's own consecutive events, which is
unit exponential under a correctly specified model, plus the censored final
span running from that actor's last event to the end of the observation
window. The default `level = "event"` SHALL remain the unstratified series.

The censored final span SHALL be marked as censored rather than dropped or
silently treated as observed. It is what makes the stratified set a genuine
survival object — one censored observation per acting actor rather than one
for the whole fit — and therefore what makes a Kaplan-Meier or Nelson-Aalen
rendering informative rather than a re-drawing of the Q-Q plot.

The stratified residuals SHALL reconcile with the margins: an actor's residuals
sum to its expected count and number its observed count, so the margin reports
their level and the stratified set their shape. The documentation SHALL state
that an actor with few events supports no distributional reading.

Requesting the stratification on a family that defines no waiting time SHALL
abort as the unstratified type already does.

#### Scenario: each actor contributes one censored span
- **WHEN** stratified waiting-time residuals are computed on an exact-time fit
- **THEN** each acting actor contributes one censored observation, running from
  its last event to the end of the observation window

#### Scenario: stratification is refused where timing is undefined
- **WHEN** `level = "actor"` is requested with `type = "cox_snell"` on a
  multinomial family
- **THEN** the call aborts naming that family's likelihood, as the unstratified
  request does

