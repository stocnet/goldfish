# residual-methods

`residuals()`, `fitted()`, `predict()`, and `augment()` methods for fitted
goldfish models, following the survival::coxph type vocabulary and broom
column conventions.

## ADDED Requirements

### Requirement: residuals method with coxph-style types
`residuals.result.goldfish(object, type, preprocessed = NULL, ...)` SHALL
support `type = c("deviance", "schoenfeld", "scaled_schoenfeld", "score",
"cox_snell", "response", "martingale", "dfbeta", "dfbetas", "cooks")` with
`"deviance"` as default. Definitions: deviance = `-2 * intervalLogL`;
schoenfeld = per-event observed-minus-expected statistic rows (the stored
`event_scores` for multinomial submodels); scaled_schoenfeld =
`coef(object) + n * solve(Vbar) %*% s_k` with `Vbar` the average
information; score = the per-event score increments (equal to schoenfeld
for ordinal/choice submodels, including the exposure term for exact-time
submodels); cox_snell = interevent time times the total fitted rate
(exact-time rate/REM submodels only; requesting it elsewhere aborts with a
cli error); response = observed indicator minus fitted probability per
alternative; martingale = per-actor-margin observed minus expected counts;
dfbeta/dfbetas = `solve(I) %*% s_k` (scaled by standard errors for
dfbetas); cooks = `t(s_k) %*% solve(I) %*% s_k`, the scalar one-step
self-influence (Cook's-distance analog; the frequentist counterpart of a
per-event influence flag such as PSIS-LOO's Pareto k). dfbeta, dfbetas,
and cooks are stored-primitive types (scores plus the stored information
matrix). Types computable from stored primitives SHALL NOT trigger an
evaluation pass; the remaining types SHALL recompute via
`evaluate_engine()` under the diagnostic-primitives replay rules. For DyNAM
fits all residuals SHALL be conditional per submodel (rate residuals over
the sender risk set, choice residuals over the receiver risk set given the
observed sender).

#### Scenario: deviance residuals from stored loglik
- **WHEN** `residuals(fit)` is called on a fit with stored `intervalLogL`
- **THEN** the result equals `-2 * fit$intervalLogL` with no evaluation
  pass.

#### Scenario: schoenfeld residuals sum to zero at the MLE
- **WHEN** `residuals(fit, type = "schoenfeld")` is called on a converged
  choice-model fixture
- **THEN** the column sums are zero within the convergence tolerance.

#### Scenario: cox_snell residuals are unit exponential under the model
- **WHEN** cox_snell residuals are computed on data simulated from a known
  exact-time rate model at the true parameters
- **THEN** a Kolmogorov-Smirnov test against Exp(1) does not reject at the
  1% level on the fixture seed.

#### Scenario: unavailable type triggers replay rules
- **WHEN** a recompute-requiring type is requested without stored
  primitives, an attached preprocessed object, or a `preprocessed` argument
- **THEN** the method aborts with the diagnostic-primitives guiding error.

### Requirement: Methods apply per process on flavored fits
Every residual, fitted, predict, and augment method SHALL apply to each
process result of a flavored (multi-process) estimation exactly as to a
single-model fit. Each fid's result carries its own stored primitives and
preprocessed linkage; methods SHALL NOT pool across processes and SHALL NOT
require the flavored container. The Fisheries Treaties
creation/dissolution example is the reference fit shape.

#### Scenario: per-process residuals on a flavored fit
- **WHEN** `residuals()` is called on one process result of a flavored
  Fisheries Treaties fit (creation or dissolution)
- **THEN** the output equals what a standalone single-process fit of that
  flavor would produce, with the same types available.

### Requirement: fitted method
The method `fitted.result.goldfish()` SHALL accept
`type = c("outcome", "probabilities")` and
SHALL return, for `"outcome"` (default), `exp(intervalLogL)` — the fitted
probability (density contribution for exact-time submodels) of each
observed event from stored primitives; and for `"probabilities"`, the full
per-event fitted probability vectors via stored primitives or
`evaluate_engine()`.

#### Scenario: outcome probabilities are free
- **WHEN** `fitted(fit)` is called on a fit with stored `intervalLogL`
- **THEN** it returns `exp(fit$intervalLogL)` without an evaluation pass.

### Requirement: in-sample predict method
The method `predict.result.goldfish()` SHALL accept
`type = c("probabilities", "ranks")`, an optional `events` index subset,
and `preprocessed`, and SHALL return, at the observed decision points, the
fitted next-event probability vectors or observed ranks via
`evaluate_engine()`. The documentation SHALL state that this is in-sample
prediction given the observed history — not forecasting (which requires
`simulate()`) and not marginal effects.

#### Scenario: predicted ranks match stored ranks
- **WHEN** `predict(fit, type = "ranks")` is called on a fit estimated with
  the `"ranks"` primitive
- **THEN** the result equals the stored `observed_rank` vector.

### Requirement: augment gains broom residual columns
`augment.result.goldfish()` SHALL add `.fitted` (fitted outcome
probability) and `.resid` (deviance residual) columns alongside the
existing event columns and `intervalLogL`, following broom naming
conventions. Existing columns SHALL be unchanged.

#### Scenario: augmented tibble carries broom columns
- **WHEN** `augment(fit)` is called on a fit with stored `intervalLogL`
- **THEN** the tibble contains `.fitted = exp(intervalLogL)` and
  `.resid = -2 * intervalLogL` for non-censored rows, with prior columns
  intact.

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
cross-reference `examine_onset()` and the warm-start / event-exclusion
remedies.

#### Scenario: caveats section present and complete
- **WHEN** the rendered man page for the canonical residuals topic is
  inspected
- **THEN** it contains a caveats section addressing likelihood-vs-history
  deletion, the onset/null-benchmark reading, and the cold-start
  zero-score property, with a cross-reference to `examine_onset()`.

### Requirement: cross-package validation of residual definitions
The scaled Schoenfeld residuals SHALL be validated against
`survival::cox.zph`-consistent computations on a REM fixture expressible as
a Cox model, and residual outputs SHALL be compared against
`remstimate::diagnostics()` on a shared small dataset, within documented
tolerances, as NOT_CRAN tests.

#### Scenario: agreement with survival on a Cox-expressible fixture
- **WHEN** the scaled Schoenfeld computation runs on the shared fixture
- **THEN** it matches the survival-package reference within the documented
  tolerance.
