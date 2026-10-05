## MODIFIED Requirements

### Requirement: ABEM results carry Fisher-based vcov and Monte-Carlo error

The `estimate_dynes()` result SHALL contain the parameter estimates, a
`converged` flag, the Fisher-information approximation, Monte-Carlo standard
errors, and the `em_trace` per-iteration
diagnostics: parameters, Q and its standard error, the accept/grow/stop
decision, pool size and new draws, effective sample size, the weighting scheme
in force, MCMC chain diagnostics (acceptance rate, autocorrelation index), and
— opt-in via `em_trace_se` — per-iteration parameter standard errors. `coef()`
and `vcov()` SHALL be **flat** (a named vector and matrix over the free
parameters, named by the `joint-parameters` composite labels); fixed effects
are the specification's formula offsets and do not enter the free parameter
vector, their display padding reusing the existing `GetFixed()` /
`stats::.vcov.aliased()` machinery. `summary()` SHALL display asymptotic and
Monte-Carlo error side by side, grouping the flat coefficients into per-process
blocks via `coef_layout()` and surfacing the `converged` flag; `print()` /
`summary()` SHALL render via cli semantic elements, stable under a pinned cli
context for snapshot tests.

The Fisher approximation and both error estimates SHALL come from a single
dedicated full-pool evaluation at the returned parameters, run on **every
returning termination regardless of the `converged` flag** — including the
`max_iterations`-exhaustion path that returns `converged = FALSE`. A returned
fit SHALL therefore always carry a populated `vcov()` and Monte-Carlo error; a
non-converged result SHALL have standard errors (which `summary()` caveats
beside the `converged = FALSE` status), never a `NULL` covariance. Only the
`max_retries`-abort path produces no return and thus no such pass.

The Fisher approximation SHALL be the **Louis observed-data information**
estimated over the whole pool with the run's normalized weights `w̄ᵢ`
(whatever the `use` setting, and after any weight `transformation`; the
`estimate_dynes()` documentation SHALL state that the estimator is then an
approximation), from each kept sequence's complete-data score
`sᵢ` and complete-data information `Jᵢ` at the returned parameters:
`I_obs = Σ w̄ᵢJᵢ − Σ w̄ᵢsᵢsᵢᵀ + S̄S̄ᵀ` with `S̄ = Σ w̄ᵢsᵢ`, a degenerate or
zero-weight sequence contributing nothing. `vcov()` SHALL return `I_obs⁻¹`. The
Monte-Carlo covariance SHALL be `I_obs⁻¹ (Σ w̄ᵢ² (sᵢ − S̄)(sᵢ − S̄)ᵀ) I_obs⁻¹`.
The opt-in `em_trace` per-iteration standard errors SHALL be the square roots
of the diagonal of the same `I_obs⁻¹` at each pass's proposal. Neither
covariance SHALL be formed from the complete-data scores alone, as
`I_c⁻¹ (Σ w̄ᵢsᵢsᵢᵀ) I_c⁻¹` with `I_c = Σ w̄ᵢJᵢ`: at the maximum that middle
term is the missing information, not the observed information.

#### Scenario: em_trace records the ascent
- **WHEN** a converged result is inspected
- **THEN** its `em_trace` holds one row per EM iteration with the ascent
  decision, Q and its standard error, pool bookkeeping, and effective sample
  size.

#### Scenario: vcov from the Fisher approximation
- **WHEN** `vcov()` is called on a converged `estimate_dynes()` result
- **THEN** it returns the inverse of the Louis observed-data information over
  the free parameters, named by the composite labels, with offset (fixed)
  effects padded via the existing aliased-vcov machinery.

#### Scenario: covariances match the hand-computed Louis forms
- **WHEN** a fit's final pool has per-sequence scores `sᵢ`, information
  matrices `Jᵢ` and normalized weights `w̄ᵢ` known in closed form
- **THEN** `vcov()` equals `(Σ w̄ᵢJᵢ − Σ w̄ᵢsᵢsᵢᵀ + S̄S̄ᵀ)⁻¹` and the
  Monte-Carlo covariance equals that inverse around
  `Σ w̄ᵢ² (sᵢ − S̄)(sᵢ − S̄)ᵀ`, each within 1e-10.

#### Scenario: no missing information gives the complete-data inverse
- **WHEN** every kept sequence of the final pool has the same score
- **THEN** `vcov()` equals `(Σ w̄ᵢJᵢ)⁻¹` and the Monte-Carlo covariance is
  zero.

#### Scenario: missing information widens the standard errors
- **WHEN** the kept sequences' scores differ across the final pool and the
  observed information is positive definite
- **THEN** each `vcov()` standard error is at least the corresponding standard
  error of `(Σ w̄ᵢJᵢ)⁻¹`.

#### Scenario: standard errors match a closed-form oracle
- **WHEN** `estimate_dynes()` fits a count panel whose effects are all
  exogenous with covariates constant over the window, beside an observed
  stream that reads no modeled panel layer, so that the observed-data
  likelihood is the Poisson likelihood of the dyad counts
- **THEN** each panel-layer standard error from `vcov()` is within 10% of the
  standard error of the closed-form Poisson-regression MLE fitted to the same
  counts.

#### Scenario: observed information equals the Hessian of the pool log-likelihood
- **WHEN** the observed information is evaluated with untransformed
  importance weights over a fixed pool whose sequences carry different
  complete-data scores
- **THEN** it equals the central-difference negative Hessian in θ of
  `log Σᵢ exp(ℓᵢ(θ) − log qᵢ)`, with `ℓᵢ` each sequence's complete-data
  log-likelihood and `qᵢ` its proposal density, within 1e-5 relative.

#### Scenario: MC error is visible
- **WHEN** `summary()` renders a DyNES result estimated with a small pool
- **THEN** the Monte-Carlo standard errors appear alongside the asymptotic
  standard errors, making pool-limited precision visible.

#### Scenario: non-converged fit still carries standard errors
- **WHEN** `vcov()` is called on a result returned after `max_iterations`
  exhaustion (`converged = FALSE`)
- **THEN** it returns the covariance from the final full-pool pass at the
  last-accepted parameters, not `NULL`, and `summary()` shows the standard
  errors beside the non-convergence status.

#### Scenario: warm-start fit failure falls back to zero for that fid
- **WHEN** `warm_start = TRUE` and one fid's internal `set_algorithm_newton()`
  fit fails on the single drawn sequence
- **THEN** that fid's θ₀ block falls back to zero with a cli warning naming the
  fid, the other fids keep their fitted starts, and estimation proceeds.

## ADDED Requirements

### Requirement: A DyNES fit reports its covariance diagnostics

An `estimate_dynes()` result SHALL carry `covariance_status`, one of `"ok"`,
`"pool_dropped"`, `"singular"` or `"not_pd"`. It SHALL be set by the first of
these that applies: the optimization loop stopped because no sequence of the
pool had a finite likelihood (`"pool_dropped"`); the pooled complete-data
information `I_c = Σ w̄ᵢJᵢ` is singular (`"singular"`); the observed
information `I_obs` is not positive definite (`"not_pd"`). It SHALL be `"ok"`
when none applies.

The result SHALL carry `fraction_missing`, the largest eigenvalue of
`I_c⁻¹ (Σ w̄ᵢsᵢsᵢᵀ − S̄S̄ᵀ)` at the returned parameters. It SHALL be a number
on an `"ok"` fit and on a `"not_pd"` fit (where it is at least one), and
`NA` on a `"singular"` or `"pool_dropped"` fit. It SHALL NOT trigger a
warning.

The result SHALL carry `covariance_ess`, the effective sample size
`1 / Σ w̄ᵢ²` of the normalized weights the covariance pass used. Under
importance weighting it SHALL be a number on an `"ok"`, `"not_pd"` or
`"singular"` fit. It SHALL be `NA` on a `"pool_dropped"` fit, and `NA` on
every fit estimated with `weighting = "uniform"`, where it would equal the
number of kept sequences whatever the chain's mixing. It SHALL NOT trigger a
warning.

`glance()` SHALL return all three as columns. `summary()` SHALL print
`fraction_missing` on an `"ok"` fit, and `covariance_ess` on any fit where
it is not `NA`. On any other status it SHALL print,
via cli semantic elements, why the standard-error columns are blank:
for `"not_pd"` and `"singular"`, the lever the corresponding warning names;
for `"pool_dropped"`, that the pool stopped with no finite likelihood.
`print()` on the fit SHALL be unchanged. The `em_trace` SHALL NOT carry
per-pass statuses or fractions.

#### Scenario: an ok fit reports its fraction of missing information
- **WHEN** a fit's final pool has per-sequence scores, information matrices
  and normalized weights known in closed form, and `I_obs` is positive
  definite
- **THEN** `covariance_status` is `"ok"`, `fraction_missing` equals the
  largest eigenvalue of `I_c⁻¹ (Σ w̄ᵢsᵢsᵢᵀ − S̄S̄ᵀ)` within 1e-10 and lies
  in `[0, 1)`, and `summary()` prints it.

#### Scenario: a non-positive-definite observed information is recorded
- **WHEN** the final pool's observed information is not positive definite
  while `I_c` is invertible
- **THEN** `covariance_status` is `"not_pd"`, `fraction_missing` is at least
  one, and `summary()` states why the standard-error columns are blank and,
  under importance weighting, prints `covariance_ess`.

#### Scenario: a singular or dropped pool has no fraction
- **WHEN** `I_c` is singular, or the loop stopped because no sequence of the
  pool had a finite likelihood
- **THEN** `covariance_status` is `"singular"` or `"pool_dropped"`
  respectively, and `fraction_missing` is `NA`.

#### Scenario: the covariance ESS is that of the covariance weights
- **WHEN** a fit estimated with importance weighting has a final pool whose
  normalized weights `w̄ᵢ` are known in closed form
- **THEN** `covariance_ess` equals `1 / Σ w̄ᵢ²` within 1e-10, and
  `summary()` prints it on an `"ok"` fit.

#### Scenario: the covariance ESS is not reported under uniform weighting
- **WHEN** a fit is estimated with `weighting = "uniform"`, or the loop
  stopped because no sequence of the pool had a finite likelihood
- **THEN** `covariance_ess` is `NA`.

#### Scenario: glance carries the diagnostics
- **WHEN** `glance()` is called on a DyNES fit
- **THEN** its one row holds `covariance_status`, `fraction_missing` and
  `covariance_ess` beside the existing columns.

### Requirement: A DyNES fit exposes its information decomposition

An `estimate_dynes()` result SHALL carry `information`, a list of three p×p
matrices over the free parameters, named by the composite labels and
evaluated in the same full-pool pass as the covariances: `complete`, the
pooled complete-data information `I_c = Σ w̄ᵢJᵢ`; `missing`, the weighted
conditional variance of the complete-data scores `Σ w̄ᵢsᵢsᵢᵀ − S̄S̄ᵀ`; and
`observed`, the Louis information `I_obs`, so that `observed` equals
`complete − missing`. It SHALL be populated on `"ok"`, `"not_pd"` and
`"singular"` fits and SHALL be `NULL` on a `"pool_dropped"` fit.

`vcov()` on a DyNES fit SHALL take an `information` argument with values
`"observed"` (the default, `I_obs⁻¹`) and `"complete_data"` (`I_c⁻¹`, which
ignores the missing information), and SHALL reject any other value with an
error naming the allowed values. `"complete_data"` SHALL return the finite
`I_c⁻¹` on a `"not_pd"` fit without a warning, and `NA` on a `"singular"` or
`"pool_dropped"` fit. The Monte-Carlo covariance, `tidy()`, `summary()` and
`coef_layout()` SHALL take no such argument and SHALL report the observed
information's standard errors only. No covariance SHALL be offered as
`I_c⁻¹ (Σ w̄ᵢsᵢsᵢᵀ) I_c⁻¹`. The `estimate_dynes()` documentation SHALL
explain why the standard errors subtract the missing information rather
than use that form, citing Louis (1982), McLachlan and Krishnan (2008) and
Ruth (2024).

#### Scenario: the decomposition is an exact subtraction
- **WHEN** a fit's final pool has per-sequence scores, information matrices
  and normalized weights known in closed form
- **THEN** `information$complete` equals `Σ w̄ᵢJᵢ`, `information$missing`
  equals `Σ w̄ᵢsᵢsᵢᵀ − S̄S̄ᵀ`, and `information$observed` equals
  `information$complete − information$missing`, each within 1e-10, with
  dimnames equal to the composite labels.

#### Scenario: vcov inverts the requested information
- **WHEN** `vcov()` is called on an `"ok"` fit with
  `information = "observed"` and with `information = "complete_data"`
- **THEN** the first equals `solve(information$observed)` and the default
  call, the second equals `solve(information$complete)`, and every diagonal
  entry of the first is at least the corresponding entry of the second.

#### Scenario: the complete-data inverse survives a non-positive-definite observed information
- **WHEN** `vcov(information = "complete_data")` is called on a `"not_pd"`
  fit
- **THEN** it returns the finite `I_c⁻¹` with no warning, while the default
  `vcov()` returns `NA`, and `information` holds all three finite matrices.

#### Scenario: a dropped pool carries no decomposition
- **WHEN** the loop stopped because no sequence of the pool had a finite
  likelihood
- **THEN** `information` is `NULL` and both `vcov()` types return `NA`.

#### Scenario: an unknown information type is rejected
- **WHEN** `vcov()` is called on a DyNES fit with an `information` value
  other than `"observed"` or `"complete_data"`
- **THEN** it errors and names the allowed values.
