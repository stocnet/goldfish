# rate-distributions Delta Specification

## ADDED Requirements

### Requirement: A distribution argument selects the waiting-time model
`estimate_dynam()`, `estimate_rem()`, and `estimate_dynami()` SHALL accept a
`distribution` argument with values `c("exponential", "weibull", "gompertz",
"cox")`, defaulting to `"exponential"`, valid only for rate-family sub-models.
The argument is one axis to the user, but the spec's behavioral descriptor
SHALL keep two facts apart: `distribution` on the descriptor SHALL name the
hazard family only where a hazard is integrated, and `distribution = "cox"`
SHALL instead resolve to the ordinal timing regime, because a Cox fit
integrates no hazard and the recipe reads that fact from `timing`.
Supplying `distribution` (other than the default) together with a choice-family
sub-model SHALL abort with a cli error (inert arguments are signaled, not
dropped). The default SHALL reproduce the current exponential fits exactly.

#### Scenario: default is the exponential model
- **WHEN** `estimate_dynam(formula, sub_model = "rate", data = d)` is called
  without `distribution`
- **THEN** coefficients, standard errors, and log-likelihood equal the
  pre-change exponential fit to within 1e-6

#### Scenario: weibull accepted on the rate family
- **WHEN** `estimate_rem(formula, distribution = "weibull", data = d)` is called
- **THEN** estimation completes and the result records
  `distribution == "weibull"` with an estimated shape

#### Scenario: distribution on a choice sub-model aborts
- **WHEN** `estimate_dynam(formula, sub_model = "choice", distribution =
  "weibull", data = d)` is called
- **THEN** a cli error explains that `distribution` applies only to rate-family
  sub-models

#### Scenario: specification carries the distribution
- **WHEN** `make_specification(rate = ~ 1 + indeg, model = "DyNAM",
  distribution = "gompertz", layer = "calls", data = d)` is estimated
- **THEN** the rate block is fitted under the Gompertz baseline without any
  estimate-time argument

### Requirement: Fixed-shape estimation reuses the exponential machinery
Estimation with a fixed shape SHALL be the exponential score/Hessian with the
waiting times replaced by the integrated baseline G_m (Weibull: w^k; Gompertz:
(e^(γw) − 1)/γ), so the fixed-shape inner problem stays globally concave and
the fixed-shape β estimate equals an exponential fit on transformed exposures.

#### Scenario: fixed shape equals transformed-exposure exponential fit
- **WHEN** a Weibull fit is run with the shape held at a fixed k
- **THEN** the resulting β equals the exponential estimate computed on waiting
  times transformed to w^k, to within the optimizer tolerance

### Requirement: Joint shape estimation by damped Fisher scoring
The cpp backend SHALL estimate (β, κ = log k) for Weibull and (β, γ) for
Gompertz jointly, accumulating the shape row/column of the information in the
same per-event pass as the β blocks, with step-halving until the
log-likelihood increases, a trust region capping the shape step, and the
expected (Fisher) information substituted when the observed Hessian loses
negative definiteness. Standard errors SHALL come from the inverse of the full
joint information. When the shape gradient stays bounded away from zero while
the shape drifts monotonically, the fit SHALL report shape non-convergence
instead of iterating indefinitely.

#### Scenario: shape recovery on simulated Weibull data
- **WHEN** events are simulated from a Weibull rate model with known k ≠ 1 and
  the model is re-estimated with `distribution = "weibull"`
- **THEN** the estimated k covers the truth within simulation tolerance and
  its reported SE derives from the joint information (not a profiled inner SE)

#### Scenario: exponential nested at the boundary value
- **WHEN** data simulated from an exponential model are fitted with
  `distribution = "weibull"`
- **THEN** the estimated k is close to 1 and the Wald test against k = 1 is
  non-significant at conventional levels

#### Scenario: weak timing information is reported, not looped
- **WHEN** the shape update diverges (gradient bounded away from zero, shape
  drifting monotonically)
- **THEN** estimation stops with a convergence report naming the shape as
  non-converged rather than exceeding the iteration limit silently

### Requirement: Profiling initializes and backstops the joint update
A profile-likelihood sweep SHALL be available: a 1-D outer optimization of the
shape wrapping the unmodified fixed-shape inner fit on transformed exposures,
used to initialize the joint update and used as the estimation fallback when
the joint update fails to converge.

#### Scenario: profiling fallback converges where it is used
- **WHEN** the joint update reports failure and the profiling route runs
- **THEN** the returned fit is the profile optimum with a note in the result
  that profiling produced it

### Requirement: Gompertz numerics are stable near zero shape
Gompertz computations of G and its derivatives SHALL switch to the series
expansion G ≈ w(1 + γw/2 + γ²w²/6) when |γw| is below the numeric threshold,
so the exponential-null region evaluates without 0/0.

#### Scenario: no NaN at tiny gamma
- **WHEN** the Gompertz likelihood and derivatives are evaluated at γ = 1e-12
- **THEN** all quantities are finite and match the exponential limit to
  numeric precision

### Requirement: Weibull rejects zero waiting times with guidance
Weibull estimation SHALL abort with a cli error when any waiting time is zero,
naming the remedies (aggregate simultaneous events, jitter, or bound waiting
times below by the time resolution). Exponential and cox fits SHALL be
unaffected by zero waiting times handling.

#### Scenario: tied events under weibull abort
- **WHEN** the event stream contains simultaneous events (w = 0) and
  `distribution = "weibull"` is requested
- **THEN** a cli error names the zero waiting times and the remedies, and no
  fit is returned

### Requirement: Shape reporting in the fit surface
The estimated shape SHALL appear in `coef()` and `vcov()` under a reserved
term name (`log_shape` for Weibull, `gamma` for Gompertz), and `summary()`
SHALL report the natural-scale shape with a delta-method SE and a Wald test of
the exponential null. `logLik()` SHALL be comparable across distributions for
the same data and sub-model.

#### Scenario: shape term present in coefficient surface
- **WHEN** `coef(fit)` and `vcov(fit)` are read from a Weibull fit
- **THEN** both contain the `log_shape` entry and the vcov row/column comes
  from the joint information

#### Scenario: summary shows the shape test
- **WHEN** `summary(fit)` prints for a Gompertz fit
- **THEN** the output (cli-rendered, snapshot-pinned) includes γ̂, its SE, and
  the Wald test against γ = 0

### Requirement: The cox level replaces rate_ordered with identical numbers
`distribution = "cox"` SHALL select the partial-likelihood (ordering-only)
model previously reached by `sub_model = "rate_ordered"`, producing identical
coefficients, and the `rate_ordered` token SHALL be removed from every
estimator and from `make_specification()` (dev-line deletion, NEWS records the
mapping).

#### Scenario: cox resolves to the ordinal timing regime
- **WHEN** a spec is constructed with `sub_model = "rate", distribution =
  "cox"`
- **THEN** its descriptor reports the ordinal timing regime, and its
  `distribution` field does not report `"cox"`, because no hazard is
  integrated for it to name

#### Scenario: cox reproduces rate_ordered coefficients
- **WHEN** a model previously fitted with `sub_model = "rate_ordered"` is
  fitted with `sub_model = "rate", distribution = "cox"`
- **THEN** coefficients agree to within 1e-6

#### Scenario: rate_ordered token is gone
- **WHEN** `estimate_dynam(formula, sub_model = "rate_ordered", data = d)` is
  called after the change
- **THEN** the call fails match.arg-style with the valid sub_model values, and
  NEWS documents `distribution = "cox"` as the replacement
