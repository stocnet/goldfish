# Proposal: parametric-rates

## Why

The rate sub-models of DyNAM and REM assume exponential waiting times (constant
baseline hazard), but empirical event streams show duration dependence —
burstiness with heavy-tailed lulls, escalation under silence, decay to a stable
background rate. Weibull and Gompertz baselines with a **common shape
parameter** capture these patterns while preserving the timing/choice
factorization and reusing the exponential machinery (the β-block is the
exponential score/Hessian with transformed exposures G_m; derivation in
`.plan/sp/parametric_rates_dynam_rem.md`). The pre-2.0.0 window (ADR-0016) is
the moment to settle the naming surface: the waiting-time distribution becomes
an orthogonal axis instead of multiplying `sub_model` tokens.

## What Changes

- New `distribution = c("exponential", "weibull", "gompertz", "cox")` argument
  on `estimate_dynam()` (rate sub-model), `estimate_rem()`,
  `estimate_dynami()` (rate sub-model), and `make_specification()`. Default
  `"exponential"` reproduces current results exactly; `sub_model = "rate"`
  keeps meaning the rate family.
- **BREAKING (dev-line only)**: the `sub_model = "rate_ordered"` token is
  deleted outright (born after v1.7.0, never released) — `distribution =
  "cox"` replaces it; the existing partial-likelihood machinery is reused
  unchanged.
- C++ joint damped Fisher scoring on (β, log k) for Weibull and (β, γ) for
  Gompertz: shape row/column added to the information matrix, with
  step-halving, a trust region on the shape step, the Gompertz series
  expansion near γ = 0, a divergence monitor, and a Weibull tie rule for
  zero waiting times.
- Profiling over the shape (outer 1-D optimization wrapping the unmodified
  fixed-shape inner fit on transformed exposures) as initializer and robust
  fallback.
- Shape reporting: the shape parameter joins the coefficient vector and
  `vcov()` (full joint information → correct standard errors), appears in
  `summary()` with a Wald test against the exponential null (k = 1 / γ = 0);
  `logLik()`/AIC comparability across distributions is documented.
- Validation tests cross-check covariate-only fits against established
  survival implementations (e.g. `survival::survreg` for Weibull) and add
  simulation-recovery tests for the shape.

## Capabilities

### New Capabilities
- `rate-distributions`: the waiting-time distribution axis — argument surface
  and validation, fixed-shape estimation via transformed exposures, joint
  shape estimation with safeguards, numerics (Gompertz series, Weibull ties),
  shape reporting and tests, and the `"cox"` level replacing `rate_ordered`.

### Modified Capabilities
- `model-recipe-dispatch`: the `rate_ordered` sub_model requirements are
  removed (folded into the distribution axis); `estimate_rem()` sub_model
  values shrink to `"rate"` (+ deprecated `"choice"` alias).
- `model-specification`: `make_specification()` accepts and stores
  `distribution`; `rate_sub_model` loses the `"rate_ordered"` token.

## Impact

- **R**: `R/model_estimate.R` (argument surface, dispatch), `R/make_specification.R`,
  `R/model_preprocess.R` / preprocessing output (waiting-time exposures),
  result-object components and `summary()`/`print()` (cli).
- **C++** (`src/`): estimation engine gains the shape-augmented scoring pass —
  `cpp-recompile` skill after every edit; the frozen 1e-6 coefficient/C++
  golden baselines must stay PASS (the exponential default path is untouched).
- **Docs/tests**: roxygen for four functions (inherited params), NEWS entries,
  new test files (surface, numerics, recovery, cross-validation against
  survival packages in Suggests, test-only).
- **Downstream**: `two-sided-coordination` deliberately does NOT get this axis
  (Cox-only there); the vocabulary is settled here first.
