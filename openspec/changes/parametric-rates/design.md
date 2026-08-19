# Design: parametric-rates

## Context

The rate likelihood in DyNAM/REM assumes h_i(t) = λ_i · h₀(t) with h₀ ≡ 1
(exponential). `.plan/sp/parametric_rates_dynam_rem.md` derives the Weibull
(common k) and Gompertz (common γ) extensions: closure under minima holds, the
choice sub-model is untouched (shape cancels from the mark kernel), and the
β-block of the score/Hessian is exactly the exponential one with Δt_m replaced
by the integrated baseline G_m(shape). The Cox partial likelihood (ordering
only) is the unspecified-baseline member of the same family and is already
implemented as the dev-line `rate_ordered` token.

Current surface: `estimate_dynam(sub_model = c("choice", "rate",
"rate_ordered", "choice_coordination"))`, `estimate_rem(sub_model = c("rate",
"rate_ordered", "choice"))`, `estimate_dynami(sub_model = c("choice",
"rate"))`, `make_specification(rate_sub_model = c("rate", "rate_ordered"))`.
`rate_ordered` did not exist at v1.7.0 → dev-line-only, free deletion.

Interview decisions (2026-08-19, recorded as ADR-0021): orthogonal
`distribution` argument; both changes fully pre-2.0.0; full C++ joint scoring;
axis applies to DyNAM rate, REM, and DyNAMi rate.

## Goals / Non-Goals

**Goals:**
- One orthogonal `distribution` axis on all three rate estimators and the
  specification object; `"exponential"` default bit-reproduces current fits.
- Joint (β, shape) estimation in the C++ engine with correct joint standard
  errors; profiling as initializer/fallback.
- Retire `rate_ordered` into `distribution = "cox"` with identical numbers.
- Shape inference surfaced in `summary()`; documented LR route via two fits.

**Non-Goals:**
- Parametric baselines for the two-sided coordination model (Cox-only there;
  see `two-sided-coordination` and the unresolved thinning question).
- Unit-specific shapes, renewal (unit-clock) models, piecewise-constant
  baselines, log-normal/log-logistic (no closure under minima), frailty.
- New user knobs on `set_algorithm_newton()` — safeguards are internal.

## Decisions

### D1 — `distribution` is an orthogonal argument, not new `sub_model` tokens
`sub_model` keeps naming the likelihood factor (rate vs choice); the
waiting-time distribution is a separate argument `distribution =
c("exponential", "weibull", "gompertz", "cox")` on `estimate_dynam()`,
`estimate_rem()`, `estimate_dynami()`, and `make_specification()`.
*Alternatives rejected*: flat tokens (`rate_weibull`, …) multiply against the
coordination mechanisms (4 × 5) and give a joint fit no honest single token;
spec-only placement would strand the still-useful direct formula path.
(ADR-0021.)

### D2 — `"rate"` stays the exponential-compatible family label
`sub_model = "rate"` + `distribution = "exponential"` (the default) is
byte-for-byte today's model, preserving backward compatibility and the frozen
baselines. `distribution` supplied together with a choice sub-model aborts
(inert-argument policy, ADR-0007) rather than being silently ignored.

### D3 — `rate_ordered` folds into `distribution = "cox"`, deleted outright
Dev-line-only token (absent at v1.7.0) → outright deletion per the dev-line
policy: no stub, no lifecycle shim, NEWS records the rename. The internal
REM-ordered / rate-ordered recipes become the `"cox"` branch of the rate
recipe; coefficients must match to 1e-6. `"cox"` (not `"coxph"`: that is
survival's function name, not a distribution; not `"ordered"`: it names the
data reduction, not the model).

### D4 — Shape parameterized on the estimation scale κ = log k (Weibull), γ raw
κ keeps k > 0 with no clipping (boundary guard by reparameterization);
Gompertz γ is unconstrained. Chain rule terms for the log scale are applied in
the C++ assembly. Reporting shows the natural-scale k̂ with delta-method SE
alongside κ̂; the Wald test is on κ = 0 (resp. γ = 0).

### D5 — Full C++ joint damped Fisher scoring (user decision)
The engine computes the shape row/column of the augmented information in the
same per-event pass as the β-blocks (∂G/∂shape terms accumulate next to the
G_m·Σλ_i x_i accumulators). Safeguards, all internal: step-halving until ℓ
increases; trust region capping |Δκ| (resp. |Δγ·scale|) at ~1.5 per
iteration; expected (Fisher) information substituted when the observed
Hessian loses negative definiteness; divergence monitor that reports
non-convergence of the shape (weak timing information) instead of iterating.
*Alternative rejected*: R-level outer loop feeding transformed exposures —
simpler but two passes per iteration and linear (block-coordinate)
convergence; kept only as the profiling fallback (D6).

### D6 — Profiling sweep as initializer and fallback
`stats::optimize()` over κ ∈ [−3, 3] (resp. γ bracket) of the profile
likelihood, where the inner problem is the **unmodified** fixed-shape fit on
transformed exposures G_m — globally concave, cannot get lost. One sweep
initializes κ for the joint update; the full profiling route is the fallback
when the joint update fails to converge.

### D7 — Gompertz series near γ = 0
When |γ·w| < ε, G and its derivatives switch to the series G ≈ w(1 + γw/2 +
γ²w²/6) to avoid 0/0 — this region is exactly where the exponential null is
evaluated, so it must be numerically clean.

### D8 — Weibull with zero waiting times aborts with guidance
w = 0 breaks the log w terms. The fit aborts with a cli error naming the
remedies (aggregate simultaneous events; jitter; bound waiting times below by
the time resolution) rather than silently jittering — a silent perturbation
of the data would be the kind of surface this repo has been removing.
Exponential and Cox paths are unaffected. (The general tied-event-times
question remains deferred per the release plan.)

### D9 — Shape inference: Wald automatic, LR by two fits
`summary()` reports the shape with its joint-information SE and a Wald test
of the exponential null. No automatic double fit for an LR test (it would
double estimation cost invisibly); the docs show the two-fit LR recipe and
note both nulls are interior points (plain χ²₁, no boundary correction).
Weibull vs Gompertz: equal parameter count, compare log-likelihoods (docs).

### D10 — Shape joins the parameter vector
The shape enters `coef()`/`vcov()` as a reserved named term (`log_shape` for
Weibull, `gamma` for Gompertz), like `survreg`'s Log(scale) row. This is what
makes the joint SEs (D5's rationale) visible to every downstream consumer
(`test_parameter`, delta methods) without a parallel accessor surface.

### D11 — Cross-validation against survival implementations
Covariate-only (no endogenous-statistic) fits are checkable against
`survival::survreg` (Weibull, via the AFT↔PH mapping) and a Gompertz
reference (`flexsurv` or `eha`), Suggests + test-only, skipped if absent.
Simulation-recovery tests cover the endogenous case the external packages
cannot fit.

### D12 — DyNAMi rate shares the axis and the machinery
`estimate_dynami(sub_model = "rate")` accepts `distribution` with the same
semantics (common clock, common shape). Its choice sub-model is untouched.
Testing scope: one recovery test on DyNAMi data; the heavy numerics tests run
on DyNAM/REM.

## Risks / Trade-offs

- [Joint Hessian indefinite far from optimum] → expected-information
  substitution + damping (D5); profiling fallback (D6).
- [Weak timing information: shape drifts, no interior optimum] → divergence
  monitor reports shape non-convergence explicitly; docs name the causes
  (few events, ties, covariates absorbing duration dependence).
- [k̂ < 1 read as burstiness when it is unmodeled heterogeneity] →
  documentation caveat (frailty/richer covariates comparison), not code.
- [Frozen baselines regress] → exponential default path is not edited
  behaviorally; every task verified with `not-cran-test` (PASS not SKIP);
  `cpp-recompile` after every `src/` edit.
- [Both this change and `two-sided-coordination` modify the same
  `model-specification` requirement] → this change archives first; the other
  change's delta is written against the post-archive wording (noted in its
  design).

## Migration Plan

Dev-line only: `rate_ordered` disappears in the same commit that makes
`distribution = "cox"` work; NEWS documents the mapping
(`sub_model = "rate_ordered"` → `sub_model = "rate", distribution = "cox"`).
No released users are affected (token absent from v1.7.0/CRAN).

## Open Questions

- None blocking. The coordination-side parametric question (thinning of
  rejected proposals under a parametric clock) is deliberately out of scope
  and recorded in ADR-0023 / `two-sided-coordination`.
