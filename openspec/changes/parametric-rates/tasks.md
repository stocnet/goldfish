# Tasks: parametric-rates

## 1. Vocabulary and argument surface

- [ ] 1.1 Add `distribution = c("exponential", "weibull", "gompertz", "cox")`
      to `estimate_dynam()`, `estimate_rem()`, `estimate_dynami()`: match.arg,
      rate-family-only validation (cli abort on choice sub-models), threaded
      into the internal bundle/recipe untouched by later tasks
- [ ] 1.2 Delete the `rate_ordered` token from all estimator signatures and
      internal vocabulary; map `distribution = "cox"` onto the existing
      partial-likelihood recipe (identical coefficients); no-intercept
      exponential rate formula aborts pointing at `distribution = "cox"`
- [ ] 1.3 Add `distribution` to `make_specification()` (stored, validated,
      shown by print; abort when no rate formula), drop `"rate_ordered"` from
      `rate_sub_model`; update estimate-from-specification wiring
- [ ] 1.4 Roxygen for the new argument (documented once, `@inheritParams`
      elsewhere), `devtools::document()`, NEWS entry with the
      `rate_ordered` → `distribution = "cox"` mapping
- [ ] 1.5 Re-key the merged multivariate substrate's regime map to the
      distribution axis (D13): `is_timed_joint_specification()` reads
      `distribution == "exponential"`, `assert_compatible_regime()` reads
      `"cox"` (cli message updated), intercept-only-rate regime guard and
      `complete_generative_spec()` completion wording follow;
      `make_joint_specification()` rejects composed weibull/gompertz
      processes with a cli error
- [ ] 1.6 Surface tests: defaults reproduce exponential fits (1e-6), cox
      reproduces rate_ordered (1e-6), aborts for choice sub-model,
      spec-without-rate, and weibull/gompertz in a joint composition; verify
      with not-cran-test (baselines PASS), bump DESCRIPTION + NEWS (phase
      milestone)

## 2. Fixed-shape machinery (transformed exposures)

- [ ] 2.1 Compute integrated-baseline exposures G_m(shape) for weibull and
      gompertz in the preprocessing/likelihood layer (piecewise-constant
      covariate intervals split and accumulated); weibull zero-waiting-time
      abort with cli guidance
- [ ] 2.2 Fixed-shape estimation path: exponential engine on transformed
      exposures; test the fixed-shape β equals a transformed-exposure
      exponential fit
- [ ] 2.3 Cross-validation tests against survival packages (Suggests,
      test-only, skip-if-absent): covariate-only Weibull vs
      `survival::survreg` via the AFT↔PH mapping; Gompertz vs
      flexsurv/eha reference
- [ ] 2.4 Verify group: not-cran-test PASS (baselines untouched),
      `devtools::document()` if signatures moved

## 3. C++ joint shape scoring

- [ ] 3.1 Extend the cpp rate engine with the shape row/column: per-event
      accumulation of ∂G/∂shape and ∂²G/∂shape² terms next to the β-block
      accumulators, κ = log k chain rule for weibull, gompertz series
      switch for small |γw| (cpp-recompile skill after every src/ edit)
- [ ] 3.2 Damped joint update: step-halving on the log-likelihood, trust
      region on the shape step, expected-information substitution when the
      observed Hessian loses definiteness, divergence monitor reporting
      shape non-convergence
- [ ] 3.3 Joint SEs: full-information inverse feeding `vcov()`; reserved
      coefficient names `log_shape` / `gamma` in `coef()`/`vcov()`
- [ ] 3.4 Simulation-recovery tests: weibull k < 1 and k > 1 recovery,
      gompertz γ ≠ 0 recovery, exponential data under weibull gives k̂ ≈ 1;
      weak-information divergence test reports instead of looping
- [ ] 3.5 Verify group: cpp-recompile then not-cran-test with baselines PASS
      (exponential path bit-stable), bump DESCRIPTION + NEWS (phase milestone)

## 4. Profiling initializer and fallback

- [ ] 4.1 Profile-likelihood sweep (`stats::optimize()` over the shape
      bracket, inner fixed-shape fit warm-started) used to initialize the
      joint update
- [ ] 4.2 Fallback route on joint non-convergence, with the result noting
      profiling produced the fit; tests exercise the fallback on a
      constructed hard case
- [ ] 4.3 Verify group: not-cran-test PASS

## 5. Reporting and documentation

- [ ] 5.1 `summary()`/`print()` show distribution and natural-scale shape
      with delta-method SE and Wald test vs the exponential null
      (cli-rendered, snapshot tests with pinned cli context)
- [ ] 5.2 Documentation: LR-by-two-fits recipe, Weibull-vs-Gompertz
      comparison by log-likelihood, burstiness-vs-heterogeneity caveat,
      model-choice table from the derivation note
- [ ] 5.3 DyNAMi rate: enable the axis, one recovery test on DyNAMi data
- [ ] 5.4 Final verification: full NOT_CRAN suite green with baselines PASS,
      `devtools::document()`, air format + lintr on touched files, bump
      DESCRIPTION + NEWS (change complete)
