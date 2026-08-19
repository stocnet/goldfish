# Tasks: two-sided-coordination

## 1. Estimator surface and backward-compatible bridge

- [ ] 1.1 `estimate_dynamu()` skeleton: signature (positional choice formula,
      `rate =`, `acceptance =`, `mechanism =`, `data`), validation (acceptance
      gated to confirmation both ways, cli errors), conjunctive constant-rate
      path delegating to the existing coordination machinery via a typed spec
- [ ] 1.2 Bridge test: conjunctive constant-rate `estimate_dynamu()` equals
      `estimate_dynam(sub_model = "choice_coordination")` to 1e-6 on existing
      fixtures
- [ ] 1.3 Deprecation redirect: `estimate_dynam(sub_model =
      "choice_coordination")` → `deprecate_warn()` (single-hop, `with =`) +
      forward to `estimate_dynamu()`; snapshot the message; NEWS entry with
      the removal horizon (r-lib:lifecycle skill)
- [ ] 1.4 Delete `"choice_coordination"` from `make_specification()`'s
      `choice_sub_model` with a cli error naming `estimate_dynamu()`; NEWS
      records the dev-line deletion
- [ ] 1.5 Roxygen for the new estimator (`@inheritParams` where shared),
      `devtools::document()`; verify with not-cran-test (baselines PASS);
      bump DESCRIPTION + NEWS (phase milestone)

## 2. Mechanism likelihoods, constant rates (r backend reference)

- [ ] 2.1 Generic score/Hessian assembly (∂g_obs − E_D[∂g]; Hessian with
      Cov_D term) over per-mechanism g-functions; primitives: multinomial
      moments (μ, V), Bernoulli acceptance, rate-sum, log-sum mixture rule
- [ ] 2.2 g-functions for forcing and disjunctive (constant rates); left-limit
      (t⁻) discipline asserted by a consecutive-same-dyad test
- [ ] 2.3 g-function for confirmation incl. the acceptance block (α) wired
      from the `acceptance` formula through preprocessing
- [ ] 2.4 g-function for compensatory (s_kl + s_lk sum statistic; globally
      concave under constant rates)
- [ ] 2.5 Simulation fixtures for the five constant-rate mechanisms
      (extend the `.plan/datasets/Simulation.R` approach; DGP matches each
      mechanism's own thinning construction) as testthat fixtures
- [ ] 2.6 Recovery tests per mechanism; mechanism-comparison test
      (log-likelihood ranking favors the generating mechanism on average);
      verify with not-cran-test; bump DESCRIPTION + NEWS (phase milestone)

## 3. Joint estimation with actor-varying rates

- [ ] 3.1 Additive rate block (conjunctive, disjunctive, compensatory):
      log(ρ̃_k + ρ̃_l) with ν-weights in score/Hessian; joint (β, θ)
      optimization
- [ ] 3.2 Mixture-weight variants (forcing, confirmation): mixture rule for
      the (β, θ[, α]) cross-blocks; joint optimization only, no two-stage
      route offered
- [ ] 3.3 Safeguards: step-halving, OPG-information fallback on
      indefiniteness, non-convergence report with suspected cause (no mixed
      dyads / mixture identification / ridge)
- [ ] 3.4 Joint recovery tests (mixed-dyad designs); identification-failure
      test (constant covariate reports, does not return spurious estimates);
      verify with not-cran-test; bump DESCRIPTION + NEWS (phase milestone)

## 4. C++ port and backend parity

- [ ] 4.1 Port the mechanism g-derivative assembly to the cpp coordination
      engine (cpp-recompile skill after every src/ edit)
- [ ] 4.2 Parity tests r vs cpp per mechanism and regime (per-event
      contributions and final fits, established parity tolerances)
- [ ] 4.3 Verify group: cpp-recompile then not-cran-test with frozen
      baselines PASS

## 5. Reporting, methods, and documentation

- [ ] 5.1 Result object: block-labeled coefficients (rate / choice /
      acceptance), `coef()`/`vcov()`/`logLik()`; class name per the
      class-naming scheme
- [ ] 5.2 `summary()`/`print()` with per-block tables and the mechanism in
      the header (cli-rendered, pinned-context snapshots)
- [ ] 5.3 Documentation: mechanism catalogue with the φ table, constant-rate
      vs joint regimes, identifiability requirements, AIC comparison of
      non-nested mechanisms, and the Cox-only rationale (thinning caveat,
      ADR-0023)
- [ ] 5.4 Final verification: full NOT_CRAN suite green with baselines PASS,
      `devtools::document()`, air format + lintr on touched files, bump
      DESCRIPTION + NEWS (change complete)
