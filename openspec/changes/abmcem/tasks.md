## 1. Control constructors and the warm-start option

- [ ] 1.1 Child constructors `set_alg_augment()`, `set_alg_weights()`,
      `set_alg_sgd()` with child-local cli validation (errors naming valid
      options; descriptive argument names per design D2)
- [ ] 1.2 `set_alg_em()` nesting the three: single `seed`, `n_cores` (CRAN
      2-core default), stop-rule quantiles, `max_retries`, `em_trace_se`; the
      cross-object validity matrix and precedence table (warn-and-ignore /
      abort per design D4); the `set_estimation_opt()` warm-start
      initial-parameters option (inert outside augmentation-based
      estimation); `devtools::document()`
- [ ] 1.3 Tests (testthat 3e): constructor validation snapshots, validity
      matrix warn-and-ignore cases, precedence fixtures, warm-start option
      passthrough
- [ ] 1.4 Verification: full `NOT_CRAN=true` run (frozen baselines PASS not
      SKIP); version bump in DESCRIPTION + NEWS.md entry (controls milestone);
      commit

## 2. Weighting machinery and Q/ASE dispatch

- [ ] 2.1 Weight state per design D5: permanent per-sequence reference records
      (θ_ref, log-likelihood at θ_ref, log proposal density) on the log
      scale; likelihood-ratio reweighting with multiple-importance-sampling
      combination for mixed-θ_ref pools; pre-normalization `transformation`
      (with the resampling warning); stratified/residual/random resampling;
      `refresh` mode and the ESS guard with its two warnings (never at
      startup)
- [ ] 2.2 Classed E-step object (`estep_is`/`estep_resampling`/`estep_uniform`
      inheriting `dynes_estep`) and internal `compute_q()`/`compute_ase()`
      generics with scheme-correct estimators (design D8); MCMC
      autocorrelation index recorded for the trace
- [ ] 2.3 Tests: hand-computed weight/ESS fixtures, reweighting-ratio
      fixtures, scheme-dispatch equivalences against closed forms from an
      analytic stub evaluator (design D11), guard-warning snapshots
- [ ] 2.4 Verification `NOT_CRAN=true` (PASS not SKIP); version bump + NEWS
      (weighting milestone); commit

## 3. SGD M-step optimizer

- [ ] 3.1 Optimizer contract + `optimize_abem_sgd()` per design D6: weighted
      with-replacement batches with unweighted means (default) and
      deterministic cyclic batches with importance-weighted gradients
      (weight × K/B); constant default step size plus AdaGrad/Adam/momentum at
      fixed literature defaults; accumulator reset per M-step; score-only
      evaluation requests; fixed-parameter step masking;
      `devtools::document()`
- [ ] 3.2 Tests: known-optimum convergence on the analytic stub evaluator,
      cyclic-vs-weighted expected-gradient equivalence fixture, fixed
      parameters immobile, fresh-state-per-M-step check
- [ ] 3.3 Verification `NOT_CRAN=true` (PASS not SKIP); version bump + NEWS
      (optimizer milestone); commit

## 4. Prototype-path pool evaluator

- [ ] 4.1 Pool structure and entry path: per-sequence data build +
      preprocess-once through the existing pipeline, stored statistics,
      reference-record attachment; the internal per-sequence map seam (serial
      `lapply()` default) per design D10
- [ ] 4.2 Zero-iteration adapter per design D3: `evaluate_sequence_pool(pool,
      theta, what)` splitting/reassembling the concatenated θ across
      sub-models, harvesting logLikelihood/finalScore/finalInformationMatrix
      from `estimate_wrapper()` at `max_iterations = 0L`, honoring `what` at
      the harvest level; `devtools::document()`
- [ ] 4.3 Tests: adapter equals direct zero-iteration engine evaluation on an
      event-stream toy fixture; preprocessing runs once per sequence across
      repeated evaluations; numerical cross-check against the
      `.plan/dynes/` prototype behavior where feasible (design D11)
- [ ] 4.4 Verification `NOT_CRAN=true` (PASS not SKIP); version bump + NEWS
      (evaluator-adapter milestone); commit

## 5. ABEM loop, estimation surface, and results

- [ ] 5.1 The ABEM loop per design D7, written purely against the three
      contracts: accept/grow/stop from `compute_q()`/`compute_ase()`, bounded
      ⌈pool/`max_retries`⌉ within-iteration growth, `cli_abort()` on retry or
      iteration exhaustion with trace-derived diagnosis, always-on `em_trace`
      with opt-in per-iteration SEs; contract-conformant stub augmenter as a
      test fixture (design D11)
- [ ] 5.2 `estimate_dynes()` surface: lifecycle experimental badge, θ₀ via
      `set_estimation_opt()` (zero default / warm start), the mirai-backed
      parallel seam behind the serial default (Suggests dependency, parallel
      RNG streams, non-nested `workers × BLAS threads ≤ cores` budget,
      seed-identical serial vs parallel); mirai added to Suggests;
      `devtools::document()`
- [ ] 5.3 Result class per design D9: Fisher approximation, `vcov()` with
      NA-padded fixed parameters, MC standard errors, `em_trace` carried on
      the object; `print()`/`summary()` via cli semantic elements with
      asymptotic and MC error side by side, pinned-context snapshots
- [ ] 5.4 Tests: end-to-end toy ascent (stub augmenter + real adapter + real
      SGD, seeded recovery within bounds, skip_on_cran), grow-then-accept and
      abort-path fixtures, `em_trace` content fixture, control-dispatch test
      (no variant branching in the loop), serial-vs-parallel seed identity,
      §2.2-vs-§3.3 SE cross-check fixture
- [ ] 5.5 Final verification: full `NOT_CRAN=true` suite (frozen baselines
      PASS not SKIP); version bump in DESCRIPTION + NEWS.md entry (ABMCEM
      milestone); commit
