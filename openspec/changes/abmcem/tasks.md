> **Merge order (cross-change).** This change is coupled to `dynes-augmentation` (the
> augmenters, batched C++ evaluator, panel data path) — the coupling is
> **bidirectional**: `dynes-augmentation` task 4.3 swaps its real batched evaluator in
> *behind* this change's evaluator contract, and this change's `estimate_dynes()`
> surface wires `complete_generative_spec(wave_times = …)` *from* `dynes-augmentation`'s
> wave grid. **Landing order:** this change lands **first** against a
> **prototype-path (zero-iteration) evaluator** — that adapter is the deliberate
> integration seam that lets the surface, EM loop, and controls ship without the real
> augmenters; `dynes-augmentation` then retires the adapter and swaps in the real
> augmenters + batched evaluator. Keep this order in sync with the mirror note in
> `dynes-augmentation/tasks.md`.
>
> **Prerequisite (cross-change).** `set_algorithm_em(initial_parameters=)` accepts
> a `goldfishParams` and results group/pad via `coef_layout()` — both from
> the `joint-parameters` change, which therefore lands **before** this change's
> surface (tasks 1.2, 5.3). `joint-parameters` has no dependency on this change.

## 1. Control constructors and the warm-start option

- [ ] 1.1 Child constructors `set_augmenter_options()` (incl. `warm_start`),
      `set_weights_options()`, `set_sgd_options()` (`step_schedule` incl.
      `decay`; `convergence` gradient/iterations) with child-local cli
      validation (errors naming valid options; descriptive argument names per
      design D2)
- [ ] 1.2 `set_algorithm_em()` nesting the three: single `seed`, `n_cores` (CRAN
      2-core default), stop-rule quantiles, `stop_count`, `max_retries`,
      `em_trace_se`, and **`initial_parameters`** — a `goldfishParams`
      (`joint-parameters`) whose free (non-offset NA) slots are θ₀, default
      `NULL` → zero over the free set; **no `fixed_parameters` argument** (fixed
      = the spec's formula offsets, design D2/D4); the cross-object validity
      matrix and precedence table (warn-and-ignore / abort per design D4);
      `warm_start` (on `set_augmenter_options()`) wiring the **per-fid** internal
      default-`set_algorithm_newton()` fits, warn-and-ignored when
      `initial_parameters` is supplied — `set_algorithm_newton()` itself gains no
      argument; `devtools::document()`
- [ ] 1.2a Cold-start construction guard (design D4, dynes-augmentation
      D1/D14): `set_algorithm_em()` warns when `routine = "random"` meets a
      non-zero initial parameter vector or `warm_start = TRUE` (recommend a
      model-driven routine or θ₀ = 0); no abort, and silent for `random` at
      θ₀ = 0
- [ ] 1.3 Tests (testthat 3e): constructor validation snapshots, validity
      matrix warn-and-ignore cases, precedence fixtures, `warm_start`
      passthrough, cold-start construction-guard warning snapshot
      (`random` + non-zero θ₀ warns; `random` + θ₀ = 0 silent)
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
- [ ] 2.2 Classed E-step object (`goldfishEstepIS`/`goldfishEstepResampling`/
      `goldfishEstepUniform` inheriting `goldfishDynesEstep`) and internal
      `compute_q()`/`compute_ase()`
      generics with scheme-correct estimators (design D8); MCMC
      autocorrelation index recorded for the trace
- [ ] 2.2a Cold-start ESS diagnostic (design D5, dynes-augmentation D14): a
      one-shot startup diagnostic, distinct from the recurring guard warning,
      fires when first-iteration ESS is below a hard pathology floor (strictly
      under `ess_threshold` × K); it names the proposal/target mismatch and the
      mitigations and never redraws or grows the pool; quiet on the default
      θ₀ = 0 path
- [ ] 2.3 Tests: hand-computed weight/ESS fixtures, reweighting-ratio
      fixtures, scheme-dispatch equivalences against closed forms from an
      analytic stub evaluator (design D11), guard-warning snapshots, cold-start
      diagnostic snapshot (fires below the pathology floor, distinct from the
      guard warning, silent at θ₀ = 0)
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
      sub-models, harvesting `log_likelihood`/`final_score`/`final_information_matrix`
      from `estimate_wrapper()` at `max_iterations = 0L`, honoring `what` at
      the harvest level; **degenerate-sequence guard (D-ROBUST)** — wrap the
      per-sequence engine call in `tryCatch` so a singular information matrix
      (the engine inverts it unconditionally and `stop()`s) yields a non-finite
      per-sequence result instead of aborting the pool; `devtools::document()`
- [ ] 4.2a v1 attachment-optional conformance pin (design D3, `dynes-augmentation`
      D23): the pool-entry record constructor (4.1) accepts an **optional**
      precomputed `(stats, loglik@θ_ref)` attachment argument — **ignored in v1**
      (every `routine` takes the from-scratch A0 path), present only so the A3
      fast-follow is a drop-in with no signature churn; and the internal
      `make_proposal_evaluator()` closure returns a **named list** whose contract can
      widen from `(loglik, rates)` to add a `stats_handle` without breaking callers.
      No reuse logic ships here — this task only fixes the two shapes so the seam
      widen (5.1's deferred note) touches exactly one return contract later
- [ ] 4.3 Tests: adapter equals direct zero-iteration engine evaluation on an
      event-stream toy fixture; preprocessing runs once per sequence across
      repeated evaluations; numerical cross-check against the
      `.plan/dynes/` prototype behavior where feasible (design D11); v1
      attachment-optional conformance (4.2a): pool entry succeeds with **no**
      attachment and takes the from-scratch path, a supplied attachment is
      **ignored** in v1 (result byte-identical to the no-attachment build), and
      `make_proposal_evaluator()` returns a named-list contract (widen-able return
      pinned by name, not positionally); **degenerate-sequence guard (4.2)** — a
      deliberately collinear sequence in the pool yields a non-finite result and
      the pool evaluation completes for the rest, with no matrix-inversion abort
- [ ] 4.4 Verification `NOT_CRAN=true` (PASS not SKIP); version bump + NEWS
      (evaluator-adapter milestone); commit

## 5. ABEM loop, estimation surface, and results

- [ ] 5.1 The ABEM loop per design D7, written purely against the three
      contracts: accept/grow/stop from `compute_q()`/`compute_ase()`, bounded
      ⌈pool/`max_retries`⌉ within-iteration growth, **`cli_abort()` on
      within-iteration retry exhaustion** but **`max_iterations` exhaustion
      returns the last accepted θ with a non-convergence warning and
      `converged = FALSE`** (design D7, like the Newton estimators), always-on
      `em_trace` with opt-in per-iteration SEs; contract-conformant stub
      augmenter as a test fixture (design D11)
      **MCMC pool-reuse fast-follow — abmcem's single seam piece (`dynes-augmentation`
      design D23).** The ABEM loop builds `make_proposal_evaluator(spec, θ_k)` per EM
      iteration and injects it into the augmenter's `init()`. A retained MCMC draw's
      θ-free statistics + `loglik@θ_k` are already materialized inside the chain's
      acceptance ratio, so reuse into the pool is retain-vs-discard (never CPU-worse than
      the from-scratch baseline). Of D23's A1/A2/A3 alternatives, **exactly one piece is
      abmcem's**: widening this closure's return from `(loglik, rates)` to
      `(loglik, rates, stats_handle)`. **Deferred — not v1**: v1 ships the from-scratch
      baseline (D23 A0). The stats-emitting primitive and the attachment-optional
      pool-entry path are `dynes-augmentation`'s (its tasks 4.2/4.3, built A0 now so the
      widened return is a drop-in later); the accepted-not-last-proposal correctness
      guard is its task 3.4. Keep this deferral in sync with `dynes-augmentation` D23.
- [ ] 5.2 `estimate_dynes()` surface: lifecycle experimental badge, θ₀ from
      `set_algorithm_em()`'s `initial_parameters` (zero default) or the
      `warm_start` augmenter option (one internal default-`set_algorithm_newton()`
      fit), the mirai-backed
      parallel seam behind the serial default (Suggests dependency, non-nested
      `workers × BLAS threads ≤ cores` budget); the seam seeds one
      `L'Ecuyer-CMRG` root from the single `seed`, keys augmentation substreams by
      `(iteration, draw_index)` (worker-independent, spawned lazily), keeps the MCMC
      chain and control (batch/resample) substreams coordinator-side, injects each
      substream state into its draw task, and **reduces per-sequence results in
      sequence-index order** (design D10, resolved RNG open question); mirai added to
      Suggests; `devtools::document()`.
      **Reproducibility doc (design D10, resolved RNG open question):** the `seed`
      (`set_algorithm_em()`) and `n_cores` roxygen state the **two-part promise** — at a
      fixed `seed` the **drawn sequences** are identical across any `n_cores` (serial
      included, even across machines: pure-R augmenter draws, no BLAS), while the
      **numeric estimates** are identical across any `n_cores` only on the **same machine
      / BLAS build** (compiled kernels + multithreaded BLAS round per build, so
      cross-machine bit-for-bit estimates are never promised). Documentation-only — the
      cross-machine caveat is not a CI-testable assertion.
- [ ] 5.3 Result class per design D9: Fisher approximation, **flat** `coef()`/
      `vcov()` named by the `joint-parameters` composite labels (offset effects
      padded via the existing `GetFixed()`/`stats::.vcov.aliased()`, **not**
      `add_na_rowcol()`), MC standard errors, the `converged` flag, and
      `em_trace` carried on the object; `print()`/`summary()` via cli semantic
      elements grouping the flat coefficients per process via `coef_layout()`,
      asymptotic and MC error side by side plus the `converged` status,
      pinned-context snapshots
- [ ] 5.4 Tests: end-to-end toy ascent (stub augmenter + real adapter + real
      SGD, seeded recovery within bounds, skip_on_cran), grow-then-accept,
      retry-abort, and **`max_iterations`-returns-last-accepted-with-warning**
      (`converged = FALSE`) fixtures, `em_trace` content fixture,
      control-dispatch test
      (no variant branching in the loop), **cross-worker-count reproducibility**
      (identical drawn pool and estimates across `n_cores ∈ {1, 2, ...}` at fixed
      `seed` — Option B keying plus index-ordered reduction, design D10),
      §2.2-vs-§3.3 SE cross-check fixture
- [ ] 5.5 Final verification: full `NOT_CRAN=true` suite (frozen baselines
      PASS not SKIP); version bump in DESCRIPTION + NEWS.md entry (ABMCEM
      milestone); commit
