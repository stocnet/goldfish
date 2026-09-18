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
> **`process-simulation` status (2026-09-18).** Its driver, five plug points,
> exponential clock, guard/pool surface and walk-handle lifts have landed on
> `feature_simulation` and are merged into this branch (`8c8275d`). This change
> does not consume `simulate()` directly, but two things follow for it: the
> result class now owes a **`simulate` verdict** in `fit-class-hierarchy`
> (task 5.3c, design D9), and `initial_parameters` stays one-form where
> `simulate()`'s `coef` took three (design D2 note).
>
> **Prerequisite (cross-change).** `set_algorithm_em(initial_parameters=)` accepts
> a `goldfishParams` and results group/pad via `coef_layout()` — both from
> the `joint-parameters` change, which therefore lands **before** this change's
> surface (tasks 1.2, 5.3). `joint-parameters` has no dependency on this change.

> **TDD.** Implementation tasks below run as red/green pairs — an "(a)" task
> writes a failing test and commits it RED, the paired "(b)" task makes it pass and
> commits GREEN (per `openspec/config.yaml` `rules.tasks`). Spikes, studies, vignettes,
> and characterization tests over already-implemented behavior are not paired — there
> is no new implementation for them to drive red-then-green — and stay single tasks.

## 1. Control constructors and the warm-start option

- [x] 1.1a Write failing tests for the child constructors: constructor
      validation snapshots (incl. `transformation` enum rejects a non-member
      and `sgd_tolerance` is the M-step tolerance name). Run to confirm RED;
      commit the tests alone.
- [x] 1.1b Implement: child constructors `set_augmenter_options()` (incl.
      `warm_start`), `set_weights_options()` (`transformation` a **named enum**
      `c("identity", "clipping", "smoothing")`, not a user function, design
      D2/D5), `set_sgd_options()` (`step_schedule` incl.
      `decay`; `convergence` gradient/iterations; the M-step tolerance named
      **`sgd_tolerance`** — distinct from the EM `tolerance`, design D2/D6,
      default `1e-3`; `step_size` default `0.001`; **`batch_size` a fixed
      absolute count defaulting to `NULL`**, resolved at the parent in task 1.2,
      design D6) with child-local cli
      validation (errors naming valid options; descriptive argument names per
      design D2). Re-run the 1.1a tests to confirm GREEN; commit.
- [x] 1.2a Write failing tests for `set_algorithm_em()`: validity matrix
      warn-and-ignore cases, precedence fixtures, and `warm_start` passthrough
      + **precedence** (a supplied `initial_parameters` warn-and-ignores the
      augmenter's `warm_start = TRUE`; the explicit start wins). Run to confirm
      RED; commit the tests alone. (The **warm-start-fit-failure fallback** — a
      fid whose singular fit falls back to zero with a fid-naming warning — is a
      runtime behavior needing a drawn sequence and the Group-4 evaluator path,
      so it and its test are deferred to task 5.2; see there.)
- [x] 1.2b Implement: `set_algorithm_em()` nesting the three: single `seed`, `n_cores` (CRAN
      2-core default), stop-rule quantiles, `stop_count`, `max_retries`,
      **`max_pool = 5000L`** (clamps the post-acceptance pool-sizing rule; first
      pass to bind the cap warns once — design D2/D7),
      `em_trace_se`, and **`initial_parameters`** — **defaults pinned from the
      prototype driver** (design D2/Open-Q `[defaults]`): `max_iterations = 50L`,
      `accept_quantile = 0.2`, `growth_quantile = 0.2`, `stop_quantile = 0.1`,
      `tolerance = 1e-3`, `stop_count = 1L`, `max_retries = 20L`, and
      **`n_sequences = 100L`** (no prototype default; a shipped default since the
      constructor exports — the toy fixtures override it) — a `goldfishParams`
      (`joint-parameters`) whose free (non-offset NA) slots are θ₀, default
      `NULL` → zero over the free set; **no `fixed_parameters` argument** (fixed
      = the spec's formula offsets, design D2/D4); the cross-object validity
      matrix and precedence table (warn-and-ignore / abort per design D4);
      the **`warm_start` precedence** rule, enforced here at construction: when
      `initial_parameters` is supplied and the augmenter's `warm_start = TRUE`,
      warn and disable the warm start (the explicit start wins) —
      `set_algorithm_newton()` itself gains no argument. (The per-fid warm-start
      **fit** on a drawn sequence and its singular-fit→zero fallback with a
      fid-naming warning are runtime behaviors deferred to the loop, task 5.2b,
      design D7 — `set_algorithm_em()` never sees the `spec`.) **Resolve the
      nested `set_sgd_options()` `batch_size = NULL`
      default to `min(n_sequences, 10)` once here and freeze it** (design D6);
      `devtools::document()`. Re-run the 1.2a tests to confirm GREEN; commit.
- [x] 1.3a Write failing test for the cold-start construction guard:
      cold-start construction-guard warning snapshot (`random` + non-zero θ₀
      warns; `random` + θ₀ = 0 silent). Run to confirm RED; commit the test
      alone.
- [x] 1.3b Implement: cold-start construction guard (design D4, dynes-augmentation
      D1/D14): `set_algorithm_em()` warns when `routine = "random"` meets a
      non-zero initial parameter vector or `warm_start = TRUE` (recommend a
      model-driven routine or θ₀ = 0); no abort, and silent for `random` at
      θ₀ = 0. Re-run the 1.3a test to confirm GREEN; commit.
- [x] 1.4 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      `devtools::document()` if this group touched roxygen/exports; commit.
      **No DESCRIPTION version bump and no root `NEWS.md` edit
      on this branch** — record the controls milestone as a bullet in the
      change-local `openspec/changes/abmcem/NEWS.md`. The version bump,
      root-NEWS fold, and archival are deferred to branch merge (see
      progress.md "Version / NEWS / archival")

## 2. Weighting machinery and Q/ASE dispatch

- [x] 2.1a Write failing tests for weight state: hand-computed weight/ESS
      fixtures, reweighting-ratio fixtures, guard-warning snapshots (the ESS
      guard's two warnings). Run to confirm RED; commit the tests alone.
- [x] 2.1b Implement: weight state per design D5: permanent per-sequence reference records
      (θ_ref, log-likelihood at θ_ref, log proposal density) on the log
      scale; likelihood-ratio reweighting (the Casella & Levine 2001
      sample-reuse update — add the cite to `inst/REFERENCES.bib`) with
      multiple-importance-sampling combination for mixed-θ_ref pools;
      **MCMC + `refresh = FALSE` defaults to `weighting = "importance"` and
      warns→importance on `weighting = "uniform"`, with both
      `use = "importance"` and `use = "resampling"` valid** (design D4);
      pre-normalization `transformation`
      (with the resampling warning); stratified/residual/random resampling;
      `refresh` mode and the ESS guard with its two warnings (never at
      startup). Re-run the 2.1a tests to confirm GREEN; commit.
- [x] 2.2a Write failing tests for the classed E-step object and
      `compute_q()`/`compute_ase()` generics: scheme-dispatch equivalences
      against closed forms from an analytic stub evaluator (design D11);
      negative-variance ASE floor (a near-constant-Λ resampling fixture
      yields ASE = 0 not `NaN`, records the `ase_floored` flag; design D8).
      Run to confirm RED; commit the tests alone.
- [x] 2.2b Implement: classed E-step object (`goldfishEstepIS`/`goldfishEstepResampling`/
      `goldfishEstepUniform` inheriting `goldfishDynesEstep`) and internal
      `compute_q()`/`compute_ase()`
      generics with scheme-correct estimators (design D8); MCMC
      autocorrelation index recorded for the trace. Re-run the 2.2a tests to
      confirm GREEN; commit.
- [x] 2.3a Write failing test for the cold-start ESS diagnostic: cold-start
      diagnostic snapshot (fires below the pathology floor, distinct from the
      guard warning, silent at θ₀ = 0). Run to confirm RED; commit the test
      alone.
- [x] 2.3b Implement: cold-start ESS diagnostic (design D5, dynes-augmentation D14): a
      one-shot startup diagnostic, distinct from the recurring guard warning,
      fires when first-iteration ESS is below a hard pathology floor (strictly
      under `ess_threshold` × K); it names the proposal/target mismatch and the
      mitigations and never redraws or grows the pool; quiet on the default
      θ₀ = 0 path. Re-run the 2.3a test to confirm GREEN; commit.
- [x] 2.4a Write failing tests for the runtime ESS-collapse recommendation
      and restart-warning dedup: **runtime ESS-collapse recommendation**
      snapshot (fires under a high-dimension IS pool, names
      `augment_seq_mcmc()`, distinct from the redraw guard) and
      **restart-warning dedup** (a run with many restarts emits exactly one
      coupling warning; a coupling-free run emits none). Run to confirm RED;
      commit the tests alone.
- [x] 2.4b Implement: runtime ESS-collapse recommendation + restart-warning dedup (design
      D5, dynes-augmentation D14/#5 and D20): (a) when mid-run ESS collapses under
      an importance-weighted augmenter (`routine = "random"`/`"sim"`) the loop
      emits an actionable cli recommendation to switch to `augment_seq_mcmc()` —
      **this change owns the message surface; the numeric ESS floor comes from
      `dynes-augmentation`'s 6.2/6.3 study** (do not hard-code a guess) — distinct
      from the redraw guard and the cold-start diagnostic; the recommendation only
      informs, never switches augmenters; (b) the loop owns the **run-level dedup
      state** for the `sim` reject-and-restart coupling warning, so **exactly one**
      warning fires per estimation run (first restart at any iteration/draw),
      never per iteration or per draw. Re-run the 2.4a tests to confirm GREEN;
      commit.
- [x] 2.5 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      `devtools::document()` if this group touched roxygen/exports; commit.
      **No DESCRIPTION version bump and no root `NEWS.md` edit on this
      branch** — record the weighting milestone as a bullet in the
      change-local `openspec/changes/abmcem/NEWS.md` (see progress.md
      "Version / NEWS / archival")

## 3. SGD M-step optimizer

- [x] 3.1a Write failing tests for the optimizer: known-optimum convergence
      on the analytic stub evaluator, cyclic-vs-weighted expected-gradient
      equivalence fixture, fixed parameters immobile, fresh-state-per-M-step
      check. Run to confirm RED; commit the tests alone.
- [x] 3.1b Implement: optimizer contract + `optimize_abem_sgd()` per design D6: weighted
      with-replacement batches with unweighted means (default) and
      deterministic cyclic batches with importance-weighted gradients
      (weight × K/B); constant default step size plus AdaGrad/Adam/momentum at
      fixed literature defaults; accumulator reset per M-step; score-only
      evaluation requests; fixed-parameter step masking;
      `devtools::document()`. Re-run the 3.1a tests to confirm GREEN; commit.
- [ ] 3.2 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      `devtools::document()` if this group touched roxygen/exports; commit.
      **No DESCRIPTION version bump and no root `NEWS.md` edit on this
      branch** — record the optimizer milestone as a bullet in the
      change-local `openspec/changes/abmcem/NEWS.md` (see progress.md
      "Version / NEWS / archival")

## 4. Prototype-path pool evaluator

- [ ] 4.1a Write failing test for pool structure and entry path:
      preprocessing runs once per sequence across repeated evaluations. Run
      to confirm RED; commit the test alone.
- [ ] 4.1b Implement: pool structure and entry path: per-sequence data build +
      preprocess-once through the existing pipeline, stored statistics,
      reference-record attachment; the internal per-sequence map seam (serial
      `lapply()` default) per design D10. Re-run the 4.1a test to confirm
      GREEN; commit.
- [ ] 4.2a Write failing tests for the zero-iteration adapter: adapter equals
      direct zero-iteration engine evaluation on an event-stream toy fixture;
      numerical cross-check against the `.plan/dynes/` prototype behavior
      where feasible (design D11); **degenerate-sequence guard (4.2)** — a
      deliberately collinear sequence in the pool yields a non-finite result
      and the pool evaluation completes for the rest, with no
      matrix-inversion abort. Run to confirm RED; commit the tests alone.
- [ ] 4.2b Implement: zero-iteration adapter per design D3: `evaluate_sequence_pool(pool,
      theta, what)` splitting/reassembling the concatenated θ across
      sub-models, harvesting `log_likelihood`/`final_score`/`final_information_matrix`
      from `estimate_wrapper()` at `max_iterations = 0L`, honoring `what` at
      the harvest level; **degenerate-sequence guard (D-ROBUST)** — wrap the
      per-sequence engine call in `tryCatch` so a singular information matrix
      (the engine inverts it unconditionally and `stop()`s) yields a non-finite
      per-sequence result instead of aborting the pool; `devtools::document()`.
      Re-run the 4.2a tests to confirm GREEN; commit.
- [ ] 4.3a Write failing tests for the v1 attachment-optional conformance
      pin: pool entry succeeds with **no** attachment and takes the
      from-scratch path, a supplied attachment is **ignored** in v1 (result
      byte-identical to the no-attachment build), and
      `make_proposal_evaluator()` returns a named-list contract (widen-able
      return pinned by name, not positionally). Run to confirm RED; commit
      the tests alone.
- [ ] 4.3b Implement: v1 attachment-optional conformance pin (design D3, `dynes-augmentation`
      D23): the pool-entry record constructor (4.1) accepts an **optional**
      precomputed `(stats, loglik@θ_ref)` attachment argument — **ignored in v1**
      (every `routine` takes the from-scratch A0 path), present only so the A3
      fast-follow is a drop-in with no signature churn; and the internal
      `make_proposal_evaluator()` closure returns a **named list** whose contract can
      widen from `(loglik, rates)` to add a `stats_handle` without breaking callers.
      No reuse logic ships here — this task only fixes the two shapes so the seam
      widen (5.1's deferred note) touches exactly one return contract later.
      Re-run the 4.3a tests to confirm GREEN; commit.
- [ ] 4.4 Closing: full `NOT_CRAN=true` run (frozen baselines PASS not SKIP);
      `devtools::document()` if this group touched roxygen/exports; commit.
      **No DESCRIPTION version bump and no root `NEWS.md` edit on this
      branch** — record the evaluator-adapter milestone as a bullet in the
      change-local `openspec/changes/abmcem/NEWS.md` (see progress.md
      "Version / NEWS / archival")

## 5. ABEM loop, estimation surface, and results

- [ ] 5.1a Write failing tests for the ABEM loop: end-to-end toy ascent
      (stub augmenter + real adapter + real SGD, seeded recovery within
      bounds, skip_on_cran); grow-then-accept; **retry-abort** (asserting the
      accumulated `em_trace` is attached to the error condition);
      **`max_iterations`-returns-last-accepted-with-warning**
      (`converged = FALSE`) fixture; `em_trace` content fixture;
      **`max_pool` cap fixture** (a near-zero-Q acceptance clamps the next
      pool to `max_pool` and warns once; design D7); **`stop_count > 1`
      fall-through fixture** (a non-terminal stop-hit still accepts and
      advances θ; design D7); control-dispatch test (no variant branching in
      the loop). Run to confirm RED; commit the tests alone.
- [ ] 5.1b Implement: the ABEM loop per design D7, written purely against the three
      contracts: accept/grow/stop from `compute_q()`/`compute_ase()`, bounded
      ⌈pool/`max_retries`⌉ within-iteration growth, **post-acceptance sizing
      clamped to `max_pool` with a one-time cap-bound warning** (design D7), a
      **non-terminal stop-hit falling through to the accept test on the same
      proposal** (θ keeps moving; design D7), **`cli_abort()` on
      within-iteration retry exhaustion, carrying the accumulated `em_trace` as
      structured condition data** (design D7) but **`max_iterations` exhaustion
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
      Re-run the 5.1a tests to confirm GREEN; commit.
- [ ] 5.2a Write failing tests for the `estimate_dynes()` surface:
      **explicit-`n_cores`-without-mirai warns-once-then-serial** fixture
      (design D10/Q6); **cross-worker-count reproducibility exercising a
      growth retry (and an ESS-guard redraw)** — identical drawn pool
      (appended + redrawn sequences included) and estimates across
      `n_cores ∈ {1, 2, ...}` at fixed `seed` — Option B keying plus
      index-ordered reduction, monotonic per-iteration `draw_index` (design
      D10); **warm-start-fit-failure fallback** (relocated from 1.2a): with
      `warm_start = TRUE` a fid whose internal `set_algorithm_newton()` fit is
      singular on the single drawn sequence falls back to zero over that fid's
      free set with a cli warning naming the fid, the other fids keep their
      fitted starts, and estimation proceeds (spec scenario "warm-start fit
      failure falls back to zero for that fid"). Run to confirm RED; commit the
      tests alone.
- [ ] 5.2b Implement: `estimate_dynes()` surface — **implemented and tested via the stub
      augmenter but NOT exported here** (no `@export`, absent from `NAMESPACE`;
      exercised through `goldfish:::estimate_dynes()`); the `@export` lands with
      `dynes-augmentation` once a real augmenter exists (design D2/Q1). The four
      control constructors do export here. lifecycle experimental badge, θ₀ from
      `set_algorithm_em()`'s `initial_parameters` (zero default) or the
      `warm_start` augmenter option (per-fid internal
      default-`set_algorithm_newton()` fits on one drawn augmentation, drawn at
      the D7 `iteration = 0` RNG key; a per-fid fit that fails on the single
      drawn sequence falls back to zero over that fid's free set with a cli
      warning naming the fid, never aborting — `tryCatch` mirroring D-ROBUST,
      relocated from 1.2b), the mirai-backed
      parallel seam behind the serial default (Suggests dependency, non-nested
      `workers × BLAS threads ≤ cores` budget; an **explicit** `n_cores > 1`
      with mirai not installed warns **once** and runs serial, the default path
      silent — design D10/Q6); the seam seeds one
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
      Re-run the 5.2a tests to confirm GREEN; commit.
- [ ] 5.3a Write failing tests for the result class: **`vcov()` populated**
      from the final full-pool pass — not `NULL` — on the
      `max_iterations`-exhaustion path (design D9/Q3); §2.2-vs-§3.3 SE
      cross-check fixture. Run to confirm RED; commit the tests alone.
- [ ] 5.3b Implement: result class per design D9: Fisher approximation, **flat** `coef()`/
      `vcov()` named by the `joint-parameters` composite labels (offset effects
      padded via the existing `GetFixed()`/`stats::.vcov.aliased()`, **not**
      `add_na_rowcol()`), MC standard errors, the `converged` flag, and
      `em_trace` carried on the object; `print()`/`summary()` via cli semantic
      elements grouping the flat coefficients per process via `coef_layout()`,
      asymptotic and MC error side by side plus the `converged` status,
      pinned-context snapshots. **The class carries the shared parent
      `goldfishBaseFit` and records its verdicts in the `fit-class-hierarchy`
      capability spec — one per generic, written before the class is, since
      it is an argument to be made rather than a fixture to fill. The
      `logLik` verdict is the one to decide rather than copy: a Monte-Carlo
      estimate reaching `AIC()`/`BIC()` through inheritance is the hazard the
      contract exists for (design D9, ADR-0052).** Re-run the 5.3a tests to
      confirm GREEN; commit.
- [ ] 5.3c Write the `fit-class-hierarchy` spec delta this change owes (added
      2026-09-18). The obligation predates today — design D9 has always said the
      verdicts are recorded in that capability's spec — but this change carries no
      `specs/fit-class-hierarchy/spec.md`, and `process-simulation` has since
      added a **`simulate`** row to the generic list, so the DyNES result class
      now owes one more verdict than when the design was written. Record
      `override` for `coef`, `vcov`, `coef_layout` and `simulate` (design D9: the
      result holds its own joint spec and panel waves, so the parent's
      data-demanding rebuild is wrong for it), the decided `logLik` verdict, and
      `inherit` for the rest. **Ordering:** the delta is a `## MODIFIED` block
      over "Every generic-by-class combination carries a recorded verdict", which
      `process-simulation` and `printing-homogenization` also modify — write it
      against whichever of those has already folded into `openspec/specs/`, and
      run `bash .plan/opsx-spec-placement-check.sh abmcem` before syncing.
      Design-time obligation, no implementation to drive red-then-green, so not a
      pair.
- [ ] 5.4 Final verification: full `NOT_CRAN=true` suite (frozen baselines
      PASS not SKIP); `devtools::document()` if this group touched
      roxygen/exports; commit. **No DESCRIPTION version bump, no root
      `NEWS.md` edit, and no `/opsx:archive` on this branch** — record the
      ABMCEM milestone as a bullet in the change-local
      `openspec/changes/abmcem/NEWS.md`. The version bump, root-NEWS fold, and
      archival happen once at branch merge (see progress.md "Version / NEWS /
      archival")
