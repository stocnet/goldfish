> **Landing order.** `depends-on: fixed-loglik-degenerate` (both deltas are
> written against its wording of "Pool evaluation through the zero-iteration
> engine path" and "Fixed-term degeneracy is reported"). Lands on
> `feature/dynes` before `dynes-augmentation` task 6.2; at the merge, archive
> `abmcem`, `fixed-loglik-degenerate`, then this change. No version bump and
> no `NEWS.md` edit on the branch: a `NEWS.d/` fragment instead.
>
> **Reproducer.** `recipe_state_spec()` (`helper-estimate_dynes.R`): its calls
> choice `inertia` is identically zero on every path, so every per-sequence
> information is singular. Today `estimate_dynes(recipe_state_spec(), ...,
> control_prep = set_preprocessing(start_time = 0))` crashes in
> `sample.int()` under `random`/`sim` and aborts with "An augmenter draw must
> report a single finite log_proposal" under `mcmc`.

## 1. A singular sequence keeps its weight (design D1)

- [x] 1.1 (a) Write failing tests: a pooled sequence whose information matrix
      is singular with finite entries is not degenerate and keeps its finite
      log-likelihood, score and Fisher information (rewrite
      `test-evaluator_dynes.R` "a singular sequence yields a non-finite
      result, not a pool abort" and "the cpp evaluator keeps the
      singular-information guard" to the new contract); an engine evaluation
      returning a non-finite log-likelihood is still degenerate and the pool
      completes; the proposal evaluator and `compute_lik_seq()` return a
      finite, non-degenerate density for a singular candidate. Add a
      `skip_on_cran()` end-to-end test on the reproducer that runs
      `estimate_dynes()` under `random`, `sim` and `mcmc` without error.
      Confirm RED and commit the tests alone.
- [x] 1.2 (b) Implement D1: `pool_cpp_eval()` stops inverting the free
      block; keep the non-finite/`NA` checks. Update the comments that state
      the old contract (no OpenSpec references), rewrite `compute_lik_seq()`'s
      roxygen paragraph on degenerate sequences (a singular information no
      longer makes one) and run `devtools::document()`. Point
      `dynes-augmentation/progress.md`'s `solve()` and follow-up notes
      (~lines 1054, 1152-1159) at this change. `air format`, lint, GREEN,
      not-cran-test (baselines PASS, not SKIP), commit
      (`fix(singular-sequence-information): ...`).

## 2. A pool with no weight stops, whatever the cause (design D2)

- [x] 2.1 (a) Write failing tests: with every pooled sequence degenerate for
      a non-fixed-term cause (mock the evaluation to return non-finite at
      theta, and separately a failed walk), `run_abem_loop()` stops before Q
      and before minibatch selection, returns the last accepted parameters
      with `converged = FALSE`, and raises no error, under importance and
      under uniform weighting; `estimate_dynes()` then emits exactly one
      `goldfish_dynes_pool_degenerate` warning (cli snapshot, reproducible
      cli context); the fully fixed-term-degenerate case keeps today's
      single fixed-term warning and emits no pool-degenerate one, under
      importance and under uniform weighting; a pool lost to mixed causes
      (fixed-term and walk) emits one warning of each class, the fixed-term
      one without a stop statement. Uniform weighting: `compute_pool_weights()`
      gives a sequence with a `-Inf` current log-likelihood zero weight and
      splits the rest equally; a uniform E-step with one degenerate sequence
      has finite `compute_q()`/`compute_ase()` over the others (RED: `NaN`
      today); a finite current with a `-Inf` proposal still gives
      `Q = -Inf`. Confirm RED and commit the tests alone.
- [x] 2.2 (b) Implement D2: replace `pool_lost_to_fixed_terms()` at both
      call sites in `run_abem_loop()` with
      `!any(is.finite(run$loglik) & weights > 0)` at `theta_k`, keep the
      `pool_dropped` tally flag, and add the `goldfish_dynes_pool_degenerate`
      warning (cli, emitted once after the loop, only when the fixed-term
      warning does not already report the stop). Uniform weighting: zero
      weight on a non-finite current log-likelihood, equal weight on the
      rest, and the uniform `compute_q()`/`compute_ase()` methods average
      over the positive-weight sequences (key on the current log-likelihood,
      not on `lambda`). `air format`, lint, GREEN, not-cran-test, commit.

## 3. A singular pooled information is reported (design D3)

- [x] 3.1 (a) Write failing tests: on the reproducer, the fit returns with
      `NA` covariances and exactly one `goldfish_dynes_singular_information`
      warning naming both zero-information coefficients by their composite
      labels, the calls choice `inertia` and the friendship dissolution
      choice `indeg` (every dissolution in `recipe_state_data()` is a forced
      choice), and saying they stay at their starting values (cli snapshot), with
      `fit$parameters` unchanged; a fixture whose per-sequence information is
      singular on some paths but whose pooled information is invertible
      emits no such warning and has finite covariances (candidate: a sibling
      of `recipe_state_spec()` whose calls choice uses `indeg(friendship)`,
      a path-dependent statistic on the augmented layer; confirm at RED that
      some drawn paths are singular, else pick another; do not edit
      `recipe_state_spec()`, which is the reproducer); the `em_trace`
      standard errors do not warn; a run stopped on a degenerate pool emits
      no singular-information warning. Unit-test on synthetic inputs:
      `singular_attribution()` on a collinear information with no zero
      diagonal names nothing, on an all-zero diagonal names every free
      coefficient, and on a diagonal that is a tiny positive rounding
      residue (a statistic constant but nonzero within alternatives) names
      nothing; the renderer states the starting-value wording for a
      coefficient whose returned value equals its start and the
      "no information in the final pool, value not an estimate" wording,
      with the value, for one that moved. Confirm RED and commit the tests
      alone.
- [x] 3.2 (b) Implement D3: `singular_attribution(information)` returns the
      free coefficients with `I_jj <= 0` (no relative tolerance) and an empty
      group list; a DyNES renderer takes the attribution, the composite
      labels and the starting and returned values and warns once. Call it in
      `new_dynes_fit()` after the covariance pass at the returned estimate,
      before the pool store is closed, when `sandwich_covariances()` cannot
      invert the pooled information, unless the loop stopped on a degenerate
      pool (`pool_dropped`). `air format`, lint, GREEN, not-cran-test,
      commit.

## 4. Close

- [x] 4.1 Write `NEWS.d/singular-sequence-information--singular-sequences.md`
      per `NEWS.d/README.md`, bullets under 81 characters each:
      `estimate_dynes()` keeps augmented sequences whose own information is
      singular (they were dropped, biasing the E-step, and crashed a run
      when all were); `compute_lik_seq()` returns a finite, non-degenerate
      result for a singular sequence; a pool with no finite likelihood stops
      with `converged = FALSE` and a warning; uniform weighting no longer
      turns Q into `NaN` on a degenerate sequence; a singular pooled
      information warns, naming the coefficients with no information.
      Commit it.
- [x] 4.2 Close the change: full not-cran-test run in the background
      confirming the frozen 1e-6 baselines PASS (not SKIP). Add
      `status: landed (feature/dynes, awaiting fold)` to `proposal.md`, and
      commit. Record in `dynes-augmentation`'s `tasks.md` and `progress.md` that
      its 6.2 gate is met. Close with close-task.
