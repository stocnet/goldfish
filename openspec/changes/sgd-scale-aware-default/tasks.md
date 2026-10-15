Every task is verified with the `not-cran-test` skill before its commit (the
frozen 1e-6 baselines report PASS, not SKIP), closed with `close-task`, and
follows `r-lib:r-package-development` / `r-lib:testing-r-packages`. New
console output follows `r-lib:cli`. Touched R files are `air format`-ed and
then linted. This is a feature branch (`feature/dynes`), so the milestone
writes a `NEWS.d/` fragment and bumps no Version.

**Gate.** Group 1 may start at once (no package code). Group 2 starts only
after `focal-stream-dedup` 3.2 is triaged, so every seeded expectation that
moves here has this change as its one cause. *Met 2026-10-15:*
`focal-stream-dedup` landed 16/16 (3.2 and the group 4 re-check CONFORMANT);
group 2 onward is unblocked, and its baseline is that change's suite
(PASS 16704 / FAIL 0 / SKIP 5).

The spec deltas name `"preconditioned"` (candidate B) as the default. If
task 1.2 picks A, task 1.3 amends the deltas, design D1/D5 and group 3 before
group 2 begins.

## 1. Bake-off: choose the default on evidence (D1, D2)

- [x] 1.1 Record the "before" values with no package code changed, at the
      HEAD where group 2 will start. On the abmcem toy
      (`.plan/dynes/toy/toy_lib.R`) and the count-panel oracle
      (`.plan/dynes/recovery/lib.R`, seed 3), under today's defaults: fit
      seconds, peak allocated MB (`bench::mark(memory = TRUE)` or
      `profmem`), EM iterations, `converged`, per pass the M-step iterations
      and gradient norm (`instrument_optimizer()`), and D25's `newton_se`.
      On the oracle, also |theta_hat - MLE| / oracle SE per coordinate. Note
      the NOT_CRAN=true suite counts at that HEAD. Put the values in
      `progress.md`, and keep the script beside them
      (`.plan/sgd-scale-aware/before.R`).
- [ ] 1.2 Run the bake-off (`.plan/sgd-scale-aware/bakeoff.R`, no package
      code): candidates A (adagrad 0.01), B-mini, B-full, and the references
      (constant 0.001, adam 0.01), on the toy, the oracle, the B3 cells (f33
      coupled and uncoupled, `random` and `mcmc`, first passes) and the B4
      replicates (`verify_sgd.R`'s f02 aborts). Put B's stepper and D2's
      statistic in the script by wrapping `optimize_abem_sgd()`, as
      `instrument_optimizer()` does, not in `R/`. Record per candidate and
      fixture: the four D1 criteria, M-step iterations per pass, the D2
      statistic on the last accepted pass, and the fit's exact `newton_se`.
      Table the results in `progress.md`.
- [ ] 1.3 Decide. Record the winner, its `step_size`, and the
      `sgd_tolerance` default (0.1 unless the M-step flag and the fit gate
      disagree on more than 10% of the bake-off fits) as an amendment under
      design D1/D2. If A wins: amend both spec deltas (default schedule,
      resolved `step_size`, the default-overshoot scenario) and design D5,
      and replace group 3's 3.1 pair with the default flip alone. If no
      candidate survives, stop for a design review. Run
      `openspec validate sgd-scale-aware-default`, and commit the artifacts
      (`docs:`).
- [ ] 1.4 Close the group: NOT_CRAN=true full suite at the group-2 starting
      HEAD (baselines PASS, counts as in 1.1). No package code changed, so
      there is nothing to document. Commit only if the artifacts moved.

## 2. The stop rule in standard-error units (D2, D3)

- [ ] 2.1 Cost guard (D3): wrap the injected closures of
      `optimize_abem_sgd()` in counters on the analytic stub
      (`helper-abem_loop.R`) and on one `estimate_dynes()` run of the
      two-wave fixture (`abem_two_wave_data()`). Assert that each M-step
      calls the information closure exactly once, at its starting theta,
      that every batch call is score-only, and that the number of full-pool
      evaluations per pass is the 1.1 value plus one. Record it FAILING on
      the unfixed code (no information closure exists), and commit it alone.
- [ ] 2.2a Failing test: `optimize_abem_sgd(information_fn = )` on the
      analytic stub with known `P`. The returned `newton_se` equals
      `max |P^-1 g| / sqrt(diag(P^-1))` at the final iterate under
      `variant = "full"`. The result carries `iterations`, `converged`,
      `newton_se` and `rank_deficient`. With a `fixed` mask, the step and the
      statistic equal those of the free block `P[free, free]` alone (D3).
      Confirm RED and commit the test alone.
- [ ] 2.2b Add the required `information_fn` argument, the once-per-M-step
      call and Cholesky factorization, and the statistic. Build
      `information_fn` in `run_abem_loop()` from
      `evaluate_sequence_pool(what = "fisher")`, weighted as `score_fn`
      (importance sum, multiset mean under resampling, degenerate sequences
      excluded). Update the existing optimizer tests' calls to pass the
      stub's information. Confirm GREEN for 2.2a and 2.1, and commit.
- [ ] 2.3a Failing tests: the stop rule tests the statistic against
      `sgd_tolerance` in SE units, for every schedule. Include the
      scale-invariance scenario (score and information times 1000: same
      flag, `newton_se` within stub noise) and the B5 scenario (minibatch
      EMA at the optimum: converged within `max_iterations`). Confirm RED
      (today's raw-norm test fails both), and commit alone.
- [ ] 2.3b Replace the gradient-norm test with the statistic. Keep
      `convergence = "iterations"` untested but recording the statistic.
      `sgd_tolerance` stays in the control under that mode and supplying it
      does not warn (D2). Confirm GREEN, and commit.
- [ ] 2.4a Failing tests for `set_sgd_options()` (D5): `step_size = NULL`
      resolves per schedule (`constant`/`decay`/`momentum` 0.001,
      `adagrad`/`adam` 0.01). An explicit `step_size` is kept verbatim.
      `sgd_tolerance` defaults to 0.1. Selecting `"constant"`, `"adagrad"` or
      `"adam"` works with `step_size` unset, and each ascends the stub.
      Confirm RED, and commit alone.
- [ ] 2.4b Implement the resolution in `set_sgd_options()` itself (the
      schedule is known there, and the spec scenario reads the resolved
      `step_size` off its control; `batch_size` resolves in
      `set_algorithm_em()` only because `n_sequences` lives there), and update
      the roxygen: the resolution table, `sgd_tolerance` in standard-error
      units, the distinct-scale note against `tolerance`. Run
      `devtools::document()`. Re-check `_snaps/algorithm_controls.md` and
      update only what this pair moved. Confirm GREEN, and commit.
- [ ] 2.5a Failing test (D3): a stub whose information is singular along one
      direction. The M-step does not move that direction's combination, it
      tests convergence on the rest, and it returns `rank_deficient = TRUE`.
      Confirm RED, and commit alone.
- [ ] 2.5b Pseudo-inverse fallback on a failed Cholesky (eigendecomposition,
      relative tolerance `sqrt(.Machine$double.eps)`), with the null space
      excluded from the step and the statistic. Confirm GREEN, and commit.
- [ ] 2.6 Close the group: NOT_CRAN=true full suite (baselines PASS),
      `devtools::document()` if roxygen moved since 2.4b, and commit.

## 3. The default step rule (D1, as amended by 1.3)

- [ ] 3.1a Failing tests for the `"preconditioned"` schedule:
      `step = step_size * P^-1 g` with the held `P`. On the stub under
      `"full"` with `step_size = 1`, it reaches the optimum in one step on a
      quadratic. Its trajectory in SE units is the same for the score and
      information scaled by 1000. Confirm RED, and commit alone. (Dropped by
      1.3 if A wins.)
- [ ] 3.1b Add `"preconditioned"` to `make_sgd_stepper()` and to
      `set_sgd_options()`'s `step_schedule` choices, with `step_size` 1 in
      the resolution table. Update the roxygen, and run
      `devtools::document()`. Confirm GREEN, and commit.
- [ ] 3.2a Failing tests for the default: `set_sgd_options()` carries the
      1.3 winner. On the stub, the default does not overshoot a large summed
      score (information of 10,000 events, start 1 SE away: converged, never
      more than 1 SE past the optimum), and a start at the optimum stays
      within `sgd_tolerance` SE of it. Confirm RED (today's constant 0.001
      fails the first), and commit alone.
- [ ] 3.2b Flip the `step_schedule` default to the winner. Run
      `devtools::document()`, and confirm GREEN.
- [ ] 3.3 Rerun every DyNES suite (`test-estimate_dynes.R`,
      `test-em_loop_dynes.R`, `test-optimizer_dynes.R`, `test-fit_dynes.R`,
      the augmenter suites, and the cli snapshots under `_snaps/`). Every
      expectation or snapshot that moved must trace to the new default or
      the new stop rule. Record each in `progress.md` with its cause, then
      update them in one `test:` commit, separate from 3.2b's `feat:`
      commit, so the default flip and its consequences review apart.
- [ ] 3.4 Close the group: NOT_CRAN=true full suite (baselines PASS),
      `devtools::document()` if needed, and commit.

## 4. Reporting the M-step's verdict (D4)

- [ ] 4.1a Failing test: the `em_trace` of a two-wave fit carries
      `mstep_iterations`, `mstep_converged`, `mstep_newton_se` and
      `mstep_rank_deficient` with one value per pass, and no per-iteration
      M-step history. Confirm RED, and commit alone.
- [ ] 4.1b Keep the optimizer's whole result in `run_abem_loop()`, and add
      the four columns to the trace row and to `abem_trace_tibble()`.
      Confirm GREEN, and commit.
- [ ] 4.2a Failing test: with `max_iterations = 1L` over a multi-pass run,
      exactly one `goldfish_abem_mstep_unconverged` warning is raised, every
      capped pass reads `mstep_converged = FALSE`, and the optimizer itself
      warns nothing. Rewrite the existing "hitting the iteration cap ...
      warns" test (`test-optimizer_dynes.R`) to the new contract. Confirm
      RED, and commit alone.
- [ ] 4.2b Remove the optimizer's per-pass warning. Add the loop's
      once-per-run classed cli warning (the pass, the statistic against
      `sgd_tolerance`, the levers `max_iterations` and `step_schedule`),
      following `warn_pool_capped()`'s shape and `run_state`'s once-flag.
      Pin a cli context in its snapshot. Confirm GREEN, and commit.
- [ ] 4.3a Failing tests (`dynes-estimation`, ADDED requirement): `newton_se`
      equals `max(abs(V %*% S) / sqrt(diag(V)))` within 1e-10 on a fit with a
      closed-form final pool. `at_optimum` is `newton_se < sgd_tolerance`.
      Both are `NA` on `"singular"`/`"not_pd"`/`"pool_dropped"`. `glance()`
      carries both. The pool is evaluated at the estimate no more often than
      before (reuse 2.1's counter). Confirm RED, and commit alone.
- [ ] 4.3b Compute both in `new_dynes_fit()` from `dynes_covariances()`'s
      pass. Add the weighted mean score `S` to `louis_covariances()`'s return
      (it already computes it; `dynes_covariances()` forwards it), rather than
      re-evaluating the pool. Existing callers and the mock in
      `test-fit_dynes.R` pass through the real function, so the extra field is
      safe. `at_optimum` reads the control's `sgd_tolerance` in either
      `convergence` mode. Add both to `glance()`. Confirm GREEN, and commit.
- [ ] 4.4a Failing snapshot tests: `print()`/`summary()` state the optimum
      gate beside `converged`. That is a success line when `at_optimum` is
      TRUE, a warning alert naming `newton_se` and `sgd_tolerance` when it is
      FALSE, and nothing when it is `NA`. Pin a reproducible cli context.
      Confirm RED, and commit alone.
- [ ] 4.4b Extend `dynes_fit_status()` with cli semantic elements. Confirm
      GREEN, and commit.
- [ ] 4.5 Measure, as the design's cost section requires: on 1.1's toy and
      oracle, fit seconds, peak allocated MB, EM iterations, and M-step
      iterations per pass, against the 1.1 values and the prediction
      (0.7x-1.5x fit time, memory unchanged within noise). Add one grown-pool
      cell (a pool of 1,000-5,000 sequences, C4) and record its per-pass time
      ratio against the cost table in the design (about 1x at K = 1,000 and
      1.7x at K = 5,000); it is recorded, not gated. Confirm `walk_cost.csv`
      and `estimation_cost.csv` are unmoved. Record the results in
      `progress.md`.
- [ ] 4.6 Check the success criterion on the default `estimate_dynes()`. On
      the toy, the oracle and the B3/B4 cells (1.2's harness, now without the
      wrapper), check that `at_optimum` agrees with the study's `newton_se`
      gate, and record how often the last accepted pass's `mstep_converged`
      agrees with it, over importance-weighted fits only (D4: under
      resampling the two target different objectives). On the oracle, check |theta_hat - MLE| < 0.01 SE.
      Record in `progress.md`. A miss stops the group for review.
- [ ] 4.7 Close the group: NOT_CRAN=true full suite (baselines PASS),
      `devtools::document()` if needed, and commit.

## 5. Milestone

- [ ] 5.1 Write the `NEWS.d/sgd-scale-aware-default--mstep-default.md`
      fragment (`NEWS.d/README.md`): the new default schedule and
      `step_size` resolution, `sgd_tolerance` now in standard-error units
      (an old raw value has no exact equivalent), `newton_se`/`at_optimum`
      on the fit and in `glance()`, the `em_trace` M-step columns, and the
      classed warning. Do not touch the DESCRIPTION Version or `NEWS.md`.
      Flip `proposal.md` to `status: landed (feature/dynes, awaiting fold)`.
      Commit.
- [ ] 5.2 Run `conformance-inspector` on the change from a fresh session,
      never the applier. Triage its report, and re-check the scoped findings
      before the change is trusted.
- [ ] 5.3 Compare the study's tuned setting (adagrad 0.01, 500 M-step
      iterations, `dynes-augmentation` D25) with the new default on 1.2's
      cells, and record the comparison in `dynes-augmentation/progress.md`
      for that change's 6.2.6 onward. No code. It does not block the fold.
