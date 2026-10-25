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

The spec deltas name `"fisher_scoring"` with `variant = "full"` (candidate
B-full) as the default, as decided in task 1.3 (design D1 amendment). Both
variants stay selectable under every schedule.

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
- [x] 1.2 Run the bake-off (`.plan/sgd-scale-aware/bakeoff.R`, no package
      code): candidates A (adagrad 0.01), B-mini, B-full, and the references
      (constant 0.001, adam 0.01), on the toy, the oracle, the B3 cells (f33
      coupled and uncoupled, `random` and `mcmc`, first passes) and the B4
      replicates (`verify_sgd.R`'s f02 aborts). Put B's stepper and D2's
      statistic in the script by wrapping `optimize_abem_sgd()`, as
      `instrument_optimizer()` does, not in `R/`. Record per candidate and
      fixture: the four D1 criteria, M-step iterations per pass, the D2
      statistic on the last accepted pass, and the fit's exact `newton_se`.
      Table the results in `progress.md`.
- [x] 1.3 Decide. *Decided 2026-10-16 (explore session):* B-full is the
      default (`fisher_scoring`, `step_size` 1, `variant = "full"` for every
      schedule, both variants selectable under every schedule); `sgd_tolerance`
      0.05 for the M-step; the fit's `at_optimum` gate fixed at 0.1; the
      test before the step (D2); the information, the first score and the
      converged proposal's evaluation served from harvests the loop already
      holds (D3); `mstep_start_newton_se` in the trace (D4). Recorded as
      amendments under design D1/D2/D3/D4/D5, with the spec deltas and
      groups 2-4 amended to match; the gate-threshold option and the B-mini
      revisit carried to `dynes-augmenter-driver-v2`. *Amended 2026-10-23
      (design review):* the schedule is named `"fisher_scoring"`; a
      shrink-only step halving on a rising `d_t` under `"fisher_scoring"` with
      the full variant (`mstep_damped` in the trace), backtracking carried to
      `dynes-augmenter-driver-v2`; `max_iterations` resolved per schedule (20
      under `"fisher_scoring"`, 100 otherwise); the reused harvest includes
      `degenerate`; a resampling fit's `at_optimum` alert names the
      importance-weighted pool; D2 states the `stop_count > 1` stop-hit.
      Validated, and the artifacts committed (`docs:`; not `.gitignore`).
- [x] 1.4 Close the group: NOT_CRAN=true full suite at the group-2 starting
      HEAD (baselines PASS, counts as in 1.1). No package code changed, so
      there is nothing to document. Commit only if the artifacts moved. *Closed
      2026-10-23 at `b7d12441`:* PASS 16704 / FAIL 0 / SKIP 5, baselines
      PASS, as in 1.1.

## 2. The stop rule in standard-error units (D2, D3)

- [x] 2.1 Cost guard (D3): wrap the injected closures of
      `optimize_abem_sgd()` and `evaluate_sequence_pool()` in counters on the
      analytic stub (`helper-abem_loop.R`) and on one `estimate_dynes()` run
      of the two-wave fixture (`abem_two_wave_data()`), counting sequence
      evaluations per pass outside pool entry. Assert that each M-step calls
      the information closure exactly once, at its starting theta, and that
      answering it evaluates no sequence; that every minibatch batch call is
      score-only; that under `variant = "full"` a converged M-step of n
      iterations costs (n - 1) x K for the whole pass, `at_proposal`
      included (0 when it passes at its start), and a capped one
      (cap - 1) x K + K; and that under `"minibatch"` the pass costs its
      batches plus one `at_proposal` of K. Record it FAILING on the unfixed
      code (no information closure exists), and commit it alone. It turns
      GREEN in stages: the information-call count at 2.2b, the
      no-evaluation information at 2.2d, the proposal reuse at 2.3d.
- [x] 2.2a Failing test: `optimize_abem_sgd(information_fn = )` on the
      analytic stub with known `P`. The returned `newton_se` equals
      `max |P^-1 g| / sqrt(diag(P^-1))` at the final iterate under
      `variant = "full"`, and the returned `start_newton_se` equals it at the
      starting theta. The result carries `iterations`, `converged`,
      `start_newton_se`, `newton_se` and `rank_deficient`. With a `fixed`
      mask, the step and the
      statistic equal those of the free block `P[free, free]` alone (D3).
      The first full-variant gradient, taken from the information call's
      `scores`, equals a fresh `score_fn` gradient at the starting theta under
      importance weights and under a resampled multiset with repeated
      indices; `variant = "minibatch"` ignores the cached scores. Confirm RED
      and commit the test alone.
- [x] 2.2b Add the required `information_fn` argument, the once-per-M-step
      call and Cholesky factorization, and the statistic. Build
      `information_fn` in `run_abem_loop()` from
      `evaluate_sequence_pool(what = c("score", "fisher"))`, returning
      `list(information, scores)`: the information weighted as `score_fn`
      weights (importance sum, multiset mean under resampling, degenerate
      sequences excluded), and the pool's `K x p` scores with non-finite
      entries set to zero. Build the first full-variant gradient from the
      cached scores (`colSums(w * scores)`, or `colMeans(scores[idx, ])` under
      resampling) and leave the minibatch path unchanged. Update the existing
      optimizer tests' calls to pass the stub's information and scores.
      This pair builds the closure from a fresh evaluation; 2.2d serves it
      from the held harvest. Confirm GREEN for 2.2a and 2.1's
      information-call count, and commit.
- [x] 2.2c Failing test (D3, the information reuse): on a two-wave
      `estimate_dynes()` run forced through a rejected pass whose retry grows
      the pool, an accepted pass, and an ESS-guard redraw, every information
      request evaluates no sequence, and its `information` and `scores` are
      bit-identical (`expect_identical()`) to a fresh
      `evaluate_sequence_pool()` of the whole pool at that M-step's starting
      theta, under importance and under resampling; the held harvest is
      compared whole, `degenerate` flags included. Confirm RED, and commit
      alone.
- [x] 2.2d In `run_abem_loop()`, hold the per-sequence score and Fisher at
      `theta_k` beside `run$loglik`, maintained at the same places:
      `draw_into_pool()` keeps the whole harvest of its entry evaluation
      (appended on top-up and growth, replaced on a fresh build or redraw),
      and acceptance takes `at_proposal`'s. Serve `information_fn` from it
      with the current weights. Confirm GREEN for 2.2c and 2.1's
      no-evaluation part, and commit.
- [x] 2.3a Failing tests: the stop rule tests the statistic against
      `sgd_tolerance` in SE units, for every schedule. Include the
      scale-invariance scenario (score and information times 1000: same
      flag, `newton_se` within stub noise), the B5 scenario (minibatch
      EMA at the optimum: converged within `max_iterations`), and the
      test-before-step scenario (a start whose statistic is already below
      `sgd_tolerance` returns that theta unchanged, converged, after one
      iteration). Confirm RED (today's raw-norm test, which steps before it
      tests, fails all three), and commit alone.
- [x] 2.3b Replace the gradient-norm test with the statistic, tested at the
      iterate before the step (D2). Keep `convergence = "iterations"`
      untested but recording the statistic; `sgd_tolerance` is ignored in
      that mode and supplying it does not warn (D2). Confirm GREEN, and
      commit.
- [x] 2.3c Failing test (D3, the proposal reuse): under `variant = "full"`,
      a pass whose M-step converges after stepping, and one whose M-step
      passes at its start, make no pool evaluation at the proposal, and the
      pass's whole evaluation at the proposal (log-likelihoods, scores,
      information and `degenerate` flags, compared with `expect_identical()`)
      is bit-identical to a fresh evaluation there; under importance and under
      resampling (one evaluation per member, never per duplicate); a capped
      M-step evaluates the pool at the proposal once. The `em_trace` and the
      returned fit are identical to a run with the reuse disabled. Confirm
      RED, and commit alone.
- [x] 2.3d Evaluate the whole pool once per full-variant iteration after the
      first (the multiset gradient a row lookup), keep the last whole-pool
      harvest with its theta in the loop's evaluator, and take `at_proposal`
      from it, or from the held harvest at `theta_k`, when the returned
      theta is `identical()` to it; otherwise evaluate as today. Confirm
      GREEN for 2.3c and the rest of 2.1, and commit.
- [x] 2.4a Failing tests for `set_sgd_options()` (D5): `step_size = NULL`
      resolves per schedule (`constant`/`decay`/`momentum` 0.001,
      `adagrad`/`adam` 0.01), and `max_iterations = NULL` resolves to 100
      under each of them (`fisher_scoring`'s 20 joins at 3.1). An explicit
      `step_size` or `max_iterations` is kept verbatim. `sgd_tolerance`
      defaults to 0.05. Selecting `"constant"`, `"adagrad"` or `"adam"` works
      with `step_size` and `max_iterations` unset, and each ascends the stub.
      Confirm RED, and commit alone.
- [x] 2.4b Implement the resolution in `set_sgd_options()` itself (the
      schedule is known there, and the spec scenario reads the resolved
      `step_size` and `max_iterations` off its control; `batch_size` resolves
      in `set_algorithm_em()` only because `n_sequences` lives there), and
      update the roxygen: the resolution table (that the first-order
      schedules usually reach their cap), `sgd_tolerance` in standard-error
      units and ignored under `convergence = "iterations"`, the
      distinct-scale note against `tolerance`. Run
      `devtools::document()`. Re-check `_snaps/algorithm_controls.md` and
      update only what this pair moved. Confirm GREEN, and commit.
- [x] 2.5a Failing test (D3): a stub whose information is singular along one
      direction. The M-step does not move that direction's combination, it
      tests convergence on the rest, and it returns `rank_deficient = TRUE`.
      Confirm RED, and commit alone.
- [x] 2.5b Pseudo-inverse fallback on a failed Cholesky (eigendecomposition,
      relative tolerance `sqrt(.Machine$double.eps)`), with the null space
      excluded from the step and the statistic. Confirm GREEN, and commit.
- [x] 2.6 Close the group: NOT_CRAN=true full suite (baselines PASS),
      `devtools::document()` if roxygen moved since 2.4b, and commit.

## 3. The default step rule (D1, as amended by 1.3)

- [x] 3.1a Failing tests for the `"fisher_scoring"` schedule:
      `step = step_size * P^-1 g` with the held `P`. On the stub under
      `"full"` with `step_size = 1`, it reaches the optimum in one step on a
      quadratic. Its trajectory in SE units is the same for the score and
      information scaled by 1000. Under `"minibatch"` the step uses the
      bias-corrected EMA gradient and ascends the stub. Every schedule x
      variant pair is selectable and ascends the stub. Confirm RED, and
      commit alone.
- [x] 3.1b Add `"fisher_scoring"` to `make_sgd_stepper()` and to
      `set_sgd_options()`'s `step_schedule` choices, with `step_size` 1 and
      `max_iterations` 20 in the resolution table. Update the roxygen (a
      Fisher-scoring step with the information held at the M-step's start,
      not `set_algorithm_newton()`), and run `devtools::document()`. Confirm
      GREEN, and commit.
- [x] 3.1c Failing tests for the damping (D1 2026-10-23 amendment): on a
      stub whose information grows like `exp(theta)`, from a start where the
      undamped full-variant `"fisher_scoring"` step overshoots so that `d_t`
      rises, `step_size` halves at each rise, the iteration continues from
      its current iterate (no backtrack), the M-step converges within the
      cap of 20, and the result reports the count of halvings (`damped`).
      With `step_size` held fixed instead (damping disabled in the test), the
      same start does not converge within the cap. The same objective under
      `variant = "minibatch"` and under every other schedule never halves
      (`damped = 0`). Confirm RED, and commit alone.
- [x] 3.1d Implement the shrink-only halving in `optimize_abem_sgd()` for
      `"fisher_scoring"` under `"full"`, reading `d_t` against the previous
      iteration's, and return `damped`. Confirm GREEN, and commit.
- [x] 3.2a Failing tests for the default: `set_sgd_options()` carries the
      1.3 winner (`fisher_scoring`, `step_size` 1, `variant = "full"`,
      `max_iterations` 20). On the
      stub, the default does not overshoot a large summed
      score (information of 10,000 events, start 1 SE away: converged, never
      more than 1 SE past the optimum), and a start at the optimum stays
      within `sgd_tolerance` SE of it. Confirm RED (today's constant 0.001
      fails the first), and commit alone.
- [x] 3.2b Flip the `step_schedule` and `variant` defaults to the winner.
      The roxygen for `variant` states the trade of `"minibatch"` (cheap steps,
      iterate noise that can cost EM retries until the augmentation accounts for
      the unobserved flips). Run `devtools::document()`, and confirm GREEN.
- [x] 3.3 Rerun every DyNES suite (`test-estimate_dynes.R`,
      `test-em_loop_dynes.R`, `test-optimizer_dynes.R`, `test-fit_dynes.R`,
      the augmenter suites, and the cli snapshots under `_snaps/`). Every
      expectation or snapshot that moved must trace to one of four causes:
      the default schedule (its damping included), the default variant, the
      `max_iterations` default (100 to 20 under `"fisher_scoring"`), or the
      stop rule (its SE units, its 0.05 default and its test-before-step
      order). The harvest
      reuse is bit-identical by 2.2c/2.3c, so a move traced to it is a
      defect, not an attribution. Record each in `progress.md` with its cause, then
      update them in one `test:` commit, separate from 3.2b's `feat:`
      commit, so the default flip and its consequences review apart.
- [ ] 3.4 Close the group: NOT_CRAN=true full suite (baselines PASS),
      `devtools::document()` if needed, and commit.

## 4. Reporting the M-step's verdict (D4)

- [ ] 4.1a Failing test: the `em_trace` of a two-wave fit carries
      `mstep_iterations`, `mstep_converged`, `mstep_start_newton_se`,
      `mstep_newton_se`, `mstep_rank_deficient` and `mstep_damped` with one
      value per pass, and no per-iteration M-step history; on a pass whose
      M-step passes at its start, the two statistics are equal. Confirm RED,
      and commit alone.
- [ ] 4.1b Keep the optimizer's whole result in `run_abem_loop()`, and add
      the six columns to the trace row and to `abem_trace_tibble()`.
      Confirm GREEN, and commit.
- [ ] 4.2a Failing test: with `max_iterations = 1L` over a multi-pass run,
      exactly one `goldfish_abem_mstep_unconverged` warning is raised, every
      capped pass reads `mstep_converged = FALSE`, and the optimizer itself
      warns nothing. Rewrite the existing "hitting the iteration cap ...
      warns" test (`test-optimizer_dynes.R`) to the new contract. Confirm
      RED, and commit alone.
- [ ] 4.2b Remove the optimizer's per-pass warning. Add the loop's
      once-per-run classed cli warning (the pass, the statistic against
      `sgd_tolerance`, the pass's step halvings when non-zero, the levers
      `max_iterations` and `step_schedule`),
      following `warn_pool_capped()`'s shape and `run_state`'s once-flag.
      Pin a cli context in its snapshot. Confirm GREEN, and commit.
- [ ] 4.3a Failing tests (`dynes-estimation`, ADDED requirement): `newton_se`
      equals `max(abs(V %*% S) / sqrt(diag(V)))` within 1e-10 on a fit with a
      closed-form final pool. `at_optimum` is `newton_se < 0.1` whatever `sgd_tolerance` (a fixed
      internal constant). Both are `NA` on `"singular"`/`"not_pd"`/`"pool_dropped"`. `glance()`
      carries both. The pool is evaluated at the estimate no more often than
      before (reuse 2.1's counter). Confirm RED, and commit alone.
- [ ] 4.3b Compute both in `new_dynes_fit()` from `dynes_covariances()`'s
      pass. Add the weighted mean score `S` to `louis_covariances()`'s return
      (it already computes it; `dynes_covariances()` forwards it), rather than
      re-evaluating the pool. Existing callers and the mock in
      `test-fit_dynes.R` pass through the real function, so the extra field is
      safe. `at_optimum` compares with the fixed 0.1 constant in either
      `convergence` mode. Add both to `glance()`. Confirm GREEN, and commit.
- [ ] 4.4a Failing snapshot tests: `print()`/`summary()` state the optimum
      gate beside `converged`. That is a success line when `at_optimum` is
      TRUE, a warning alert naming `newton_se` and the 0.1 threshold when it is
      FALSE, and nothing when it is `NA`. Under `use = "resampling"` the
      alert adds an `i` bullet saying the gate reads the importance-weighted
      pool. Pin a reproducible cli context. Confirm RED, and commit alone.
- [ ] 4.4b Extend `dynes_fit_status()` with cli semantic elements. Confirm
      GREEN, and commit.
- [ ] 4.5 Measure, as the design's cost section requires: on 1.1's toy and
      oracle, fit seconds, peak allocated MB, EM iterations, and M-step
      iterations per pass, against the 1.1 values and the prediction
      (0.7x-1.5x fit time on the oracle, where the reference converges;
      memory unchanged within noise; toy_base and toy_inter ratios recorded,
      not gated: design D1 amendment). Add one grown-pool cell (a pool of
      1,000-5,000 sequences, C4) and record its per-pass sequence-evaluation
      count and time ratio against the cost table in the design (full default
      0.5x-1x at K = 1,000 and 0.8x-1.7x at K = 5,000; minibatch about 0.55x
      and 0.85x), for `variant = "full"` and `"minibatch"`, and its peak
      memory against the design's estimate (about twice `at_proposal`'s
      harvest); it is recorded, not gated. Confirm `walk_cost.csv`
      and `estimation_cost.csv` are unmoved. Record the results in
      `progress.md`.
- [ ] 4.6 Check the success criterion on the default `estimate_dynes()`. On
      the toy, the oracle and the B3/B4 cells (1.2's harness, now without the
      wrapper), check that no run aborts (`goldfish_abem_retry_exhausted`:
      criterion 1 at the shipped 0.05, which the bake-off measured at 0.1
      only), that `at_optimum` agrees with the study's `newton_se` gate, and
      record how often the stop pass's `mstep_start_newton_se <
      sgd_tolerance` agrees with `at_optimum`, over importance-weighted fits
      that ended on the stop rule only (D2, D4: a capped EM has no pass whose
      M-step started at the returned estimate, and under resampling the two
      target different objectives), with the gate-to-flag ratio of each
      disagreement against D2's bound. On the oracle, check |theta_hat - MLE| < 0.01 SE, and on toy_inter that
      `newton_se` is under 0.1 (the D1 amendment's re-reading).
      Record in `progress.md`. A miss stops the group for review.
- [ ] 4.7 Close the group: NOT_CRAN=true full suite (baselines PASS),
      `devtools::document()` if needed, and commit.

## 5. Milestone

- [ ] 5.1 Write the `NEWS.d/sgd-scale-aware-default--mstep-default.md`
      fragment (`NEWS.d/README.md`): the new default schedule
      `"fisher_scoring"` with its step halving, `variant = "full"`, the
      `step_size` and `max_iterations` resolution (both variants selectable
      under every schedule), `sgd_tolerance` now in standard-error units at
      0.05 (an old raw value has no exact equivalent), the fixed 0.1
      `at_optimum` gate, `newton_se`/`at_optimum`
      on the fit and in `glance()`, the `em_trace` M-step columns, the
      classed warning, and that the M-step now reuses the pool evaluations
      the loop already holds (no numeric effect). Do not touch the DESCRIPTION Version or `NEWS.md`.
      Flip `proposal.md` to `status: landed (feature/dynes, awaiting fold)`.
      Commit.
- [ ] 5.2 Run `conformance-inspector` on the change from a fresh session,
      never the applier. Triage its report, and re-check the scoped findings
      before the change is trusted.
- [ ] 5.3 Compare the study's tuned setting (adagrad 0.01, 500 M-step
      iterations, `dynes-augmentation` D25) with the new default on 1.2's
      cells, and record the comparison in `dynes-augmentation/progress.md`
      for that change's 6.2.6 onward. No code. It does not block the fold.
