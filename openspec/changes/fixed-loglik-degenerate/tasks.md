> **TDD.** 1.1/1.2, 2.1/2.2 and 3.1/3.2 are red/green pairs (per
> `openspec/config.yaml` `rules.tasks`): the first of each commits the
> failing tests alone, the second the change that turns them green.
>
> **Branch.** This runs on `feature/dynes`, so no `DESCRIPTION` Version bump
> and no `NEWS.md` edit: task 4.1 writes a `NEWS.d/` fragment instead, and the
> change is flipped to `status: landed (feature/dynes, awaiting fold)` when
> done, not archived. `depends-on: abmcem`, so it folds after `abmcem` at the
> merge; until then `opsx-spec-placement-check.sh` is expected to fail on its
> `## MODIFIED` header.
>
> **Gate.** Group 1 alone unblocks `dynes-augmentation` task 6.1; groups 2
> and 3 do not gate it. Group 3 needs group 2 (it reads `degenerate_cause`
> and adds the warning's extra bullet).

## 1. A non-finite fixed-only term is degenerate

- [x] 1.1 (a) Write failing tests in `tests/testthat/test-evaluator_dynes.R`,
      beside "a fixed-only fid adds a log-likelihood, no score or Fisher
      block", following r-lib:testing-r-packages. Build the pin with a `−Inf`
      first plateau: `make_intercept_only_rate(c(-Inf, -2), model_type =
      "DyNAM-M-Rate", wave_times = c(0, 10, 20))`. Period 1 holds the events at
      1, 3, 6, so the term is `−Inf` and the engine does not error. Add a
      `fixed_only_*` fixture variant that takes the pin as an argument; keep
      the existing fixtures' behavior unchanged.
      (i) `pool_fixed_loglik()` returns `loglik = -Inf`, `degenerate = TRUE`.
      (ii) Through `fixed_only_pool()` and `evaluate_sequence_pool()` at
      `theta = c(0.3, -0.2)`: `degenerate[[1]]` is `TRUE`, `loglik[[1]]` is
      `-Inf`, and score and Fisher are all `NA`.
      (iii) `+Inf`, `NaN` and `NA` terms: a pin cannot carry them
      (`make_intercept_only_rate()` rejects `+Inf`/`NA`), so mock
      `fixed_only_loglik()` with `testthat::local_mocked_bindings()` to return
      each of `Inf`, `NaN`, `NA_real_` in turn, over the existing finite
      fixture. Each gives `degenerate = TRUE` with `loglik = -Inf` exactly
      (not `NaN`/`NA`/`Inf`).
      (iv) Through `evaluate_candidate()` on the same decomposition and data:
      no error, `degenerate` is `TRUE`, `loglik` is `-Inf`, score and Fisher
      all `NA`.
      (v) Regression guard: the existing two-plateau pin `c(-1, -2)` stays
      `degenerate = FALSE` with its hand-computed term (already covered by
      the neighboring test; assert only the flag here).
      (vi) Regression guard against over-flagging: the pin `c(-2, -Inf)`
      (same `wave_times`; period 2 holds no events, the intended use of a
      zero-count pin) gives a finite term, `degenerate = FALSE`, through
      `pool_fixed_loglik()` and `evaluate_sequence_pool()` alike.
      Run the file once to confirm (i)–(iv) are RED and (v)–(vi) are GREEN;
      commit the tests alone (`test(fixed-loglik-degenerate): ...`).
- [x] 1.2 (b) In `R/evaluator_dynes.R`, `pool_fixed_loglik()`: treat a
      non-finite term like the error path, returning `loglik = -Inf`,
      `degenerate = TRUE` as soon as one is found (design D1, D2). Update the
      function's comment to say that a non-finite term, not only an engine
      failure, marks the sequence degenerate. Add the case to
      `compute_lik_seq()`'s `@details` list of degenerate causes ("a
      fixed-only term, such as a pinned rate, has a non-finite
      log-likelihood"), and update the `evaluate_candidate()` comment's list
      of causes likewise. Do not reference the change or design IDs in the
      comments. Run `devtools::document()` and check the `compute_lik_seq`
      man page. `air format` the file, then lint it. Re-run the 1.1 tests to
      GREEN, verify with not-cran-test (baselines PASS, not SKIP), and commit
      (`fix(fixed-loglik-degenerate): ...`).

## 2. Fixed-term degeneracy is reported

- [x] 2.1 (a) Write failing tests, following r-lib:testing-r-packages:
      (i) In `test-evaluator_dynes.R`: `new_pool_entry()` records
      `degenerate_cause = "fixed_term"` and `degenerate_label` equal to the
      pinned sub-model's `label` for the `−Inf` pin fixture; `"walk"` with
      `NA` label for `stats = NULL`; `NA`/`NA` for the finite fixture.
      (ii) The pool store's tally after `new_sequence_pool()` then
      `append_sequence_pool()` with the `−Inf` pin counts every built entry
      and every fixed-term entry across both calls, and collects the failing
      label once; a walk-failure entry counts as built but not as
      fixed-term.
      (iii) The warning helper (design D4), called on a store whose tally
      has a positive fixed-term count: one warning of class
      `goldfish_dynes_fixed_term_degenerate`, with a cli snapshot of its
      message under a pinned cli context (`local_reproducible_output()`)
      showing "{n} of {N} drawn sequences", the label, and the bug-report
      line; a zero count warns nothing.
      (iv) One integration run: no natural fixture produces the case end to
      end (the augmenter samples under the same pin, so drawn events never
      fall in a `−Inf` period), so run the smallest `estimate_dynes()`
      fixture on the serial seam with
      `testthat::local_mocked_bindings(fixed_only_loglik = ...)` returning
      `-Inf` for **some** sequences (keyed on the sequence, so the pool is
      not fully dropped and group 3's early stop does not trip). Assert
      `expect_warning(class = "goldfish_dynes_fixed_term_degenerate")`
      exactly once, and an integer `em_trace$fixed_degenerate` column,
      positive on its passes. Mocks do not reach mirai workers, hence serial.
      (v) A run with finite pins emits no such warning and has
      `fixed_degenerate` all zero.
      Confirm RED and commit the tests alone.
- [x] 2.2 (b) Implement design D3–D5: `pool_fixed_loglik()` returns `cause`
      and `label`; `new_pool_entry()` stores `degenerate_cause` and
      `degenerate_label` (not added to `pool_store_mutate()`'s resident
      field list); the store keeps the tally (built, fixed-term, distinct
      labels), updated after each `pool_entries()` call in
      `new_sequence_pool()` and `append_sequence_pool()`;
      `abem_trace_tibble()` gains the integer `fixed_degenerate` column after
      `pool_size`, filled per pass from the current pool; a helper raises the
      `goldfish_dynes_fixed_term_degenerate` warning via `cli::cli_warn()`
      with cli semantic markup, interpolating the counts and labels
      (r-lib:cli), called from `estimate_dynes()` between `run_abem_loop()`
      and `new_dynes_fit()`. Review and accept the intentional `em_trace`
      snapshot changes in `_snaps/em_loop_dynes.md` / `_snaps/fit_dynes.md`;
      reject any other snapshot diff. `air format`, lint, GREEN,
      not-cran-test (baselines PASS, not SKIP), commit
      (`feat(fixed-loglik-degenerate): ...`).

## 3. A fully fixed-term-degenerate pool stops the run

- [x] 3.1 (a) Write failing tests, following r-lib:testing-r-packages, in
      `test-em_loop_dynes.R` (stub augmenter) and one `estimate_dynes()`
      run, with `fixed_only_loglik()` mocked to `-Inf` for every sequence on
      the serial seam:
      (i) Importance weighting: the loop returns without error,
      `converged = FALSE`, parameters equal to the starting values; today it
      returns `converged = TRUE` after `stop_count` stop-hits.
      (ii) Uniform weighting (MCMC + refresh): the same result, no error;
      today it aborts with retries exhausted.
      (iii) A trip after a redraw or a retry's growth (the pool becomes fully
      dropped only then): stops at that point with the last accepted
      parameters.
      (iv) The trip on the first pool leaves a zero-row `em_trace` that the
      fit constructor accepts, with all-`NA` covariances.
      (v) Through `estimate_dynes()`: the single
      `goldfish_dynes_fixed_term_degenerate` warning carries the
      "every sequence in the pool was dropped" bullet (cli snapshot);
      a partly dropped run (2.1(iv)) does not.
      Confirm RED and commit the tests alone.
- [x] 3.2 (b) Implement design D6: after every pool build and append in
      `run_abem_loop()`, before weights, SGD and Q, return
      `abem_loop_result(theta_k, FALSE, ...)` when every sequence in the
      current pool has `degenerate_cause == "fixed_term"`; record the stop
      so the warning helper adds its extra bullet. Comment the *why* inline
      (both weighting paths fail past this point), with no OpenSpec
      references. `air format`, lint, GREEN, not-cran-test (baselines PASS,
      not SKIP), commit (`fix(fixed-loglik-degenerate): ...`).

## 4. Close

- [ ] 4.1 Write `NEWS.d/fixed-loglik-degenerate--non-finite-fixed-term.md`
      per `NEWS.d/README.md`, bullets under 81 characters each, naming the
      visible effects: `compute_lik_seq()` and `estimate_dynes()` now drop a
      sequence whose fixed-only term (e.g. a pinned rate) is non-finite as
      degenerate; `estimate_dynes()` warns once with the count and the
      failing sub-models; a run whose whole pool is dropped stops with
      `converged = FALSE`; `em_trace` gains `fixed_degenerate`. Commit it.
- [ ] 4.2 Close the change: full not-cran-test run in the background
      confirming the frozen 1e-6 baselines PASS (not SKIP). Add
      `status: landed (feature/dynes, awaiting fold)` to `proposal.md`, and
      commit. (`dynes-augmentation` 6.1b's gate needs only group 1: when 1.2
      commits, mark that gate met in its 6.1b note.) Close with close-task.
