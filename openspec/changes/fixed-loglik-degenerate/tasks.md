> **TDD.** 1.1/1.2 are a red/green pair (per `openspec/config.yaml`
> `rules.tasks`): 1.1 commits the failing tests alone, 1.2 the fix that turns
> them green.
>
> **Branch.** This runs on `feature/dynes`, so no `DESCRIPTION` Version bump
> and no `NEWS.md` edit: task 1.3 writes a `NEWS.d/` fragment instead, and the
> change is flipped to `status: landed (feature/dynes, awaiting fold)` when
> done, not archived. `depends-on: abmcem`, so it folds after `abmcem` at the
> merge; until then `opsx-spec-placement-check.sh` is expected to fail on its
> `## MODIFIED` header.

## 1. A non-finite fixed-only term is degenerate

- [ ] 1.1 (a) Write failing tests in `tests/testthat/test-evaluator_dynes.R`,
      beside "a fixed-only fid adds a log-likelihood, no score or Fisher
      block", following r-lib:testing-r-packages. Build the pin with a `−Inf`
      first plateau: `make_intercept_only_rate(c(-Inf, -2), model_type =
      "DyNAM-M-Rate", wave_times = c(0, 10, 20))`. Period 1 holds the events at
      1, 3, 6, so the term is `−Inf` and the engine does not error. Add a
      `fixed_only_*` fixture variant that takes the pin as an argument; keep
      the existing fixtures' behavior unchanged.
      (i) `pool_fixed_loglik()` returns `list(loglik = -Inf, degenerate =
      TRUE)`.
      (ii) Through `fixed_only_pool()` and `evaluate_sequence_pool()` at
      `theta = c(0.3, -0.2)`: `degenerate[[1]]` is `TRUE`, `loglik[[1]]` is
      `-Inf`, and score and Fisher are all `NA`.
      (iii) A `NaN` term, from an infinite plateau with exposure
      (`c(Inf, -2)`), is degenerate with `loglik = -Inf`, not `NaN`.
      (iv) Through `evaluate_candidate()` on the same decomposition and data:
      `loglik` is `-Inf`, with no error.
      (v) Regression guard: the existing two-plateau pin `c(-1, -2)` stays
      `degenerate = FALSE` with its hand-computed term (already covered by
      the neighboring test; assert only the flag here).
      Run the file once to confirm (i)–(iv) are RED and (v) is GREEN; commit
      the tests alone (`test(fixed-loglik-degenerate): ...`).
- [ ] 1.2 (b) In `R/evaluator_dynes.R`, `pool_fixed_loglik()`: treat a
      non-finite term like the error path, returning `list(loglik = -Inf,
      degenerate = TRUE)` as soon as one is found (design D1, D2). Update the
      function's comment to say that a non-finite term, not only an engine
      failure, marks the sequence degenerate. Do not reference the change or
      design IDs in the comment. `air format` the file, then lint it.
      Re-run the 1.1 tests to GREEN, verify with not-cran-test (baselines
      PASS, not SKIP), and commit (`fix(fixed-loglik-degenerate): ...`).
- [ ] 1.3 Write `NEWS.d/fixed-loglik-degenerate--non-finite-fixed-term.md`:
      one `Fixed ...` bullet under 81 characters, per `NEWS.d/README.md`.
      (Internal evaluator, so the wording names the visible effect: a pooled
      sequence whose pinned rate scores non-finite is now dropped as
      degenerate in `estimate_dynes()`.) Commit it.
- [ ] 1.4 Close the group: full not-cran-test run in the background
      confirming the frozen 1e-6 baselines PASS (not SKIP); no
      `devtools::document()` needed (no roxygen or export change). Add
      `status: landed (feature/dynes, awaiting fold)` to `proposal.md`, tick
      `dynes-augmentation`'s 6.1 prerequisite note if it names this gate, and
      commit. Close with close-task.
