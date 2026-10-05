> **Landing order.** `depends-on: singular-sequence-information`. The
> `abmcem-optimization` delta modifies that change's `## ADDED` requirement,
> and the `dynes-estimation` delta modifies `abmcem`'s. This change lands on
> `feature/dynes` before `dynes-augmentation` tasks 6.2/6.2b/6.2c, which it
> gates. At the merge, archive `abmcem`, `fixed-loglik-degenerate`,
> `singular-sequence-information`, then this change. No version bump and no
> root `NEWS.md` on this branch: one `NEWS.d/` fragment instead.

## 1. The complete-data information is the negative Hessian (design D1)

- [x] 1.1 Characterization test (not a red/green pair; it pins an assumption
      Louis relies on over already-landed behavior): on a small completed
      fixture with a timed θ-rate, a pinned rate and a choice (start from
      `dynes_pinned_raw_spec()` / `recipe_count_spec()` in
      `helper-estimate_dynes.R`), assert that `compute_lik_seq()`'s `fisher`
      equals the central-difference negative Hessian of its `loglik` at θ,
      within 1e-5 relative. Commit it GREEN. If it fails, stop: D1's
      assumption is false for that family, and the design must be revisited
      before task 2.

## 2. Louis covariance and its MC bread (design D1, D2)

- [x] 2.1 (a) Write failing tests in `test-fit_dynes.R`. Rework "the standard
      and MC covariances match the hand-computed forms" to expect
      `vcov() = (Σ w̄J − Σ w̄ssᵀ + S̄S̄ᵀ)⁻¹` and the MC covariance
      `I_obs⁻¹ (Σ w̄²(s−S̄)(s−S̄)ᵀ) I_obs⁻¹`, within 1e-10. Choose the stub
      sequences' `μᵢ` so that `Var_w(μ) < 1` in every direction and `I_obs`
      stays positive definite. Add: equal scores give `vcov() = I_c⁻¹` and a
      zero MC covariance; with distinct scores every SE is at least
      `sqrt(diag(I_c⁻¹))`; the em_trace per-pass SEs (`em_trace_se = TRUE`)
      equal `sqrt(diag(I_obs⁻¹))` at the pass's proposal; and on a run with
      `em_trace_se = TRUE` that returns directly after an accepted pass
      (assert that precondition on the fixture first), the last accepted
      row's `se` equals `sqrt(diag(vcov(fit)))` within 1e-10, pinning that
      the per-pass and final covariances use the same paths and weights
      (design D1). Add a
      `skip_on_cran()` closed-form oracle test. Its fixture goes in
      `helper-estimate_dynes.R`, from the pilot in `.plan/dynes/recovery/lib.R`
      (`oracle_*`, `project_to_waves()`): an n = 12 count panel with
      `~ 1 + ego(x)` / `~ alter(y)` beside a separable observed `emails`
      stream, from a seeded `simulate()`. Assert each `calls` SE is within 10%
      of the glm closed-form SE. Add the pool-Hessian identity test (design
      D4): on a pool whose sequences carry different scores (at least one
      endogenous effect; if no helper fixture has one, add it to
      `helper-estimate_dynes.R`), with `transformation = "identity"`,
      `louis_covariances()`'s `observed_information` equals the
      central-difference negative Hessian of
      `log Σ exp(ℓᵢ(θ) − log qᵢ)`, through `evaluate_sequence_pool()` and
      `pool_log_proposal()`, within 1e-5 relative (`skip_on_cran()` if
      slow). Run to confirm RED (the oracle should read about 0.003 against
      0.18; the Hessian test fails on the missing function), then commit the
      tests alone.
- [x] 2.2 (b) Implement D1/D2. In `R/fit_dynes.R`, `sandwich_covariances()`
      becomes `louis_covariances()`, which returns `covariance`,
      `mc_covariance`, `information` (still `I_c`, for
      `singular_attribution()`), `observed_information`, `singular` and
      `not_pd`. In `R/em_loop_dynes.R`, `abem_sandwich_se()` becomes
      `abem_louis_se()`. Rewrite the code comments, removing the "Ruth 2024,
      2.2" attribution and keeping each comment's reasoning: why the middle
      term is the missing information, and the delta-method bread. Update the
      `@return` formulas in `estimate_dynes()`'s roxygen, stating that the
      estimator is approximate under a non-identity weight `transformation`
      (D1). Add D2's fixed-point reasoning for the uniform scheme to the
      bread comment. Then run
      `devtools::document()`. `air format` the touched files, then `lintr`.
      Re-run 2.1 GREEN and commit.

## 3. A non-positive-definite observed information (design D3)

- [x] 3.1 (a) Write failing tests. On a stub fixture whose score spread makes
      `I_obs` indefinite while `I_c` is invertible: the fit returns with `NA`
      covariances and exactly one
      `goldfish_dynes_observed_information_not_pd` warning, and no
      singular-information warning (cli message snapshot under a pinned cli
      context). On a `converged = FALSE` fit the same warning carries the
      extra "may not be a maximum" bullet (a second snapshot), which a
      converged fit's warning lacks. A singular `I_c` still emits only
      `goldfish_dynes_singular_information`. With `em_trace_se = TRUE`, an
      indefinite pass records `NA` SEs with no warning. `covariance_status`
      is `"ok"`, `"pool_dropped"`, `"singular"` or `"not_pd"` on the
      matching fixtures (design D5). Run RED and commit the tests alone.
- [x] 3.2 (b) Implement D3 and D5: a Cholesky check on the symmetrized
      `I_obs` in `louis_covariances()`, the `not_pd` flag, and
      `warn_observed_information_not_pd()` in `new_dynes_fit()`, after the
      pool-stop and singular checks. Use cli semantic markup and suggest
      `n_sequences` or a model-driven `routine`. Add the non-maximum bullet
      when `converged = FALSE`. Set `res$covariance_status` from the first
      check that applies. Re-run 3.1 GREEN. Then amend
      `dynes-augmentation`'s artifacts in place, citing this estimator
      instead of the old sandwich: its `sequence-augmentation` delta
      ("Batched pool evaluation with per-sequence sugar" and the "evaluator
      exposes both score and information" scenario), its design D7 and the
      4.3b task text (dated amendment notes). Add a dated amendment block to
      `abmcem` design D9. Commit.

## 4. Covariance diagnostics surfaced (design D5, D6)

- [x] 4.1 (a) Write failing tests. On the hand-computed fixture,
      `fraction_missing` equals the largest eigenvalue of
      `I_c⁻¹ (Σ w̄ssᵀ − S̄S̄ᵀ)` within 1e-10 and lies in `[0, 1)`. On the
      not-PD fixture it is at least one. On the singular and pool-dropped
      fixtures it is `NA`. `covariance_ess` equals `1 / Σ w̄²` of the
      fixture's weights within 1e-10 on the ok, not-PD and singular
      fixtures, and is `NA` on the pool-dropped fixture and on a
      `weighting = "uniform"` fit. `glance()` carries `covariance_status`,
      `fraction_missing` and `covariance_ess` beside its existing columns.
      `summary()` snapshots (pinned cli context) cover all four statuses:
      the fraction and ESS line on `"ok"`, and the reason for blank SE
      columns on the other three, with the ESS beside it on `"not_pd"` and
      `"singular"`. Run RED and commit the tests alone.
- [x] 4.2 (b) Implement D6: `louis_covariances()` returns
      `fraction_missing`, computed as the largest eigenvalue of the symmetric
      `L⁻¹ Var_w(s) L⁻ᵀ` with `L = chol(I_c)` (`eigen(symmetric = TRUE)`),
      `NA` when `I_c` is singular. `new_dynes_fit()` stores it (`NA` on a
      dropped pool), and stores `covariance_ess` via
      `effective_sample_size(res$weights)`, `NA` on a dropped pool or when
      `control$weights$weighting` is `"uniform"`. Document `covariance_ess`
      in `estimate_dynes()`'s `@return`, pointing uniform-weighting users
      to the em_trace chain diagnostics. Add the three columns to
      `glance.goldfishDynesFit()`, and the
      diagnostics line to `summary()` / `print.goldfishSummDynesFit()` with cli
      semantic elements. `print()` on the fit and the em_trace schema are
      unchanged. `air format`, then `lintr`. Re-run 4.1 GREEN and commit.

## 5. The information decomposition and `vcov()`'s `information` argument (design D7)

- [x] 5.1 (a) Write failing tests in `test-fit_dynes.R`. On the
      hand-computed fixture: `fit$information$complete`, `$missing` and
      `$observed` equal `Σ w̄J`, `Σ w̄ssᵀ − S̄S̄ᵀ` and their difference within
      1e-10, with composite-label dimnames. `vcov(fit)` and
      `vcov(fit, information = "observed")` equal
      `solve(fit$information$observed)`, and
      `vcov(fit, information = "complete_data")` equals
      `solve(fit$information$complete)`, with every observed SE at least the
      complete-data one. On the not-PD fixture: `"complete_data"` is finite
      with no warning (`expect_no_warning()`), the default is `NA`, and all
      three matrices are finite. On the singular fixture: all three matrices
      are present, and both types are `NA`. On the pool-dropped fixture:
      `information` is `NULL`, and both types are `NA`. An unknown value
      errors, with an `expect_snapshot(error = TRUE)` under a pinned cli
      context. `tidy()`, `summary()` and `coef_layout()` keep their
      signatures. Run RED and commit the tests alone.
- [x] 5.2 (b) Implement D7. `louis_covariances()` returns the three matrices
      (the existing `information`/`observed_information` fields fold into
      one `information` list, and `singular_attribution()` reads
      `$complete`). `new_dynes_fit()` names their dimensions and stores
      `res$information`, `NULL` on a dropped pool.
      `vcov.goldfishDynesFit(object, information = c("observed",
      "complete_data"), ...)` uses `rlang::arg_match()`. `"complete_data"`
      inverts `$complete` with the same `tryCatch(solve())` rule, so it
      returns `NA` when singular. Document the argument with roxygen: it
      states that `I_c⁻¹` ignores the missing information and is a lower
      bound. Add `louis1982finding` and `mclachlan2008em` to
      `inst/REFERENCES.bib`. Write the `estimate_dynes()` `@details`
      paragraph (D7), once, citing them and `ruth2024review`, and quoting
      Ruth: the bread-meat-bread form "applies to Monte Carlo variability
      within an iteration; it does not measure sampling variability due to
      the observed data". Run `devtools::document()` and confirm the man
      page renders the citations. `air format`, then `lintr`. Re-run 5.1
      GREEN and commit.

## 6. Close

- [x] 6.1 Write `NEWS.d/louis-observed-information--observed-information.md`
      per `NEWS.d/README.md`, bullets under 81 characters each. Cover five
      points. `estimate_dynes()` standard errors are now the inverse of
      Louis's observed information; they were the missing information, too
      small by orders of magnitude when little is missing. The Monte-Carlo
      error uses the same bread. An observed information that is not
      positive definite returns `NA` covariances and warns. `glance()`
      gains `covariance_status`, `fraction_missing` and `covariance_ess`,
      which `summary()` also reports. The fit stores the information
      decomposition, and `vcov(information = "complete_data")` returns the
      complete-data inverse. Commit it.
- [x] 6.2 Close the change. Run the full not-cran-test suite in the
      background and confirm the frozen 1e-6 baselines PASS (not SKIP).
      Re-accept any `summary()`/`print()` snapshot whose SEs moved, but only
      after reading its diff. Add `status: landed (feature/dynes, awaiting
      fold)` to `proposal.md` and commit. Record in `dynes-augmentation`'s
      `tasks.md` (the 6.2 gate note) and `progress.md` that 6.2/6.2b/6.2c are
      unblocked, and that 6.2b reuses the oracle fixture now in
      `helper-estimate_dynes.R`. Close with close-task.
