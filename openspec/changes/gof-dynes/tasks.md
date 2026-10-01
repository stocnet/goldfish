# Tasks — gof-dynes

Sequenced after `abmcem` and `dynes-augmentation` land. Commit per task
(conventional message, tests green). Run `devtools::document()` within any task that
touches roxygen/exports/signatures. Bump `DESCRIPTION` + `NEWS.md` at each phase
milestone. Verify with `NOT_CRAN=true` (baselines PASS, not SKIP). Invoke
`r-lib:r-package-development`, `r-lib:testing-r-packages`, `r-lib:cli`, and
`r-lib:lifecycle` for the work they cover; `cpp-recompile` after any `src/` edit.

Delivers two functions: `lr_test_dynes(m1, m0)` (Phases 1–2) and `gof_dynes(fit)`
(Phase 3).

> **TDD.** Implementation tasks below run as red/green pairs — an "(a)" task
> writes a failing test and commits it RED, the paired "(b)" task makes it pass and
> commits GREEN (per `openspec/config.yaml` `rules.tasks`). Phase-milestone version
> bumps, the conditional `src/`-hot-path refactors, and the documentation/close-out
> tasks have no new testable behavior to pair red-then-green and stay single tasks.

## 0. Shared foundation

- [ ] 0.1a Write failing tests for the opt-in `retain_pool = FALSE` control: assert
      retention is a no-op at `FALSE` (D12). Run to confirm RED; commit the tests
      alone.
- [ ] 0.1b Implement the opt-in `retain_pool = FALSE` control on the estimation
      surface (`set_algorithm_em()` / `estimate_dynes()`): when `TRUE` attach the
      lightweight pool bundle (per draw: sequence, `log w`, `log q`) tagged with the
      augmentation scheme (`last_iteration` | `accumulated`) and a `theta`
      fingerprint; always record the `pool_recipe` (seed, augmenter/weighting
      settings, size). Add the opt-in heavier tier that additionally persists
      per-draw preprocessed statistics. roxygen + `document()` (D12). Re-run the
      0.1a tests to confirm GREEN; commit.
- [ ] 0.2a Write failing tests for `resolve_gof_pool(fit)` across fixtures:
      retained-`last_iteration`, retained-`accumulated` (asserting reweighting), and
      no-pool (regenerate via recipe); assert the recorded source, the
      stale/scheme-mismatch cli warning + fallback to regeneration, and that an
      empty/degenerate pool aborts with a cli error (D2, D12). Run to confirm RED;
      commit the tests alone.
- [ ] 0.2b Implement `resolve_gof_pool(fit)` internal returning the sample pool at
      the fit's parameters: reuse the carried bundle when present, consistent
      (fingerprint + scheme match), and not forced — reweighting `accumulated`-scheme
      draws to the target from `log q` — else regenerate a user-sized pool by one
      augmentation pass at the fit's `θ̂` replaying the recipe; record source
      (reused | regenerated) and size (D2, D12). Re-run the 0.2a tests to confirm
      GREEN; commit.
- [ ] 0.3a Write failing tests for the scoring adapter: the carried-stats and
      compute-in-call paths agree, and that a sequence is never preprocessed twice
      within a call (D3, D12). Run to confirm RED; commit the tests alone.
- [ ] 0.3b Implement a scoring adapter that returns a pool's per-sequence
      log-likelihood at a given θ: use the pool's preprocessed-statistics tier when
      carried, else preprocess each unique sequence once and cache within the call
      (through the batched evaluator, falling back to the zero-iteration
      `estimate_wrapper()` path) (D3, D12). Re-run the 0.3a tests to confirm GREEN;
      commit.
- [ ] 0.4a Write failing tests for the `mirai` map seam (`gof_map()`): serial and
      parallel produce identical output under a fixed seed (D13). Run to confirm
      RED; commit the tests alone.
- [ ] 0.4b Implement the `mirai` map seam (`gof_map()`) with a serial fallback and
      stream-split seeds, under the shared non-nested thread budget (D13). Re-run
      the 0.4a tests to confirm GREEN; commit.

## 1. Phase 1 — `lr_test_dynes()`: deviance + asymptotic test

- [ ] 1.1a Write failing tests for `lr_test_dynes(m1, m0)` input handling:
      accept/reject on nested and non-nested pairs (D4). Run to confirm RED; commit
      the tests alone.
- [ ] 1.1b Implement `lr_test_dynes(m1, m0)` input handling: accept two fitted
      DyNES results, validate `m0` is parameter-nested in `m1`, cli-abort naming
      non-nested terms; no re-fit, no same-data check beyond nesting (D4). Re-run
      the 1.1a tests to confirm GREEN; commit.
- [ ] 1.2a Write failing tests for the weighted Monte-Carlo deviance: test against
      an analytic toy likelihood (D3). Run to confirm RED; commit the tests alone.
- [ ] 1.2b Implement the weighted Monte-Carlo deviance
      `D̃ = 2·Σ wᵢ[ℓ(θ̂₁) − ℓ(θ̂₀)]/Σ wᵢ` on one shared pool **drawn under `m0`** via
      `resolve_gof_pool(m0)` + the scoring adapter (common random numbers, D3);
      handle empty/all-zero-weight pools with a cli abort. Re-run the 1.2a tests to
      confirm GREEN; commit.
- [ ] 1.3a Write failing tests for the asymptotic `χ²_p` test: the guard and
      message on a near-indistinguishable pair (D3). Run to confirm RED; commit the
      tests alone.
- [ ] 1.3b Implement the asymptotic `χ²_p` test (`p` = params `m1` has beyond `m0`)
      on `max(D̃, 0)`, with the raw `D̃` retained and the informative negative-`D̃`
      cli message (D3). Re-run the 1.3a tests to confirm GREEN; commit.
- [ ] 1.4a Write failing pinned cli-context snapshot tests for `print()`/
      `summary()` on the classed LR-test result. Run to confirm RED; commit the
      tests alone.
- [ ] 1.4b Assemble the classed LR-test result (two model descriptions, raw `D̃`,
      `p`, pool source/size, asymptotic p-value) and `print()`/`summary()` via cli.
      roxygen + `document()`; experimental lifecycle badge. Re-run the 1.4a tests to
      confirm GREEN; commit.
- [ ] 1.5 Phase-1 milestone: `NOT_CRAN=true` suite green (baselines PASS), bump
      `DESCRIPTION` + add `NEWS.md` entry.

## 2. Phase 2 — RE resampling + bootstrap calibration (folded into `lr_test_dynes()`)

- [ ] 2.1a Write failing tests for the RE-permutation move: only the two REs move,
      PE times untouched, sequence stays ordered (incl. adjacent-pair window) (D5).
      Run to confirm RED; commit the tests alone.
- [ ] 2.1b Implement the RE-permutation move: choose an RE pair, apply the D20
      window rule and truncated-exponential time proposals with roles swapped (RE
      times move, PE times fixed), excluding order/support-violating proposals
      before construction (D5). Re-run the 2.1a tests to confirm GREEN; commit.
- [ ] 2.2a Write failing tests for the Metropolis–Hastings acceptance: detailed
      balance on a toy analytic likelihood (stubbed `eval_fn`). Run to confirm RED;
      commit the tests alone.
- [ ] 2.2b Implement the Metropolis–Hastings acceptance
      `α = [f(Ω′)/f(Ω)]·[q_rev/q_fwd]` with move-type/pick probabilities
      cancelling. Re-run the 2.2a tests to confirm GREEN; commit.
- [ ] 2.3a Write failing tests for the null-drawn whole-space pool builder: grow
      count, approximate independence, and serial/parallel agreement under a fixed
      seed (D6). Run to confirm RED; commit the tests alone.
- [ ] 2.3b Implement the null-drawn whole-space pool builder: from `m0`'s pool,
      grow ≥10 retained sequences per seed under `θ̂₀` (user-set, default 10),
      burn-in per restart, thinning; each output carries its `m0`-target
      likelihood/weight bookkeeping (D6); run behind the `gof_map()` seam. Re-run
      the 2.3a tests to confirm GREEN; commit.
- [ ] 2.4a Write failing tests for the bootstrap: seed reproducibility (D6). Run to
      confirm RED; commit the tests alone.
- [ ] 2.4b Implement the bootstrap: `B` resamples of size `n_T` with replacement,
      `D̃_b` per resample, empirical `p̂ = #{b: D̃_b ≤ D̃}/B`; retain all `D̃_b`; fixed
      default `B` with `(B+1)·α` integer, `B`/seed user-set and reproducible (D6).
      Re-run the 2.4a tests to confirm GREEN; commit.
- [ ] 2.5a Write failing cli snapshot tests for `print()`/`summary()` showing
      asymptotic and bootstrap p-values side by side. Run to confirm RED; commit
      the tests alone.
- [ ] 2.5b Extend `lr_test_dynes()` to run the bootstrap and fold both references
      into the result; extend `print()`/`summary()` to show asymptotic and
      bootstrap p-values side by side. Re-run the 2.5a tests to confirm GREEN;
      commit.
- [ ] 2.6 If a benchmark on realistic pool sizes shows the resampling chain is a hot
      path, move the kernel to `src/` (else keep in R); on any `src/` edit follow
      `cpp-recompile` and re-verify baselines.
- [ ] 2.7 Phase-2 milestone: `NOT_CRAN=true` suite green (baselines PASS), bump
      `DESCRIPTION` + add `NEWS.md` entry.

## 3. Phase 3 — `gof_dynes()`: simulation-based goodness of fit

- [ ] 3.1a Write failing tests for layer-isolated fixed-time simulation: times are
      unchanged, flavors respect the layer mask, and serial/parallel agree under a
      fixed seed (D7). Run to confirm RED; commit the tests alone.
- [ ] 3.1b Implement layer-isolated fixed-time simulation **at `θ̂`**: fix all
      event times, redraw the sender–receiver–flavor tuple per stamp with the
      flavor constrained to the owning layer's flavor set (D7); built on the
      per-event generative source, not the constrained augmenters; run behind the
      `gof_map()` seam. Re-run the 3.1a tests to confirm GREEN; commit.
- [ ] 3.2a Write failing tests for the events-per-panel-flavor statistic: a small
      fixture with known counts (D8). Run to confirm RED; commit the tests alone.
- [ ] 3.2b Implement the events-per-panel-flavor statistic `A_{φ,φ',c}` as a count
      vector over `c = 1..C` for every (RE-flavor, PE-flavor) pair, with the
      `C`-truncation/overflow rule applied identically to observed and simulated
      sequences (D8). Re-run the 3.2a tests to confirm GREEN; commit.
- [ ] 3.3a Write failing tests for the 2-path-closure statistic: a fixture with a
      known closed 2-path. Run to confirm RED; commit the tests alone.
- [ ] 3.3b Implement the 2-path-closure statistic `A_{φ,φ',c}` (ordered RE 2-paths
      closed on the PE layer) with the same vectorization and truncation. Re-run
      the 3.3a tests to confirm GREEN; commit.
- [ ] 3.4a Write failing tests for the Monte-Carlo Mahalanobis comparison: the
      guard on an all-zero-cell fixture (D9). Run to confirm RED; commit the tests
      alone.
- [ ] 3.4b Implement the Monte-Carlo Mahalanobis comparison
      `MD = (A(z) − μ̃)ᵀ Σ̃⁻¹ (A(z) − μ̃)` with a singular/ill-conditioned `Σ̃` guard
      (generalized inverse or documented ridge) surfaced as a warning, plus the MC
      tail position (D9). Re-run the 3.4a tests to confirm GREEN; commit.
- [ ] 3.5 Implement `gof_dynes(fit)` (experimental lifecycle badge): statistic
      selection, simulated-pool size, pool-source resolution; assemble the classed
      result (per statistic: observed, simulated distribution, Mahalanobis, tail).
      roxygen + `document()` (no bespoke unit test of its own — it is exercised
      through task 3.6's `print()`/`summary()`/`plot()` tests).
- [ ] 3.6a Write failing tests for `print()`/`summary()` and `plot()`: cli snapshot
      + a `vdiffr`-or-smoke test for the plot. Run to confirm RED; commit the tests
      alone.
- [ ] 3.6b Implement `print()`/`summary()` (cli: per-statistic distance + tail) and
      `plot()` (observed value overlaid on the simulated distribution per flavor
      pair / cell). Re-run the 3.6a tests to confirm GREEN; commit.
- [ ] 3.7 If auxiliary-statistic accumulation over large simulated pools is a hot
      path, move the kernel to `src/` (else keep in R); follow `cpp-recompile` and
      re-verify baselines on any `src/` edit.
- [ ] 3.8 Phase-3 milestone: `NOT_CRAN=true` suite green (baselines PASS), bump
      `DESCRIPTION` + add `NEWS.md` entry.

## 4. Documentation and close-out

- [ ] 4.1 Add the shared "DyNES model diagnostics" umbrella vignette walking through
      `lr_test_dynes()` (compare nested models) and `gof_dynes()` (assess a single
      fit) on toy fitted models; cross-reference the two help pages (`@seealso`);
      precompile if it estimates.
- [ ] 4.2 Update `_pkgdown.yml` reference index with `lr_test_dynes()` / `gof_dynes()`
      and their methods; confirm all experimental lifecycle badges render.
- [ ] 4.3 Final `NOT_CRAN=true` full-suite run and `R CMD check` clean; resolve the
      design Open Questions that Phase 1–3 work settled in the change's `progress.md`.
