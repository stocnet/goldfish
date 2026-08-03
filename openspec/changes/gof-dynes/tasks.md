# Tasks — gof-dynes

Sequenced after `abmcem` and `dynes-augmentation` land. Commit per task
(conventional message, tests green). Run `devtools::document()` within any task that
touches roxygen/exports/signatures. Bump `DESCRIPTION` + `NEWS.md` at each phase
milestone. Verify with `NOT_CRAN=true` (baselines PASS, not SKIP). Invoke
`r-lib:r-package-development`, `r-lib:testing-r-packages`, `r-lib:cli`, and
`r-lib:lifecycle` for the work they cover; `cpp-recompile` after any `src/` edit.

Delivers two functions: `lr_test_dynes(m1, m0)` (Phases 1–2) and `gof_dynes(fit)`
(Phase 3).

## 0. Shared foundation

- [ ] 0.1 Add the opt-in `retain_pool = FALSE` control to the estimation surface
      (`set_algorithm_em()` / `estimate_dynes()`): when `TRUE` attach the lightweight pool
      bundle (per draw: sequence, `log w`, `log q`) tagged with the augmentation
      scheme (`last_iteration` | `accumulated`) and a `theta` fingerprint; always
      record the `pool_recipe` (seed, augmenter/weighting settings, size). Add the
      opt-in heavier tier that additionally persists per-draw preprocessed
      statistics. Assert retention is a no-op at `FALSE`. roxygen + `document()` (D12).
- [ ] 0.2 Add a `resolve_gof_pool(fit)` internal returning the sample pool at the
      fit's parameters: reuse the carried bundle when present, consistent (fingerprint
      + scheme match), and not forced — reweighting `accumulated`-scheme draws to the
      target from `log q` — else regenerate a user-sized pool by one augmentation pass
      at the fit's `θ̂` replaying the recipe; record source (reused | regenerated) and
      size (D2, D12).
- [ ] 0.3 Add a scoring adapter that returns a pool's per-sequence log-likelihood at a
      given θ: use the pool's preprocessed-statistics tier when carried, else
      preprocess each unique sequence once and cache within the call (through the
      batched evaluator, falling back to the zero-iteration `estimate_wrapper()` path).
      Unit-test the carried-stats and compute-in-call paths agree, and that a sequence
      is never preprocessed twice within a call (D3, D12).
- [ ] 0.4 Add the `mirai` map seam (`gof_map()`) with a serial fallback and
      stream-split seeds, under the shared non-nested thread budget; test serial and
      parallel produce identical output under a fixed seed (D13).
- [ ] 0.5 Test `resolve_gof_pool()` across fixtures: retained-`last_iteration`,
      retained-`accumulated` (asserting reweighting), and no-pool (regenerate via
      recipe); assert the recorded source, the stale/scheme-mismatch cli warning +
      fallback to regeneration, and that an empty/degenerate pool aborts with a cli
      error.

## 1. Phase 1 — `lr_test_dynes()`: deviance + asymptotic test

- [ ] 1.1 Implement `lr_test_dynes(m1, m0)` input handling: accept two fitted DyNES
      results, validate `m0` is parameter-nested in `m1`, cli-abort naming non-nested
      terms; no re-fit, no same-data check beyond nesting (D4). Test accept/reject on
      nested and non-nested pairs.
- [ ] 1.2 Implement the weighted Monte-Carlo deviance
      `D̃ = 2·Σ wᵢ[ℓ(θ̂₁) − ℓ(θ̂₀)]/Σ wᵢ` on one shared pool **drawn under `m0`** via
      `resolve_gof_pool(m0)` + the scoring adapter (common random numbers, D3); handle
      empty/all-zero-weight pools with a cli abort. Test against an analytic toy
      likelihood.
- [ ] 1.3 Implement the asymptotic `χ²_p` test (`p` = params `m1` has beyond `m0`) on
      `max(D̃, 0)`, with the raw `D̃` retained and the informative negative-`D̃` cli
      message (D3). Test the guard and message on a near-indistinguishable pair.
- [ ] 1.4 Assemble the classed LR-test result (two model descriptions, raw `D̃`, `p`,
      pool source/size, asymptotic p-value) and `print()`/`summary()` via cli; pinned
      cli-context snapshot tests. roxygen + `document()`; experimental lifecycle badge.
- [ ] 1.5 Phase-1 milestone: `NOT_CRAN=true` suite green (baselines PASS), bump
      `DESCRIPTION` + add `NEWS.md` entry.

## 2. Phase 2 — RE resampling + bootstrap calibration (folded into `lr_test_dynes()`)

- [ ] 2.1 Implement the RE-permutation move: choose an RE pair, apply the D20 window
      rule and truncated-exponential time proposals with roles swapped (RE times move,
      PE times fixed), excluding order/support-violating proposals before construction
      (D5). Test only the two REs move, PE times untouched, sequence stays ordered
      (incl. adjacent-pair window).
- [ ] 2.2 Implement the Metropolis–Hastings acceptance
      `α = [f(Ω′)/f(Ω)]·[q_rev/q_fwd]` with move-type/pick probabilities cancelling;
      test detailed balance on a toy analytic likelihood (stubbed `eval_fn`).
- [ ] 2.3 Implement the null-drawn whole-space pool builder: from `m0`'s pool, grow
      ≥10 retained sequences per seed under `θ̂₀` (user-set, default 10), burn-in per
      restart, thinning; each output carries its `m0`-target likelihood/weight
      bookkeeping (D6); run behind the `gof_map()` seam. Test grow count, approximate
      independence, and serial/parallel agreement under a fixed seed.
- [ ] 2.4 Implement the bootstrap: `B` resamples of size `n_T` with replacement, `D̃_b`
      per resample, empirical `p̂ = #{b: D̃_b ≤ D̃}/B`; retain all `D̃_b`; fixed default
      `B` with `(B+1)·α` integer, `B`/seed user-set and reproducible (D6). Test seed
      reproducibility.
- [ ] 2.5 Extend `lr_test_dynes()` to run the bootstrap and fold both references into
      the result; extend `print()`/`summary()` to show asymptotic and bootstrap
      p-values side by side (cli snapshot).
- [ ] 2.6 If a benchmark on realistic pool sizes shows the resampling chain is a hot
      path, move the kernel to `src/` (else keep in R); on any `src/` edit follow
      `cpp-recompile` and re-verify baselines.
- [ ] 2.7 Phase-2 milestone: `NOT_CRAN=true` suite green (baselines PASS), bump
      `DESCRIPTION` + add `NEWS.md` entry.

## 3. Phase 3 — `gof_dynes()`: simulation-based goodness of fit

- [ ] 3.1 Implement layer-isolated fixed-time simulation **at `θ̂`**: fix all event
      times, redraw the sender–receiver–flavor tuple per stamp with the flavor
      constrained to the owning layer's flavor set (D7); built on the per-event
      generative source, not the constrained augmenters; run behind the `gof_map()`
      seam. Test times are unchanged, flavors respect the layer mask, and
      serial/parallel agree under a fixed seed.
- [ ] 3.2 Implement the events-per-panel-flavor statistic `A_{φ,φ',c}` as a count
      vector over `c = 1..C` for every (RE-flavor, PE-flavor) pair, with the
      `C`-truncation/overflow rule applied identically to observed and simulated
      sequences (D8). Test on a small fixture with known counts.
- [ ] 3.3 Implement the 2-path-closure statistic `A_{φ,φ',c}` (ordered RE 2-paths
      closed on the PE layer) with the same vectorization and truncation. Test on a
      fixture with a known closed 2-path.
- [ ] 3.4 Implement the Monte-Carlo Mahalanobis comparison
      `MD = (A(z) − μ̃)ᵀ Σ̃⁻¹ (A(z) − μ̃)` with a singular/ill-conditioned `Σ̃` guard
      (generalized inverse or documented ridge) surfaced as a warning, plus the MC
      tail position (D9). Test the guard on an all-zero-cell fixture.
- [ ] 3.5 Implement `gof_dynes(fit)` (experimental lifecycle badge): statistic
      selection, simulated-pool size, pool-source resolution; assemble the classed
      result (per statistic: observed, simulated distribution, Mahalanobis, tail).
      roxygen + `document()`.
- [ ] 3.6 Implement `print()`/`summary()` (cli: per-statistic distance + tail) and
      `plot()` (observed value overlaid on the simulated distribution per flavor pair
      / cell); cli snapshot + a `vdiffr`-or-smoke test for the plot.
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
