## Why

`estimate_dynes()` (`abmcem` / `dynes-augmentation`) fits the flavored
competing-process model to multi-layer networks that mix relational-event (RE)
layers with panel-observed (PE) layers, but it ships **no way to ask whether the
fitted model actually reproduces the data**. Without a goodness-of-fit (GoF)
surface a DyNES fit cannot be defended: users cannot test a specification against
a null, cannot see which structural features the model misses, and reviewers of
the method have no diagnostic to point at. The background note
(`dynes-augmentation/augmentation_background_info.md`, "Goodness of fit") already
works out three complementary routines; the estimation routine already produces
the augmented PE sample pool they consume. This change turns that theory into the
package's GoF core.

## What Changes

**Two distinct diagnostic functions** (a likelihood-ratio test is model comparison;
goodness-of-fit interrogates a single fit), delivered as gated phases. Both resolve
their sample pool by the shared rule — **reuse the augmented pool the relevant fit
carries when present, else regenerate it** from the fit's recipe at its fitted
parameters — and both parallelize their heavy loops behind a `mirai` map seam.

- **`lr_test_dynes(m1, m0)` — bootstrap-adjusted likelihood-ratio test** (Phases
  1–2). Compares two **already-fitted, nested** DyNES models on the same data. The
  weighted Monte-Carlo deviance
  `D̃ = 2 · Σᵢ wᵢ[ℓ(Ω*ᵢ, θ̂₁) − ℓ(Ω*ᵢ, θ̂₀)] / Σᵢ wᵢ` is evaluated on **one shared
  pool drawn under the null `m0`** (common random numbers), so `θ̂₀` is the null's
  own MLE. It reports the asymptotic `χ²_p` p-value (on `max(D̃, 0)`, with an
  informative message when `D̃ < 0`) **and** a bootstrap p-value
  `p̂ = #{b : D̃_b ≤ D̃}/B` side by side. The bootstrap draws `B` resamples (fixed
  default with `(B+1)·α` integer) of the null-drawn whole-space pool built by:
  - **Relational-event resampling (`relational-event-resampling`)** — a
    Metropolis–Hastings sampler that **permutes RE times while holding PE times
    fixed** (the mirror of the estimation MCMC), growing ≥10 whole-space sequences
    per seed under `θ̂₀` so the reference varies *both* layers.
- **`gof_dynes(fit)` — simulation-based goodness of fit** (Phase 3). The SAOM/REM
  diagnostic for a single fit: compare observed auxiliary statistics — capturing
  structure **not** in the specification — against their simulated distribution via
  a Monte-Carlo Mahalanobis distance `MD = (A(z) − μ̃)ᵀ Σ̃⁻¹ (A(z) − μ̃)`.
  - **Layer-isolated fixed-time simulation at `θ̂`** — the constrained augmenters
    cannot serve (they only reproduce observed changes). Instead **fix every time
    stamp** and redraw the sender–receiver–flavor tuple at each, **constraining the
    flavor to the layer that owns that stamp** (`φ ∈ 𝓕(X₁)` for an RE stamp,
    `φ ∈ 𝓕(X₂)` for a PE stamp). Simulation is at the point estimate `θ̂`;
    `θ ~ N(θ̂, vcov)` bands are a recorded future extension.
  - **Built-in auxiliary statistics (fixed set, v1)** — **number of RE events per
    observed panel-data flavor** `A_{φ,φ',c}` and **closing of RE 2-paths on the
    panel layer** `A_{φ,φ',c}`, each a count vector over `c = 1..C`. Generalized
    geodesics and tie-creation/deletion variants are future extensions. No
    user-extension surface in v1.
- **Estimation-surface addition + reporting.** One additive, default-off
  `retain_pool` control on `set_alg_em()`/`estimate_dynes()` (lightweight bundle +
  recipe + scheme tag; an opt-in heavier preprocessed-statistics tier). Two classed
  result objects with cli `print()`/`summary()` (LR: two references side by side;
  GoF: per-statistic Mahalanobis + tail), a `plot()` for `gof_dynes()`, and a shared
  "DyNES model diagnostics" umbrella vignette. Phased `DESCRIPTION`/`NEWS.md` bumps.

Out of scope: any change to the estimation loop itself; generalized-geodesic and
tie-change auxiliary statistics; a user-pluggable statistic contract; `θ ~ N(θ̂,
vcov)` GoF bands; adaptive bootstrap `B`; GoF for the event-stream estimators
(`estimate_dynam()` / `estimate_rem()`).

## Capabilities

### New Capabilities
- `nested-model-lr-test`: the `lr_test_dynes(m1, m0)` surface — two-fitted-nested-
  model input + nesting validation, the weighted Monte-Carlo deviance over a
  null-drawn shared pool, the opt-in `retain_pool` estimation control and the
  pool-source rule, the asymptotic `χ²_p` test with the negative-deviance guard, the
  bootstrap deviance distribution and empirical p-value, and the classed result with
  its cli `print()`/`summary()` (both references side by side).
- `relational-event-resampling`: the Metropolis–Hastings sampler that permutes RE
  times with PE times fixed, its within-anchor time windows and truncated-
  exponential proposals, the acceptance ratio, and the null-drawn whole-space pool
  builder (≥10 grown sequences per seed under `θ̂₀`, behind a `mirai` map seam) that
  feeds the bootstrap.
- `gof-simulation-statistics`: the `gof_dynes(fit)` surface — layer-isolated
  fixed-time simulation at `θ̂` (behind a `mirai` map seam), the fixed built-in
  auxiliary statistics (events-per-panel-flavor and 2-path closure) with their
  `C`-truncation rule, the Monte-Carlo Mahalanobis comparison, and the classed result
  with its cli `print()`/`summary()`/`plot()`.

### Modified Capabilities
<!-- None: dynes-estimation / process-simulation / sequence-augmentation /
     multi-process-walk are not yet in openspec/specs/ (they live in the abmcem,
     dynes-augmentation, process-simulation, and make-multivariate-spec changes).
     GoF consumes their result, simulate(), and walk-handle contracts; the coupling
     — including the pool the result must optionally retain — is documented in
     design.md and handled defensively (reuse-if-present, else regenerate). -->

## Impact

- **New R surface** (experimental, cli-reported): `lr_test_dynes(m1, m0)` for the
  bootstrap-adjusted LR test (Phases 1–2) and `gof_dynes(fit)` for the
  auxiliary-statistic GoF (Phase 3), plus their `print`/`summary`(/`plot`) methods,
  two classed result objects, and a shared diagnostics vignette. NAMESPACE / roxygen
  regenerated per task.
- **Consumes the DyNES estimation contract**: the fitted result (`theta`, `vcov`,
  and — when retained — the augmented sample pool), the batched pool evaluator
  (log-likelihood over a pool at a θ), the general `simulate()` primitive
  (`process-simulation`), and the `multi-process-walk` handle its layer-isolated
  GoF simulation drives (`make-multivariate-spec`). GoF is a separate function
  taking the fit; it reuses a retained pool when present, else regenerates from a
  stored recipe.
- **One additive estimation-surface change**: an opt-in `retain_pool = FALSE`
  control on `set_alg_em()` / `estimate_dynes()` (GoF is the consumer that needs it,
  so this change adds it). When `TRUE`, the result carries a lightweight pool bundle
  (per draw: sequence, log-weight, log proposal density) plus a scheme tag and a
  `theta` fingerprint; the fit always records a reproducible pool recipe. The change
  is a no-op at the default `FALSE`, so no existing estimation behavior changes.
- **New C++ likely for Phase 2/3 hot paths** (the RE-resampling chain and the
  auxiliary-statistic accumulation over large simulated pools); if added, the
  `cpp-recompile` discipline and the frozen 1e-6 baselines apply.
- **Depends on** `abmcem` (the ABEM loop, result contract, pool evaluation),
  `dynes-augmentation` (panel semantics, the augmenters), `process-simulation`
  (the general `simulate()`), and `make-multivariate-spec` (the multivariate spec
  + walk handle) having landed; this change sequences after them.
- **Dependencies**: no new hard package dependency anticipated (bootstrap and
  Mahalanobis are base R / RcppArmadillo); `mirai` (already Suggests) may parallelize
  bootstrap resamples and simulation batches.
