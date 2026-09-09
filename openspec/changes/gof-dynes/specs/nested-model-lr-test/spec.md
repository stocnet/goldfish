## ADDED Requirements

### Requirement: Bootstrap-adjusted likelihood-ratio test entry point

The package SHALL provide an experimental entry point `lr_test_dynes(m1, m0)` that
takes two **already-fitted** DyNES models — `m1` (full) and `m0` (null) — estimated
on the same data, and returns a classed likelihood-ratio-test object. The function
MUST validate that `m0` is nested within `m1` (its modeled parameters are a subset of
`m1`'s), aborting with a cli error naming the offending terms otherwise. It MUST NOT
re-fit either model, and it MUST NOT verify beyond nesting that the two fits used the
same data (passing same-data fits is the caller's responsibility).

#### Scenario: Nested pair accepted

- **WHEN** the user calls `lr_test_dynes(m1, m0)` with `m0`'s modeled parameters a
  strict subset of `m1`'s
- **THEN** the call returns a classed LR-test object carrying the deviance statistic,
  its degrees of freedom `p` (the number of parameters `m1` has beyond `m0`), and the
  asymptotic and bootstrap p-values

#### Scenario: Non-nested pair rejected

- **WHEN** `m0` has a modeled parameter or sub-model absent from `m1`
- **THEN** the call aborts with a cli error identifying the non-nested term(s) and
  returns no result

### Requirement: Weighted Monte-Carlo deviance on a null-drawn shared pool

The deviance SHALL be the weighted Monte-Carlo statistic
`D̃ = 2 · Σᵢ wᵢ [ℓ(Ω*ᵢ, θ̂₁) − ℓ(Ω*ᵢ, θ̂₀)] / Σᵢ wᵢ`, where `θ̂₁`, `θ̂₀` are the
fitted parameters of `m1`, `m0`, `Ω*ᵢ` are augmented sequences in a **single shared
pool drawn under the null model `m0`**, and `wᵢ` their importance weights to the `m0`
target. Both log-likelihoods MUST be evaluated on the **same** pool sequences
(common random numbers) in one pass per parameter vector. Scoring MUST use the pool's
preprocessed statistics when the fit carries the opt-in statistics tier, and
otherwise MUST compute them within the call, preprocessing each unique sequence once
and caching within the call so no sequence is preprocessed twice.

#### Scenario: Both models scored on one null-drawn pool

- **WHEN** the deviance is computed
- **THEN** the shared pool is drawn under `m0`, each sequence contributes
  `wᵢ[ℓ(θ̂₁) − ℓ(θ̂₀)]` using its `m0`-target weight, and the weight-normalized
  average is doubled to yield `D̃`

#### Scenario: Unique sequences preprocessed once per call

- **WHEN** the fit does not carry the opt-in preprocessed-statistics tier
- **THEN** the test preprocesses each unique pool sequence once, caches it within the
  call, and reuses the cache for the `θ̂₁` and `θ̂₀` evaluations and every bootstrap
  resample

### Requirement: Completed components' likelihood terms are separable

The batched evaluator SHALL record the per-fid log-likelihood contribution by
regime (modeled / completed / anchored-replay) as read from the specification's
`process_map`, not only as one total, and the Monte-Carlo deviance and every
likelihood-based comparison SHALL be computed over the modeled components only,
reporting the excluded mass; a completed component's parameter-free but
state-dependent term SHALL never enter a comparison between two fits.

#### Scenario: two fits differing only in a completed flavor compare equal

- **WHEN** two nested fits differ only in that one flavor's choice was completed
  by the uniform default in one and left out of the specification in the other
- **THEN** the deviance between them excludes the completed component's term and
  reports it separately

### Requirement: Opt-in pool retention on the estimation surface

The estimation control SHALL expose an opt-in `retain_pool` option that defaults to
`FALSE`. When `FALSE`, a fitted result MUST carry no augmented pool, only a
reproducible pool recipe (seed, augmenter and weighting settings, size). When
`TRUE`, the result MUST additionally carry a lightweight pool bundle holding, per
draw, the sampled sequence, its log-weight, and its log proposal density — never the
preprocessed objects — tagged with the augmentation scheme (`last_iteration` |
`accumulated`) and a `theta` fingerprint. A further opt-in MAY additionally persist
the per-draw preprocessed statistics (the heavier tier). Enabling any retention MUST
NOT change any estimation result other than adding the bundle.

#### Scenario: Retention off by default

- **WHEN** a model is fitted without setting `retain_pool`
- **THEN** the result carries no pool bundle but does carry the pool recipe, and all
  other result contents are identical to a fit made before the option existed

#### Scenario: Retention on stores the lightweight bundle

- **WHEN** a model is fitted with `retain_pool = TRUE` and no heavier tier requested
- **THEN** the result carries a bundle of (sequence, log-weight, log proposal
  density) per draw, tagged with the augmentation scheme and a `theta` fingerprint,
  and no preprocessed objects are stored

### Requirement: Sample-pool source resolution

The test SHALL obtain its `m0` pool by reusing the augmented pool carried on `m0`
when it is present and consistent, and otherwise regenerating from `m0`'s recipe.
Regeneration MUST be a single augmentation pass at `θ̂₀` replaying the recipe (seed,
augmenter/weighting settings, size) — not a replay of the estimation iterations —
and its size MUST be user-controllable. A reused `accumulated`-scheme pool MUST be
reweighted to the `m0` target from the stored log proposal density before use. The
chosen source (reused | regenerated) and pool size MUST be recorded on the result.

#### Scenario: Null pool reused from m0

- **WHEN** `m0` carries a non-empty, consistent augmented pool and the user does not
  force regeneration
- **THEN** the test uses that pool (reweighting to the `m0` target from the stored log
  proposal densities when the pool's scheme is `accumulated`) and records the source
  as reused

#### Scenario: Null pool regenerated from the recipe

- **WHEN** `m0` carries no pool (default `retain_pool = FALSE`) or the user forces
  regeneration
- **THEN** the test regenerates a pool of the requested size by one augmentation pass
  at `θ̂₀` replaying `m0`'s recipe, and records the source as regenerated

#### Scenario: Stale or scheme-mismatched pool falls back to regeneration

- **WHEN** a carried pool's `theta` fingerprint does not match `m0`'s parameters, or
  its scheme tag is inconsistent with the recipe
- **THEN** the test emits a cli warning and regenerates from the recipe rather than
  scoring on the stale pool

### Requirement: Asymptotic reference with a negative-deviance guard

The test SHALL report the asymptotic p-value referencing the deviance against the
`χ²_p` distribution. Because `D̃` is a Monte-Carlo difference and can come out
negative for a near-indistinguishable pair, the χ² p-value MUST be computed on
`max(D̃, 0)`, the raw `D̃` MUST still be reported, and a negative `D̃` MUST trigger an
informative cli message explaining that the two fits are near-indistinguishable on
this pool and that the bootstrap p-value is the reliable reference.

#### Scenario: Asymptotic p-value reported

- **WHEN** an LR-test object is produced
- **THEN** its summary reports the raw `D̃`, the degrees of freedom `p`, and the
  upper-tail `χ²_p` p-value computed on `max(D̃, 0)`

#### Scenario: Negative deviance guarded and explained

- **WHEN** the Monte-Carlo deviance `D̃` is negative
- **THEN** the χ² p-value uses `max(D̃, 0)`, the raw negative `D̃` is still shown, and
  a cli message directs the user to the bootstrap p-value

### Requirement: Bootstrap deviance distribution and empirical p-value

The test SHALL calibrate the deviance by bootstrap over the null-drawn whole-space
pool of size `n_T` (built by relational-event resampling under `θ̂₀`). It MUST draw
`B` resamples of size `n_T` with replacement, compute a bootstrap deviance `D̃_b` on
each, and report the empirical p-value `p̂ = #{b : D̃_b ≤ D̃} / B`. The default `B`
MUST be fixed (not adaptive) and chosen so that `(B + 1)·α` is an integer for the
conventional significance levels; `B` and the seed MUST be user-controllable, the
bootstrap MUST be reproducible under a fixed seed, and the `B` bootstrap deviances
MUST be retained on the result.

#### Scenario: Empirical p-value from resamples

- **WHEN** the user runs the bootstrap on the null-drawn whole-space pool
- **THEN** the result carries the `B` bootstrap deviances and reports
  `p̂ = #{b : D̃_b ≤ D̃} / B` alongside the asymptotic p-value

#### Scenario: Bootstrap reproducible under a fixed seed

- **WHEN** the bootstrap is run twice with the same seed, pool, and `B`
- **THEN** both runs produce identical bootstrap deviance draws and the same `p̂`

### Requirement: Reporting of the likelihood-ratio-test result

The LR-test object SHALL provide `print()` and `summary()` methods that render
through cli semantic elements, showing the `m1` and `m0` descriptions, the raw
deviance `D̃` (flagged when negative), the degrees of freedom, the pool source and
size, and the asymptotic `χ²_p` and bootstrap p-values side by side. The object MUST
be marked experimental via a lifecycle badge in its documentation.

#### Scenario: Summary renders both references

- **WHEN** a user prints or summarizes an LR-test object
- **THEN** the output shows the two model descriptions, `D̃`, `p`, the pool
  source/size, the asymptotic `χ²_p` p-value, and the bootstrap empirical p-value
  side by side
