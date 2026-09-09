## Context

`estimate_dynes()` (surface + ABEM loop in `abmcem`; panel semantics, augmenters,
and the batched pool evaluator in `dynes-augmentation`; the general `simulate()`
in the `process-simulation` change; the multivariate spec + walk handle in
`make-multivariate-spec`) fits the flavored competing-process model to a **joint
multi-layer estimand** (D19 of `dynes-augmentation`): any number of RE
(event-stream) and PE (panel-observed) layers, θ the concatenation of all
sub-models' parameters, a Fisher-based `vcov()`,
Monte-Carlo standard errors, and — during estimation — a large pool of augmented PE
sequences with importance weights. What is missing is any way to *assess fit*.

The background note (`dynes-augmentation/augmentation_background_info.md`,
"Goodness of fit") derives routines that build directly on those estimation
artifacts. This change delivers them as **two distinct user surfaces**, because they
answer two different questions:

1. **A bootstrap-adjusted likelihood-ratio test** — `lr_test_dynes(m1, m0)` — a
   **model-comparison** procedure over two *nested, already-fitted* DyNES models on
   the same data. It reports the weighted Monte-Carlo deviance against both an
   asymptotic `χ²_p` reference and a bootstrap-calibrated p-value, the bootstrap
   drawing its reference sequences under the **null** model. This subsumes the
   background's deviance-LR and its RE-resampling bootstrap.
2. **A goodness-of-fit function** — `gof_dynes(fit)` — the simulation-based
   auxiliary-statistic diagnostic à la `sienaGOF` / the REM GoF of Amati et al.,
   assessing whether *one* fitted model reproduces structure it was not fit on.

The likelihood-ratio machinery is deliberately **not** part of the GoF function: an
LR test compares two models, whereas goodness-of-fit interrogates a single fit.
Both surfaces consume the estimation contracts — never modifying the estimation
loop — and both are delivered phased against them.

Constraints inherited from the repo: snake_case American-English surface; cli for
all console output; lifecycle badges for the experimental surface; `cpp-recompile`
+ frozen 1e-6 baselines if any `src/` code is added; commit-per-task with phased
`DESCRIPTION`/`NEWS.md` bumps; `NOT_CRAN=true` test gate.

## Goals / Non-Goals

**Goals:**
- `lr_test_dynes(m1, m0)`: a bootstrap-adjusted likelihood-ratio test over two
  nested fitted DyNES models, reporting the weighted Monte-Carlo deviance against
  **both** an asymptotic `χ²_p` reference and a bootstrap p-value, with the bootstrap
  reference sequences drawn under the null model `m0`.
- An RE-permutation Metropolis–Hastings sampler (the mirror of the estimation
  augmenter) and a whole-space pool builder that varies both layers, feeding the
  bootstrap.
- `gof_dynes(fit)`: the simulation-based goodness-of-fit of a single fit —
  layer-isolated fixed-time simulation at `θ̂`, a fixed set of built-in cross-layer
  auxiliary statistics, and a Monte-Carlo Mahalanobis comparison with
  `print`/`summary`/`plot`.
- Reuse the estimation sample pool when the relevant fit carries it; otherwise
  regenerate.
- Parallelize the heavy bootstrap / simulation loops with `mirai` under the shared
  thread budget.

**Non-Goals:**
- Changing the estimation loop, augmenters, the general `simulate()`
  (`process-simulation`), the walk handle (`make-multivariate-spec`), or the core
  estimation contract of `abmcem` / `dynes-augmentation` — the surfaces consume them
  (the one additive exception is the opt-in `retain_pool` flag, D12).
- Folding the LR test into the GoF function — they are separate functions (model
  comparison vs single-fit assessment).
- Propagating estimation uncertainty into the GoF simulation (drawing
  `θ ~ N(θ̂, vcov)`); v1 simulates at the point estimate `θ̂` only (D7).
- Generalized-geodesic and tie-creation/deletion-conditioned auxiliary statistics
  (recorded as future extensions), and any user-pluggable statistic contract — the
  v1 set is fixed and built-in.
- GoF for `estimate_dynam()` / `estimate_rem()`. This surface is DyNES-only.
- Excursion (insert/delete) moves in the RE sampler; v1 is permutation-only,
  parallel to the estimation augmenter's `permute`/`shift`.

## Decisions

### D1 — Two surfaces (LR test, GoF), phased, consuming the fit
The change ships two distinct user functions across gated, version-bumped phases:
- **Phase 1–2 → `lr_test_dynes(m1, m0)`**: the bootstrap-adjusted likelihood-ratio
  test over two nested fitted models — the weighted-MC deviance and asymptotic
  `χ²_p` (Phase 1), then the RE-resampling + bootstrap calibration (Phase 2).
- **Phase 3 → `gof_dynes(fit)`**: the simulation-based auxiliary-statistic
  goodness-of-fit of a single fit.

Each returns its own classed object with cli `print`/`summary` (and `plot` for
`gof_dynes`). *Why one change, not two*: the LR test and the GoF share the
pool-source rule (D2), the retained-pool contract (D12), the pool evaluator (D3),
the parallel map seam (D13), and the reporting idiom — splitting them would
duplicate that surface and its tests. *Why two functions, not one*: an LR test is
model comparison (needs two fitted models), goodness-of-fit interrogates a single
fit; conflating them would force an unnatural signature and hide that they answer
different questions. *Why consume-only*: the estimation contracts are still
stabilizing in sibling changes; anything not exposed is handled defensively (D2,
D12) rather than by demanding a contract change (the one additive exception is the
opt-in `retain_pool` flag).

### D2 — Pool source: reuse the fit's pool if present, else regenerate
GoF is a **separate function** taking a fitted `estimate_dynes()` result; it does
not run inside estimation. Every routine resolves its base sample pool by: use the
augmented pool carried on the fitted result when one is present and consistent
(D12), and the user did not force regeneration; otherwise regenerate a pool of a
user-set size from the fit's `spec` + `thetâ` through the augmenters / `simulate()`.
The resolved source (reused | regenerated) and pool size are recorded on every GoF
result. Retention itself is opt-in on the estimation side and **off by default**, so
the regenerate branch is the default path (D12). *Why*: the background assumes the
estimation pool is reused, but retaining a pool costs memory, so the estimation
surface exposes it as a flag and GoF stays correct whether or not it was set —
reuse-if-present-else-regenerate keeps the cross-change coupling soft.
*Alternative rejected*: always regenerate (discards the expensive estimation pool
the background wants reused, and loses the exact chains the estimate used); make
retention mandatory (forces the memory cost on every fit). The retained-pool
contract — flag, bundle contents, scheme, recipe, and staleness guard — is D12.

### D3 — Deviance via the batched evaluator on one shared null-drawn pool
`lr_test_dynes(m1, m0)` compares two nested **already-fitted** models: `m1` (full,
`θ̂₁`) and `m0` (null, `θ̂₀`). Because the user fits both, `θ̂₀` is the null's *own*
MLE — a proper likelihood ratio, not a profile at the full fit's nuisance values.
The weighted Monte-Carlo deviance
`D̃ = 2·Σᵢ wᵢ[ℓ(Ω*ᵢ, θ̂₁) − ℓ(Ω*ᵢ, θ̂₀)] / Σᵢ wᵢ` is computed by evaluating **one
shared pool** at both `θ̂₁` and `θ̂₀` through the batched pool evaluator that
`dynes-augmentation` supplies (falling back to the zero-iteration `estimate_wrapper()`
path `abmcem` uses if the batched evaluator has not landed). **The shared pool is
drawn under the null model `m0`** (see D6 for why), with weights `wᵢ` its importance
weights to the `m0` target; both likelihoods are scored on those same `Ω*ᵢ`.
Degrees of freedom `p` = number of parameters `m1` has beyond `m0`. Scoring uses the
pool's preprocessed statistics when the fit was retained with the opt-in statistics
tier (D12); otherwise the test computes them itself, preprocessing each unique
sequence once per call and caching within the call so the two model evaluations
(and the `B` bootstrap resamples, D6) never re-preprocess the same sequence.

**Negative-deviance guard**: `D̃` is a Monte-Carlo *difference*, so for a
near-indistinguishable pair it can come out slightly negative. The asymptotic
`χ²_p` p-value is computed on `max(D̃, 0)`; the **raw** `D̃` is still reported, and a
`D̃ < 0` triggers an informative cli message — that the two fits are near-
indistinguishable on this pool and the bootstrap p-value (D6) is the reliable
reference — not a bare warning.

*Why one shared pool*: the difference estimator has far lower variance when both
terms use the same `Ω*ᵢ` and weights (common random numbers). *Why the pool is fit
by the user, not the test*: passing two fitted models keeps `θ̂₀` the null's real
MLE and avoids the test running a second full EM internally. *Alternative rejected*:
score the null at the full fit's nuisance values (a profile deviance, cheaper but
not the true LR statistic the two-model framing now affords).

### D4 — Two fitted nested models in; nesting validated
`lr_test_dynes()` takes two **fitted** DyNES results, `m1` and `m0`, both estimated
on the same data. It validates **only** that `m0` is nested within `m1` — its modeled parameters are a
subset of `m1`'s — aborting with a cli error naming the offending terms otherwise. It
**does not** verify the two fits used the same data/spec beyond that (no node-set,
wave, or schedule comparison): passing two models fit on the same data is the
caller's responsibility, and a mismatched pair yields a meaningless — not
crash-loud — result. *Why trust the caller here*: a robust same-data fingerprint is
its own design problem (what counts as "same"), and the nesting check already
catches the common structural error; the umbrella vignette (D10) shows the correct
paired-fit workflow. No internal null re-fitting and no new null-DSL: the user
constructs the null the same
way they build any DyNES model (a nested sub-formula and/or fixed-at-zero
parameters) and fits it. *Why two fitted models*: it makes `θ̂₀` the null's own MLE
(the textbook LR), keeps the surface a plain model-comparison call like `anova()`,
and avoids the test embedding a second estimation loop. *Alternative rejected*:
`lr_test_dynes(m1, null_formula)` with internal null estimation — hides an expensive
full EM inside a "test" call and couples the test tightly to the estimation loop.
*Alternative rejected*: free-form (non-nested) model pairs — the deviance LR
asymptotics do not hold and the bootstrap reference is ill-defined.

**Revised 2026-08-21 — the surface is `test_nested()`, not `lr_test_dynes()`,
and not `anova()`.** The two-fitted-nested-models contract above is unchanged;
only the name and its home move. `test_nested()` is a new S3 generic joining the
`test_gof()` / `test_parameter()` / `test_time()` family, and this change
supplies its DyNES method. Two reasons, both self-contained (ADR-0037 is a
lookup key for the maintainer's decision log, not required reading):

- *Why not `anova()`, despite D4 above calling this "a plain model-comparison
  call like `anova()`":* a survey of the installed REM ecosystem — relevent
  1.2.1, amorem 1.0.0, dream 2.1.4, remstats 4.0.0, remify 4.0.0, remstimate
  3.0.0, remverse 0.1.0 — found that **none of them implements `anova`**, as a
  method, an export, or a generic. Comparison is done instead through
  information criteria (remstimate defines `AICC` and `WAIC` as generics) or a
  refit-from-specification function (amorem's `compare_models()`). Registering
  `anova` buys conformity with a convention that does not exist here, and costs
  a real hazard: `anova`'s one-test-column table also cannot carry the
  asymptotic and bootstrap references side by side that D6 requires.
- *Why a generic rather than a standalone `lr_test_dynes()`:* nested comparison
  is a question every goldfish fit family will ask, but its **validity is not
  uniform across them**. DyNAM and REM maximize an exact likelihood and admit
  the classical χ² LR test; a DyNES fit maximizes a Monte-Carlo estimate, so the
  naive deviance is biased and its null is not χ² — which is precisely why this
  change specifies a bootstrap adjustment. Separate methods on one generic means
  the exact-likelihood and Monte-Carlo tests can never be reached by inheritance
  from one another, so the invalid combination is a method that was never
  written rather than a guard someone must remember.

`anova()` is **not** to be registered for any goldfish fit class. A
`test_nested.default` naming the generic in its error message is the mitigation
for users who reach for `anova()` first.

### D5 — RE-permutation sampler mirrors the estimation augmenter (permute-only v1)
Phase 2's sampler is the estimation MCMC augmenter with the layers' roles swapped:
**RE times move, PE times are fixed**. It reuses the exact window rule and
truncated-exponential time proposals of `dynes-augmentation` D20 — `t′_k ∈
(t*_pred(h), t*_succ(h))`, `t′_h ∈ (max{t′_k, t*_pred(k)}, t*_succ(k))` with
neighbors taken among unmoved events and padded by boundaries — and the same
acceptance ratio `α = [f(Ω′)/f(Ω)]·[q_rev/q_fwd]`, with move-type and pick
probabilities cancelling. Invalid (order/support-violating) proposals are excluded
before construction, never evaluated. v1 is permutation-only (plus the degenerate
single-event shift), matching the augmenter; excursion inserts/deletes are deferred.
*Why reuse the augmenter's machinery*: the math is identical up to which layer is
frozen; a parallel implementation would double the surface to test and risk drift
from the augmenter's carefully-argued detailed balance. *Why not the background's
plain `f′/f` ratio*: that is exact only for uniform non-adjacent swaps; the
rate-based proposals dominate and the density bookkeeping is already required
(`dynes-augmentation` D20 rejected uniform draws for the same reason).

### D6 — Bootstrap reference drawn under the null; ≥10 grown sequences per seed; both references reported
The bootstrap calibrates the deviance's reference distribution, and a p-value tests
H₀: the null model — so the reference sequences are drawn **under `m0`**. The base
pool (D2) is `m0`'s pool (reuse `m0`'s retained pool at `θ̂₀`, else regenerate at
`θ̂₀`); the RE sampler (D5) grows ≥10 retained sequences per seed (user-set), with
burn-in per restart and thinning, into a whole-space pool of size `n_T` that varies
**both** layers under `θ̂₀`. The bootstrap then draws `B` resamples of size `n_T`
with replacement, computes `D̃_b` on each, and reports
`p̂ = #{b : D̃_b ≤ D̃}/B`; `B`, the grow count, and the seed are user-set, and all
`D̃_b` are retained for inspection/plotting. The default `B` is fixed (not adaptive)
and chosen so that `(B + 1)·α` is an integer for the conventional levels (e.g.
`B = 999`), which keeps the empirical quantile exact; the default per-seed grow count
is 10 (D6 minimum). Adaptive `B` (grow until the MC standard error of `p̂` falls below
a tolerance) is recorded as a future extension, not built in v1 — it adds a
sequential stopping rule and unpredictable runtime for a robustness gain only near
`p̂ ≈ α`.

`lr_test_dynes()` reports the asymptotic `χ²_p` p-value (D3) **and** the bootstrap
`p̂` side by side: the two agreeing is reassurance, a large gap flags that the
asymptotics / MC error are unreliable and the bootstrap should be trusted.

*Why draw under `m0`*: the null-reference logic — the reference distribution of the
deviance is its distribution when the null is true, so the pool's sampling model
must be `m0`; drawing under `m1` would center the reference on the alternative and
mis-calibrate the p-value. *Why grow the RE layer*: `m0`'s estimation pool only
varies PE placement (RE times were fixed), but the deviance lives in the whole
space, so the reference must sample RE placements too. *Why nonparametric bootstrap
of the pool*: it targets the MC error of `D̃` directly, per the background, without a
parametric model of the deviance's null distribution. *Alternative rejected*: pool
sequences from both `m0` and `m1` (needs reweighting to a common target and hedges a
question the null-reference logic already answers).

### D7 — Layer-isolated fixed-time simulation for Phase 3 (new sim mode)
The auxiliary-statistic GoF cannot use the constrained augmenters or the estimation
MCMC — they only ever reproduce the observed changes, so every simulated pool has
the same aggregate structure as the data and the statistics degenerate. Instead
(following Amati et al., adapted to the multilevel case) **fix all times** (observed
RE, sampled PE) and, at each stamp, redraw the sender–receiver–flavor tuple with the
flavor constrained to the layer that owns the stamp (`φ ∈ 𝓕(X₁)` for an RE stamp,
`φ ∈ 𝓕(X₂)` for a PE stamp). This is a **new simulation mode**, distinct from the
general `simulate()` (`process-simulation` change); it is a driver over the same
`multi-process-walk` handle (`make-multivariate-spec`: `walk_advance` to each fixed
stamp, `walk_evaluate` for the redraw distribution, `walk_inject` the redrawn tuple)
but with fixed times and per-layer flavor masks, so it lives in this change and does
not modify `process-simulation`.

**Parameters: `θ̂` point estimate only in v1.** Every simulated sequence is drawn at
the fitted `θ̂`, so the auxiliary-statistic distribution reflects the model's
inherent stochasticity plus Monte-Carlo noise, but **not** estimation uncertainty.
Drawing `θ ~ N(θ̂, vcov)` per sequence to widen the bands with parameter uncertainty
is recorded as a **future extension** (it needs a clean, PD, NA-pad-aware `vcov()`
and an extra outer draw, and changes how the Mahalanobis reference is interpreted) —
deliberately out of v1.

*Why fixed times + layer isolation*: fixing times removes the degeneracy of
free-running simulation under super-linear feedback, and isolating layers keeps each
simulated stamp on a flavor its layer actually carries. *Why `θ̂` only*: it is the
standard `sienaGOF` / Amati choice and keeps the simulated distribution's spread
interpretable as model+MC variation; the `vcov()` variant is additive later without
reworking the statistics. *Alternative rejected*: free-running `simulate()` from
`θ̂` — degeneracy / rate explosion, exactly what the background warns against.

*Groomed 2026-09-09 against `process-simulation` (ADR-0033, ADR-0034, its
D6/D9/D11).* What this decision calls "fixed-time simulation" is
`simulate(times = "observed")` — the **time-anchored** variant that change
makes first-class on every family — with two additions that stay here: the
per-layer flavor mask at each stamp (a P3 mark-kernel restriction in D11's
vocabulary) and the sampled PE times. It is therefore not a second simulation
mode but the general driver with this change's mark step, so the "new sim
mode" wording above is superseded: this change supplies a `goldfishSimSteps`
with its own `mark`, and drives the same loop. Two consequences ride along.
**Vocabulary**: the artifacts of this change say *time-anchored* /
*free-running*, never conditional/unconditional (ADR-0033's rejected
namings), and the `lr_test_dynes` → `test_nested()`, `gof_dynes` →
`test_gof(type =)` sweep recorded on 2026-08-21 remains owed. **Exclusions**:
a simulated sequence that hit the explosion guard carries a capped flag and
is excluded from the statistics pool by default and reported in aggregate;
components the completion transform filled (a uniform choice, a pinned rate)
or that were replayed rather than modeled are named in the per-component
regime record, and statistics touching them are excluded from the
Mahalanobis comparison rather than trusted to user discipline.

### D8 — Fixed built-in auxiliary statistics, two in v1
v1 ships two cross-layer statistics, no extension surface: **events-per-panel-
flavor** `A_{φ,φ',c}` and **2-path closure** `A_{φ,φ',c}`, each a count vector over
`c = 1..C` for every ordered (RE-flavor, PE-flavor) pair. `C` defaults to the
observed maximum count; overflow folds into the top cell, applied identically to
observed and simulated sequences so the vectors are commensurable. Generalized
geodesics and the tie-creation/deletion-conditioned variants are documented as
future work. *Why fixed, not a contract*: a user-pluggable statistic contract (like
the writer/step-family pattern) is real API surface to stabilize and test; the two
built-ins prove the pipeline and the interplay-focused diagnostics the method most
needs, and a contract can be added later without reworking them. *Why these two*:
they capture the RE↔PE interplay the single-layer SAOM/REM statistics miss, and the
events-per-flavor statistic is most informative exactly in the repeated-event
regime (emails/calls) DyNES targets.

### D9 — Mahalanobis comparison with a singularity guard
Discrepancy is the Monte-Carlo Mahalanobis distance `MD = (A(z) − μ̃)ᵀ Σ̃⁻¹ (A(z) −
μ̃)` with `μ̃`, `Σ̃` from the simulated statistics (Lospinoso & Snijders). Count
vectors routinely have constant/all-zero cells → singular `Σ̃`; the routine uses a
generalized inverse (or documented ridge regularization) and records a warning on
the result rather than returning `Inf`/`NaN`. A Monte-Carlo tail position (fraction
of simulations at least as extreme) accompanies each distance. *Why guard
explicitly*: `sienaGOF` hits this constantly; silent `Inf` distances are a known
foot-gun. *Alternative rejected*: dropping degenerate cells silently — changes the
statistic's dimension invisibly and hides that the model perfectly reproduces that
cell.

### D10 — Reporting: two classed objects + cli, plot where it earns its place
`lr_test_dynes()` and `gof_dynes()` return **different** S3 classes; `print`/
`summary` render via cli semantic elements (interpolated data, pinned cli context
for snapshot tests). The LR-test summary shows the two model descriptions, the
deviance `D̃` (raw, flagged if negative), `p` degrees of freedom, the pool
source/size, and the asymptotic `χ²_p` **and** bootstrap p-values side by side. The
GoF summary lists each auxiliary statistic with its Mahalanobis distance and MC tail
position. Only `gof_dynes()` gets a `plot()` (observed value overlaid on the
simulated distribution per flavor pair / cell); `lr_test_dynes()` retains its `B`
bootstrap `D̃_b` so a caller *can* plot them, but no bespoke plot method ships in v1.
Both carry experimental lifecycle badges. The two functions stay independent
(separate classes, help pages, examples, cross-referenced `@seealso`) but are
presented together under a single **"DyNES model diagnostics"** umbrella vignette —
compare nested models with `lr_test_dynes()`, then assess a single fit with
`gof_dynes()` — so users meet both as one diagnostic story. *Why cli + snapshots*:
repo standard. *Why plot only for GoF*: the auxiliary-statistic distribution is
inherently visual; a single deviance statistic is not. *Why one vignette, two
functions*: the code separation reflects that they answer different questions, but a
shared narrative is the natural onboarding path.

**Revised 2026-08-21 — the GoF `plot()` goes to autograph, not goldfish.** D10
above ships a `plot()` for `gof_dynes()` in goldfish. That is reversed: goldfish
ships the classed result, its cli `print()`/`summary()`, and a **documented
plot-data contract**; autograph implements the plot method and dispatches on the
class. Self-contained rationale (ADR-0039 is a lookup key only):

The stocnet packages are layered by vocabulary — manynet owns the data and
network generics, goldfish owns the model and post-estimation generics, and
**autograph owns the plot methods**. goldfish's own diagnostic plot methods were
externalized to autograph under the archived `residuals-gof` phase 2 and nothing
broke; autograph is Suggests-level in `DESCRIPTION`; and `class-naming-scheme`
is already aligning goldfish's class strings to the names autograph@develop has
shipped, so the dispatch target is being settled anyway. Shipping a `plot()` here
would be the first method to cross back over that boundary, on the newest and
least settled surface in the package — and "revisit later" for a *released* plot
method means a deprecation cycle in two packages rather than a move.

This also makes D10 internally consistent. Its two halves now withhold a plot
for the same reason of ownership, where previously the nested-comparison half
withheld one only because a scalar deviance is not worth plotting. Both halves
retain their draws — the bootstrap `D̃_b` values, and the simulated
auxiliary-statistic distributions — so a user is never blocked from plotting,
only from getting a method for free.

*Accepted cost*: the plot is gated on autograph, so Phase 3 ships a diagnostic
whose best rendering arrives separately. Phase 3's task list **loses** the
`plot()` deliverable and **gains** a plot-data-contract deliverable: the
contract must be documented in goldfish before autograph can implement against
it, and an autograph-side issue should track the method. Open: whether autograph
receives the raw simulated draws or a pre-summarized per-cell frame.

### D11 — C++ only where the profile demands it; baselines protected
Phases 2 and 3 (the RE-resampling chain; auxiliary-statistic accumulation over large
simulated pools) are the likely hot paths. Start in R against the contracts; move a
kernel to `src/` only when a benchmark on realistic pool sizes shows it necessary —
and when doing so, follow `cpp-recompile` (compileAttributes + force recompile) and
confirm the frozen 1e-6 baselines report PASS, not SKIP. *Why defer C++*: premature
Rcpp adds build/maintenance cost; the contracts keep an R kernel swappable for a C++
one with no surface change, exactly as `dynes-augmentation` keeps the evaluator
behind its contract.

### D12 — Retained-pool contract: `retain_pool = FALSE`, lightweight bundle (+ opt-in stats), recipe, scheme-aware
The estimation surface gains an opt-in **`retain_pool = FALSE`** control (on
`set_algorithm_em()` / `estimate_dynes()`); GoF is the consumer that needs it, so this
change owns adding it. When `FALSE` (default) the result carries no pool, only the
**recipe**; when `TRUE` the result additionally carries a lightweight pool bundle.
This is additive and a no-op at `FALSE`, so it does not change any existing
estimation behavior.

- **What is retained (lightweight, default when on):** per draw, the **sampled
  sequence**, its **log-weight** `log w`, and its **log proposal density** `log q` —
  never the preprocessed objects (respecting the ~5 GB pool-memory concern, `dynes-
  augmentation` D4). Storing `log q` (not just `log w`) lets the functions
  **recompute** importance weights against a *different* target — the null model in
  the deviance test and the shifted target in the RE-bootstrap — rather than being
  stuck with the θ̂ weights.
- **Optional preprocessed-statistics tier (opt-in only):** the augmenter already
  computes each retained draw's **preprocessed statistics** (and hence a loglik) as a
  byproduct (`dynes-augmentation` D20). The user MAY additionally opt to persist
  those statistics on the pool (a heavier bundle) so `lr_test_dynes()` / `gof_dynes()`
  score at any θ with **no** re-preprocessing. This is **off by default** (it
  reintroduces the memory cost the lightweight bundle avoids). **If the statistics
  are not carried on the fit, the functions compute them themselves** — preprocessing
  each unique sequence once per call and caching within the call, so the `B` bootstrap
  resamples (which reuse the same `n_T` sequences) never re-preprocess the same
  sequence twice. Persisted retention stays lightweight unless the user asks
  otherwise; the compute-in-function path is always available.
- **Which pool, inherited from the estimation scheme:** if estimation augments a
  **fresh pool every iteration**, the **last iteration's** pool (all draws at θ̂) is
  retained; if it augments **one pool with adapted weights**, the **accumulated**
  pool is retained. The bundle is tagged with its `scheme`
  (`last_iteration` | `accumulated`); GoF branches on the tag — the accumulated pool
  carries draws under mixed θ, so GoF reweights each draw to its target from the
  stored `log q` before use, while the last-iteration pool is already at θ̂.
- **Recipe for reproducible regeneration:** the result always records a
  `pool_recipe` = (seed, augmenter + weighting settings, size). When GoF regenerates
  (the default, since `retain_pool = FALSE`), *regenerate* means **one augmentation
  pass at θ̂** replaying the recipe — `augment_seq_*(spec, θ̂, size, seed)` — **not**
  a replay of the EM trajectory. For a fit whose scheme was `accumulated`, a
  single-pass-at-θ̂ pool is a **θ̂-equivalent proxy**, not a bit-identical
  reconstruction of the accumulated pool; this is deliberate (replaying the whole
  loop's augmentation is out of scope and rarely worth it).
- **Staleness guard:** the retained bundle carries `meta = (scheme,
  theta_fingerprint)`. On GoF, if the fingerprint does not match the fit's θ̂, or the
  scheme is inconsistent with the recipe, GoF emits a cli warning and falls back to
  regeneration rather than scoring on a stale/mismatched pool. A hand-built or edited
  fit therefore degrades to the regenerate path with a visible warning, not to
  silently wrong numbers.

*Why store `log q` in the "lightweight" bundle:* without it, the null-model deviance
term and the RE-bootstrap would have to reuse the θ̂ weights, which are only correct
for the θ̂ target; `log q` is small and makes both correct. *Why a proxy for the
accumulated case:* the accumulated pool's exact reconstruction needs every
iteration's θ; the deviance and simulation diagnostics need a valid θ̂-target pool,
which the single pass delivers. *Alternative rejected:* store final normalized
weights only (blocks correct null/bootstrap reweighting); replay the EM trajectory
on regenerate (heavy, and the proxy pool is statistically adequate).

### D13 — Parallelism: `mirai` map seam under the shared thread budget
The two heavy loops — the `B` bootstrap resamples (`lr_test_dynes`) and the
simulation batches (`gof_dynes`) — are embarrassingly parallel. Both run behind a
single map indirection backed by **`mirai`** (already Suggests; the estimation side's
choice), respecting the `dynes-augmentation` D10 **non-nested thread budget** — GoF
runs after estimation so the budget is normally free, but the seam must not nest
inside an estimation pool. Parallel RNG uses stream-split seeds so results reproduce
under a fixed seed regardless of worker count (interacting with the recipe seed of
D12). A serial fallback is the default when no daemons are configured. *Why `mirai`*:
one parallel framework across estimation and GoF, minimal deps, low latency. *Why a
map seam*: the statistics code stays backend-agnostic, so swapping to serial (tests,
CRAN's 2-core limit) or another backend needs no change to the numerics.
*Alternative rejected*: a second framework (`future`/`parallel`) alongside the
estimation side's `mirai`; C++/OpenMP threading of the kernels (orthogonal, revisited
only under D11 if the R-level map is insufficient).

### D14 — `gof_dynes()` is a second discrepancy under `test_gof()`, selected by `type` and parameterized by a control object (added 2026-08-21)

`test_gof()` already ships: the Boschi–Wit cumulative-score bridge test, which
reads the per-effect standardized cumulative score process from stored
`event_scores` and refers its sup-statistic to the analytic Kolmogorov
distribution. `gof_dynes()` as proposed in this change is **not** that test and
is not a DyNES implementation of it. The two are the two classical families:

| | analytic reference | predictive reference |
|---|---|---|
| **score discrepancy** (in-model effects) | `test_gof()` today — Brownian bridge, DyNAM/REM | the DyNES score GoF — **new, see below** |
| **auxiliary discrepancy** (out-of-model structure) | *structurally empty* — no general asymptotic null, which is why this family is simulation-based | `gof_dynes()` as proposed here; also sienaGOF, ergm's `gof()` |

They are folded under one generic rather than given two names, because after
`process-simulation` lands the *same* pair of options exists for an ordinary
`goldfishFit` too — a DyNAM or REM fit will be able to answer either question —
so two generics would mean the same choice expressed two different ways
depending on the fit class.

**The surface.** `type` selects the discrepancy. The **reference** distribution
is *not* a user argument: it is determined by the fit class and what is
available (analytic where the exact-likelihood bridge holds, predictive where it
does not), and is reported on the returned object.

```
test_gof(fit, type = "score")                        # default where available
test_gof(fit, type = "simulation", control = ...)    # auxiliary statistics
```

**Why a control object rather than loose arguments.** The two types take
genuinely disjoint parameters — `type = "simulation"` needs the simulation
count, the seed, the auxiliary-statistic set and its `C`-truncation; `type =
"score"` needs the `clock` argument the shipped test already has and none of the
others. Loose arguments that are inert for half the calls are how a generic
rots: an argument misspelled or passed to the wrong type is silently ignored
rather than rejected. A control object makes an invalid combination a
constructor error at the call site.

This also follows the convention the estimation surface already uses —
`set_algorithm_newton()` / `set_algorithm_em()` feeding `control_algo`, and
`set_preprocessing()` feeding `control_prep` — so it introduces no new pattern.
The constructor name belongs to the `set_*` family per the repo's naming
guidelines (`set_*` constructs a control or configuration object); the exact
name is left to the `algorithm-naming` work rather than minted here.

**Where the DyNES score GoF comes from.** The top-right cell above is new work
that this change does not currently scope. It is worth flagging because it is
plausibly the *cheapest* of the three surfaces: it reuses the stored per-event
scores and the pool this change is already retaining under D12, and needs no
simulator. Under importance-sampled augmentation the score process is no longer
a bridge — the stationarity condition holds for the weighted score, not for any
single augmented sequence, so no individual path returns to zero — but the
statistic remains well defined as a *realized discrepancy* referred to a
weighted predictive reference. The derivation, its two variants, and the two
caveats that must be documented alongside any p-value it produces (posterior
predictive p-values are conservative; the Monte-Carlo precision is governed by
effective sample size, not pool size) are worked out in
`.plan/sp/dynes_gof_score.md`. Nothing in this change depends on that surface
existing; it is recorded here so the `type` axis is designed with all three
cells in view rather than retrofitted for the third.

**Sequencing.** Do not rename `gof_dynes()` until the control-object shape is
settled — this decision fixes the *axis*, not the constructor. The rename and
the `lr_test_dynes` → `test_nested` rename should be swept together in one
apply-time task.

**Scope note.** The non-DyNES half of this — giving an ordinary `goldfishFit`
the simulation discrepancy — has **no home change**. The archived
`residuals-gof` deferred it explicitly ("phase 3 (`simulate()`, simulation-based
GOF, auxiliary statistics) waits for DyNES to land", and again under *Out of
scope*), and no successor was ever created. It needs one, sequenced after
`process-simulation`.

### D15 — Completed components' likelihood terms are kept separable (added 2026-09-09)

A joint DyNES fit evaluates its Monte-Carlo likelihood over every fid of the
completed specification, including the fids `complete_generative_spec()`
filled with zero-free-parameter defaults. Those fids move no parameter, but
their terms are not constants: a completed uniform choice contributes the log
of one over the current support size at each of its events, which depends on
the simulated state, and a pinned rate contributes its fixed exposure term.
Any quantity that compares two fits or penalizes a fit by its likelihood
value — the deviance of D3, `test_nested()`, an information criterion — must
therefore be able to subtract the completed components' contribution, or two
models that differ only in which flavor was completed will look different for
a reason that is not a model difference. The batched evaluator records the
per-fid log-likelihood by regime (modeled / completed / replayed) from the
`process_map`'s record rather than as one total, and the deviance reads the
modeled components only, reporting the excluded mass. *Rejected*: a single
total with a documented caveat — the caveat is exactly the kind of user
discipline D9 of `process-simulation` was written to remove.

## Risks / Trade-offs

- **Estimation contracts still moving in sibling changes** → GoF consumes only, and
  the pool-source rule (D2) plus the evaluator fallback (D3) tolerate either
  outcome of the open `abmcem` result-contract questions; no GoF code depends on an
  unlanded contract detail.
- **Monte-Carlo error of the asymptotic deviance is large for small pools** → the
  bootstrap (Phase 2) is the mitigation the background prescribes; `summary()`
  reports both references so a wide gap between them is visible.
- **RE-resampling mixing / detailed-balance bugs give a silently wrong bootstrap
  reference** → reuse the estimation augmenter's proven window/acceptance machinery
  (D5); keep a toy fixture (small n, known statistic distribution) asserting
  detailed balance and that PE times are untouched on every accepted move.
- **Simulated-statistic covariance is singular** → generalized inverse + surfaced
  warning (D9); tested on an all-zero-cell fixture.
- **Free-running simulation degeneracy** → avoided by construction via fixed-time
  layer-isolated simulation (D7); a test asserts simulated times equal the fixed
  input times exactly.
- **New C++ drifts from a stale baseline** → gated behind a profile need (D11) and
  the `cpp-recompile` + NOT_CRAN baseline discipline.

## Migration Plan

Additive and experimental — no existing behavior changes, no deprecations (the one
addition, the opt-in `retain_pool` flag, is a no-op by default, D12). Sequenced
after `abmcem` and `dynes-augmentation` land (it consumes their surfaces). Delivered
as three phases, each a milestone with its own `DESCRIPTION`/`NEWS.md` bump and green
`NOT_CRAN=true` run: **Phase 1** — `lr_test_dynes()` deviance + asymptotic `χ²_p`
(and the estimation-side `retain_pool`); **Phase 2** — RE resampling + bootstrap
calibration folded into `lr_test_dynes()`; **Phase 3** — `gof_dynes()` simulation
statistics. Rollback is per-phase; the two functions are independent classed
surfaces, so a problem in one does not block the other.

## Open Questions

- **`retain_pool` on the estimation surface** is now decided (D12: opt-in,
  `FALSE` by default, lightweight `log w` + `log q` bundle, scheme tag, recipe).
  The open piece is *where exactly* the flag and the retention logic land in the
  `abmcem` / `dynes-augmentation` code and how the two augmentation schemes report
  their `scheme` tag — settle when wiring the estimation-side task, without
  changing the GoF-side contract.
- **`theta_fingerprint` definition** for the staleness guard (D12) — a hash of the
  full `θ̂` vector vs a coarser signature; pick something cheap that still catches a
  mismatched or hand-edited fit.
- **Accumulated-pool reweighting fidelity** (D12) — reweighting each accumulated
  draw to θ̂ from the stored `log q` assumes the recorded densities suffice; confirm
  against the `abmcem` weighting scheme (especially resampling/refresh modes) before
  Phase 1 finalizes.
- **Default `C` and overflow behavior for the auxiliary statistics** (D8) — observed
  maximum vs a fixed cap; decide with a realistic fixture so simulated pools do not
  routinely overflow the top cell.
- **Bootstrap over regenerated vs reused whole-space pools** — whether the ≥10-per-
  seed grow count (D6) is enough for stable `p̂` at realistic `n_T`; tune from a
  bootstrap-variance check, not a priori.
- **Multiple-testing across many (φ, φ') statistic vectors** (D8/D9) — whether to
  report per-pair distances only or also an aggregate; defer to Phase 3 once the
  per-pair output exists.
- **`m0` must carry a recipe to regenerate its null pool** (D6) — the bootstrap
  draws under `θ̂₀`, so if `m0` was fit with `retain_pool = FALSE` the null pool is
  regenerated from `m0`'s recipe; confirm the recipe (D12) is always present on a
  fitted result even when the bundle is not.
