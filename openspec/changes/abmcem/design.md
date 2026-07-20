## Context

This change is a **carve-out** of the ABMCEM algorithm core from the
`dynes-augmentation` change, whose design (D1, D7, D13–D18 there) settled every
decision the loop needs. Those decisions are restated below as this change's
own (with provenance noted), because this change implements them;
`dynes-augmentation` retains the spikes, panel diffing, augmenters, batched C++
evaluator, process simulation, specification validation, and the recovery
study.

Reference implementations live in `.plan/dynes/`:
`sgd_estimation_algorithm.R` drives the full loop (initial pool → weights →
SGD update → Q/ASE accept/grow/stop → standard errors) and
`estimation_algorithm_functions.R` holds the pieces — `sgd_refactor()` (the
M-step with importance weighting, Bottou step decay, fixed-parameter masking,
and Fisher aggregation), `get_weights()` (log-weight normalization with
clipping/smoothing transforms), `stratified_resample()`/`residual_resample()`,
`get_se_from_list()` (the two SE formulas from Ruth 2024 §3.3 / §2.2), and the
fixed-parameter helpers (`unfixed_params()`, `add_na_rowcol()`,
`std_error_fixed_params()`). The decisive observation for sequencing: the
prototypes obtain every E-step quantity (logLikelihood, finalScore,
finalInformationMatrix) from the **existing engine at zero optimizer
iterations** (`estimate_wrapper(..., max_iterations = 0L)` over per-sequence
preprocessed statistics). The loop therefore needs neither the batched C++
evaluator nor the real augmenters to be implemented and validated — only
contracts they can later fill.

## Goals / Non-Goals

**Goals:**
- The complete ABMCEM machinery — controls, weighting, Q/ASE, SGD M-step, EM
  control flow, result contract — implemented, tested, and committed against
  the current engine.
- The three step-family contracts (augmenter / evaluator / optimizer) frozen
  from the loop's side, so `dynes-augmentation`'s implementations plug in with
  no loop changes.
- Honest uncertainty: Fisher-based `vcov()` plus MC standard errors and the
  `em_trace` diagnostics.

**Non-Goals:**
- Panel-flag activation, wave diffing, and the three real `augment_seq_*()`
  augmenters (all `dynes-augmentation`; tests here use a stub augmenter
  fixture).
- The batched C++ pool evaluator and pool storage/memory work
  (`dynes-augmentation` D3/D4; the prototype-path adapter here is the v1
  evaluator).
- Multi-layer specification validation, PE-independence detection, and history
  spans (`dynes-augmentation` D19).
- Process simulation (`dynes-augmentation` D12).
- Incremental re-preprocessing, resample-then-mutate, within-SGD weight
  refresh, exported Q/ASE generics — all recorded future work.

## Decisions

### D1 — Carve-out boundary: this change owns the loop and the contracts' consumer side

`abmcem` implements everything the EM loop touches directly: the four control
constructors, the weighting machinery, `compute_q()`/`compute_ase()`, the SGD
optimizer, the EM control flow, the result class, and the map seam.
`dynes-augmentation` implements everything that produces or accelerates
sequences: augmenters, the batched evaluator, panel diffing, simulation. The
step contracts are the boundary — defined here (the loop consumes them),
implemented there (except the prototype-path evaluator and the test stubs,
which live here). Cross-change protocol: contract signatures change only by
amending both changes' artifacts. *Rejected:* implementing the loop inside
`dynes-augmentation` phase 5 as originally planned — the loop has no hard
dependency on phases 1–4, and burying it serializes work that can land now.

### D2 — Surface: `estimate_dynes()` + nested `set_alg_*()` constructors (from dynes-augmentation D1)

`estimate_dynes(spec, algorithm = set_alg_em(...))` with four constructors,
one per concern, nested under the EM constructor:

- `set_alg_em(n_sequences, max_iterations, accept_quantile, growth_quantile,
  stop_quantile, tolerance, stop_count = 1L, max_retries, seed,
  em_trace_se = FALSE, n_cores, augmenter = set_alg_augment(),
  weights = set_alg_weights(), optimizer = set_alg_sgd())` — the EM loop plus
  the **only** `seed` (one RNG stream governs augmentation draws, batch
  selection, and resampling). `stop_count`: the number of **consecutive**
  stopping-rule satisfactions required to terminate (checked once per
  decision pass, D7; any miss resets the streak).
- `set_alg_augment(routine = c("mcmc", "random", "sim"), burn_in, thinning,
  initialize = c("random", "sim"), initial_sequence = NULL,
  move_probs = c(permute = 0.5, shift = 0.5))` — validated here, consumed by
  the `dynes-augmentation` augmenters through the contract.
- `set_alg_weights(weighting = c("importance", "uniform"), use = c("importance",
  "resampling"), resampling_scheme = c("stratified", "residual", "random"),
  transformation = identity, refresh = FALSE, ess_threshold = 0.5)`.
- `set_alg_sgd(variant = c("minibatch", "full"), batch_size, batch_scheme =
  c("weighted", "cyclic"), step_size, step_schedule = c("constant", "decay",
  "adagrad", "adam", "momentum"), max_iterations, tolerance,
  convergence = c("gradient", "iterations"))` — `"decay"` is the prototypes'
  Bottou schedule, now its own named option; `convergence` selects the
  M-step stopping mode, `"gradient"` the default for both variants (D6).

Argument names descriptive, never Greek (`accept_quantile`/`growth_quantile`/
`stop_quantile` for the paper's α/β/γ, correspondence documented in roxygen).
Child constructors validate only their own arguments (cli errors naming valid
options); **all cross-object rules run in `set_alg_em()`** — the D4 validity
matrix and the precedence table, warn-and-ignore for inconsistent-but-ignorable
combinations, abort for impossible ones. The `set_alg_*` prefix remains
provisional pending the `set_*_opt()` naming discussion; renames are cheap
until the surface ships. `estimate_dynes()` carries the lifecycle experimental
badge. *Rejected:* one flat constructor (hides the cross-object rules);
overloading `estimate_dynam()` with an `algorithm` switch (different estimand,
different uncertainty semantics).

### D3 — Prototype-path evaluator: zero-iteration engine calls behind the evaluator contract

The v1 implementation of the evaluator contract
(`evaluate_sequence_pool(pool, theta, what = c("loglik", "score", "fisher"))`)
is an internal adapter reproducing the prototypes' pattern:

- **Preprocess once per sequence** (on pool entry): the sequence's data object
  is built and preprocessed through the existing pipeline; the preprocessed
  statistics are stored with the sequence.
- **Evaluate at any θ**: per sub-model, `estimate_wrapper()` with
  `preprocessing_init` = the stored statistics and
  `set_estimation_opt(initial_parameters = theta_block, fixed_parameters = ...,
  max_iterations = 0L, engine = "default")`, harvesting `logLikelihood`,
  `finalScore`, and `finalInformationMatrix`; θ is the concatenation across
  sub-models, split/reassembled by the adapter.
- **`what` is honored at the harvest level** in v1 (the engine computes what it
  computes; the adapter returns only what was requested) so callers are honest
  about their needs now and the batched evaluator can optimize later without
  call-site changes.
- The adapter attaches the importance-weight inputs: each pooled sequence's
  permanent reference record (θ_ref, log-likelihood at θ_ref, log proposal
  density — supplied by the augmenter), kept on the log scale.
- Per-sequence granularity is contract, not implementation detail: score
  requests return the **per-sequence score vectors**, never only a pooled
  total — the D9 covariance estimators need Σ w̄ᵢ sᵢsᵢᵀ and its centered
  variant, and aggregating before the outer product is exactly the prototype
  bug D9 rejects.

When `dynes-augmentation`'s batched C++ evaluator lands it replaces this
adapter behind the same contract; the 1e-10 equivalence scenario in its spec is
the swap's acceptance test. `compute_lik_seq()` sugar ships with the batched
evaluator, not here. *Rejected:* waiting for the batched evaluator (serializes
the whole program); building a temporary C++ path here (duplicate engine work).

### D4 — Augmenter × weighting validity matrix (from dynes-augmentation D13)

Uniform weighting is valid exactly when the proposal *is* the model's
conditional distribution at the current θ:

| `routine`  | draws from                         | `weighting = "uniform"`           | importance weights                  |
|------------|------------------------------------|-----------------------------------|-------------------------------------|
| `"mcmc"`   | ≈ target at θ_k                    | valid within the drawing iteration | likelihood ratio for reuse          |
| `"random"` | uniform over orderings/times       | **invalid**                       | required: target / uniform density  |
| `"sim"`    | sequential draws, endpoint-distorted | invalid (conditioning bias)     | required: target / draw-prob product |

`weighting = "uniform"` is accepted only with `routine = "mcmc"` and
`refresh = TRUE`; every other combination warns and switches to importance
weighting (the default). This matrix, the batch-scheme coupling (D6), and the
refresh-inapplicability rules (D5) form the precedence table `set_alg_em()`
enforces.

### D5 — Weight state: reference records, refresh, ESS guard (from dynes-augmentation D14)

Every pooled sequence permanently stores (θ_ref, log-likelihood at θ_ref, log
proposal density); for MCMC draws the last two coincide. Weights stay on the
log scale; cross-iteration reweighting is the likelihood ratio against each
sequence's own θ_ref (mixed-θ_ref pools = standard multiple importance
sampling; balance heuristic recorded as future refinement). The
`transformation` (e.g. the prototypes' `clipping()`/`smoothing()`) applies to
raw log-weights before normalization and feeds both IS estimates and resampling
probabilities; non-identity transformation + resampling warns
(truncated-IS caveat). Two staleness defenses, one code path:

- `refresh = TRUE`: redraw the whole pool every EM iteration at its grown
  size; cross-iteration reweighting, the ESS guard, and staleness
  transformations become inapplicable — non-default settings warn and are
  ignored.
- `refresh = FALSE`: ESS = 1/Σw̄² monitored per iteration; full redraw at
  current θ when ESS < `ess_threshold` × K (default 0.5). Warn when the guard
  fires (new draws generated) and when it is disabled but the condition is met;
  never at startup.

Weights are fixed for the duration of each M-step (no within-SGD refresh).

### D6 — M-step: optimizer-owned SGD loop over the concatenated θ (from dynes-augmentation D17, refined)

One M-step optimizes the concatenated parameter vector across all sub-models.
The ABMCEM is a **generalized EM** — the accept test (D7) is what certifies
ascent — but the SGD routine itself runs **for convergence**, not a few
ascent steps. `optimize_abem_sgd()` **owns its inner loop**: at each pass the
EM loop hands it an injected batch-evaluator closure (built per pass, current
pool and `what = "score"` baked in — the mirror of the augmenter's injected
proposal evaluator) plus the normalized selection weights; the optimizer
draws batches, requests scores, and updates until its stopping rule fires.
The earlier `step(e_stats, theta) → theta'` contract shape is superseded — a
pure step function cannot express batch selection and fresh evaluations at
the moving inner θ. Tests stub the closure with an analytic gradient.

- **Batch schemes**: `"weighted"` (default) draws batch members with
  replacement proportional to normalized weights and applies the unweighted
  batch mean (unbiased for the weighted gradient — selection probabilities
  equal the weights). `"cyclic"` rotates fixed batches deterministically and
  **must** importance-weight the within-batch gradient (w̄ᵢ·K/B) or it
  maximizes the wrong objective. The two coincide under uniform weights.
  Under `use = "resampling"` (D8) batches are drawn uniformly from the
  resampled multiset with unweighted gradients. Recorded v1 note: the M-step
  might later be given access to the full weighted pool alongside a
  resampled E-step — not in v1.
- **Step-size schedules**: `"constant"` (default), `"decay"` (the prototypes'
  Bottou `η₀ / (1 + η₀·t)` with `step_size` = η₀), and AdaGrad / Adam /
  momentum at fixed literature defaults, hyperparameters not exposed. (The
  previous framing of Bottou decay as "the constant schedule's
  implementation" was a contradiction — decay is its own schedule.)
- **Convergence**: `convergence = c("gradient", "iterations")`. The default,
  `"gradient"` (both variants), tests the **gradient norm** against
  `tolerance` — never the step norm (a step-norm test fires from step-size
  decay alone, regardless of the gradient; relative parameter change is a
  recorded future alternative). Under `variant = "full"` the tested gradient
  is exact; under `"minibatch"` a single batch gradient is too noisy to test
  (its norm has a sampling-noise floor even at the exact optimum), so the
  test statistic is an exponential moving average of batch gradients, with
  the smoothing constant fixed internally (consistent with the
  no-exposed-hyperparameters rule). `"iterations"` is the opt-out fixed
  budget: run to `max_iterations` with `tolerance` unused (supplying it
  warns, D4 convention). Exiting on `max_iterations` under `"gradient"` with
  the criterion unmet **warns** (a diagnostic, not an error — the D7 accept
  test still protects correctness).
- **State**: optimizer accumulators reset at every M-step.
- **Fixed parameters**: step components at fixed positions zeroed (prototype
  `fixed_positions` masking); Fisher/vcov NA-padded at those positions.
- **Cost**: score-only evaluation requests; Fisher only where consumed.

Resampling inside SGD stays disallowed; per-sub-model factorized SGD is a
recorded future development.

### D7 — EM control flow: the per-pass decision loop (from dynes-augmentation D18, refined)

- **θ₀** via `set_estimation_opt()`'s existing `initial_parameters`, which
  gains a **warm-start option** (draw one random augmentation, estimate on it,
  use those estimates); default zero vector.
- The loop runs in **passes** — an EM iteration is one initial pass plus up
  to `max_retries` grow-retries:

  ```
  per pass:
    M-step from θ_k (last accepted) on the current pool → θ′
    full-pool log-likelihood pass at θ′ (the θ_k values are cached from the
      previous iteration, never recomputed)
    Λ_i = Σ_submodels [L_i(θ′) − L_i(θ_k)];  Q, ASE per D8
    1. STOP:   |Q + z(stop_quantile)·ASE| < tolerance ?
         hit  → streak++ ; if streak == stop_count → terminate, return θ_k
         miss → streak ← 0
    2. ACCEPT: Q − z(accept_quantile)·ASE > 0 ?
         yes → θ_{k+1} = θ′; cache L(θ′) as the next reference; size the
               next pool m ← max(m, ⌈σ̂²·(z(accept) + z(growth))²/Q²⌉)
         no  → GROW: append ⌈m/k⌉ sequences (m = pool size at iteration
               start, k = 2, 3, … per consecutive rejection), discard θ′,
               retry from the M-step
  ```

- **Stop checked before accept**, deliberately: near the optimum θ′ ≈ θ_k
  drives Q and ASE to zero together and the accept bound fails ~half the
  time regardless of pool size; a stop rule reachable only after an
  acceptance would deadlock into growth until `max_retries` aborts — at the
  exact moment of convergence.
- **`stop_count`** (D2, default 1L): the streak counts consecutive
  satisfactions **per pass**; any miss — including on a grow pass — resets
  it (growth may legitimately reveal real ascent). The returned estimate is
  always the last accepted θ.
- **Growth**: harmonic increments of the iteration-start pool (m/2, m/3, …;
  ≈ 1.6·m total over the default `max_retries = 20`); k resets to 2 at every
  new EM iteration and after an ESS-guard redraw. A rejected θ′ is discarded
  and the retry's M-step restarts from θ_k. The post-acceptance sizing rule
  is the ascent-based MCEM power formula (`growth_quantile` = the paper's β);
  the prototype computed it with the sign flipped (`nChains − ceiling(…)` ≤ 0
  always), so post-acceptance growth **never ran** in the prototype
  experiments — the corrected rule is validated by the toy fixtures, not by
  prototype reproduction.
- **Failure semantics**: exhausting `max_retries` or `max_iterations` aborts
  with a `cli_abort()` carrying the trace-derived diagnosis — never a flagged
  result.
- **`em_trace`**: **one row per pass**, indexed (iteration, retry) — θ, Q,
  ASE, decision (accept / grow / stop-hit / stop), streak, pool size, new
  draws, ESS, weighting scheme in force, MCMC chain diagnostics when supplied
  by the augmenter; per-iteration parameter SEs opt-in (`em_trace_se = TRUE`,
  one extra Fisher pass per accepted iteration).

### D8 — Q and ASE as internal S3 generics, with pinned estimators (from dynes-augmentation D15, refined)

`evaluate_sequence_pool()` returns a classed E-step object
(`c("estep_is", "dynes_estep")`; likewise `estep_resampling`,
`estep_uniform`); `compute_q()` and `compute_ase()` dispatch on it. The EM
loop is written once, scheme-agnostically. Internal only; exporting for user
extensibility is a recorded follow-up.

The estimators, with Λᵢ the per-sequence log-likelihood differences
(summed across sub-models) and K the pool size:

- **Importance (default)** — no resampling anywhere: Q̂ = Σ w̄ᵢΛᵢ;
  ASE² = Σ w̄ᵢ²(Λᵢ − Q̂)² (self-normalized IS delta-method variance).
- **Resampling** — once per pass, counts c ~ Multinomial(K, w̄) convert the
  pool into an unweighted multiset that feeds **everything downstream**: SGD
  batches uniform over the multiset (D6), Q̂ = K⁻¹ Σ cᵢΛᵢ, and the ASE is the
  prototype's ratio/delta form σ̂² = Q̂²·[ΣcᵢΛᵢ²/(ΣcᵢΛᵢ)² − 1/K], which
  includes the multinomial resampling noise. Under importance weighting no
  resample ever happens.
- **Uniform** (MCMC + refresh only): Q̂ = mean(Λ); ASE² = s²(Λ)/K.

The prototype's "not enough unique samples" patch (forcing distinct indices
into a collapsed resample) is dropped — weight degeneracy is the ESS guard's
job (D5). Under MCMC draws the ASE treats sequences as independent given the
thinning; an autocorrelation index lands in `em_trace` with a user-facing
suggestion to raise `thinning` when high.

### D9 — Result contract: two covariance estimators from one final full-pool pass (from dynes-augmentation D7, refined)

After termination, **one dedicated full-pool evaluation at the returned θ̂**
supplies per-sequence scores sᵢ and information matrices Iᵢ — SEs are never
harvested from the last minibatch (the prototype's `sgd_refactor()` did
exactly that). With I = Σ w̄ᵢIᵢ and S̄ = Σ w̄ᵢsᵢ, two estimators ship:

- **Standard error** — `vcov()` returns this — the sandwich
  `I⁻¹ (Σ w̄ᵢ sᵢsᵢᵀ) I⁻¹`.
- **MC error** — carried on the result, shown by `summary()` beside the
  standard errors: `I⁻¹ (Σ w̄ᵢ² (sᵢ − S̄)(sᵢ − S̄)ᵀ) I⁻¹` — the Monte-Carlo
  noise of the score estimate mapped through the information.

Ruth (2024) presents the §3.3 covariance without writing the middle term
out; the prototype's literal `S̄S̄ᵀ` (aggregate first, outer product second)
is rejected as a bug — it is rank-1 and, because the M-step stops on the
gradient norm ‖S̄‖, bounded by the optimizer tolerance rather than by any
statistical quantity. Cross-check tests therefore target hand-computed
fixtures of the two formulas above, not `get_se_from_list()`'s output.

Both estimators always use **importance weights over the full pool,
regardless of `use`** (resampling is an E-step device, not part of the
estimand); when `use = "resampling"` a warning at the SE pass makes this
explicit. Fixed parameters: NA rows/cols (prototype
`add_na_rowcol()`/`std_error_fixed_params()`). `summary()` shows standard
and MC error side by side; `print()`/`summary()` render via cli semantic
elements under a pinned context for snapshots.

### D10 — Parallel map seam: sequence-level, serial default (from dynes-augmentation D10)

One internal map seam for every per-sequence loop (pool preprocessing on
entry, pool evaluation): default serial `lapply()`; when mirai (Suggests) is
installed and `n_cores > 1`, persistent daemons hold the data once and receive
only θ and draws per iteration. Governing invariant: workers × BLAS threads ≤
cores (`blas_threads = max(1, floor(n_cores / n_workers))`). `n_cores` lives
on `set_alg_em()` (default within CRAN's 2-core cap) with parallel RNG
streams. FORK/`mclapply` excluded; PSOCK rejected (the prototype's
`clusterExport` tax across hundreds of iterations is the anti-pattern this
seam removes). MCMC chain generation stays serial by nature — the seam covers
draws and evaluation, not the chain.

### D11 — Testing strategy: stubs first, prototype cross-checks second

- **Stub augmenter**: a contract-conformant test double returning fixed or
  seeded endpoint-hitting toy sequences with known proposal densities —
  exercises the loop, the pool bookkeeping, and the weighting with no panel
  machinery.
- **Analytic stub evaluator**: a closed-form likelihood (small exponential
  model) behind the evaluator contract — unit-tests Q/ASE dispatch, SGD
  convergence on a known optimum, accept/grow/stop transitions, and failure
  aborts deterministically and fast.
- **Prototype-path integration**: a small event-stream fixture (fully observed
  toy data, sequences = the observed sequence and perturbations) run through
  the real adapter (D3), cross-checking logLik/score/Fisher against direct
  `estimate_wrapper()` calls and, where feasible, `sgd_refactor()`'s numerical
  behavior.
- **End-to-end toy ascent**: stub augmenter + real adapter + real SGD from a
  perturbed θ₀ recovering the toy optimum within bounds (seeded,
  skip_on_cran). The full panel recovery study stays in `dynes-augmentation`.

## Risks / Trade-offs

- **[Contract drift]** `dynes-augmentation`'s augmenters/evaluator arrive later
  and strain the frozen contracts → both changes amend artifacts together
  (D1); the stub augmenter and adapter are contract-conformance tests by
  construction.
- **[Prototype-path performance]** per-sequence R-loop evaluation is slow for
  large pools → acceptable for landing and testing the loop; the batched
  evaluator is the designated remedy and swaps in behind the contract
  (`dynes-augmentation` B1 spike governs it).
- **[Surface ships before its data path]** `estimate_dynes()` exists before
  panel diffing/augmenters land → experimental badge, and the surface aborts
  informatively when given inputs the not-yet-landed machinery would need;
  exercised via fixtures until then.
- **[Weighting subtleties silently wrong]** (mis-weighted cyclic gradient,
  transformation-vs-resampling interactions) → each rule in D4–D6 has a
  dedicated unit test with hand-computed fixtures; the validity matrix is
  enforced in one place.
- **[Q/ASE estimator bugs mask non-convergence]** → analytic stub evaluator
  gives closed-form Q and ASE to test against; `em_trace` makes every decision
  auditable post hoc.

## Migration Plan

Purely additive: new surface + one extension to `set_estimation_opt()`
(warm-start option). No existing estimator behavior changes; frozen coefficient
baselines untouched. mirai enters Suggests only. Rollback is reverting the
change's commits. The `dynes-augmentation` trim (tasks 5.x/7.1 removal, spec
delta split, design pointers) happens with this change's proposal so the two
changes never double-claim.

## Open Questions

- **[naming]** `set_alg_*` prefix vs the `set_*_opt()` house convention —
  inherited from `dynes-augmentation`; decide before the constructors ship
  (rename is cheap until then).
- **[surface]** What `estimate_dynes()` accepts as its specification in v1
  (interim named-list-of-flavored-specs vs blocking on the multivariate-spec
  change) — inherited open question; until resolved, the surface phase here
  implements the loop behind a minimal internal entry point and the public
  signature is finalized when the spec-surface question closes.
- **[to discuss]** Whether MCMC retained draws enter the pool through the
  proposal evaluator's cache (preprocessed object + loglik reuse) — affects
  the pool-entry path of the D3 adapter; deliberately open, decided with
  `dynes-augmentation`'s MCMC phase.
- **[defaults]** Package default values for `step_size`, `batch_size`,
  `n_sequences`, and the stop-rule quantiles — set from the toy fixtures'
  behavior during implementation, documented in roxygen. Prototype-derived
  starting points: `batch_size = ⌈K/25⌉`, `step_size = 0.001`,
  `tolerance = 1e-3` (the driver's `epsEnd`).
