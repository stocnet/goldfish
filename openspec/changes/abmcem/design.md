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
prototypes obtain every E-step quantity (`log_likelihood`, `final_score`,
`final_information_matrix`) from the **existing engine at zero optimizer
iterations** (`estimate_wrapper(..., max_iterations = 0L)` over per-sequence
preprocessed statistics). The loop therefore needs neither the batched C++
evaluator nor the real augmenters to be implemented and validated — only
contracts they can later fill.

**A superseded fragment of this algorithm used to sit in the package.**
`R/functions_estimate_emdynam.R` (`estimate_emdynam()`) and
`R/functions_preprocess_em.R` (`preprocess_emdynam_competition()`) were the
driver half of an earlier pass at this same research line — they called
`sgd_refactor()`, `get_weights()`, `residual_resample()`,
`stratified_resample()`, `getChainSample()` and `computeSupportConstrain()`,
i.e. exactly the `.plan/dynes/` helpers listed above, which were never brought
into the package. They were unexported, referenced by nothing, and could not
execute (14 unresolvable functions, ~30 free variables read from an absent
calling frame, and a `retunr(...)` typo). `backend-parity` removed them to
`.plan/dynes/` (its design D18) so the whole prototype sits in one place. Noted
here so this change is not read as duplicating something still in `R/`, and so
a later reader does not go looking for a fragment that is deliberately gone:
the algorithm lands here as `set_algorithm_em()` / `estimate_dynes()`, not as
a revival of `estimate_emdynam()`.

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

### D2 — Surface: `estimate_dynes()` + `set_algorithm_em()` with nested component constructors (from dynes-augmentation D1)

`estimate_dynes(spec, control_algo = set_algorithm_em(...))`, where `spec` is a
`make_joint_specification()` object (`make-multivariate-spec`) — the spec-input
question is resolved to consuming that constructor, not an interim named-list. The
loop machinery here has no hard dependency on it (tests drive the loop with a stub
augmenter and analytic evaluator over a minimal spec fixture), but the public
signature binds to the multivariate spec so the surface does not churn when the real
data path lands. The joint spec MAY span multiple mode-pairs — one- and
two-mode dependent processes over distinct mode-pairs, `make-multivariate-spec`
D8 — but the loop is **fid-indexed** throughout: θ concatenates per-fid
sub-model parameters, the pools and the coupling/`coupled` separability column
are per fid, and mode-pair keying lives entirely in the walk beneath the
evaluator contract. So node-space generality is transparent to this change's
machinery; nothing here keys on mode. The user authors θ₀ against this fid
concatenation through the shared **`goldfishParams`** surface
(`joint-parameters`): `set_parameters(spec, ...)` keyed by readable
`layer[:flavor]:sub_model` labels, `coef_layout(spec)` to discover names/order,
with the flat free-parameter projection consumed here; fixed effects are the
spec's formula offsets and never enter θ (`joint-parameters` D4). Four
constructors, one per concern, nested under the EM constructor:

- `set_algorithm_em(n_sequences, max_iterations, accept_quantile, growth_quantile,
  stop_quantile, tolerance, stop_count = 1L, max_retries, seed,
  initial_parameters = NULL,
  em_trace_se = FALSE, n_cores, augmenter = set_augmenter_options(),
  weights = set_weights_options(), optimizer = set_sgd_options())` — the EM loop plus
  the **only** `seed` (one RNG stream governs augmentation draws, batch
  selection, and resampling). `stop_count`: the number of **consecutive**
  stopping-rule satisfactions required to terminate (checked once per
  decision pass, D7; any miss resets the streak). **`initial_parameters` lives
  here** and accepts **only a `goldfishParams`** (`joint-parameters`
  D1): it is θ₀, whose free (non-offset `NA`) slots are the parameters
  estimated, default `NULL` → zero over the free set. **There is no
  `fixed_parameters` argument** (D-GAP-1 resolution, superseded by
  `joint-parameters` D4): the fixed set lives in the specification's per-effect
  formula offsets (`offset()`), not on the EM control — the zero-iteration
  evaluator (D3) reads each sub-spec's offsets directly, and the warm-start
  Newton fits inherit them from the sub-specs. `n_sequences` is the **initial
  pool size** (the loop's m₀ / K, D7).
- `set_augmenter_options(routine = c("mcmc", "random", "sim"), burn_in, thinning,
  initialize = c("random", "sim"), initial_sequence = NULL, warm_start = FALSE,
  move_probs = c(permute = 0.5, shift = 0.5))` — validated here, consumed by
  the `dynes-augmentation` augmenters through the contract. **`warm_start`**
  (D-GAP-1 resolution): when `TRUE`, before iteration 1 the loop draws one
  augmentation and runs, **per fid, an independent internal
  `set_algorithm_newton()` with its default arguments** on that fid's drawn
  sequence — each fit reading its sub-spec's formula offsets automatically (no
  `fixed_parameters` to inherit) — concatenating the per-fid estimates into θ₀.
  The fits need not fully converge (they only supply a θ₀ "closer" to the
  solution); `set_algorithm_newton()` itself gains **no** new argument. Default
  `FALSE` → θ₀ is the EM control's `initial_parameters`. **Precedence: when
  `initial_parameters` is supplied (non-`NULL`) and `warm_start = TRUE`, the
  warm start is warn-and-ignored** — the user's explicit start wins (D4
  precedence convention).
- `set_weights_options(weighting = c("importance", "uniform"), use = c("importance",
  "resampling"), resampling_scheme = c("stratified", "residual", "random"),
  transformation = identity, refresh = FALSE, ess_threshold = 0.5)`.
- `set_sgd_options(variant = c("minibatch", "full"), batch_size, batch_scheme =
  c("weighted", "cyclic"), step_size, step_schedule = c("constant", "decay",
  "adagrad", "adam", "momentum"), max_iterations, tolerance,
  convergence = c("gradient", "iterations"))` — `"decay"` is the prototypes'
  Bottou schedule, now its own named option; `convergence` selects the
  M-step stopping mode, `"gradient"` the default for both variants (D6).

Argument names descriptive, never Greek (`accept_quantile`/`growth_quantile`/
`stop_quantile` for the paper's α/β/γ, correspondence documented in roxygen).
Child constructors validate only their own arguments (cli errors naming valid
options); **all cross-object rules run in `set_algorithm_em()`** — the D4 validity
matrix and the precedence table, warn-and-ignore for inconsistent-but-ignorable
combinations, abort for impossible ones. The EM constructor's name is settled:
`set_algorithm_em()`, joining the `goldfishAlgorithm` superclass
(post class-naming-scheme spelling, ADR-0031) that
`set_algorithm_newton()` introduces, so `estimate_dynes()` gates on the same
`inherits()` check as the DyNAM/REM estimators. The three nested component
constructors take the `set_<component>_options()` sub-control names —
`set_augmenter_options()`, `set_weights_options()`, `set_sgd_options()` — distinct
from the `set_algorithm_<family>()` pattern because they are option bundles nested
under the algorithm object, not algorithm objects (no `goldfishAlgorithm`
superclass). This change ships the surface and owns the names; they are settled.
`estimate_dynes()` carries the lifecycle experimental badge. *Rejected:* one flat constructor (hides the cross-object rules);
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
  `preprocessed` = the stored statistics and
  `set_algorithm_newton(initial_parameters = theta_block,
  max_iterations = 0L, backend = "r")` — `theta_block` the sub-model's slice of
  the concatenated θ over that fid's **free** effects; the fid's fixed effects
  are the sub-spec's formula offsets, already baked into its preprocessed
  statistics (no `fixed_parameters`/`offset_coef` argument — those are rejected
  on the multi-process path and unnecessary here) — harvesting
  `log_likelihood`, `final_score`, and
  `final_information_matrix`; θ is the concatenation across sub-models,
  split/reassembled by the adapter.
- **`what` is honored at the harvest level** in v1 (the engine computes what it
  computes; the adapter returns only what was requested) so callers are honest
  about their needs now and the batched evaluator can optimize later without
  call-site changes.
- **Degenerate-sequence guard (D-ROBUST)**: the zero-iteration engine path
  **unconditionally inverts the information matrix** and errors on a singular one
  (`estimation_core.R`'s "cannot be inverted; collinearity" `stop()`), even for a
  `what = "loglik"`/`"score"` request. So the per-sequence engine call is wrapped
  in `tryCatch`: a sequence whose information matrix is singular at θ returns a
  **non-finite** log-likelihood/score (never aborting the whole pool). Its weight
  then normalizes to ~0 and the ESS guard (D5) absorbs the loss; the batched
  evaluator (`dynes-augmentation`) inherits the same total-tolerance contract.
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

**Pool-entry path stays attachment-optional (MCMC-reuse fast-follow, see Open
Questions / `dynes-augmentation` D23).** v1 preprocesses every pooled sequence
from scratch on entry, uniformly for every `routine` — the A0 baseline. But MCMC
retained draws already materialized their θ-free statistics and `loglik @ θ_k`
inside the MH acceptance ratio, so a later fast-follow (A3) lets the pool adopt a
precomputed `(stats, loglik@θ_ref)` attachment when one is present and
`θ_ref == θ_k` and the layout tag matches, else fall through to the fresh
preprocess. To keep that a drop-in rather than a rework, the v1 pool-entry record
construction here is shaped to accept an optional attachment (ignored in v1), and
`make_proposal_evaluator()`'s return contract is kept widen-able (from
`(loglik, rates)` to also surface a stats handle) — that closure return is the
single point where the reuse path crosses the carve-out seam, and it is this
change's to own.

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
refresh-inapplicability rules (D5) form the precedence table `set_algorithm_em()`
enforces.

**Cold-start construction guard (dynes-augmentation D1/D14 resolution).** Also enforced
here at construction: `routine = "random"` combined with a non-zero user θ₀
(via `set_algorithm_em()`'s `initial_parameters`) or `warm_start = TRUE` warns that
uniform draws will sit far from the target at iteration 1 and the E-step weights may
collapse, recommending `routine = "sim"`/`"mcmc"` or θ₀ = 0. This is the
settle-at-construction complement to the runtime cold-start diagnostic (D5); it does not
abort (the combination is valid, just risky) and stays silent for `random` at the
default θ₀ = 0, where the proposal is near the model.

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

**Cold-start diagnostic (dynes-augmentation D14 resolution).** The "never at
startup" rule silences the recurring guard warning at iteration 1, where a redraw is
pointless. It does **not** silence a pathological cold start: a **one-shot** diagnostic
— separate from the guard warning — fires when the startup ESS falls below a hard
**pathology floor** (well under the 0.5 guard), naming the proposal/target mismatch and
the levers (`routine = "sim"`/`"mcmc"` with `initialize = "sim"`, or θ₀ = 0). It only
informs — never redraws (a uniform redraw is statistically identical) or grows the
pool. It is quiet on the default θ₀ = 0 path (`random` ≈ the near-uniform model there)
and earns its keep on the `random` + non-zero-θ₀ corner. The **construction-time**
complement lives in `set_algorithm_em()` (D4): `routine = "random"` with a non-zero
user θ₀ or `warm_start = TRUE` warns before any compute.

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
- **Fixed parameters**: none enter θ — fixed effects are the spec's formula
  offsets, excluded from the free-parameter vector before the M-step ever sees
  it (the prototype's `fixed_positions` in-θ masking is obsoleted by the
  offset-in-spec model, `joint-parameters` D4). Their display padding is a
  result-rendering concern (D9), not an optimizer one.
- **Cost**: score-only evaluation requests; Fisher only where consumed.

Resampling inside SGD stays disallowed; per-sub-model factorized SGD is a
recorded future development.

### D7 — EM control flow: the per-pass decision loop (from dynes-augmentation D18, refined)

- **θ₀** from `set_algorithm_em()`'s `initial_parameters` — a
  `goldfishParams` whose free (non-offset `NA`) slots are the estimand,
  default `NULL` → zero over the free set (`joint-parameters` D1/D4). The
  **`warm_start` option** on `set_augmenter_options()` (D2) replaces θ₀ with the
  concatenation of **per-fid independent default-`set_algorithm_newton()` fits**
  on a single augmentation (each fit reading its sub-spec's offsets
  automatically); it need not fully converge. When `initial_parameters` is
  supplied and `warm_start = TRUE`, the warm start is **warn-and-ignored**
  (explicit start wins).
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
- **Failure semantics** — the two caps differ:
  - **`max_retries`** (growth cannot find ascent *within* one iteration):
    `cli_abort()` with the trace-derived diagnosis — there is no meaningful
    partial result at a stalled iteration.
  - **`max_iterations`** (the *outer* budget is exhausted mid-ascent):
    **return the last accepted θ** carrying a **non-convergence warning** and a
    `converged = FALSE` flag on the result — never silent — mirroring how the
    goldfish Newton estimators return an unconverged fit rather than discarding
    the work. `summary()`/`print()` surface the non-convergence prominently
    (D9). Throwing away many accepted ascent steps at a budget boundary is the
    behavior this deliberately avoids.
- **`em_trace`**: **one row per pass**, indexed (iteration, retry) — θ, Q,
  ASE, decision (accept / grow / stop-hit / stop), streak, pool size, new
  draws, ESS, weighting scheme in force, MCMC chain diagnostics when supplied
  by the augmenter; per-iteration parameter SEs opt-in (`em_trace_se = TRUE`,
  one extra Fisher pass per accepted iteration).

### D8 — Q and ASE as internal S3 generics, with pinned estimators (from dynes-augmentation D15, refined)

`evaluate_sequence_pool()` returns a classed E-step object
(`c("goldfishEstepIS", "goldfishDynesEstep")`; likewise `goldfishEstepResampling`,
`goldfishEstepUniform`); `compute_q()` and `compute_ase()` dispatch on it. The EM
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
explicit. **Fixed parameters do not enter θ** (they are the spec's formula
offsets, `joint-parameters` D4), so there is nothing to invert for them; their
display padding **reuses the existing single-process machinery** —
`GetFixed()` + `stats::.vcov.aliased()` (`methods_postestimate.R`) — rather
than the prototype's `add_na_rowcol()`/`std_error_fixed_params()`, which are
retired. `coef()`/`vcov()` stay **flat** (base-generic contract), named by the
`joint-parameters` composite labels; `summary()`/`print()` use
`coef_layout(result)` to group the flat vector into per-process blocks and show
standard and MC error side by side, and surface the `converged` flag (D7) —
rendering via cli semantic elements under a pinned context for snapshots.

### D10 — Parallel map seam: sequence-level, serial default (from dynes-augmentation D10)

One internal map seam for every per-sequence loop (pool preprocessing on
entry, pool evaluation): default serial `lapply()`; when mirai (Suggests) is
installed and `n_cores > 1`, persistent daemons hold the data once and receive
only θ and draws per iteration. Governing invariant: workers × BLAS threads ≤
cores (`blas_threads = max(1, floor(n_cores / n_workers))`). `n_cores` lives
on `set_algorithm_em()` (default within CRAN's 2-core cap) with parallel RNG
streams. FORK/`mclapply` excluded; PSOCK rejected (the prototype's
`clusterExport` tax across hundreds of iterations is the anti-pattern this
seam removes). MCMC chain generation stays serial by nature — the seam covers
draws and evaluation, not the chain.

**RNG reproducibility (Option B — resolved with dynes-augmentation).** The single
`seed` seeds an `L'Ecuyer-CMRG` root (`RNGkind()` set at estimation entry,
`withr`-restored on exit) fanning into three substream families: **augmentation**
substreams keyed by a structured `(iteration, draw_index)` — worker identity never
enters the key, so the drawn pool is identical at any `n_cores`, serial included;
the **MCMC chain** substream, one long serial stream advanced continuously across
warm starts on the coordinator (never crosses the seam); and a coordinator-side
**control** substream for batch selection and resampling. The seam injects each
augmentation substream state into its draw task and — the second half of
cross-`n_cores` bit-for-bit — **collects per-sequence results and reduces them in
sequence-index order, never arrival order**, so the M-step sums do not re-round when
worker count changes. Augmenters consume an injected substream state and are keying-
agnostic (dynes-augmentation D2/D20); the seam owns the keying and the reduction
order. Documented promise is two-part: the **drawn sequences** are identical across
any `n_cores` (pure-R draws, no BLAS — holds across machines); the **numeric
estimates** are identical across any `n_cores` **on the same machine / BLAS build**
(compiled kernels + multithreaded BLAS round per build — cross-machine bit-for-bit is
never promised).

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

Purely additive: new surface only. `set_algorithm_newton()` is unchanged — the
warm-start reuses it at its defaults, so no existing estimator behavior changes
and frozen coefficient baselines are untouched. mirai enters Suggests only. Rollback is reverting the
change's commits. The `dynes-augmentation` trim (tasks 5.x/7.1 removal, spec
delta split, design pointers) happens with this change's proposal so the two
changes never double-claim.

## Open Questions

- **[resolved]** Component names for the three nested constructors under the
  settled `set_algorithm_em()` parent — `set_augmenter_options()`,
  `set_weights_options()`, `set_sgd_options()`, the `set_<component>_options()`
  sub-control pattern (distinct from the algorithm-naming `set_algorithm_<family>()`
  since these are option bundles, not algorithm objects carrying
  `goldfishAlgorithm`). The house convention stays fixed for the parent; the
  components take the options pattern.
- **[resolved]** What `estimate_dynes()` accepts as its specification: a
  `make_joint_specification()` object (`make-multivariate-spec`). The interim
  named-list option is dropped. The loop is still implemented and tested behind a
  minimal spec fixture so this change stays landable before the full data path
  exists, but the public signature is the multivariate spec from the start.
- **[direction set — `dynes-augmentation` D23]** Whether MCMC retained draws
  enter the pool through the proposal evaluator's cache (preprocessed object +
  loglik reuse) — affects the pool-entry path of the D3 adapter.
  `dynes-augmentation` D23 enumerates the alternatives (A0 from-scratch
  re-preprocess; A1 widened `eval_fn` return; A2 + layout adapter; A3
  source-agnostic optional attachment) and sets the direction: **v1 ships A0**
  (the from-scratch path D3 already describes), with **A3 the recommended
  fast-follow**. Reuse is retain-vs-discard, never CPU-worse than A0 — gated not
  on cost but on the one contract crossing this change owns: widening
  `make_proposal_evaluator()`'s return to surface the stats handle (see D3).
  abmcem's v1 obligation is to keep that closure return widen-able and shape the
  pool-entry path so the later attachment is a drop-in; the correctness guard
  (retain the chain's *accepted* state, never the last — possibly rejected —
  `eval_fn` return) lives on the `dynes-augmentation` side of the seam.
- **[defaults]** Package default values for `step_size`, `batch_size`,
  `n_sequences`, and the stop-rule quantiles — set from the toy fixtures'
  behavior during implementation, documented in roxygen. Prototype-derived
  starting points: `batch_size = ⌈K/25⌉`, `step_size = 0.001`,
  `tolerance = 1e-3` (the driver's `epsEnd`).
