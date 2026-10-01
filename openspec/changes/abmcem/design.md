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

- `set_algorithm_em(n_sequences = 100L, max_iterations = 50L, accept_quantile = 0.2,
  growth_quantile = 0.2, stop_quantile = 0.1, tolerance = 1e-3, stop_count = 1L,
  max_retries = 20L, max_pool = 5000L, seed, initial_parameters = NULL,
  em_trace_se = FALSE, n_cores, augmenter = set_augmenter_options(),
  weights = set_weights_options(), optimizer = set_sgd_options())` — the EM loop plus.
  Defaults `max_iterations`/`accept_quantile`/`growth_quantile`/`stop_quantile`/
  `tolerance`/`stop_count` are the prototype driver's `nSteps`/`alpha`/`beta`/
  `gamma`/`epsEnd`/`stopIteration` (Open Questions `[defaults]`, resolved).
  `n_sequences` (the initial pool size) has no prototype default — the prototype
  parameterizes `nChains` from the run — and takes a shipped default of **`100L`**
  for the exported constructor (`set_algorithm_em()` exports here, so its signature
  is user-visible before `estimate_dynes()` does; a required argument with no
  default is not a usable exported surface). **`max_pool`** (default `5000L`) is the
  hard ceiling on the post-acceptance pool-sizing rule (D7): the `Q²`-denominator
  formula is clamped to it and hitting the cap warns once — the cap binding *is*
  the near-optimum `Q → 0` diagnostic.
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
  Newton fits inherit them from the sub-specs. *Note (2026-09-18):* `simulate()`
  reads the same parameter surface but accepts **three** forms — a numeric
  vector, a `goldfishParams`, or a `goldfishParamProvider` from
  `set_parameter_provider()` (`process-simulation` D11). `initial_parameters`
  deliberately stays at the one form: a provider is a *per-step* parameter path,
  meaningful for a generative draw and meaningless for a starting point the
  M-step immediately moves off, and a bare numeric vector would reintroduce the
  positional concatenation `joint-parameters` exists to remove. The narrowing is
  a choice, not an oversight — a provider or a vector here gets an error naming
  `set_parameters()`. `n_sequences` is the **initial
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
  transformation = c("identity", "clipping", "smoothing"), refresh = FALSE,
  ess_threshold = 0.5)` — `transformation` is a **named enum** in v1 (the
  prototypes' `clipping()`/`smoothing()` plus the `identity` default), not a
  user-supplied function: a named choice is validatable at construction
  (monotone, non-negative by construction), snapshot-testable, and lets the
  non-identity-transform-with-resampling warning (D5) fire at construction. A
  user-supplied transformation closure is recorded future work.
- `set_sgd_options(variant = c("minibatch", "full"), batch_size = NULL, batch_scheme =
  c("weighted", "cyclic"), step_size = 0.001, step_schedule = c("constant", "decay",
  "adagrad", "adam", "momentum"), max_iterations, sgd_tolerance = 1e-3,
  convergence = c("gradient", "iterations"))` — the M-step convergence tolerance
  is named **`sgd_tolerance`** (not `tolerance`) to keep it visibly distinct from
  `set_algorithm_em()`'s EM-stop `tolerance`, which lives on a different scale
  (Q-scale vs gradient-norm) — a user setting one must not silently set the
  other. `step_size` default `0.001` is the prototype's `eps_sgd`. `"decay"` is
  the prototypes'
  Bottou schedule, now its own named option; `convergence` selects the
  M-step stopping mode, `"gradient"` the default for both variants (D6).
  `batch_size` is a **fixed absolute count** exposed here alongside the other
  SGD settings; its default `NULL` is resolved **once** by `set_algorithm_em()`
  (which alone knows `n_sequences`) to `min(n_sequences, 10)` and then frozen for
  the whole run (D6). At the `n_sequences = 100L` default this is a batch of 10;
  it floors at `n_sequences` for tiny pools so the batch never exceeds the pool.

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
`estimate_dynes()` carries the lifecycle experimental badge.

**Export deferred to `dynes-augmentation` (Q1 resolution).** The real
augmenters (`mcmc`/`random`/`sim`) land in `dynes-augmentation`; here the only
augmenter is the test-fixture stub (D11). So `estimate_dynes()` is **implemented
and fully tested in this change but NOT exported** (no `@export`, absent from
`NAMESPACE`) — it is exercised through the stub augmenter via
`goldfish:::estimate_dynes()` in tests. The `@export` (and thus the
user-reachable experimental surface) is added by `dynes-augmentation` once a real
augmenter exists, so no user can call an exported function that would only abort
for lack of an augmenter. The control constructors (`set_algorithm_em()` and the
three nested option bundles) still export here — downstream code and docs bind to
the settled signatures now; only the driver waits. *Rejected:* exporting
`estimate_dynes()` here with an informative "augmenters not yet available" abort
on every real call — an exported experimental function that never does real work
is a worse surface than one that is not yet visible.

*Rejected:* one flat constructor (hides the cross-object rules);
overloading `estimate_dynam()` with an `algorithm` switch (different estimand,
different uncertainty semantics).

### D3 — Prototype-path evaluator: zero-iteration engine calls behind the evaluator contract

The v1 implementation of the evaluator contract
(`evaluate_sequence_pool(pool, theta, what = c("loglik", "score", "fisher"))`)
is an internal adapter reproducing the prototypes' pattern:

- **Preprocess once per sequence** (on pool entry): the sequence's data object
  is built and preprocessed through the existing pipeline's **merged joint
  walk** (`preprocess_joint()`), one walk per sequence over the joint spec
  pointed at that sequence's data. The per-fid statistics are stored with the
  sequence. The DyNES target is the **joint single-clock likelihood**: a
  process's own events are dependent events for its fids. A modeled event of
  any other process (another layer or another flavor) is a right-censoring
  boundary for every other timed rate fid and a state change for the choice
  fids. Every timed rate is exposed to the end of the observation window.
  The engine refuses effect-free formulas, so fixed-only fids (completed
  uniform choices, pinned rates) enter the walk through a **pool engine
  spec**, built once at decomposition. That spec swaps each effect-free
  bundle for a parsed bundle of its `pool_engine_formula()`.
  *(Amended 2026-09-25; v1 first shipped per-sub-model preprocessing, see
  Open Questions "The pool evaluator scored each process on its own".)*
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

**Proposal evaluator and rate queries (amended 2026-09-24, `rate-queries`).**
Task 4.3b shipped `make_proposal_evaluator()` with a `rate_queries` argument that
`cli_abort()`ed, naming the capability as missing rather than returning a
fabricated value. The `rate-queries` change (its D4/D6/D7) replaced that abort
with real computation; the named-list `(loglik, rates)` contract pinned here is
unchanged. The landed shape:

- Signature `make_proposal_evaluator(sub_models, theta, spec = NULL,
  control_prep = set_preprocessing())` — `spec` is the **completed joint
  specification**, optional, read only by the rate path.
- At construction, when `spec` is supplied, `assert_theta_layout(spec,
  sub_models)` checks that the sub-models' `theta_index` union equals
  `coef_layout(spec)`'s free `index` set (no position claimed twice), and
  `flat_to_fid_theta()` maps the flat θ onto per-fid vectors once.
- `eval_fn(candidate_seq, rate_queries = NULL)`: with `rate_queries = NULL` it
  returns `rates = NULL` and does no walk work; with queries (a list of
  `list(after = k, layer, flavor, sender, receiver)`) it rebinds the candidate
  via `bind_candidate_data()` and returns `rates` as a numeric vector in the
  caller's query order, computed through `query_rate_at_position()` — one
  `walk_open()` and one forward pass. Queries without `spec` abort naming the
  missing argument; an unresolvable query aborts (classed
  `goldfish_rate_query_unresolved`), outside the loglik `tryCatch`, so it is
  never read as a `-Inf` loglik.
- **Two paths, one closure.** `loglik` comes from the zero-iteration engine per
  sub-model (above); `rates` come from the walk handle, the same `Λ` the
  simulation driver draws with (`dynes-augmentation` D20 invariant 2). They
  agree through `multi-process-walk`'s batch-vs-replay equality; this change
  does not re-prove it.

Two obligations fall on this change's loop and surface (tasks 5.1b/5.2b). First,
`spec` must be the completed spec `estimate_dynes()` produces **once** at entry
(`dynes-augmentation` D21), never re-completed per iteration or candidate — a
re-completion can move the pinned rates between proposals with no parameter to
explain it (`rate-queries` D6). Second, whatever decomposes the joint spec into
`sub_models` must number `theta_index` from `coef_layout(spec)$index` (free
slots in canonical fid order, a timed rate's intercept included); otherwise the
construction check aborts on the first evaluator the loop builds. No such
producer exists yet — `estimate_dynes()` is the first.

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

**MCMC + persistent pool (`routine = "mcmc"`, `refresh = FALSE`).** This is the
Levine & Casella (2001) sample-reuse regime — the sequences drawn at the start
are dragged through the run and reweighted every iteration by the likelihood
ratio against each sequence's own θ_ref (the D5 importance-weight update). Its
default and only valid weighting is therefore `weighting = "importance"`;
`weighting = "uniform"` here **warns and switches to importance** (uniform is
exact only *within* the iteration a pool is drawn, which a persistent pool
leaves immediately — it stays valid only under `refresh = TRUE`, where every
iteration redraws). On this path **both `use = "importance"` and
`use = "resampling"` are valid** — resampling (D8) is the E-step multiset device
layered *on top of* the importance weights, not a substitute for them, so
`mcmc + refresh = FALSE + importance + {importance | resampling}` is an accepted
combination, not warned. (The inert complementary corner —
`use = "resampling"` under `weighting = "uniform"`, i.e. `mcmc + refresh = TRUE`
— resamples a near-uniform multiset: allowed, no effect, neither warned nor
aborted.)

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
proposal density); for MCMC draws the last two coincide. The stored log proposal
density is taken **as reported by the augmenter**: for `sim` draws it is already the
**restart-normalized** (success-conditioned) density — divided by the completion
probability the augmenter estimates as its restart acceptance rate
(`dynes-augmentation` D20) — so the weight machinery consumes it verbatim, never
re-normalizing (a coupling-free draw reports normalizer 1 and is unaffected). Weights stay on the
log scale; cross-iteration reweighting is the likelihood ratio against each
sequence's own θ_ref — the Monte-Carlo-EM sample-reuse importance-weight update
of Levine & Casella (2001), added to `inst/REFERENCES.bib` and cited from the
`set_weights_options()`/`estimate_dynes()` roxygen. **Mixed-θ_ref pools in v1
(the D8 estimator as written): self-normalized importance sampling with
heterogeneous proposals** — each sequence's weight is the likelihood ratio
against *its own* θ_ref, `w̄ᵢ ∝ L(θ)/L_ref,ᵢ · 1/q_ref,ᵢ`, self-normalized
across the pool. This is exact IS, not a mixture-denominator estimator: growth
appends sequences at the current θ (D7), so every persistent pool is mixed-ref
after the first acceptance, and v1 handles that by the per-sequence own-ratio
form. **True multiple importance sampling** (a shared mixture denominator
`Σₖ (mₖ/M)·qₖ` with the balance heuristic) is a **recorded future refinement**,
not v1 — it would redefine `w̄ᵢ` in D8 and propagate into the D9 SE denominators.
The
`transformation` — a **named enum** `c("identity", "clipping", "smoothing")`
(the prototypes' `clipping()`/`smoothing()`), not a user-supplied closure in v1
(D2, Q4 resolution) — applies to raw log-weights before normalization and feeds
both IS estimates and resampling probabilities; a non-identity transformation
with resampling warns (truncated-IS caveat), and because the choice is a
construction-time enum the warning fires in `set_algorithm_em()`, not at
runtime. Two staleness defenses, one code path:

- `refresh = TRUE`: redraw the whole pool every EM iteration at its grown
  size; cross-iteration reweighting, the ESS guard, and staleness
  transformations become inapplicable — non-default settings warn and are
  ignored. **Refresh is the between-iteration rule; within-iteration growth
  still appends.** A grow-retry (D7) inside one iteration **appends** `⌈m/k⌉`
  fresh sequences to the current pool as usual — it does not re-trigger a
  whole-pool redraw. The whole-pool redraw happens once per iteration, at the
  *start* of the next iteration, at the size growth has accumulated to. So the
  two loops compose cleanly: growth sets the target size within an iteration;
  refresh regenerates the whole pool at that size when the next iteration opens.
- `refresh = FALSE`: ESS = 1/Σw̄² is computed in the weighting machinery from
  the normalized weights once per EM pass, before the M-step. A **full redraw**
  fires when ESS < `ess_threshold` × K (default 0.5): the augmenter regenerates
  all K sequences at the current θ, so their θ_ref ← θ and the weights are
  ~uniform again — the stale dragged pool is discarded, not reweighted. A redraw
  also **resets** the harmonic-growth counter k to 2 (D7), since the fresh pool
  restarts the grow schedule, and it consumes a fresh `draw_index` range (D10).
  Warn when the guard fires (new draws generated) and when it is disabled but the
  condition is met; never at startup.

**Runtime ESS-collapse recommendation (dynes-augmentation D14/#5 resolution).**
Distinct from the redraw guard above (which regenerates the pool) and from the
startup cold-start diagnostic (below, a bad-θ₀ corner): when ESS collapses
**mid-run** under an importance-weighted augmenter (`routine = "random"`/`"sim"`)
because flips-per-interval is high — the IS-weight variance grows ~exponentially in
the sequence-space dimension, so no redraw at any θ recovers it — the loop emits an
actionable `cli` recommendation to **switch to `augment_seq_mcmc()`** (which does not
pay the IS-dimension tax). **This change owns the message surface** (the recommendation
fires here, in the per-iteration ESS diagnostics); **`dynes-augmentation` owns the
numeric collapse threshold** — the ESS floor that means "switch now" comes from its
6.2/6.3 degeneracy study (the flips-per-interval cliff), not guessed here — so the two
cannot drift. The recommendation only informs; it does not itself switch augmenters.

**Restart-coupling warning is deduplicated by the loop (dynes-augmentation D20).** The
`sim` augmenter's reject-and-restart fires inside augmenter draws this loop drives; the
augmenter reports that a restart occurred, but the **loop owns the run-level dedup
state** so that **exactly one** warning is emitted per estimation run (the first time
any interval restarts, at any iteration or draw), naming the coupled constraint and its
plug-in-normalizer bias — never one warning per iteration or per draw. A coupling-free
run (no restart ever fires) emits none.

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
- **Batch size**: `batch_size` (a `set_sgd_options()` argument, alongside the
  other SGD settings) is a **fixed absolute count held constant across EM
  iterations** — as the pool K grows (harmonic on rejection, the power rule on
  acceptance, D7) the batch/pool ratio shrinks by design; the batch count never
  tracks K. Its default `NULL` is resolved **once**, at construction, by
  `set_algorithm_em()` (the only object that knows `n_sequences`) to
  `min(n_sequences, 10)` and then frozen for the run; an explicit `batch_size` is
  used verbatim. This compute-once default replaces the prototype's per-iteration
  `⌈K/25⌉` recomputation, which would have made the batch pool-relative; the
  `min(·, n_sequences)` floor keeps the batch from exceeding a tiny initial pool.
  At a single-digit `batch_size` the minibatch gradient-norm test runs on the EMA
  of batch gradients (not one noisy batch), noted in the `set_sgd_options()` roxygen.
- **Step-size schedules**: `"constant"` (default), `"decay"` (the prototypes'
  Bottou `η₀ / (1 + η₀·t)` with `step_size` = η₀), and AdaGrad / Adam /
  momentum at fixed literature defaults, hyperparameters not exposed. (The
  previous framing of Bottou decay as "the constant schedule's
  implementation" was a contradiction — decay is its own schedule.)
- **Convergence**: `convergence = c("gradient", "iterations")`. The default,
  `"gradient"` (both variants), tests the **gradient norm** against
  `sgd_tolerance` (D2, Q5) — never the step norm (a step-norm test fires from step-size
  decay alone, regardless of the gradient; relative parameter change is a
  recorded future alternative). Under `variant = "full"` the tested gradient
  is exact; under `"minibatch"` a single batch gradient is too noisy to test
  (its norm has a sampling-noise floor even at the exact optimum), so the
  test statistic is an exponential moving average of batch gradients, with
  the smoothing constant fixed internally (consistent with the
  no-exposed-hyperparameters rule). `"iterations"` is the opt-out fixed
  budget: run to `max_iterations` with `sgd_tolerance` unused (supplying it
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
  - **Warm-start draw RNG key (D10 interaction).** The single warm-start
    augmentation is drawn **at `iteration = 0`** in the D10 `(iteration,
    draw_index)` augmentation keying — a distinct iteration index from the
    iteration-1 pool, so the warm-start draw owns its own substream range and can
    never collide with (or reorder against) the first pool draw. `draw_index`
    starts fresh at iteration 1 for the initial pool. This keeps the two-part
    reproducibility promise (D10) intact for the warm-start path: the warm-start
    sequence, like every drawn sequence, is identical across any `n_cores`.
  - **Warm-start fit failure (Q2 resolution).** A single drawn sequence is
    exactly where a per-fid Newton fit is likely to hit a singular information
    matrix (the engine's unconditional inversion `stop()`s, as in D3). A per-fid
    fit that errors therefore **falls back to θ₀ = 0 over that fid's free set**
    with a cli **warning** naming the fid — never aborting the run — consistent
    with the "warm start supplies only a closer θ₀, need not converge" framing;
    the other fids keep their fitted starts. The per-fid engine call is wrapped
    in `tryCatch` for this, mirroring the evaluator's D-ROBUST guard.
- The loop runs in **passes** — an EM iteration is one initial pass plus up
  to `max_retries` grow-retries:

  ```
  per pass:
    M-step from θ_k (last accepted) on the current pool → θ′
    full-pool log-likelihood pass at θ′ (the θ_k values are cached from the
      previous iteration, never recomputed)
    Λ_i = Σ_submodels [L_i(θ′) − L_i(θ_k)];  Q, ASE per D8 (ASE floored at 0)
    1. STOP:   |Q + z(stop_quantile)·ASE| < tolerance ?
         hit  → streak++ ; if streak == stop_count → terminate, return θ_k
                else FALL THROUGH to ACCEPT on the same θ′ (θ keeps moving)
         miss → streak ← 0 ; FALL THROUGH to ACCEPT
    2. ACCEPT: Q − z(accept_quantile)·ASE > 0 ?
         yes → θ_{k+1} = θ′; cache L(θ′) as the next reference; size the
               next pool m ← min(max_pool,
                                  max(m, ⌈σ̂²·(z(accept) + z(growth))²/Q²⌉))
         no  → GROW: append ⌈m/k⌉ sequences (m = pool size at iteration
               start, k = 2, 3, … per consecutive rejection), discard θ′,
               retry from the M-step
  ```

- **Stop checked before accept**, deliberately: near the optimum θ′ ≈ θ_k
  drives Q and ASE to zero together and the accept bound fails ~half the
  time regardless of pool size; a stop rule reachable only after an
  acceptance would deadlock into growth until `max_retries` aborts — at the
  exact moment of convergence.
- **A non-terminal stop-hit falls through to ACCEPT on the same θ′ — θ keeps
  moving.** When `stop_count > 1`, a pass can satisfy the stopping bound
  (`streak++`) without terminating; it then still evaluates the accept test on
  that same θ′, so an accepted stop-hit pass advances θ_{k+1} = θ′ and re-caches
  the reference. The consecutive-hit streak is therefore measured across passes
  whose θ is *moving*, not frozen at the first hit — the loop keeps taking ascent
  steps while it accumulates the confirmations `stop_count` requires. (At the
  default `stop_count = 1L` the first hit terminates and this is moot.)
- **`max_pool` clamp (near-optimum `Q → 0` guard).** The post-acceptance sizing
  rule has `Q²` in the denominator, so a small-but-positive Q that clears the
  accept bound without clearing the stop bound can request an astronomical pool.
  The result is clamped to `max_pool` (default `5000L`, D2); the **first** pass
  that binds the cap emits a one-time cli warning — the cap binding is itself the
  diagnostic that the loop is sitting near the optimum with a tiny Q (typically a
  sign `tolerance` wants relaxing, or `stop_count` lowering). The clamp never
  *shrinks* the pool; it only bounds the requested growth.
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
    `cli_abort()` with the trace-derived diagnosis, **carrying the accumulated
    `em_trace` as structured condition data** (retrievable via
    `rlang::last_error()$em_trace`) so the accepted-iteration history and the
    stalled retries stay inspectable. There is no *returnable fit* at a stalled
    iteration — hence the abort, not a `converged = FALSE` return — but the trace
    is not thrown away.
  - **`max_iterations`** (the *outer* budget is exhausted mid-ascent):
    **return the last accepted θ** carrying a **non-convergence warning** and a
    `converged = FALSE` flag on the result — never silent — mirroring how the
    goldfish Newton estimators return an unconverged fit rather than discarding
    the work. `summary()`/`print()` surface the non-convergence prominently
    (D9). Throwing away many accepted ascent steps at a budget boundary is the
    behavior this deliberately avoids.
- **`em_trace`**: **one row per pass**, indexed (iteration, retry) — θ, Q,
  ASE, `ase_floored` (whether the variance estimate was floored at 0, D8),
  decision (accept / grow / stop-hit / stop), streak, pool size, new
  draws, `pool_capped` (whether the sizing rule bound `max_pool`), ESS,
  weighting scheme in force, MCMC chain diagnostics when supplied
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

**Non-negative-variance floor.** The resampling σ̂² above is a sample estimate
whose bracket `[ΣcᵢΛᵢ²/(ΣcᵢΛᵢ)² − 1/K]` can go **negative** — precisely when the
Λᵢ are near-constant across the multiset, i.e. the low-variance, near-convergence
regime. `compute_ase()` therefore **floors σ̂² at 0** (so `ASE = 0`) rather than
returning `NaN` from `sqrt(negative)`: a negative estimate there means the variance
is below the resolution floor, not a pathology. `ASE = 0` degrades both decision
bounds to the honest limit — STOP becomes `|Q| < tolerance`, ACCEPT becomes
`Q > 0` — instead of poisoning every comparison with `NaN` and silently forcing
growth to `max_retries`. The importance ASE² (a sum of squares) cannot go
negative, so only the resampling form needs the floor. Each pass that floors
records an `ase_floored = TRUE` flag in `em_trace` for auditability.

The prototype's "not enough unique samples" patch (forcing distinct indices
into a collapsed resample) is dropped — weight degeneracy is the ESS guard's
job (D5). Under MCMC draws the ASE treats sequences as independent given the
thinning; an autocorrelation index lands in `em_trace` with a user-facing
suggestion to raise `thinning` when high.

### D9 — Result contract: two covariance estimators from one final full-pool pass (from dynes-augmentation D7, refined)

After termination, **one dedicated full-pool evaluation at the returned θ̂**
supplies per-sequence scores sᵢ and information matrices Iᵢ — SEs are never
harvested from the last minibatch (the prototype's `sgd_refactor()` did
exactly that). **This SE pass runs on every *returning* termination regardless
of the `converged` flag (Q3 resolution)** — including the `max_iterations`
exhaustion path (D7), which returns the last-accepted θ̂ with `converged =
FALSE`. So a returned fit always carries a populated `vcov()`/MC error; a
non-converged result has SEs (which `summary()` caveats alongside the
`converged = FALSE` status), never a `NULL` covariance. Only the `max_retries`
abort path (D7) produces no return and thus no SE pass — there is no θ̂ to
evaluate at. With I = Σ w̄ᵢIᵢ and S̄ = Σ w̄ᵢsᵢ, two estimators ship:

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

**The result class arrives with its verdicts recorded**
(`fit-class-hierarchy`, ADR-0038, ADR-0050 and ADR-0052). Every fitted-model
class carries the shared parent `goldfishBaseFit`, and the
`fit-class-hierarchy` capability in `openspec/specs/` records one verdict —
`inherit`, `override` or `refuse` — for every generic dispatching on a fit
class. That record is a **design-time obligation, not a test fixture**: this
change states its verdicts in that capability's spec before writing the
class, which is the point at which the question can still be argued. What a
test does check is reachability — a generic registered on any fit surface
must be reachable for every fit class — so a class that simply inherits
everything passes it while still owing the argument.

Two rows are decided by the paragraphs above rather than left open. `coef()`
and `vcov()` are `override` (flat, composite-labelled, MC error carried
alongside), and `coef_layout()` is `override` — the DyNES result is a joint
surface, so unlike a single-process fit it *has* per-process blocks to lay
out.

*Added 2026-09-18 — a row that did not exist when this was written.*
`process-simulation` made **`simulate`** a generic dispatching on fit classes
(`override` on `goldfishFit` and `goldfishFlavFit`; its `fit-class-hierarchy`
delta adds the row). The requirement it modifies obliges *any* change adding a
fit class to record a verdict for every such generic, so this change now owes
one. The verdict is **`override`**, for a reason specific to this estimator
rather than inherited: `simulate.goldfishFit()` rebuilds a specification out of
the fit's formula, model and sub-model and demands the data as an argument,
because a single-process fit stores neither its specification nor its stocnet.
A DyNES result stores **both** — it was fitted *from* a `goldfishJointSpec` and
carries the panel waves — so the parent's method would ask the user for data the
result is already holding, and would rebuild a specification from formulas that
are only one fid's half of a joint one. The override passes the held joint spec
and θ̂ (as the `goldfishParams` of `joint-parameters`) straight to the driver.
This is not hypothetical plumbing: `dynes-augmentation`'s recovery study (its
task 6.2) draws its panels through exactly this path, so the fit → re-simulate
round trip is on the validation path of the pair of changes.

The row that needs a decision made rather than read off is **`logLik`**:
this estimator maximizes a Monte-Carlo estimate of the observed-data
likelihood, so an inherited exact-likelihood `logLik()` would let `AIC()` and
`BIC()` return numbers that look like information criteria and are not. That
verdict was deliberately left to this change — `fit-class-hierarchy` D8 —
because deciding it there would have meant deciding this estimator's
semantics before the estimator existed. If the verdict is `refuse`, the
message says the quantity is a Monte-Carlo estimate and names what to use
instead.

### D10 — Parallel map seam: sequence-level, serial default (from dynes-augmentation D10)

One internal map seam for every per-sequence loop (pool preprocessing on
entry, pool evaluation): default serial `lapply()`; when mirai (Suggests) is
installed and `n_cores > 1`, persistent daemons hold the data once and receive
only θ and draws per iteration. **mirai-absent fallback (Q6 resolution):** a
requested `n_cores > 1` with mirai not installed emits a **one-time** cli
warning (the requested parallelism is unavailable; install mirai) and runs
serial — the run still succeeds and is bit-identical to the parallel path (D10
RNG). The **default** `n_cores` path stays silent with no reference to the
missing package (the serial-works-without-mirai scenario); only a user's
request is warned. Governing invariant: workers × BLAS threads ≤
cores (`blas_threads = max(1, floor(n_cores / n_workers))`). `n_cores` lives
on `set_algorithm_em()` (default `1L`, serial; see the "Parallel only on an
explicit `n_cores`" verdict under Open Questions) with parallel RNG
streams. FORK/`mclapply` excluded; PSOCK rejected (the prototype's
`clusterExport` tax across hundreds of iterations is the anti-pattern this
seam removes). MCMC chain generation stays serial by nature — the seam covers
draws and evaluation, not the chain.

**Resident pool store (added 2026-09-28, task 6.2).** "Hold the data once" is
a pool store keyed by sequence id, taking exactly the two mutations the loop
makes (D7): **append** a chunk (growth, a post-acceptance top-up) or
**replace** the whole pool (the first pool, a `refresh` or ESS-guard redraw).
A replace clears the old entries, so memory stays (1 + n_workers) x pool: the
coordinator's own copy (weights, reference records, the D9 covariance pass)
plus one resident copy per worker. On daemons each mutation is a
`mirai::everywhere()` with `.min = n_workers`; every daemon's result is
collected and checked, and a failed or missing daemon aborts
(`goldfish_dynes_broadcast_failed`). The serial path runs the same resident
functions over a store in this process, so both paths are one code path. The
static header (sub-models with their coefficient masks, `theta_length`, the
preprocessing control) crosses the seam once, when `estimate_dynes()` opens
the store right after the seam. The masks are read from the completed spec's
own data, since the coefficient layout depends on the formulas and not on the
drawn events. Each mutation advances a **pool generation**; the
coordinator's pool records it, every evaluation task carries it, and the
resident store aborts on a mismatch (`goldfish_dynes_stale_pool`), so a pool
object captured before an append or replace can never be scored against
sequences it does not list. An evaluation task is built from the store key,
the generation and θ, and maps over sequence ids, so its payload does not
depend on K. The fit drops the closed store handle, so a serial and a parallel
fit are identical objects. Evaluation stays deterministic, so the RNG keying
above is untouched.

**RNG reproducibility (Option B — resolved with dynes-augmentation).** The single
`seed` seeds an `L'Ecuyer-CMRG` root (`RNGkind()` set at estimation entry,
`withr`-restored on exit) fanning into three substream families: **augmentation**
substreams keyed by a structured `(iteration, draw_index)` — worker identity never
enters the key, so the drawn pool is identical at any `n_cores`, serial included.
`draw_index` is a **strictly monotonic per-iteration counter**, incremented once
for every sequence the iteration draws — the initial pool, each growth append
(D7), and each full redraw (ESS guard or `refresh`, D5) alike — never rewinding
within an iteration and resetting only when `iteration` advances; a redraw
therefore consumes a **fresh** `draw_index` range rather than reusing the
discarded pool's, so it cannot reproduce the collapsed sequences it replaces.
Because every logical draw thus owns one immutable substream key regardless of
how retries chunk it or which worker runs it, the **grown and redrawn** pool is
bit-identical across `n_cores`, not only the no-growth pool.
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
- **[Proposal evaluator's loglik and rates use different compiles]** (added
  2026-09-24) → the loglik path is per-sub-model `estimate_wrapper()`, the rate
  path the merged walk; they agree through `multi-process-walk`'s
  batch-vs-replay equality. *Amended 2026-09-28:* task 6.3 moves the loglik
  onto the joint likelihood, per-fid engine calls over the stats of one
  `preprocess_joint()` walk of the pool engine spec, so both paths then read the
  merged walk. They still compile separately (`loglik` and `rates` stay two
  paths), and test 6.3a(viii) pins the closure's `loglik` to the pool-entry
  reference at 1e-10. A rate query also pays a second compile per proposed
  move (`walk_open()` recompiles every formula); caching a live handle across a
  chain's proposals belongs to `augment_seq_mcmc()`'s driver (`rate-queries` D5)
  and is the first place to look if profiling finds it dominant.

- **[Task payload silently grows with the pool]** (added 2026-09-28) → a task
  closure must not reach the pool, and a closure can reach it without naming
  it: a forced promise in the task constructor's frame serialized with its
  caller's frame, so the first store-backed evaluation task still shipped the
  whole pool (369 KB against 1.2 KB on the toy fixture; the same held for the
  pool-entry task). The task constructors rebind their arguments as plain
  values, and `test-pool_store_dynes.R` pins the payload as independent of K
  and free of `stats`/`data`. The serial path serializes nothing, so only a
  recording seam (or real daemons) can see this class of regression.

## Migration Plan

Purely additive: new surface only. `set_algorithm_newton()` is unchanged — the
warm-start reuses it at its defaults, so no existing estimator behavior changes
and frozen coefficient baselines are untouched. Dependencies: mirai and
pkgload enter Suggests, and `parallel` (base R) moves from Suggests to Imports
for the RNG substream keying. Rollback is reverting the
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
- **[resolved]** Package default values — **pinned from the prototype driver**
  (Q7): `step_size = 0.001` (`eps_sgd`), `sgd_tolerance = 1e-3` and EM
  `tolerance = 1e-3` (`epsEnd`), `accept_quantile = 0.2` / `growth_quantile =
  0.2` (`alpha` / `beta`), `stop_quantile = 0.1` (`gamma`), `max_iterations =
  50` (`nSteps`), `stop_count = 1L` (`stopIteration`), `max_retries = 20L`. Only
  `n_sequences` has **no** prototype default — the prototype parameterizes
  `nChains` from the run — so it takes a shipped default of **`100L`** for the
  exported `set_algorithm_em()` (an exported constructor cannot require it), the
  toy fixtures overriding it as needed.
- **[resolved]** Near-optimum `Q -> 0` blow-up of the post-acceptance pool-sizing
  rule → a **`max_pool` argument on `set_algorithm_em()` (default `5000L`)** clamps
  the requested size (D7); the first pass to bind the cap warns once. The corrected
  sizing rule never ran in the prototype (sign flip), so both rule and clamp are
  validated by the toy fixtures.
- **[resolved]** Negative resampling variance estimate → `NaN` ASE poisoning the
  decision bounds. `compute_ase()` **floors the variance at 0** (D8): a negative
  estimate is the low-variance regime, and `ASE = 0` degrades the bounds to
  `|Q| < tolerance` / `Q > 0` rather than forcing spurious growth. Recorded per
  pass as `ase_floored` in `em_trace`.
- **[resolved]** `stop_count > 1` control flow — a **non-terminal stop-hit falls
  through to ACCEPT on the same proposal, so θ keeps moving** (D7); the
  consecutive-hit streak is measured across passes whose θ advances, not frozen at
  the first hit.
- **[resolved]** Warm-start single draw vs. the D10 `(iteration, draw_index)`
  keying — the warm-start augmentation is drawn at **`iteration = 0`** (D7), a
  distinct substream range from the iteration-1 pool, so it cannot collide with or
  reorder against the first pool draw.
- **[resolved]** `refresh = TRUE` vs. within-iteration growth — **growth appends,
  refresh redraws between iterations** (D5): a grow-retry appends within the
  iteration; the whole-pool redraw fires once at the next iteration's start.
- **[resolved]** `batch_size` — a **fixed absolute count** exposed on
  `set_sgd_options()`, held constant across iterations (D6); its default `NULL`
  is filled once by `set_algorithm_em()` to **`min(n_sequences, 10)`** and frozen
  (a batch of 10 at the `n_sequences = 100L` default, floored at `n_sequences` for
  tiny pools; the prototype's per-iteration `⌈K/25⌉` is not carried forward).
- **[resolved 2026-09-25 — allow fixed-only sub-models, as a θ-free per-path term]**
  `new_pool_submodel()` accepts a zero-length `theta_index`, and a fixed-only fid is
  evaluated at its fixed values with zero optimizer iterations and no score or Fisher
  block. A multi-wave pinned rate is **not** one fixed value: it has one plateau
  `intercept_w` per wave period, and the engine carries one intercept coefficient per
  rate fid. It is therefore evaluated through a **per-period offset column**: the
  intercept column holds `c_{w(e)}` for the period containing each interval
  (`intercept_only_rate_period()`, read from `spec$completed_rates`, the same table the
  walk's `rate_queries` read), with its coefficient fixed at 1. This is exact only
  when no interval straddles a wave boundary, which the preprocess must guarantee
  (per-wave windows or an injected interior-wave boundary; `dynes-augmentation` D5
  and D20 invariant 2). That includes the closure's per-candidate preprocess.
  *Amended 2026-09-25 (task 5.1b):* the guarantee is met analytically. The risk
  set is constant within an engine interval, so `pinned_rate_compensator()` cuts
  a straddling interval's exposure at each interior wave time and scores each
  piece at its own plateau. That equals an event injected on the boundary, needs
  no re-preprocess, and applies at pool entry and in the closure alike. The
  earlier `goldfish_pinned_rate_straddle` abort is retired, so a candidate the
  closure scored can never abort at pool entry. A
  single-plateau pin is the K = 1 case of the same column. The same rule covers every
  fixed-only fid, whoever fixed it: an authored fid whose effects are all `offset()`,
  and a completed uniform choice (its `−log |support(t)|` also moves with the path as
  the legal set changes). Authored and completed fids differ only in how
  `coef_layout()` reports them, not in how they are scored. Its log-likelihood is a
  θ-free per-path term. It enters the recorded proposal density `q` (`sim`'s draw density; MCMC's
  forward/reverse proposal densities through `rate_queries`), since `q` must be the
  density actually drawn from. It also enters the target `f` wherever **paths** are
  compared: `make_proposal_evaluator()`'s `loglik` (the acceptance ratio `f′/f`) and
  `evaluate_sequence_pool()`'s stored reference log-likelihood (the pool's importance
  weights, normalized across paths). Leaving it out of `f` there would weight toward a
  posterior different from the one `q` drew under. Where only **θ** varies on a fixed
  path, it drops out on its own: it cancels in the D14 reweight
  `f(Ω;θ)/f(Ω;θ_ref)` and in the ascent test's ΔQ, and contributes nothing to Q's
  score or Fisher information, so the M-step never sees it. Because it is θ-free, it
  is computed once per sequence at draw time and never recomputed at a new θ.
  "Argue cancellation" is rejected: the term is not candidate-invariant (the `|R(t)|`
  compensator moves with the latent path; `dynes-augmentation` D21 and its resolved
  open question). Constrains task 5.2's decomposition: a fixed-only fid becomes a
  sub-model with a log-likelihood and no `theta_index` slots.

  The question as originally raised: fixed-only fids and the
  log-likelihood target. `new_pool_submodel()` (task 4.1b) requires a non-empty
  `theta_index`, so a fid with no free slot — a completed intercept-only rate, or an
  authored fid whose effects are all `offset()` — cannot be a sub-model. The
  log-likelihood half of `make_proposal_evaluator()` and `evaluate_sequence_pool()`
  (the E-step weights) then omits that fid's density, while the `rate_queries` half
  evaluates it at its pinned/fixed values through the walk. The omission is harmless
  only if the term is constant across candidates; it is not in general: a pinned rate
  bucketed on `wave_times` (`dynes-augmentation` D21) or a changing composition makes a
  timed rate's density depend on where augmented events fall, so f(Ω′)/f(Ω) and the
  importance weights would be missing a factor. Options:
  - **Allow fixed-only sub-models** — relax `new_pool_submodel()` to accept a
    zero-length `theta_index` and evaluate the fid at its fixed values (a pinned
    intercept passed as an offset / fixed initial value, zero optimizer iterations,
    no score or Fisher block). Keeps f complete; needs a check that the engine accepts
    an all-fixed parameter vector.
  - **Argue cancellation** — state in the design which fixed-only fids provably
    contribute a candidate-invariant term, and abort at decomposition for any that do
    not.
  Coupled with `dynes-augmentation`'s open question on whether a completed fid's pin
  is estimated. Resolve before task 5.2 decomposes the joint spec into `sub_models`.

*Added 2026-09-25, from task 5.2b.* The surface landed with the gaps and choices
below. Each needs a review verdict (accept as-is, amend the spec, or schedule the
work) before `abmcem` lands.

- **[resolved 2026-09-25: scheduled as task 6.1] mirai path never run.** mirai is not installed on the development
  machine, so `dynes_daemons_start()` / `dynes_parallel_map()` /
  `dynes_daemons_stop()` (`R/map_seam_dynes.R`) have never run. The
  cross-`n_cores` tests stand in for them with an in-process map that returns
  its results in reverse order, which proves the keying and the index-ordered
  reduction but not the daemon plumbing. A real-daemon test (`skip_if_not_installed("mirai")`,
  `skip_on_cran()`) is still owed.
  *Update 2026-09-25:* mirai 2.7.3 is now installed and the plumbing was run
  by hand from a `load_all()` session: daemons start under their own compute
  profile, `mirai_map()` results come back in input order, a worker error
  surfaces through `[mirai::.stop]`, and `daemons(0)` stops them. The
  real-task test is still owed and is **blocked by the next item**: from a dev
  tree it fails on the first goldfish-internal task. Unblocked by that item's
  verdict (A); the test is scheduled as task 6.1.
  *Closed 2026-09-28 (task 6.1):* the real-daemon test now runs in the
  `NOT_CRAN=true` gate.
- **[resolved 2026-09-25: A] Daemons load the installed goldfish.** Workers run
  `loadNamespace("goldfish")`, i.e. the *installed* version, not a
  `load_all()` development tree. So a parallel run from a dev session evaluates
  with stale code, or fails if the installed version predates these internals.
  Options: `pkgload::load_all()` on the daemons when goldfish is a dev package,
  or document "install before running in parallel".
  *Reproduced 2026-09-25:* with 1.9.31 loaded via `load_all()` and 1.9.28
  installed, a plain closure (`function(i) i^2`) maps correctly, but a
  `keyed_draw_task()` closure fails on the daemons with `could not find
  function "set_rng_state"`. A goldfish closure's environment serializes as a
  *reference* to the `goldfish` namespace, which each worker resolves to the
  installed version. So the owed real-daemon test would fail under
  `devtools::test()` / the `NOT_CRAN=true` gate and pass only under
  `R CMD check`, which installs first. Options:
  - **A (proposed):** in `dynes_daemons_start()`, when
    `pkgload::is_dev_package("goldfish")`, run `pkgload::load_all(<path>)` on
    the daemons (likely `compile = FALSE`, reusing the built `.so`, so
    concurrent daemons do not race a compile), and `loadNamespace()`
    otherwise. The real-daemon test then runs in the dev loop. Costs a few
    seconds per daemon start; pkgload goes into Suggests (today it arrives
    only through devtools).
  - **B:** the test does `skip_if(pkgload::is_dev_package("goldfish"))`, so it
    runs only under `R CMD check`; the daemon path stays unchecked day to day.
  - **C:** document "install before running in parallel"; same coverage as B,
    and a trap for anyone developing.
  **Verdict (2026-09-25): A.** `dynes_daemons_start()` runs
  `pkgload::load_all(<path>, compile = FALSE)` on the daemons when
  `pkgload::is_dev_package("goldfish")`, and `loadNamespace("goldfish")`
  otherwise; pkgload enters Suggests. B is rejected because it leaves the
  daemon path out of the `NOT_CRAN=true` gate every commit runs; C for the
  same reason, plus the silent stale-code trap. Scheduled as task 6.1.
  Side note: `dynes_map_seam()` sets `n_workers <- n_cores`, so
  `blas_threads = n_cores %/% n_workers` is always 1. The workers-times-BLAS
  cap holds, but a split such as 2 workers x 2 BLAS threads never happens; a
  known v1 simplification.
- **[resolved 2026-09-25: broadcast-resident] No daemon-resident data (spec
  deviation).** The D10 requirement
  says daemons "receive the data once and only parameters and draws per
  iteration". v1 does not: each `evaluate_sequence_pool()` call ships every
  sequence again, one task per sequence. The tasks close over the sub-models
  only (`pool_entry_task()`, `pool_evaluation_task()`), so a task carries one
  sequence rather than the whole pool, but per-iteration traffic still scales
  with pool size. Keeping pool sequences on the daemons (`mirai::everywhere()`
  keyed by sequence id, kept in sync across growth and redraw) is the fix;
  alternatively amend the spec to per-call dispatch for v1.
  *Correction, measured 2026-09-25:* the "one sequence rather than the whole
  pool" claim above is wrong for `pool_evaluation_task()`. It extracts
  `header` from `pool`, but the forced `pool` argument stays in the closure
  environment and is serialized with it: on the toy fixture the task payload is
  0.97 / 1.59 / 2.83 MB at K = 2 / 4 / 8, and mirai ships `.f` with every
  mirai, so one full-pool evaluation moves traffic quadratic in K. The serial
  path serializes nothing, so no test sees it. (`pool_entry_task()` is clean:
  287 KB, sub-models and control only.) One built sequence is ~416 KB, of which
  `stats` is ~364 KB, on a 6-node toy; realistic panels scale `stats` with
  events x actors, so even without the bug per-call dispatch re-sends the
  dominant payload on every SGD batch.
  **Verdict (2026-09-25): broadcast-resident**, for realistic panel sizes.
  - A pool store keyed by sequence id, with exactly the two mutations
    `draw_into_pool()` already performs: **append** a chunk, or **replace**
    the whole pool. On daemons both go through `mirai::everywhere()`, so every
    daemon holds the full pool; the serial path implements the same store API
    over a local environment, so both paths run one code path.
  - The static header (sub-models, `theta_length`, control) is broadcast once
    when the seam opens.
  - Evaluation tasks carry only sequence ids and θ; their payload does not
    depend on pool size. Evaluation stays deterministic, so the RNG keying is
    untouched.
  - The main process keeps its own copy of the pool for the weights and
    reference bookkeeping and the final `fit_dynes()` evaluation.
  - Cost: memory (1 + n_workers) x pool, the usual trade for resident workers.
  - Rejected: **sharded-resident** (one daemon per shard) because mirai's
    dispatcher cannot route a task to a chosen daemon, so sharding needs one
    compute profile per daemon plus our own routing and load balancing;
    recorded future refinement if memory binds. **Per-call dispatch** for v1
    because at realistic sizes the parallel path would likely be slower than
    serial.
  Scheduled as task 6.2; the spec gains the "pool data reaches the daemons
  once" scenario.
  *Closed 2026-09-28 (task 6.2):* implemented as the resident pool store in
  D10, with the generation counter on serial and daemon stores alike. The
  quadratic traffic measured above had a second cause beyond the forced `pool`
  argument: any forced promise in a task constructor's frame carries the
  caller's frame, which is why the constructors now rebind their arguments
  (Risks, "task payload silently grows with the pool").
- **[resolved 2026-09-25: Imports] `parallel` moved to Imports.** The L'Ecuyer-CMRG stream jumps need
  `parallel::nextRNGStream()`/`nextRNGSubStream()`, so `parallel` moved from
  Suggests to Imports. It ships with base R (no install cost), but it is a
  DESCRIPTION dependency change the proposal's Impact section does not list.
  **Verdict (2026-09-25): keep it in Imports.** `estimate_dynes()` builds the
  substream keyer on every run, the serial default included: the keying is what
  makes the drawn pool identical at any `n_cores`, so it is not a parallel-only
  path. The earlier Suggests placement (commit 280c51e) rested on test helpers
  being the only consumer, and on Imports forcing an install. The first no
  longer holds, and the second never did for a base package. Every call is
  `parallel::` with no NAMESPACE import, so the namespace loads lazily on first
  use and `R CMD check` sees the use. *Rejected:* Suggests with a
  `requireNamespace()` guard, because CRAN requires conditional use of
  Suggests and the serial path has no fallback short of re-implementing the
  CMRG jump matrices. The `requireNamespace("parallel")` guards in
  `tests/testthat/helper-baselines.R` are now always true; they stay, since
  they are harmless test code. The proposal's Impact section now lists the
  move.
- **[resolved 2026-09-25: default `n_cores = 1L`] Parallel only on an explicit
  `n_cores`.** The default `n_cores = 2L`
  runs serially; only an explicit `n_cores > 1` asks for daemons, matching the
  existing `set_algorithm_em()` roxygen. D10 reads as if the default could be
  parallel. Confirm that serial-by-default is the intended reading.
  **Verdict (2026-09-25): serial by default, expressed as `n_cores = 1L`.**
  Serial is the right default: under broadcast-resident pool data, opening
  the seam costs daemon start-up (a `load_all()` per daemon in a dev tree) and
  (1 + n_workers) x pool memory, which small and example-sized runs do not
  repay; and D10 makes the estimates identical at any `n_cores`, so the
  default decides only speed and resources. The v1 encoding, though (a
  default of `2L` plus a `missing()`-derived `n_cores_explicit` flag), made
  `set_algorithm_em()` and `set_algorithm_em(n_cores = 2L)` equal in value but
  different in behavior, and the flag was lost silently whenever a control was
  copied or rebuilt (`modifyList()`, reconstruction from a stored fit), while
  a fit recorded `n_cores = 2` for a serial run. With the default at `1L`, any
  value above 1 was set by the user by construction, so the seam tests only
  `n_cores > 1`, the "warn only on an explicit request" rule holds without the
  flag, and the recorded value matches how the run executed. `1L` is within
  CRAN's two-core check limit. *Rejected:* keeping `2L` plus the flag, for
  the value/behavior split above. Scheduled as task 6.5; the spec gains the
  "default core count runs serially" scenario.
- **[resolved 2026-09-25: joint likelihood] The pool evaluator scored each
  process on its own.** Found while checking the flavored fix: pool entry
  preprocessed each sub-model separately
  (`estimate_wrapper(..., preprocessing_only = TRUE)`), so no fid saw the other
  processes' events as censoring boundaries. On `dynes_multiwave_spec()` (no
  flavors), compared with the joint walk:
  - the pinned friendship rate ended at its own last event (t = 4) instead of
    the window end (t = 5), so the fixed-only log-likelihood was off by exactly
    1.0 (rate 1 x the missing [4, 5] interval)
  - the calls rate had 5 intervals instead of 8, with no censoring rows at the
    friendship events

  Every `estimate_dynes()` input is a joint spec, and no other goldfish path
  estimates one, so every DyNES run so far evaluated the per-process
  likelihood. **Verdict:** the target is the joint single-clock likelihood
  with cross-process right-censoring, the contract the merged walk implements
  (D3 amended). This is a correctness change to every DyNES likelihood, not
  only to flavored specs. Frozen coefficient baselines are not involved (no
  baseline covers a joint spec), but DyNES test expectations computed on the
  old path may shift. Scheduled as task 6.3.
- **[resolved 2026-09-25: lift in abmcem, via the joint likelihood] Flavored
  processes abort in the decomposition.**
  `decompose_joint_spec()` aborts on any fid with a flavor: the pool evaluator
  (D3) scores a layer's events through a formula with no flavor filter, so a
  flavored fid would be scored on the wrong events. This makes a pre-existing
  evaluator limit loud rather than adding scope, but it means flavored joint
  specifications cannot reach `estimate_dynes()` until the evaluator (or the
  `dynes-augmentation` batched evaluator) filters by flavor.
  *Root cause:* `preprocess_pool_sequence()` preprocesses each sub-model
  separately through `estimate_wrapper(sm$engine_formula, ...,
  preprocessing_only = TRUE)`, the unflavored path with no flavor key. Every
  flavor on a layer is scored with all of that layer's events as its dependent
  events, where a flavor's own events should be dependent and the other
  flavors' events only state changes and, on timed rate sub-models,
  right-censoring boundaries.
  **Verdict (2026-09-25): lift the limit in abmcem.** Pool entry runs one
  `preprocess_joint()` walk per sequence, the merged single-clock walk ordinary
  flavored estimation already uses through `preprocess_flavored()`, over the
  joint spec pointed at that sequence's data, and files the resulting
  `goldfishStat`s by fid. That is one walk per sequence instead of one per
  sub-model, with dependent/censoring routing correct by construction.
  *Verified 2026-09-25 (scratchpad probes):*
  - Swapping a flavored spec's `$data` gives stats identical to a spec rebuilt
    on the new data. The walk reads only `joint_spec$data`; tested on a single
    flavored spec, and a multi-member joint spec's stale member `$data` copies
    are not yet exercised.
  - Per-fid joint stats through the zero-iteration engine reproduce the
    ordinary flavored fit's log-likelihood per fid to 1e-8, with score ≈ 0 at
    its estimates.
  - **Correction:** fixed-only fids do *not* pass through unchanged. The joint
    walk aborts on their effect-free bundles ("A model needs at least one
    effect term"), hence the pool engine spec in D3.

  Flavor keys are stamped from the data by `add_flavor()`, so augmented
  sequences must carry them. That is a requirement on the augmenter, and 6.3a
  pre-checks it.
  *Why now:* `dynes-augmentation`'s first goal (panel layers estimable with a
  Stage-A flavored specification) needs flavored specs, and the change lands
  at the pool entry task 6.2 is already reshaping, so the `dynes-augmentation`
  batched evaluator becomes an optimization of a correct path rather than its
  fix. Scheduled as task 6.3; the spec gains the "flavored processes are
  scored on their own events" scenario.
- **[resolved] Citation author order.** `inst/REFERENCES.bib` lists the
  sample-reuse reference as Levine & Casella (2001). This design, D4/D5, and
  older roxygen/comments said "Casella & Levine". The new `estimate_dynes()`
  roxygen uses the bib order; the older text was untouched. **Resolved
  (2026-09-25): the bib order wins everywhere.** This design, the
  `abmcem-optimization` spec delta and task 2.1b now say Levine & Casella;
  task 6.4 carries the code side (comments, roxygen, the bib key renamed to
  the `firstauthorYEARword` convention as `levine2001implementations`, and a
  bib entry for Ruth (2024), which D9 cites but the bib lacked).
- **[resolved] Pinned-rate entry warning added beyond the task text.** Landing
  `estimate_dynes()` tripped three `skip_if_not(exists("estimate_dynes"))`
  placeholders in `test-intercept_only_rate.R`. Resolved (user decision) by
  calling `warn_pinned_rates(completed, "estimate_dynes")` at entry, mirroring
  `simulate()`, and replacing the placeholders with real tests: the entry
  warning, the re-fire across both consumers, and pool score/Fisher unchanged
  by a pinned rate's sub-model. Review that this entry warning belongs in this
  change's `dynes-estimation` spec delta, which does not yet state it.
  **Resolved (2026-09-25): no `dynes-estimation` delta.** The entry warning is
  already required by the living `intercept-only-rate` spec ("Each consumer
  warns that the rate is pinned, worded for its source"), so the call fills that
  requirement and is not extra scope. The review found that requirement's
  wording wrong: it follows the consumer, but the pin's count source follows how
  the layer is observed, so the `estimate_dynes()` entry snapshot claims a
  Hamming-diff pin for the event-observed `emails` layer. A completed rate also
  warned twice and was named `Rate 3`. The fix is a separate change,
  `pinned-rate-warning-source`, which re-records that snapshot.
- **[deferred — v2] Pinned rates on flavored relational layers.** No
  flavor-aware relational risk set exists yet. Until one does, a completed rate
  on a *flavored*, event-observed layer is pinned through the panel
  wave-endpoint path (`panel_wave_risk_set()`, the stopgap dispatch in
  `pin_completed_rates()`). Its count is therefore a Hamming diff between wave
  endpoints, not the flavor's observed event count. Since
  `completion-idempotency` (D2/D3), such a pin also records its wave grid, and
  completing it again on a different grid aborts. v2's flavor-aware relational
  risk set would pin these layers from their own events (`event_count` source),
  with no grid. Their grid abort then stops applying, and their pinned
  intercept, which the evaluator's pinned-rate compensator reads, changes
  value. DyNES fits on flavored relational data will move once at that point,
  so record it as a behavior change then. No v1 action for this change.
- **[deferred — follow-up changes] Defects found after landing.** The
  `dynes-augmentation` task-1.4 toy (and its 1.5 revision) found three problems
  in this change's code, recorded 2026-09-30. All 46 tasks here are done, so
  each goes to its own change rather than reopening this one:
  1. **`pool_fixed_loglik()` does not flag a −Inf fixed-only term as
     degenerate.** When the engine *returns* −Inf instead of erroring (e.g. a
     pinned plateau of −Inf with events in its period), the result is
     `loglik = −Inf, degenerate = FALSE`. The D3 `tryCatch` guard catches only
     the erroring path, so the sequence reaches the weight normalizer as −Inf.
     Any non-finite term should set `degenerate = TRUE`. This is a bug against
     the existing "degenerate sequence does not abort the pool" requirement,
     so the follow-up is a small fix change that `## MODIFIED`s that
     requirement once this change is archived. **Gate:** `dynes-augmentation`
     task 6.1 must not wire `wave_times` before it lands.
     → **Taken up by `fixed-loglik-degenerate`** (2026-10-01; its group 1
     is the 6.1 gate). That change also stops a run whose whole pool is
     dropped for a fixed term with `converged = FALSE`, which the D7 stop
     rule otherwise reports as converged.
  2. **SGD defaults under-step (D6).** `step_size = 0.001`, constant, on a raw
     summed score: on the toy (K = 30, 6–11 free θ) every M-step hit the
     100-iteration cap with gradient norm 0.9–14, Q decayed geometrically, and
     no 15-iteration run met the stop rule. A scale-aware default
     (adagrad/adam, or a step normalized by event count or information) is a
     defaults change that moves results, so it needs its own change with
     evidence.
  3. **Resident store replication (D10).** The parallel seam's (1 + W)
     replicated store is the binding memory bound once the start state is
     shared (K_max ≈ 850 vs ≈ 2700 sharded at W = 5, 4820 path events, 12 GB).
     Sharding the store (coordinator plus 1/W of the pool per daemon, tasks
     routed by id) is a performance change for its own proposal.

  Found later (2026-10-01, `fixed-loglik-degenerate` explore pass), numbered
  to follow `progress.md`, where follow-up 4 is a resolved doc-drift item:

  5. **Uniform weighting ignores `degenerate`.** On the MCMC + refresh path
     the E-step weights are 1/n whatever a sequence's flag, so a degenerate
     sequence's `lambda` is `−Inf − (−Inf) = NaN`: Q is NaN, nothing is ever
     accepted, and the loop aborts with `abort_retries_exhausted()`.
     Importance weighting escapes only because softmax zeroes `−Inf` and the
     loop sets `lambda[weights == 0] <- 0`. Any degenerate sequence triggers
     it (walk failure, singular information, fixed term). A fully
     fixed-term-degenerate pool is caught earlier by
     `fixed-loglik-degenerate` D6; a *partly* degenerate pool is not. Rare on
     MCMC, since the chain rejects `−Inf` candidates and stays degenerate
     only if it starts there. The fix (drop degenerate sequences from the
     uniform mean, or zero their `lambda` and renormalize) changes the
     uniform estimator, so it needs its own change.
