## Context

Stage A (`flavored-processes`) makes fully observed competing creation/dissolution
processes estimable; DyNES handles the panel-observed case: states seen at waves, the
event sequence between waves latent. `refactor-single-data-object` reserved the
per-layer panel-semantics flag (D5) and defined change-list semantics for exogenous
panel covariates; the writer-strategy contract documented (without implementing) a
per-event simulation hook — exactly the surface a model-driven sequence draw needs.
Prototypes in `.plan/DyNES/` implement the full algorithm against goldfish 1.6.10:
`getChainSample()` (uniform Hamming-consistent sequences), `getChainSampleFromModel()`
(model-driven draws), `permuteChainSample()` (MCMC mutations), `sgd()` /
resampling / importance-sampling optimizers, `computeLl()` and
`get_weights()` evaluator pieces, and `get_se_from_list()` Fisher aggregation. They
lean on workarounds the current pipeline has since solved (opportunity lists for
support constraints, manual state/label bookkeeping, per-chain data reloading).
RSiena's ML estimator (`~/Documents/repos/rsiena`) implements MCMC sequence sampling
between waves — its proposal moves, endpoint handling, and diagnostics answer the
statistics questions (OQ B5). The change-local `augmentation_background.md` records
the mathematical background for the three augmentation routines (notation, the
constrained simulation, the MCMC move); D19–D20 refine it and supersede its simple
acceptance ratio and window formulas where they differ.

Decisions below marked **[spike-gated]** are written from the prototypes and the OQ
answers but are revised from the Phase-1 spike measurements before their implementing
phase starts.

## Goals / Non-Goals

**Goals:**
- Panel-flagged focal layers estimable via `estimate_dynes()` with a Stage-A flavored
  specification, by ABEM over pools of augmented sequences.
- Pluggable step families (augmenters / evaluators / optimizers) mirroring the
  writer-contract modularity, so algorithm variants are recipes, not forks.
- Batched, memory-bounded sequence evaluation with the existing C++ kernels.
- Honest uncertainty: Fisher-based `vcov()` plus Monte-Carlo error diagnostics.

**Non-Goals:**
- Incremental re-preprocessing of mutated sequences (suffix patching) — full
  re-preprocess is the baseline (OQ B4); revisit only with profiling evidence.
- Flavor-filtered effects, the `make_multivariate_spec()` constructor (own changes;
  the batched evaluator is written so a multivariate spec pool can adopt it). The
  *estimand* here is nonetheless multi-layer (D19) — deferred is the general
  spec-construction surface, not the joint likelihood; how v1 passes the
  multi-layer specification is an Open Question.
- GoF statistics/plots — the `simulate()` primitive IS in scope (D12); the
  goodness-of-fit surface built on it (observed-vs-simulated statistic
  distributions) is a follow-up.
- Modeling the rate part of choice-only (ordered) data by treating timings as
  latent and imputing them through the augmentation machinery — a genuinely new
  estimand the DyNES core makes reachable, recorded as a future extension
  (completely different estimation), not attempted here.
- Missing-data / partially observed waves beyond complete snapshots.

## Decisions

### D1 — Surface: `estimate_dynes()` + nested `set_alg_*()` control constructors
`estimate_dynes(spec, algorithm = set_alg_em(...))`. The originally proposed flat
`set_algorithm_abem()` is superseded by **four constructors, one per algorithm
concern, nested under the EM constructor** so every cross-object rule has one home:

- `set_alg_em(n_sequences, max_iterations, accept_quantile, growth_quantile,
  stop_quantile, tolerance, max_retries, seed, em_trace_se = FALSE,
  augmenter = set_alg_augment(), weights = set_alg_weights(),
  optimizer = set_alg_sgd())` — the ascent-based MCEM loop (D18) plus the **only
  `seed`**: one RNG stream governs augmentation draws, batch selection, and
  resampling (per-block seeds rejected as an irreproducibility trap).
- `set_alg_augment(routine = c("mcmc", "random", "sim"), burn_in, thinning,
  initialize = c("random", "sim"), initial_sequence = NULL,
  move_probs = c(permute = 0.5, shift = 0.5))` — sequence generation (chain
  lifecycle in D16; moves, windows, and densities in D20). Routine strings match
  the `augment_seq_*()` constructor names. `burn_in`/`thinning` are counted in
  **sweeps** (one sweep = as many proposals as there are panel events), so the
  same setting means the same thing at every dataset size; their default values
  are a Phase-1 tuning outcome (the arguments ship now). `initialize` picks the
  first chain's start; `initial_sequence` overrides it. `move_probs` (MCMC only)
  is the user-facing move-type mix, normalized internally — a named vector so
  future move types extend it without deprecating a scalar.
- `set_alg_weights(weighting = c("importance", "uniform"), use = c("importance",
  "resampling"), resampling_scheme = c("stratified", "residual", "random"),
  transformation = identity, refresh = FALSE, ess_threshold = 0.5)` — how the
  E-step expectations are weighted (D13, D14). A user-supplied custom weight
  function is dropped: the ABMCEM accept/stop machinery requires a variance
  estimator per scheme (D15), which an arbitrary function cannot supply.
- `set_alg_sgd(variant = c("minibatch", "full"), batch_size, batch_scheme =
  c("weighted", "cyclic"), step_size, step_schedule = c("constant", "adagrad",
  "adam", "momentum"), max_iterations, tolerance)` — the M-step (D17).

Argument names are **descriptive, never Greek**: `accept_quantile` /
`growth_quantile` / `stop_quantile` for the ascent-based MCEM paper's α/β/γ, with
the correspondence documented in roxygen. All names above are working names; the
`set_alg_*` prefix is provisional pending the package naming-convention discussion
(`set_*_opt()` alignment). Each child constructor validates only its own
arguments; **all cross-object rules (the D13 validity matrix and precedence
table) execute in `set_alg_em()`'s constructor**, since siblings cannot see each
other — inconsistent-but-ignorable combinations warn and are ignored, impossible
ones abort.

The estimand is the joint multi-layer model of D19 (multiple modeled RE and PE
layers, per layer × flavor rate and choice formulas, θ the concatenation across all
modeled sub-models); event-stream estimators continue to abort on panel focal layers,
naming `estimate_dynes()`. How the multi-layer specification is passed is an Open
Question (the multivariate spec constructor is a separate change). *Rejected:* overloading `estimate_dynam()` with an
`algorithm` switch — panel augmentation changes the estimand's data requirements and
the result's uncertainty semantics; a distinct verb keeps both surfaces honest (and
matches the dev plan's naming). *Also rejected:* the flat single constructor (this
decision's original shape) — the four concerns have disjoint validation and
future-extension surfaces, and a flat argument bag hides the cross-object rules.

### D2 — Step families as contracts mirroring the writer strategy
Three constructor families, each returning an object with `init(spec, waves, control)`
/ step / `finalize()` hooks, dispatched by the `set_alg_*()` controls (D1):

- **Augmenters** — `augment_seq_random()` (iid-uniform times with within-chain
  sorting over the wave-diff flip set, D20; prototype `getChainSample()`),
  `augment_seq_sim()` (constrained sequential simulation from the model at the
  current parameters, consuming the per-event simulation hook; prototype
  `getChainSampleFromModel()`), `augment_seq_mcmc()` (MCMC moves on a current
  sequence: the permute + shift move set with rate-based time redraws, D20 — the
  RSiena-studied insert/delete excursion moves remain future extensions, D16;
  prototype `permuteChainSample()`). Contract:
  `augment(data_window, theta, control) → sequence(s)`, where `data_window`
  carries the full wave span — snapshots at every wave, per-interval flip sets,
  and the observed (modeled and exogenous) event streams — since the MCMC chain
  is global over the whole sequence (D19). Every returned sequence MUST hit every
  wave snapshot exactly and reports its **log proposal density over the full
  generative path** (D20), stored permanently with the sequence (D14). The MCMC
  augmenter additionally receives an injected proposal-evaluator closure at
  `init()` (D20) — it never touches the evaluator's pool API.
- **Evaluators** — `evaluate_sequence_pool(pool, theta, what = c("loglik", "score",
  "fisher"))` → the requested per-sequence quantities plus importance weights.
  Callers request only what they consume: the SGD loop runs score-only; Fisher is
  computed once at convergence and, opt-in, per accepted iteration for the
  `em_trace` SEs (D17, D18). Weights belong to the evaluator (the
  augmenter reports its proposal density; the evaluator forms the ratio); prototype
  `computeLl()` + `get_weights()`. `compute_lik_seq(spec, sequence, theta)` is
  per-sequence sugar over the batched path — named to avoid colliding with the
  `logLik()` S3 method (OQ B1 answer).
- **Optimizers** — `optimize_abem_sgd()` (stochastic gradient over batches;
  prototype `sgd()`): `step(e_stats, theta) → theta'` plus convergence
  bookkeeping. The previously listed `optimize_abem_resampling()` /
  `optimize_abem_is()` variants dissolve: IS-vs-resampling is a **weighting**
  concern (D13/D14), orthogonal to the optimizer, so one SGD optimizer serves all
  schemes.

*Rejected:* one monolithic EM loop with `if` branches per variant (the prototypes'
shape) — the three prototype scripts diverged precisely because steps weren't
separable; the writer contract demonstrated the modular alternative in this codebase.

### D3 — Engine contract: batched C++ over flat preprocessed pools **[spike-gated]**
The evaluator's engine is one C++ call over a list/pool of default-format flat
preprocessed objects (`stat_mat_update` + pointers + broadcast encoding — the
memory-efficient representation), returning per-sequence logLik/score/Fisher at theta
with zero optimizer iterations. Rate/probability computation at a given process state
reuses the estimation kernels (replacing the prototypes' hand-rolled
`getDyNAMRates()`/`getDyNAMChoices()`). The B1 spike (K ∈ {10, 100, 1000, 10000}
sequences on the packaged `social_evolution` dataset, wall time + peak RSS,
R-loop-per-sequence vs batched C++) confirms or revises this before the evaluator
phase. *Rejected:* per-spec R calls as the engine
(sugar only — K×event-loop overhead in R); gather-format pools (the stacked event rows
duplicate what broadcasting avoids; no collective-estimation gain, OQ A1/B1 answers).

### D4 — Pool storage: in-memory flat objects; ~5 GB acceptance; DBI spill fallback **[spike-gated]**
The pool is an in-memory list of flat preprocessed objects. Acceptance threshold
(updated from the OQ B3 answer's original ~2 GB): pools of 100–1000 sequences at the
scale of the packaged `social_evolution` dataset stay under ~5 GB (a per-machine
bound, divided across any parallel workers). Pool growth is bounded per EM iteration
by D18's retry rule but unbounded across iterations — deliberately no cap argument
and no warning; the spill fallback is the whole memory story. The B3 spike profiles a grid (n × events × K); if exceeded, the fallback order
is (1) a broadcast-exploiting on-disk variant of the default format, (2) the existing
DBI writer — noting its caveat that the gather event stack is itself the memory/disk
heavy part. No new storage format is invented before the measurements.

### D5 — Full re-preprocess per drawn sequence (OQ B4)
Each accepted sequence is preprocessed from scratch through the existing recipe path.
A mutated sequence invalidates statistics from its first perturbed event onward, so
suffix recomputation approaches full reprocessing in expectation; incrementality is
premature until the B1/B3 spikes show re-preprocessing dominating the E-step (> ~20%
rule from the OQ). The initial-state materializer (single-data-object D16) makes each
wave's starting state cheap to construct.

### D6 — RSiena's ML estimator answers the sequence-sampling statistics (OQ B5)
The proposal-space questions — endpoint-hitting paths (Hamming-length orderings and
permutations), whether excursion moves (create-then-dissolve inside an interval) enter
the proposal space, acceptance ratios, identifiability of separate
creation/dissolution parameters from waves, and MC-error / convergence diagnostics —
are settled by studying RSiena's MLE sampling code and re-implementing the applicable
moves under the augmenter contract. The Phase-1 study task produces a written note
(`.plan/DyNES/rsiena_mle_notes.md`) mapping RSiena's moves onto the augmenter/evaluator
contracts; ideas are re-implemented, no code is copied (both packages are GPL-3, but a
clean re-implementation against our contracts is required regardless).

### D7 — Result contract: Fisher approximation + MC standard errors (OQ B6)
ABEM returns the parameter estimates and the Fisher-information approximation
accumulated by the evaluator at convergence; `vcov()` inverts it, and the result
additionally carries MC standard errors and the **`em_trace`** per-iteration
diagnostics object (D18) — the ascent trajectory, ESS, pool bookkeeping, and MCMC
chain statistics — as a documented part of this contract, so convergence problems
are diagnosable from the result alone. `summary()`
displays asymptotic and MC error side by side so users see when the pool, not the
data, limits precision. Fixed-parameter handling follows the prototypes'
`unfixed_params()`/`std_error_fixed_params()` (NA rows/cols for fixed positions).

### D8 — Panel flag activation and wave diffing
The per-layer panel-semantics flag (reserved by single-data-object D5) becomes
readable metadata: a panel-flagged layer's rows are snapshots; consecutive waves diff
into the candidate flip set per interval, with wave times as hard boundaries the
augmented sequence must hit. Flavors are arbitrary, not just creation/dissolution:
the diff consumes the layer's transition/support specification and inverts the
(from-value, to-value) → flavor mapping, which MUST be injective — the flavor is
never a latent variable. A value change larger than one allowed step decomposes into
a **per-dyad ordered chain** of events (0→2 under ±1 transitions is two events with
a forced order); net-zero changes produce no events (excursions are out of v1, D16).
v1 data restrictions: all panel-flagged layers share **one wave grid** (nested grids
— e.g. yearly surveys plus weekly ones — are a recorded future development); the
**node set is fixed** over the whole period (composition changes / presence windows
are a future development); an observed RE falling exactly on a wave time belongs to
the **earlier** interval (it precedes the snapshot). A modeled panel layer is
modeled for **all its flavors or not at all** — partially specified layers abort
(D19); a fully unmodeled panel layer keeps the existing exogenous change-list
semantics (state jumps at wave times), untouched. Diffing is a conversion-module
function also usable standalone (the upstream snapshot-diff-verb proposal to manynet
remains open and non-blocking).

### D9 — Simulation hook: implement the documented recipe-loop extension point as the walk's source port
The per-event simulation hook lands exactly as documented by
`preprocess-output-writers`: after the stats update for event i, before advancing, a
registered callback sees the visible process state and may append to the event stream.
`augment_seq_sim()` is its first consumer (drawing event i+1 from the rates at
state i). Conceptually the hook is the **source port** of the recipe walk — the mirror
of the writer contract's sink port: the walk's event supply is, in general, a **merge
of the observed exogenous schedule (covariate/composition changes) with a
generated-or-imputed dependent stream**. Pure-observed preprocessing and
pure-generative simulation are the degenerate ends of that hybrid; the DyNES augmenter
is the hybrid source conditioned on the wave endpoint. Implementing the hook with this
framing (the callback participates in the schedule merge, not a bolted-on side
channel) directly carries the `simulate()` surface (D12) — a generative source +
recorder sink + the multi-consumer (rate + choice) walk from `flavored-processes`
D3 — now IN SCOPE for this change rather than a follow-up. Like the writer, the
source resolves to a plain local closure before the loop — no per-event dispatch
machinery. The augmenter-side risk-set restriction (observed pairs with remaining
support-applicable events, D20) happens **in R**: the callback receives the kernels'
full rate/choice vectors and subsets + renormalizes — no risk-set mask argument is
added to the C++ kernels in v1 (worth revisiting only at large n, D11). The
sampling-writer and parallel-chunking extension points stay documentation-only.

### D10 — Parallelization: sequence-level only, under a non-nested thread budget **[spike-gated]**
Every expensive step of an ABEM iteration (augmentation, the D5 full re-preprocess,
likelihood evaluation) is per-sequence independent, so the **sequence is the only
sanctioned unit of parallelism**; within a sequence the event loop is a serial state
fold. The likelihood kernels are tall-skinny (`m × p` with p ≈ 5–20), giving an
arithmetic intensity of ~p/8 flops per byte — memory-bandwidth-bound, where
multithreaded BLAS caps at the machine's bandwidth ratio (~2–4×, never core count) and
pays a fork/join synchronization tax on every one of the ~4 × events × K × iterations
BLAS calls. Sequence-level parallelism hits the same bandwidth ceiling on that op but
also parallelizes what BLAS cannot touch (the masked exponentials, R glue, and the
dominant R-side re-preprocess) with one dispatch per sequence. The governing invariant
is therefore **"don't nest": workers × BLAS threads ≤ cores** — not blanket pinning.
Workers pin BLAS to 1 when workers ≈ cores; a lone call (e.g. `compute_lik_seq()` on a
large dataset, or an end-of-pool straggler regime) may give BLAS the machine; the
adaptive budget `blas_threads = max(1, floor(n_cores / n_workers))` covers the middle.
Backend: the loop over sequences goes through **one internal map seam**, default
serial `lapply()`; when **mirai (Suggests, not Imports)** is installed and
`n_cores > 1`, persistent daemons hold the data object once (`everywhere()`) and
receive only θ and sequence draws per iteration — precisely the access pattern where
base PSOCK's `clusterExport`/re-serialization is at its worst (visible in the
prototype's `sgd_refactor()` cluster block). FORK/`mclapply` is excluded outright
(Windows unsupported; forking a process with live OpenBLAS threads is undefined
behavior). `set_alg_em()` gains `n_cores` (default honoring CRAN's 2-core
check cap) and parallel RNG streams for reproducible draws; no per-block parallel
arguments exist — the SGD batch evaluation is the same per-sequence loop this
decision governs. The B1 spike's
BLAS-default vs pinned × serial vs sharded crossing measures the crossover before the
evaluator phase, including a large-n synthetic cell. Two DyNES-specific boundaries:
the `augment_seq_mcmc()` chain is inherently **serial** (each step consumes the
previous state's likelihood), so sequence-level parallelism applies to random/sim
draws and to pool evaluation, not to MCMC generation — multiple parallel chains and
parallel tempering are recorded future developments (D16); and **wave-level
parallelism is ruled out**, because windowed/memory effects are allowed (D19) and
the likelihood therefore does not factorize over between-wave intervals. *Rejected:* multithreaded BLAS as
the parallelism strategy (bandwidth-capped, non-portable — reference-BLAS users get
nothing — and useless for preprocessing); base PSOCK as the parallel backend (the
worker-state tax across hundreds of iterations); mirai in Imports (parallelism is
optional acceleration; serial must work dependency-free); OpenMP inside the batched
C++ call as the primary axis (only accelerates evaluation, requires an R-API-free
region, and pthread-OpenBLAS inside OpenMP regions is a known hazard) — it remains a
measured, optional second layer under the same thread budget.

### D11 — Large-n dyadic models: risk-set sampling is the real lever, out of scope here
For REM-family (dyad-indexed) models the per-event cost is m = n² rows: at n = 5,000
that is ~2 GB streamed per event, which no threading strategy rescues. The standard
remedy is case-control sampling of the risk set — the **alternatives-sampling gather
writer** already documented as an extension point by `preprocess-output-writers`. It
stays out of scope for this change (it requires its own estimation adaptation and
weighting theory) but is recorded as the designated follow-up for large-n dyadic
DyNES/REM applications; the D10 thread budget is not expected to make n ≥ several
thousand dyadic models practical on its own.

### D12 — Process simulation: per-family timing strategies, stopping rules, explosion guard
`simulate()` generates event sequences from a specification and parameters through
the D9 generative source feeding the multi-consumer walk, with observed exogenous
streams merged in. Grounded in the reference packages: relevent's
`simulate.rem.dyad()` never estimates a nonparametric baseline — it simulates a
**fixed number of events** (`nsim`, default the observed count) and supports keeping
observed timings while redrawing only the dyads (`redraw.timing = FALSE`); remulate
hard-codes exponential waiting times to a horizon with no ordinal mode and no
explosion guard. goldfish adopts the union:

- **Stopping rules**: simulate to a time horizon OR for a fixed event count; the
  fixed count doubles as the guard against **process explosion** (super-linear
  feedback terms driving the total rate unbounded); horizon simulation carries a
  hard `max_events` cap with a diagnostic abort.
- **Timed sub-models** (DyNAM-rate, REM): exponential waiting times from the total
  rate (θ including the intercept) — exact simulation.
- **Ordered/Cox-like sub-models** (rate_ordered, REM-ordered, choice-only): the
  baseline is unidentified, so two modes — (a) **fixed template times, redraw
  dyads** (relevent's precedent) and (b) **pseudo-time from the crude rate**, which
  is exactly the intercept-scalar formula
  `log(n_dep_events / total_time / avg_active_actors)` already carried (per flavor)
  by every preprocessed object — no new estimation needed.
- **Choice-coordination**: mutual-choice rejection sampling — rate assumed
  constant; a sender is drawn uniformly; a waiting time from the crude rate; ego
  proposes an alter from its choice probabilities; the event realizes iff the
  alter's choice reciprocates, otherwise the proposal is rejected and redrawn. The
  acceptance rate is reported as a diagnostic.
- **Flavored specifications** simulate as competing processes: the next event is
  drawn across all flavors' total rates with each flavor's derived mask maintained —
  a hard dependency on `flavored-processes`.
- **Output**: simulated pools ARE evaluator-compatible sequences (the same format
  the augmenters produce and `evaluate_sequence_pool()` consumes), closing the
  simulate → augment → evaluate loop; trajectory statistics are optionally
  recordable through the writer sink.

*Rejected:* nonparametric baseline (Breslow-style) recovery to draw calendar times
from ordinal fits — neither reference package does it, it adds an estimation surface
for timing that the ordinal estimand deliberately ignores, and the two supported
modes cover the GoF and augmentation uses.

### D13 — Augmenter × weighting validity matrix
Uniform weighting of the E-step is valid exactly when the proposal *is* the model's
conditional distribution `p(sequence | endpoints, θ_k)`:

| `routine`      | draws from                                 | `weighting = "uniform"`            | importance weights                         |
|----------------|--------------------------------------------|------------------------------------|--------------------------------------------|
| `"mcmc"`       | ≈ target at θ_k                            | valid within the drawing iteration | likelihood ratio for cross-iteration reuse |
| `"random"`     | uniform over orderings/times               | **invalid** (mis-weighted E-step)  | required: target / uniform density         |
| `"simulation"` | sequential model draws, endpoint-distorted | invalid (conditioning bias)        | required: target / product of draw probs   |

`weighting = "uniform"` is therefore accepted only with `routine = "mcmc"` and only
while no stale (previous-iteration) sequences remain in the pool (i.e. with
`refresh = TRUE`); every other combination warns and switches to importance
weighting — the default. This matrix, the batch-scheme/gradient coupling (D17), and
the refresh-inapplicability rules (D14) form the **precedence table**
`set_alg_em()` enforces via warn-and-ignore (e.g. cyclic batches ⇒ weight-based
selection arguments are ignored with a warning; the dominant setting wins). The
prototypes' random-draw + equal-weight combination is deliberately not carried into
the API: it estimates an expectation under the uniform law, not under the model.

### D14 — Weight state model: per-sequence reference records, refresh, ESS guard
Every pooled sequence permanently stores the triple **(θ_ref, log-likelihood at
θ_ref, log proposal density)** — for MCMC draws the last two coincide (the only
tractable "proposal" is the target the chain converged to). Weights are kept on the
log scale throughout. Cross-iteration reweighting is the likelihood ratio against
each sequence's own θ_ref; with mixed-θ_ref pools this is standard multiple
importance sampling (consistent — the balance heuristic is a recorded
variance-reduction refinement, not v1). The weight `transformation`
(clipping/smoothing) applies to raw log-weights **before** normalization and feeds
both IS estimates and resampling probabilities; default identity, and a
non-identity transformation combined with resampling warns that the target
distribution changes (truncated-IS caveat).

Two staleness defenses, one code path:

- `refresh = TRUE`: redraw the whole pool every EM iteration at its grown size
  (e.g. 100 sequences grown to 150 last iteration ⇒ draw 150 fresh). This makes
  cross-iteration reweighting, the ESS guard, and staleness transformations
  inapplicable — non-default settings of those arguments warn and are ignored.
- `refresh = FALSE` (persistent pool): the **ESS threshold** guards weight
  degeneracy. ESS = 1/Σw̄² — the weighted pool of K sequences carries the
  information of ≈ ESS equal-weight draws (K under uniform weights, →1 under one
  dominant weight). A full redraw at current θ triggers when
  ESS < `ess_threshold` × K (default 0.5, the particle-filter convention).
  Warnings: when the guard is disabled but the condition is met anyway, and when
  the guard fires (informing that new draws are generated); never at startup.

Refresh and ESS-triggered redraws share one implementation — warm-started chains
(D16). **Resample-then-mutate** (particle-filter resample-move: O(K) weighted
resampling, then a few mutation moves to break duplicates) is recorded as a future
optimization, not a third code path — warm-started chains already capture most of
its saving. **Within-SGD weight refresh is dropped for v1** (each refresh at a
proposed θ′ costs a full-pool likelihood pass, defeating mini-batching): weights
are fixed for the duration of each M-step, so no provisional-weight state exists —
the pool's canonical weights only change between EM iterations.

### D15 — Q and its standard error as internal S3 generics
The accept/grow/stop rules all test `Q ± z·ASE`, but the correct ASE estimator is
scheme-specific (weighted-mean variance under IS; resampling noise included under
resampling; plain mean under uniform) — the estimator formally belongs to the
weighting scheme, not the EM loop. `evaluate_sequence_pool()` returns a classed
E-step object (`c("estep_is", "dynes_estep")`; likewise `estep_resampling`,
`estep_uniform`) and two generics dispatch on it: `compute_q()` and
`compute_ase()`. The EM loop is thus written once, scheme-agnostically. **Internal
only** for now; exporting them for user extensibility is a recorded follow-up.
Under MCMC draws the ASE treats sequences as **independent given the thinning**;
the augmenter records an autocorrelation index and basic chain statistics into the
`em_trace`, with a user-facing suggestion to increase `thinning` when the index is
high — no autocorrelation-corrected ASE method in v1.

### D16 — MCMC chain lifecycle: warm starts, burn-in at every restart
The first chain initializes per `initialize`: a random endpoint-consistent draw
(`"random"`, via `augment_seq_random()`) or a constrained-simulation draw (`"sim"`,
typically nearer the target, shortening the unavoidable burn-in); a user-supplied
`initial_sequence` — validated for endpoint-hitting, chain order, and within-wave
times — overrides both. Burn-in and thinning are counted in sweeps (D1). Lifecycle
rules:

- **New EM iteration** (new target at θ_{k+1}): the new chain starts from the
  previous iteration's last draw — the SAEM/RSiena warm start. Since θ_{k+1} ≈ θ_k
  this *shortens* burn-in; it does not remove it: **burn-in applies at every chain
  (re)start, including warm starts** (the inherited draw is from the previous
  target).
- **Within-iteration growth** (rejected update, D18): continue the *same* chain —
  same target, no new burn-in beyond thinning.
- **Persistent pool** (`refresh = FALSE`): new draws start a warm-started chain at
  the current target; the pool then mixes θ_refs, handled by D14's
  multiple-importance-sampling weighting.

Recorded future developments of the augmenter, deliberately absent from the v1
argument surface: parallel tempering and multiple parallel chains (v1 generation is
serial, D10); insert/delete excursion moves (net-zero within-interval changes never
enter the v1 proposal space, D8); nested wave grids and composition changes /
presence windows (D8). The v1 move set itself is in D20 — the original "permute
order only" plan is superseded: both v1 moves redraw times from rate-based
proposals, which the joint likelihood's waiting-time densities require anyway.

### D17 — M-step: one SGD over the concatenated θ; batching semantics pinned
One M-step optimizes the concatenated parameter vector across sub-models
(rate + choice); exploiting the likelihood factorization into per-sub-model
independent SGD problems is a recorded future development (the EM-level Q/ASE
decision stays joint either way). Semantics:

- **Batch schemes.** `"weighted"` (default): draw batch members *with replacement*
  proportional to the normalized weights and apply the **unweighted** batch mean —
  already unbiased for the weighted gradient (E[mean g] = Σw̄ᵢgᵢ); no
  Horvitz–Thompson correction argument is needed because selection probabilities
  equal the weights (defensive selection mixtures with HT correction: recorded
  future refinement). `"cyclic"`: divide the pool into fixed batches and rotate
  deterministically, no reshuffling — the reproducible incremental-gradient
  alternative (epoch reshuffling is the ML standard but not v1). Because cyclic
  selection is uniform, the within-batch gradient **must be importance-weighted**
  (w̄ᵢ·K/B per contribution) or the M-step maximizes the unweighted — wrong —
  objective. Under uniform weights the two schemes coincide.
- **Step sizes.** Constant (the default, with a package default value) or the
  adaptive schedules from the literature — AdaGrad, Adam, momentum — with their
  hyperparameters fixed at literature defaults, not exposed (exposing them is a
  future decision).
- **State.** Optimizer accumulators (moment estimates, squared-gradient sums)
  reset at every M-step: each M-step is a fresh optimization.
- **Cost.** The SGD loop requests score-only evaluation (D2 `what` flag); Fisher
  is computed only where consumed (final SEs; opt-in `em_trace` SEs). Resampling
  inside SGD remains disallowed, and `batch_scheme` reserves no value for it
  until its weighting theory is settled.

### D18 — EM control flow: θ₀, bounded pool growth, hard failure, `em_trace`
- **θ₀**: user-supplied through the existing `initial_parameters` mechanism in
  `set_estimation_opt()`, which gains a **warm-start option** (draw one random
  augmentation, estimate on it, use those estimates as θ₀); the default is the
  zero vector.
- **Bounded within-iteration growth**: a rejected update (ascent lower bound < 0)
  appends ⌈pool/`max_retries`⌉ sequences and retries, at most `max_retries` times
  (default 20 — the compounding growth caps the pool at (1+1/20)²⁰ ≈ 2.65× per EM
  iteration). The argument name deliberately avoids "augmentation" (that word
  belongs to the sequence-generation routines); `max_retries` is the working name.
- **Failure semantics**: exhausting `max_retries` or `max_iterations` without
  convergence **aborts with an error** explaining the non-convergence (a
  `cli_abort()` carrying the trace-derived diagnosis) — not a flagged result.
- **`em_trace`** (name chosen over bare "trace" to say what it traces): one row
  per EM iteration — θ, Q, its ASE, decision (accept/grow/stop), pool size, new
  draws, ESS, weighting scheme in force, and the D15/D16 MCMC diagnostics
  (acceptance rate, autocorrelation index). The cheap fields are always collected;
  per-iteration parameter SEs are opt-in (`em_trace_se = TRUE`, one extra Fisher
  pass per accepted iteration). Carried on the returned object as part of the D7
  result contract.

### D19 — Joint multi-layer estimand, PE-independence detection, v1 restrictions
`estimate_dynes()` estimates one **joint likelihood over multiple layers**: any
number of RE (event-stream) layers and PE (panel-flagged) layers, each modeled
layer × flavor carrying its own rate and choice formulas — exclusively endogenous,
or with exogenous nodal statistics and interactions between flavors within and
between layers. θ is the concatenation of all modeled sub-models' parameters; the
result reports all of them (D7). RE layers and flavors may be modeled or
exogenous-only (exogenous REs merge into the schedule like covariate changes: they
update state, contribute no likelihood terms, never compete in the simulation); PE
layers are modeled all-or-nothing (D8). Windowed/memory effects are **allowed** in
v1 — the price is that the likelihood does not factorize over between-wave
intervals, so the MCMC augmenter runs one global chain over the whole sequence
(D20) and wave-level parallelism is off the table (D10).

**PE-independence detection**: from the formulas and the objects they create,
`estimate_dynes()` detects sub-models whose statistics read no modeled-PE state —
their likelihood terms are constant under PE placement (no MC error, and they
cancel from every MH acceptance ratio). A **fully** PE-independent specification
aborts: nothing is latent, and `estimate_dynam()` covers it. A **mixed**
specification also aborts, naming both remedies: run `estimate_dynam()` on the
PE-independent sub-models separately, or fold them as flavors of one single layer
in a flavored specification. *Rejected:* automatic internal partitioning (standard
estimation for the independent block inside `estimate_dynes()`) — two estimation
paths and a block-structured vcov for a case the error message resolves.

**History spans**: modeled REs before the first wave are history — they inform the
process state at the first wave but contribute no likelihood terms; modeled REs
after the last wave are discarded. (A spec wanting full RE history with no PE
coupling is exactly the fully-independent case that aborts to `estimate_dynam()`,
which has no such trimming.)

### D20 — Augmenter internals: moves, windows, proposal densities, evaluator injection
Shared rules: augmented sequences span [first wave, last wave]; sampled times lie in
**open** intervals strictly between their anchors (with a numerical guard against
equaling an anchor after rounding); every wave snapshot is hit by construction; log
proposal densities are accumulated over the **full generative path**, on the log
scale, and stored with the sequence (D14).

- **`augment_seq_random()`** — per interval of length L with n flips: draw n iid
  uniform times, then within each same-dyad ordered chain (length c_j, D8)
  reassign that chain's drawn times to its events in sorted order, then sort the
  interval. Exactly uniform over the valid configurations, rejection-free; log
  proposal density per interval: Σ_j log(c_j!) − n·log L.
- **`augment_seq_sim()`** — constrained sequential simulation. At each step the
  sender–flavor risk set is: the observed PE pairs with remaining
  **support-applicable** events (the support constraints supply the
  value-awareness — a 1→2 event has zero support until its dyad sits at 1, so
  chain order emerges with no extra bookkeeping structure) plus the **single
  globally-next unplaced modeled RE across all RE layers**. Selection
  probabilities are the conditional rates over that set, restricted and
  renormalized in R (D9). If the RE wins, it is placed at its observed time — the
  selection probability still enters the proposal density (it is part of the
  generative path). If a PE pair wins: draw the receiver among its remaining
  observed receivers by conditional choice probabilities, then draw the waiting
  time from a **truncated exponential** — the selected pair's rate, support
  (0, next anchor − t) with anchor = min(next unplaced RE time, wave end) — by
  inverse CDF (never rejects). Repeat until every flip is placed.
- **`augment_seq_mcmc()`** — one global chain over the whole sequence; moves
  restricted within one wave; retained draws thinned in sweeps (D1); generation
  serial in v1 (D10). Two move types, mixed by `move_probs` (state-independent,
  with a deterministic fallback when a type has no valid move, so the type choice
  cancels in the acceptance ratio):
  - **permute** — pick a same-wave PE pair (ω_h, ω_k), h < k, uniformly among the
    pairs whose swap respects same-dyad chain order (violating swaps are excluded
    **before** proposing — an invalid sequence is never built, never evaluated);
    swap their slots and redraw both times.
  - **shift** — pick one PE uniformly and redraw its time inside its slot window:
    relative order among PE is preserved; only the position relative to REs can
    change. Fixes the mixing hole of single-PE intervals (which have no pairs).

  **Windows, one rule**: with pred(i)/succ(i) the immediate neighbors of slot i
  among the *unmoved* PE events, padded by the wave boundaries (t*_0 = wave start,
  t*_{n+1} = wave end): t′_k ∈ (t*_pred(h), t*_succ(h)) and
  t′_h ∈ (max{t′_k, t*_pred(k)}, t*_succ(k)). This covers adjacent (k = h+1) and
  non-adjacent pairs without branching — for k > h+1 the max collapses to
  t*_{k−1}; for k = h+1 it delivers t′_k — and the shift window is the degenerate
  case (t*_pred(i), t*_succ(i)). REs never appear in window bounds (they are
  crossed freely); both bounds always stay inside the event's wave.

  **Time proposals** are truncated exponentials with the moved pair's rate
  **frozen at the process state after the preceding PE** — a deliberate proposal
  simplification: the target waiting-time density is piecewise across intervening
  REs and exogenous events, and Metropolis–Hastings corrects the mismatch through
  the target ratio, because the recorded density is the density actually drawn
  from and its support covers the whole window.

  **Acceptance**: α = [f(Ω′)/f(Ω)] × [q_rev/q_fwd], with q_fwd/q_rev the products
  of the truncated-exponential time densities (two for permute, one for shift).
  The move-type and pick probabilities cancel because they depend only on
  move-invariant structure (event counts, wave membership, chain constraints) — a
  property to protect when extending the move set. q_rev needs the moved pair's
  frozen rate **under Ω′**, emitted as a byproduct of the same pass that computes
  f(Ω′): the engine's `what` surface (D3) gains a named-pair rate-at-position
  query.

  **Evaluator injection** (the likelihood inside the chain): each EM iteration the
  ABEM loop builds `make_proposal_evaluator(spec, theta_k)` —
  `eval_fn(candidate_seq, rate_queries) → list(loglik, rates)`, one preprocess +
  one likelihood pass per proposal — and hands it to the augmenter's `init()`.
  θ_k is baked in (no stale-θ hazard), and tests stub `eval_fn` with an analytic
  toy likelihood to verify the chain's acceptance logic and detailed balance with
  no preprocessing at all. *Note for further discussion (not contract)*: a
  retained draw's preprocessed object and log-likelihood were just computed inside
  the accept/reject step and could enter the pool without re-preprocessing;
  whether the closure's return value becomes a pool-entry cache is deliberately
  left open.

*Rejected:* uniform time draws in the MCMC windows (the background note's simple
acceptance ratio f′/f is exact only for uniform non-adjacent swaps; rate-based
proposals dominate, and the density bookkeeping is required anyway); auto-rejecting
support-violating proposals via zero target density (never pay a preprocess for a
proposal a lookup can exclude — and the walk is not guaranteed to return −Inf on an
illegal transition rather than error); a separate per-dyad pending-queue structure
in the simulation (the support masks already encode applicability).

## Risks / Trade-offs

- **Spike results overturn a gated decision** (batched C++ not worth it; pools blow
  the 5 GB budget) → D3/D4 are explicitly provisional; Phase 1 ends with a
  design-revision task that amends them and the affected specs before later phases
  start.
- **Identifiability of separate creation/dissolution parameters from sparse waves** →
  the RSiena study + a simulation-study task quantify it; `summary()` exposes MC error
  so weak identification is visible, and the simulation study defines guidance for the
  vignette.
- **MCMC mixing / augmenter validity bugs produce silently wrong estimates** → the
  toy end-to-end prototype (small n, 2 waves, random augmenter, existing likelihood)
  is kept as a test fixture with known-parameter recovery bounds; endpoint-hitting is
  asserted on every accepted sequence in tests.
- **Runtime: full re-preprocess × pool × iterations is heavy** → measured in Phase 1
  on the real grid before committing; the contracts keep re-preprocessing behind the
  evaluator so a future incremental strategy is a drop-in.
- **Prototype drift** (scripts target 1.6.10 APIs) → prototypes are treated as
  algorithm references only; every step re-implements against current contracts, with
  the prototypes' numerical results reproduced as cross-checks where feasible.
- **Long change** → phases gate on committed, tested milestones (config discipline:
  version bump + NEWS per phase; baselines PASS at every commit).

## Migration Plan

Purely additive surface (`estimate_dynes()`, the nested `set_alg_*()` controls, step
constructors) plus one extension to an existing control (`set_estimation_opt()`
gains the warm-start initializer, D18); no existing estimator behavior changes except the panel-focal error
message gaining the DyNES pointer it already promises. The frozen coefficient
baselines are untouched throughout. Rollback is reverting the change's commits;
Phase-1 spike artifacts live in `.plan/` (gitignored) and impose nothing on the
package.

## Open Questions

- **[gated by Phase 1]** Final engine batching shape (D3) and pool storage format
  (D4) — resolved by the B1/B3 spike measurements, folded back into design + specs by
  the Phase-1 revision task.
- **[resolved in design]** Acceptance-ratio bookkeeping for the v1 move set —
  settled by D20 (unified pred/succ windows, frozen-rate truncated-exponential
  proposals, q_fwd/q_rev with the pick-factor cancellation); the RSiena study note
  remains a cross-check and the source for future excursion moves.
- **[spec surface]** How `estimate_dynes()` receives the multi-layer specification
  (D19): block on the separate multivariate-spec change and consume its
  constructor, or ship an interim surface (e.g. a named list of Stage-A flavored
  specs, one per layer) that the future constructor slots under. Decide before the
  estimator-surface phase.
- **[to discuss]** Whether MCMC retained draws enter the pool through the proposal
  evaluator's cache (preprocessed object + loglik reuse, D20 note) — deliberately
  not in the v1 contract yet.
- **[naming]** The `set_alg_*` prefix vs the `set_*_opt()` house convention —
  under discussion; the working names in D1 are provisional and rename cheaply
  before the surface ships.
- **[gated by Phase 1]** The D10 thread-budget crossover (when, if ever, BLAS threads
  beat sequence sharding on the large-n cell) — resolved by the B1 spike's
  BLAS × sharding crossing.
- **[simulation surface, resolve before its phase]** Entry points: `simulate()` S3 on
  the fitted result (θ̂, stats convention `object, nsim, seed`), on a specification
  with explicit `coef`, or both; base-generic naming vs a snake_case verb.
- **[simulation surface]** Ordered-family default mode (pseudo-time vs
  fixed-template times) and whether a template event list is required or optional.
- **[simulation surface]** Which writer sinks are legal on a simulation run; output
  class name.
- **[simulation surface]** Exogenous horizon: simulating past the last observed
  covariate/composition change (freeze state and warn?).
- **[simulation surface]** `max_events` default; abort vs truncate-with-warning;
  coordination rejection bookkeeping (redraw sender+time or keep the drawn time;
  acceptance-rate warning threshold).
- **[simulation surface]** Simulation under parameter uncertainty for DyNES results
  (θ drawn from `vcov()` for GoF bands) — likely out, decide explicitly.
