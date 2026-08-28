## Context

Stage A (`flavored-processes`) makes fully observed competing creation/dissolution
processes estimable; DyNES handles the panel-observed case: states seen at waves, the
event sequence between waves latent. The living `single-data-object` spec carries the
`observation = "panel"` metadata and change-list semantics for exogenous panel
covariates; `make-multivariate-spec` landed the `make_joint_specification()` composition
surface and the `multi-process-walk` handle (`walk_open`/`walk_advance`/
`walk_evaluate`/`walk_inject`) — the family-agnostic stepping + injection substrate a
model-driven sequence draw drives. Panel augmentation is triggered by a panel layer
being **referenced in the multivariate specification's formulas** (as a modeled
process or an exogenous covariate); there is no separate reserved flag.
Prototypes in `.plan/DyNES/` implement the full algorithm against goldfish 1.6.10:
`getChainSample()` (uniform Hamming-consistent sequences), `getChainSampleFromModel()`
(model-driven draws), `permuteChainSample()` (MCMC mutations), `sgd()` /
resampling / importance-sampling optimizers, `computeLl()` and
`get_weights()` evaluator pieces, and `get_se_from_list()` Fisher aggregation. They
lean on workarounds the current pipeline has since solved (opportunity lists for
support constraints, manual state/label bookkeeping, per-chain data reloading).
RSiena's ML estimator (`~/Documents/repos/rsiena`) implements MCMC sequence sampling
between waves — its proposal moves, endpoint handling, and diagnostics answer the
statistics questions (OQ B5). `.plan/dynes/augmentation_background.md` records
the mathematical background for the three augmentation routines (notation, the
constrained simulation, the MCMC move); D19–D20 refine it and **supersede its MCMC
section** where they differ. Specifically superseded: the simple acceptance ratio
`α = f(Ω′)/f(Ω)` (correct only for uniform time draws — D20 uses
`α = [f(Ω′)/f(Ω)]·[q_rev/q_fwd]` with rate-based, non-cancelling proposal densities),
the uniform time proposals (D20: truncated-exponential at the walk handle's `Λ`), and
the branchy `±1`-index window formulas (D20: unified `pred/succ`-over-unmoved-PE,
wave-bounded). Absent from the note and added by D20: the `shift` move, the three
time-domain invariants (the `−Δt·Λ` trailing-censor compensator chief among them), and
the `q ⊊ f` normalization gap. The note's notation, the `random` concept, and the `sim`
core (two-step sender-flavour → receiver draw over the observed-constrained risk set)
remain current. The note itself carries inline **[SUPERSEDED …]** / **[ADDED …]**
markers pointing at these decisions.

Decisions below marked **[spike-gated]** are written from the prototypes and the OQ
answers but are revised from the Phase-1 spike measurements before their implementing
phase starts.

**Carve-out**: the ABMCEM algorithm core — the `set_algorithm_em()` surface (D1), the
result contract (D7), the weighting/Q-ASE/M-step/EM-control decisions
(D13–D15, D17, D18), and the map-seam half of D10 — is implemented by the
separate `abmcem` change against a prototype-path evaluator (zero-iteration
engine calls); those decisions remain recorded here because D16, D19, and D20
reference them, but their implementation, tests, and any future amendments
live in `abmcem`. Decision headers below carry a **[→ abmcem]** marker. This
change keeps the spikes, panel diffing, augmenters, batched C++ evaluator,
process simulation, specification validation, and the recovery study.

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
- The `make_joint_specification()` constructor and the `multi-process-walk` handle
  (`make-multivariate-spec`) — consumed here, not built here. `estimate_dynes()`
  takes a multivariate spec (D1); the batched evaluator reads the merged walk's
  per-fid preprocessed outputs.
- The general `simulate()` primitive (`process-simulation` change) — consumed
  here; `augment_seq_sim()` reuses its per-step drawing core. GoF statistics/plots
  (`gof-dynes`) are a follow-up.
- Modeling the rate part of choice-only (ordered) data by treating timings as
  latent and imputing them through the augmentation machinery — a genuinely new
  estimand the DyNES core makes reachable, recorded as a future extension
  (completely different estimation), not attempted here.
- Missing-data / partially observed waves beyond complete snapshots.

## Decisions

### D1 — Surface: `estimate_dynes()` + `set_algorithm_em()` with nested component constructors **[→ abmcem]**
`estimate_dynes(spec, control_algo = set_algorithm_em(...))`. The originally proposed flat
`set_algorithm_abem()` is superseded by **four constructors, one per algorithm
concern, nested under the EM constructor** so every cross-object rule has one home:

- `set_algorithm_em(n_sequences, max_iterations, accept_quantile, growth_quantile,
  stop_quantile, tolerance, max_retries, seed, em_trace_se = FALSE,
  augmenter = set_augmenter_options(), weights = set_weights_options(),
  optimizer = set_sgd_options())` — the ascent-based MCEM loop (D18) plus the **only
  `seed`**: one RNG stream governs augmentation draws, batch selection, and
  resampling (per-block seeds rejected as an irreproducibility trap).
- `set_augmenter_options(routine = c("mcmc", "random", "sim"), burn_in, thinning,
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
- `set_weights_options(weighting = c("importance", "uniform"), use = c("importance",
  "resampling"), resampling_scheme = c("stratified", "residual", "random"),
  transformation = identity, refresh = FALSE, ess_threshold = 0.5)` — how the
  E-step expectations are weighted (D13, D14). A user-supplied custom weight
  function is dropped: the ABMCEM accept/stop machinery requires a variance
  estimator per scheme (D15), which an arbitrary function cannot supply.
- `set_sgd_options(variant = c("minibatch", "full"), batch_size, batch_scheme =
  c("weighted", "cyclic"), step_size, step_schedule = c("constant", "adagrad",
  "adam", "momentum"), max_iterations, tolerance)` — the M-step (D17).

Argument names are **descriptive, never Greek**: `accept_quantile` /
`growth_quantile` / `stop_quantile` for the ascent-based MCEM paper's α/β/γ, with
the correspondence documented in roxygen. The parent constructor's name is
settled as `set_algorithm_em()` (algorithm-naming: `set_algorithm_<family>()`,
returning an object with the shared `goldfishAlgorithm` superclass —
post class-naming-scheme spelling, ADR-0031); the three
nested component constructors take the `set_<component>_options()` sub-control
names — `set_augmenter_options()`, `set_weights_options()`, `set_sgd_options()` —
deliberately *not* the `set_algorithm_<family>()` pattern, because they are option
bundles nested under the algorithm object, not algorithm objects themselves (they
carry no `goldfishAlgorithm` superclass). abmcem ships the surface and owns any
final adjustment. Each child constructor validates only its own
arguments; **all cross-object rules (the D13 validity matrix and precedence
table) execute in `set_algorithm_em()`'s constructor**, since siblings cannot see each
other — inconsistent-but-ignorable combinations warn and are ignored, impossible
ones abort. One such construction-time warning is the **cold-start guard**:
`routine = "random"` combined with a non-zero warm-start or user-supplied θ₀ (via
`set_algorithm_newton()`'s `initial_parameters`) warns that uniform draws will sit far
from the target at iteration 1 and the E-step weights may collapse, recommending
`routine = "sim"`/`"mcmc"` or θ₀ = 0 — the settle-at-construction complement to D14's
runtime cold-start diagnostic (see the cold-start resolution in Open Questions).

The estimand is the joint multi-layer model of D19 (multiple modeled RE and PE
layers, per layer × flavor rate and choice formulas, θ the concatenation across all
modeled sub-models); event-stream estimators continue to abort on panel focal layers,
naming `estimate_dynes()`. **`estimate_dynes()` takes a `make_joint_specification()`
object** (`make-multivariate-spec`) — the earlier open question of how the multi-layer
specification is passed is resolved to consuming that constructor; the interim
named-list surface is dropped. *Rejected:* overloading `estimate_dynam()` with an
`algorithm` switch — panel augmentation changes the estimand's data requirements and
the result's uncertainty semantics; a distinct verb keeps both surfaces honest (and
matches the dev plan's naming). *Also rejected:* the flat single constructor (this
decision's original shape) — the four concerns have disjoint validation and
future-extension surfaces, and a flat argument bag hides the cross-object rules.

### D2 — Step families as contracts mirroring the writer strategy
Three constructor families, each returning an object with `init(spec, waves, control)`
/ step / `finalize()` hooks, dispatched by the `set_algorithm_em()` controls (D1):

- **Augmenters** — `augment_seq_random()` (iid-uniform times with within-chain
  sorting over the wave-diff flip set, D20; prototype `getChainSample()`),
  `augment_seq_sim()` (constrained sequential simulation from the model at the
  current parameters, driving the `multi-process-walk` handle and reusing the
  `process-simulation` per-step drawing core under wave-endpoint conditioning;
  prototype `getChainSampleFromModel()`), `augment_seq_mcmc()` (MCMC moves on a current
  sequence: the permute + shift move set with rate-based time redraws, D20 — the
  RSiena-studied insert/delete excursion moves remain future extensions, D16;
  prototype `permuteChainSample()`). Contract:
  `augment(data_window, theta, control) → sequence(s)`, where `data_window`
  carries the full wave span — snapshots at every wave, the compiled shared
  `augmentation_recipe` (D22), and the observed (modeled and exogenous) event
  streams — since the MCMC chain is global over the whole sequence (D19). Each
  augmenter reads the shared static recipe and keeps only a light **mutable
  per-draw cursor** over it (D22). Every returned sequence MUST hit every wave
  snapshot exactly and reports its **log proposal density over the full generative
  path** (D20), stored permanently with the sequence (D14). The MCMC
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
The evaluator's engine is one C++ call over a list/pool of preprocessed objects,
returning per-sequence logLik/score/Fisher at theta with zero optimizer iterations.
The pooled unit is the **`multi-process-walk` merged walk's per-fid output** (one
flat `goldfishPrep` (post class-naming-scheme spelling) per fid,
`stat_mat_update` + pointers + broadcast
encoding — the memory-efficient representation), so a multivariate spec's pool
evaluates through the same contract without a separate assembly path. Rate/probability
computation at a given process state reuses the estimation kernels (the same
`process-state-evaluators` the walk handle's `walk_evaluate()` wraps), replacing the
prototypes' hand-rolled `getDyNAMRates()`/`getDyNAMChoices()`. This is
`evaluate_sequence_pool()` (D2), built on the `walk_evaluate()` substrate
`make-multivariate-spec` D6 landed. The B1 spike (K ∈ {10, 100, 1000, 10000}
sequences on the packaged `social_evolution` dataset, wall time + peak RSS,
R-loop-per-sequence vs batched C++) confirms or revises this before the evaluator
phase. *Rejected:* per-spec R calls as the engine
(sugar only — K×event-loop overhead in R); gather-format pools (the stacked event rows
duplicate what broadcasting avoids; no collective-estimation gain, OQ A1/B1 answers).

**Two evaluation substrates, not one — do not conflate them.** The phrase "reuses the
estimation kernels (the same `process-state-evaluators` … `walk_evaluate()` wraps)"
above collapses two distinct code paths that must stay distinct in the implementation:

- The **augmenter** draws the latent path one event at a time. It reaches
  rate/probability through `walk_evaluate()` → `evaluate_process_state()`, which is a
  **pure-R reimplementation** of the per-event math (`stat_mat %*% theta`,
  `stable_softmax()`, the exact-time hazard) — it makes **no call into `src/`**. It
  reads the live statistics `walk_fold_engine()` maintains incrementally, so advancing
  the cursor is O(1) per event. This is the path that replaces the prototypes'
  `getDyNAMRates()`/`getDyNAMChoices()`.
- The **evaluator** scores a finished sequence (logLik/score/Fisher over the whole
  event stream at theta). It reaches the **compiled** `compute_*_selection()` kernels
  and the `event_reductions.{cpp,h}` primitives — one C++ pass over all events, not a
  per-event call.

The two implementations of the same per-event math are pinned together at 1e-10 by the
consistency test (`test-process_state_evaluators.R`, extended to the multi-event walk
by `test-walk_handle.R`). That pin is a **reused correctness contract**, not merely a
regression guard: the augmenter's proposal density `q` (R mirror) and the evaluator's
target density `f` (C++ kernel) must agree fid-for-fid or the D14 importance weights are
silently wrong, and that agreement is exactly what the pin already guarantees. DyNES's
batch-vs-replay test (task 3.5) extends the same pin; it does not re-establish it.

Corollary — the evaluator's only genuinely new C++ is the **K-loop batching wrapper**
over the pool; the per-event math is 100% reuse of code that already clears the 1e-6
baselines. The **decode-one-event-and-reduce-without-materializing-the-dense-stat-matrix
fusion** (`apply_broadcast_updates` + `event_score_row`/`accumulate_event_information`)
is a *conditional* second piece, built only if the B1 spike shows the dense-gather pool
losing on memory; until then the batching wrapper over the existing dense path is the
baseline (this is the Option A/B/C fork task 1.1 measures).

**The gather stack `stat_all_events` IS the dense expansion, and it is θ-free.** The
kernels do not consume the compact flat/broadcast form: `gather_()`
(`gather_sender_model_r` and siblings) replays the flat updates into a live dense
`stat_mat` incrementally (cheap, O(1)/event) but then `.gather_reduce`s and `rbind`s the
risk-set rows into `stat_all_events` — the `Σ_e |risk_set_e| × p` stack D3 lists as the
*rejected* pool format. So "the pooled unit is the merged walk's per-fid flat output …
without a separate assembly path" describes only **storage**: the kernel still eats the
expanded stack, and the expansion runs per sequence. Crucially `stat_all_events` never
touches θ (`parameters` enters only as the kernel's `stat_all_events %*% theta`), so the
pool can store the **compact flat** and expand-and-discard one sequence at a time — peak
memory `K × compact + 1 × transient dense stack`, not `K × dense stack`. D4's
memory blow-up therefore comes **only from caching the expanded stacks**, never from the
pool itself.

Because reweighting re-evaluates every pooled sequence at each new θ, the θ-free property
turns D3/D4/B1/B3 into one **cache-vs-recompute triangle**:

| variant | memory (K seqs) | per-EM-iteration cost |
| --- | --- | --- |
| A. cache dense stacks | K × heavy stack | GEMV only (fast reweight) — the D4 blow-up |
| B. cache compact flats, re-expand in R | K × compact + 1 transient | re-expand in R every iteration |
| C. cache compact flats, fuse decode-reduce in C++ | K × compact | decode-reduce in C++, no dense stack ever |

Option C is the decode-reduce fusion above — the only path both light **and** free of
re-paying R expansion each iteration. B1 is really measuring A vs B vs C, with the
EM-iteration reweighting (not the one-shot score) as the tiebreaker: that is where a
cached stack's GEMV beats a recomputed one, and where C's C++ decode beats B's R `rbind`.
See D14 — its "θ-free broadcast statistics object (the expensive part … re-reduced at
each new θ)" is concretely this `stat_all_events` expansion; the caching decision there
and the engine decision here are the same object seen from two rooms.

**But the triangle collapses to Option A for the DyNES estimand — the fusion is
deferred, not merely conditional.** The A/B/C tradeoff turns on the gather stack being
heavy, and that is **family-dependent** (`.gather_reduce` + the three gather assemblers):

| family | per-event expand | rows stored / event | stack footprint |
| --- | --- | --- | --- |
| choice (receiver\|sender) | none (slice n2 rows) | ~n | O(events · n · p) — thin |
| rate (sender) | `.gather_reduce`, O(n²·p) | ~n | O(events · n · p) — light |
| dyad (REM / DyNAM-MM) | all present dyads | up to n² | O(events · n² · p) — **heavy** |

Only the **dyad-indexed** family stores O(n²) rows/event — the sole memory blow-up
Option C exists to avoid. DyNES estimates each flavor as a DyNAM **rate × choice**
competing process (`dynes-estimation` spec: "each modeled layer × flavor carrying rate
and choice formulas"), i.e. the two **light** families; the heavy dyad family (REM /
coordination) is **not on the panel-state critical path**. So the pool footprint is
**O(K · events · n · p) — linear in n, not quadratic**, and:

- **choice**: thin stack → caching (A) is trivially cheap;
- **rate**: stored output is only ~n rows but `.gather_reduce` is O(n²·p), so caching the
  *reduced* n×p stack (A) is both light in memory **and** skips the reduce — A dominates
  B (re-pays the reduce) and C outright.

Therefore for DyNES v1 the engine is **Option A**: loop `compute_multinomial_selection`
(choice) and `compute_poisson_selection` (rate) over cached, θ-free, reduced gather
stacks, θ reweight as their internal GEMV — zero new kernels, one batching wrapper. The
**decode-reduce fusion (Option C) is deferred**: it has no rate/choice justification and
becomes relevant only if a REM-flavored panel layer is ever in scope. B1's dyad-indexed
large-n cell is a bandwidth-ceiling / future-proofing probe, **not** a gate on the v1
engine decision — the actor-oriented rate+choice cells are the load-bearing measurement.

### D4 — Pool storage: in-memory flat objects; budget set by measurement; DBI spill fallback **[spike-gated]**
The pool is an in-memory list of flat preprocessed objects. There is **no fixed byte
threshold in this decision**: the OQ B3 answer's original ~2 GB and this design's
later ~5 GB both anchored to the pre-`make-multivariate-spec` heavy preprocessed
representation, which no longer exists on this path — D3 evaluates over the broadcast
(`stat_mat_update` + pointers) representation, whose footprint is far lighter and
scales along too many axes (n × events × K × #modeled fids × effect count) for a
single per-machine scalar to bound honestly. Acceptance is therefore **empirical**:
the pool fits the available (or a configured) memory budget as measured by the B3
spike on the broadcast representation, divided across any parallel workers. Pool
growth is bounded per EM iteration by D18's retry rule but unbounded across iterations
— deliberately no cap argument and no warning; the spill fallback is the whole memory
story. The B3 spike profiles a grid (n × events × K) and sets the practical target; if
the budget is exceeded, the fallback order is (1) a broadcast-exploiting on-disk
variant of the default format, (2) the existing DBI writer — noting its caveat that
the gather event stack is itself the memory/disk heavy part. No new storage format is
invented, and no threshold is fixed, before the measurements.

### D5 — Full re-preprocess per sequence entering the pool (OQ B4)
Each sequence entering the pool is preprocessed from scratch through the existing
recipe path. A mutated sequence invalidates statistics from its first perturbed event
onward, so suffix recomputation approaches full reprocessing in expectation;
incrementality is premature until the B1/B3 spikes show re-preprocessing dominating
the E-step (> ~20% rule from the OQ). The initial-state materializer
(single-data-object D16) makes each wave's starting state cheap to construct.

**Per-wave start states are materialized once, not per draw.** Because every augmented
sequence hits every wave snapshot exactly (endpoint-hitting, D8/D20), the full process
state *at* each wave is identical across all sequences and all EM iterations, and is
θ-free — only the between-wave paths differ. The per-wave start states are therefore
materialized once (via the D16 materializer) and cached in the static
`augmentation_recipe` (D22), shared by every sequence's re-preprocess rather than
reconstructed per draw; only the θ-dependent re-preprocess of each drawn between-wave
path is per sequence.

**Per-wave-window preprocessing is also a correctness mechanism, not only a
performance one — it makes the trailing compensator automatic.** The rate kernel scores
`−Δt·Λ` on every interval including the event-free trailing stretch `[t_last, t_{m+1}]`
(D20 invariant 3), but the merged-walk right-censoring contract inserts RC boundaries
only at **modeled events** (cross-process) and **covariate events** — a panel wave time
is neither, so no existing rule emits an interval there. If each between-wave path is
preprocessed as its own observation **window** anchored at the D5 per-wave start state,
then `t_{m+1}` *is* that window's end and the ordinary end-of-observation right-censoring
preprocessing already performs emits the trailing `−Δt·Λ` with **no new RC logic**.
Endpoint-hitting makes the RC row's statistics consistent (the augmented state at
`t_{m+1}⁻` equals the observed snapshot); only `Δt` is augmentation-specific, and the
window-end rule owns `Δt`. The alternative — preprocessing the whole multi-wave sequence
as one stream with waves as interior points — would require **explicitly injecting an
interior-wave RC boundary for every timed rate fid at every wave time** (new behavior,
its own test), and is easy to get wrong precisely because the merged-walk RC contract
*looks* complete. So the per-wave-window choice is load-bearing for D20 invariant 3, not
just for cheap start states / θ-free caching.

**MCMC caveat (interacts with D20).** For `augment_seq_mcmc()` this "from scratch"
pass is a *second* preprocess of a sequence the D20 proposal evaluator already
preprocessed once inside the accept/reject step (to get `f(Ω′)` for the MH ratio). So
for MCMC draws the pool-entry re-preprocess is redundant with a pass already paid; for
`random`/`sim` draws — which never evaluate a likelihood to generate — it is literal
and unavoidable. This redundancy is exactly the D20 cache open question (whether the
proposal evaluator's return value becomes a pool-entry cache), and the MCMC path is
the case that tips it toward **reuse**: the preprocessed object and log-likelihood of
a retained draw are already in hand.

**Why the reuse is retain-vs-discard, not a recompute gamble.** The pool entry splits
into a **θ-free broadcast statistics** object (sequence-specific, the expensive part —
the same object cross-iteration reweighting re-reduces at each new θ under D14) and a
**θ-dependent loglik at θ_ref** (a cheap reduction on top, one field of D14's permanent
triple). The target `f(Ω′)` is `statistics · θ` reduced over the whole path, so
evaluating it **necessarily materializes the full statistics** — there is no
likelihood-only shortcut that skips them (the D20 frozen-rate simplification touches
only the *proposal* density `q`, never the target `f`). Hence caching is purely a
memory decision (retain the object vs discard it), never extra computation: reuse
**cannot** be CPU-worse than D5, it strictly deletes the second preprocess. And for an
MCMC retained draw θ_ref = θ_k, the iteration it was drawn in, so the accept/reject
step produces **both** fields of the permanent record at exactly the θ that becomes
θ_ref — D5's "from scratch" recomputes a record already in hand.

Resolution stays gated on the D20 note; D5's baseline "from scratch" is the worst case,
not a mandate to recompute what MCMC already holds. What gates it is not cost but the
two conditions in D20's note: widening the `eval_fn` return contract (which crosses the
abmcem carve-out seam) and asserting the evaluator's statistics layout is identical to
D4's pool/gradient representation.

### D6 — RSiena's ML estimator informs the sequence-sampling statistics (OQ B5) — **v1: de-scoped to a written justification + optional cross-check**
The proposal-space questions this decision opened — endpoint-hitting paths
(Hamming-length orderings and permutations), whether excursion moves
(create-then-dissolve inside an interval) enter the proposal space, acceptance ratios,
and MC-error / convergence diagnostics — **are already settled in this design**: the v1
move set (permute + shift), its windows, proposal densities, and the
`α = [f(Ω′)/f(Ω)]·[q_rev/q_fwd]` acceptance ratio are fully specified in D20; the
diagnostics feed D15/D18's `em_trace`. Reading RSiena's code is therefore **not a v1
prerequisite** — it is downgraded from a gating study to (a) a short **written
justification** that the v1 move set is complete for DyNES's estimand (below) and (b) an
**optional** acceptance-ratio cross-check against a known-correct implementation. The
identifiability question moves entirely to the simulation recovery study (**task 6.2**),
which answers it on DyNES's own estimand rather than by analogy to RSiena's.

**Why the v1 move set needs no increment/reduce (insert/delete) moves.** RSiena's
insert/delete (excursion) moves change the *number* of ministeps because a tie may
flicker on-and-off invisibly between its waves. DyNES v1 **excludes that latent path by
a modeling assumption**: the number of PE events per interval is fixed by the wave diff
(the Hamming flip set plus same-dyad ordered chains), and net-zero excursions produce no
events (D8). So the sequence-length-changing moves are not a deferred *implementation* —
they draw paths outside v1's estimand entirely. Over the resulting **fixed-cardinality**
space, permute + shift are **ergodic** at the desk: permute swaps any same-wave pair
`(h,k)` so transpositions generate the full ordering space (modulo chain order), and
shift covers the time-continuous dimension and single-PE intervals permute cannot reach.
Insert/delete would only connect *different* event counts — which v1 has none of. This
is the justification a reviewer will ask for; it is derived here, not extracted from
RSiena. **Accepted limitation:** the no-excursion assumption cannot represent a genuine
between-wave flicker; it is the minimal-flip interpretation, recorded as a D16 future
extension. Skipping the RSiena study forgoes RSiena's *quantification* of that cost, not
the assumption itself.

RSiena's ML estimator (Snijders, Koskinen &
Schweinberger 2010, *AoAS*) samples the latent micro-sequence between two observed
waves by Metropolis–Hastings over **chains of ministeps** (single tie toggles), the
chain from wave `m` to `m+1` conditioned to carry `x(t_m)` exactly into `x(t_{m+1})` —
structurally the DyNES "augmented sequence hits every wave snapshot." What is taken:

| RSiena ML move                                | changes #ministeps?          | DyNES v1 fate                                                                                       |
|-----------------------------------------------|------------------------------|----------------------------------------------------------------------------------------------------|
| permute (reorder a segment)                   | no                           | re-implemented → D20 `permute`                                                                      |
| moveMiniStep (retime one step)                | no                           | re-implemented → D20 `shift`                                                                        |
| insertPermute / deletePermute, insertDiagonal | **yes** (± a canceling pair) | deferred — these are the excursion moves (net-zero within an interval, D8/D16 future extension)     |
| insert/deleteMissing                          | yes                          | out of scope (no missing waves, non-goal)                                                           |

So the order/retime moves are re-implemented and the insert/delete (excursion) moves
are deliberately deferred. Also re-implemented as *concepts*: endpoint-conditioned MH,
the acceptance ratio's proposal-probability structure, and the diagnostics (per-move
acceptance rates, statistic autocorrelation, thinning) that feed D15/D18's `em_trace`.
**Not** taken: RSiena's actor-oriented rate/ministep probability model (DyNES uses its
own rate/choice kernels), its chain data structures, and the proposal *distributions*
(D20 uses rate-based truncated-exponential time draws, which the joint likelihood's
waiting-time densities force anyway). The Phase-1 task now produces the **written
justification note** (`.plan/DyNES/rsiena_mle_notes.md`) recording the fixed-cardinality
ergodicity argument above and the deferral of excursion moves; the RSiena code-reading /
move-mapping cross-check is **optional de-risking** folded into it if the `rsiena` repo
is on hand, not a blocker. Should the cross-check ever be done, ideas are re-implemented,
no code is copied (both packages are GPL-3, but a clean re-implementation against our
contracts is required regardless).

### D7 — Result contract: Fisher approximation + MC standard errors (OQ B6) **[→ abmcem]**
ABEM returns the parameter estimates and the Fisher-information approximation
accumulated by the evaluator at convergence; `vcov()` inverts it, and the result
additionally carries MC standard errors and the **`em_trace`** per-iteration
diagnostics object (D18) — the ascent trajectory, ESS, pool bookkeeping, and MCMC
chain statistics — as a documented part of this contract, so convergence problems
are diagnosable from the result alone. `summary()`
displays asymptotic and MC error side by side so users see when the pool, not the
data, limits precision. Fixed-parameter handling follows the prototypes'
`unfixed_params()`/`std_error_fixed_params()` (NA rows/cols for fixed positions).

### D8 — Panel augmentation trigger (formula reference) and wave diffing
There is **no separate panel-semantics flag**. A panel-observed layer
(`observation = "panel"` in the living `single-data-object` spec) becomes an
augmentation target **iff it is a modeled process** (its own focal/dependent layer
carries rate/choice formulas in the multivariate specification). A modeled panel
process is always augmented by the chosen model-driven routine, and any fid reading
its latent state is coupled to it (D19). When a panel layer is referenced **only as
an exogenous covariate** (it carries no formulas describing its own dynamics), it is
**not** augmented: it stays a **static step-covariate** — state jumps only at wave
times (the living `single-data-object` panel behavior), the layer is *not* latent,
contributes no Monte-Carlo variation, and no random sampling of its between-wave path
is done. There is no per-layer static-vs-random choice: an exogenous-only panel
reference is always static. A panel layer referenced nowhere is likewise not
augmented. `augment_seq_random()` survives as an augmenter routine for *modeled*
panel processes (and as the MCMC initializer, D16); it is simply never the treatment
of an exogenous-only reference. Wave diffing is unchanged: a panel layer's rows are
snapshots; consecutive waves diff into the candidate flip set per interval, with
wave times as hard boundaries the augmented sequence must hit. Flavors are arbitrary, not just creation/dissolution:
the diff consumes the layer's transition/support specification and inverts the
(from-value, to-value) → flavor mapping, which MUST be injective — the flavor is
never a latent variable. A value change larger than one allowed step decomposes into
a **per-dyad ordered chain** of events (0→2 under ±1 transitions is two events with
a forced order); net-zero changes produce no events (excursions are out of v1, D16).
**A panel (latent) layer MAY be two-mode** — a disjoint mode-pair under
`multimode-network-support`'s mode map (e.g. an advice layer `{staff}×{director}`).
Wave-diffing and the per-dyad ordered chains run over its n1×n2 dyad space exactly
as over a one-mode n×n space; the candidate flip set is the two-mode dyad set.
A coupled RE reader lives on its own mode-pair block (`make-multivariate-spec` D8)
and reads the latent layer across a **whole shared mode** (identity-conforming) —
subset/nested coupling (a mode ⊂ a union containing it) is out of v1 and rejected at
composition.
v1 data restrictions: all panel-flagged layers share **one wave grid** (nested/
misaligned grids — e.g. yearly surveys plus weekly ones — are a recorded future
development, see the V2 note below); the **node set is fixed** over the whole period
(composition changes / presence windows are a future development); an observed RE
falling exactly on a wave time belongs to the **earlier** interval (it precedes the
snapshot). A *modeled panel* layer is modeled for **all its flavors or not at all** —
partially specified panel layers abort (D19). **By contrast, an RE layer MAY be
partially modeled**: unmodeled flavors are not an error — they update network state as
exogenous events while only the keyed flavors are modeled processes (D19; living
`multivariate-specification` "RE subset modeling stays legal"). A fully unmodeled
panel layer keeps the existing exogenous change-list semantics (state jumps at wave
times), untouched. Diffing is a conversion-module function also usable standalone (the
upstream snapshot-diff-verb proposal to manynet remains open and non-blocking). The
diff output — flip sets, per-dyad ordered chains, and the injective (from,to)→flavor
map — is compiled **once at `estimate_dynes()` entry** into the shared
`augmentation_recipe` that every augmenter reads; that plan, its membership-only
risk-set boundary, and its cached per-wave start states are specified in **D22**.

**V2 note — multiple panel grids (future development).** When two panel layers are
observed on *different* grids, sequences remain generable but more constrained. Every
panel layer's wave times are hard boundaries the one shared global walk must cross, so
the **union** of the grids partitions the timeline while each layer is pinned only at
its **own** snapshots. The finer-gridded layer therefore carries *more* restriction on
its sampling times (its own snapshots chop its free windows short), while the coarser
layer floats its flips across many fine sub-intervals. Coupling (one layer reading the
other's latent state) only **sharpens the rates** through the shared state at each
crossed snapshot — it adds no endpoint constraints: a layer's sampling-time freedom is
set by the tightness of its *own* grid, independent of the other's. This makes the
nested-grid extension a matter of unioning boundaries into the wave-diff and
per-interval windowing, not new estimand machinery.

### D9 — Augmenters are external drivers of the `multi-process-walk` handle
The stepping substrate is `make-multivariate-spec`'s walk handle, not a callback
bolted into the batch recipe loop. Control inverts: the augmenter **owns the loop**
and steps a stateful handle — `walk_open(spec, data)`, then
`walk_advance(handle, t)` (apply exogenous events up to t),
`walk_evaluate(handle, fid, theta)` (the fid's masked rate/choice at the live
state), and `walk_inject(handle, event)` (apply the sampled event to the shared
state). Batch preprocessing is the degenerate driver that injects the observed
sequence; `augment_seq_sim()` is the hybrid driver conditioned on the wave
endpoint; the general `simulate()` (`process-simulation` change) is the
unconditioned generative driver. One substrate, several drivers — this is the
compute/emit separability `flavored-processes` D10 reserved and
`make-multivariate-spec` D6 realized.

The augmenter-side risk-set restriction (observed pairs with remaining
support-applicable events, D20) happens **in R**: the driver reads the
`walk_evaluate()` rate/choice vectors and subsets + renormalizes — no risk-set mask
argument is added to the C++ kernels in v1 (worth revisiting only at large n, D11).
The static `augmentation_recipe` (D22) supplies only the flip *membership* universe;
this R-side `walk_evaluate()` read is the live support applicability.
Consequently this change **no longer implements a per-event hook in the recipe
loop** and no longer modifies `preprocess-output-writers` — that capability's
extension points (sampling writer, parallel chunking) stay documentation-only,
untouched by this change.

### D10 — Parallelization: sequence-level only, under a non-nested thread budget **[spike-gated]** (map seam → abmcem)
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
behavior). `set_algorithm_em()` gains `n_cores` (default honoring CRAN's 2-core
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

### D12 — Process simulation: moved to the `process-simulation` change
The general `simulate()` surface — per-family timing strategies (exact /
fixed-template / pseudo-time), stopping rules, the explosion guard, the
coordination rejection scheme, flavored competing-process draws, and the
evaluator-compatible pool output — is **no longer this change's**. It is a
family-agnostic S3 generic driving the `multi-process-walk` handle, and it lives in
the standalone `process-simulation` change (that change's design records the
decisions and rejected alternatives — including the rejected nonparametric-baseline
recovery for ordinal fits). This change **consumes** it: `augment_seq_sim()` (D20)
reuses that change's per-step drawing core under wave-endpoint conditioning, and the
recovery study (D19) simulates panels through it.

### D13 — Augmenter × weighting validity matrix **[→ abmcem]**
Uniform weighting of the E-step is valid exactly when the proposal *is* the model's
conditional distribution `p(sequence | endpoints, θ_k)`:

| `routine`      | draws from                                 | `weighting = "uniform"`            | importance weights                         |
|----------------|--------------------------------------------|------------------------------------|--------------------------------------------|
| `"mcmc"`       | ≈ target at θ_k                            | valid within the drawing iteration | likelihood ratio for cross-iteration reuse |
| `"random"`     | uniform over orderings/times               | **invalid** (mis-weighted E-step)  | required: target / uniform density         |
| `"sim"`        | sequential model draws, endpoint-distorted | invalid (conditioning bias)        | required: target / product of draw probs   |

`weighting = "uniform"` is therefore accepted only with `routine = "mcmc"` and only
while no stale (previous-iteration) sequences remain in the pool (i.e. with
`refresh = TRUE`); every other combination warns and switches to importance
weighting — the default. This matrix, the batch-scheme/gradient coupling (D17), and
the refresh-inapplicability rules (D14) form the **precedence table**
`set_algorithm_em()` enforces via warn-and-ignore (e.g. cyclic batches ⇒ weight-based
selection arguments are ignored with a warning; the dominant setting wins). The
prototypes' random-draw + equal-weight combination is deliberately not carried into
the API: it estimates an expectation under the uniform law, not under the model.

### D14 — Weight state model: per-sequence reference records, refresh, ESS guard **[→ abmcem]**
The "θ-free broadcast statistics object (the expensive part, re-reduced at each new θ)"
this decision refers to is concretely the gather-stack expansion `stat_all_events` that
`gather_()` builds (see the D3 note): pure statistics, no θ, `Σ_e |risk_set_e| × p`.
"Re-reduced at each new θ" is the kernel's `stat_all_events %*% theta` GEMV. So D14's
retain-vs-discard caching decision and D3's cache/re-expand/fuse engine decision are the
**same object seen from two rooms** — resolve them together, not separately.

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

**Cold-start diagnostic — distinct from the guard [→ abmcem].** The "never at
startup" rule above silences the *recurring* ESS-guard warning at iteration 1, where
low ESS is expected and a redraw is pointless (see the cold-start resolution in Open
Questions). It must **not** silence a genuinely pathological cold start: a **one-shot**
diagnostic — a different message from the guard warning — fires when the startup ESS
falls below a hard **pathology floor** (well under the 0.5 guard, e.g. ESS/K in the
"a handful of sequences carry all the weight" range), naming the proposal/target
mismatch and the levers (`routine = "sim"`/`"mcmc"` with `initialize = "sim"`, or
θ₀ = 0). It is a diagnostic only — it never redraws (a uniform redraw is statistically
identical) and never grows the pool. At the default θ₀ = 0 the `random` proposal is
near the target, so this diagnostic stays quiet on the default path; it earns its keep
on the `random` + non-zero-θ₀ corner D1 also guards at construction.

Refresh and ESS-triggered redraws share one implementation — warm-started chains
(D16). **Resample-then-mutate** (particle-filter resample-move: O(K) weighted
resampling, then a few mutation moves to break duplicates) is recorded as a future
optimization, not a third code path — warm-started chains already capture most of
its saving. **Within-SGD weight refresh is dropped for v1** (each refresh at a
proposed θ′ costs a full-pool likelihood pass, defeating mini-batching): weights
are fixed for the duration of each M-step, so no provisional-weight state exists —
the pool's canonical weights only change between EM iterations.

### D15 — Q and its standard error as internal S3 generics **[→ abmcem]**
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

**Carve-out seam (who builds the classed object).** The classed E-step object itself —
its class tag (`estep_is` / `estep_resampling` / `estep_uniform`) and the per-sequence
quantities + weights it carries — is **constructed by this change's evaluator**
(`evaluate_sequence_pool()`, task 4); it is the evaluator's return type, not a wrapper
abmcem adds. `compute_q()` and `compute_ase()` — the generics that dispatch **on** that
class — are `abmcem`'s (the accept/grow/stop machinery consumes them). So this change
owns the object and its class contract; abmcem owns the methods that read it. The
`sequence-augmentation` spec states the classed return; abmcem's spec states the
generics.

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

### D17 — M-step: one SGD over the concatenated θ; batching semantics pinned **[→ abmcem]**
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

### D18 — EM control flow: θ₀, bounded pool growth, hard failure, `em_trace` **[→ abmcem]**
- **θ₀**: user-supplied through the existing `initial_parameters` mechanism in
  `set_algorithm_newton()`, which gains a **warm-start option** (draw one random
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

**Separability: consume the multivariate spec's coupling detection, don't
re-derive it.** `make-multivariate-spec` D4 already marks each fid `coupled` iff any
of its effect arguments or support-constraint atoms reads a **modeled** panel layer's
state (direct reference; not transitive). A fid reading only a static exogenous panel
covariate is *not* coupled — nothing about it is latent. `estimate_dynes()` reads that
`coupled` column rather than recomputing PE-independence at sub-model granularity.
Behavior on the resulting cases, aligned with `make-multivariate-spec` D4 (the newer
decision):
- **All fids separable** → abort: no panel layer is a modeled dependent process, so
  nothing is latent. The error names `estimate_dynam()`, explaining the specification
  fits DyNAM (not DyNES) and that its panel layers would only be considered as static
  exogenous covariates. This is **reachable** through `make_joint_specification()`
  whenever every panel reference is exogenous-only (D2 composes such specs; the
  DyNES-viability check is estimation-time) — catching a user who reached for
  `estimate_dynes()` on what is really a DyNAM model.
- **Mixed** (some coupled, some separable) → **proceed** with a cli message naming
  the separable fids (their likelihood terms touch no latent path, so joint
  estimation equals separate estimation for them). This supersedes this design's
  earlier "mixed also aborts" — the multivariate surface owns the policy and it
  informs rather than aborts.
*Rejected:* automatic internal partitioning (a separate standard-estimation path and
block-structured vcov inside `estimate_dynes()`) — the joint fit already handles the
separable fids correctly; the message is a user-guidance nicety, not a fork.

**Node-space generality** (`make-multivariate-spec` D8): the joined RE and PE
layers MAY be one- or two-mode over one shared mode-map object, and dependent
layers MAY be over *distinct* mode-pairs. Every cross-layer coupling read
(an RE reader reading a modeled PE layer's latent state) conforms by **mode-set
identity** — the reader and the panel layer share a *whole* mode. A read bridging
a mode *subset* to a union containing it is rejected at composition (Gap B, future
development), so `estimate_dynes()` never sees one; the augmenter and the batched
evaluator therefore evaluate every coupled fid on its own mode-pair block with no
subset projection required. The two-mode panel case (D8) is the concrete driver.

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
scale, and stored with the sequence (D14). Endpoint-hitting being guaranteed by
construction, production **trusts** it — the endpoint validator runs in tests and an
opt-in debug path, never as a per-draw state-replay on the hot path. Each augmenter
draw is a **pure function of an injected substream state**: the augmenter is keying-
agnostic (it neither knows nor chooses the RNG substream it consumes), which keeps
draws reproducible across worker counts and unit-testable in isolation (inject a known
state → deterministic sequence). The map seam owns the keying = `(iteration,
draw_index)` and the index-ordered reduction (resolved open question; abmcem D10).

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

  **Tied observed REs.** "Globally-next unplaced modeled RE" is well-defined even when
  two observed REs share a timestamp: they are the **only** tie an augmented sequence can
  carry (a sampled PE never equals an anchor by the open-interval guard above; PE–PE
  draws tie with probability zero), and they are forced **adjacent** — once the first is
  placed at `t`, the next anchor is the second (also `t`) and any remaining PE's support
  (0, next anchor − t) collapses to `∅`. Their mutual state-fold order is **fixed input,
  not sampled**: DyNES neither invents a tie-break rule nor draws the order — it consumes
  the observed event stream's order, the quantity the `tied-event-times` change makes
  explicit and user-controllable. The one local requirement is that the augmenter (`q`)
  and the evaluator (`f`) fold state over tied observed events in **identical** order, or
  the importance weight `f/q` is silently wrong though either order alone is a defensible
  fit — a special case of the `q`/`f` agreement the D3 note already pins at 1e-10, not
  new machinery.
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
  retained draw's preprocessed statistics and log-likelihood could enter the pool
  without re-preprocessing, feeding directly into that member's D14 permanent record
  (the `loglik@θ_ref` field and the θ-free broadcast statistics, with θ_ref = θ_k).
  The reused pair is **not** "the last proposal's return value" — the retained state
  after thinning is the chain's *current accepted* state, which may have been accepted
  several rejects before the thinning point. The invariant is a chain-loop one:
  `(current_stats, current_loglik)` are adopted together on accept and held together
  across rejects (the proposal's freshly computed pair is discarded on a reject), then
  frozen into the pool at the thinning point. `current_loglik` is already a required
  cache — the MH ratio needs `f(Ω)` in its denominator every step — so only the
  statistics object is newly retained, at O(1) live chain memory (the pool already
  holds K such objects). Two conditions gate turning this into contract: (1) the
  `eval_fn` return must widen from `(loglik, rates)` to also surface the statistics
  handle, which crosses the abmcem carve-out seam below; (2) that statistics layout
  must be asserted identical to D4's pool/gradient representation (else a cheap layout
  adapter, still far below a full re-preprocess). Whether the closure's return value
  becomes a pool-entry cache is deliberately left open for v1 — gated on coordination, not
  on cost (see D5's MCMC caveat: reuse is retain-vs-discard, never CPU-worse than D5).
  **D23 enumerates the alternatives (A0–A3) and sets the direction:** v1 ships the A0
  from-scratch baseline; the source-agnostic optional-attachment shape (A3) is the
  recommended fast-follow, and the accepted-not-last-proposal invariant above is its
  correctness guard.

  **Carve-out seam (who builds `make_proposal_evaluator()`).** The closure wraps a
  single-sequence preprocess + one likelihood pass with the named-pair rate-at-position
  query — i.e. **this change's** evaluator primitive (the `compute_lik_seq()` / batched
  `evaluate_sequence_pool()` machinery of D2/D3, task 4). But it is *built* — θ_k baked
  in, one per EM iteration — and *injected into the augmenter's `init()*` by **abmcem's**
  EM loop (D18). Division: this change ships the single-sequence evaluation + rate-query
  primitive; `abmcem` constructs the θ_k-closed `make_proposal_evaluator()` over it and
  owns the injection wiring. Tests here stub `eval_fn` with an analytic toy likelihood
  (no preprocessing) to verify the chain's acceptance logic and detailed balance
  independently of either side of the seam.

**Three time-domain invariants (rate family) — the compensator couples the augmenter to
the kernel.** The rate kernel scores each interval as
`ℓ_e = log λ_obs − Δt_e · Λ_e` (`compute_poisson_selection`: `intervalLogL = −timespan·Σλ
+ [dependent]·lin_pred_obs`), so the augmented **times**, not merely their order, enter
the target `f`. That imposes three invariants every augmenter and the evaluator MUST
jointly satisfy, or the importance weights `f/q` are silently wrong:

1. **`q` is a density over event times, not a probability over orderings.** The weight is
   a ratio of time-densities. `augment_seq_random`'s uniform draws still count: its
   reported `Σ_j log(c_j!) − n·log L` is the uniform order-statistic density (jacobian
   included), a valid `q` even though the times are model-inconsistent — reweighting
   corrects them, at higher variance.
2. **The waiting-time rate is the walk handle's `Λ` — the time-domain fid-for-fid pin.**
   `sim`/`mcmc` draw truncated exponentials at the *same* `Λ` the compensator uses
   (`walk_evaluate()` → `.pse_eval_rate`, pinned R↔C++ at 1e-10). Draw times from any
   other rate and `q` diverges from `f` exactly where it is least visible (weights still
   normalize; they are just biased).
3. **The trailing censored interval `[t_last, t_{m+1}]` is a non-cancelling term in both
   `f` and `q`.** `intervalLogL = −Δt·Λ` fires on **every** interval including
   `is_dependent = 0`, so the elapsed event-free stretch to the wave endpoint contributes
   `−Δt·Λ` to `f` and a survival term to `q`. Both the merged walk/evaluator (must *emit*
   that interval) and the augmenter (must *account* for it) carry it; dropping it removes
   a term that does **not** cancel in the ratio (different `Λ` under target vs proposal).
   This is the likelihood-side load on the merged single-clock walk's right-censoring
   contract.

**Choice is compensator-free.** `compute_multinomial_selection` takes no `timespan`; the
choice fid is pure softmax. In the rate×choice decomposition one augmented tie flip feeds
**both** fids — its (time, sender) score through the rate compensator, its receiver
through the choice softmax — so the augmenter draws one `(time, sender, receiver)` tuple
and `q` is the **joint** over it. Building `q` per-fid and multiplying risks omitting the
time factor from the joint or double-counting the shared draw.

**Proposal and target normalize over DIFFERENT sets — deliberately; that gap is what
`f/q` corrects.** The proposal `q` and target `f` do **not** share a risk set, and it is a
mistake to try to make them equal. The proposal (`sim`/`random`) normalizes over the
**endpoint-restricted observed-flip set** — the remaining flips in the wave's Hamming
diff, "restricted and renormalized in R" (the `augment_seq_sim` bullet above) — because
drawing anything else would break endpoint-hitting. The target `f` (the evaluator's gather
risk set) normalizes over the **full structural support**: every structurally-possible
flip at that state, including flips no wave ever observes.

Example: sender `s`, between waves, with `s→j3` observed `0→1` and `s→j2` unchanged at `0`.
At the step placing `s→j3`, the choice **likelihood** denominator is `{j2, j3, …}` — every
currently-absent tie is a counterfactual creation the model weighs — but the **proposal**
denominator is `{j3}` alone (`j2` never changes across the wave, so it is not an
augmentation candidate). The rate fid restricts the same way: `q`'s sender set is senders
with a remaining observed flip, while `f`'s compensator `Δt·Λ` integrates the hazard of
**every** structurally-at-risk sender, endpoint-inconsistent "wrong-way" flips included.

So the relationship is a strict restriction `q`-set ⊊ `f`-set, and the only quantity
**shared** across the seam is the placed flip's **numerator** rate/prob (the density pin —
`rate(j3)` computed identically on both sides). The denominators differ by construction:
`q` divides by `Σ_{observed} rate`, `f` by `Σ_{full support} rate`, and the importance
weight is exactly that accumulated ratio — which is why even the model-driven `sim`
augmenter has `f/q ≠ 1`. Hard requirements: (a) every observed flip is in the full support
(trivially true — an observed `0→1` is a legal creation, so `q`-set ⊆ `f`-set always
holds); (b) each side computes **its own** normalizer over **its own** set; (c) the
recorded `q` is the restricted-set density, so the weight is `f_full / q_restricted`.
There is **no** "sets must be equal" invariant.

*Rejected:* uniform time draws in the MCMC windows (the background note's simple
acceptance ratio f′/f is exact only for uniform non-adjacent swaps; rate-based
proposals dominate, and the density bookkeeping is required anyway); auto-rejecting
support-violating proposals via zero target density (never pay a preprocess for a
proposal a lookup can exclude — and the walk is not guaranteed to return −Inf on an
illegal transition rather than error); a separate per-dyad pending-queue structure
in the simulation (the support masks already encode applicability).

### D21 — Generative-readiness completion is consumed, not built here (see `make-multivariate-spec` D9)
The joint likelihood needs every modeled DyNAM flavor complete (rate **and**
choice): the augmenters draw waiting times and receivers from rates/choices, and
the evaluator scores both halves. That completion — filling a half-specified
flavor's missing sub-model with its zero-information default (uniform choice /
uniform ordered rate; intercept-only baseline for a missing timed rate) and
warning once, erroring only when a **modeled panel** layer omits a flavor entirely
(the "all flavors or not at all" rule — panel layers only; RE layers may be partially
modeled, D8/D19) — is owned by `make-multivariate-spec`
**D9** and consumed here, not re-derived.

The decisive constraint this change imposes on D9: **only `augment_seq_sim()` drives
`walk_open`**. `augment_seq_mcmc()` evaluates through the injected
`make_proposal_evaluator()` closure (D20), `augment_seq_random()` draws over the
flip set, and `evaluate_sequence_pool()` (D2) is a batched pass — none call
`walk_open`. So completion **cannot** live inside `walk_open`; `estimate_dynes()`
runs it **once** at entry and hands the single completed spec to all four paths, so
the augmenters and the evaluator agree fid-for-fid. `make_proposal_evaluator(spec,
θ_k)` (D20) is built on that completed spec. Completed fids are marked in the
`process_map` and appear in θ (0 params for a uniform choice, 1 for an added
baseline hazard), so the result (D7) reports every auto-supplied sub-model. This
supersedes any reading of D2/D20 in which an augmenter completes its own spec.

The landed `complete_generative_spec(joint_spec, consumer, wave_times, call)` takes
`wave_times` as the K+1 period boundaries a timed rate's pin buckets on. `estimate_dynes()`
MUST supply the panel wave grid this change's wave diffing (D8) already holds — the same
boundaries used to diff waves into flip sets — as that argument when calling
`complete_generative_spec(spec, consumer = "estimate_dynes", wave_times = <wave grid>)`;
omitting it silently falls back to a single window spanning the data's event times, which
degrades a multi-wave pin to one flat plateau. This is the `abmcem`-shipped surface's
responsibility to wire (task 6.1), sourced from this change's panel data path.

### D22 — The `augmentation_recipe`: one static plan shared by every augmenter
The wave diff (D8), the per-dyad chains, the flavor map, and the per-wave start states
are all **θ-free and sequence-free** — they depend only on the observed wave snapshots,
so they are constant across every EM iteration and every drawn sequence. This change
compiles them **once, at `estimate_dynes()` entry**, into a single shared
`augmentation_recipe` and gives every augmenter a read-only handle on it plus a light
**mutable per-draw cursor** — the same static-plan / mutable-state-container split the
recipe loop already uses for preprocessing (D9; `preprocess_builders.R`). The plan
holds:

- **flip sets** per between-wave interval and the **per-dyad ordered chains** (a >1-step
  change decomposed, D8);
- the **injective (from,to)→flavor map**, validated **exactly once** here — D8 requires
  injectivity, and validating on the shared plan gives that check one home instead of
  one per augmenter;
- the **risk-set membership universe** — *which* dyads flip and in what chain order,
  **membership only**: live support applicability (a 1→2 flip is legal only once its
  dyad sits at 1) is **not** encoded here; it comes from `walk_evaluate()`'s masks at
  the current state (D9), so the recipe and the walk never carry two support views that
  can disagree;
- the **same-wave pair/constraint graph** the MCMC moves enumerate over (D20);
- the **per-wave start states**, materialized once (D5/D16): because every augmented
  sequence hits every wave snapshot exactly (D20), the process state *at* each wave is
  identical across all sequences, so it is materialized once and shared by every
  sequence's re-preprocess rather than reconstructed per draw.

Each augmenter's mutable cursor is small — `augment_seq_sim()`'s remaining
support-applicable counts, `augment_seq_mcmc()`'s live pred/succ windows over the
current order, `augment_seq_random()`'s per-interval draw scratch — and no augmenter
mutates the shared plan. The single shared wave grid is a v1 bound; multiple/nested
panel grids (D8's V2 note) extend the plan by unioning boundaries, not by changing this
contract. *Rejected:* each augmenter reading raw snapshots and re-deriving the diff per
sequence (the prototype shape — repeats θ-free work every iteration and gives the
injective-map validation no single home); baking support applicability into the recipe
(two support views to keep consistent, D9).

### D23 — MCMC retained-draw pool reuse: alternatives and the v1 default **[deferred; seam-gated → abmcem]**
The D5 MCMC caveat and the D20 evaluator-injection note leave one question open: how a
retained `augment_seq_mcmc()` draw enters the pool, given that computing its acceptance
ratio *already* materialized its θ-free statistics stack and its `loglik @ θ_k`
(evaluating `f(Ω) = stat_all_events · θ_k` cannot skip the statistics — D5). Reuse is
therefore **retain-vs-discard, never CPU-worse than D5** (it strictly deletes the second
preprocess) and **memory-neutral** (the pool holds one copy of the stack either way; the
live cost is one accepted-state stack held to the thinning point, O(1) against the pool's
K). The question is not cost but contract shape. The alternatives:

- **A0 — from-scratch re-preprocess (the D5 baseline).** Discard the chain's stack;
  preprocess the retained sequence fresh at pool entry. One code path shared with
  `random`/`sim`, no seam crossing, no layout coupling — but a redundant full preprocess
  per retained MCMC draw per EM iteration. **This is the v1 default** (and the only option
  for `random`/`sim`, which never compute a target `f`).
- **A1 — reuse via a widened return contract.** `eval_fn` widens from `(loglik, rates)` to
  `(loglik, rates, stats_handle)`; the augmenter freezes the *accepted* state's
  `(stats, loglik)` into the D14 record at the thinning point with θ_ref = θ_k. Gated on
  (a) widening the return across the abmcem seam and (b) asserting the handle's layout
  equals D4's pool/gradient representation. Condition (b) is nearly free: the chain's `f`
  routes through `compute_lik_seq()`, the per-sequence sugar over the **same** batched
  kernel the pool uses (D2/D3), so both emit the D3 Option-A reduced gather stack by
  construction — a test pin, not a converter.
- **A2 — reuse with a layout adapter.** A1 without requiring byte-identical layout: a
  cheap adapter maps the evaluator's internal statistics layout to D4's pool
  representation (still ≪ a full preprocess). The hedge if the spikes reveal incidental
  layout drift (ordering/padding) between the two paths; it drops gating condition (b).
- **A3 — source-agnostic optional cache-attachment (recommended shape).** Rather than a
  `routine == "mcmc"` fork in the pool-write path, the pool-entry contract accepts a
  sequence *with or without* a precomputed `(stats, loglik@θ_ref)` attachment: adopt it
  iff present **and** `θ_ref == θ_k` **and** the layout tag matches, else preprocess from
  scratch (A0). `random`/`sim` carry no attachment and fall through to A0 exactly as they
  must; MCMC carries one and takes the fast path. One code path, no fork, and it degrades
  safely — a stale θ_ref or a layout mismatch recomputes rather than corrupts. A3 is
  A1/A2's mechanism wrapped so the pool is agnostic to which augmenter produced the
  sequence.

*Rejected:* **cache the loglik only, recompute the statistics** — the loglik is the
*cheap* reduction (and is already held free as the MH denominator `f(Ω)`); the θ-free
statistics stack is the expensive object the SGD score (D17) and cross-iteration
reweighting (D14) both need, so this saves nothing and adds a consistency surface (a
cached loglik paired with a separately recomputed stack that must correspond to the same
Ω). Dominated by A1 on every axis. *Also rejected for v1:* **resolving it in the storage
representation** (the pool stores the compact form and expands on eval — D3 Option B/C) so
there is no heavy handoff — D3 already fixed the DyNES engine to Option A for the light
rate+choice families, where re-expansion per θ loses; this only reopens if a REM-flavored
(dyad-indexed) panel layer ever enters scope.

**The correctness guard, loud (from D20).** Any reuse path (A1/A2/A3) MUST retain the
chain's **current accepted** state at the thinning point — NOT the last `eval_fn` return,
which may be a *rejected* proposal. The invariant is chain-loop-local:
`(current_stats, current_loglik)` are adopted together on accept, held together across
rejects (the proposal's freshly computed pair discarded on reject), and frozen into the
pool at the thinning point. `current_loglik` is already required every step (the MH
denominator), so only `current_stats` is newly retained. Caching "the last proposal's
stats" is silent corruption, not a missed optimization.

**Carve-out seam (who owns what).** This change owns the statistics-emitting primitive
(`compute_lik_seq()` / `evaluate_sequence_pool()`, task 4), the pool-entry record
construction (tasks 4.2/4.3 — where the optional attachment would be adopted), and the
chain-loop invariant inside `augment_seq_mcmc()` (task 3.4). `abmcem` owns
`make_proposal_evaluator()` — the θ_k-closed closure whose return would widen to surface
the stats handle — and its injection into the augmenter's `init()`. So A1/A2/A3 straddle
the seam at exactly one point (the closure's return contract); everything else is this
change's. That single seam-crossing is why v1 ships A0 and the reuse is a coordinated
fast-follow, and why the v1 pool-entry path (task 4.2) is built **attachment-optional** so
the abmcem-side widening is the only remaining work when A3 lands.

## Risks / Trade-offs

- **Spike results overturn a gated decision** (batched C++ not worth it; pools blow
  the 5 GB budget) → D3/D4 are explicitly provisional; Phase 1 ends with a
  design-revision task that amends them and the affected specs before later phases
  start.
- **Identifiability of separate creation/dissolution parameters from sparse waves** →
  the simulation recovery study (task 6.2) quantifies it on DyNES's own estimand, with
  the optional RSiena cross-check (D6) as supporting prior art only; `summary()` exposes
  MC error so weak identification is visible, and the simulation study defines guidance
  for the vignette.
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

Purely additive surface (`estimate_dynes()`, `set_algorithm_em()` and its nested controls, step
constructors) plus one extension to an existing control (`set_algorithm_newton()`
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
- **[resolved]** How `estimate_dynes()` receives the multi-layer specification
  (D19): it consumes a `make_joint_specification()` object (`make-multivariate-spec`);
  the interim named-list surface is dropped. This change hard-depends on that one.
- **[direction set — see D23]** Whether MCMC retained draws enter the pool through the
  proposal evaluator's cache (statistics + loglik reuse into the D14 record, D20 note /
  D5 MCMC caveat). D23 enumerates the alternatives (A0 from-scratch baseline; A1 widened
  return; A2 + layout adapter; A3 source-agnostic optional attachment) and sets the
  direction: **v1 ships A0**, with **A3 the recommended fast-follow** once the abmcem seam
  is coordinated. Still gated — not on cost (reuse is retain-vs-discard, never CPU-worse
  than D5) but on the one contract condition that crosses the seam: widening the `eval_fn`
  return to surface the stats handle (the statistics-layout pin to D4's pool
  representation is nearly free — the chain's `f` already routes through the pool's
  kernel). The v1 pool-entry path is shaped attachment-optional so A3 is a drop-in.
- **[resolved]** Component names for the three nested constructors under the
  settled `set_algorithm_em()` parent — `set_augmenter_options()`,
  `set_weights_options()`, `set_sgd_options()` (the `set_<component>_options()`
  sub-control pattern, distinct from `set_algorithm_<family>()` since these are
  option bundles, not algorithm objects). abmcem ships the surface and holds final
  say, but the names are settled here to keep both changes in sync.
- **[gated by Phase 1]** The D10 thread-budget crossover (when, if ever, BLAS threads
  beat sequence sharding on the large-n cell) — resolved by the B1 spike's
  BLAS × sharding crossing.
- **[moved]** The `simulate()` surface questions (entry points/naming, ordered-family
  default mode, legal writer sinks, exogenous horizon, `max_events` default,
  coordination bookkeeping, θ-uncertainty bands) now live in the `process-simulation`
  change's design; `augment_seq_sim()` consumes whatever that change settles.
- **[resolved]** Reproducibility across worker counts — **Option B: substreams keyed
  to the sequence/draw index, not the worker.** The single `seed` (D1) seeds an
  `L'Ecuyer-CMRG` root (`RNGkind()` set at estimation entry, `withr`-restored on exit)
  that fans out into three substream families, all derived from the one seed — the
  mechanical meaning of D1's "one stream governs augmentation, batch selection,
  resampling":
  - **augmentation substreams** (`random`/`sim`, K independent draws) — the *only*
    branch that crosses the map seam. Keyed by a **structured `(iteration, draw_index)`**
    (not a flat monotone counter): the structure survives D18's dynamic within-iteration
    pool growth (draw_index simply keeps climbing, substreams spawned lazily) and lets a
    single pathological draw be regenerated in isolation ("iteration 3, draw 47") without
    replaying history. Worker identity **never** enters the key, so the drawn pool is
    identical at any `n_cores`, serial included.
  - **MCMC chain substream** — one long serial stream on the coordinator, advanced
    continuously across warm-started iterations (D16). MCMC generation is serial by
    construction (D10, D16), never crosses the seam, and is therefore worker-count-
    invariant for free.
  - **control substream** — coordinator-side, for batch selection and resampling (global
    pool ops, D14/D17), also seam-independent.

  Cross-`n_cores` bit-for-bit needs **two** guarantees, not one: worker-independent RNG
  (above) **and** a worker-independent floating-point reduction order — the map seam
  MUST collect per-sequence results and reduce them in **sequence-index order**, never
  arrival order, or the M-step sums re-round under a different worker count. The
  documented promise is therefore **two-part**: (1) the **drawn sequences** are identical
  across any `n_cores` (pure-R augmenter draws, no BLAS — holds even across machines);
  (2) the **numeric estimates** are identical across any `n_cores` **on the same
  machine / BLAS build** (compiled kernels + multithreaded BLAS round per build, so
  cross-machine bit-for-bit is never promised).

  **Carve-out split (dissolves the "shared with abmcem" awkwardness).** The seam lives in
  abmcem (D10 → abmcem) but the augmenters live here, so the decision splits cleanly along
  the seam with no cross-reach:
  - **This change (D2/D20):** each augmenter draw is a **pure function of an injected
    substream state**. The augmenter neither knows nor chooses how its substream was
    keyed — which also makes it unit-testable in isolation (inject a known state → assert
    a deterministic sequence, the endpoint/density fixtures of task 3.x).
  - **abmcem (map seam, D10):** owns the keying = `(iteration, draw_index)`, the
    index-ordered reduction, and the two-part documented promise. Mirrored into abmcem's
    D10.
- **[resolved — cold-start weight degeneracy]** The premise is narrower than it
  looks. At the default θ₀ = **0** the model is (near-)uniform — equal rates ⇒ event
  times are uniform order statistics, `softmax(0)` ⇒ uniform ordering — which is
  exactly what `augment_seq_random()` draws (D20). So at the **default** cold start
  `random` ≈ the target and the weights do **not** collapse. Degeneracy is a property
  of one specific corner: `routine = "random"` paired with a θ₀ **far from 0** (the
  D18 warm-start option, or a user-supplied non-zero `initial_parameters`), where
  uniform draws sit systematically far from `p(sequence | endpoints, θ₀)`. A redraw
  cannot fix it — resampling uniform gives statistically identical draws — which is
  precisely why the D14 ESS *guard* (a redraw trigger) is silent at startup; growing
  the pool (D18) only adds more low-weight draws. The resolution is therefore
  **diagnose-and-guide, not resample**, on levers already in the design:
  - a **one-shot cold-start diagnostic**, distinct from the recurring ESS-guard
    warning, firing only below a hard **pathology floor** (well under the 0.5 guard),
    naming the proposal/target mismatch and the levers — folded into **D14**
    (implemented in abmcem);
  - a **construction-time warning** in `set_algorithm_em()` when `routine = "random"`
    meets a non-zero warm-start / user θ₀ — folded into **D1**'s cross-object rules
    (implemented in abmcem);
  - `initialize = "sim"` (a nearer-target constrained draw, D16) as the sanctioned
    mitigation **for MCMC**, scoped honestly: `initialize` is the MCMC chain start and
    has no effect on a standalone `random` run, whose sanctioned cold start is θ₀ = 0.
- **[resolved]** Tied observed event times during augmentation. The only tie an
  augmented sequence can carry is **observed-RE ↔ observed-RE**: sampled PE times lie in
  *open* intervals strictly between anchors (D20 numeric guard), so a PE can never tie an
  RE anchor, and continuous PE–PE draws tie with probability zero (same-dyad chains are
  order-assigned regardless). A tied RE pair is forced **adjacent** by construction —
  once the first is placed at `t`, the next anchor is the second (also `t`), collapsing
  any intervening PE's truncated-exponential support to `∅`. So the only residual freedom
  is the tied pair's **mutual state-fold order**, which is **fixed input, not a latent
  quantity** — DyNES neither breaks ties with its own rule nor samples the order.
  Resolution: **defer the tie *policy* to the `tied-event-times` change** (it makes the
  observed order explicit and user-controllable; multiple-imputation-over-orderings is
  the researcher's job, the shared stance of both changes), and enforce **locally** the
  agreement invariant that the augmenter (`q`) and evaluator (`f`) fold state over tied
  observed events in *identical* order — a special case of the D3 `q`/`f` 1e-10 pin, not
  new machinery. Folded into D20 next to the "globally-next unplaced modeled RE" clause.
  *Rejected:* sampling the tie order inside augmentation — it conflates two distinct
  missing-data mechanisms (latent PE path vs. unknown observed order) and corrupts `f/q`
  for zero statistical gain.
