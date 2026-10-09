## Context

Stage A (`flavored-processes`) makes fully observed competing creation/dissolution
processes estimable; DyNES handles the panel-observed case: states seen at waves, the
event sequence between waves latent. The living `single-data-object` spec carries the
`observation = "panel"` metadata and change-list semantics for exogenous panel
covariates; `make-multivariate-spec` landed the `make_joint_specification()` composition
surface and the `multi-process-walk` handle (`walk_open`/`walk_advance`/
`walk_evaluate`/`walk_inject`) — the family-agnostic stepping + injection substrate a
model-driven sequence draw drives; `process-simulation` has since widened it
(`walk_replay`, `walk_schedule`, `walk_next_breakpoint`,
`walk_evaluate(sender =)`) and lifted its refusals of support constraints,
composition change and windowed effects (D9 amendment). Panel augmentation is triggered by a panel layer
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
phase starts. **That revision was made on 2026-09-30 (task 1.5):** D2, D3, D4, D5, D6,
D10, D15 and D19 carry dated revision blocks folding in B1
(`.plan/dynes/b1/B1_results.md`), B3 (`.plan/dynes/b3/B3_results.md`), the move-set
note (`.plan/dynes/rsiena_mle_notes.md`) and the toy prototype
(`.plan/dynes/toy/TOY_results.md`). No decision remains spike-gated. Where a revision
block and the older text above it disagree, the revision block governs.

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
  here; `augment_seq_sim()` reuses its per-step drawing core as a set of supplied
  steps (D12), and the recovery study draws its panels from it. Its exported
  surface (`simulate()`, `set_simulation_steps()`, `set_parameter_provider()`,
  `set_simulation_guard()`, `filter_simulation()`, the handle accessors) is not
  this change's to build or to re-document. GoF statistics/plots (`gof-dynes`)
  are a follow-up.
- Modeling the rate part of choice-only (ordered) data by treating timings as
  latent and imputing them through the augmentation machinery — a genuinely new
  estimand the DyNES core makes reachable, recorded as a future extension
  (completely different estimation), not attempted here.
- Missing-data / partially observed waves beyond complete snapshots — a recorded
  **future goal**, not a v1 capability. v1 validates snapshot completeness and **aborts
  loudly** on `NA` tie values rather than silently coercing them to tie-absence (which
  would manufacture spurious flips); handling item nonresponse / structural missingness
  in a wave is the deferred work.
- Per-interval factorized augmentation (independent pools, weights and chains per
  between-wave interval) — **declined for v1** in D19, recorded there as a future
  development for specifications carrying no latent-timing statistic across a wave
  boundary.
- Ordinal / multistate panel layers (K > 2 states, the `(from,to) → flavor` chains D8
  describes) — out of v1 and currently unconstructible: `add_flavor()` stamps
  dichotomous mappings only. The **count** (multiplicity) panel kind *is* in v1; it is
  a different cell, not a weaker form of this one (D24).

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
final adjustment. **Export handoff (abmcem D2/Q1):** abmcem ships
`estimate_dynes()` **unexported** (stub-augmenter-tested only; its four control
constructors export, the driver does not), so no user reaches a surface that
would only abort for lack of a real augmenter. **This change adds the `@export`**
(task 6.1) once the real augmenters (Phase 3) and batched evaluator (Phase 4)
exist — that is the point the experimental surface becomes user-reachable with a
working augmenter behind it. Each child constructor validates only its own
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

*Revised 2026-09-30 (task 1.5, from the task-1.4 toy prototype) — the signatures
as landed by `abmcem` are the contract; the shapes above are superseded where they
differ.*

- **Augmenter.** There is no `augment(data_window, theta, control)` call and no
  `init(spec, waves, control)` / step / `finalize()` hook set. An augmenter is an
  object built **once** by its constructor, `augment_seq_*(recipe, control)`,
  which closes over the compiled `augmentation_recipe` (D22). The data window is
  therefore bound at construction, not passed per call. The loop
  (`run_abem_loop()`) calls three slots:
  `draw(iteration, draw_index, theta)` returns one
  `list(data, log_proposal, restarted)` per entry of `draw_index` (`restarted`
  optional, feeding the one-per-run coupling warning); the optional
  `init(proposal_evaluator, theta)` receives the θ_k-closed evaluator once per
  EM iteration (MCMC only); the optional `diagnostics()` return goes into
  `em_trace`. `resolve_dynes_augmenter(spec, control)` in `estimate_dynes()` is
  the factory seam: it gains the recipe compiled once at entry and dispatches on
  `control$augmenter$routine`. The map seam wraps `draw` so each draw runs on its
  keyed substream (`keyed_augmenter()`), which is how the D20 "pure function of
  an injected substream" rule is met without the augmenter seeing the key. The
  toy's prototype `random` augmenter needed nothing beyond these three slots.
- **Emission.** `data` is a whole stocnet: the panel layer's rows retimed, the
  layer's change list re-sorted chronologically, and `info` **byte-identical** to
  the specification's data (`bind_candidate_data()` refuses any other `info`).
- **Evaluator.** `evaluate_sequence_pool(pool, theta, what)` returns a plain
  `list(loglik, score, fisher, degenerate)` per sequence. The classed E-step
  object is built by the **loop** (`new_dynes_estep()`), not the evaluator, which
  keeps the evaluator scheme-agnostic (D15 revised to match).
- **Optimizer.** `optimize_abem_sgd(theta, score_fn, weights, control, use,
  resample_indices)`, where `score_fn(theta, indices)` is a closure over the
  score-only evaluator. There is no `step(e_stats, theta)` object. This change
  never calls the optimizer directly.

### D3 — Engine contract: batched C++ over flat preprocessed pools **[revised 2026-09-30 from B1 — see "Phase-1 revision" at the end of D3]**
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
phase. *Note (2026-09-28, `abmcem` D3 as amended):* the target the evaluator must
reproduce is the **joint single-clock likelihood**: pool entry is one merged
`preprocess_joint()` walk per sequence over `abmcem`'s pool engine spec, so each
timed rate fid is right-censored at every other process's events (other layers,
other flavors) and exposed to the end of the window, and each flavor is scored on
its own events. The 1e-10 swap equivalence against the adapter (task 4.1a) carries
this over; no separate scenario restates it here. *Rejected:* per-spec R calls as the engine
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
batch-vs-replay test (task 3.6) extends the same pin; it does not re-establish it.

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

**Phase-1 revision (2026-09-30, task 1.5 from B1 — supersedes the Option-A
conclusion above).** The load-bearing rate+choice cells (`social_evolution`,
`.plan/dynes/b1/B1_results.md`) **reverse** it:

- **The engine is Option C, and it already exists.** The `cpp` backend
  (`make_engine_evaluator(backend = "cpp")` → `estimate_DyNAM_rate` /
  `estimate_DyNAM_choice`) decodes the flat broadcast form in C++ and never
  materializes the dense stack. That is the "decode-reduce fusion" this decision
  called deferred. On rate+choice it is **faster than A (19.8 vs 24.2 ms per
  sequence per θ) and ~7× lighter (0.54 vs 3.95 MB per sequence)**. A reaches
  only K ≈ 3000 on an 18 GB machine (12.1 GB peak) and is infeasible at
  K = 10 000 (≈ 40 GB). The dyad family agrees: C is 8× faster and ~350× lighter
  than A on REM.
- **The family table's "rate is light" premise was wrong at pool scale.** The rate
  stack is O(**intervals** · n · p), not O(events · n · p): every exogenous change
  opens a right-censored interval, and a DyNES pool carries the whole panel's
  covariate stream (122 MB per rate stack at n = 1000 on a synthetic fixture).
- **No batching wrapper and no new kernel.** The R glue per sequence is ~2% of
  the kernel call, so the v1 evaluator is a plain per-sequence loop over
  `cpp`-backend engine closures. The new work is the **harvest** (per-sequence
  loglik / score / Fisher at the free positions) and the `what`-flag plumbing.
  The kernels compute Fisher on every call today, so a loglik/score-only kernel
  flag is an optional optimization, not a v1 requirement. "A for rate" survives
  only as a possible opt-in for small-n, few-covariate pools (1.4× per row on
  the rate kernel); it is not built in v1.
- **BLAS is irrelevant** for these p ≈ 3–5 kernels (reference ≈ vecLib, threads
  default ≈ pinned), so the D10 thread budget stays as hygiene with no crossover
  to tune.
- **The adapter is the cost to retire.** abmcem's zero-iteration adapter is
  ~9× slower than C (172–199 ms per sequence). The task-1.4 toy spent
  350–550 s on K = 30 × 15 iterations, almost all of it in the M-step's
  score-only passes through the adapter. That makes the task-4.3 swap the
  first-order runtime fix.

Everything above this revision about the two substrates (augmenter's R mirror vs
the evaluator's compiled kernels) and the 1e-10 `q`/`f` pin stands. The A/B/C
triangle and the "collapses to Option A" paragraph are kept as the reasoning the
measurement overturned.

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

**Phase-1 revision (2026-09-30, task 1.5 from B3 — the storage decision).** Measured
on abmcem's real `pool_entry_task()` over a DyNES-shaped 4-fid joint spec, on
peak footprint (`.plan/dynes/b3/B3_results.md`):

- **In memory. Neither the on-disk variant nor DBI is built in v1.** The
  condition is that the **per-window start state is held once per process**, not
  once per sequence (D5). With it shared, a pooled sequence costs 0.53–0.55 MB at
  every n from 84 to 1000 (1205 path events), about 2 MB at 4820 events:
  ≈ 0.24–0.45 KB per path event, independent of n. Unshared, today's entry is
  O(n²): 59 MB per sequence at n = 1000, 97% of it the choice fids' dense
  n × n × p `initial_stats`.
- **The acceptance bound task 4.2a asserts:** a pooled sequence's marginal memory
  is **≤ 1 KB per path event** (≥ 2× headroom over the measured range), and the
  start state is stored **once per window per process**, checked by object
  identity (one address per window, not one per sequence). Still no fixed byte
  threshold.
- **The cpp closure must not re-duplicate the start state.**
  `make_engine_evaluator()` holds `stat_mat_init`, a dense reshape of
  `initial_stats`, in every choice closure (48 MB per sequence at n = 1000 even
  with the entry shared). v1 builds closures **at evaluation time** by default
  (20–45 ms per build, ≤ 3% overhead from n = 250). Caching `stat_mat_init` with
  the start state, which needs a small `make_engine_evaluator()` signature change,
  is the large-n option.
- **Drop `data` from the resident entry** once the adapter retires (task 4.3):
  the adapter is its only resident reader, and it is 10–30% of the shared
  per-sequence cost at `social_evolution` scale.
- **The replicated resident store is the bound that binds** once sharing is in:
  abmcem's seam keeps (1 + W) copies, so at W = 5 and 12 GB it reaches
  K ≈ 850 at 4820 path events, versus ≈ 2700 with a **sharded** store (the
  coordinator plus 1/W of the pool per daemon). Sharding is abmcem's seam,
  flagged there, not built here.
- **Spill threshold, for the record:** at 0.44 KB per event and 12 GB, the
  in-memory design stops clearing K = 1000 near 2.7 × 10⁴ path events per
  sequence serial, 1.2 × 10⁴ sharded (W = 5), and 4 × 10³ replicated (W = 5).
  Past it, the fallback order above applies.

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

*Revised 2026-09-30 (task 1.5, from the task-1.4 toy) — the one-stream route needs no
injected boundary, so per-window preprocessing is not load-bearing.* The paragraph above
over-states what the interior wave boundary does. Checked on the toy
(`.plan/dynes/toy/TOY_results.md`, finding 5):

- **The trailing compensator comes from `end_time`, not from windowing.** One merged
  walk over the whole sequence with `set_preprocessing(end_time = t_M)` emits the
  timed rates' trailing interval up to the last wave (the ordinary
  end-of-observation right-censoring). The only requirement is that the observation
  end **is** the last wave, and the start `t0`.
- **An interval straddling an interior wave is harmless for a θ-rate.** Rates are
  constant between state changes, and a wave time is not a state change of a modeled
  layer: exogenous panel covariates enter as change-list rows, which are already
  interval boundaries. Splitting such an interval at the wave gives the same
  `−Δt·Λ`. (The trailing compensator at an *interior* wave is not a separate term
  on the one-stream route; the interval simply continues across it.)
- **The pinned plateau is already handled on the evaluator side.** The one place a
  straddle matters is a completed rate's per-period `intercept_w` (D20 invariant 2
  amendment), and `pinned_rate_compensator()` (landed with abmcem) already splits a
  straddling interval's exposure at interior waves, with the identical result an
  injected boundary would give.

So v1 keeps abmcem's **one-stream** pool entry, and the per-wave start-state cache
(above) stays a **memory** decision, not a correctness one. What becomes load-bearing
instead: **`estimate_dynes()` MUST pin the preprocessing window to `[t0, t_M]`** (the
resolved observation start and the last wave) for every `f` path: pool entry, the
MCMC closure, and warm start. Today it takes the user's `control_prep` unchanged; the
toy passed the window by hand. This joins task 6.1's entry wiring (the same wave grid
feeds `complete_generative_spec(wave_times =)`). The "no interval straddles a wave"
test task 4.2b asked for is replaced by two assertions: the trailing interval ends at
`t_M`, and a pinned rate's straddling interval is split.

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

*Qualified 2026-09-30 (task 1.5, from the 1.3 note `.plan/dynes/rsiena_mle_notes.md`).*
"Ergodic at the desk" holds on the **product** space: orderings (linear extensions
of the per-dyad chain poset, connected under adjacent cross-dyad transpositions) ×
the open order polytope of times (shift alone is irreducible there) × intervals.
Aperiodicity follows from continuous proposals with positive rejection probability,
and the pick probabilities are ordering-invariant (valid permute pairs
`C(n_w, 2) − Σ_j C(c_j, 2)` per wave), so they cancel in α as D20 requires. Under
cross-dyad support coupling the target support is a strict subset of that product,
and irreducibility means the **legal** set must be connected under legal moves:

- it **holds** on coupling-free specs, when legality is a **precedence order** (the
  legal set is the linear extensions of a larger poset), and for **one-sided counter
  bounds** such as max-outdegree `≤ c` (every legal ordering reaches the legal
  "dissolves first" ordering through legal swaps);
- it is **not guaranteed** for general state-dependent constraints (two-sided
  bounds, constraints reading another latent layer). The legal set can split into
  pieces joined only through illegal intermediates; the chain is then **reducible**,
  not merely slow, and D14's weights are computed over the piece the chain started
  in. **Recorded as a known v1 gap**, beside D20's V2 cycle-structured note; no
  detector is built in v1. The practical check is comparing chains started from
  `initialize = "random"` and `"sim"` (D16).

The minimal-flip limitation above is also **directional**: the latent event count is
the Hamming distance, a lower bound on the true count, so on a state panel creation
and dissolution rates are biased **downward** by `O(((λ_c + λ_d)·Δt)²)` per
dyad-interval — negligible when `(λ_c + λ_d)·Δt ≪ 1`, material in the sparse-wave
regime DyNES targets. `summary()`'s MC error cannot show it (it is in the estimand,
not the pool); task 6.2 measures it because `simulate()` draws excursions, and the
optional two-state-CTMC sibling of 6.2b measures it rather than passing to MC error.
Count panels (D24) carry no such bias.

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

### D7 — Result contract: covariance (Louis observed information, amended from abmcem D9's sandwich) + MC standard errors (OQ B6) **[→ abmcem]**
ABEM returns the parameter estimates and an **observed-data** covariance — deliberately
**not** the naive complete-data Fisher inverse, which is anti-conservative (it ignores that
the between-wave path is imputed, so SEs would be too small exactly where waves are sparse,
the regime DyNES exists for). The estimator is the inverse of **Louis's (1982)
observed-data information** (`louis-observed-information` D1). With importance weights
`w̄_i`, per-sequence complete-data score `S_i = ∇ℓ_{c,i}` and information `J_i = −∇²ℓ_{c,i}`:

```
  vcov(θ̂) = I_obs⁻¹,   I_obs = Σ_i w̄_i J_i − ( Σ_i w̄_i S_i S_iᵀ − S̄S̄ᵀ ),   S̄ = Σ_i w̄_i S_i
                                └ complete ┘   └────── missing ───────┘
```

reported beside the **MC standard error** `I_obs⁻¹ (Σ_i w̄_i² (S_i − S̄)(S_i − S̄)ᵀ) I_obs⁻¹`
(finite-pool noise in θ̂, weight-squared scaled; the delta method's bread is the
derivative of the self-normalized weighted score, which is `−I_obs`). `summary()` shows both side by side so
users see when the pool, not the data, limits precision. Full detail — the rejected
aggregate-then-outer-product `S̄S̄ᵀ` bug, the `use = "resampling"` warning, and
fixed-parameter padding via `GetFixed()` + `stats::.vcov.aliased()` — lives in **abmcem
D9**; this decision fixes the *interface* that feeds it.

*Amended 2026-10-05 (`louis-observed-information`):* this decision previously named
abmcem D9's "Ruth (2024) §3.3 sandwich" `I_c⁻¹ (Σ w̄_i S_i S_iᵀ) I_c⁻¹` as the estimator,
arguing that a weighted estimating equation calls for `A⁻¹ B A⁻¹` and that Louis is exact
only for ideal EM. Both premises fail here. A sandwich's meat is a sampling variance only
over independent units, and the `S_i` are scores of imputed paths of the same data, so
their spread is the missing information; Ruth (2024) says the form "applies to Monte
Carlo variability within an iteration; it does not measure sampling variability due to
the observed data". And Louis's identity is exact for raw self-normalized importance
weights, the estimating equation's own (approximate only under a weight
`transformation`). The 6.2b pilot measured the sandwich 50× or more too small against a
closed form (`.plan/dynes/recovery/PILOT_findings.md`). The MC error keeps its middle
term and takes `I_obs⁻¹` as its bread. A non-positive-definite MC estimate of `I_obs`
returns `NA` covariances with a `goldfish_dynes_observed_information_not_pd` warning.

**Carve-out seam (who emits what).** The covariance is assembled by **abmcem** (D7 is
`[→ abmcem]`), but its ingredients are **this change's** evaluator:
`evaluate_sequence_pool()` MUST return, per sequence, both the score `S_i` **and** the
complete-data information `J_i` — the missing information `Σ w̄_i S_i S_iᵀ − S̄S̄ᵀ` needs
the per-sequence scores, the complete information `Σ w̄_i J_i` needs the per-sequence
information, and neither reconstructs from the other. The `what = "fisher"` path already
computes `J_i`; the per-sequence scores are a required co-output, not an abmcem-side
recomputation. If the evaluator returned only `J_i`, the missing information would be
physically unavailable and `vcov()` would silently fall back to the anti-conservative
complete-data inverse `I_c⁻¹` — the failure mode this seam exists to prevent.

The result
additionally carries MC standard errors and the **`em_trace`** per-iteration
diagnostics object (D18) — the ascent trajectory, ESS, pool bookkeeping, and MCMC
chain statistics — as a documented part of this contract, so convergence problems
are diagnosable from the result alone. `summary()`
displays asymptotic and MC error side by side so users see when the pool, not the
data, limits precision. Fixed-parameter padding is abmcem D9's (`GetFixed()` +
`stats::.vcov.aliased()`; the prototypes' `unfixed_params()`/`std_error_fixed_params()`
are retired there).

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
**v1 assumes the decomposition is unique** — the transition/support spec provides a single
finest step, so an observed multi-step change has exactly one chain length. This is what
makes the event count per interval **fixed by the wave diff**, the premise D6's
fixed-cardinality ergodicity argument (permute + shift suffice, no insert/delete moves)
rests on. A spec offering *overlapping* step sizes (e.g. both a direct 0→2 flavor and a
0→1/1→2 path) would make the chain length ambiguous and reopen D6; such specs are **out of
v1 scope** (kept as-is for v1), recorded here so the fixed-cardinality premise is stated,
not silent.
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

*Emission contract (added 2026-09-28, `abmcem` D3 as amended):* an augmenter's
emitted sequence must carry, on every flavored layer, the flavor keys
`add_flavor()` stamps from the data. `abmcem`'s pool entry is one merged
`preprocess_joint()` walk per sequence, and it routes each event to its flavor by
those keys, so a sequence without them would score every flavor on the wrong
events. Asserted per augmenter in task 3.6.

*Amended 2026-09-18 — the handle the augmenters actually get is wider than the
original four.* `process-simulation` tasks 2.0/2.0e/2.0f landed on
`feature_simulation` and grew the substrate while building its own driver; the
augmenters inherit all of it without asking:

- **`walk_evaluate(handle, fid, theta, sender =)`** — `sender` slices one
  sender's receiver distribution out of the choice evaluator instead of
  materializing the full `n1 × n2` matrix and discarding all but one row. This
  is exactly `augment_seq_sim()`'s access pattern (draw the sender, then its
  receiver among the remaining observed ones), so the augmenter reads per-sender
  and never pays the matrix. `.pse_eval_choice()` also stopped building the index
  `data.frame` its callers discard; the return is parallel integer vectors.
- **`walk_next_breakpoint(handle)`** — the time of the next change the handle
  applies on its own clock (an exogenous row, a scheduled event, a composition
  change), or `Inf`. Between now and it every rate is constant unless the driver
  injects. This is the frozen-rate interval `sim`'s truncated exponential must
  respect: the anchor is `min(next unplaced RE time, wave end, walk_next_breakpoint())`,
  not `min(next unplaced RE time, wave end)` as the D20 bullet still reads.
- **`walk_schedule(handle, event)`** — queue an event at a future time, applied
  when `walk_advance()` reaches it, resolved against the state *then*. This is
  how window expiries self-schedule; an augmenter that places a flip inside a
  windowed spec needs no expiry bookkeeping of its own.
- **`walk_replay(handle, rows)`** — register observed rows of a focal layer as
  exogenous, so a flavor no process models keeps its observed events while the
  driver draws the modeled ones around them. Registered once, before stepping.
  This is the substrate for a DyNES spec whose panel layer carries an unmodeled
  flavor (D19's partially-modeled RE layers).

Three guards the augmenters were blocked on are also gone (see the resolved Open
Question at the end of this document): user and derived support constraints are
maintained live on the handle, composition changes advance through
`walk_advance()`, and windowed effects run on the merged walk. One refusal
remains (`goldfish_walk_unsupported`): a constraint reading a *changing* object
no formula term reads, which `constraint-objects-on-shared-walk` is open against.

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

### D10 — Parallelization: sequence-level only, under a non-nested thread budget **[revised 2026-09-30 from B1]** (map seam → abmcem)
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

*Revised 2026-09-30 (task 1.5, from B1).* Sequence-level parallelism is confirmed:
W = 5 sharded workers give 4.6–4.7× for every engine up to K = 10 000, and
per-sequence dispatch through abmcem's seam matches sharding on time (dispatch
overhead is negligible at ~20 ms tasks). The measured crossover the adaptive BLAS
budget was meant to tune **does not exist**: nothing in these kernels reaches BLAS.
The binding cost of the seam is **memory**: the replicated resident store pays
(1 + W)× (C at K = 1000: 4.7 GB replicated vs 2.1 GB sharded). Sharding the resident
store is flagged to `abmcem` (D4 revision); it is not this change's to build.

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
recovery for ordinal fits). That change's D11 (added 2026-09-09, revised
2026-09-15 under ADR-0074) crystallizes the per-step drawing core into **five**
named plug points on a `goldfishSimSteps` object from the exported
`set_simulation_steps(parameters, evaluate, clock, mark, accept)`, every slot
defaulting to goldfish's own descriptor-keyed step:

```
per replicate:  P0  params$init(r, handle)            replicate-level draws
per step:       P1  params$at(k, t, handle, latent)   parameters per fid for this step
                PE  evaluate(stats, theta, risk, meta) values over the fid's candidate space
                P2  clock(rates, t, handle)           the waiting-time draw
                P3  mark(fid, evaluation, handle)     who acts on whom
                P4  accept(event, handle)             keep or redraw the step
                    walk_inject(handle, event)
```

`coef` is likewise a **provider**: a numeric vector, a `goldfishParams`
(`joint-parameters`), or a `goldfishParamProvider` from the exported
`set_parameter_provider(init, at)`; `at()` resolves to the length-`p_fid` vector
PE's default step accepts, and may also return a `breakpoint` the clock treats as
a competing exit. Step closures reach the handle only through the exported
accessors (`n_actors()`, `process_map()`, `current_time()`, `regime_of()`,
`times_of()`, `risk_set_axis()`) — the `walk_*` handle itself stays internal, so
an augmenter written as steps never learns what the walk is.

**Which plugs `augment_seq_sim()` actually supplies.** D11's table lists "DyNES
endpoint conditioning" under `accept` alone, and its prose says the augmenter "is
P1 plus P4 and never touches PE". That is the shorthand, and it is narrower than
what D20 specifies: wave-endpoint conditioning is not a post-hoc filter on a
freely drawn event but a **restriction of the draw itself**, so
`augment_seq_sim()` supplies

- **P3 `mark`** — the sender–flavor set restricted in R to observed pairs with
  remaining support-applicable events plus the single globally-next unplaced
  modeled RE, renormalized, then the receiver drawn among that pair's remaining
  observed receivers (D20). Drawing the unrestricted mark and rejecting would
  discard almost every step and destroy the proposal-density bookkeeping.
- **P2 `clock`** — a truncated exponential on `(0, anchor − t)` at the selected
  pair's rate rather than the free competing exponential, with `anchor =
  min(next unplaced RE time, wave end, walk_next_breakpoint())` (D9 amendment).
- **P4 `accept`** — the completion / reject-and-restart guard (D20): a step that
  strands the interval rejects the whole interval draw, which is what feeds the
  success-conditioned `P̂(complete)` normalizer.
- **P1 `parameters`** — θ_k held constant within an EM iteration; a plain
  numeric vector suffices, no provider needed.

It never touches **PE**: the augmenter draws from the model's own rates and
choices at `θ_k` (the time-domain fid-for-fid pin of D20 invariant 2 *requires*
the default evaluator), so a supplied `evaluate` step would break the `q`/`f`
pin. The one claim D11 and D20 agree on unconditionally is that the augmenter is
a **caller of the one loop, not a fork of it**. Reconciling D11's table entry
with this list is `process-simulation`'s to do at its task 2.1/2.2 review; this
change implements against the list above, and task 3.4b names all four plugs.

The recovery study (D19) simulates panels through the same driver, and inherits
what the loop grew around it: `nsim > 1` returns a `goldfishSimPool` (one
replicate is a `goldfishSim`), `filter_simulation()` drops replicates by a stated
criterion, and the stopping targets and explosion guard travel in one
`control_sim = set_simulation_guard()` object (ADR-0085/ADR-0089 — a runaway
reaches `max_events` and is flagged-and-kept by default; rate-trajectory early
stops are opt-in).

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

**What ESS measures, and what it must not be asked to mean** *(added 2026-09-18).*
`1/Σw̄²` is a property of the **proposal**, not of the estimand. Two quite different
situations print the same low number, and the calls/emails case is the second one:

- a **diffuse, high-dimensional** path posterior — many paths are about equally
  plausible, so no proposal concentrates and the weights spread thin (the
  flips-per-interval axis);
- a **sharply concentrated** posterior that the proposal cannot find. Coupling a panel
  layer to a densely observed event-stream layer is *information*: if the email rate
  reads recent calls, a burst of emails is evidence about when a call happened. That
  evidence sits in the events **after** the placement a forward draw is making, and
  `sim` places each event knowing only the past — so its weights degenerate **because**
  the coupling is informative, not despite it.

Both diagnoses call for the same remedy (`augment_seq_mcmc()`, whose target is the whole
path and therefore sees the later evidence), so ESS remains the right **trigger**. It is
the wrong **verdict**: a richly coupled specification can be better identified than an
uncoupled one while scoring worse on ESS. Trustworthiness is the D7 result contract's
**MC standard error against the asymptotic SE** — is Monte-Carlo noise small relative to
statistical uncertainty — which is augmenter-invariant and is what the operating envelope
is stated in. ESS is an input to that ratio, never a substitute for it.

Two ESS notions, one name, **no shared threshold**. The guard above is importance-sampling
ESS. Under `routine = "mcmc"` with `weighting = "uniform"` (valid within the drawing
iteration, D13) every weight is equal, so IS-ESS is trivially K: it reads "recovered" the
moment a user follows the switch recommendation, whether or not the chain mixes. The MCMC
analogue is the autocorrelation-based effective size `em_trace` already carries separately
(D15/D18). The numeric floor the 6.2 sweep sets is an **IS-ESS** floor and applies only to
the `random`/`sim` path; the MCMC path is judged on acceptance rate and autocorrelation.

Because `coupled` is θ-free and known at construction, the recommendation does not have to
wait for a measured collapse: a specification with coupled fids is predictably hard for
importance sampling, so `set_algorithm_em()` can recommend `routine = "mcmc"` up front
(D1's cross-object rules, implemented in abmcem) rather than spending EM iterations to
discover it.

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
E-step object (`c("goldfishEstepIS", "goldfishDynesEstep")`; likewise
`goldfishEstepResampling`, `goldfishEstepUniform`) and two generics dispatch
on it: `compute_q()` and
`compute_ase()`. The EM loop is thus written once, scheme-agnostically. **Internal
only** for now; exporting them for user extensibility is a recorded follow-up.
Under MCMC draws the ASE treats sequences as **independent given the thinning**;
the augmenter records an autocorrelation index and basic chain statistics into the
`em_trace`, with a user-facing suggestion to increase `thinning` when the index is
high — no autocorrelation-corrected ASE method in v1.

**Carve-out seam (who builds the classed object).** The classed E-step object itself —
its class tag (`goldfishEstepIS` / `goldfishEstepResampling` / `goldfishEstepUniform`)
and the per-sequence
quantities + weights it carries — is **constructed by this change's evaluator**
(`evaluate_sequence_pool()`, task 4); it is the evaluator's return type, not a wrapper
abmcem adds. `compute_q()` and `compute_ase()` — the generics that dispatch **on** that
class — are `abmcem`'s (the accept/grow/stop machinery consumes them). So this change
owns the object and its class contract; abmcem owns the methods that read it. The
`sequence-augmentation` spec states the classed return; abmcem's spec states the
generics.

*Revised 2026-09-30 (task 1.5, from the task-1.4 toy):* the seam landed the other
way and it stays that way. `evaluate_sequence_pool()` returns the raw per-sequence
`list(loglik, score, fisher, degenerate)`; the ABMCEM loop forms the per-sequence
log-likelihood differences and weights and builds the classed object
(`new_dynes_estep()`, `R/weights_dynes.R`). The evaluator therefore never needs to
know the weighting scheme. This change's evaluator owns the per-sequence
quantities (score **and** information, D7); abmcem owns the classed object and its
generics. The `sequence-augmentation` spec is amended to match.

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
- **Convergence criterion (outer loop)**: the EM loop converges on **Q and its
  tolerance** — the outer-loop test is the change in the (ascent lower-bounded) Q
  objective between accepted iterations against `set_algorithm_em()`'s `tolerance`, not a
  parameter-norm test. This is deliberately **distinct from `set_sgd_options()`'s inner
  `tolerance`**, which governs a single M-step's SGD stopping; the two tolerances live at
  different loop levels and are not interchangeable. `tolerance` here is Q-based so the
  criterion is on the quantity the accept/grow/stop machinery already estimates (`Q ± z·ASE`,
  D15), keeping convergence and the ascent test on one currency. (Owned by `abmcem` with the
  rest of the EM loop; pinned here so the recovery study's pre-registered bounds and the
  `em_trace` columns reference a defined criterion rather than each assuming the other set
  it.)
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
v1 over **observed** layers (a window over a modeled panel layer is out of v1, see
the 2026-09-30 narrowing under "Interval factorization" below). v1 still runs one
global MCMC chain over the whole sequence (D20) and no wave-level parallelism (D10):
one pool and weight structure, whether or not a given specification would factorize.

**Separability: consume the multivariate spec's coupling detection, don't
re-derive it.** `make-multivariate-spec` D4 already marks each fid `coupled` iff any
of its effect arguments or support-constraint atoms reads a **modeled** panel layer's
state (direct reference; not transitive). A fid reading only a static exogenous panel
covariate is *not* coupled — nothing about it is latent, and a panel layer referenced
only that way is a static step-covariate, so a specification built entirely of such
references really is a DyNAM. `estimate_dynes()` reads that `coupled` column rather
than recomputing PE-independence at sub-model granularity.

*Amended 2026-09-18 — `coupled` is the right input for the message and the wrong one
for the gate.* Latent dependence has **two** sources and D4 detects one:

- **(a) the fid BELONGS to a modeled panel layer** — its event *times* are latent,
  whatever its formulas read;
- **(b) the fid READS a modeled panel layer's state** — its *statistics* are latent.

`coupled` is (b). For a dichotomous `mutually_exclusive` layer (a) implies (b) by
accident rather than by design: `derive_flavor_constraint()` emits `~ tie(L)` /
`~ !tie(L)`, whose atoms read L, so every fid of such a layer is coupled and the two
criteria agree. A modeled panel layer that derives **no** constraint breaks the
implication — an unflavored count panel (D24), or any `redundant`-style layer — because
its own fids can read nothing latent while its events are entirely latent. Reading the
gate off `coupled` would then bounce a perfectly good DyNES specification to
`estimate_dynam()`, which cannot estimate it either (its focal layer is panel-observed)
and points back: two errors and nowhere to go. So the **gate** is (a) — *is any panel
layer a modeled dependent process?*, which D8's augmentation trigger already answers —
and the `coupled` column keeps its narrower job of naming which fids are separable in
the mixed message.

Behavior on the resulting cases, aligned with `make-multivariate-spec` D4 (the newer
decision):
- **No panel layer is a modeled dependent process** → abort: nothing is latent. (The
  gate is criterion (a); on a flavored state panel this coincides with "all fids
  separable", which is why the earlier wording read that way.) The error names
  `estimate_dynam()`, explaining the specification fits DyNAM (not DyNES) and that its
  panel layers would only be considered as static exogenous covariates. This is
  **reachable** through `make_joint_specification()` whenever every panel reference is
  exogenous-only (D2 composes such specs; the DyNES-viability check is estimation-time)
  — catching a user who reached for `estimate_dynes()` on what is really a DyNAM model.
  The paradigm instance is an emails specification reading `tie(calls)` with no call
  formulas of its own: calls is a static step-covariate jumping at wave times, nothing
  is latent, and DyNAM is genuinely the right estimator.
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

**History spans** *(reworked 2026-09-18 — no wave is ever history)*: the left edge of
the estimand is the **resolved observation start** `t0` (the `observation-window`
capability's `start_time`, defaulting to the span of the non-window event streams), not
the first wave. Events before `t0` are burn-in exactly as they already are on the
event-stream path: they advance process state and the per-event order counter and
contribute no likelihood terms. The first wave `t1` is the **right** endpoint of the
first modeled interval `[t0, t1]`, whose entering state is the initial condition (the
data object's starting network, or zero counts for a layer with no prior history) and
whose flips are diffed against that state. A panel of M waves therefore yields **M
modeled intervals over M+1 boundaries** `t0 < t1 < … < tM`, and modeled REs inside
`[t0, t1]` contribute likelihood terms like any others. Modeled REs after `tM` are
discarded. This matters most where the RE layer is the data-rich one: under the
superseded reading, a calls-panel + emails-stream specification silently dropped every
email likelihood term before the first wave. The grid D21 hands
`complete_generative_spec()` is this M+1 boundary set, `t0` included.

**Interval factorization: what it is, and why v1 declines it.** Given the wave
snapshots the complete-data likelihood *would* split into one factor per interval,
`f(Ω | θ) = ∏_m f(Ω_m | S(t_m), θ)`, because the state entering each interval is
observed rather than sampled: the panel layer's state at `t_m` is the snapshot every
endpoint-hitting sequence reproduces exactly (D20 — and D22 caches those start states
for precisely this reason), the RE layers are observed throughout, and exogenous
covariates are given. **Coupling does not break this.** A calls/emails specification
whose layers read each other's state still factorizes: the emails are observed
everywhere and the calls' state at each wave is known.

What breaks it is a statistic whose value depends on **when** events fell in an
*earlier* interval — a window or memory effect **over a modeled panel layer**
(`inertia(calls, window = 30d)` at `t_m` counts calls whose placement differs across
sampled paths, whereas plain `inertia(calls)` at `t_m` *is* the snapshot), and
`recency_*` once `recency-effects` lands. A window over an **observed** layer does not
break it, and a window much shorter than the interval leaks only within a `w`-wide
boundary band. This is the precise condition; "windowed effects are allowed, so nothing
factorizes" is the coarse statement it replaces.

*Narrowed 2026-09-30 (task 1.5, from the task-1.4 toy) — a window over a modeled panel
layer is out of v1.* The toy could not construct one: `tie(friendship, window = 3)`
on the modeled panel layer is refused at pool entry by the panel-window rule in
`data_source.R`, which was written for **exogenous** panel covariates, where expiry
would reset every snapshot tie at once. Re-declaring the drawn layer as an event layer
is refused in turn by `bind_candidate_data()`, which requires `info` to be identical.
For a *modeled* panel layer the rule is wrong in principle (an augmented path is an
event stream, so expiry decays one event), but lifting it means separating the two
cases in the refusal and giving augmented paths an event-observed view of the layer.
That is recorded as a **future development**, not v1 work. v1 therefore allows windows
over **observed** layers only (the toy's `window_observed` variant runs end to end,
expiries self-scheduled on the walk). The refusal stays, and its message SHOULD
name the modeled-panel case and point to this limitation (a message-only change
riding task 6.1's validation work).

A consequence worth recording: with windows over modeled panel layers out of v1 and
`recency_*` not landed, **no v1 specification carries a latent-timing statistic
across a wave boundary**, so every v1 specification factorizes across intervals. v1
still declines the per-interval scheme below (the decision is about maintaining one
pool/weight structure, not about whether factorization holds). What changes is the
future development's trigger: per-interval pools become available as soon as they are
built, and the static check stays necessary only once modeled-panel windows or
recency effects arrive.

Nothing about the unfactorized case is *wrong*: per-path statistics are correct on each
path and the M-step aggregates over them without bias. What factorization buys is the
**dimension of the sampling problem**, and the two augmenter families cash it
differently:
- **`random` / `sim` + importance weights** — the score is a sum of per-interval terms
  and the path posterior factorizes with it, so a factorized scheme runs M independent
  importance-sampling problems of dimension `N_m` in place of one of dimension
  `N = Σ N_m`. Weight variance grows roughly exponentially in that dimension (the same
  premise behind the runtime ESS-collapse note), so this is a **statistical** lever, not
  a speed one.
- **`mcmc`** — M independent shorter chains instead of one global chain, and a move
  re-evaluates one factor rather than everything downstream of it. MCMC pays no
  IS-dimension tax, so here the lever is **mixing and compute**.

**v1 declines it, deliberately and unconditionally.** The pool is a list of
whole-sequence objects with per-sequence weights (D4/D14); a factorized scheme is a
different pool and weight structure, per interval — not a flag on this one. v1 therefore
pays the global cost even for specifications that would factorize cleanly: the price of
admitting windowed/memory effects without maintaining a second algorithm path. Recorded
as a **future development**: for a specification carrying no latent-timing statistic
across a wave boundary — statically checkable, since it asks only whether any
window/memory effect reads a modeled panel layer — per-interval pools, weights and
chains become available and divide the IS dimension by the number of intervals.
Deferred, not rejected.

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
  interval. This respects same-dyad chain order by construction but is otherwise
  **support-blind** — it never consults live support — so under cross-dyad support
  coupling (the same regime that strands `sim`) the sorted ordering can fire a flip
  whose support is not yet applicable (e.g. under a max-outdegree-1 constraint,
  `create i→k` placed before `dissolve i→j`). `random` therefore carries the **same
  reject-and-redraw** treatment as `sim`: the drawn ordering is replayed against the
  support constraints (a legality check `sim` gets for free from its emptying risk set
  but `random` must perform explicitly — θ-free, so computable once per interval) and
  an illegal ordering is rejected and redrawn until legal, so the delivered draw is
  uniform over the **legal** configurations; a restart-count ceiling aborts with the
  coupled constraint named if a legal ordering is vanishingly likely. Log proposal
  density per interval: Σ_j log(c_j!) − n·log L − log P(legal), where P(legal) is the
  legal-ordering fraction (estimated by the redraw acceptance rate, ≡ 1 on any
  coupling-free spec). *Amended 2026-09-30 (task 3.3, user decision):* the rate is
  estimated **once, when the augmenter is built**: a pilot of redraws per coupled
  interval, run on a private L'Ecuyer-CMRG stream seeded from the run seed, which
  leaves the session stream untouched. Every draw records that same constant. A
  per-draw `1/attempts` is all a substream-pure draw can see, but it varies across
  draws, so it would not cancel (and `E[1/N] ≠ p`). Draws still redraw until legal;
  their own attempt counts only set `restarted`.

  **Why the normalizer cancels — random's redraw is pool hygiene, not weight
  correction.** Unlike `sim`'s θ-dependent, per-draw `P̂(complete)`, random's `q_raw =
  Σ_j log(c_j!) − n·log L` is **identical for every draw** (a uniform proposal assigns
  equal density to every configuration) and `P(legal)` is a **θ-free per-interval
  structural constant**, so `q = q_raw − log P(legal)` is a pool-global constant that
  **cancels** in the normalized importance weights (w_i = f_i / Σ_k f_k — D13's
  "target / uniform density" with the uniform density made explicit). Consequences:
  (a) random incurs **no plug-in-normalizer bias**, so the one-per-run coupling warning
  `sim` emits (D20 "Known bias") does **not** fire for random; (b) the operative reason
  to reject is therefore not the weight but that an illegal draw has `f = 0` (or may
  **error** rather than cleanly return −Inf — the rejected alternative below), making it
  dead weight that shrinks the effective pool below K — so excluding it honors the
  "never build/evaluate an invalid sequence" principle, it is not a weight fix.
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
  (0, next anchor − t) with anchor = min(next unplaced RE time, wave end,
  `walk_next_breakpoint(handle)`) — by inverse CDF (never rejects). Repeat until
  every flip is placed. *Amended 2026-09-18:* the breakpoint term is the third
  one, added once the handle grew the API (D9 amendment). It is not cosmetic:
  the frozen-rate draw is only exact while the rate is constant, and an
  exogenous row, a queued window expiry or a composition change inside the
  interval breaks that constancy. Truncating at the breakpoint, advancing, and
  redrawing keeps `q` piecewise-exact at the same `Λ` the compensator uses —
  invariant 2 below — where truncating only at the RE/wave anchor would silently
  draw from a rate the target never had. A windowed DyNES spec (D19 allows them,
  and the substrate now runs them) is exactly where this fires.

  *Amended 2026-09-30 (task 3.4b, user decision): what "bounded at the
  breakpoint" means, and what a restart redraws.* A breakpoint is a
  **censoring** bound, not a truncation. Before a breakpoint `b` that is earlier
  than the anchor `A = min(next unplaced RE time, wave end)`, the waiting time is
  an untruncated exponential at the pair's rate. If it would pass `b`, the walk
  advances to `b` with survival factor `e^{−λ(b−t)}`, and the **same selected
  pair** redraws at the rate the walk holds there. Only the segment ending at
  `A` is a truncated exponential on `(0, A − t)`. The per-step density is then
  `π_pair · ∏_censored e^{−λ_j Δ_j} · λ_last e^{−λ_last w} / (1 − e^{−λ_last (A − t_last)})`,
  a function of the sequence alone. The pair persists across a censor, so no
  marginalization over the pair a censored segment might have selected is
  needed. This is the only reading in which a draw can "reach the bound" and
  redraw. Truncating at `b` would make that impossible. A restart
  redraws **only the stranded interval**. Each attempt reopens the walk and
  replays the observed events up to the interval, plus the earlier intervals
  as already drawn, contributing nothing to `q`. It then draws the interval from
  its opening wave. The completion normalizer is therefore per interval,
  `P̂_k = 1 / attempts_k`, and the recorded `q` is `Σ_k (log q_raw,k − log P̂_k)`.

  **Completion is not guaranteed by construction — reject-and-restart (v1).** The
  greedy forward draw hits every wave snapshot only if the risk set never empties with
  flips still unplaced. Within a single per-dyad chain the earliest unplaced event is
  always support-applicable, so a chain never strands itself; the stranding case is
  **cross-dyad support coupling** — a flip whose support depends on *another latent flip*
  in the same interval, where a locally-legal greedy step can dissolve the enabling state
  before the dependent flip is placed and never reopen it before the wave end. v1 does not
  prove this away: it **rejects the partial draw and restarts** the whole interval's `sim`
  when the risk set empties before all flips are placed, drawing until the interval
  completes, so the delivered draw is from the **success-conditioned** distribution
  `q_success(Ω) = q_raw(Ω) / P(complete)`. Consequently the recorded proposal density `q`
  is the **restart-normalized** density — the accumulated per-step density `q_raw` divided
  by the draw's completion probability `P(complete)` — and the importance weight `f/q` must
  use it, or it is biased. Fixing the number of *completed* draws this way keeps the pool
  size fixed at K (no random live-count bookkeeping), which is why v1 conditions on success
  rather than counting attempts. `random` and `mcmc` are **also exposed** to this coupling
  — both are **support-blind** (neither consults live support the way `sim` does), so the
  real axis is support-blind-vs-live-support, not greedy-vs-not — and each needs its own
  legality check, resolved differently: `random` reject-and-redraws with a θ-free
  normalizer that cancels (see its bullet), while `mcmc` needs **no** redraw normalizer —
  an illegal cross-dyad proposal is rejected natively by the MH ratio
  (`f(Ω′) = 0 ⇒ α = 0`), i.e. just another (rejected) step on the chain (see the permute
  bullet). Both must still *detect* the illegality via a support lookup, contra the
  earlier "unaffected" claim.

  **Where `P(complete)` comes from — the restart acceptance rate.** `P(complete)` has no
  closed form on a coupled spec, but it does not need one: the restart loop *is* its
  estimator. For each interval draw at `θ_ref`, `P̂(complete) = successes / attempts` (the
  fraction of forward draws that completed) is a free Monte-Carlo estimate, recorded with
  the sequence's reference record (D14) so the same `θ_ref`-specific normalizer travels with
  the sequence across cross-iteration reweighting. On a coupling-free spec no draw ever
  strands, `P̂(complete) ≡ 1`, and `q = q_raw` exactly — the plug-in normalizer departs from
  1 only in the coupled regime. A restart-count ceiling guards against a spec whose coupling
  makes completion vanishingly likely; exhausting it aborts with a diagnostic naming the
  coupled support constraint rather than looping.

  **Known bias (v1 sharp edge → v2 removal).** `P̂(complete)` is a plug-in (ratio-estimator)
  normalizer, so on a coupled spec `f/q` carries a small, bounded bias — least visible
  exactly where it acts (weights still normalize; they are just slightly off). v1 carries
  this as a guarded sharp edge: the restart-normalization is a hard requirement (tested on a
  coupled fixture), and the **first** time any interval draw restarts in an estimation run
  the loop emits **exactly one** `cli` warning for the whole run — deduplicated, never per
  iteration or per draw — that cross-dyad coupling triggered success-conditioned proposals
  whose importance weights may carry a plug-in-normalizer bias, naming the coupled
  constraint. It is not fully *removed* until v2's cycle-aware risk sets eliminate the
  restart regime altogether (below), at which point there is no success-conditioning left to
  get wrong.

  **V2 note — cycle-structured latent processes (future development).** Reject-and-restart
  is the minimal-assumption v1 mechanism; it degrades badly when completion is rare because
  the latent process has genuine *cycle* structure — e.g. project life-cycles
  (join → produce → close) where the "produce" flip is support-gated by a "join" that a
  greedy order may not have placed. For those estimands the risk set and the simulation
  should be **determined differently** (cycle-aware ordering / staged risk sets that respect
  the life-cycle DAG), not rescued by restarts — which also dissolves the restart-
  normalization bias above. Recorded as a v2 direction, out of v1 scope.

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
    pairs whose swap respects same-dyad chain order (same-dyad-violating swaps are
    excluded **before** proposing — such an invalid sequence is never built, never
    evaluated; *cross-dyad* support illegality is handled by rejection instead, see the
    legality note below); swap their slots and redraw both times.
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

  **No bias from freezing the rate across intervening REs (shift included).** The
  frozen-rate simplification above is not an approximation *of* `q` — it is `q`'s
  exact, closed-form density (a `TruncExp` with a stated support and a stated
  frozen `λ`), and Metropolis–Hastings is invariant-distribution-exact for *any*
  proposal whose density is honestly reported in the ratio, however far it
  diverges from the true state-path-dependent rate. So letting `shift` reposition
  a PE across an arbitrary number of intervening REs — already licensed by the
  windows rule above, which never bounds a window at an RE — costs **nothing in
  correctness**, only in efficiency (below).

  For `shift` specifically the ratio stays exactly as cheap as the no-crossing
  case. The move touches only slot `i`'s own time, so `λ_i` is evaluated once, at
  the state right after `pred(i)` — a point that precedes the whole window and is
  therefore identical under `Ω` and `Ω′` (nothing before `pred(i)` changed) no
  matter how many REs sit inside `(t*_pred(i), t*_succ(i))` or how much they move
  the true rate. Window and rate both match forward and backward, so the ratio
  collapses to
  `q_rev/q_fwd = exp[λ_i·(t′_i − t*_i)]`, the normalizing constant cancelling
  exactly as it does today. This is the same cancellation permute's *first*
  factor gets (its window's lower bound, `pred(h)`, is likewise untouched by the
  pair being moved); permute's *second* factor is the one exception, because its
  window's lower bound is the freshly-placed partner event itself (`t′_k` vs.
  `t*_k`), and that point's downstream state can differ across an RE the two
  candidate times straddle differently (why q_rev needs a fresh rate query under
  Ω′ there, below). `shift` has no second factor, so it never inherits that
  complication: multi-RE crossing changes which regime `λ_i` would "truly" sit in
  if `q` tried to track the piecewise rate, but since `q` deliberately doesn't
  try, there is nothing for a forward/backward rule to track. The alternative of
  staging the draw through each RE crossing (re-truncating and re-freezing at
  each stage, akin to `sim`'s per-step core) would tighten `q` against `f` and is
  a legitimate v2 mixing improvement, but it is optional: it changes the
  acceptance rate, not what the chain converges to.

  **What the simplification actually costs, and how to see it.** Every RE
  strictly inside a move's window that changes the moved dyad's true rate
  contributes to `f(Ω′)/f(Ω)` a compensator term the frozen `q` never modeled, so
  the mismatch shows up as excess rejections, never as a wrong stationary
  distribution. For a single RE at `t_r` splitting the window into true regimes
  `λ₀` (before) / `λ₁` (after), the expected log-acceptance penalty for a draw
  landing at `t′ > t_r` scales with `(λ₁ − λ₀)·(t′ − t_r)` — negligible when the
  RE barely moves the rate or the draw lands close to `t_r`, real when a large
  reciprocity/inertia jump combines with a draw deep into the far regime. This is
  a mixing-cost prediction, testable once the phase-1 toy-likelihood spike exists
  (D6) by stratifying the per-move acceptance-rate diagnostic (already feeding
  `em_trace`, D15/D18) by intervening-RE count per window; a measured acceptance
  crater specifically in the many-RE bins is the signal to consider the staged
  v2 proposal, not a correctness fix.

  **Support, not just rate magnitude, is the one place a crossing matters for
  correctness — and it is already handled.** If an intervening RE changes the
  moved dyad's *support* rather than just its rate's magnitude (the flip becomes
  cross-dyad-illegal past that point), a frozen-rate draw spanning the boundary
  can land in `f(Ω′) = 0` territory. That is exactly the cross-dyad
  support-illegality path specified below (rejected, `α = 0`, counted as an
  ordinary rejected step) — RE-crossing introduces no new case here, only more
  opportunities to hit the existing one.

  **Acceptance**: α = [f(Ω′)/f(Ω)] × [q_rev/q_fwd], with q_fwd/q_rev the products
  of the truncated-exponential time densities (two for permute, one for shift).
  The move-type and pick probabilities cancel because they depend only on
  move-invariant structure (event counts, wave membership, chain constraints) — a
  property to protect when extending the move set. q_rev needs the moved pair's
  frozen rate **under Ω′**, emitted as a byproduct of the same pass that computes
  f(Ω′): the engine's `what` surface (D3) gains a named-pair rate-at-position
  query. *Amended 2026-09-24 (`rate-queries`):* the query is **not** a `what`
  extension of the engine pass — `rate-queries` D1 rejected that path, because a
  rate read from preprocessed statistics would be a second definition of the
  state beside the walk and could drift from the `Λ` the time redraws use
  (invariant 2 below). The rate is read from the walk handle instead: the
  candidate is replayed to the requested position (`walk_replay_to()`) and the
  pair's intensity read through the simulation driver's evaluation steps
  (`pair_intensity()`: DyNAM rate_i × p(j | i), REM the dyad's rate). The
  closure returns it alongside f(Ω′) in the same call, but from a separate
  forward pass over the walk.

  **Cross-dyad support legality — check without a redraw normalizer.** Two illegality
  sources are handled differently. *Same-dyad chain-order* violations are excluded
  **before** proposing (permute bullet): the check is local (the two events' dyad +
  flavor order, no state replay) and the exclusion is move-invariant, so it cancels in
  α. *Cross-dyad support* violations — the coupling regime that strands `sim` and forces
  `random` to redraw — are **not** excluded from the proposal set: doing so would make
  the valid-pair set state-dependent and break the pick-probability cancellation α relies
  on. Instead the move is proposed freely and, when a cheap support lookup finds the swap
  fires a flip without applicable support, it is treated as `f(Ω′) = 0 ⇒ α = 0` and
  rejected — **just another (rejected) step on the chain**, counted in the sweep like any
  rejection. This needs **no** reject-and-redraw normalizer (unlike `random`): the target
  genuinely has zero mass on support-illegal sequences, so `f(Ω′) = 0` is the correct
  target, not a proposal correction, and q_fwd/q_rev are untouched. The lookup earns its
  keep by (a) skipping the full preprocess of a doomed proposal and (b) dodging the walk's
  not-guaranteed-−Inf-on-illegal-transitions hazard (rejected alternative below). Heavy
  coupling that makes most permutes illegal is a **mixing** cost (low acceptance), not a
  correctness one — a future move-set refinement, not v1.

  **Evaluator injection** (the likelihood inside the chain): each EM iteration the
  ABEM loop builds `make_proposal_evaluator(spec, theta_k)` —
  `eval_fn(candidate_seq, rate_queries) → list(loglik, rates)`, one preprocess +
  one likelihood pass per proposal — and hands it to the augmenter's `init()`.
  *Amended 2026-09-24 (`rate-queries`, landed):* the built signature is
  `make_proposal_evaluator(sub_models, theta_k, spec = <completed spec>)`, and
  `rate_queries` is a list of `list(after = k, layer, flavor, sender, receiver)`
  answered as a numeric vector in query order; `after = k` is the state
  immediately after the candidate's k-th dependent event is applied (no later
  row, even one sharing its stamp), so "frozen at the state after the preceding
  panel event" is `after = ` that event's ordinal among Ω′'s dependent events.
  A proposal that requests rates also costs one `walk_open()` (a formula
  recompile) plus one forward replay; `rates = NULL` does no walk work. Caching
  a live handle across a chain's successive proposals is left to this change's
  chain driver (`rate-queries` D5), an optimization rather than a contract.
  *Amended 2026-09-30 (task 3.5b, user decision): where the forward rates come
  from.* The text above supplies `q_rev` but not the rates `q_fwd` draws with,
  which are needed **before** the candidate exists. Rates are frozen on the
  sequence a move **starts from**. Forward rates are read on the current Ω;
  reverse rates are read on Ω′, since the reverse move starts there. Each
  placement's rate is anchored at its window's lower bound, which is an unmoved
  panel event or a wave. `after =` counts Ω's (or Ω′'s) modeled events stamped
  at or before that bound. For a shift, and for permute's first factor, the
  anchor lies in the shared prefix, so forward and reverse read the same state.
  The permute's second anchor is `pred(k)` among unmoved events; after the swap,
  the state up to it differs between Ω and Ω′. The closure therefore gained a
  third argument,
  `eval_fn(candidate_seq, rate_queries = NULL, loglik = TRUE)`. With
  `loglik = FALSE` it answers the rate queries alone: one walk, no preprocess,
  and `loglik = NA`. The chain calls it that way on Ω, whose log-likelihood it
  already holds, draws the times, runs the cross-dyad support lookup, then
  calls it once on Ω′ for `f(Ω′)` and the reverse rates. Per proposal that is
  one rates walk on Ω, one legality walk (coupled specifications only), and one
  preprocess plus one rates walk on Ω′. *Rejected:* a full call on Ω, which
  repeats a preprocess whose result is discarded; requesting the next move's
  forward rates inside the candidate's call, which saves a walk after an
  accept, but a reject still needs a fresh call. That is left as an
  optimization. A window bounded by a wave reads the walk's clock at the last
  event before that wave, so a pinned rate there takes the previous period's
  plateau. The proposal stays exact, since MH corrects any honestly reported
  `q`, but it is looser.
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
  independently of either side of the seam. *Amended 2026-09-24:* the rate-query
  primitive was split out into its own change, **`rate-queries`** (landed on this
  branch: `walk_replay_to()`, `bind_candidate_data()`, `pair_intensity()`,
  `query_rate_at_position()` in `R/rate_queries.R`), which also wired
  `make_proposal_evaluator()`'s `rate_queries` argument. This change therefore
  **consumes** the rate query rather than shipping it; its evaluator work
  (D2/D3, task 4) is the batched loglik/score/Fisher pass only.

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
   normalize; they are just biased). *Amended 2026-09-25 (pinned fids):* for a completed
   timed rate, that `Λ` is not computed from θ_k on either side. Both sides read the
   pin from `spec$completed_rates` of the spec completed once at entry (D21): the walk
   through `query_rate_at_position(pinned =)`, the evaluator through its fixed-only
   sub-model. One table and one period rule (`intercept_only_rate_period()`: half-open
   `[w_{k-1}, w_k)`, the last period closed) serve both sides. The pin has one plateau
   per wave period, and every interval's compensator must read exactly one
   `intercept_w`. The merged-walk right-censoring contract does **not** by itself put a
   boundary at panel wave times (D5). Non-straddling comes from D5's per-wave-window
   preprocessing, where each wave is a window end, or from the injected interior-wave
   boundary if a sequence is preprocessed as one stream. On the `q` side it comes from
   `sim`'s truncation anchor including the wave end. Every `f` path inherits this
   requirement: the batched pool evaluator, and `make_proposal_evaluator()`'s `loglik`
   preprocess of an MCMC candidate. A candidate preprocessed as one stream without the
   injected boundary would score a straddling interval at one plateau.
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
θ_k)` (D20) is built on that completed spec. *Amended 2026-09-24 (`rate-queries`):*
"none call `walk_open`" no longer holds for `augment_seq_mcmc()` — its injected
closure opens a walk per proposal whenever it requests rates. The conclusion is
unchanged, and stronger: that walk is opened over the candidate **rebound** into the
entry-completed spec (`bind_candidate_data()`, which checks `info`/`nodes` are
identical and assigns the data), never over a re-completed one, so the pinned rates
cannot move between proposals (`rate-queries` D6). The signature is
`make_proposal_evaluator(sub_models, θ_k, spec =)`. Completed fids are marked in the
`process_map` and contribute **no** θ: a completed timed rate is pinned (a θ-free
function of the wave counts, exposures and endpoint risk sets), and a uniform draw
has no coefficient at all. They appear in the result's `coef_layout()` as
`fixed = TRUE` slots at their frozen value (the pinned log-hazard, or `NA` for a
uniform draw), so the result (D7) still reports every auto-supplied sub-model, but
with no standard error and never in `coef()`. Not estimated does not mean absent
from the likelihood: a completed fid's density is a **θ-free per-path term**. It
enters the proposal `q` the augmenters record (they draw with it) **and** the target
`f` wherever paths are compared (the MCMC acceptance ratio, the stored reference
log-likelihood, the pool's importance weights). It cancels exactly in every cross-θ
ratio on a fixed path and contributes nothing to Q's score or Fisher information,
so the M-step never sees it (*resolved 2026-09-25*, see Open Questions). A user who wants the baseline hazard
estimated writes `rate = ~ 1` explicitly: an authored intercept-only rate is
modeled and reads its intercept from θ. This supersedes any reading of D2/D20 in
which an augmenter completes its own spec.

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

*Wave grid source (added 2026-09-30, task 2.3, user decision).* No data field names
the waves, so the grid the recipe is compiled on is **inferred**: the distinct times of
the **modeled** panel layers' rows (`panel_wave_grid()`). An exogenous panel covariate
adds no boundary, since one where the modeled layer records no snapshot would force its
flips to one side of it. A given `start_time` (`set_preprocessing()`) opens the grid
(earlier rows are history); without one the **first recorded wave** opens it and its
snapshot is the initial condition. `estimate_dynes(wave_times =)` overrides the whole
grid. Accepted limitation of the inferred grid: a wave at which no modeled panel dyad
changed leaves no row and is invisible, so its two neighboring intervals merge; the
explicit `wave_times` is the remedy. The compiled recipe (`goldfishAugRecipe`) carries
`events` (contiguous per chain in step order), `chains` (with `length`, the count-panel
multiplicity), `pairs` (same-interval, different-chain event pairs), per-layer `kind`,
flavor map and derived `support`, and the per-wave `start_states` of every network
layer; the cursor is `new_recipe_cursor()` (`take`, `remaining`, `done`, `reset`).

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
chain-loop invariant inside `augment_seq_mcmc()` (task 3.5). `abmcem` owns
`make_proposal_evaluator()` — the θ_k-closed closure whose return would widen to surface
the stats handle — and its injection into the augmenter's `init()`. So A1/A2/A3 straddle
the seam at exactly one point (the closure's return contract); everything else is this
change's. That single seam-crossing is why v1 ships A0 and the reuse is a coordinated
fast-follow, and why the v1 pool-entry path (task 4.2) is built **attachment-optional** so
the abmcem-side widening is the only remaining work when A3 lands.

### D24 — Count (multiplicity) panel layers: the second v1 panel kind
v1 supports **two** kinds of modeled panel layer, distinguished by what a wave records
rather than by how many flavors the layer carries:

| | state panel | count panel |
|---|---|---|
| a wave records | the tie state (0/1) | the cumulative count as of the wave |
| processes | two competing flavors (creation / dissolution) | one monotone process, **unflavored** |
| derived support constraint | yes (`~ tie(L)` / `~ !tie(L)`) | none — every dyad is always at risk |
| events per dyad per interval | 0 or 1 | `Δ = c(t_{m+1}) − c(t_m)` ≥ 0 |
| diff blind spot | create+dissolve inside one interval is invisible (minimal-flip) | **none** — a call cannot be un-made, so `Δ` is exact |

The motivating case is a phone-call panel estimated jointly with an observed email
stream. Such a layer is **not** flavored: `add_flavor()` requires a mapping of exactly
two values matching the layer's ±1 / 1-0 encoding, and a count layer has a single
monotone process with no inverse — so it is an ordinary unflavored dependent layer that
happens to be panel-observed. This is the **multiplicity** cell, orthogonal to the
ordinal (K > 2 states) cell D8's `(from,to) → flavor` chain text anticipates; that one
stays out of v1, unconstructible until `add_flavor()` accepts a multistate mapping.

What the count kind needs that the state kind does not:
- **A diff to a multiset.** Per dyad an interval carries `Δ` indistinguishable events,
  not a flip. `Δ < 0` on a monotone layer is a **data error** and aborts — a layer that
  genuinely decreases is the ordinal cell, not this one. The `augmentation_recipe` (D22)
  holds per-dyad remaining counts, which its `sim` cursor already anticipated, in place
  of chain membership.
- **Same-dyad moves are identities, not illegalities.** In a state panel a same-dyad
  permute would violate chain order and is excluded before proposing (D20). In a count
  panel the two events are indistinguishable, so the swap yields the *same* sequence:
  excluded at the same place for a different reason, and the pair-count normalizer in
  `q` must be computed under whichever convention is chosen and applied identically
  forward and backward.
- **No support constraint, so `sim` cannot strand.** D20's reject-and-restart exists for
  cross-dyad support coupling; a count layer derives no support, so its greedy forward
  draw always completes. The restart path stays, for state panels.

What it gets for free: the likelihood (repeated dyadic events are goldfish's paradigm
case), the effect machinery — where `weighted =` and `transformer_fn =` become
*meaningful* rather than decorative, the latent state being valued — and D6's
fixed-cardinality ergodicity argument, which here holds exactly instead of modulo the
minimal-flip assumption.

**A closed-form oracle comes with it.** With exogenous-only effects and covariates
constant between waves, a count panel's dyad counts are Poisson in `λ_ij · Δt` with
times distributed as uniform order statistics given the counts, so the whole ABEM stack
must reproduce the Poisson-regression MLE to Monte-Carlo error. That is a stronger test
than the recovery study, whose target is another draw from the same machinery. (The
state-panel analogue is the two-state CTMC with closed-form transition probabilities.)

*Rejected:* representing a count layer as a one-flavor flavored layer — `add_flavor()`
rejects it on two counts, and the flavor would carry no information; the honest
description is an unflavored dependent layer whose panel observation yields
multiplicities.

### D25 — The validation study reads only converged replicates
The SE calibration added to task 6.2 (from `louis-observed-information`) compares, per
coefficient, the empirical SD of θ̂ across replicates with the mean `vcov()` SE. That
comparison measures Louis's missing-information term only if every θ̂ is at its M-step
optimum. The abmcem SGD defaults (`step_size = 0.001`, constant, 100 M-step iterations)
do not reach it: in the 6.2b pilot every M-step hit the cap with gradient norm 0.009, and
on the toy with norms of 0.9–14 (abmcem D17 follow-up). A fit that stops early sits
between θ₀ and θ̂. That shrinks the replicate spread and inflates Wald coverage, so the
SD/SE ratio would measure the optimizer rather than the covariance.

The study therefore:
- runs every replicate under **one fixed, tuned `set_sgd_options()`** (step size, decay
  schedule, a raised `max_iterations`), chosen on a pilot cell and reported with the
  results;
- records each replicate's final M-step **gradient norm and convergence flag**, and
  treats a non-converged replicate like a `covariance_status` that is not `"ok"`: counted
  per cell, stated beside the summaries, and excluded from the MC-to-asymptotic error
  ratio and the SE calibration.

A rising non-converged share is reported as a finding of its cell, not hidden by the
exclusion.

**The v1 gate (amended 2026-10-09, user decision).** A replicate counts as converged
when the exact Newton step from θ̂ to the optimum of its final pool is under **0.1 SE**
in every coordinate: `max |I⁻¹ s̄ / SE| < 0.1`, where `s̄` is the weighted mean score
at θ̂ in the fit's final covariance pass and `I⁻¹` is `vcov()`. The M-step's own
convergence flag is recorded but does not gate. Its 1e-3 tolerance on the raw summed
score sits below the minibatch noise, so it almost never fires: on the cluster sweep
it was FALSE in nearly every coupled cell and every cell from f15 up, while the median
Newton step there was 0.01–0.05 SE. Gating on the flag would have discarded fits that
sit at their optimum. The Newton step answers what D25 asks, whether θ̂ is at the
M-step optimum, in the units the SE calibration reads. The 0.1 SE threshold is a v1
choice: it keeps the optimizer's share of the replicate spread to about 1% of the
variance (0.1² of one SE²).
Possible improvements in later versions, none required for v1: a stop rule in the
M-step itself that is scale-aware (`sgd-scale-aware-default`), so the package's own flag
becomes the gate; a per-coefficient gate in place of the maximum, so a flat,
unidentified direction (the `recip` choice at low flip counts) does not decide for the
whole replicate; and a final Newton step on the pool, which would make the gate hold
by construction.

*Rejected:* fixing the SGD default first. A scale-aware default (adagrad/Adam, or a
step scaled by the information) changes every DyNES fit and is its own change
(`sgd-scale-aware-default`), so the study does not wait on it. *Rejected:* running at
the defaults and reading the bias as MC error. That is how the pilot's 0.02-SE gap was
first read, and SE calibration would inherit the same confusion.

### D26 — The IS-ESS collapse floor is set by a tested code change
`importance_ess_collapse_threshold()` (`R/diagnostics_dynes.R`, abmcem's message
surface) returns `NA_real_`, so the runtime switch-to-`augment_seq_mcmc()`
recommendation is switched off until this change calibrates it. Task 6.2 locates the
floor, but the study is not a red/green task. Writing a number into the accessor turns
on a warning that existing tests and snapshots have never seen. So the value lands as
its own red/green pair (task 6.2d), not as a side effect of the study write-up.
- **RED:** below the floor, the recommendation fires under `random`/`sim`, never under
  `mcmc` (whose IS-ESS is trivially the pool size), and at most once per run.
- **GREEN:** set the value the study located, and re-check the snapshots that start to
  carry it.

If 6.2c finds the floor differs with coupling, the accessor gains a coupling argument
and the vignette states both floors. A single scalar would otherwise give the wrong
trigger to one of the two cases.

**The cliff does not abort the run (2026-10-05, from the 6.2 pilots).** At the top
flips level, random, sim and mcmc all aborted with `goldfish_abem_retry_exhausted`
at iteration 1–2 (Q < 0 on all 21 passes), and an amendment briefly proposed that the
abort name `augment_seq_mcmc()` below the floor. It was withdrawn. `mcmc`, whose ESS is
K by construction, aborted the same way, and adagrad certified every step on the same
cell. The abort was the constant default SGD step overshooting a large summed score,
not importance-sampling collapse. With a stable step, the collapsed run survives to the
mid-run check (ESS ≈ 10 of 50–5000 on every pass), which is where the recommendation
fires. The abort keeps its "smaller `step_size`" advice. The step-size default itself
belongs to `sgd-scale-aware-default`.

## Risks / Trade-offs

- **Spike results overturn a gated decision** — *happened* (2026-09-30): B1
  reversed D3's Option-A engine to the existing `cpp` backend, B3 fixed D4 as
  in-memory with a shared per-window start state, and the task-1.4 toy made D5's
  per-window route non-load-bearing. Task 1.5 amended the design and the
  `sequence-augmentation` spec before any later phase started.
- **The completed pin reads wave snapshots one period late** (found by the task-1.4
  toy): `panel_wave_risk_set()` reads each boundary with strictly-before semantics,
  so rows stamped at a wave are missing from its snapshot and the first period pins
  to `−Inf`. Latent today only because `estimate_dynes()` passes no `wave_times`.
  → fixed inside task 2.2 (added pair 2.2e/2.2f: one inclusive-at-wave snapshot
  definition shared by the wave diff and the pin), which must land before task 6.1
  wires `wave_times`.
- **Defects in `abmcem`'s landed code** (task-1.4 toy): `pool_fixed_loglik()` can
  return `−Inf` with `degenerate = FALSE`, and the SGD defaults (`step_size =
  0.001`, constant, on a raw summed score) under-step so every M-step hits its
  iteration cap. → flagged to `abmcem` as follow-ups; the degenerate flag must be
  fixed before task 6.1 can expose a `−Inf` pin (2.2f removes the known cause).
  *Amended 2026-10-01:* the degenerate flag (follow-up 1) is fixed by the
  change `fixed-loglik-degenerate`, landed on `feature/dynes`: any non-finite
  fixed-only term marks the sequence degenerate, `estimate_dynes()` warns once
  (`goldfish_dynes_fixed_term_degenerate`) and records `fixed_degenerate` in
  `em_trace`, and a fully dropped pool stops with `converged = FALSE`. The SGD
  step-size follow-up remains open.
  *Amended 2026-10-02:* a real `estimate_dynes()` on `recipe_state_spec()` still
  crashed (SGD `sample.int(..., prob)`, or the MCMC single-finite-`log_proposal`
  abort) when every pooled sequence was singular, because the pool evaluator dropped
  any sequence whose free-block information was not invertible. That is a selection
  on the latent path, so it biased the weights. The change
  `singular-sequence-information`, landed on `feature/dynes`, fixes it: degeneracy
  now means a non-finite quantity, never a singular one (a singular sequence keeps
  its weight); the loop stops with `converged = FALSE` when no sequence has both a
  finite log-likelihood and positive weight
  (`goldfish_dynes_pool_degenerate`); and a singular pooled information warns,
  naming zero-information coefficients (`goldfish_dynes_singular_information`).
  Nothing in D19–D22 changes. The 6.2 recovery study is unblocked.
- **Identifiability of separate creation/dissolution parameters from sparse waves** →
  the simulation recovery study (task 6.2) quantifies it on DyNES's own estimand, with
  the optional RSiena cross-check (D6) as supporting prior art only; `summary()` exposes
  MC error so weak identification is visible, and the simulation study defines guidance
  for the vignette.
- **Importance-weighted augmentation degenerates as flips-per-interval grows** (the
  sequence space dimension is ~#flips; IS weight variance grows roughly exponentially in
  it, and the ESS guard/pool-growth *detect* collapse but do not *recover* it in high
  dimensions) → the recovery study (task 6.2) adds a flips-per-interval stress sweep
  (~2 → 5 → 15 → 40) to locate the ESS cliff empirically, and the vignette (task 6.3)
  states the resulting operating envelope so sparse-wave / many-change regimes are a
  documented limit rather than a silent one. At **runtime**, when effective sample size
  collapses under an importance-weighted augmenter (`random`/`sim`), the loop emits an
  actionable `cli` recommendation to switch to `augment_seq_mcmc()` rather than only
  surfacing silently-wide MC error — the **message surface owned by `abmcem`** (the EM
  loop/diagnostics), the **collapse threshold owned by this change** (the 6.2/6.3
  degeneracy study that measures where ESS cliffs), so the two cannot drift.
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

- **[resolved 2026-09-30, task 1.5]** Final engine batching shape (D3) and pool
  storage format (D4): the existing `cpp` backend as a per-sequence loop (no batching
  wrapper, no new kernel), and an in-memory pool with the per-window start state held
  once per process. See the Phase-1 revisions under D3 and D4.
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
- **[resolved 2026-09-30, task 1.5]** The D10 thread-budget crossover: none exists
  on the measured kernels. BLAS (reference ≈ vecLib, default ≈ pinned threads) is
  irrelevant at p ≈ 3–5, and sequence sharding gives 4.6–4.7× at W = 5 for every
  engine up to K = 10 000. The "don't nest" invariant stays as hygiene; the adaptive
  `blas_threads` formula has no measured crossover behind it. The large-n dyad cell
  was not run (non-gating).
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
- **[resolved — runtime ESS collapse, distinct from cold start]** Separate from the
  cold-start corner above (bad θ₀), effective sample size also collapses **mid-run** as
  flips-per-interval grows — the sequence-space dimension is ~#flips and IS-weight variance
  grows roughly exponentially in it, so at high dimension the `random`/`sim` importance
  weights degenerate no matter the θ. The guard *detects* this but cannot *recover* it by
  resampling. Resolution: **diagnose-and-guide**, emitting an actionable `cli`
  recommendation to switch to `augment_seq_mcmc()` (which does not pay the IS-dimension
  tax). **Ownership split so the two changes cannot drift:** `abmcem` owns the **message
  surface** (it holds the EM loop and per-iteration ESS diagnostics where the recommendation
  fires); this change owns the **collapse threshold** — the numeric ESS floor that means
  "switch now" is set by the 6.2/6.3 degeneracy study (the flips-per-interval stress sweep
  that measures the cliff), not guessed at the abmcem desk. Recorded here, mirrored into
  abmcem's D14. *Amended 2026-09-18 (see D14's "What ESS measures"):* the floor is an
  **IS-ESS** floor, applying to `random`/`sim` only — under MCMC's uniform weighting
  IS-ESS is trivially K, so the same number must never be read as an all-clear there; and
  the collapse has a **second axis besides dimension**, namely a posterior sharpened by
  coupling to a densely observed layer, which a forward proposal cannot track (task 6.2c
  sweeps it). Both axes recommend MCMC, so the trigger is unchanged; what changes is that
  the **operating envelope** is stated as MC standard error against the asymptotic SE
  (D7), not as the ESS cliff, and that a coupled specification can be recommended to MCMC
  at construction time from the θ-free `coupled` column without waiting for a measured
  collapse (D1).
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
- **[resolved 2026-09-18 — no longer blocks task 3.1]** Does the
  `multi-process-walk` handle actually support what D9/D20 assume it supports?
  Both gaps below were lifted by `process-simulation`'s substrate tasks, which
  hit the same two walls from the `simulate()` side and landed the fixes on
  `feature_simulation` (merged into this branch at `8c8275d`):

  - **Support constraints — lifted** (`process-simulation` task 2.0a, `8641656`).
    The handle advances the batch walk's own atom stores and recomputes each
    fid's mask with `build_mask_maintainer()`; user constraints and derived
    flavor masks ride the same path, and replay matches batch exactly on dyadic,
    receiver-axis and flavored fixtures. So `walk_evaluate()` does return
    constraint-masked output and D20's support-applicable risk set is readable
    off it, as assumed. Task 2.0f (`2026-09-16`, D13) additionally put every
    compiled unit's focal layer in the shared registry, which is what stops
    `walk_assert_covered_constraints()` refusing a derived flavor mask whose
    atoms read the focal layer — the paradigm DyNES case.
  - **Windowed effects — lifted** (`process-simulation` task 2.0c, `a21b20c`,
    on top of `preprocess-one-walk` running windows on the merged walk). The
    `build_walk_engine()` abort is gone and expiries self-schedule through the
    new `walk_schedule()` / `walk_next_breakpoint()` API. D19's "windowed/memory
    effects are allowed in v1" is therefore realized rather than aspirational;
    the trailing-row question windows raise is
    `observation-tail-right-censoring`'s, and it bears directly on D20 invariant
    3 (the trailing censored interval must be emitted at the observation extent,
    not at a window expiry past it).
  - **Composition change — lifted** (`process-simulation` task 2.0b): presence
    cursors advance through `walk_advance()`.

  **Residual gap, tracked elsewhere, not blocking:** a support constraint whose
  atoms read a *changing* object that **no formula term reads** is still not
  stepped by the shared schedule, and the handle refuses it
  (`goldfish_walk_unsupported`). That is the whole subject of the
  `constraint-objects-on-shared-walk` change (its `multi-process-walk` delta
  makes live maintenance of every constraint a SHALL, with the
  "constraint on an object no formula reads replays exactly" scenario). DyNES
  inherits the fix; nothing here needs to duplicate it. Task 3.1 keeps a
  verification step — assert on a real DyNES fixture that `walk_open()` opens
  and `walk_evaluate()` masks — rather than an implementation step.

  The original finding, kept for the record:
  - **Support constraints.** `walk_open()` aborts whenever `merged$support_constraints`
    is non-`NULL` (`R/walk_handle.R:325-330`), with a message naming "user support
    constraints." But `spec_constraint_plans()` / `and_compose_constraint()`
    (`R/make_specification.R:296-313`) compose the **derived** flavor-transition
    constraint (`derive_flavor_constraint()` — `~ !tie(layer)` / `~ tie(layer)`, emitted
    automatically for any mutually-exclusive flavored layer) into the same plan a user's
    own `support_constraint=` would occupy; nothing downstream distinguishes
    derived-only from user-supplied. Since a mutually-exclusive flavored layer is DyNES's
    normal case (Stage-A `flavored-processes`), `merged$support_constraints` is non-`NULL`
    on essentially every real DyNES spec, so `walk_open()` would abort on the paradigm
    case, not merely on the literal-user-constraint edge the message implies. Batch
    preprocessing (`run_merged_walk()`) carries no equivalent guard and already masks
    constraints correctly, so this is a live-handle-specific gap, not a substrate-wide
    one. D20's augmenter design (reject-and-redraw legality checks, `sim`'s
    support-applicable risk set) assumes `walk_evaluate()` returns constraint-masked
    output — task 3.1 ("augmenter as an external driver of the walk handle") has no
    corresponding task to extend `walk_open`/`walk_evaluate` to carry the constraint
    through.
  - **Windowed effects.** `build_walk_engine()` aborts on any windowed effect
    (`ctx$is_window_effect`, `R/preprocess_joint.R:1116-1120`, `.internal = TRUE`). This
    guard is shared by both `walk_open()` and batch preprocessing's `run_merged_walk()`
    (both call `build_walk_engine()`), so it is an inherited `make-multivariate-spec` v1
    substrate boundary, not specific to the handle or to this change. It does mean D19's
    "windowed/memory effects are allowed in v1 [for DyNES]" is currently unrealized:
    nothing in the merged multi-process walk — batch or live — executes a windowed
    formula yet.

  Both readings were correct when written (2026-09-07) and both are now stale;
  the resolution path taken was the third one nobody listed — the change that
  needed the same substrate first paid for it.
- **[resolved 2026-09-25 — pinned, reported only; θ-free per-path term in f]** Is a
  completed fid's pinned parameter part of the estimated θ? **No.** The pin is not
  estimated θ: the baseline hazard stays at its entry-time pin for the whole fit, and
  D21 is amended to say so (fixed `coef_layout()` slots, no SE, not in `coef()`). The
  landed code already reads this way, so it stands, and so do `rate-queries` D7 and
  D6's pinned-rate invariance argument (a pin cannot move between candidates because
  nothing estimates it). A completed fid's density is still **path-dependent**:
  `Σ_w e^{c_w} ∫_w |R(t)| dt` moves with where the latent flips land (every creation
  shrinks a dyad risk set; a saturating sender or a composition change shrinks a
  sender one; a completed rate on an observed RE layer whose inherited support reads a
  modeled panel layer follows the latent path). So it is not a constant that can be
  dropped by a cancellation argument. Where it belongs depends on what a comparison
  holds fixed:

  ```
  comparison                           varies     pinned term
  -----------------------------------  ---------  ---------------------------
  MCMC acceptance f(Ω')/f(Ω)            path       does not cancel -> in f
  pool weight f(Ω)/q(Ω), normalized     path       does not cancel -> in f
  D14 reweight f(Ω;θ)/f(Ω;θ_ref)        θ only     cancels exactly
  Q's score / Fisher, ascent-test ΔQ    θ only     zero / cancels
  ```

  So it enters `q` (`sim`'s draw density, MCMC's forward/reverse proposal densities
  via `rate_queries`: `q` is the density actually drawn from, D20) **and** `f`
  wherever paths are compared. Leaving it out of `f` would weight toward a
  posterior different from the one `q` drew under. Because it is θ-free, it is
  computed once per sequence when the sequence is drawn, added to the stored
  reference log-likelihood (D14), and never recomputed at a new θ. The landed
  contract's "constant offset" is correct as constant **per path**, not across the
  pool. `abmcem` carries this as the fixed-only sub-model rule.

  The question as originally raised: D21 said completed fids "appear in θ (0 params
  for a uniform choice, 1 for an added baseline hazard)". The landed code treats them
  as **fixed**: `coef_layout()` on a completed spec renders an autocompleted fid's slot
  as `fixed = TRUE` at the frozen intercept (`autocompleted_layout_block()`,
  `R/joint_parameters.R`), `flat_to_fid_theta()` never reads θ for a completed fid, and
  `assert_theta_layout()` therefore expects no sub-model to claim it. Two readings:
  - **Pinned, reported only** — the baseline hazard stays at its entry-time pin for
    the whole fit and D7 merely lists it in the result. Then D21's "appear in θ" is
    loose wording; amend it to "appear in the result's layout as fixed slots", and
    the code stands.
  - **Estimated** — the EM loop moves the baseline hazard. Then the pin is only a
    starting value, `coef_layout()` must render the slot free on a DyNES spec,
    `rate-queries` D7 (completed fids read nothing from θ) and D6's pinned-rate
    invariance argument need revisiting, and the log-likelihood path must carry the
    fid (see `abmcem`'s open question on fixed-only sub-models).
  Resolve before `abmcem` task 5.2 decomposes the joint spec into `sub_models`, since
  the construction-time layout check fixes one reading.
- **[deferred — v2] Should completion own the wave grid?** (added 2026-10-01,
  from `completion-idempotency`.) "No grid" has two meanings today:
  `estimate_dynes(wave_times = NULL)` resolves `panel_wave_grid()`, while
  `complete_generative_spec(wave_times = NULL)` pins on `default_window()`, one
  period over the layer's event-time range. v1 keeps completion as it is and
  has each DyNES consumer (`estimate_dynes()`, `compute_lik_seq()`,
  `simulate.goldfishDynesFit()`) resolve its grid through `panel_wave_grid()`
  and pass it in (task 6.1). The v2 alternative is that on a spec with a
  modeled panel layer, a NULL `wave_times` in completion itself means
  `panel_wave_grid()`. Gain: every consumer agrees by construction, including
  plain `simulate()` and any future one, and the per-consumer wiring goes away.
  Cost: single-window pins change for every panel spec completed without a
  grid, so `simulate()` snapshots of panel specs need re-recording, and the
  completion warning's count source changes for them. Revisit once
  `process-simulation`'s spec is final, so the simulation default is settled
  before completion's default changes under it.
