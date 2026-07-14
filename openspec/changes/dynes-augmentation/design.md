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
statistics questions (OQ B5).

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
- Flavor-filtered effects, `make_multivariate_spec()` (own changes; the batched
  evaluator is written so a multivariate spec pool can adopt it).
- GoF statistics/plots — the `simulate()` primitive IS in scope (D12); the
  goodness-of-fit surface built on it (observed-vs-simulated statistic
  distributions) is a follow-up.
- Modeling the rate part of choice-only (ordered) data by treating timings as
  latent and imputing them through the augmentation machinery — a genuinely new
  estimand the DyNES core makes reachable, recorded as a future extension
  (completely different estimation), not attempted here.
- Missing-data / partially observed waves beyond complete snapshots.

## Decisions

### D1 — Surface: `estimate_dynes()` + `set_algorithm_abem()`
`estimate_dynes(spec, algorithm = set_algorithm_abem(augmentation = c("mcmc",
"random", "model"), n_sequences, importance_sampling = FALSE, resampling = FALSE,
batch_size, max_iter, tolerance, seed))`, aligned with the `set_*_opt()` family.
The specification is a Stage-A flavored spec whose focal layer carries the
panel-semantics flag; event-stream estimators continue to abort on panel focal layers,
naming `estimate_dynes()`. *Rejected:* overloading `estimate_dynam()` with an
`algorithm` switch — panel augmentation changes the estimand's data requirements and
the result's uncertainty semantics; a distinct verb keeps both surfaces honest (and
matches the dev plan's naming).

### D2 — Step families as contracts mirroring the writer strategy
Three constructor families, each returning an object with `init(spec, waves, control)`
/ step / `finalize()` hooks, dispatched by `set_algorithm_abem()`:

- **Augmenters** — `augment_sequence_random()` (uniform ordering + uniform times over
  the wave-diff flip set; prototype `getChainSample()`),
  `augment_sequence_model()` (sequential draw from the model at the current
  parameters, consuming the per-event simulation hook; prototype
  `getChainSampleFromModel()`), `augment_sequence_mutate()` (MCMC moves on a current
  sequence: order permutations plus the RSiena-studied insert/delete excursion moves;
  prototype `permuteChainSample()`). Contract:
  `augment(state_t, state_t1, theta, control) → sequence(s)` — every returned sequence
  MUST hit the endpoint state exactly.
- **Evaluators** — `evaluate_sequence_pool(pool, theta)` → per-sequence logLik, score,
  Fisher contribution, and importance weights. Weights belong to the evaluator (the
  augmenter reports its proposal density; the evaluator forms the ratio); prototype
  `computeLl()` + `get_weights()`. `compute_lik_seq(spec, sequence, theta)` is
  per-sequence sugar over the batched path — named to avoid colliding with the
  `logLik()` S3 method (OQ B1 answer).
- **Optimizers** — `optimize_abem_sgd()` (damped stochastic gradient over sampled
  batches; prototype `sgd()`), `optimize_abem_resampling()`,
  `optimize_abem_is()` (importance-sampling updates): `step(e_stats, theta) → theta'`
  plus convergence bookkeeping.

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

### D4 — Pool storage: in-memory flat objects; ~2 GB acceptance; DBI spill fallback **[spike-gated]**
The pool is an in-memory list of flat preprocessed objects. Acceptance threshold from
the OQ B3 answer: pools of 100–1000 sequences at the scale of the packaged
`social_evolution` dataset stay under ~2 GB (a per-machine bound, divided across any
parallel workers). The B3 spike profiles a grid (n × events × K); if exceeded, the fallback order
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
additionally carries MC standard errors and convergence diagnostics (iterations,
ascent trajectory, pool effective sample size under importance weights). `summary()`
displays asymptotic and MC error side by side so users see when the pool, not the
data, limits precision. Fixed-parameter handling follows the prototypes'
`unfixed_params()`/`std_error_fixed_params()` (NA rows/cols for fixed positions).

### D8 — Panel flag activation and wave diffing
The per-layer panel-semantics flag (reserved by single-data-object D5) becomes
readable metadata: a panel-flagged layer's rows are snapshots; consecutive waves diff
into the candidate flip set (creations where 0→1, dissolutions where 1→0) per
interval, with wave times as hard boundaries the augmented sequence must hit. The
exogenous change-list interpretation of unflagged panel layers is untouched. Diffing
is a conversion-module function also usable standalone (the upstream
snapshot-diff-verb proposal to manynet remains open and non-blocking).

### D9 — Simulation hook: implement the documented recipe-loop extension point as the walk's source port
The per-event simulation hook lands exactly as documented by
`preprocess-output-writers`: after the stats update for event i, before advancing, a
registered callback sees the visible process state and may append to the event stream.
`augment_sequence_model()` is its first consumer (drawing event i+1 from the rates at
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
machinery. The sampling-writer and parallel-chunking extension points stay
documentation-only.

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
behavior). `set_algorithm_abem()` gains `n_cores` (default honoring CRAN's 2-core
check cap) and parallel RNG streams for reproducible draws. The B1 spike's
BLAS-default vs pinned × serial vs sharded crossing measures the crossover before the
evaluator phase, including a large-n synthetic cell. *Rejected:* multithreaded BLAS as
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

## Risks / Trade-offs

- **Spike results overturn a gated decision** (batched C++ not worth it; pools blow
  the 2 GB budget) → D3/D4 are explicitly provisional; Phase 1 ends with a
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

Purely additive surface (`estimate_dynes()`, `set_algorithm_abem()`, step
constructors); no existing estimator behavior changes except the panel-focal error
message gaining the DyNES pointer it already promises. The frozen coefficient
baselines are untouched throughout. Rollback is reverting the change's commits;
Phase-1 spike artifacts live in `.plan/` (gitignored) and impose nothing on the
package.

## Open Questions

- **[gated by Phase 1]** Final engine batching shape (D3) and pool storage format
  (D4) — resolved by the B1/B3 spike measurements, folded back into design + specs by
  the Phase-1 revision task.
- **[gated by Phase 1]** Exact MCMC move set for `augment_sequence_mutate()`
  (permutation-only vs excursion insert/delete) and its acceptance-ratio bookkeeping —
  resolved by the RSiena study note.
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
