## Why

Relational states are often observed only as **panel waves** (friendship at t1, t2, …),
not as time-stamped event streams — the event-sequence likelihood that goldfish
maximizes is unavailable because the sequence between waves is latent.
DyNES (Dynamic Network Evolution of States) closes the roadmap: it augments the latent
creation/dissolution event sequences between consecutive waves and estimates the
flavored competing-process parameters by ascent-based Monte Carlo EM (ABEM). The
specification surface is Stage A's (`flavored-processes`) unchanged; the panel seam is
the per-layer panel-semantics flag reserved by `refactor-single-data-object` (its D5);
working prototypes of every algorithm step exist in `.plan/DyNES/` (SGD, resampling,
and importance-sampling estimators over sampled chains) and RSiena's ML estimator
answers the sequence-sampling statistics. This change turns those prototypes into the
package's estimation core.

## What Changes

- **Panel-semantics flag implemented**: a layer flagged with panel (snapshot)
  semantics is interpreted as state observations; consecutive waves are diffed into
  candidate flip events (Hamming set), and such a layer MAY be `focal` — but only under
  `estimate_dynes()` (event-stream estimators keep aborting).
- **`estimate_dynes(spec, algorithm = set_algorithm_abem(...))`**: new estimation
  surface consuming a Stage-A flavored specification over a panel focal layer.
  `set_algorithm_abem()` (aligned with the `set_*_opt()` family) controls the
  augmentation flavor (random / model-driven / MCMC-mutation), pool size, importance
  sampling / resampling, batch size, `max_iter`, and `tolerance`.
- **Three step-family contracts**, mirroring the writer strategy contract
  (init / step / finalize), so variants plug in like preprocessing writers:
  - **Augmenters** build wave-consistent latent sequences:
    `augment_sequence_random()` (uniform ordering of the Hamming flip set),
    `augment_sequence_model()` (model-driven draw at the current parameters), and
    `augment_sequence_mutate()` (MCMC moves on an existing sequence — permutations and,
    per the RSiena study, excursion insert/delete moves).
  - **Evaluators** compute E-step quantities for a pool at a parameter vector:
    batched log-likelihood / score / Fisher over preprocessed sequences, plus the
    importance weights (weights live in the evaluator, never the augmenter).
    `compute_lik_seq()` is the per-sequence sugar (named to avoid the `logLik` method
    collision).
  - **Optimizers** perform the ascent step: stochastic-gradient, resampling, and
    importance-sampling ABEM variants from the prototypes.
- **Per-event simulation hook implemented**: the recipe loop's
  documented-not-implemented hook (visible state at event i, event-stream append)
  becomes the surface the model-driven augmenter consumes.
- **Batched sequence evaluation in C++**: the engine contract is C++ over a pool of
  default flat preprocessed objects (memory-efficient via the broadcast encoding); a
  full re-preprocess per drawn sequence is the baseline (incremental patching
  explicitly deferred).
- **Result contract**: ABEM delivers estimates and a Fisher-information approximation
  usable for `vcov()`, plus Monte-Carlo standard errors and convergence diagnostics
  surfaced by `summary()`.
- **Process simulation**: a `simulate()` surface generating event sequences from a
  specification and parameters through the same generative source + multi-consumer
  walk the augmenter uses. Stopping by time horizon or **fixed event count** (the
  latter doubling as the guard against rate explosion under super-linear feedback
  terms, with a hard `max_events` diagnostic); timed sub-models draw exponential
  waiting times from the total rate; ordered/Cox-like sub-models (no identified
  baseline) support fixed-template-times-redraw-dyads and crude-rate pseudo-time
  modes (relevent's `simulate.rem.dyad` precedent: `nsim` + `redraw.timing`);
  choice-coordination simulates by mutual-choice rejection (constant rate, uniform
  sender, crude-rate waiting time, accept iff the proposal is reciprocated).
  Simulated pools are evaluator-compatible sequences, closing the simulate →
  augment → evaluate loop.
- **Phase-1 spikes are tasks, not prerequisites**: the B1 likelihood-evaluation
  benchmark (K ∈ {10, 100, 1000, 10000}), the B3 memory/pool-format profiling
  (acceptance: Social-Evolution-scale pools of 100–1000 sequences under ~2 GB), the
  RSiena ML-estimator study (proposal moves, excursions, identifiability, MC-error
  diagnostics), and an end-to-end toy prototype that shakes the three contracts.
  Spike-gated design decisions are marked and revised from the measured results.

## Capabilities

### New Capabilities
- `sequence-augmentation`: the algorithm-step contracts — wave diffing into candidate
  flip events, the augmenter/evaluator/optimizer interfaces, endpoint-hitting sequence
  validity, pool storage, batched C++ sequence evaluation and `compute_lik_seq()`, and
  the per-event simulation hook consumption.
- `dynes-estimation`: the user surface — `estimate_dynes()`, `set_algorithm_abem()`,
  panel focal layers, the ABEM iteration/convergence contract, and the result object
  (estimates, Fisher-based `vcov()`, MC standard errors, `summary()` diagnostics).
- `process-simulation`: generating event sequences from a specification and
  parameters — stopping rules and explosion guard, per-family timing strategies
  (exact, fixed-template, pseudo-time), the coordination rejection scheme, and the
  evaluator-compatible output contract.

### Modified Capabilities
- `single-data-object`: the reserved per-layer panel-semantics flag activates — a
  panel-flagged layer resolves as snapshot observations diffable into candidate events,
  and the "panel layer cannot be focal" error is scoped to event-stream estimators
  (DyNES accepts panel focal layers).
- `preprocess-output-writers`: the per-event simulation hook moves from
  documented-not-implemented to implemented (the sampling writer and parallel chunking
  extension points remain documentation-only).

## Impact

- **R**: new `R/model_estimate_dynes.R` (surface + ABEM loop), `R/algorithm_steps_*.R`
  (augmenter/evaluator/optimizer constructors), panel diffing in the conversion module,
  simulation hook in the recipe loop, result/summary/vcov methods.
- **C++ (`src/`)**: batched pool evaluation (logLik/score/Fisher over a list of flat
  preprocessed objects); rate/probability computation at a given process state reused
  from the estimation kernels (the prototypes' workarounds replaced by engine calls).
- **Prototypes retired**: `.plan/DyNES/*.R` (sgd / resampling / importance-sampling
  scripts against goldfish 1.6.10) are the reference implementations; their workarounds
  (opportunity-list support constraints, manual state bookkeeping, label sanitization)
  are all superseded by the current pipeline.
- **Sequencing**: consumes `flavored-processes` (specification surface, derived
  constraints, per-flavor preprocessing) and `refactor-single-data-object` (panel flag
  seam, stocnet input); interacts with the future `make_multivariate_spec()` change
  (multivariate specs over a pool evaluate through the same batched contract).
- **Dependencies**: **mirai enters Suggests** (optional sequence-level parallelism
  through a serial-default map seam; parallelization is sequence-level only, under a
  non-nested workers × BLAS-threads ≤ cores budget — design D10); pool spill-to-disk
  (if the B3 spike forces it) would reuse the existing DBI writer machinery.
- **Large-n limitation recorded**: dyad-indexed models scale as n² rows per event;
  the designated remedy is the alternatives-sampling gather writer (documented
  extension point of `preprocess-output-writers`), a follow-up change, not part of
  this one (design D11).
- **Docs**: new vignette on panel-state estimation; `summary()`/`vcov()` semantics
  under MCEM documented (MC error alongside asymptotic error).
