## Why

Relational states are often observed only as **panel waves** (friendship at t1, t2, …),
not as time-stamped event streams — the event-sequence likelihood that goldfish
maximizes is unavailable because the sequence between waves is latent.
DyNES (Dynamic Network Evolution of States) closes the roadmap: it augments the latent
creation/dissolution event sequences between consecutive waves and estimates the
flavored competing-process parameters by ascent-based Monte Carlo EM (ABEM). The
specification surface is the multivariate one (`make-multivariate-spec`,
composing `flavored-processes` specifications); the panel seam is the
`observation = "panel"` metadata the living `single-data-object` spec already
carries — augmentation is triggered by a panel layer being **referenced in the
formulas** (as a modeled process or an exogenous covariate), no separate flag;
working prototypes of every algorithm step exist in `.plan/DyNES/` (SGD, resampling,
and importance-sampling estimators over sampled chains) and RSiena's ML estimator
answers the sequence-sampling statistics. This change turns those prototypes into the
package's estimation core.

## What Changes

- **Panel augmentation by formula reference**: a panel-observed layer that is a
  **modeled process** (its focal/dependent layer) is interpreted as state
  observations; consecutive waves are diffed into candidate flip events (Hamming
  set) whose latent between-wave path is augmented. A modeled panel layer is a
  **dependent (focal) process of its composed specification** — resolved through
  `make_joint_specification()` and the `process_map` (`make-multivariate-spec`), not
  through the data object's `info$focal`, which selects the single dependent stream
  only on the event-stream path. Such a panel-dependent process is estimable only
  under `estimate_dynes()` (event-stream estimators keep aborting). A panel layer referenced only as an **exogenous covariate** (no
  formulas of its own) is **not** augmented: it stays a **static step-covariate**
  (state jumps only at wave times, the living `single-data-object` behavior — not
  latent, no random sampling). A panel layer referenced nowhere is not augmented.
- **DyNES requires a latent panel path**: `estimate_dynes()` aborts when **no panel
  layer is a modeled dependent process** (nothing is latent). The error names
  `estimate_dynam()`, explaining the specification fits DyNAM and that its panel
  layers would only be considered as static exogenous covariates.
- **`estimate_dynes(spec, control_algo = set_algorithm_em(...))`** — the estimation
  surface, `set_algorithm_em()` and its three nested control constructors, the ABMCEM
  loop, and the result contract are **carved out to the `abmcem` change**
  (which implements them now against a prototype-path evaluator); this change
  supplies the panel data path and validation those contracts consume, and
  wires the multi-layer specification validation into that surface.
- **Three step-family contracts**, mirroring the writer strategy contract
  (init / step / finalize), so variants plug in like preprocessing writers:
  - **Augmenters** build wave-consistent latent sequences:
    `augment_seq_random()` (uniform ordering of the flip set),
    `augment_seq_sim()` (constrained model-driven draw at the current parameters),
    and `augment_seq_mcmc()` (MCMC moves on an existing sequence — permute and
    shift moves with rate-based time redraws in v1; the RSiena-studied excursion
    insert/delete moves are recorded future extensions).
  - **Evaluators** compute E-step quantities for a pool at a parameter vector:
    batched log-likelihood / score / Fisher over preprocessed sequences, plus the
    importance weights (weights live in the evaluator, never the augmenter).
    `compute_lik_seq()` is the per-sequence sugar (named to avoid the `logLik` method
    collision).
  - **Optimizers** perform the ascent step: one SGD optimizer (weighted or
    deterministic-cyclic batching, constant or adaptive step sizes); the
    prototypes' IS-vs-resampling distinction lives in the weighting scheme,
    orthogonal to the optimizer. *Implemented by the `abmcem` change* — here
    the contract is only consumed (the batched evaluator swaps in behind it).
- **Augmenters drive the walk handle**: the model-driven augmenter steps the
  `multi-process-walk` handle (`make-multivariate-spec`:
  `walk_open`/`advance`/`evaluate`/`inject`) as an external driver, rather than
  registering a callback in the recipe loop. The stepping substrate is owned by
  `multi-process-walk`; this change consumes it.
- **Batched sequence evaluation in C++**: the engine contract is C++ over a pool of
  default flat preprocessed objects (memory-efficient via the broadcast encoding); a
  full re-preprocess per drawn sequence is the baseline (incremental patching
  explicitly deferred).
- **Result contract**: ABEM delivers estimates and a Fisher-information approximation
  usable for `vcov()`, plus Monte-Carlo standard errors, the `em_trace`
  per-iteration diagnostics, and convergence diagnostics
  surfaced by `summary()`.
- **Process simulation moved out**: the general `simulate()` surface is no longer
  this change's — it is lifted into the standalone `process-simulation` change (a
  family-agnostic S3 generic over the same walk handle). This change retains
  `augment_seq_sim()` as the **endpoint-conditioned** consumer of that change's
  per-step drawing core (it adds wave-endpoint conditioning, risk-set restriction,
  and proposal-density bookkeeping plain simulation has no reason to carry).
- **Phase-1 spikes are tasks, not prerequisites**: the B1 likelihood-evaluation
  benchmark (K ∈ {10, 100, 1000, 10000}), the B3 memory/pool-format profiling
  (acceptance set **empirically** from the measured pool footprint on the broadcast
  representation over Social-Evolution-scale pools of 100–1000 sequences — no fixed
  byte threshold; design D4), the move-set justification note (the fixed-cardinality
  ergodicity argument that permute + shift span v1's endpoint-conditioned space, so
  RSiena's increment/reduce excursion moves stay out of scope; identifiability handled
  by the recovery study, RSiena code-reading an optional cross-check — design D6), and
  an end-to-end toy prototype that shakes the three contracts.
  Spike-gated design decisions are marked and revised from the measured results.

## Capabilities

### New Capabilities
- `sequence-augmentation`: the algorithm-step contracts — wave diffing into candidate
  flip events, the augmenter/evaluator/optimizer interfaces, endpoint-hitting sequence
  validity, pool storage, batched C++ sequence evaluation and `compute_lik_seq()`, and
  the augmenters as external drivers of the `multi-process-walk` handle.
- `dynes-estimation`: this change's share of the shared capability — panel
  augmentation under `estimate_dynes()`, the multi-layer specification validation
  (consuming `make-multivariate-spec`'s coupling detection, not re-deriving it),
  and the parameter-recovery study. (The `estimate_dynes()` surface, the
  `set_algorithm_em()` constructors, the ABEM loop, and the result contract are the
  `abmcem` change's share of the same capability.)

### Modified Capabilities
- `single-data-object`: a panel-observed layer that is a modeled process resolves as
  snapshot observations diffable into candidate events, and the "panel layer cannot be
  focal" error is scoped to event-stream estimators (DyNES accepts a panel focal layer
  when it is a modeled process). A panel layer referenced only as an exogenous
  covariate keeps the static change-list step-covariate semantics (no augmentation). No
  reserved flag is introduced — the `observation = "panel"` metadata plus modeled-process
  reference is the augmentation trigger.

## Impact

- **R**: augmenter constructors (`R/algorithm_steps_*.R`) driving the
  `multi-process-walk` handle, panel diffing in the conversion module, multi-layer
  specification validation consuming the multivariate spec's `coupled` column.
  (Surface + ABEM loop, control constructors, optimizer/weighting machinery, and
  result/summary/vcov methods land via the `abmcem` change; the general
  `simulate()` surface lands via the `process-simulation` change.)
- **C++ (`src/`)**: batched pool evaluation (logLik/score/Fisher over a list of flat
  preprocessed objects); rate/probability computation at a given process state reused
  from the estimation kernels (the prototypes' workarounds replaced by engine calls).
- **Prototypes retired**: `.plan/DyNES/*.R` (sgd / resampling / importance-sampling
  scripts against goldfish 1.6.10) are the reference implementations; their workarounds
  (opportunity-list support constraints, manual state bookkeeping, label sanitization)
  are all superseded by the current pipeline.
- **Sequencing**: hard-consumes `make-multivariate-spec` (the `make_joint_specification()`
  surface `estimate_dynes()` takes, and the `multi-process-walk` handle the augmenters
  drive), `flavored-processes` (per-flavor preprocessing, derived constraints), the
  living `single-data-object` (panel `observation` metadata, stocnet input), and
  `process-simulation` (`augment_seq_sim()` reuses its per-step drawing core). Precedes
  `gof-dynes`.
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
