## Why

The ascent-based Monte Carlo EM (ABMCEM) algorithm that powers DyNES — the EM
ascent loop with its accept/grow/stop rules, the E-step weighting/resampling
machinery, the SGD M-step, and the Fisher + Monte-Carlo uncertainty — is fully
prototyped (`.plan/dynes/sgd_estimation_algorithm.R` drives it end to end;
`estimation_algorithm_functions.R` holds `sgd_refactor()`, `get_weights()`, the
resampling schemes, and `get_se_from_list()`) and fully decided
(`dynes-augmentation` design D1, D7, D13–D18). But inside the seven-phase
`dynes-augmentation` change it sits behind the spikes, the augmenters, and the
batched C++ evaluator. The prototypes prove the loop needs none of that to run:
they evaluate pools through the **existing zero-iteration estimation path**
(`estimate_wrapper()` with `max_iterations = 0L` returning logLikelihood /
finalScore / finalInformationMatrix). Carving the algorithm core into its own
change lets it land, be tested, and stabilize its contracts now — and gives the
later augmenter/evaluator work a running loop to plug into.

## What Changes

This change **carves out** `dynes-augmentation` phases 5 and 7.1 (the
`dynes-augmentation` artifacts are trimmed accordingly; its design decisions
D1, D7, D13–D18 are restated here as this change's own decisions and
implemented here):

- **Nested `set_alg_*()` control constructors**: `set_alg_em()` (EM loop,
  stop-rule quantiles, single seed, `n_cores`, `em_trace_se`) nesting
  `set_alg_augment()`, `set_alg_weights()`, and `set_alg_sgd()`; child-local
  cli validation, all cross-object rules (augmenter × weighting validity
  matrix, precedence warn-and-ignore) enforced in `set_alg_em()`.
- **E-step weighting machinery**: importance/uniform weighting, likelihood-ratio
  reweighting against each sequence's stored reference record, pre-normalization
  weight transformations, resampling schemes (stratified / residual / random),
  `refresh` mode, and the ESS guard with its two warnings.
- **Q and ASE as internal S3 generics** (`compute_q()` / `compute_ase()`)
  dispatching on the classed E-step object, so the EM loop is scheme-agnostic.
- **SGD M-step optimizer** behind an optimizer contract: weighted
  with-replacement batches with unweighted updates (default) and deterministic
  cyclic batches with importance-weighted gradients; constant default step size
  plus AdaGrad/Adam/momentum at literature defaults; state reset per M-step;
  score-only evaluation requests.
- **EM ascent control flow**: θ₀ via `set_estimation_opt()` (which gains a
  warm-start initializer option), bounded within-iteration pool growth
  (`max_retries`), hard `cli_abort()` failure semantics, and the always-on
  `em_trace` per-iteration diagnostics.
- **`estimate_dynes()` surface** running the ABEM loop purely through the
  augmenter / evaluator / optimizer contracts (no branching on variants inside
  the loop), marked experimental; the sequence-map seam (serial default,
  optional mirai daemons under the non-nested thread budget); mirai to Suggests.
- **Prototype-path pool evaluation**: an internal evaluator-contract
  implementation that computes per-sequence logLik / score / Fisher through the
  existing zero-iteration `estimate_wrapper()` path (as the prototypes do), so
  the loop is implementable and testable **before** the batched C++ evaluator
  and the sim/MCMC augmenters exist; the `dynes-augmentation` batched evaluator
  later replaces it behind the same contract with no loop changes.
- **Result contract**: estimates, Fisher-information approximation inverted by
  `vcov()` (NA-padded fixed parameters), Monte-Carlo standard errors,
  `em_trace`, and `summary()`/`print()` via cli showing asymptotic and MC error
  side by side.

Out of scope, staying in `dynes-augmentation`: the Phase-1 spikes, panel flag +
wave diffing, the three `augment_seq_*()` augmenters, the batched C++ pool
evaluator, process simulation, the multi-layer specification validation, and
the full parameter-recovery study. This change's end-to-end tests run on toy
fixtures with a contract-conformant stub augmenter and an analytic stub
evaluator.

## Capabilities

### New Capabilities

- `abmcem-optimization`: the algorithm machinery — E-step weighting schemes and
  the augmenter × weighting validity matrix, resampling, the ESS staleness
  guard, `compute_q()`/`compute_ase()` scheme dispatch, the SGD optimizer
  contract and its batching/step-size semantics, the EM ascent control flow
  (accept/grow/stop, bounded growth, failure semantics, `em_trace`), and the
  prototype-path pool evaluation contract.
- `dynes-estimation`: the user surface — `estimate_dynes()` running the ABEM
  loop through the step contracts, the nested `set_alg_*()` constructors, and
  the result object (Fisher-based `vcov()`, MC standard errors, `em_trace`,
  `summary()` diagnostics). *Shared capability*: `dynes-augmentation` also adds
  to `dynes-estimation` (specification validation, parameter recovery); the
  loop/constructor/result requirements move from its delta into this change's.

### Modified Capabilities

- `optimizer-selection`: `set_estimation_opt()` gains a warm-start
  initial-parameters option (draw one random augmentation, estimate on it, use
  those estimates as θ₀; default remains the zero vector).

## Impact

- **R**: new `R/model_estimate_dynes.R` (surface + ABEM loop),
  `R/algorithm_controls.R` (the four constructors),
  `R/algorithm_weights.R` (weighting, resampling, ESS, Q/ASE generics),
  `R/algorithm_optimizer_sgd.R` (M-step), `R/algorithm_evaluate_pool.R`
  (prototype-path evaluator adapter), result/`vcov()`/`summary()`/`print()`
  methods. No `src/` changes (the prototype path reuses the existing engine).
- **Dependencies**: mirai enters Suggests (serial default map seam).
- **Prototypes retired**: `sgd_estimation_algorithm.R` Part 2 and the
  optimizer/weighting/SE functions in `estimation_algorithm_functions.R`
  (`sgd_refactor()`, `get_weights()`, `stratified_resample()`,
  `residual_resample()`, `clipping()`/`smoothing()`, `get_se_from_list()`,
  `unfixed_params()`, `add_na_rowcol()`, `std_error_fixed_params()`) become
  reference implementations superseded by package code.
- **`dynes-augmentation` trimmed**: tasks 5.1–5.4 and 7.1 removed (this change
  implements them); its `dynes-estimation` spec delta keeps only specification
  validation and parameter recovery; its design gains carve-out pointers.
- **Sequencing**: this change is implementable now against the current engine
  (loop + contracts tested behind a minimal spec fixture and stub steps).
  `estimate_dynes()`'s public signature takes a `make_multivariate_spec()` object
  (`make-multivariate-spec`), so that change lands before the surface ships;
  `dynes-augmentation`'s augmenters and batched evaluator plug into the contracts
  this change ships. Full panel-data estimation end to end still requires
  `dynes-augmentation` (panel diffing + real augmenters).
