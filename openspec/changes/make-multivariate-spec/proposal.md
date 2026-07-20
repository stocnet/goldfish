## Why

DyNES estimates the co-evolution of panel-observed relational states and
time-stamped relational events: the panel process's latent event sequence is
augmented, and augmentation *couples* the processes — a relational layer whose
effects read the panel layer's state has likelihood terms that depend on the
latent paths. That joint model needs a specification that portrays several
co-evolving processes at once, and a preprocessing substrate that evaluates all
their formulas against one shared clock and process state. Neither exists:
`make_specification()` describes one process (possibly K-flavored on one layer,
via `flavored-processes`), and the recipe walks serve one specification.

For *fully observed* processes this gap does not matter — the factorized
likelihood (the `flavored-processes` D2 note, which applies unchanged across
layers) makes separate per-process estimation exact, so no multivariate
estimator is needed there. The multivariate surface exists precisely for the
case where augmentation makes the factors interdependent. This change builds
that surface and the walk substrate; the augmentation and estimation themselves
are `dynes-augmentation`'s.

## What Changes

- **`make_multivariate_spec(...)`**: combines `make_specification()` objects
  into a multivariate specification. At least one process MUST be
  panel-observed (the augmentation target); a combination of only fully
  observed processes is rejected at construction — those are exactly separable
  and should be estimated with the per-process estimators. DyNAM-i processes
  are excluded; choice_coordination and mixed ordered/timed processes are in
  scope; all processes share one node set in v1 (`multimode-network-support`
  relaxes this later).
- **fid vocabulary extended, unchanged in kind**: the `flavored-processes` D9
  `process_map` gains rows for every process (a K-flavored rate+choice process
  contributes 2K fids, a plain one 2) and a `coupled` column; integer fid stays
  the canonical identity, labels stay rendered-only.
- **Coupling detection**: a fid is coupled iff its effects or constraint atoms
  directly reference a panel-observed layer's state (direct reference only —
  observed events of intermediate layers are exogenous regardless of what those
  layers' own models reference). The specification print marks separable fids;
  the estimation surface (`estimate_dynes()`, in `dynes-augmentation`) MUST
  inform about separable fids in a mixed specification and abort when every fid
  is separable ("nothing here needs DyNES").
- **Merged one-walk preprocessing**: the two per-family walks become one
  single-clock walk hosting both statistic blocks (sender-indexed and
  dyad-indexed) with the multi-consumer routing generalized from flavor keys to
  the `(layer, flavor) → fid` lookup. Cross-process events right-censor timed
  rate fids of other processes exactly as cross-flavor events do; effect
  deduplication extends across processes within a statistic block, never across
  effect-dispatch families.
- **Walk handle (stepping + injection)**: `walk_open()` / `walk_advance()` /
  `walk_evaluate()` / `walk_inject()` — an external driver advances the clock,
  queries per-fid evaluations (rate vector / choice matrix at given
  parameters), and injects observed or sampled events into the shared state.
  Replay evaluation over an observed sequence falls out as a driver loop. This
  is the surface `dynes-augmentation`'s model-driven augmenter and `simulate()`
  consume (its "per-event simulation hook").
- **No estimation surface in this change**: there is no `estimate()` generic
  and no `estimate_multivariate()`; `estimate_dynes()` is the only multivariate
  estimator and lives in `dynes-augmentation`.

## Capabilities

### New Capabilities

- `multivariate-specification`: the `make_multivariate_spec()` surface —
  composition and validation of process specifications, the panel-process
  requirement, the extended fid/process_map vocabulary, coupling detection and
  separability marking, and the multivariate specification print.
- `multi-process-walk`: the preprocessing substrate — the merged single-clock
  walk over both statistic blocks, `(layer, flavor) → fid` consumer routing
  with cross-process right-censoring, per-statistic-block effect deduplication,
  the per-fid preprocessed output contract, and the
  `walk_open`/`advance`/`evaluate`/`inject` handle.

## Impact

- **Sequencing**: post-2.0.0, on the DyNES track. Consumes `flavored-processes`
  (fid/process_map vocabulary D9, walk-count-agnostic consumers D10, derived
  flavor constraints) and precedes `dynes-augmentation`, whose augmenters,
  `simulate()`, and `estimate_dynes()` bind to this change's walk handle —
  re-ground that proposal's "per-event simulation hook" and evaluator seams
  against this change when implementation starts. `multimode-network-support`
  later relaxes the same-node-set restriction.
- **Parallel development vs the 2.0.0 release changes** (branch plan in
  `.plan/mv_branch.md`, local): sections 1–2 (surface, coupling, union
  planning, routing) and section 4 (walk handle — its evaluation substrate,
  the `process-state-evaluators` capability, is already implemented) are
  parallel-safe against `residuals-gof` (file overlap: only `R/model_estimate.R`,
  different regions). Section 3 (the merged walk) MUST wait for
  `spec-driven-dispatch` (rewrites `fold_active_dyad_support` in
  `R/model_preprocess.R` and the writer's dyad encoding) and for
  `multimode-network-support` (touches `R/make_specification.R` /
  `R/formula_parser.R`) to land — all three edit the loop/writer/surface files
  this change's riskiest step rewrites. This change needs nothing from
  `residuals-gof`; the `evaluate_engine()` dependency is
  `dynes-augmentation`'s (E-step evaluation), not this change's.
- **R**: `R/make_multivariate_spec.R` (surface, validation, coupling, print);
  generalization of `R/preprocess_flavored.R` (routing lookup, cross-process
  union planning); the merged walk refactor of `run_sender_recipe_loop()` /
  `run_dyad_recipe_loop()` (the frozen-baseline gate applies: the
  single-process and flavored paths must stay byte-identical); the walk-handle
  API.
- **Frozen baselines**: untouched as the 1e-6 floor; the merged-walk refactor
  is the riskiest step and is gated by baselines PASS at every commit.
- **Docs**: multivariate specification vignette section; walk-handle developer
  documentation for the `dynes-augmentation` consumers.
