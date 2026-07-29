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

- **`make_joint_specification(...)`** (the constructor, renamed from the working
  `make_multivariate_spec()`; returns a distinct S3 class
  `joint_specification.goldfish` — "multivariate specification" stays the concept
  in prose): combines `make_specification()` objects into a multivariate
  specification. At least one **panel-observed layer MUST be referenced** in the
  composed formulas — as a process's focal/dependent layer *or* as an exogenous
  covariate read by another process. Construction checks structural panel presence
  only; a spec whose sole panel reference is an exogenous covariate composes (that
  covariate enters as a static step-covariate, a legitimate DyNAM-with-panel spec),
  and the latent-path requirement is deferred to `estimate_dynes()`, which aborts
  such a spec toward `estimate_dynam()`. A combination referencing no panel-observed
  layer is rejected at construction — those are exactly separable and should be
  estimated with the per-process estimators. **Each joined specification must model
  a distinct focal layer** — a layer is modeled by at most one specification (a
  duplicated dependent layer aborts, naming it), while covariate reuse across specs
  is allowed (it is the coupling); a layer's flavors all live in one specification.
  DyNAM-i processes are excluded; choice_coordination and mixed ordered/timed
  processes are in scope. Processes compose over **one shared mode-map object**
  (`multimode-network-support`, now landed): one- and two-mode processes may be
  joined, and dependent processes over *distinct* mode-pairs compose as long as
  every cross-process read **conforms by mode-set identity** — i.e. the shared
  node space is a *whole* shared mode (advice `{staff}×{director}` + nominations
  `{director}×{project}`). A cross-process read bridging a mode *subset* to a
  union containing it (directors-only ↔ all-employees) does not conform and
  aborts at construction, recorded as future development.
- **Event-stream estimators reject the joint object**: `estimate_dynam()` and
  `estimate_rem()` abort on a `make_joint_specification()` object (pointing to
  `estimate_dynes()`) and on a single specification with a panel-observed focal
  layer (the existing focal-not-panel guard, retargeted to `estimate_dynes()` by the
  `single-data-object` delta); `estimate_dynami()` aborts on a PE-focal spec, with
  its joint-object rejection a recorded future development.
- **fid vocabulary extended, unchanged in kind**: the `flavored-processes` D9
  `process_map` gains rows for every process (a K-flavored rate+choice process
  contributes 2K fids, a plain one 2) and a `coupled` column; integer fid stays
  the canonical identity, labels stay rendered-only.
- **Coupling detection**: a fid is coupled iff its effects or constraint atoms
  directly reference a **modeled** panel-observed layer's state (a panel layer that
  is itself a process, whose path is latent; direct reference only — observed events
  of intermediate layers, and static exogenous panel covariates, are exogenous
  regardless of what those layers' own models reference). The specification print
  marks separable fids; the estimation surface (`estimate_dynes()` — surface in
  `abmcem`, data path in `dynes-augmentation`) MUST inform about separable fids in a
  mixed specification and abort when every fid is separable — equivalently, no panel
  layer is a modeled process — naming `estimate_dynam()` and explaining the panel
  layers would only be static exogenous covariates.
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
  is the substrate `dynes-augmentation`'s model-driven augmenter and the general
  `simulate()` (the `process-simulation` change) consume as external drivers —
  replacing the recipe-loop "per-event simulation hook" `dynes-augmentation`
  originally planned.
- **No estimation surface in this change**: there is no `estimate()` generic
  and no `estimate_multivariate()`; `estimate_dynes()` is the only multivariate
  estimator — its surface + ABEM loop live in `abmcem`, its panel data path in
  `dynes-augmentation`.

## Capabilities

### New Capabilities

- `multivariate-specification`: the `make_joint_specification()` surface —
  composition and validation of process specifications, the referenced-panel-layer
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
  flavor constraints) and precedes `abmcem` (whose `estimate_dynes()` takes a
  `make_joint_specification()` object), `dynes-augmentation` (whose augmenters and
  batched `evaluate_engine()` bind to this change's walk handle), and
  `process-simulation` (whose `simulate()` drives it). These proposals are
  re-grounded against this change's walk handle (2026-07-21).
  `multimode-network-support` (landed) supplies the mode map this change's
  per-mode-pair walk blocks build on (D8); `formula-drives-focal` supplies the
  per-process focal/side/mode resolution a join of several dependent processes
  requires. D9's timed-regime rate completion consumes the standalone
  `intercept-only-rate-spec` primitive (pinned per-period constant rate), which
  must land before §1c's timed branch.
- **Parallel development vs the 2.0.0 release changes** (the `.plan/mv_branch.md`
  branch plan and the 2.0.0 release plan were deleted 2026-07-24 as obsolete once
  every upstream gate landed — see `.plan/goldfish_versions.csv` for the landing
  order): sections 1–2 (surface, coupling, union planning, routing) and section 4
  (walk handle — its evaluation substrate, the `process-state-evaluators`
  capability, is already implemented) were parallel-safe against `residuals-gof`
  (file overlap: only `R/model_estimate.R`, different regions). Section 3 (the
  merged walk) required `spec-driven-dispatch` (rewrites `fold_active_dyad_support`
  in `R/model_preprocess.R` and the writer's dyad encoding) — landed v1.9.10 as
  capability `risk-set-dispatch`, so §3 is now unblocked.
  `multimode-network-support` (which touched `R/make_specification.R` /
  `R/formula_parser.R`) has landed; its mode map is the per-mode-pair block
  substrate D8 builds on. This change needs nothing from
  `residuals-gof`; the `evaluate_engine()` dependency is
  `dynes-augmentation`'s (E-step evaluation), not this change's.
- **R**: `R/make_joint_specification.R` (surface, validation, coupling, print);
  generalization of `R/preprocess_flavored.R` (routing lookup, cross-process
  union planning); the merged walk refactor of `run_sender_recipe_loop()` /
  `run_dyad_recipe_loop()` (the frozen-baseline gate applies: the
  single-process and flavored paths must stay byte-identical); the walk-handle
  API.
- **Frozen baselines**: untouched as the 1e-6 floor; the merged-walk refactor
  is the riskiest step and is gated by baselines PASS at every commit.
- **Docs**: multivariate specification vignette section; walk-handle developer
  documentation for the `dynes-augmentation` consumers.
