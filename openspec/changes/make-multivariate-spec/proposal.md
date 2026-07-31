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

For *fully observed* processes the factorized likelihood (the `flavored-processes`
D2 note, which applies unchanged across layers) makes separate per-process
*estimation* exact — so no multivariate *estimator* is needed there. But
estimation-separability is not generative independence: a forward draw interleaves
the processes on one shared clock and shared state, so a process whose effects read
another's layer is coupled *generatively* even when its likelihood factorizes. The
multivariate surface therefore serves two consumers with different viability
conditions — `estimate_dynes()`, which needs a *modeled* (latent-path) panel layer
to be worth more than separate estimation, and `simulate()`, which needs only ≥2
processes over shared state. This change builds that surface and the walk substrate
both consume; the augmentation and estimation themselves are `dynes-augmentation`'s
and `abmcem`'s, the general `simulate()` is `process-simulation`'s.

## What Changes

- **`make_joint_specification(...)`** (the constructor, renamed from the working
  `make_multivariate_spec()`; returns a distinct S3 class
  `joint_specification.goldfish` — "multivariate specification" stays the concept
  in prose): combines `make_specification()` objects into a multivariate
  specification. **Construction composes any join of ≥2 processes over one shared
  mode-map object and does NOT require a panel-observed layer** — all viability is
  consumer-owned. A spec whose sole panel reference is an exogenous covariate composes
  (that covariate enters as a static step-covariate, a legitimate DyNAM-with-panel
  spec); a combination referencing **no** panel-observed layer *also* composes — it is
  estimation-separable (the factorized likelihood) yet generatively coupled through the
  shared clock, so it is a valid `simulate()` input with no other constructor (flavored
  `make_specification()` is single-layer). Each consumer enforces its own viability:
  `estimate_dynes()` aborts on an all-separable spec toward `estimate_dynam()` (no
  latent path to couple), the event-stream estimators reject the joint object by class,
  and `simulate()` accepts any composition. Construction MAY note separability but does
  not abort on it. **Each joined specification must model
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
  contributes 2K fids, a plain one 2) and a `coupled` column (plus a `completed`
  column for fids auto-supplied by generative completion, below); integer fid stays
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
  layers would only be static exogenous covariates. A fid whose **own** focal layer
  is a modeled panel process is never separable even if its formula reads nothing, so
  `separable := NOT coupled AND layer NOT a modeled panel process` (D4).
- **Generative-readiness completion** (D9/D9a): a specification that will *generate*
  events (drive the walk handle via `simulate()` or an augmenter) or be estimated
  *jointly* must be **generatively complete** — every modeled DyNAM flavor carrying
  both a rate and a choice. A single completion transform, run **once at each
  consumer's entry** (never inside `walk_open`, which only *asserts* completeness),
  fills each half-specified flavor with a **zero-free-parameter** default: a uniform
  choice over the support-legal alternatives, a uniform `choice_coordination`, a
  uniform `rate_ordered` in the ordered regime, or — in the timed regime — the pinned
  **intercept-only rate** (`intercept-only-rate-spec` primitive; per-actor
  `intercept_w = log(count_w / (T_w · |R_w|))`, θ-independent → excluded from
  score/Hessian), with `(count_w, T_w, |R_w|)` supplied by the consumer (D9a). It
  warns per fill at each consumer entry, aborts when a **modeled panel** layer omits a
  flavor entirely (RE-subset modeling stays legal), and marks completed fids in the
  `process_map`. To let a half-specified spec reach these consumers,
  `make_specification()` **relaxes** its same-flavor-set abort (recording the gap, not
  fabricating a default); the single-process estimators re-impose it at estimation time
  on the excluded path. A **mixed ordered+timed composition** is rejected at join time
  in `make_joint_specification()` (a mix is only visible across ≥2 processes).
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
  composition and validation of process specifications, consumer-owned viability (no
  panel layer required at construction, D2), the extended fid/process_map vocabulary,
  coupling detection and separability marking, **generative-readiness completion**
  (D9/D9a — the half-spec completion transform, the `completed` column, the
  `make_specification()` same-flavor-set relaxation, and the join-time ordered/timed
  regime guard), and the multivariate specification print. This surface has
  two consumers: `estimate_dynes()` (joint estimation, `abmcem`/`dynes-augmentation`)
  and `simulate()` (generative draw, `process-simulation`), which takes a
  `make_joint_specification()` object + `coef` directly.
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
  `process-simulation` (whose `simulate()` consumes this surface directly — a
  `joint_specification.goldfish` + `coef` drawn forward — as well as driving the walk
  handle; an estimation-separable no-panel join is still a valid simulation input, so
  the D2 relaxation to consumer-owned viability is what admits it). These proposals are
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
- **R**: `R/make_joint_specification.R` (surface, validation, coupling, print,
  join-time regime guard); the generative-completion transform (D9, e.g.
  `complete_generative_spec()`) plus its shared `(count_w, T_w, |R_w|)` helper
  (D9a, wrapping `materialize_network_state()` / `active_dyad_count()` /
  `time_weighted_risk_set()`); the `make_specification()` same-flavor-set relaxation
  in `R/make_specification.R`; generalization of `R/preprocess_flavored.R` (routing
  lookup, cross-process union planning); the merged walk refactor of
  `run_sender_recipe_loop()` /
  `run_dyad_recipe_loop()` (the frozen-baseline gate applies: the
  single-process and flavored paths must stay byte-identical); the walk-handle
  API.
- **Frozen baselines**: untouched as the 1e-6 floor; the merged-walk refactor
  is the riskiest step and is gated by baselines PASS at every commit.
- **Docs**: multivariate specification vignette section; walk-handle developer
  documentation for the `dynes-augmentation` consumers.
