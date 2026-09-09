## Why

Generating event sequences from a specification and parameters is a primitive
several surfaces need — DyNES sequence augmentation (`augment_seq_sim()`), the
goodness-of-fit diagnostics (`gof-dynes`), and users who want to simulate from a
fitted model — yet none of them owns it, and it is not DyNES-specific. Every
model family goldfish estimates (DyNAM rate/choice/choice_coordination, REM, and
later DyNAM-i) is a competing-process model over a shared clock and process
state, so one generative loop over the `multi-process-walk` handle
(`make-multivariate-spec`) simulates all of them. `dynes-augmentation` originally
carried this as its D12 and specced a `process-simulation` capability, but the
primitive is general and the walk handle is now the substrate that makes a
family-agnostic simulator fall out cleanly. This change lifts `simulate()` out
of `dynes-augmentation` into its own home directly on the walk handle.

## What Changes

- **`simulate()` as a general S3 generic** — mirroring `stats::simulate(object,
  nsim, seed, ...)` — dispatching on a fitted model result (using its θ̂) or on a
  specification with an explicit `coef`. Its body is a **driver over the walk
  handle**: `walk_open()` the spec+data, then loop
  `walk_advance()` / `walk_evaluate()` (draw the next event) / `walk_inject()`.
  Because the handle evaluates whatever fids the composed spec carries, one
  implementation covers DyNAM, REM, and multivariate/flavored specifications;
  DyNAM-i joins once `refactor-dynami-engine` lands its recompute-to-delta
  effect adapter (its D1; the `dynami-stocnet-boundary` change this bullet
  used to name archived on 2026-07-23).
- **Four plug points and a parameter provider** (added 2026-09-09, design
  D11): the loop takes a `goldfishSimSteps` from an exported
  `set_simulation_steps(parameters, clock, mark, accept)`, every slot
  defaulting to goldfish's descriptor-keyed step, and `coef` is a provider —
  a numeric vector, a `goldfishParams`, or `set_parameter_provider(init, at)`
  — resolving each step to a per-fid vector or per-ego matrix. Parametric
  clocks, the two-sided mechanisms, the DyNES augmenter and goldfish.latent's
  random effects and hidden Markov regimes are callers of one loop; the walk
  handle stays internal. Detail per variant in `.plan/sp/sim_variants.md`.
- **The `times =` axis** (revised 2026-08-19, ADR-0033): every family gets both
  simulation variants — **free-running** (`times = "generated"`, default: draw
  clock and marks) and **time-anchored** (`times = "observed"`: hold the
  observed times, redraw the marks) — the anchored variant being the honest
  mode for Cox-family fits and the cheap GOF workhorse everywhere.
- **Distribution-keyed timing strategies**: exponential draws exact
  competing-exponential waiting times (intercept included); Weibull/Gompertz
  (common shape) draw exactly by analytic inversion per constant-rate segment
  — also the DGP for `parametric-rates`' recovery tests; Cox/choice-only has
  no estimated clock — anchored is the clean variant, free-running uses
  crude-rate pseudo-time (labeled up-to-scale); coordination simulates
  per mechanism (all five): anchored from the mechanism's mark multinomial (no
  rejection loop), free-running via each mechanism's generative thinning
  construction (the mutual-choice rejection loop is the conjunctive instance),
  acceptance rate reported.
- **Windowed effects free-run correctly**: simulated events self-schedule their
  window expiries in a per-window FIFO (constant window ⇒ insertion-order
  expiry) treated as breakpoints, keeping the exponential draw exact with
  `window =` terms in the model.
- **Stopping targets vs the explosion guard** (revised 2026-08-19, ADR-0034):
  `horizon =` / `n_events =` are statistical targets (whichever binds first); a
  separate `max_events` guard (default `10 * n_dep`) trips early on the
  total-rate trajectory with a diagnosis, capped replicates are flagged and
  excluded from GOF pools by default.
- **Per-component regime record**: every result records modeled / completed /
  anchored-replay per component; replayed events whose precondition fails in
  the simulated state are skipped and counted, with an incoherence flag past a
  documented threshold — never force-applied as state clamps.
- **Flavored/multivariate simulation**: the next event is drawn across all
  modeled flavors' total rates with each flavor's derived support mask
  maintained — the competing-process draw the walk handle already routes.
- **Evaluator-compatible output**: repeated simulation (`nsim > 1`) returns
  sequences in the same format the augmenters produce and the pool evaluator
  consumes, closing the simulate → augment → evaluate loop; trajectory statistics
  are optionally recordable through a writer sink on the walk.

Out of scope: the endpoint-conditioned draw `augment_seq_sim()` (it stays in
`dynes-augmentation` — it adds wave-endpoint conditioning, risk-set restriction,
and proposal-density bookkeeping that plain simulation has no reason to carry);
simulation under parameter uncertainty (θ ~ N(θ̂, vcov) bands — a recorded GoF
extension); the ABEM loop and augmenters (`abmcem` / `dynes-augmentation`).

## Capabilities

### New Capabilities

- `process-simulation`: the `simulate()` surface — the general S3 generic driving
  the walk handle, per-family timing strategies, stopping rules and the explosion
  guard, the coordination rejection scheme, flavored competing-process draws, and
  the evaluator-compatible pool output.

### Modified Capabilities

- `fit-class-hierarchy`: the generic-by-class verdict table gains the
  `simulate` row (`override` on `goldfishFit` and `goldfishFlavFit`; the
  flavored override is one competing run, the recorded exception to the
  container fan-out shape).

## Impact

- **New R surface** (experimental, cli-reported): `simulate()` S3 methods for a
  fitted result and a specification-plus-`coef`, driving the `multi-process-walk`
  handle; the exported constructors `set_simulation_steps()` and
  `set_parameter_provider()` (classes `goldfishSimSteps`,
  `goldfishParamProvider`) plus a small accessor surface on the opaque handle
  for step closures; NAMESPACE / roxygen regenerated per task.
- **External consumer**: goldfish.latent registers `simulate()` methods on
  its own fit classes and supplies a provider built from a posterior draw;
  it depends on the two constructors and the accessors only.
- **Consumes** the landed `multi-process-walk` handle (`walk_open`/`advance`/
  `evaluate`/`inject`, class `goldfishWalk`, archived from
  `make-multivariate-spec` 2026-08-05) and the already-landed
  `process-state-evaluators` rate/probability kernels; no new C++ expected (the
  handle's evaluators supply rates/choices at a state). One `walk_open()` per
  replicate serves every fid: simulation never re-preprocesses per sub-model or
  per flavor (design D10). The handle today refuses window effects, user
  support constraints, node-composition changes and effect-free sub-models;
  the first three are lifted here as substrate task 2.0, the fourth is
  evaluated by the driver (design D4/D5/D8 re-grounding notes, 2026-09-07).
- **Extends the fit verdict table** (`fit-class-hierarchy` delta): `simulate`
  is `override` on `goldfishFit` and on `goldfishFlavFit`, the latter as one
  competing run rather than the container's fan-out. New result class
  `goldfishSim` (per `class-naming`).
- **Sequencing**: after `make-multivariate-spec` (owns the walk handle); before
  (and consumed by) `dynes-augmentation` — `augment_seq_sim()` shares this
  change's per-step drawing core — and `gof-dynes`, whose simulation-based
  diagnostics drive the same surface. Cross-consumers: `parametric-rates`
  (recovery-test DGP), `two-sided-coordination` (per-mechanism DGPs; lifts its
  D15 conjunctive-only simulation gate), `window-profiling` (its bootstrap leg
  is gated on `simulate()`).
- **`dynes-augmentation` trimmed**: its D12 and the `process-simulation` spec
  delta move here; its section-5 tasks become a consumes-pointer, with
  `augment_seq_sim()` retained as the conditioned consumer.
- **Docs**: a `simulate()` reference and a simulation section in the model-usage
  vignette; the family-specific timing modes documented per sub-model.
