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
  DyNAM-i joins once `dynami-stocnet-boundary` settles its shape.
- **Per-family timing strategies**: timed sub-models (DyNAM-rate, REM) draw
  exponential waiting times from the total rate (intercept included);
  ordered/Cox-like sub-models (rate-ordered, REM-ordered, choice-only) — whose
  baseline is unidentified — support (a) fixed-template-times-redraw-marks and
  (b) crude-rate pseudo-time (the intercept scalar already carried per flavor);
  choice_coordination simulates by mutual-choice rejection (uniform sender,
  crude-rate waiting time, accept iff reciprocated), reporting the acceptance
  rate.
- **Stopping rules and explosion guard**: stop at a time horizon OR a fixed event
  count; the fixed count doubles as the guard against process explosion under
  super-linear feedback, and horizon runs carry a hard `max_events` cap that
  aborts with a total-rate diagnostic.
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

## Impact

- **New R surface** (experimental, cli-reported): `simulate()` S3 methods for a
  fitted result and a specification-plus-`coef`, driving the `multi-process-walk`
  handle; NAMESPACE / roxygen regenerated per task.
- **Consumes** `make-multivariate-spec`'s `multi-process-walk` handle
  (`walk_open`/`advance`/`evaluate`/`inject`) and the already-landed
  `process-state-evaluators` rate/probability kernels; no new C++ expected (the
  handle's evaluators supply rates/choices at a state).
- **Sequencing**: after `make-multivariate-spec` (owns the walk handle); before
  (and consumed by) `dynes-augmentation` — `augment_seq_sim()` shares this
  change's per-step drawing core — and `gof-dynes`, whose simulation-based
  diagnostics drive the same surface.
- **`dynes-augmentation` trimmed**: its D12 and the `process-simulation` spec
  delta move here; its section-5 tasks become a consumes-pointer, with
  `augment_seq_sim()` retained as the conditioned consumer.
- **Docs**: a `simulate()` reference and a simulation section in the model-usage
  vignette; the family-specific timing modes documented per sub-model.
