## Context

Created 2026-07-21 by lifting the process-simulation primitive out of
`dynes-augmentation` (its D12 and its `process-simulation` spec delta). The
motivation for the move: `make-multivariate-spec` landed the `multi-process-walk`
handle (`walk_open`/`walk_advance`/`walk_evaluate`/`walk_inject`) — a
family-agnostic stepping + injection substrate — which turns simulation into a
short external driver rather than a DyNES-owned recipe-loop callback. The design
below is grounded in the reference packages: relevent's `simulate.rem.dyad()`
(fixed event count, optional keep-timings-redraw-dyads) and remulate
(exponential waiting times to a horizon). goldfish adopts their union and adds
the ordinal pseudo-time mode and the explosion guard neither provides.

Sequencing: after `make-multivariate-spec`; before `dynes-augmentation` and
`gof-dynes`, which consume `simulate()` and its per-step drawing core.

## Goals / Non-Goals

**Goals:**
- One `simulate()` generic covering every estimable family by driving the walk
  handle.
- Per-family timing strategies (exact / fixed-template / pseudo-time /
  coordination rejection), stopping rules, and a hard explosion guard.
- Evaluator-compatible pool output that closes the simulate → augment → evaluate
  loop.

**Non-Goals:**
- Endpoint-conditioned draws (`augment_seq_sim()` in `dynes-augmentation`):
  conditioning on wave snapshots, risk-set restriction, and proposal-density
  bookkeeping are the augmenter's, not plain simulation's.
- Simulation under parameter uncertainty (θ drawn from `vcov()` for GoF bands) —
  a recorded `gof-dynes` extension.
- Incremental re-preprocessing; a nonparametric baseline recovery for ordinal
  fits (see D2 rejection).

## Decisions

### D1 — `simulate()` is a general S3 generic driving the walk handle

`simulate()` is the base-generic S3 (`simulate(object, nsim = 1, seed = NULL,
...)`), with methods dispatching on a fitted model result (its θ̂ used as `coef`)
and on a specification carrying an explicit `coef`. The body opens the
`multi-process-walk` handle on the spec + data and loops
advance → evaluate → draw → inject; observed exogenous streams (covariate and
composition changes) merge into the walk unchanged via `walk_advance()`. Nothing
about the loop is family-specific — the handle evaluates whatever fids the
composed spec carries — so DyNAM, REM, and multivariate/flavored specifications
share one implementation. The augmenter-side per-step drawing core (draw the next
mark from a masked rate/choice vector) is factored so `augment_seq_sim()` reuses
it under its endpoint conditioning. *Rejected:* a DyNES-private simulate on a
recipe-loop callback (the walk handle is the general substrate; a callback bolted
into the batch loop invites divergence between the simulate path and the batch
path — the risk `make-multivariate-spec` D6 already names).

### D2 — Per-family timing strategies

- **Timed sub-models** (DyNAM-rate, REM): exponential waiting times from the total
  rate (θ including the intercept) — exact simulation.
- **Ordered/Cox-like sub-models** (rate-ordered, REM-ordered, choice-only): the
  baseline is unidentified, so two modes — (a) **fixed template times, redraw
  marks** (relevent's precedent) and (b) **pseudo-time from the crude rate**
  `log(n_dep_events / total_time / avg_active_actors)` already carried per flavor
  by every preprocessed object; pseudo-time output documents that times are
  meaningful only up to scale.
- **Choice-coordination**: mutual-choice rejection — constant rate, sender drawn
  uniformly, waiting time from the crude rate, an alter proposed from the sender's
  choice probabilities, the event realized iff the alter's choice reciprocates;
  the acceptance rate is a reported diagnostic.

*Rejected:* nonparametric (Breslow-style) baseline recovery to draw calendar times
from ordinal fits — no reference package does it, it adds a timing estimand the
ordinal model deliberately ignores, and the two supported modes cover the GoF and
augmentation uses.

### D3 — Stopping rules and the explosion guard

Simulation stops at a **time horizon** OR a **fixed event count**. The fixed count
is also the guard against process explosion (super-linear feedback driving the
total rate unbounded); horizon runs carry a hard `max_events` cap that aborts with
a diagnostic naming the total-rate trajectory rather than looping unbounded.

### D4 — Flavored/multivariate draws and evaluator-compatible output

A flavored/multivariate specification simulates as competing processes: the next
event is drawn across all modeled flavors' total rates with each flavor's derived
support mask maintained — exactly the competing-process routing the walk handle
performs. Repeated simulation (`nsim > 1`) returns sequences in the augmenter/pool
format, so simulated pools feed the DyNES pool evaluator without conversion;
trajectory statistics are optionally recordable via a writer sink on the walk.

## Risks / Trade-offs

- **Divergence from the batch walk** → `simulate()` is a driver over the *same*
  handle the batch preprocessing replay driver uses; batch-vs-replay equality
  tests (owned by `make-multivariate-spec`) protect the shared substrate.
- **Explosion under feedback terms** → the `max_events` guard is mandatory on
  horizon runs; the fixed-count mode is the safe default for feedback-heavy specs.
- **Ordinal timing is only up-to-scale** → the pseudo-time output documents the
  caveat; fixed-template mode is offered for users who have observed timings.

## Migration Plan

Purely additive surface. `dynes-augmentation` drops its D12 and its
`process-simulation` spec delta (they move here) and retains `augment_seq_sim()`
as the conditioned consumer of this change's drawing core. Rollback is reverting
the change's commits; no baseline impact (the frozen coefficient baselines are
untouched — simulation adds no estimation path).

## Open Questions

- **[surface]** Entry points: `simulate()` S3 on the fitted result, on a
  specification with explicit `coef`, or both (leaning both); base-generic vs a
  snake_case verb (leaning the base generic for `stats::simulate` compatibility).
- **[surface]** Ordered-family default mode (pseudo-time vs fixed-template) and
  whether a template event list is required or optional.
- **[surface]** Which writer sinks are legal on a simulation run; the output class
  name.
- **[surface]** Exogenous horizon: simulating past the last observed
  covariate/composition change (freeze state and warn?).
- **[surface]** `max_events` default; abort vs truncate-with-warning; coordination
  rejection bookkeeping (redraw sender+time or keep the drawn time; acceptance-rate
  warning threshold).
