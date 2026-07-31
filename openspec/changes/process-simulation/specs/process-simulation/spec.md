## ADDED Requirements

### Requirement: Simulation drives the walk handle from a specification and parameters

The package SHALL provide a `simulate()` S3 generic that, given a specification
and a parameter vector (a fitted result supplies its θ̂; a specification supplies
an explicit `coef`), generates event sequences by driving the `multi-process-walk`
handle: `walk_open()` the specification and data, then loop
`walk_advance()` / `walk_evaluate()` (drawing the next dependent event from the
fid's masked rate/choice evaluation at the current process state) /
`walk_inject()`, while observed exogenous streams (covariate and composition
changes) merge into the walk through `walk_advance()` unchanged. One
implementation SHALL cover DyNAM, REM, and multivariate/flavored specifications
without family-specific branching. Simulation SHALL stop either at a **time
horizon** or after a **fixed event count**; horizon runs SHALL enforce a
`max_events` cap that aborts with a diagnostic naming the total-rate trajectory
(guarding against process explosion under super-linear feedback terms). Simulating
a flavored specification SHALL draw the next event across all modeled flavors'
total rates with each flavor's derived support mask maintained.

#### Scenario: fixed-count simulation

- **WHEN** a DyNAM specification is simulated for a fixed count of 200 events
- **THEN** exactly 200 dependent events are generated, each drawn from the rates
  and choice probabilities the walk handle evaluates at the state after the
  previous injected event.

#### Scenario: explosion guarded

- **WHEN** a horizon simulation's parameterization drives the total rate to exceed
  `max_events` generated events before the horizon
- **THEN** the run aborts with a diagnostic reporting the event count reached and
  the total-rate growth, rather than looping unbounded.

#### Scenario: exogenous streams march along

- **WHEN** a covariate change is observed at time t inside the simulated horizon
- **THEN** events drawn after t see the updated covariate state (the walk handle
  applied the exogenous change on `walk_advance()`).

### Requirement: Per-family timing strategies

Timed sub-models (DyNAM-rate, REM) SHALL draw exponential waiting times from the
total rate implied by the parameters (intercept included). Ordered/Cox-like
sub-models (rate-ordered, REM-ordered, choice-only), whose baseline rate is not
identified, SHALL support two modes: (a) **fixed template times** — a supplied
event list's timings are kept and only the sender/receiver marks are redrawn; and
(b) **pseudo-time** — waiting times drawn from the crude rate
`log(n_dep_events / total_time / avg_active_actors)` carried by the preprocessed
object, with the output documenting that such times are meaningful only up to
scale.

#### Scenario: timed model draws exact waiting times

- **WHEN** a DyNAM-rate specification with intercept is simulated to a horizon
- **THEN** inter-event times are exponential draws from the current total rate and
  the event-count distribution matches the model's intensity on seeded fixtures.

#### Scenario: ordered model keeps template times

- **WHEN** an REM-ordered specification is simulated in fixed-template mode against
  an observed edgelist
- **THEN** the output carries the observed timestamps unchanged with only
  sender/receiver marks redrawn from the multinomial probabilities.

### Requirement: Coordination simulates by mutual-choice rejection

Choice-coordination simulation SHALL assume a constant rate: a sender is drawn
uniformly at random, a waiting time from the crude rate, and the sender's proposed
alter from its choice probabilities; the event realizes only if the alter's own
choice reciprocates the proposal, otherwise the proposal is rejected and redrawn.
The realized output SHALL report the acceptance rate as a diagnostic.

#### Scenario: only reciprocated proposals realize

- **WHEN** a coordination specification is simulated
- **THEN** every emitted event corresponds to a proposal chosen by both sides, and
  the result records the proportion of proposals accepted.

### Requirement: Simulated pools are evaluator-compatible

Repeated simulation (`nsim` > 1) SHALL return sequences in the same format the
DyNES augmenters produce and the pool evaluator consumes, so simulated pools feed
`evaluate_sequence_pool()` and the batched likelihood machinery without
conversion. Trajectory statistics SHALL be optionally recordable by attaching a
writer sink to the simulation walk.

#### Scenario: simulate feeds the evaluator

- **WHEN** 50 sequences are simulated from a specification at known parameters
- **THEN** the pool evaluates through `evaluate_sequence_pool()` directly, and the
  mean log-likelihood at the generating parameters exceeds that at perturbed
  parameters on seeded fixtures.

### Requirement: Simulation requires a generatively-complete specification

`simulate()` SHALL require a generatively-complete specification — every modeled
DyNAM flavor carrying both a rate and a choice, so an event's who/when (rate) and
whom (choice) can both be drawn — by running the generative-readiness completion
transform (`make-multivariate-spec`) **once** at entry, before the draw loop opens
the walk handle. A flavor keyed in one sub-model and omitted from the other SHALL
be completed with its zero-information default and warned once: a **rate-only
DyNAM** flavor SHALL gain a **uniform choice** (receivers drawn equiprobably, no
extra `coef`); a missing **timed** rate SHALL gain an intercept-only baseline hazard
whose per-actor intercept SHALL be **pinned** (zero free parameters, never read from
`coef`) — from observed counts as `log(count_w / (T_w · |R_w|))` for a mixed
process, or from the requested event count / time window for a fully rate-less
process. A **choice-only** (ordered) DyNAM
SHALL NOT be rate-completed — its timing uses the ordered strategies (fixed-template
or pseudo-time). REM requires only a rate and is already complete. `walk_open()`
SHALL assert completeness, so the draw loop never opens an incomplete specification.
Because `simulate()` runs the same completion transform as `estimate_dynes()` and
the augmenters, a simulated pool and an augmented pool SHALL carry identical fid
sets into `evaluate_sequence_pool()`.

#### Scenario: rate-only DyNAM simulates with a uniform choice

- **WHEN** a rate-only DyNAM specification (no choice) is simulated
- **THEN** the completion transform adds a uniform choice, a single warning reports
  it, and each event's receiver is drawn equiprobably from the risk set — no
  additional coefficient is required.

#### Scenario: incomplete spec that reaches the walk is rejected

- **WHEN** a code path reaches `walk_open()` with a half-specified DyNAM
  specification that skipped the completion transform
- **THEN** `walk_open()` aborts naming the incomplete flavor, rather than
  simulating events with a missing rate or choice.

#### Scenario: choice-only DyNAM keeps ordered timing

- **WHEN** a choice-only (ordered) DyNAM specification is simulated
- **THEN** no baseline rate is fabricated; timing comes from the fixed-template or
  pseudo-time strategy, and only the choice marks are drawn from the model.
