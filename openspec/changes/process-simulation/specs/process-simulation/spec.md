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
