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
without family-specific branching. Simulating a flavored specification SHALL draw
the next event across all modeled flavors' total rates with each flavor's derived
support mask maintained. Free-running simulation past the last observed
covariate/composition change SHALL freeze the exogenous state and warn once.

#### Scenario: fixed-count simulation

- **WHEN** a DyNAM specification is simulated for a fixed count of 200 events
- **THEN** exactly 200 dependent events are generated, each drawn from the rates
  and choice probabilities the walk handle evaluates at the state after the
  previous injected event.

#### Scenario: exogenous streams march along

- **WHEN** a covariate change is observed at time t inside the simulated horizon
- **THEN** events drawn after t see the updated covariate state (the walk handle
  applied the exogenous change on `walk_advance()`).

#### Scenario: exogenous horizon is frozen, not invented

- **WHEN** a free-running simulation continues past the last observed
  covariate/composition change
- **THEN** exogenous state stays frozen at its last observed value and a single
  warning reports the freeze point.

### Requirement: The `times` axis names the two simulation variants

`simulate()` SHALL accept `times = c("generated", "observed")`, default
`"generated"`. Under `times = "generated"` (free-running) the clock and the marks
are both drawn from the model, holding nothing from the observed stream except
the initial state. Under `times = "observed"` (time-anchored) the observed event
times are held and the marks (sender, receiver, flavor, or pair as the family
dictates) are redrawn from the fitted conditional distributions at each observed
stamp. Both variants SHALL be available for every simulable family; anchored
runs are bounded by the observed stream and take no stopping arguments.
Documentation SHALL use the vocabulary "free-running" and "time-anchored" and
SHALL NOT describe the axis as conditional/unconditional.

#### Scenario: anchored simulation preserves the observed clock

- **WHEN** a fitted DyNAM model is simulated with `times = "observed"`
- **THEN** the output carries the observed timestamps unchanged with senders and
  receivers redrawn from the fitted rate and choice distributions at each stamp.

#### Scenario: free-running is the default draw

- **WHEN** `simulate()` is called on a fitted exact-time model without a `times`
  argument
- **THEN** the clock is generated from the fitted rates (a new draw from the
  model, matching `stats::simulate` semantics).

### Requirement: Free-running clocks are keyed on the distribution axis

Free-running waiting-time draws SHALL be keyed on the specification's
`distribution`: for `"exponential"`, exact competing-exponential draws from the
total rate (intercept included), redrawn at every breakpoint (events, exogenous
changes, window expiries); for `"weibull"` and `"gompertz"` (common shape),
exact draws by analytic inversion of the integrated intensity
Σλ_i·[G(t+w) − G(t)] per constant-rate segment, with the same breakpoint
discipline; for `"cox"` (and choice-only sub-models), no clock is estimated —
free-running SHALL use pseudo-time from the stored crude rate with output
documenting that such times are meaningful only up to scale, and time-anchored
simulation SHALL be documented as the statistically clean variant. The
exponential draw's exactness rests on every statistic being piecewise-constant
between breakpoints; a specification carrying any effect that violates this
SHALL be rejected with an error naming the effect rather than simulated with a
silently frozen approximation.

#### Scenario: timed model draws exact waiting times

- **WHEN** a DyNAM-rate specification with intercept is simulated to a horizon
- **THEN** inter-event times are exponential draws from the current total rate and
  the event-count distribution matches the model's intensity on seeded fixtures.

#### Scenario: parametric clock recovers its shape

- **WHEN** sequences are simulated from a Weibull specification at known (β, k)
  and the shape is re-estimated from them
- **THEN** the recovered shape concentrates around the generating k on seeded
  fixtures (the DGP for the parametric-rates recovery tests).

#### Scenario: pseudo-time output is labeled

- **WHEN** a Cox/ordered specification is simulated free-running
- **THEN** the output identifies its times as crude-rate pseudo-time meaningful
  only up to scale.

### Requirement: Coordination simulates per mechanism, both variants

Coordination simulation SHALL be keyed by the specification's `mechanism`
(conjunctive, forcing, confirmation, disjunctive, compensatory). Under
`times = "observed"`, the realized pair at each observed stamp SHALL be drawn
from the mechanism's normalized mark multinomial q_kl = φ_kl/Σφ_ab with no
rejection loop for any mechanism. Under `times = "generated"`, each mechanism
SHALL simulate its latent proposal/rejection process by its generative thinning
construction (the mutual-choice rejection loop being the conjunctive instance),
with rejected proposals consuming clock time on the crude-rate pseudo-time
clock; the realized output SHALL report the acceptance rate, and the rejection
loop SHALL carry a max-proposals bound whose abort message names the acceptance
rate reached.

#### Scenario: only reciprocated proposals realize under conjunctive

- **WHEN** a conjunctive coordination specification is simulated free-running
- **THEN** every emitted event corresponds to a proposal chosen by both sides, and
  the result records the proportion of proposals accepted.

#### Scenario: anchored coordination needs no rejection loop

- **WHEN** a coordination specification of any mechanism is simulated with
  `times = "observed"`
- **THEN** each observed stamp's pair is drawn directly from that mechanism's
  normalized pair probabilities, with no latent proposals simulated.

#### Scenario: polarized choices cannot loop forever

- **WHEN** a free-running coordination run's acceptance rate approaches zero
- **THEN** the max-proposals bound aborts the run with a diagnostic naming the
  acceptance rate, rather than looping unbounded.

### Requirement: Stopping targets are separate from the explosion guard

Free-running simulation SHALL stop at a statistical target — `horizon =`
(calendar time) or `n_events =` (event count), whichever binds first when both
are given — and SHALL carry a separate `max_events` safety guard defaulting to a
single documented, overridable value. The guard SHALL trigger early on the rate
trajectory (total rate exceeding a large multiple of its start-of-run value, or
median waiting time collapsing below a resolvable scale), aborting with the
total-rate trajectory in the condition message; the count cap SHALL remain as
the backstop. A replicate that hits the guard SHALL be flagged on the returned
object, excluded from `gof_*` summaries by default, and reported in aggregate —
never silently pooled. Time-anchored runs SHALL take neither targets nor guard
(the observed stream bounds them).

#### Scenario: explosion diagnosed, not just bounded

- **WHEN** a horizon simulation's parameterization drives the total rate to a
  large multiple of its starting value before the horizon
- **THEN** the run aborts early with a diagnostic reporting the total-rate
  trajectory, rather than spending the full event budget in a vanishing sliver
  of simulated time.

#### Scenario: capped replicates are flagged and reported

- **WHEN** 7 of 100 free-running replicates hit `max_events`
- **THEN** those replicates carry a capped flag, the pool result reports the
  count, and default GOF summaries exclude them.

### Requirement: Windowed effects self-schedule expiry during free-running simulation

Free-running simulation of windowed effects SHALL schedule each simulated
event's window (`window =`) expiry at entry time plus the window length in a
per-window FIFO (constant windows expire in insertion
order), and SHALL treat each expiry as a breakpoint: window state updates and
waiting-time draws restart at the expiry. Simulator-scheduled expiries SHALL be
schedule-visible events replayable from the simulated stream. Time-anchored
runs SHALL replay the observed schedule's materialized expiry pseudo-events
unchanged.

#### Scenario: simulated events expire from windows

- **WHEN** a specification with a windowed inertia term is simulated free-running
  past one window length
- **THEN** each simulated event's contribution leaves the windowed network exactly
  one window length after it entered, and the statistics at any point match an
  eager recomputation on the simulated stream.

#### Scenario: expiry is a breakpoint for the clock

- **WHEN** a window expiry falls inside a drawn waiting time
- **THEN** the draw advances to the expiry, updates state, and redraws — the
  realized inter-event distribution reflects the lower post-expiry rate.

### Requirement: Simulations record per-component regimes and guard replay coherence

Every simulation result SHALL record, per component (sub-model or flavor),
which regime produced it: **modeled** (drawn from its fitted distribution),
**completed** (filled by the zero-information completion), or
**anchored-replay** (observed events replayed verbatim as scheduled updates
while modeled components simulate around them). The record SHALL ride the
`process_map` and the print so GOF consumers can mechanically exclude
statistics that touch completed or replayed components. A replayed event whose
precondition fails in the simulated state (e.g. a deletion of a tie no
simulated creation created) SHALL be skipped and counted, never force-applied
as a state clamp; the skip count SHALL be reported on the result, and past a
documented threshold the run SHALL be flagged incoherent.

#### Scenario: replayed deletion with no matching tie is skipped

- **WHEN** a hybrid run replays an observed deletion whose tie the simulated
  creation stream never produced
- **THEN** the deletion is skipped, the skip counter increments, and the state is
  not clamped.

#### Scenario: excessive drift flags the run

- **WHEN** the share of skipped replayed events exceeds the documented threshold
- **THEN** the result is flagged incoherent and the flag is visible in the print
  and to `gof_*` consumers.

### Requirement: Simulated pools are evaluator-compatible

Repeated simulation (`nsim` > 1) SHALL return sequences in the same format the
DyNES augmenters produce and the pool evaluator consumes, so simulated pools feed
`evaluate_sequence_pool()` and the batched likelihood machinery without
conversion; per-replicate capped flags and the per-component regime record SHALL
travel with the pool. Trajectory statistics SHALL be optionally recordable by
attaching a writer sink to the simulation walk.

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
SHALL NOT be rate-completed — its timing uses the ordered strategies
(`times = "observed"` or pseudo-time). REM requires only a rate and is already
complete. `walk_open()`
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
- **THEN** no baseline rate is fabricated; timing comes from the anchored or
  pseudo-time strategy, and only the choice marks are drawn from the model.
