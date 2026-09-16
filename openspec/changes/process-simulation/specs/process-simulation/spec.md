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
Simulating from a fitted result SHALL take the data as an argument and rebuild
the specification from the fit's stored formula, model and sub-model: a fit
carries neither its specification nor the stocnet it was fitted on, and SHALL
say so rather than failing inside the walk. The mark of an event
on a flavored layer SHALL carry that flavor's update value, so a simulated
dissolution removes the tie a creation made.
A simulation run SHALL open the walk once per replicate and SHALL NOT
re-preprocess per sub-model or per flavor: every fid is served from the
engine compiled at `walk_open()`, and one injected event updates the shared
state every engine reads.

#### Scenario: one walk serves every fid of a rate-and-choice flavored specification

- **WHEN** a flavored DyNAM specification with K flavors, each carrying a rate
  and a choice, is simulated for one replicate
- **THEN** `walk_open()` runs once, each step evaluates the K rate fids and the
  drawn flavor's choice fid at the same live state, and no per-flavor or
  per-family preprocessing pass is run inside the loop.

#### Scenario: a simulated dissolution removes a tie

- **WHEN** a flavored layer with `values_equivalence = c(creation = 1,
  dissolution = -1)` is simulated free-running
- **THEN** every drawn dissolution carries the update value `-1` and removes an
  existing tie, and no dissolution targets a tie the state does not hold.

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

#### Scenario: a specification of only exogenous covariates simulates

- **WHEN** a DyNAM specification whose rate and choice read only nodal or
  global covariates — so no formula term reads its own focal layer — is
  simulated
- **THEN** the walk carries that focal layer as a shared object anyway, each
  drawn event is applied to it, and the run draws events rather than failing
  to resolve the layer it writes to.

### Requirement: The simulation steps are pluggable and the parameter provider resolves to one known shape

`simulate()` SHALL run one driver loop with four plug points — the parameter
provider (per-replicate `init` and per-step `at`), the clock, the mark kernel,
the evaluation of a fid's values, and the acceptance rule — taken from an
exported `set_simulation_steps()`
constructor whose every slot defaults to the package's own step keyed on the
specification's behavioral descriptor, so that the parametric clocks, the
two-sided mechanisms, the DyNES sequential augmenter and external
latent-variable packages are callers of the same loop. The `coef` argument
SHALL accept a numeric vector, a `goldfishParams`, or a parameter provider
from an exported `set_parameter_provider(init, at)`, and SHALL resolve every
form, at every step, to the parameters the `evaluate` step receives — a
numeric vector of length `p_fid` for the package's own evaluator. The
`evaluate` step SHALL receive the fid's statistics rows in sender-major
order, the parameters as the provider returned them, the live risk set, and
a `meta` list (family token, `n_actors1`, `n_actors2`, the sender for a
choice row), and SHALL return the values over the fid's candidate space —
hazards for a timed family, probabilities for a multinomial one, exact zeros
outside the risk set; the package's default evaluate step SHALL be the
internal process-state evaluator on a vector, and the package SHALL NOT
carry any other model variant in its evaluators. A provider MAY return a next breakpoint time, which
the clock SHALL treat as a competing exit that re-invokes the provider and
injects no event; the provider's realized latent path SHALL be recorded per
event on the result. The walk handle, the process-state evaluators and the
default steps SHALL remain internal; step closures SHALL reach the handle
only through the exported accessor surface.

#### Scenario: a constant provider is the plain path

- **WHEN** `simulate()` runs with a numeric `coef` and with an equivalent
  provider whose `at()` returns that vector at every step, under one seed
- **THEN** the two runs produce identical sequences.

#### Scenario: a supplied evaluate step drives per-ego rates

- **WHEN** `set_simulation_steps(evaluate = )` supplies a step forming a
  row-wise product with an `n_ego × p` matrix the provider returned, with a
  nonzero deviation on one actor's intercept for a rate fid
- **THEN** that actor's simulated rate equals the common rate times
  `exp(deviation)` at every step, the other actors' rates are unchanged, and
  the package's own evaluator was never handed a matrix.

#### Scenario: a regime jump is a breakpoint, not an event

- **WHEN** a continuous-time regime provider reports an exit rate and the
  clock draws the exit before any event
- **THEN** the driver advances to the exit time, calls `at()` again, redraws
  from the new parameters, injects nothing, and the result's latent column
  records the switch.

### Requirement: The `times` axis names the two simulation variants

`simulate()` SHALL accept `times = c("generated", "observed")`, its default given by the exported accessor
`times_of(object)`, read from the specification: `"generated"` when every process carries a timed
rate, `"observed"` when any process is ordinal or a DyNAM process is
choice-only. An explicit value SHALL be validated against what the
specification makes possible and SHALL abort stating why when it cannot be
honored; `"generated"` on a choice-only DyNAM SHALL be honored by completing a
constant exponential rate pinned at the process's crude rate, with a warning.
The result SHALL record whether `times` was derived or requested. Under `times = "generated"` (free-running) the clock and the marks
are both drawn from the model, holding nothing from the observed stream except
the initial state. Under `times = "observed"` (time-anchored) the observed event
times are held and the marks (sender, receiver, flavor, or pair as the family
dictates) are redrawn from the fitted conditional distributions at each observed
stamp. Free-running simulation SHALL be available only where the
specification carries a clock — a timed rate, or the constant exponential rate a
choice-only DyNAM gains on request — and an ordered (Cox) rate or a coordination
process SHALL simulate time-anchored only, `times = "generated"` on it aborting
stating why; anchored
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

#### Scenario: a choice-only specification defaults to time-anchored

- **WHEN** a choice-only DyNAM specification is simulated without a `times`
  argument
- **THEN** `times` resolves to `"observed"`, no rate is fabricated, and the
  result records `times` as derived.

#### Scenario: a choice-only specification generates only when asked

- **WHEN** the same specification is simulated with `times = "generated"`
- **THEN** completion installs a constant exponential rate pinned at the
  process's crude rate, a warning reports it, the rate's regime is `completed`,
  and the result records `times` as requested.

#### Scenario: an override of the specification's times is announced

- **WHEN** a timed specification is simulated with `times = "observed"`
- **THEN** a message states that the specification's default is `"generated"`
  and that the observed stamps are held, and the result records `times` as
  requested.

### Requirement: Free-running clocks are keyed on the distribution axis

Free-running waiting-time draws SHALL be keyed on the specification's
behavioral descriptor, read in two steps: first `timing`, then, for a timed
sub-model, `distribution`. For `timing = "timed"` and `distribution =
"exponential"`, exact competing-exponential draws from the total rate
(intercept included), redrawn at every breakpoint (events, exogenous changes,
window expiries); for `"weibull"` and `"gompertz"` (common shape), exact draws
by analytic inversion of the integrated intensity Σλ_i·[G(t+w) − G(t)] per
constant-rate segment, with the same breakpoint discipline; for `timing = "ordinal"` — the Cox family a user requests as
`distribution = "cox"` — no clock is estimated and none SHALL be invented: no
pseudo-time SHALL be drawn, and such a specification SHALL simulate time-anchored
only. A timed fid SHALL be recognized through
`is_exact_time` on its preprocessed object. The
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

#### Scenario: an ordered specification is time-anchored only

- **WHEN** a Cox/ordered specification is simulated with `times = "generated"`
- **THEN** `simulate()` aborts stating that the ordered rate estimates no clock,
  and without a `times` argument the same specification simulates time-anchored.

### Requirement: Coordination simulates per mechanism, time-anchored

Coordination simulation SHALL be keyed by the specification's `mechanism`
(conjunctive, forcing, confirmation, disjunctive, compensatory) and SHALL be
time-anchored only: at each observed stamp the realized pair SHALL be drawn from
the mechanism's normalized mark multinomial q_kl = φ_kl/Σφ_ab, with no rejection
loop for any mechanism. `times = "generated"` on a coordination specification
SHALL abort stating that coordination timing is estimated under Cox and that its
proposal rate is not identified from the realized events.

#### Scenario: anchored coordination needs no rejection loop

- **WHEN** a coordination specification is simulated under any of the five
  mechanisms
- **THEN** each observed stamp carries a pair drawn from that mechanism's
  normalized mark multinomial, no proposal is rejected, and the observed
  timestamps are unchanged.

#### Scenario: a coordination specification cannot run free

- **WHEN** a coordination specification is simulated with `times = "generated"`
- **THEN** `simulate()` aborts naming the coordination process and stating that
  its proposal rate is not identified, before any walk opens.

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
process. A **choice-only** DyNAM SHALL NOT be rate-completed by default — its default
timing is time-anchored — and SHALL gain a constant exponential rate only when
`times = "generated"` is requested. REM requires only a rate and is already
complete. `walk_open()`
SHALL assert completeness, so the draw loop never opens an incomplete specification.
A completed fid SHALL be evaluated through its built-in evaluate step: a
completed uniform choice as an equiprobable draw over the fid's receiver
support, and a completed pinned rate as the constant per-actor rate
`exp(intercept_w)` of the current period. Because
`simulate()` runs the same completion transform as `estimate_dynes()` and the
augmenters, a simulated pool and an augmented pool SHALL carry identical fid
sets into `evaluate_sequence_pool()`. A flavor named in neither sub-model
list of a relational layer SHALL NOT be completed: it is unmodeled, and
`simulate()` SHALL replay its observed events as scheduled state updates
that right-censor the timed engines, recording the flavor as
`anchored-replay`; on a modeled panel layer such a flavor SHALL abort as it
does for `estimate_dynes()`.

Completion and each process's regime SHALL be read from the specification's
recorded completion (`process_map$completed`), never from a formula's shape: an
authored intercept-only rate is modeled and reads its intercept from `coef`. A
DyNAM process whose authored rate is intercept-only and which carries no choice
SHALL abort at entry, since completion would leave it with no effect to draw
from. A completed default's candidate space SHALL be its process's support
mask, the mask the process's modeled sub-model reads.

#### Scenario: an authored intercept-only rate is modeled, not pinned

- **WHEN** a DyNAM specification with `rate = ~ 1` and a modeled choice is
  simulated
- **THEN** no pinned-rate warning fires, the rate's regime is `modeled`, and
  its intercept is read from `coef`.

#### Scenario: an intercept-only rate with no choice is refused

- **WHEN** a DyNAM specification with `rate = ~ 1` and no choice is simulated
- **THEN** `simulate()` aborts at entry naming the process, before completion
  would add a uniform choice to a model with no effect.

#### Scenario: an unmodeled flavor is replayed, not completed

- **WHEN** a flavored treaties specification models `creation` in rate and
  choice and names `dissolution` in neither, and is simulated free-running
- **THEN** no completion runs, creation events are drawn, each observed
  dissolution is replayed at its observed time as a state update that
  right-censors the creation rate, the regime record shows `dissolution` as
  `anchored-replay`, and a replayed dissolution of a tie the simulation
  never created is skipped and counted.

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

#### Scenario: a flavored gap needs no deferral

- **WHEN** a flavored specification whose `dissolution` flavor lacks the rate
  its `creation` flavor carries is completed and simulated
- **THEN** the walk opens with the completed rate as a member of the rate unit,
  reading its own derived mask, and it evaluates to the pinned constant.

#### Scenario: a whole effect-free family is deferred without renumbering

- **WHEN** a rate-only DyNAM specification is completed with a uniform choice
  and simulated
- **THEN** the walk opens without the choice family, every fid keeps the
  number the completed specification gave it, the completed choice draws over
  its process's support mask, and asking the walk to evaluate it directly
  aborts naming it.

#### Scenario: a completed pinned rate evaluates to its pinned constant

- **WHEN** a flavor whose rate was completed with a pinned intercept-only
  default is simulated free-running
- **THEN** its per-actor rate is the constant `exp(intercept_w)` of the current
  period, evaluated through the built-in `constant` step and reading no
  coefficient, and the fid is marked `completed` in the regime record.

#### Scenario: choice-only DyNAM keeps ordered timing unless asked

- **WHEN** a choice-only (ordered) DyNAM specification is simulated without
  `times = "generated"`
- **THEN** no baseline rate is fabricated; timing comes from the anchored
  strategy, and only the choice marks are drawn from the model.
