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

Explore session 2026-08-19 (task 1.1): the `.plan/sp/simulation.md` survey
(Amati et al. published + 2021 draft, remulate, relevent, redeem, dream,
amorem, Boschi–Wit) and the four active neighbors (`parametric-rates`,
`two-sided-coordination`, `recency-effects`, `window-profiling`) resolved the
open questions below as design amendments: the `times =` variant axis and
per-family clock menu (D6, D2 revised; ADR-0033), the target/guard split with
the trajectory trigger and capped-replicate flagging plus the regime record
and replay coherence guard (D3 revised, D9; ADR-0034), mechanism-keyed
coordination (D7), and FIFO self-scheduled window expiry (D8).

## Goals / Non-Goals

**Goals:**
- One `simulate()` generic covering every estimable family by driving the walk
  handle, with both variants of the `times =` axis (free-running /
  time-anchored) first-class on every family.
- Distribution-keyed timing strategies (exponential exact / Weibull–Gompertz
  inversion / Cox pseudo-time-or-anchored / per-mechanism coordination
  thinning), stopping targets, and a separate trajectory-triggered explosion
  guard.
- Evaluator-compatible pool output that closes the simulate → augment → evaluate
  loop, carrying the per-component regime record GOF consumers filter on.

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
and on a specification carrying an explicit `coef`. The `coef` shape is
**two-method by spec kind**: for a single `goldfishSpec` (or a fitted
single-process result's θ̂) it is a plain **numeric** vector, unchanged from the
single-process convention; for a `goldfishJointSpec` it is a
**`goldfishParams`** object built by `set_init_param(spec, ...)`
(`joint-parameters`) — the same shared surface `estimate_dynes(initial_parameters=)`
consumes — on which `simulate()` **asserts completeness** (aborts on any free,
non-offset `NA`, naming the effects), the parameter-level parallel to
`walk_open()` asserting generative completeness (D5). The object is required only
on the joint path: a one-fid spec is unambiguous, so the numeric `coef` stays and
no existing single-process signature changes. The body opens the
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

### D2 — Timing strategies keyed on the distribution axis (revised 2026-08-19)

Free-running clocks are keyed on `parametric-rates`' `distribution` axis
(that change deletes `rate_ordered` into `distribution = "cox"`; the family
labels below follow it):

- **Exponential** (DyNAM-rate, REM, default): waiting time ~ Exp(total rate,
  θ including the intercept), redrawn at every breakpoint — exact simulation.
  **The exactness condition is an explicit assumption**: the competing-
  exponential draw is exact iff every statistic (hence every rate) is
  piecewise-constant between draw points. All current effects satisfy it —
  including the `recency-effects` family, which is event-index memory with no
  time-domain decay — and breakpoints comprise events, exogenous
  covariate/composition changes, and self-scheduled window expiries (D8). Any
  future continuously-decaying effect breaks the condition and must choose
  analytic inversion, Ogata thinning, or a documented piecewise-constant
  approximation per effect — never silently the latter.
- **Weibull / Gompertz** (common shape, `parametric-rates`): closure under
  minima holds, so the free-running draw is **analytic inversion** of the
  integrated intensity Σλ_i·[G(t+w) − G(t)] per constant-rate segment
  (Bender et al. 2005), with the same breakpoint-redraw discipline. Single-
  process `simulate()` only: composition keeps rejecting parametric clocks
  (`parametric-rates` D13, unchanged). These clocks are also the DGP for
  that change's shape-recovery tests (its task 3.4).
- **Cox / ordered sub-models** (`distribution = "cox"`, choice-only): no
  estimated clock. Time-anchored (`times = "observed"`, D6) is the
  statistically clean variant; free-running uses **pseudo-time from the
  crude rate** `log(n_dep_events / total_time / avg_active_actors)` carried
  per flavor by every preprocessed object, with output documenting that such
  times are meaningful only up to scale.
- **Coordination**: per-mechanism, D7.

*Rejected:* nonparametric (Breslow-style) baseline recovery to draw calendar times
from ordinal fits — no reference package does it, it adds a timing estimand the
ordinal model deliberately ignores, and the supported modes cover the GoF and
augmentation uses; if timing matters, fit the exact-time (parametric-baseline)
model instead (the Amati et al. Discussion's own direction). (ADR-0033.)

### D3 — Stopping targets are not the explosion guard (revised 2026-08-19)

Free-running stopping is a **statistical target**: `horizon =` (calendar
time — the natural basis for timing statistics) or `n_events =` (event
budget — the natural basis for order/mark statistics and the cheap default),
remulate-style whichever-binds-first when both are given. The **safety
guard** is a separate `max_events`, defaulting to one readable, overridable
number (`10 * n_dep` — a compute bound, no hidden size-switch), so a user
legitimately simulating far past the observed length raises the target
without fighting the guard. The guard trips early on the rate *trajectory*
(total rate exceeding a large multiple of its start-of-run value, or median
waiting time collapsing below a resolvable scale), aborting with the
total-rate trajectory in the condition message — a diagnosis, not a body
count; the count cap remains the backstop for slow drift. A **capped
replicate is not a model draw**: it is flagged on the returned object,
excluded from `gof_*` summaries by default, and reported in aggregate
("7 of 100 replicates hit max_events") — never silently pooled.
Time-anchored runs need none of this: N is fixed by the data. Reference
points: remulate has no runtime guard; redeem's flat `max_events = 4e5` is
count-only with no diagnostic — this design goes past both. (ADR-0034.)

### D4 — Flavored/multivariate draws and evaluator-compatible output

A flavored/multivariate specification simulates as competing processes: the next
event is drawn across all modeled flavors' total rates with each flavor's derived
support mask maintained — exactly the competing-process routing the walk handle
performs. Repeated simulation (`nsim > 1`) returns sequences in the augmenter/pool
format, so simulated pools feed the DyNES pool evaluator without conversion;
trajectory statistics are optionally recordable via a writer sink on the walk.

### D5 — Simulation requires a generatively-complete spec (see `make-multivariate-spec` D9)
Drawing a DyNAM event needs a rate (who acts, when) **and** a choice (whom), so
`simulate()` requires a generatively-complete specification. This closes the gap in
the base spec (which never said what `simulate()` does with a rate-only DyNAM): the
completion transform owned by `make-multivariate-spec` **D9** runs **once** at
`simulate()`'s entry and fills any half-specified flavor's missing sub-model with
its zero-information default — a **rate-only DyNAM** gains a **uniform choice** (zero
parameters, so no extra `coef` is needed; receivers are drawn equiprobably); a
missing **timed rate** gains an intercept-only baseline hazard whose per-actor
intercept is **pinned** (`make-multivariate-spec` D9, via the
`intercept-only-rate-spec` primitive), **not** a free parameter and **never** read
from `coef`: a mixed process (one flavor rate-modeled, another missing) pins the
missing flavor's constant from observed counts as
`intercept_w = log(count_w / (T_w · |R_w|))`, while a **fully** rate-less process
draws its event budget from the requested event count / time window (D2). A
**choice-only DyNAM** is *not* rate-completed — its timing
uses this change's ordered modes (D2, pseudo-time / fixed-template). REM is already
complete (rate only). Completion warns once and marks the added fids in the
`process_map` and print. `walk_open()` **asserts** completeness rather than
performing it, so the `simulate()` driver loop (D1) never opens an incomplete spec.
The completion transform is the same one `estimate_dynes()` and the augmenters run,
so a simulated pool and an augmented pool carry identical fid sets into
`evaluate_sequence_pool()`.

### D6 — The `times =` axis: free-running and time-anchored, first-class on every family (added 2026-08-19)

One argument on `simulate()`, `times = c("generated", "observed")`, default
`"generated"` (matches `stats::simulate` intuition: a new draw from the
model). Prose vocabulary: **free-running** (`"generated"` — draw the clock
and the marks; nothing held from the observed stream except initial state
and, optionally, a seed prefix) and **time-anchored** (`"observed"` — hold
the observed event times, redraw the marks from the fitted conditional
distributions). What was D2's "fixed-template-times" ordinal fallback *is*
`times = "observed"`, promoted to a first-class variant on every family —
for Cox-family fits it is the only honest variant, and everywhere else it is
the cheap, conditionally-exact GOF workhorse (mark calibration, degree and
p-shift distributions, Amati waiting-time deciles), while free-running alone
tests the clock (inter-event distributions, intercept calibration,
prediction, power studies). The two variants are the simulation mirror of
the margins' which/when split. Anchoring extends per-flavor via the regime
record (D9): anchoring a flavor's clock while redrawing its marks is
per-flavor `times = "observed"`. Exogenous horizon: free-running past the
last observed covariate/composition change **freezes state and warns** —
the only option that does not invent data. *Rejected namings:*
RSiena's "conditional/unconditional" (names run-length determination, not
the clock, and collides with the conditional-residuals vocabulary);
"resampled" (suggests bootstrap resampling of observed marks); "fixed/random
times" (reads as a frailty statement). (ADR-0033.)

### D7 — Coordination simulates per mechanism: all five, both variants (added 2026-08-19)

Keyed by `two-sided-coordination`'s `mechanism =`. **Time-anchored**: at
each observed stamp the realized pair is drawn from the mechanism's
normalized mark multinomial q_kl = φ_kl/Σφ_ab — no rejection loop for any
mechanism, since the latent proposal process cancels out of the mark kernel
(the same fact that makes Cox estimation work). **Free-running**: each
mechanism simulates its own latent proposal/rejection process via the
derivation note's §7 generative thinning construction — the previously
spec'd mutual-choice rejection loop is exactly the conjunctive instance —
with rejected proposals consuming clock time and the acceptance rate
reported per mechanism. Coordination timing stays Cox-only (ADR-0023), so
the free-running clock is crude-rate pseudo-time (or the pinned opportunity
clock ρ̃_k + ρ̃_l in a timed composition, `two-sided-coordination` D16); its
value is the mechanism's ordering dynamics and acceptance diagnostics, not
calibrated timing. This implements, for the simulation engine, the
conjunctive-only gate lift that `two-sided-coordination` D15 priced as cheap;
its task 2.5 simulation fixtures seed the tests. The rejection loop carries
a max-proposals bound with the acceptance rate in the abort message
(the same target/guard discipline as D3). *Rejected:* conjunctive-only with
aborts on the other four (leaves the new mechanisms without any GOF
counterpart for no structural saving — the anchored path needs only φ_kl,
which every mechanism already defines).

### D8 — Windowed effects free-run via FIFO self-scheduled expiry (added 2026-08-19)

The eager window architecture materializes expiry pseudo-events from the
*observed* stream at preprocessing; a free-running simulated event has no
such materialized expiry. `simulate()` therefore self-schedules: an event
entering a windowed network at t enqueues its expiry at t + ω in a
per-window **FIFO** (a constant window means insertion-order expiry — no
priority queue, `window-profiling` D7's structure), and each expiry is a
breakpoint in the piecewise-exponential draw (state updates, rates redraw).
This keeps windowed statistics piecewise-constant, so the D2 exactness
condition holds with windows in the model. The simulator-scheduled expiries
are schedule-visible walk events like any other — replayable from the
simulated stream, satisfying `process-state-evaluators` — and the future
`last_k =` is the count-driven analogue (ring-buffer eviction, no clock).
Time-anchored runs replay the observed schedule, expiries included, and need
none of this. *Rejected:* aborting on windowed specs in v1 (an avoidable
hole once the FIFO is named); freezing window state (silently simulates a
different model). (ADR-0033.)

### D9 — Per-component regime record and the replay coherence guard (added 2026-08-19)

Every simulation records, per component (sub-model or flavor), which of
three regimes produced it: **modeled** (drawn from its fitted
distribution), **completed** (the D5 zero-information default filled it), or
**anchored-replay** (its observed events replayed verbatim as scheduled
updates while modeled components simulate around them — e.g. deletions
replayed while creations free-run, or per-flavor clock anchoring with marks
redrawn). The record rides the `process_map` and the print, so `gof_*` can
mechanically exclude statistics touching completed (model-noise by
construction) or replayed (data, not model) components rather than relying
on user discipline. The coherence hazard of regime mixing: a replayed event
can reference state the simulated stream never produced (deleting a tie no
simulated creation created) — under a misfitting model the *typical* case.
Semantics are defined, not discovered: such an event is **skipped and
counted**, the count is reported on the object, and past a documented
threshold the run is flagged incoherent. The layer-isolated `gof-dynes`
design (anchor all times, constrain each stamp's redraw to the owning
layer) avoids the hazard entirely and is the safe GOF default; free-running
replay hybrids are the advanced, explicitly-flagged mode. *Rejected:*
force-applying a replayed event as a state clamp — silently corrupts
endogenous statistics. (ADR-0034.)

## Risks / Trade-offs

- **Divergence from the batch walk** → `simulate()` is a driver over the *same*
  handle the batch preprocessing replay driver uses; batch-vs-replay equality
  tests (owned by `make-multivariate-spec`) protect the shared substrate.
- **Explosion under feedback terms** → the trajectory trigger diagnoses early,
  the `max_events` cap backstops, and capped replicates are flagged and
  excluded from GOF pools by default (D3).
- **Ordinal timing is only up-to-scale** → the pseudo-time output documents the
  caveat; `times = "observed"` is the first-class variant for users who have
  observed timings (D6).
- **Scope width (two parametric clocks, five thinning constructions, FIFO
  expiry)** → each is a keyed branch on a shared driver, not a new driver;
  the anchored path needs only the mark kernels estimation already computes,
  and the inversion clocks are one formula per distribution. Task ordering
  puts the exponential/anchored core first so the extensions land on a
  working loop.
- **Simulator-scheduled expiries make the walk schedule dynamic** → expiries
  are ordinary schedule-visible events replayable from the simulated stream
  (D8); the batch-vs-replay equality tests extend to a windowed fixture.
- **Replay hybrids drift from the simulated state** → skip-and-count with a
  reported threshold (D9); the layer-isolated anchored mode stays the safe
  GOF default.

## Migration Plan

Purely additive surface. `dynes-augmentation` drops its D12 and its
`process-simulation` spec delta (they move here) and retains `augment_seq_sim()`
as the conditioned consumer of this change's drawing core. Rollback is reverting
the change's commits; no baseline impact (the frozen coefficient baselines are
untouched — simulation adds no estimation path).

### Cross-change coordination (active, not-yet-applied changes this touches)

- `parametric-rates` (0/22): D2's Weibull/Gompertz clocks are its recovery-test
  DGP (its task 3.4 note); the timing-strategy vocabulary here is written
  against its post-D3 tokens (`distribution = "cox"`, `rate_ordered` gone) —
  if this change implements first, the D2 wording maps back trivially
  (rate_ordered ⟺ cox). Its D13 composition rejection of parametric clocks is
  deliberately untouched.
- `two-sided-coordination` (0/26): D7 lifts, for the simulation engine, the
  conjunctive-only generation gate its D15 sets on the walk engines —
  its D15 wording should note the lift when either change is next edited; its
  task 2.5 fixtures seed the per-mechanism DGP tests; the §7 thinning
  constructions live in its derivation note.
- `recency-effects` (4/30): its no-time-decay stance is what keeps the D2
  exactness condition true with recency terms in the model; its future
  `last_k =` is D8's count-driven analogue (ring-buffer eviction).
- `window-profiling` (2/20): D8's FIFO is its D7 structure; its bootstrap leg
  (its D5) is gated on this change's `simulate()` landing.
- `gof-dynes`: should adopt the `times = "observed"` vocabulary for its
  layer-isolated simulation; the capped-replicate exclusion (D3) and regime
  filtering (D9) enter its pool-building requirement; the 2021 draft's
  interrupted waiting-time auxiliaries are candidates for its statistic set.

## Open Questions

- ~~Entry points and generic naming~~ Resolved 2026-08-19: both S3 methods
  (fitted result → θ̂; specification → explicit `coef`), base
  `stats::simulate` generic (D1 stands as written).
- ~~Ordered-family default mode~~ Resolved 2026-08-19: subsumed by the
  `times =` axis (D6) — default `"generated"` (pseudo-time, labeled);
  `"observed"` is the template mode, requiring an event list only in the
  spec-dispatch case (a fitted result carries its own observed events).
- ~~Exogenous horizon~~ Resolved 2026-08-19: freeze state and warn (D6) —
  the only option that does not invent data.
- ~~`max_events` default; abort vs truncate; rejection bookkeeping~~
  Resolved 2026-08-19: `10 * n_dep` single documented number, trajectory
  trigger aborts with diagnosis, capped replicates flagged/excluded (D3);
  rejected coordination proposals consume clock time, max-proposals bound
  with reported acceptance rate (D7).
- **[surface]** Which writer sinks are legal on a simulation run; the output
  class name (per `class-naming-scheme` — ADR-0031's `goldfish<Thing>`
  camelCase rule, superseding the earlier `_goldfish` suffix) — settle at
  implementation.
- **[surface]** The incoherence threshold default (share of replayed events
  skipped, D9) and the trajectory-trigger multiple (D3) — settle against
  fixtures during implementation (ADR-0034 open questions).
