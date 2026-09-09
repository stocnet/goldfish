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

Re-grounded 2026-09-07 (explore session, on `develop` at 1.9.31) against
three changes archived that day — `class-naming-scheme` (every class is
`goldfish<Thing>`, ADR-0031), `model-spec-descriptor` (one `behavior`
descriptor computed at spec construction; preprocessing dispatches once on it,
`right_censored`/`intercept_scalars` retired into `is_exact_time`/`timing`;
one `goldfishStat` output class) and `fit-class-hierarchy` (`goldfishBaseFit`
parent plus the generic-by-class verdict table) — and against the walk
substrate as it now stands in the tree: `make-multivariate-spec` archived
2026-08-05 (living specs `multi-process-walk` and
`multivariate-specification`), `R/walk_handle.R` stamping `goldfishWalk`,
`R/preprocess_joint.R` running the merged walk, `complete_generative_spec()`
in place. The amendments are marked *Re-grounded 2026-09-07* under D1, D2, D4,
D5 and D8, and D10 is new: it records how simulation relates to the three
preprocessing substrates the package now carries (one pass over all fids, never
a cycle per sub-model). The generative consumers the artifacts name as siblings
— `estimate_dynes()`, `augment_seq_sim()`, `evaluate_sequence_pool()` — do not
exist in the tree yet; they are owned by `abmcem`, `dynes-augmentation` and
`gof-dynes`, all unapplied, so this change implements first and those adapt.

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
**`goldfishParams`** object built by `set_parameters(spec, ...)`
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

*Re-grounded 2026-09-07 — fit classes and the verdict table.* The fitted
result is `goldfishFit` (single process) or `goldfishFlavFit` (the
competing-flavor container), both under the parent `goldfishBaseFit`; the
joint DyNES fit has no class yet (`estimate_dynes()` is unbuilt). The
`fit-class-hierarchy` living spec requires that a change adding a generic
dispatching on fit classes record a verdict per concrete class **before
implementing**, so this change carries a `fit-class-hierarchy` spec delta with
the `simulate` row: `goldfishFit` → `override` (it reads the fit's stored
specification, data and θ̂, which the parent surface does not carry);
`goldfishFlavFit` → `override`, **and not the container's usual fan-out** —
the flavors compete on one clock, so the container simulates as one process
family under D4, which is the one generic where a container answers with a
single run rather than a per-component list. The spec-dispatch method
(`simulate(goldfishSpec)` / `simulate(goldfishJointSpec)`) is outside the
fit table. Note the limit this exposes: the verdict table records *which*
method exists per class, not *what shape* it returns, so the one-run shape is
pinned by the delta's scenario rather than by any verdict cell — the same
boundary ADR-0051 (proposed) meets from the printing side, where the contract
governs dispatch and not presentation.

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

*Re-grounded 2026-09-07 — the descriptor carries two facts, not one.* The
landed `model-spec-descriptor` gives every spec a `behavior` descriptor with
`timing ∈ {timed, ordinal}` and a separate, reserved
`distribution ∈ {exponential; weibull, gompertz}`; `parametric-rates` D3a
(re-grounded 2026-09-06) keeps `distribution = "cox"` as the **user-facing
spelling** and has the constructor translate it to `timing = "ordinal"`,
leaving the descriptor's `distribution` field untouched. The clock selection
above therefore reads the descriptor in two steps, not one field: `timing ==
"ordinal"` (Cox / choice-only) ⇒ no clock, the pseudo-time or anchored
branch; `timing == "timed"` ⇒ `distribution` picks exponential inversion or
the Weibull/Gompertz inversion. The prose keeps "Cox" for the family because
that is what a user types. The crude-rate scalars the pseudo-time branch
reuses are the three `goldfishStat` fields `n_dep_events`, `total_time` and
`avg_active_entity` (the intercept MLE `log(n_dep_events / total_time /
avg_active_entity)` inverts them, and `intercept-only-rate-spec` already
reads them per period); "intercept scalars" was the retired name. A timed
fid is recognized by `is_exact_time` on the preprocessed object, never by
`right_censored`, which no longer exists.

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

*Re-grounded 2026-09-07 — what the handle maintains today.* `walk_open()`
currently **refuses** user support constraints and node-composition changes
(`goldfish_walk_unsupported`), and `walk_build_state()` evaluates every fid
over the trivial all-active risk set (`active_sender` / `active_dyad` all
`TRUE`), because under those refusals the live statistics equal the batch
materialization exactly. So "each flavor's derived support mask maintained"
is not something the handle performs yet: the flavored batch path folds the
derived masks into each fid's `goldfishStat`, but the stepping handle has no
live mask. Lifting that — the derived flavor masks (`support_mask_maintain.R`
already maintains them incrementally on the batch side), user constraints
compiled per `constraint_id`, and composition changes advancing through
`walk_advance()` — is substrate work this change owns as task 2.0, with the
batch-vs-replay equality test extended to each lift.

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

*Re-grounded 2026-09-07 — the handle refuses the completed defaults.*
`complete_generative_spec(consumer = c("estimate_dynes", "simulate"))` exists
and marks completed fids (`completed` column via
`completed_column_from_bundles()`), and `walk_open()` asserts completeness as
this decision says. But `walk_open()` then runs `assert_walkable_submodels()`,
which **aborts on any effect-free sub-model** — precisely the `~ 1` uniform
choice and the pinned intercept-only rate that completion installs — with the
message that "auto-supplied uniform / pinned defaults are walked by the
generative consumers". So the driver, not the handle, evaluates a completed
fid: a completed uniform choice is an equiprobable draw over the fid's
receiver support, and a completed pinned rate is the constant per-actor rate
`exp(intercept_w)` for the current period. Task 2.1 owns those two
evaluations in the driver, and the handle keeps walking effect-bearing
sub-models only; the alternative — teaching `build_walk_engine()` to compile
a zero-column engine — is not worth a `spec_map` for a constant.

*Confirmed 2026-09-09 (ADR-0056, closing the ADR-0053 → 0055 → 0056
chain).* This decision stands as written, and the shape vocabulary it rests
on is now explicit. A flavor named in **both** lists is modeled. A flavor
named in **one** list is a completion gap: estimation estimates the family it
names and treats the flavor as unmodeled in the other (the per-flavor form
of the plain rate-only formula; the current abort in
`estimate_from_specification()` goes in a `flavored-processes` follow-up),
while every generative consumer completes it at entry with a one-time
warning and marks the fid `completed`; no `complete =` argument exists
because the gap is the request. A flavor named in **neither** list is
unmodeled: estimation censors it, and `simulate()` replays it (D9). The
Fisheries treaties shape — creation modeled in rate and choice, dissolution
in neither — therefore simulates with no completion at all; a modeled panel
layer with an unmodeled flavor stays an abort
(`abort_on_unmodeled_panel_flavor()`), since the augmenter must place every
change between waves.

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

*Re-grounded 2026-09-07 — the substrate gap is larger than a breakpoint
hook.* The merged walk itself aborts on any window effect today
(`build_walk_engine()`: "The merged walk does not yet support window
effects"), and the handle's schedule is **static**: `walk_open()` precomputes
the exogenous rows (`exo_rows`) and `walk_advance()` only moves a cursor over
them, so there is no insertion point for a simulator-scheduled expiry. Task
2.7 therefore has two substrate prerequisites before the FIFO: window effects
in the merged walk (the eager expiry pseudo-events materialized per engine,
which the single-process recipe loops already do), and a schedule that admits
driver-inserted breakpoints (an insertion API on the handle, or a merge of the
FIFO heads with the exogenous cursor inside `walk_advance()`). Both belong to
task 2.0.

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

*Amended 2026-09-09 (ADR-0055).* `anchored-replay` is not only the advanced
hybrid: it is the **default regime of every unmodeled flavor** (named in
neither sub-model list) of a relational layer, in both `times` variants. The
driver schedules that flavor's observed events as state-update rows it never
draws, they right-censor the timed engines exactly as in estimation, the
regime record names the flavor `anchored-replay`, and the skip-and-count guard
applies. Under `times = "observed"` the replayed events mostly land on ties
the redrawn stream also holds; under free-running the skip share grows with
the horizon, which is what the incoherence threshold reports. The "advanced,
explicitly-flagged" wording above now describes the per-flavor *override* of
that default (replaying a modeled flavor's own events, per-flavor clock
anchoring), not the unmodeled case. Unmodeled flavors on a modeled panel
layer are not replayed; they abort (D5).

### D10 — One preprocessing pass keyed by fid, never a cycle per sub-model (added 2026-09-07)

The package carries three preprocessing substrates, and the question this
decision settles is which one `simulate()` rides and why:

| Entry | What one call preprocesses | Walks per call |
| --- | --- | --- |
| `estimate_dynam()` / `estimate_rem()` on a `goldfishSpec` | the one family `sub_model` selects (`estimate_from_specification()` picks the bundle) | 1 recipe loop; rate + choice = 2 calls = 2 walks |
| `estimate_dynam()` on a flavored `goldfishSpec` (K flavors, F families) | `preprocess_flavored()`: one union walk **per family**, K consumers routed by fid inside it | F (2 for rate + choice), never K × F |
| `preprocess_joint()` / `walk_open()` on a `goldfishJointSpec` (a single or flavored `goldfishSpec` is wrapped by `single_process_joint()`) | the merged single-clock walk: one engine per `(focal, family)` unit over one shared state and one schedule, every fid served from its engine's block | **1** |

`simulate()` drives the third. One `walk_open()` per replicate; at each
step the driver evaluates every modeled rate fid at the live state (the
competing-process draw across flavors and layers), then the chosen fid's
choice partner from its dyad engine, and injects the event once — every
engine sees it through the shared state on the next evaluation. There is no
per-sub-model or per-flavor re-preprocessing anywhere in the loop: the fids
are the routing key (`process_map`), the engines are compiled once at open,
and the per-event cost is the sum over engines of one statistics update,
exactly what the batch merged walk pays. The rate and choice halves of one
flavor are separate engines (a sender block and a dyad block) because their
statistics have different shapes, not because they are walked separately.

Two consequences follow. First, a flavored `goldfishSpec` simulates through
the merged walk although it *estimates* through `preprocess_flavored()`'s
F-pass path; the `multi-process-walk` frozen-baseline gate ("single-process
and flavored specifications preprocess byte-identically to their pre-merge
outputs") is what makes the two paths interchangeable on statistics, and the
batch-vs-replay equality tests are what make the handle trustworthy against
either. Second, the F-pass flavored batch path is now a duplicate of the
merged walk's flavored case — a candidate for the post-landing duplication
inspection ADR-0045 schedules, **not** for this change: routing estimation
through the merged walk touches the 1e-6 baselines and is a separate
decision.

*Rejected:* preprocessing each sub-model's statistics in its own cycle and
stitching the results per event (the "K × sub-models" shape). It recomputes
the shared state K × F times per event, cannot host cross-process
right-censoring or effect deduplication, and is exactly the batch-loop
callback design D1 already rejected under a different name.

### D11 — The driver has four plug points, and `coef` is a parameter provider (added 2026-09-09)

Every variant that must simulate on this surface — the parametric clocks
(`parametric-rates`), the five two-sided mechanisms
(`two-sided-coordination`), the DyNES sequential augmenter
(`dynes-augmentation`), and goldfish.latent's actor random effects, discrete-
and continuous-time hidden Markov regimes, and their combination — is a
choice at one of four points of the D1 loop, and none of them touches the
walk (`.plan/sp/sim_variants.md` carries the per-variant detail):

```
per replicate:  P0  params$init(r, handle)          replicate-level draws (u_i, z_0, a posterior draw)
per step:       P1  params$at(k, t, handle, latent) parameters per fid for THIS step
                    walk_evaluate(handle, fid, θ)
                P2  clock(rates, t, handle)         exponential | Weibull/Gompertz | pseudo-time | anchored
                P3  mark(fid, evaluation, handle)   sender→receiver | REM pair | mechanism kernel | completed defaults
                P4  accept(event, handle)           always | DyNES endpoint conditioning
                    walk_inject(handle, event)
```

The driver takes a `goldfishSimSteps` object from an exported constructor,
`set_simulation_steps(parameters, clock, mark, accept)`, in the `set_*`
family beside `set_parameters()`; every slot defaults to goldfish's own step
keyed on the behavioral descriptor, so a plain `simulate(fit)` passes no
steps and the augmenter, the parametric DGP and the mechanism thinning are
callers of one loop rather than forks of it. `coef` is a **provider**: a
numeric vector (constant, single process), a `goldfishParams` (constant,
joint, D1 unchanged), or a `goldfishParamProvider` from
`set_parameter_provider(init, at)`. Whatever the input form, `at()` resolves
each step to the **one known shape** `walk_evaluate()` accepts per fid: a
numeric vector of length `p_fid`, or an `n_ego × p_fid` matrix whose row is
the ego's parameter vector (the per-ego case is the one substrate extension
the latent variants need on `evaluate_process_state()`: a row-wise product
in place of `stat_mat %*% theta`, byte-identical in the vector case; task
2.0d). `at()` may also return a next `breakpoint` time, which the clock
treats as a competing exponential exit (a continuous-time regime jump
switches parameters and injects nothing), keeping D2's piecewise-constant
exactness intact; and the provider's realized latent path is written to the
result as a per-event column.

goldfish exports the two constructors, a small accessor surface for the
opaque `handle` argument the closures receive (`n_actors()`,
`process_map()`, `current_time()`, `regime_of()`), and nothing else: the
`walk_*` handle, `evaluate_process_state()` and the default steps stay
internal, so goldfish.latent registers `simulate()` methods on its own fit
classes, builds a provider from a posterior draw, and never learns what the
walk is. Posterior predictive checking is then a consumer loop of one
`simulate(spec, coef = provider_from_draw(d))` per draw, which is the
"simulation under parameter uncertainty" non-goal answered without the
driver knowing it. *Rejected:* exporting the walk handle (freezes an internal
substrate as an API); a per-variant `simulate_*()` function each (four copies
of the loop, the duplication ADR-0045 was written about); making the latent
variants goldfish's own (the regimes and deviations are goldfish.latent's
models and its posterior draws, goldfish owns the loop).

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
  deliberately untouched. *2026-09-07:* its D3a now re-grounds `cox` as a
  user spelling translated to the descriptor's `timing = "ordinal"`, which is
  the two-step read D2's re-grounding note adopts; until it lands, the
  descriptor's `distribution` is always `"exponential"` and the
  Weibull/Gompertz branch of task 2.4 has no spec to key on — implement 2.4
  behind the field and let its tests wait for that change.
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
- **[surface]** Which writer sinks are legal on a simulation run — settle at
  implementation.
- **[surface]** The output class name. `class-naming-scheme` is archived, so
  the rule is fixed: `goldfish<Thing>`, no dot, and the package-wide guard
  test enumerates classes from the namespace, so a new class needs no table
  row to conform. Proposed: `goldfishSim` for one run's result (event
  sequence + regime record + capped flag + acceptance diagnostics); the
  `nsim > 1` pool takes the class of the augmenter/pool format it must match,
  owned by `dynes-augmentation`, so this change mints only `goldfishSim` and
  wraps replicates in a plain list until that format lands. Confirm at task
  2.1.
- **[surface]** The incoherence threshold default (share of replayed events
  skipped, D9) and the trajectory-trigger multiple (D3) — settle against
  fixtures during implementation (ADR-0034 open questions).
- **[D11]** The exact accessor surface the step closures get on the opaque
  handle, and whether a provider's `at()` for a coordination fid returns one
  matrix per side or a single two-sided object — settle with
  goldfish.latent's first provider and `two-sided-coordination`'s random
  effects, whichever comes first.
- **[D11 / parametric-rates]** The per-actor time origin of the Weibull
  hazard across a regime switch or a window breakpoint; per-segment
  inversion handles it only if the origin is stored per actor on the handle.
