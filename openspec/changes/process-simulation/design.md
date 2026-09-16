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

*Corrected 2026-09-15 at task 2.1 (ADR-0075) — a fit carries neither its
specification nor its data.* The paragraph below read that `goldfishFit`
overrides `simulate` because "it reads the fit's stored specification, data and
θ̂". It stores none of the first two: a fit carries `parameters`, `formula`
(two-sided, the response naming the focal layer), `model`, `sub_model`,
`model_spec`, `names` and `call`, and nothing that reaches a stocnet. So
`simulate.goldfishFit()` takes **`data =`** and rebuilds the specification from
the stored formula; the estimates stay the default `coef`. *Rejected:*
attaching the specification — and with it the whole stocnet — to every fitted
object at estimation time so that `simulate(fit)` works argument-free; it grows
every fit for one consumer and moves an estimation-path object for a
simulation-path convenience. The verdict `override` is unchanged; only its
reason is, and the sentence below is corrected in place.

*Re-grounded 2026-09-07 — fit classes and the verdict table.* The fitted
result is `goldfishFit` (single process) or `goldfishFlavFit` (the
competing-flavor container), both under the parent `goldfishBaseFit`; the
joint DyNES fit has no class yet (`estimate_dynes()` is unbuilt). The
`fit-class-hierarchy` living spec requires that a change adding a generic
dispatching on fit classes record a verdict per concrete class **before
implementing**, so this change carries a `fit-class-hierarchy` spec delta with
the `simulate` row: `goldfishFit` → `override` (it rebuilds a specification
from the fit's formula, model and sub-model, which the parent surface does not
carry, and takes the data as an argument);
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

*Amended 2026-09-16 (explore, goldfish-f0) — the rebuild keeps the fit's
right-hand side verbatim.* `specification_from_fit()` rebuilt the formula
through `stats::terms()` + `stats::reformulate()`, which never writes an
explicit `1`. goldfish reads an intercept only from a written `1`, so every
fitted timed rate came back intercept-less, and `walk_open()`'s compile
re-added the intercept with the message *"a time intercept has been added …
Use `sub_model = "rate_ordered"`"* — on every `simulate(fit)` of a rate model,
never on the specification path (verified on an REM fit of `calls ~ 1 +
indeg`). The coefficients stayed aligned only by luck: estimation adds the
same intercept, and it also orders main effects before interactions, as
`reformulate()` does (checked on `~ 1 + indeg:ego(floor) + outdeg`: both give
`Intercept, odeg, ideg:ego`). The stored formula alone is not enough either:
a fit whose intercept estimation *added* stores `calls ~ indeg` with
`model_spec$has_intercept = TRUE`. So the rebuild takes the stored right-hand
side verbatim (drop the response, keep every term as written) and prepends an
explicit `1` when the descriptor says the fit carried an intercept the formula
does not write. The code comment claiming the legacy `fit$sub_model` reports
an REM rate as `"choice"` is stale — no fit this version produces carries it (a
deprecated `sub_model = "choice"` is rewritten to `"rate"`, and pre-2.0.0 fits
are refused by `abort_if_stale_result()`) — so it is rewritten to the reason
that does hold: the family belongs to the descriptor. Task 2.2h.

*Amended 2026-09-16 (explore, goldfish-f0) — simulated events are collected
into pre-allocated columns.* The 2.1 collector appends a one-row
`data.frame` per event to a list and binds them once at the end. That is not
quadratic (a list append is amortized constant), but building a data frame
costs about 130 us per event, roughly 10% of a 439-event DyNAM run (66 ms of
0.65 s), paid on every replicate of a GOF pool. The run's size is bounded
before its first draw: a run never accepts more events than it proposes, and
`max_events` bounds the proposals, so the capacity is `n_events` when that
is the target and smaller than the guard, otherwise `max_events`. The
collector allocates one typed vector per column at that capacity once (and a
list of the same length for the provider's latent state), writes event `k`
by index, and at the end cuts each column to the `k` events drawn and sets
the class and compact row names on the list — no per-event frame, no bind,
no `data.frame()` call. The cut is one vector copy per column when the run
stops short of capacity; nothing cheaper exists in R. Allocation at capacity
is the rule unless the columns would exceed a documented byte budget, in which
case the collector starts at the observed dependent count and doubles. The
output keeps its columns and types. Task 2.2j.

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
  crude rate** `log(n_dep_events / total_time / avg_active_entity)` carried
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


*Amended 2026-09-15 (explore, ADR-0076) — the crude rate is also a
completion.* A choice-only DyNAM has no clock, and `resolve_default_sub_model()`
declines to complete its rate in the ordered regime, which is right for the
default. But the three scalars pseudo-time reads — `n_dep_events`,
`total_time`, `avg_active_entity` — are exactly the pin of a constant
exponential rate, `log(n_dep_events / (total_time · avg_active_entity))`. So
"pseudo-time, meaningful up to scale" and "a completed constant exponential
rate at the crude MLE" are one object under two labels, and the second is the
honest one: it is warned, recorded `completed`, and draws from a stated hazard.
A choice-only DyNAM therefore defaults to `times = "observed"` (D6 amendment)
and, when the user asks for `"generated"`, completion installs that pinned rate.
The value comes from preprocessing — and from 2.11's exposure integral once it
replaces the event-averaged `avg_active_entity`. What an ordinal process does instead is the next amendment.

*Superseded in part 2026-09-16 (ADR-0079) — no pseudo-time.* The Cox/ordered
bullet above is withdrawn. The Cox partial likelihood leaves the baseline hazard
nonparametric and unestimated, so a constant crude-rate clock imposes a shape the
model never assumed while the marks come from estimated effects: output that looks
timed and is not. D2's own rejection of a Breslow recovery applies with more force
to a constant one. A Cox/ordered specification therefore simulates time-anchored
only, and `times = "generated"` on it aborts stating that the ordered rate
estimates no clock. A choice-only DyNAM is the one process without an estimated
rate that runs free, and only on request: an intercept-only exponential rate paired
with a choice model is a complete Poisson process specification — RSiena's default
constant rate function — not a reinterpreted ordinal fit (ADR-0076).
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

*Amended 2026-09-16 (explore, goldfish-f0; ADR-0083) — with no target, a
free-running run stops at the observed horizon.* D3 called `n_events` "the
cheap default", and 2.2d implemented it as the observed dependent count, so a
run given no target always returned exactly that many events (439 on
`social_evolution`, for an REM fit and for a choice-only model completed at
`times = "generated"` alike). That hides the one thing a free-running run
exists to test (D6): how many events the clock produces. The default target
for `times = "generated"` is therefore `horizon =` the end of the observation
window, taken from `resolve_walk_extent()` with the run's own `control_prep`,
so it is the same end the fit's likelihood integrated exposure to. The count
is then random, and at the fitted estimates of a timed rate with an intercept
its expectation over the observed trajectory equals the observed count (the
intercept's score equation); a completed crude rate satisfies the same
identity by construction. An explicit `n_events` keeps its meaning, alone or
whichever-binds-first with an explicit `horizon`. Time-anchored runs take no
target, as above.

*The guard is no longer a formality.* Under a count target a runaway run
ended at the observed count and looked healthy. Under the horizon default it
meets the guard, and it does so on real fits: an REM fit of `~ 1 + indeg` on
`social_evolution` (`ideg` 1.15) hit `max_events` (4390) on five of five
seeds, accepting 439 events in the first 7% of the window and then 3951 more
at one instant, all to actor 42 — the total rate outgrew the resolution of a
clock near 1.2e9, so `t + wait == t`. The `max_events` cap stays (`10 * n_dep`,
counting proposals, `capped` flagged). The rate-trajectory early trigger that
2.2 left unlanded and noted as "carried into 2.8", which 2.8's text never
picked up, lands with the default instead, together with its sharpest case: a
drawn wait that does not advance the clock (`t + wait == t`) aborts
immediately with the total-rate trajectory, since every later event would
share one timestamp. Task 2.2i.

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

*Corrected 2026-09-15 — the lifts landed.* Task 2.0a/2.0b (`8641656`,
`a21b20c`) put derived flavor masks, user constraints and presence cursors
on the handle, so the paragraph above describes the state before 2026-09-13.
The one residual gap — a constraint on a changing object no formula term
reads — is owned by `constraint-objects-on-shared-walk`. Nothing else must
land before a flavored specification with node-composition change
simulates; `per-family-flavor-modeling` disclaims any effect on
`simulate()`, and a flavor carrying one family is completed at entry (D5).

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

*Implemented 2026-09-15 at task 2.1.* Deferring had to become something
`walk_open()` does, not something the driver arranges around it. The refusal
fires before any build, so a completed specification could not open a walk at
all; and stripping the effect-free sub-models beforehand made the spec fail the
*completeness* assert instead — a DyNAM process missing a family. `walk_open()`
therefore takes `completed = c("abort", "defer")`. `"abort"` stays the default
and every other caller's boundary. `"defer"` builds the merged blocks from the
effect-bearing sub-models only and returns the rest on the handle as
`deferred`. Dropping sub-models rebuilds the process_map, which **renumbers the
walk's fids**, so the driver keeps the completed specification's map as
authoritative and matches the two numberings on the rendered process label —
the identity `reconcile_joint_parameters()` already matches on across
completion, and the reason that label has exactly one renderer.

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


*Amended 2026-09-15 (explore, goldfish-f8) — four corrections to the deferral
2.1 implemented, each checked against the tree.*

1. **No `completed =` argument, and in most cases nothing to defer.** No caller
   should abort instead of complete: `walk_open()` has one production caller,
   and every generative consumer completes before opening. Two cases, checked
   on the flavored fixture with the refusal patched out:
   - *Flavored gap* (a flavor lacks one family while other flavors of that
     family carry effects): the completed fid is an ordinary member of its
     family's compiled unit, with an empty effect map and its own derived mask.
     The pinned dissolution rate evaluated to exactly `exp(intercept_w)` over
     its active senders; the uniform dissolution choice, with zero statistic
     columns and `θ = numeric(0)`, gave probability 1 to sender 1's only
     existing tie and 0 to self. Nothing is deferred; the refusal was the only
     obstacle.
   - *Whole family effect-free* (a single-process rate-only DyNAM, or every
     flavor of a family completed): no unit can compile, since the merged
     compile rejects a `~ 1` choice at parse. The walk strips that family,
     keeps the completed process_map so every fid keeps its number (a unit's
     fids are looked up by layer and family, not counted), and skips map rows
     no unit owns — today keeping the map fails in `build_effect_union()` on
     the missing formula. The deferred fid stays on the handle's map, and
     asking the walk to evaluate it aborts naming it as a completed default.
2. **Regime reads `process_map$completed`, never formula shape.**
   `is_intercept_only_rate_bundle()` tests shape only, so `mark_pinned_rates()`
   marks an *authored* `rate = ~ 1` as pinned (verified). With a modeled choice
   that rate is warned "pinned, not estimated", and the 2.1 driver looks it up
   in `completed_rates`, finds nothing, and evaluates it at zero, so a
   single-process run stops `no_rate` before its first event. An authored
   intercept-only rate is modeled and reads its intercept from `coef`.
3. **An authored `rate = ~ 1` with no choice aborts at entry.** Completion would
   add a uniform choice and leave a DyNAM process with no effect anywhere; today
   it reaches the walk and fails with an internal error (verified).
4. **A completed default draws over its process's support mask, not presence.**
   A process's rate and choice share one `constraint_id` and one live mask: on
   the flavored fixture every fid's support grid is identical, and the rate's
   `active_sender` already equals `rowSums(mask) > 0 & presence` through
   `sender_gate_from_mask()` — the rate state's `active_dyad = rep(TRUE, n1)`
   is an unread placeholder on a sender block, not its gate. In a flavored gap
   the completed fid reads its own mask. In the whole-family case it reads its
   same-process sibling's live mask through the shared `constraint_id`, which
   survives deferral (verified): senders by the rowSums gate, receivers by the
   mask row and presence, self dropped on a one-mode layer. A sibling always
   exists there, because completion fills one family per process and an
   authored `rate = ~ 1` with no choice aborts (amendment 3). Either way there
   is one evaluation path: the process-state evaluator on a zero-column (or
   intercept-only) state is the uniform (or constant) draw over that set.

A partially modeled flavored DyNAM layer follows ADR-0056 as implemented: a
flavor named in one list completes the missing family with the timing of the
existing model (a pinned rate when the process is timed, nothing when it is
ordered), and a flavor named in neither list completes nothing (verified). The
second half is only correct once D9's replay runs; see D9's amendment.
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


*Amended 2026-09-15 (explore, ADR-0076) — `times` is derived, not defaulted.*
The specification already says which variants it can honor, so `times`
defaults to an expression that resolves from the specification: `"generated"`
when every process carries a timed rate, `"observed"` when any process is
ordinal or a DyNAM process is choice-only. An explicit value is validated
against that and aborts stating why when it cannot be honored — with the one
deliberate exception that `"generated"` on a choice-only DyNAM is honored by
completing a constant exponential rate (D2 amendment), with the completion
warning. Completion therefore receives the requested variant, since whether to
install that rate is the request. The result records whether `times` was
derived or requested. In the relational family the one-regime-per-join rule
costs nothing here: a DyNAM or REM specification is at most flavored and every
flavor shares the model and sub-model, so a completion changes every flavor's
timing together. The default is an expression that names its source: `simulate(object, …,
times = times_of(object))`, an exported accessor that documents the rule on
one help page and works on a specification or a fit (a fit reads its
`sub_model` and `model_spec$behavior`). An attribute was rejected: a fit has no
specification to carry one, and list operations drop attributes. The accessor
returns the value with a `reason` ("timed rate", "ordered rate", "no rate"). An
explicit value equal to the default is silent; an override is announced by what
it costs:

| specification | argument | condition |
| --- | --- | --- |
| `"generated"` | `"observed"` | message: the observed stamps are held, the marks redrawn |
| `"observed"`, no rate | `"generated"` | the completion warning, naming the override |
| `"observed"`, ordered rate or coordination | `"generated"` | abort: no clock is estimated (ADR-0079) |

The result carries `times` and `times_source`, and the print shows either
`times: "observed" — from the specification (no rate)` or
`times: "generated" — requested; the specification's default is "observed"`.
The accessor is `times_of()` (ADR-0076).
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


*Superseded in part 2026-09-16 (ADR-0079) — coordination simulates time-anchored
only.* The free-running half above is withdrawn. The constant coordination rate in
the `two-sided-coordination` derivations is a device that cancels in estimation,
exactly as a nonparametric baseline does, and every one of those derivations is
under Cox. The thinning construction also runs on the rate of proposals, realized
and rejected, while a crude rate counts realized events only; the proposal rate
depends on an acceptance probability those events do not identify, so the
free-running clock would be unknown, not merely up to scale. All five mechanisms
keep the time-anchored draw from their mark multinomial, which needs no latent
process; `times = "generated"` on a coordination specification aborts saying why.
The rejection loop, its acceptance-rate diagnostic and its max-proposals bound have
no use in simulation. Whether coordination inside a timed composition may run on
the pinned opportunity clock of `two-sided-coordination` D16 — a completion-style
pinned rate, not pseudo-time — is left to that change.
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

*Revised 2026-09-15 — no FIFO; `walk_schedule()` is the queue, and the
handle fans an injected event out to its windowed copies.* Both
prerequisites landed with 2.0c: windows run on the merged walk
(`preprocess-one-walk`) and `walk_schedule()` inserts a driver event
time-ordered. Two facts then replace the FIFO. Each `(layer, ω)` is its own
derived object `layer_ω` with its own stream, so two window lengths on one
layer are two queues, and a short window entered later legitimately expires
before a long one entered earlier — the "per-window FIFO" phrase was only
ever correct read per `(layer, ω)`. And a time-ordered queue makes the
question moot: the driver schedules each expiry at `t + ω` and any mix of
lengths orders itself; a separate FIFO structure would be a second queue
for one clock. The real gap is elsewhere: `walk_inject()` on the source
layer does not move a windowed statistic, because the effect reads
`layer_ω`, and nothing on the handle derives the entry row on `layer_ω` plus
its expiry at `t + ω` from an injected source event. Task 2.7 becomes that
fan-out on the handle (the window lengths are on
`merged$units[[k]]$spec_map$plan$derivations`), with `walk_schedule()` as
the expiry mechanism; the eager-recompute agreement test stands.
*Rejected:* a per-window FIFO beside the queue (redundant); making every
driver inject `layer_ω` rows itself (each driver reimplements the window).

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


*Amended 2026-09-15 (explore) — the 2.1 driver does not replay yet, and what
replay needs is already on the schedule.* On a flavored layer modeling only
`creation`, completion correctly adds nothing, but free-running simulation never
applies an observed dissolution: the handle excludes the focal layer's update
rows from `exo_rows` wholesale, because the driver injects the modeled flavor's
events, and the unmodeled flavor's rows go with them — the simulated network
only grows. The rows to replay are identifiable. The focal update rows carry
`flavor = NA`, but their `value` is the flavor's update value, and
`info$values_equivalence[[layer]]` maps it back (`c(creation = 1,
dissolution = -1)` on the fixture, 40 rows each). Replay applies, as scheduled
rows, the focal update rows whose mapped flavor is unmodeled, under the
skip-and-count guard. Task 2.8 therefore moves ahead of 2.3, which reads its regime record anyway,
and no interim guard is needed. Once the replayed rows are exogenous,
`walk_next_breakpoint()` reports them and the clock redraws at each one — which
is exactly the right-censoring estimation applies, with no mechanism of its
own. The same mapping is what the mark
must carry: a drawn dissolution is an `increment = -1` event (task 2.2c).
### D10 — One preprocessing pass keyed by fid, never a cycle per sub-model (added 2026-09-07)

The package carries three preprocessing substrates, and the question this
decision settles is which one `simulate()` rides and why:

| Entry | What one call preprocesses | Walks per call |
| --- | --- | --- |
| `estimate_dynam()` / `estimate_rem()` on a `goldfishSpec` | the one family `sub_model` selects (`estimate_from_specification()` picks the bundle) | 1 recipe loop; rate + choice = 2 calls = 2 walks |
| `estimate_dynam()` on a flavored `goldfishSpec` (K flavors, F families) | `preprocess_flavored()`: since `preprocess-one-walk`, one call to `preprocess_joint()` — the merged single-clock walk — re-keyed family-major (the re-key goes with `printing-homogenization` D2, which retires the wrapper) | **1** (corrected 2026-09-15; was "F, never K × F") |
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

Two consequences follow — the first corrected 2026-09-15: a flavored
`goldfishSpec` now estimates and simulates through the same merged walk
(`preprocess_flavored()` is a front-end over `preprocess_joint()` and
`walk_open()` builds from `build_merged_blocks()`), so the F-pass duplication
below no longer exists; what remains duplicated is the plain rate + choice
`goldfishSpec`, whose two `estimate_dynam()` calls run two walks because
`sub_model` selects one bundle — the one-call container fit is ADR-0002's
block selector, and the list-shaped-output objection to it is gone under
ADR-0071. Original text: a flavored `goldfishSpec` simulates through
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
                PE  evaluate(stats, θ, risk, meta)  the model variant: values over the fid's candidate space
                                                    (default: goldfish's evaluator, vector θ, exp / softmax)
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
each step to a numeric vector of length `p_fid` — the one shape goldfish's
own evaluator accepts. *Revised 2026-09-15 (ADR-0074):* the model variant
is not a parameter shape but a fifth plug point, **PE**, an `evaluate`
step in `set_simulation_steps()`. It receives the narrow contract — the
fid's statistics rows (sender-major), `θ` as the provider returned it, the
live risk set (`active_sender`, `active_dyad` with its encoding), and a
small `meta` (family token, `n_actors1`, `n_actors2`, the sender for a
choice row) — and returns the values over the fid's candidate space
(hazards for a timed family, probabilities for a multinomial one, exact
zeros outside the risk set) that P2 and P3 consume. The default is
`evaluate_process_state()` on a vector `θ`. A per-actor random effect is
then an evaluate step that takes `θ` as an `n_ego × p` matrix and forms the
row-wise product; a discrete- or continuous-time regime is P1 returning the
regime's vector (plus a breakpoint), needing no evaluate step; a regime
*mixture* marginalizing over the latent state is an evaluate step; the
DyNES augmenter is P1 plus P4 and never touches PE; a non-log-linear hazard
is an evaluate step. What the narrow contract does not cover is a variant
that reads the raw network state rather than the fid's statistics, or one
coupling fids inside a single evaluation; those would need the walk itself,
which stays internal. The `n_ego × p` matrix branch task 2.0d added to the
evaluators is therefore reverted from goldfish (its diff kept as a patch
under `.plan/` and re-homed as goldfish.latent's evaluate step), so the six
evaluator sites carry the vector case only. `at()` may also return a next `breakpoint` time, which the clock
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
models and its posterior draws, goldfish owns the loop). *Rejected 2026-09-15:* keeping the
matrix branch as the variant mechanism — it hard-codes one variant into six
internal sites and invites the next (a mixture, a different link) to do the
same; passing the whole materialized state to PE — freezes an internal shape
as the contract, where the narrow triple is what every named variant
actually reads.


*Amended 2026-09-15 (explore) — every fid goes through the evaluate seam, and
the completed defaults are built-in steps.* 2.1 evaluated a completed default
beside the seam: `rate_values()` and `mark_evaluation()` branch on whether a fid
was walked, and `pinned_rate_values()` / `uniform_choice_values()` compute
values the evaluate step never sees. That is two mechanisms for one quantity,
and it is where the authored `~ 1` rate fell through (D5 amendment 2). Each fid
now resolves an evaluate step by regime, with PE's one signature and one
output: `default` (log-linear, a modeled fid), `constant` (a completed pinned
rate, `exp(intercept_w)` over the risk set), `uniform` (a completed choice,
equal weight over the risk set given the sender). The built-ins need no sampler
of their own: the process-state evaluator on a degenerate zero-column state is
exactly uniform over the available alternatives, which is how
`intercept_only_rate_state()` already evaluates the pinned rate. The draw stays
in the mark and the clock, which consume the same shape whichever step produced
it — so a per-actor random effect and an HMM mixture are further entries in the
same table rather than further branches. A supplied `evaluate` replaces `default` only (settled 2026-09-15). A variant
that wants to act on a fid means the fid carries information, so the fid is
authored — and an authored `rate = ~ 1` is modeled after D5 amendment 2, which
lands it in `default` anyway. Replacing the built-ins as well would make every
variant author re-implement the uniform and constant draws.
### D12 — A DyNAM step evaluates one sender's row, and the evaluators stop building an index nobody reads (added 2026-09-09)

Task 1.2 measured what one replicate costs. On Social Evolution, stepping the
whole schedule with no evaluation is 0.204 s, close to the 0.130 s the batch
merged walk pays; adding rate evaluation costs 0.37 ms per event; adding choice
evaluation costs 4.85 ms per event, which is 89 percent of the replicate.
*(Measured 2026-09-09, before `preprocess-one-walk` removed the per-event
adjacency copy from both substrates; the ratios are the argument, the
absolute numbers are stale. The two diagnoses below and the `sender`
argument stand.)*

*Re-measured 2026-09-15 at task 2.0e, same method and same model
(`.plan/sp/replay_split_2026-09-15.R`): stepping the whole schedule with no
evaluation is **0.110 s**, rate evaluation costs **0.10 ms per event**, and
choice evaluation costs **4.45 ms per event** before this task's index-frame
removal and **0.67 ms** after it. That 85 percent drop confirms the first
diagnosis below rather than assuming it. A whole replicate falls from 2.401 s
to 0.447 s stacking the full matrix, and to 0.184 s (0.17 ms per event)
evaluating only the drawn sender's row — the second diagnosis. On CollegeMsg
a step falls from 104 ms to 77.9 ms with the full matrix and to 9.2 ms at the
drawn sender, putting one replicate near nine minutes rather than 1.7 hours.
Choice evaluation is no longer the replicate: at 0.17 ms per event against
0.25 ms of stepping, the substrate is now the larger half.*

Two causes, both avoidable, and neither intrinsic to the model.

**The index nobody reads.** `.pse_eval_choice()` returns its probabilities
alongside `data.frame(index_i = rep(s, n2), index_j = seq_len(n2))`, and
`walk_evaluate_choice_matrix()` keeps only `ev$value`. On Social Evolution that
is 84 discarded frames per event, 36,876 per replay, and a profile puts
`data.frame` and its internals (`deparse`, `make.names`, `as.data.frame.integer`,
`pmatch`) at 75 percent of total time; matrix multiplication does not reach the
top eighteen. Timed directly, the discarded frames are 1.732 s against 0.068 s
for the row slices and products they accompany. The R estimation backend already
solved this: it stores parallel integer vectors
(`index_i_list[[e]] <- i_vec + 1L`). The index is determined by `n1`, `n2` and
the sender, so it is derivable on demand.

**The n1 - 1 rows.** DyNAM factorizes the intensity as a sender hazard times a
receiver softmax conditional on that sender. A step therefore draws the waiting
time from the total rate, draws the sender, and needs one row of the choice
matrix. `walk_evaluate_choice_matrix()` calls the evaluator once per sender and
stacks all `n1` rows, so the per-step cost is proportional to `n1` and need not
be: Social Evolution has 84 senders, CollegeMsg 1899, where one replicate
extrapolates to about 1.7 hours at 104 ms per step. This is specific to the
factorized families; REM, REM-ordered and DyNAM-MM put every dyad in one
competing set and genuinely need all `n1 x n2` values.

The full-matrix path must survive either way: `test-walk_handle.R` compares the
whole matrix against `materialize_process_state()`, and that is the
batch-vs-replay oracle.

**Settled 2026-09-09: `walk_evaluate()` grows a `sender` argument.** Absent, it
returns today's full matrix and every existing caller and test is unchanged;
supplied, it returns that sender's row. A separate row-shaped entry point was
the alternative, and it was rejected because two functions computing the same
quantity drift, and the oracle tests are the only thing that would catch the
drift.

The objection to a defaulted argument is per-call overhead, so it was measured
before deciding. Two million calls, a three-argument function against the same
function with a fourth defaulted argument and an `is.null()` branch:

| call | per call |
| --- | ---: |
| three arguments | 0.179 us |
| four, default taken | 0.227 us |
| four, supplied | 0.235 us |

The overhead is 0.048 us. That is 27 percent of an empty function body and
0.001 percent of one choice evaluation, which task 1.2 measured at 4.85 ms. The
argument is free at the scale it is used.

*Rejected here:* maintaining the linear predictor incrementally instead **of**
the statistics. It is lossy — the statistics cannot be recovered from the
projection — and it cannot even maintain itself, since a broadcast carries a
value rather than a delta and the increment needs the previous statistic.
Simulation output is the event sequence anyway, from which any GOF auxiliary
statistic is recomputed by re-walking, so nothing downstream needs it. Carried
*in addition to* the statistics it is a live idea, for the families where every
alternative competes and only while `coef` is fixed for the replicate: kept as
an exploration in ADR-0058, not scheduled here.

### D13 — The walk registers the focal layer of every compiled unit, batch included (added 2026-09-16, ADR-0082)

A specification whose formulas read only exogenous covariates
(`rate = ~ 1 + ego(floor)`, `choice = ~ alter(floor)`) estimates on both
paths but cannot simulate. The shared object registry is the union of the
units' `plan$objects`, so a focal layer no effect term reads is not in it:
`walk_inject()` fails to resolve the layer it is asked to write
(`goldfish_walk_bad_event`), and a derived flavor mask whose atoms read that
layer is refused as uncovered (`goldfish_walk_unsupported`). Measured on
`social_evolution` 2026-09-16: the exogenous-only specification's registry
holds `nodes$floor` alone, its state container carries no network at all,
while its schedule carries all 439 dependent rows — the dependent stream is
added by layer name, never through the registry, so only the state side is
missing.

The walk therefore registers the focal layer of every compiled unit,
appended **after** the units' objects so no existing oid moves, and appends
that layer's own event stream to the joint schedule as a covariate stream.
The stream matters as much as the row: the focal network's state write rides
its covariate stream (`events_objects_link`), which today exists only
because some effect registered the object — on the endogenous model the
schedule carries 439 dependent plus 439 covariate rows on `calls`, on the
exogenous-only one zero. Registering the object without its stream would
materialize a matrix the batch never updates.

This lands in `build_merged_blocks()`, so the **batch path gets it too**. It
is byte-identical for every model that reads its own focal layer, which is
every frozen baseline: their formulas all carry `indeg`, `outdeg`,
`inertia`, `recip`, `trans`, `tie(contignet)` or an absolute-layer degree,
and the nearest miss (`~ 1 + indeg + global(seasons$winter)`) still reads
`calls`. Only an exogenous-only model — of which there is none in the
baselines — sees a new registry row, at the end.

*Which layers.* The focal of every compiled unit, which today is exactly the
set the driver injects into: a unit is one `(layer, family)` pair, so the
focals are the modeled layers, and every modeled process can fire. Taking
the rule from the units rather than from "what the driver will inject" keeps
it computable at open time and stays right when a process is scheduled
rather than drawn — an `anchored-replay` flavor (D9) writes its observed
events into the same state without the driver ever drawing it.

*Rejected:* letting `walk_inject()` no-op on an unregistered layer — within
such a run nothing can observe the difference, since nothing reads the
layer, but the handle would then hold a network the run pretends to update,
and every later consumer (replay, GOF, an augmenter) would have to rebuild
the state from the events instead of reading it. *Rejected:* waiting for
`constraint-objects-on-shared-walk` — its D1 threads *constraint sub-plan*
objects with exactly this append rule, but the injection case has no
constraint at all, so it is a third category that change does not cover as
written; the shared rule is deliberately the same, so that change
generalizes rather than collides.

*What it means for GOF (Alvaro, 2026-09-16).* Nothing, and that is the
correct expectation. A Boschi-Wit style check — whether a parameter moves
over the sequence — reads the same on an exogenous-only model. An
Amati-Snijders-Lomi style check asks whether auxiliary statistics (the
distribution of closing reciprocal events, say) are reflected, and a model
with no endogenous effects should *not* reproduce them. The value of the
exogenous-only model is elsewhere: it is the first rung of a taxonomy of
increasing complexity, where what matters is how the coefficients move as
endogenous terms are added.

*Storage.* Registering one more dense `n1 x n2` double per unread focal
layer is the cost (about 28.8 MB at 1899 actors). Whether the state should
be sparse at all is a separate question, and not this change's:
vault ADR-0081.

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
  handle. The per-side question for a coordination fid moved with the
  matrix branch: it is now a question for goldfish.latent's evaluate step
  and `two-sided-coordination`'s random effects, not for goldfish's
  contract (2026-09-15).
- **[D5 / 2.11]** Whether a free-running run whose composition drifts from
  the observed period average should re-pin a completed intercept from the
  live exposure, or keep the observed pin (2026-09-15; the exposure
  integral makes the live value available either way).
- **[D11 / parametric-rates]** The per-actor time origin of the Weibull
  hazard across a regime switch or a window breakpoint; per-segment
  inversion handles it only if the origin is stored per actor on the handle.

- ~~**[D5 / 2.2b]** How a completed default reaches its process's support
  mask~~ Resolved 2026-09-15: its own mask in a flavored gap, the same-process
  sibling's in the whole-family case (D5 amendment 4).
- ~~**[D6 / ADR-0076]** How the derived `times` is signaled~~ Resolved
  2026-09-15: `times = times_of(object)`, overrides announced by cost,
  `times_source` and a print line (D6 amendment).
- ~~**[D2 / ADR-0033]** Whether the Cox family and coordination generate at
  all~~ Resolved 2026-09-16 (ADR-0079): time-anchored only, no pseudo-time; a
  choice-only DyNAM keeps ADR-0076's requested completion.
- ~~**[D11 / built-ins]** Whether a caller may replace a built-in evaluate step
  per regime~~ Resolved 2026-09-15: `default` only (D11 amendment).
- **[D7 / ADR-0079]** Whether coordination inside a timed joint composition may
  run free on the pinned opportunity clock ρ̃_k + ρ̃_l (`two-sided-coordination`
  D16); left to that change.
