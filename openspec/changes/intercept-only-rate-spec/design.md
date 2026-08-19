## Context

Created 2026-07-27 from `make-multivariate-spec`'s D9 generative-readiness
completion. In a **timed** multivariate composition every modeled flavor must place
its events on one shared continuous clock (D9: coupling runs on that one clock). A
flavor missing its rate — a choice-only DyNAM flavor, or a flavor keyed only in the
`choice` list — has no waiting-time model and so cannot fire on that clock. D9's
completion table fills that gap with an **intercept-only rate, intercept pinned per
wave-period from observed counts**, and marks the primitive as owned by this
standalone change (a hard dependency, analogous to `make-multivariate-spec` §1b's on
`formula-drives-focal`). Three consumers need the same primitive — D9's timed-regime
completion (`complete_generative_spec()`), `process-simulation`'s `simulate()`, and
`dynes-augmentation`'s augmenters/pool evaluator — so it is defined once here rather
than re-derived three times.

Current state grounding (against the landed code):
- A timed DyNAM-rate hazard is `exp(beta^T s_i)` on the absolute scale
  (`R/process_state_evaluators.R`), with an optional intercept column at effect
  position 1 (`addInterceptEffect`, `has_intercept`). An intercept-only rate is the
  degenerate case: no covariate columns, hazard `= exp(intercept) = λ`, a constant.
- `make_specification()` already carries `has_intercept` and a `rate` /
  `rate_ordered` sub-model distinction; the flavor-keyed rate/choice lists come from
  `flavored-processes`. The pinned rate reuses this representation — it is a rate
  sub-model whose intercept is *fixed*, not a new evaluator.
- The design deliberately mirrors **RSiena's period-specific basic-rate parameters**
  (one basic rate per inter-wave period) sitting against pooled evaluation effects —
  here the "basic rate" is the pinned intercept, not an estimated one.

## Goals / Non-Goals

**Goals:**
- A rate sub-model representation: a **constant baseline hazard** with a **pinned,
  non-free intercept**, reusing the existing constant-hazard evaluation path (D1).
- The per-period pin `intercept_w = log(count_w / (T_w · |R_w|))` computed from
  per-period counts, exposures, and the period's average risk-set size — the counts
  and exposures **the consumer supplies** (the augmenter's wave Hamming diff, or
  `simulate()`'s observed count over a window) — one piecewise-constant plateau per
  period (D2, D5).
- A **per-actor constant hazard** `exp(intercept_w)`, uniform across the
  support-legal set (self-loops out, constraints inherited), so it is commensurable
  with any competing flavor's per-actor rate on the shared clock; the uniform sender
  follows as a consequence, not a separate aggregate draw (D4/D5).
- The **zero-free-parameters contract** (D4): `intercept_w` computed once and frozen (D7),
  constant across every EM / MCMC / simulation iteration, excluded from the
  optimizer's score/Hessian, optionally a reported constant offset.
- The **user surface** (D6): an intercept-only rate is understood as *pinned* in the
  generative/joint context (`estimate_dynes()` / `simulate()`), with a context-aware
  warning; the single-process estimation path is unchanged.

**Non-Goals:**
- The completion transform, the `process_map` `completed` column, and the
  per-consumer-entry warning — those are `make-multivariate-spec` D9 (this change is
  the primitive it fills the timed gap with).
- **Deriving the per-period counts or the period partition**: the consumer supplies
  them (the augmenter diffs waves into the Hamming set; `simulate()` supplies/infers
  the window). This change pins given `(count_w, T_w)` (D5).
- **Performing the sender draw and guarding an empty/saturated support set** — the
  consuming simulation/augmentation routine owns the draw; this change defines only
  the per-actor uniform support-legal *semantics* it draws against (D5).
- **Latent between-wave counts / churn beyond the net Hamming diff** — the count is
  treated as the deterministic net diff for now; the true latent count is future
  development (Open Questions).
- Ordered-regime missing-rate timing — `process-simulation`'s pseudo-time /
  fixed-template modes handle that; the intercept-only rate is timed-only (D3).
- The fully rate-less `simulate()` budget (event-count / time-window) —
  `process-simulation`, not this change.
- Any change to how a *modeled, estimated* rate (with effects) is fitted — the frozen
  1e-6 baselines gate that path and this change never touches it.

## Decisions

### D1 — A pinned rate is a rate sub-model with a fixed intercept, reusing the timed hazard path

The intercept-only rate is represented as a **rate sub-model whose sole effect is an
intercept and whose intercept value is pinned**, not an estimable parameter. It reuses
the landed timed DyNAM-rate hazard `exp(intercept)` with **no covariate columns**
(`R/process_state_evaluators.R`) rather than adding a parallel evaluator: the constant
hazard is the degenerate `has_intercept = TRUE`, zero-effect case. The representation
records that its intercept is *fixed* so downstream θ-layout and optimizer code can
exclude it (D4). *Rejected:* a bespoke "constant rate" object outside the rate
sub-model machinery — it would duplicate the hazard/evaluation path and force every
consumer (walk handle, augmenters, pool evaluator) to special-case a second rate
shape; reusing the existing path means `walk_evaluate()` and the batched evaluator get
it for free. This reuse is **per-actor**: the pin slots into the summed hazard
`Sum_i exp(intercept)` (`normalizer <- sum(hazard)` in `.pse_eval_rate`) alongside any
competing flavor's per-actor hazards, which is exactly the commensurability D8 requires.

### D2 — Pin per inter-wave period: `intercept_w = log(count_w / (T_w * |R_w|))`, piecewise-constant

The pin is **per wave-period**, not global, and lives at the **per-actor** layer: the
stored per-actor hazard is `lambda_w = count_w / (T_w * |R_w|)`, recorded as
`intercept_w = log(count_w / (T_w * |R_w|))`, where `count_w` is the flavor's per-period
count, `T_w` is that period's duration (the **exposure denominator**), and `|R_w|` is
the period's **time-weighted average risk-set size** — `(1/T_w) * integral_w |R_g(t)| dt`,
the per-period analogue of goldfish's `avg_active_entity` (D8, the relational form, left
unchanged); in the **panel** regime `|R_w|` is instead the **wave-endpoint average** of
the flavor's rate entity (D9). The aggregate rate is then
`|R(t)| * exp(intercept_w)`, and because `|R_w|` is the time-weighted average this
integrates to `count_w` over the period even as the live risk set moves. The result is a
**piecewise-constant baseline hazard**, one plateau per inter-wave period — the direct
analogue of RSiena's period-specific basic rates (which likewise divide by `n_actors`).
*Rejected:* a single global `lambda = count / T_total` — it would misplace events when the
flavor's rate genuinely differs across periods (the whole reason RSiena parameterizes
per period), biasing a mixed-process simulate/augment; per-period pinning reproduces
each period's count exactly. *Also rejected:* an un-normalized aggregate
`lambda_w = count_w / T_w` — incommensurable with a competing per-actor rate on the
shared clock (D8).

**Counts and period boundaries are consumer-supplied inputs, not derived here** (D5).
In the DyNES/panel case `count_w` is the deterministic net **Hamming diff** between the
two observed wave states bounding period `w` — a net-change *floor*, not a latent
micro-count; churn beyond the net diff (create→dissolve→recreate) is future development
(Open Questions), and the `estimate_dynes()` warning states the floor assumption. In
`simulate()` `count_w` is the observed event count over the wave grid or, absent waves,
over a single user-provided or min–max-inferred window (D5). This change consumes
`(count_w, T_w)` and pins; it neither diffs waves nor infers windows.

### D3 — Timed regime only; ordered is out of scope

The primitive is meaningful **only** where there is a shared continuous clock to place
events on. Regime is inferred upstream (D9: timed iff any process carries a
waiting-time/intensity rate), and this change is invoked only on the timed branch. In
the ordered regime a missing rate has no clock to be pinned against; its timing is
`process-simulation`'s pseudo-time / fixed-template modes. This change neither defines
nor touches the ordered path. *Rejected:* a unified "missing-rate" primitive spanning
both regimes — the two have incompatible semantics (a continuous intensity vs. a
sequence position), and conflating them would drag pseudo-time concerns into a
count/exposure pin that has no meaning without a duration.

### D4 — Per-actor constant hazard; uniform support-legal sender as a consequence; zero free parameters

`exp(intercept_w)` is a **per-actor constant hazard** — the same value for every
support-legal actor — **not** an aggregate flavor scalar and **not** a per-actor vector
to be estimated. Every support-legal actor carries this identical hazard, so on the
shared clock the pinned flavor slots into the competing-risks superposition
`Sum_i exp(intercept)` alongside any competing flavor's per-actor rates (D8). The
placement semantics follow **as a consequence**, not a separate aggregate draw: because
the per-actor hazards are equal, the competing-risks minimum selects a sender **uniformly
among the support-legal actors** at that instant — support constraints **inherited** from
the flavor (never fabricated, never borrowed from a sibling flavor — the same rule D9
states for the uniform choice), **self-loops disallowed** as the sole automatic
restriction. **The draw itself — and guarding an empty/saturated support set — is
performed by the consuming routine** (`simulate()` / the augmenter, D5); this change
fixes the *semantics* so every routine draws consistently. In the relational case, because
`|R_w|` is the period's time-weighted average risk-set size (D2), the aggregate rate
`|R(t)| * exp(intercept_w)` **integrates to `count_w`** over the period (under the
non-empty-support caveat the routine enforces) even as actors enter/leave support
legality — no re-pinning needed; in the panel case `|R_w|` is the wave-endpoint average
(D9), which reproduces `count_w` against the two observed wave states.

The **zero-free-parameters contract** follows: the intercept is a deterministic
function of the supplied count/exposure, hence **frozen** (D7) and **never recomputed
from generated events**. Crucially, it is **θ-independence** — not iteration-constancy —
that grants the exclusion: `intercept_w` does not depend on the estimated parameters, so its
timing likelihood is **additive-constant w.r.t. θ** and is **excluded from the score
and Hessian**; it never enters the fid / θ layout. (The likelihood *value* is also
iteration-constant today because the count is the fixed net Hamming diff; a future
latent count would vary per iteration yet remain θ-independent, so the exclusion would
still hold.) The fixed contribution MAY be added as a **constant offset** to a
*reported* total log-likelihood, but never to the optimization objective. *Rejected:*
an **un-normalized aggregate** scalar `count_w / T_w` — incommensurable with a competing
per-actor rate on the shared clock (D8). *Also rejected:* a **structured (e.g.
out-degree-weighted) per-actor vector** — that reintroduces structure that looks
estimable; the pin is deliberately **uniform** across the support-legal set, which is
what makes the competing-risks sender draw uniform for free. *Also rejected:*
recomputing the pin from augmented counts each EM iteration — that is the future
latent-count development (Open Questions), out of scope here.

### D5 — A thin primitive: pin + semantics + contract here; counts, periods, drawing in the consumer

The boundary is drawn to keep the primitive standalone and reusable by all three
consumers. **This change owns**: the intercept-only rate representation (D1), the pin
`intercept_w = log(count_w / (T_w * |R_w|))` given per-period counts and exposures (D2),
the per-actor uniform-support-legal-sender *semantics* (D4), the zero-free-parameters contract
(D4/D7), and the user-facing pinning rule + warning (D6). **The consumer owns**:
deriving the per-period counts (the augmenter's wave Hamming diff; `simulate()`'s
observed count) and the period partition (wave times, or a single provided/min–max
window when there are none), **performing the sender draw**, and **guarding an
empty/saturated support set**. *Rejected:* a fat primitive that also diffs waves,
infers windows, and draws senders — it would duplicate machinery the augmenter and
`simulate()` already own (wave diffing is `dynes-augmentation`'s; the window/budget is
`process-simulation`'s) and couple the pin to risk-set/mode/direction details that
differ per routine. The thin boundary makes `(count_w, T_w, |R_w|) → intercept_w` a pure
function any routine can call (returning the per-actor `intercept_w`, D2).

### D6 — User surface: intercept-only ⟺ pinned in the generative context; single-process path unchanged

An intercept-only rate — the leading `1` with no other rate effects, `rate = ~ 1` — is
understood as **pinned** in the generative/joint context (`estimate_dynes()`,
`simulate()`, and D9 completion), and **only** there. The re-interpretation is safe
because goldfish's `1` is otherwise an **estimated** baseline log-hazard (an opt-in free
parameter used pervasively as `~ 1 + effects`, `formula_parser.R:1197`), and a *bare*
intercept-only rate is the one case with nothing worth estimating — its estimated MLE is
the degenerate `log(N / T / |R|)` (goldfish's `log(n_dep / total_time /
avg_active_entity)`: the per-actor baseline hazard, the event count divided by the
elapsed time and the average risk-set size), and it has no standalone estimation use
today (only a rejection fixture). The pin is exactly the **per-period frozen** form of
that MLE (D2, D8). So pinning it takes nothing away: a rate carrying **any** effect
keeps its estimated baseline (SE and all), and only the effect-free rate is pinned. Both
routes — a user-written `rate = ~ 1` and a completion-supplied rate for a choice-only
flavor (D9) — produce the **same** pinned object, so there is one concept and one
warning. The **single-process path is untouched**: `estimate_dynam()` / `estimate_rem()`
keep the estimated-intercept meaning (frozen 1e-6 baselines gate it), so pinning is
scoped entirely to the new generative consumers.

The pinned rate carries **no standard error** and does not enter estimation; each
consumer **warns at its entry**, worded for its source: `estimate_dynes()` — pinned from
the wave Hamming diff (a net-change floor), no SE, excluded from estimation;
`simulate()` — pinned from the observed event count (SE is not a simulation concept).
*Rejected:* a dedicated sentinel (`rate = pinned()`) — unambiguous but a second way to
say a thing the generative context already determines; *also rejected:* erroring on a
user-written `~ 1` in a joint spec and pinning only completion-supplied rates — it
splits one concept into two and surprises a user who deliberately wrote an
intercept-only rate.

### D7 — Compute `intercept_w` once and freeze it

Because the count is, for now, the deterministic net Hamming diff between two
**observed** wave states (or an observed count over a fixed window) and `|R_w|` is a
property of the observed presence/constraint schedule, `intercept_w` does not change
across iterations — so it is computed **once at consumer setup** and frozen on the
completed spec / `process_map` fid, read unchanged by every EM / MCMC / simulation
iteration and never recomputed from generated events. The per-actor framing (D8) makes
this freeze clean: `exp(intercept_w)` is a genuine per-actor constant — no live
re-normalization by the risk set is needed, since the shared evaluator already forms the
aggregate `|R(t)| * exp(intercept_w) = sum(hazard)` from the live presence at each step. *Rejected:* a per-iteration
recompute hook fed by augmented counts — that is the not-yet-scoped **latent-count**
development (Open Questions); building the seam now carries structure for a feature that
does not exist, and the frozen value is trivially replaced when that change lands. The
user-facing warning already primes users that the pin is a net-diff floor today.

### D8 — Per-actor, risk-set-normalized pin (not an aggregate): commensurability on the shared clock

The pin is a **per-actor** constant hazard `exp(intercept_w)` with
`intercept_w = log(count_w / (T_w * |R_w|))`, **not** an aggregate flavor intensity
`count_w / T_w`. This decision supersedes any earlier aggregate framing and drives D1,
D2, D4, D6, D7.

The decisive reason is **cross-flavor commensurability**. On a shared continuous clock
the next event is the minimum, over every actor × flavor, of exponential waiting times,
and which `(flavor, actor)` fires is proportional to the **per-actor** hazards
`exp(β_g^T s_i)`. A competing flavor with a genuine (possibly complex) rate
specification contributes a *spread* of per-actor hazards; a pinned flavor represented
as a single aggregate scalar could not enter that superposition or that proportional
selection — it would mean summing a scalar against a vector of per-actor rates. Placing
the pin at the per-actor layer keeps every flavor in one hazard space, so both the
waiting-time superposition and the sender/flavor selection stay coherent. (Counts do not
discriminate: **both** framings reproduce `count_w` over the period — commensurability
does.)

The normalizer `|R_w|` is the period's **time-weighted average risk-set size** —
`(1/T_w) * integral_w |R_g(t)| dt` — the per-period analogue of goldfish's
`avg_active_entity`, the divisor in the estimated baseline
`log(n_dep / total_time / avg_active_entity)`. This aligns the pin with RSiena's
`n_events / T / n_actors` (RSiena's fixed `n_actors`; goldfish's time-weighted,
composition- and constraint-aware risk set) and makes the pin literally the **per-period
frozen** form of goldfish's own estimated intercept-only MLE. Because `|R_w|` is the
time-weighted average, the aggregate `|R(t)| * exp(intercept_w)` integrates to `count_w`
over the period even as the live risk set moves.

*Rejected — the aggregate scalar `count_w / T_w`:* though it reproduces `count_w` exactly
under an intra-period changing risk set, it is **incommensurable** with a competing
per-actor flavor (the core defect above) and would let a flavor's placement be advantaged
or penalized purely by its risk-set size relative to its competitors. *Also rejected — a
structured (e.g. out-degree-weighted) per-actor vector:* it reintroduces structure that
looks estimable; the pin is deliberately **uniform** across the support-legal set, which
also makes the competing-risks sender draw uniform for free (D4).

### D9 — `|R_w|` source: time-weighted active entity (relational, unchanged) vs wave-endpoint average (panel)

`|R_w|` (D2/D8) is the average size of the flavor's **rate entity** — active **senders**
for an actor-oriented flavor, active **dyads** for a tie-oriented (REM) flavor (goldfish's
`scalar_entity`). The **consumer supplies it** (D5), parallel to `count_w`, and its source
depends on how finely the risk-set schedule is observed:

- **Relational (event stream observed) — unchanged:** the full timeline is known, so
  `|R_w|` is goldfish's time-weighted average active entity `avg_active_entity` restricted
  to period `w` — `(1/T_w) * integral_w |R_g(t)| dt`. Exact; this is the D8 form and is
  **not** modified by this decision.
- **Panel / DyNES (waves only) — Tier 3 wave-endpoint average:** the between-wave timeline
  is latent, and for a **network-dependent constraint** (a creation flavor's non-ties, a
  dissolution flavor's ties, a sender gated by receiver availability) the post-constraint
  risk set itself is latent between waves. `|R_w|` is approximated by the **mean of the
  flavor's post-constraint entity count at the two observed wave states** bounding period
  `w`: `|R_w| ≈ (|R_g(w_{k-1})| + |R_g(w_k)|) / 2`. This reads only the two wave states the
  Hamming-diff `count_w` already reads.

Presence/composition is observed in **both** regimes (its own `present` event stream), so
only the event *times* and the network-dependent constraint are latent in panel data. The
wave-endpoint average is chosen over a **flat entity count** (`n_actors`, or `n(n-1)`
dyads) because a flat count badly over-states a sparse tie flavor's risk set: a dissolution
flavor's risk set is the current ties (≪ all dyads), so `|R_w| ≈ n(n-1)` would crush the
pinned hazard `count_w / (T_w * |R_w|)` toward zero by orders of magnitude. The endpoint
average is far more accurate at no extra observational cost, and reduces to the flat count
when the entity count is unchanged across the two waves.

*Rejected — a flat entity count (`n_actors` / `n(n-1)`):* correct only under a static,
presence-only risk set; knowingly lossy for the tie-flavor creation/dissolution case that
dominates DyNES. *Deferred:* integrating the true latent between-wave risk-set trajectory
travels with the latent-count development (Open Questions) — the wave-endpoint average is
its two-point degenerate form, replaced when that lands.

## Risks / Trade-offs

- **A per-period pin computed from a mis-identified period boundary** would place a
  flavor's events against the wrong duration → the period boundaries are the
  consumer-supplied wave times / window (D5); tests assert
  `intercept_w = log(count_w / (T_w * |R_w|))` against hand-computed per-period
  counts / durations / average risk-set sizes on a multi-wave fixture.
- **The "zero free parameters" contract leaking into the optimizer** (the pinned
  intercept silently appearing in θ / score / Hessian) would bias every estimated
  coefficient → the representation marks the intercept fixed, and a test asserts the
  fit's θ layout, score, and Hessian dimensions are unchanged by adding a pinned rate,
  with only a reported total log-likelihood offset differing.
- **Per-actor pin drifting from `count_w` under a changing risk set** (the frozen
  per-actor hazard times a moving `|R(t)|` failing to integrate to `count_w`) → because
  `|R_w|` is the period's time-weighted average risk-set size (D8),
  `integral_w |R(t)| * exp(intercept_w) dt = count_w`; a simulation test replays a period
  with actors entering/leaving support legality and checks the realized expected count
  matches `count_w`.
- **User `~ 1` misread as estimated** (a pinned rate silently entering the optimizer,
  or an `estimate_dynam()` `~ 1 + effects` intercept wrongly pinned) → D6 scopes
  pinning to the effect-free rate in the generative context only; tests assert
  `estimate_dynam()` on a bare `~ 1` still estimates (baselines PASS) and a pinned rate
  never appears in θ.
- **Consumer divergence** (D9 completion, `simulate()`, and the augmenters each
  re-deriving the pin slightly differently) → the pin
  `(count_w, T_w, |R_w|) → intercept_w` is one shared function defined here; all three
  consumers call it, none reimplements it (the
  draw is the routine's per D5, but against the one shared semantics).

## Migration Plan

Purely additive: a new rate sub-model representation plus its per-period pin
`(count_w, T_w, |R_w|) → intercept_w`. No existing surface changes; the single-process / flavored
estimation path (and therefore the frozen 1e-6 baselines) is untouched. Rollback is
reverting commits — no data-format or baseline impact.

## Open Questions

**Resolved for now (2026-07-27 interview), recorded as future development:**
- **Latent between-wave count (Q-A / Q-E).** `count_w` is the deterministic net
  **Hamming diff** between observed wave states — a net-change *floor* blind to
  create→dissolve→recreate churn. The true latent count (and a per-iteration recompute
  of `intercept_w` from augmented counts, whose timing likelihood would then no longer be
  iteration-constant, though still θ-independent) is **future development**, not built
  here (D7). The same development refines the panel `|R_w|` from its two-point
  wave-endpoint average (D9) to the integrated latent risk-set trajectory. The
  `estimate_dynes()` warning states the Hamming-floor assumption.
- **Zero-count period (Q-C).** `count_w = 0 ⇒ exp(intercept_w) = 0` (per-actor hazard
  zero): the flavor cannot fire in
  period `w`. Correct under the net-Hamming-diff assumption (it reproduces a net change
  of 0); revisited only with the latent-count development above.

**Resolved for now (2026-07-27 interview), applied at task time:**
- **Half-open period convention — membership (Q-F).** An event falling **exactly on**
  an interior wave boundary belongs to the **next** period (left-closed / right-open),
  with the **final** period right-**closed** so a terminal-time event (the common
  last-wave observation in panel data) is never dropped into a nonexistent period. The
  partition is
  `[w_0, w_1)  [w_1, w_2)  …  [w_{K-2}, w_{K-1})  [w_{K-1}, w_K]` — exactly R's
  `findInterval(t, wave_times, rightmost.closed = TRUE)`. Bookkeeping only; the
  `count_w / (T_w * |R_w|)` formula is unaffected. Document the chosen convention in
  roxygen at task time (task 2.3).
- **Reported-offset surfacing (Q-G).** The pinned rate's constant log-likelihood offset
  is **printed by default** in the fitted-object print (not gated behind a request
  flag); a quiet/opt-out can be added later without breaking anyone. The `estimate_dynes()`
  surface (`abmcem` / `dynes-augmentation`) implements the print, but the default is
  decided here.

**Owned by the consuming routine, not this change (D5):**
- **Empty/saturated support (Q-D).** If no actor is support-legal in a sub-interval,
  the draw cannot place an event; the **simulation/augmentation routine** detects and
  controls this. The "reproduces `count_w`" claim carries an implicit non-empty-support
  caveat the routine enforces.
- **Period partition when there are no waves (Q-B).** A pinned relational flavor with
  no wave grid uses a **single window** — user-provided or inferred from the data
  min–max — supplied by `simulate()`. This change pins given whatever periods it
  receives.
- **Final-period duration past the last wave (Q-F, second half).** How `T_w` is bounded
  when the observation window extends **past** `w_K` arises only for a `simulate()` call
  with a user window beyond the last wave; the window is **consumer-supplied** (D5). For
  pure panel data the last wave is the end, so `[w_{K-1}, w_K]` is fully determined and
  this does not arise. Membership (above) is resolved here; this duration half stays with
  the `simulate()` window.
