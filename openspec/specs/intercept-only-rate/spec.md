# intercept-only-rate Specification

## Purpose
TBD - created by archiving change intercept-only-rate-spec. Update Purpose after archive.
## Requirements
### Requirement: An intercept-only rate is a constant hazard with a pinned, non-free intercept

The package SHALL provide an **intercept-only rate** sub-model: a constant baseline
hazard carrying **no effects** and **no free coefficient**. Its only quantity is a
**pinned intercept** derived from supplied counts and exposures. On the timed
(absolute) scale the intensity is a constant `λ` (equivalently `exp(intercept)` with no
covariate columns), reusing the existing timed DyNAM-rate hazard path rather than
introducing a new evaluator. The intercept SHALL NOT be treated as an estimable
parameter: it never enters the fid / θ layout, and no optimizer ever adjusts it.

#### Scenario: intercept-only rate carries no free parameter

- **WHEN** an intercept-only rate is constructed for a flavor
- **THEN** it exposes a constant intensity and no covariate/effect columns, and it
  contributes zero free parameters to the θ layout of any joint fit.

#### Scenario: constant hazard reuses the timed-rate evaluation path

- **WHEN** the intercept-only rate is evaluated at the current state on the timed clock
- **THEN** the constant intensity is computed through the existing timed-rate hazard
  path with no covariate contribution (no second evaluator is introduced).

### Requirement: The intercept is pinned per wave-period from supplied counts over exposure

Given a per-period count `count_w`, a period duration `T_w` (the **exposure
denominator**), and the period's average risk-set size `|R_w|`, the pinned
**per-actor** hazard SHALL be `λ_w = count_w / (T_w · |R_w|)`, recorded as
`intercept_w = log(count_w / (T_w · |R_w|))`: a **piecewise-constant baseline hazard**
with **one plateau per inter-wave period**. `|R_w|` SHALL be the average size of the
flavor's rate entity (active senders for an actor-oriented flavor, active dyads for a
tie-oriented one). Dividing by `|R_w|` places the pin at the per-actor layer (the analogue
of RSiena's division by `n_actors`), so the aggregate flavor rate is
`|R(t)| · exp(intercept_w)`. The per-period counts, the period partition, and `|R_w|`
SHALL be **inputs supplied by the consuming routine**, NOT derived by this primitive, and
`|R_w|`'s form SHALL track the regime: in the **relational** case it is goldfish's
time-weighted average active entity `avg_active_entity` restricted to period `w`; in the
**panel / DyNES** case it is the **wave-endpoint average**
`(|R_g(w_{k-1})| + |R_g(w_k)|)/2` of the flavor's post-constraint entity count at the two
observed wave states bounding period `w`. Likewise `count_w` is the deterministic net
**Hamming diff** between the two observed wave states in the DyNES/panel case (a net-change
floor) and the observed event count over the wave grid or a single supplied/inferred window
in `simulate()`. The pin SHALL depend **only** on the supplied counts, durations, and
risk-set sizes and SHALL NOT be recomputed from generated, sampled, or augmented events.
Once computed, `intercept_w` SHALL be **frozen** and read unchanged across every
EM / MCMC / simulation iteration.

#### Scenario: per-period intensity computed from supplied count, duration, and risk set

- **WHEN** the consumer supplies a count `count_w` for wave-period `w` spanning
  duration `T_w` with average risk-set size `|R_w|`
- **THEN** the pinned per-actor hazard for period `w` is `λ_w = count_w / (T_w · |R_w|)`
  (stored as `intercept_w = log(count_w / (T_w · |R_w|))`), and a distinct plateau is
  computed for each inter-wave period.

#### Scenario: panel risk-set size is the wave-endpoint average

- **WHEN** an intercept-only rate is pinned for a **panel / DyNES** flavor whose risk set
  is latent between waves (e.g. a creation flavor over non-ties, a dissolution flavor over
  ties)
- **THEN** the consumer supplies `|R_w|` as the wave-endpoint average
  `(|R_g(w_{k-1})| + |R_g(w_k)|)/2` of the flavor's post-constraint entity count at the two
  observed wave states — not a flat actor/dyad count — and the primitive pins
  `intercept_w = log(count_w / (T_w · |R_w|))` from it.

#### Scenario: relational risk-set size is the time-weighted average active entity

- **WHEN** an intercept-only rate is pinned for a **relational** flavor with a fully
  observed event stream
- **THEN** the consumer supplies `|R_w|` as goldfish's time-weighted `avg_active_entity`
  restricted to period `w`, unchanged from the estimated-baseline convention.

#### Scenario: counts and periods are not derived here

- **WHEN** an intercept-only rate is pinned for a panel flavor
- **THEN** the per-period count is taken as the consumer-supplied net Hamming diff and
  the period boundaries as the consumer-supplied wave times — this primitive neither
  diffs waves nor infers a window.

#### Scenario: pin is frozen, not recomputed from generated events

- **WHEN** the intercept-only rate is used to generate or augment events within an
  EM / MCMC / simulation iteration
- **THEN** the pinned `intercept_w` values remain fixed at their setup values and are NOT
  re-derived from the generated events at any iteration.

### Requirement: The pinned hazard is per-actor with uniform support-legal-sender semantics

`exp(intercept_w)` SHALL be a **per-actor constant hazard** — identical for every
support-legal actor — **not** a single aggregate flavor scalar. Because every
support-legal actor carries this same hazard, on the shared clock the pinned flavor
enters the competing-risks superposition `Σ_i exp(·)` alongside any competing flavor's
per-actor rates, keeping every flavor commensurable. The placement **semantics** follow
**as a consequence**, not a separate aggregate draw: when an event of the flavor fires,
its sender is drawn **uniformly among the support-legal actors** for that flavor (the
competing-risks minimum over equal per-actor hazards is uniform), support constraints
**inherited** from the flavor (uniform over the support-legal actors, never over all
actors; never fabricated and never borrowed from a sibling flavor), with **self-loops
disallowed** as the only automatic restriction. The **sender draw itself, and guarding
an empty/saturated support set, SHALL be performed by the consuming
simulation/augmentation routine**, not by this primitive. Under a non-empty support set
the aggregate `|R(t)| · exp(intercept_w)` SHALL reproduce `count_w` over the period: in
the relational case exactly (because `|R_w|` is the time-weighted average risk-set size),
and in the panel case against the two observed wave states (the wave-endpoint average).

#### Scenario: semantics specify a uniform support-legal sender

- **WHEN** a routine draws a sender for a firing of the pinned-rate flavor
- **THEN** the primitive's semantics require the sender to be uniform among the
  support-legal actors (self-loops excluded), inheriting the flavor's support
  constraints where any were defined — the uniformity following from the equal per-actor
  hazards.

#### Scenario: the routine owns the draw and the empty-support guard

- **WHEN** the support-legal set is empty or saturated at some instant of period `w`
- **THEN** detecting and controlling that condition is the consuming routine's
  responsibility — the primitive supplies only the per-actor constant hazard and the
  uniform-support-legal semantics.

#### Scenario: the pinned hazard is commensurable with a competing per-actor flavor

- **WHEN** the pinned-rate flavor competes on the shared clock with a flavor carrying a
  non-constant (effect-driven) per-actor rate
- **THEN** the pinned flavor contributes a per-actor hazard `exp(intercept_w)` to the
  same superposition `Σ_i exp(·)`, so the waiting-time draw and the sender/flavor
  selection compare like with like (no aggregate scalar summed against per-actor rates).

#### Scenario: per-actor calibration reproduces the supplied count

- **WHEN** the risk set changes in size over period `w` (actors enter or leave support
  legality) while support stays non-empty
- **THEN** the aggregate `|R(t)| · exp(intercept_w)` still reproduces `count_w` in
  expectation over the period — in the relational case because `|R_w|` is the period's
  time-weighted average risk-set size (the frozen per-actor hazard integrates to
  `count_w`), and in the panel case because `|R_w|` is the wave-endpoint average of the two
  observed states.

### Requirement: The pinned intercept carries zero free parameters into any joint fit

The pinned intercept SHALL be **excluded from the optimizer's score and Hessian** on
the grounds of **θ-independence**: `intercept_w` does not depend on the estimated parameters, so
its timing likelihood is an **additive constant** with respect to θ. It SHALL NOT enter
the fid / θ layout and SHALL NOT influence any parameter update. It MAY be included as a
**constant offset** in a reported total log-likelihood.

#### Scenario: excluded from score and Hessian

- **WHEN** a joint fit computes the score and Hessian over the estimated parameters
- **THEN** the pinned intercept-only rate contributes nothing to either, and adding a
  pinned rate leaves the fit's θ layout, score, and Hessian dimensions unchanged.

#### Scenario: optionally reported as a constant offset

- **WHEN** a total log-likelihood is reported for a fit that includes a pinned
  intercept-only rate
- **THEN** the pinned rate's fixed timing contribution MAY appear as a constant offset
  in that reported value, without ever entering the optimization objective.

### Requirement: An intercept-only rate is pinned in the generative context; the single-process path is unchanged

An **intercept-only rate** — the leading `1` with no other rate effects (`rate = ~ 1`),
or a completion-supplied rate for a choice-only flavor — SHALL be understood as
**pinned** (zero-parameter) in the generative/joint context: `estimate_dynes()`,
`simulate()`, and the D9 completion transform, and **only** there. A user-written
`rate = ~ 1` and a completion-supplied rate SHALL produce the **same** pinned object. A
rate carrying **any** effect SHALL keep its estimated baseline intercept (with a
standard error) unchanged. The **single-process estimation path** (`estimate_dynam()` /
`estimate_rem()` over one `goldfishSpec`) SHALL be unchanged: a bare
`rate = ~ 1` there SHALL retain its existing estimated-intercept meaning, and the frozen
1e-6 baselines SHALL report PASS.

#### Scenario: user-written intercept-only rate is pinned under estimate_dynes

- **WHEN** a user writes `rate = ~ 1` for a flavor in a specification passed to
  `estimate_dynes()` or `simulate()`
- **THEN** that rate is pinned (zero free parameters), identical to the object a
  completion-supplied rate would produce for the same flavor.

#### Scenario: a rate with effects keeps its estimated baseline

- **WHEN** a rate is `~ 1 + inertia` (an intercept plus effects) in a generative-context
  specification
- **THEN** its intercept is estimated as today (a free parameter with a standard error)
  — only the effect-free intercept-only rate is pinned.

#### Scenario: single-process estimation is unchanged

- **WHEN** a bare `rate = ~ 1` specification is passed to `estimate_dynam()`
- **THEN** the intercept retains its existing estimated meaning, no pinning occurs, and
  the preprocessed output is byte-identical to the pre-change path (frozen baselines
  PASS).

### Requirement: Each consumer warns that the rate is pinned, worded for its source

When a rate is pinned, the consuming routine SHALL emit a `cli` **warning** at its
entry stating that the rate is pinned and not estimated, worded for its source:
`estimate_dynes()` SHALL state the rate is pinned from the wave **Hamming diff** (a
net-change floor), carries **no standard error**, and is **excluded from estimation**;
`simulate()` SHALL state the rate is pinned from the **observed event count** (a
standard error is not a simulation concept). The warning SHALL fire at **each consumer
entry** and SHALL NOT be suppressed when the same specification is routed through a
second consumer.

#### Scenario: estimate_dynes warns about the Hamming-diff pin and no SE

- **WHEN** a specification with a pinned intercept-only rate enters `estimate_dynes()`
- **THEN** a `cli` warning states the rate is pinned from the wave Hamming diff, has no
  standard error, and is excluded from estimation.

#### Scenario: simulate warns about the observed-count pin without SE language

- **WHEN** a specification with a pinned intercept-only rate enters `simulate()`
- **THEN** a `cli` warning states the rate is pinned from the observed event count, with
  no reference to a standard error.

### Requirement: The intercept-only rate is meaningful only in the timed regime

The intercept-only rate SHALL be used only in a **timed** composition (a shared
continuous clock), where it places a flavor's events on that clock. In the **ordered**
regime it SHALL NOT be applied — missing-rate timing there is handled by the
`process-simulation` pseudo-time modes and is out of scope for this primitive.

#### Scenario: pins a missing timed rate

- **WHEN** a timed composition has a modeled flavor missing its rate (including a flavor
  that would otherwise be choice-only)
- **THEN** the flavor is completed with the pinned intercept-only rate so its events
  land on the shared continuous clock.

#### Scenario: not applied in the ordered regime

- **WHEN** a composition is ordered (no process carries a waiting-time rate)
- **THEN** the intercept-only rate is not used — a missing rate's timing is deferred to
  the `process-simulation` pseudo-time modes.

