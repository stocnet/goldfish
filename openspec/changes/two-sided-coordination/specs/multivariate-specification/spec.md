# multivariate-specification Delta Specification

Note: written against the living spec as merged at v1.9.29; carries the
D14–D16 resolution (coordination joins as a dynamu specification object).

## MODIFIED Requirements

### Requirement: make_joint_specification composes process specifications

The package SHALL export `make_joint_specification(...)` accepting two or more
`make_specification()` objects over one shared data object and returning a
multivariate specification that portrays their co-evolution. Construction SHALL NOT
require a panel-observed layer to be referenced — all viability is consumer-owned. A
combination that references no panel-observed layer SHALL compose: it is
estimation-separable (the factorized likelihood) yet generatively coupled through the
shared clock, hence a valid `simulate()` input with no other constructor;
`estimate_dynes()` (not construction) SHALL abort such an all-separable spec, naming
`estimate_dynam()`. Construction MAY emit a separability note but SHALL NOT abort on
it. DyNAM-i processes SHALL be rejected. All processes
MUST reference one shared mode-map object; one- and two-mode processes MAY be
composed, and dependent processes over distinct mode-pairs MAY be joined
provided every cross-process read conforms by mode-set identity (see the
node-space conformance requirement below). DyNAM (rate, choice) and REM
processes, timed or ordered, MAY be freely mixed, flavored or plain.
Two-sided coordination processes join as
`make_specification(model = "DyNAMu")` objects — of **any** mechanism, since
construction never owns viability; what a generative consumer cannot handle
is gated at that consumer (see the mechanism-gating requirement below).

#### Scenario: panel plus relational processes compose
- **WHEN** `make_joint_specification(friendship_spec, calls_spec, data = x)` runs
  with friendship panel-observed (flavored creation/dissolution) and calls a
  fully observed relational-event process
- **THEN** a multivariate specification is returned covering both processes'
  formulas.

#### Scenario: exogenous-only panel reference composes but is not DyNES-viable
- **WHEN** no composed process's focal layer is panel-observed, but a
  relational-event process reads a panel-observed layer as an exogenous covariate
  (e.g. `calls ~ ... + tie(friendship)` with friendship panel-observed)
- **THEN** a multivariate specification is returned — the panel layer enters as a
  static exogenous step-covariate — but no fid is coupled, so `estimate_dynes()`
  will abort on it, naming `estimate_dynam()` (nothing is latent).

#### Scenario: no panel reference composes (estimation-separable, generatively simulable)
- **WHEN** no composed process references any panel-observed layer (focal or
  exogenous)
- **THEN** a multivariate specification is returned — the processes are
  estimation-separable but generatively coupled through the shared clock, so it is a
  valid `simulate()` input — and `estimate_dynes()` (not construction) aborts it,
  naming `estimate_dynam()` for per-process estimation.

#### Scenario: a dynamu coordination process composes
- **WHEN** `make_joint_specification(friendship_spec, collab_spec, data = x)` runs
  with `collab_spec` a `make_specification(model = "DyNAMu")` object (any
  mechanism) over an undirected layer of the shared mode-map
- **THEN** a multivariate specification is returned with the coordination process's
  formulas in the `process_map`, regardless of mechanism.

### Requirement: Generative-readiness completion fills half-specified flavors

A specification SHALL be made *generatively complete* before it is used to
**generate** events (drive the walk handle via `simulate()` or an augmenter) or
estimated **jointly** (`estimate_dynes()`) — every modeled DyNAM flavor (the
choice family, coordination included) carrying both a rate and a choice — by a
completion transform run at the consumer's entry. A flavor **keyed in one
sub-model list and omitted from the other** SHALL have the missing sub-model filled
with its zero-information default and SHALL emit a `cli` **warning** naming
the layer, flavor, sub-model, and the default applied; the warning SHALL fire at
**each consumer entry** (it is NOT suppressed when the same half-specified spec is
routed through a second consumer). **Every completion default is
zero-free-parameter** — completion SHALL NOT add an estimated coefficient to θ. The
defaults SHALL be: a **uniform choice over the support-legal alternatives** (no
effects) for a missing choice; a **uniform conjunctive draw on both sides** for a
coordination process's missing choice half (the zero-information member of the
mechanism family, equal to the pre-mechanism default); a **uniform ordered rate**
(no effects) for a missing rate in
the **ordered** regime; and, in the **timed** regime, an **intercept-only rate whose
intercept is pinned** per wave-period to the **per-actor** hazard
`intercept_w = log(count_w / (T_w · |R_w|))` (the `intercept-only-rate-spec`
primitive), where `|R_w|` is the period's average size of the flavor's rate entity. A
**rate-only DyNAM** flavor (choice absent) SHALL therefore complete to a uniform
choice.

The completed uniform choice SHALL **inherit the layer's support constraints** where
any were defined (uniform over the support-legal alternatives, never over all
actors); it SHALL NOT fabricate support constraints, SHALL NOT borrow a sibling
flavor's constraints on the same layer, and SHALL disallow self-loops as its only
automatic restriction.

A composed specification's timing **regime** (ordered vs timed) SHALL be
**inferred** — timed iff any process carries a waiting-time (intensity) rate — and a
composition mixing an ordered process with a timed one SHALL be **rejected at
`make_joint_specification()`** (join time — a mix is visible only across the ≥2
composed processes, never at a single-process `make_specification()`). In the
**timed** regime a modeled flavor missing its rate
(including a flavor that would otherwise be **choice-only**) SHALL be completed with
the pinned intercept-only rate so its events land on the shared clock; in the
**ordered** regime a **choice-only DyNAM** flavor SHALL NOT be rate-completed (its
timing is supplied by the `process-simulation` pseudo-time modes). The pinned
per-actor intercept `intercept_w = log(count_w / (T_w · |R_w|))` (for a modeled panel
flavor `count_w` is the net wave Hamming diff — a net-change floor, not a directly
observed micro-count) is a **per-actor constant hazard** — identical for every
support-legal actor, **commensurable** on the shared clock with a competing flavor's
per-actor rate in the superposition `Σ_i exp(·)`, **not** a single aggregate flavor
scalar — from which a sender is drawn **uniformly among the support-legal actors** as
a consequence of the equal per-actor hazards. `T_w` and `|R_w|` SHALL be **numeric**
regardless of the underlying event stream's time representation (numeric, `POSIXct`,
`Date`, or character) — the pin computation SHALL NOT propagate a `difftime` or other
non-numeric temporal object into `T_w`. `|R_w|` (the period's average rate-entity
size) SHALL be **supplied by the consumer**, its source keyed on how the flavor is
observed — a **panel** wave-endpoint average `(|R_g(w_{k-1})| + |R_g(w_k)|)/2` or, for
an **unflavored relational** layer (no flavor keying on the process), a
time-weighted `avg_active_entity` sourced from goldfish's own preprocessing scalars
(see design D9a, D3) — and the period partition SHALL follow the half-open membership
convention `findInterval(t, wave_times, rightmost.closed = TRUE)` (interior
boundaries left-closed/right-open, final period right-closed). A **flavored**
relational (event-observed, non-panel) layer's missing rate SHALL, until a
flavor-aware relational risk-set source exists, use the same wave-endpoint/Hamming-diff
derivation as a panel layer over the layer's own observed time extent — never the
range of an unrelated layer in the same joint dataset. A completed rate whose pinned
layer has **no timed events at all** (every row is `time = NA` history) SHALL, on a
**panel**-observed layer (or the flavored-relational fallback that shares the panel
derivation), emit a `cli` **warning** naming the layer and pin to a **zero hazard**
(`intercept_w = -Inf`, the flavor cannot fire); on an **unflavored relational** layer,
whose risk-set scalars (`total_time`, `avg_active_entity`) would be zero and admit no
finite pin, it SHALL **abort** with a `cli` error naming the layer. A **per-period**
empty wave in a multi-wave grid (nothing changed between two boundaries) is NOT this
case: it pins to the intended silent `intercept_w = log(0) = -Inf` and SHALL NOT warn. Its exclusion from the
optimizer rests on **θ-independence**: `intercept_w` does not depend on the estimated
parameters, so its timing likelihood is **additive-constant w.r.t. θ** and is
**excluded from the score and Hessian** (a constant offset that MAY appear in a
*reported* log-likelihood). It is also iteration-constant today because `count_w` is
the fixed net Hamming diff. REM requires only a rate and is already complete.

Completion is a **single transform** shared by every consumer (`simulate()`,
`estimate_dynes()`, and each augmenter's setup) so that the walk-driven and
non-walk-driven paths carry identical fid sets; it SHALL NOT be performed inside
`walk_open()`, and it SHALL NOT be applied to the single-process / flavored
estimation path (`estimate_dynam()` / `estimate_rem()` over one
`specification.goldfish`), which keeps rate-only and choice-only specifications
unchanged. `make_specification()` SHALL NOT abort on a half-specified flavored
specification (so it can be built and reach a generative consumer); the
same-flavor-set error is re-imposed by the single-process estimators at estimation
time on an unfilled gap.

#### Scenario: rate-only flavor completed with a uniform choice
- **WHEN** a flavored specification passed to `simulate()` or composed into a
  `estimate_dynes()` join keys a flavor in `rate` but omits it from `choice`
  (e.g. `rate = list(creation ~ x, dissolution ~ y)`, `choice = list(creation ~ z)`)
- **THEN** the missing `dissolution` choice is completed to a uniform (zero-effect)
  choice over the layer's support-legal alternatives (self-loops disallowed), a
  warning names the layer, flavor `dissolution`, the `choice` sub-model, and the
  uniform default, and no extra parameter enters θ.

#### Scenario: missing timed rate is completed with a pinned intercept-only rate
- **WHEN** a flavor is keyed in a **timed** specification's `choice` list but
  omitted from its `rate` list
- **THEN** the missing rate is completed to an intercept-only rate whose per-actor
  intercept is pinned per wave-period to `intercept_w = log(count_w / (T_w · |R_w|))`
  (the `intercept-only-rate-spec` primitive; for a panel flavor `count_w` is the net
  wave Hamming diff and `|R_w|` the consumer-supplied wave-endpoint average of the
  flavor's rate entity), the warning names the added pinned rate, and **no** free
  parameter enters θ (the pin is θ-independent, so it is excluded from the optimizer's
  score and Hessian).

#### Scenario: a completed rate pins correctly on POSIXct/Date-timed data
- **WHEN** a missing rate is completed on a joint specification whose underlying
  event stream carries `POSIXct` or `Date` event times (not plain numeric)
- **THEN** `T_w` (and, for the single-window fallback, the window boundaries it is
  derived from) are coerced to a numeric axis before the pin is computed, so
  `pin_intercept_only_rate()` receives plain numeric `count`, `duration`, and
  `risk_set_size` and the pin succeeds rather than aborting on a non-numeric
  `duration`.

#### Scenario: single-window fallback scoped to the pinned layer
- **WHEN** a completed rate falls back to the single-window boundaries (no explicit
  wave grid supplied) within a joint dataset whose other layers span a different time
  extent than the layer being pinned
- **THEN** the fallback window is derived from the pinned layer's own observed event
  times only, not the range across every layer in the joint dataset.

#### Scenario: a layer with no timed events warns (panel) or aborts (relational)
- **WHEN** a completed rate's pinned layer carries no timed events at all (every row
  is `time = NA` history)
- **THEN** a **panel**-observed layer (or a flavored relational layer on the panel
  fallback) emits a `cli` warning naming the layer and completes to a zero-hazard pin
  (`intercept_w = -Inf`), whereas an **unflavored relational** layer aborts with a
  `cli` error naming the layer, because its `total_time` / `avg_active_entity` scalars
  are zero and no finite pin exists; a per-period empty wave in a multi-wave grid does
  neither and stays a silent `-Inf`.

#### Scenario: an unflavored relational layer's completed rate uses the relational risk-set source
- **WHEN** a missing rate is completed for an **unflavored** process on a layer that
  is **not** a modeled panel layer (a fully event-observed relational layer)
- **THEN** `|R_w|`, `T_w`, and `count_w` are sourced from goldfish's own preprocessing
  scalars (`n_dep_events`, `total_time`, `avg_active_entity`) for that layer, rather
  than from a synthesized wave-endpoint Hamming diff.

#### Scenario: ordered–timed composition is rejected
- **WHEN** a `make_joint_specification()` join pairs a process with a waiting-time
  (timed) rate and another process with an ordered rate
- **THEN** `make_joint_specification()` aborts naming the incompatible regimes, before
  any completion runs.

#### Scenario: single-process estimation is not completed
- **WHEN** a rate-only DyNAM `specification.goldfish` (choice `NULL`) is passed to
  `estimate_dynam()`
- **THEN** no choice is added, the specification estimates as rate-only, and the
  preprocessed output is byte-identical to the pre-change path (frozen baselines
  PASS).

#### Scenario: modeled panel layer missing a whole flavor aborts
- **WHEN** a **modeled panel** focal layer's data carries a flavor that the
  specification keys in neither `rate` nor `choice`
- **THEN** the completion transform aborts (no default) stating a modeled panel
  layer must model all its flavors, naming the unmodeled flavor.

#### Scenario: RE subset modeling stays legal
- **WHEN** an **RE** focal layer models a subset of its data flavors (the others
  keyed nowhere)
- **THEN** no completion or abort occurs for the unmodeled flavors — they update
  network state, and only the keyed flavors are modeled processes.


## ADDED Requirements

### Requirement: Generative consumers gate coordination mechanisms
Generative consumers SHALL gate the coordination mechanisms they can generate:
the walk-driven consumers (`simulate()`, the augmenters, `estimate_dynes()`)
initially accept `mechanism = "conjunctive"` only and SHALL abort on a composed
coordination process with any other mechanism, with a `cli` error naming the
mechanism and stating that generation for it is not yet implemented.
Composition itself SHALL NOT reject any mechanism (viability is
consumer-owned). Lifting a gate SHALL be a consumer-side change keyed on the
mechanism's own thinning construction, with no change to composition.

#### Scenario: conjunctive coordination process is generable
- **WHEN** a joint specification carrying a conjunctive
  `make_specification(model = "DyNAMu")` process reaches a generative consumer
- **THEN** the consumer proceeds, generating coordination events by the
  mutual-choice (conjunctive) construction

#### Scenario: non-conjunctive mechanism aborts at the consumer, not at composition
- **WHEN** a joint specification carrying a `mechanism = "disjunctive"`
  coordination process is composed and then passed to a generative consumer
- **THEN** `make_joint_specification()` returns the composition, and the
  consumer aborts with a `cli` error naming `disjunctive` and the unimplemented
  generation path
