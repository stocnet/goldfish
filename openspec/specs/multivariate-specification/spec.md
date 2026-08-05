# multivariate-specification Specification

## Purpose
The multivariate specification surface for co-evolving processes: the exported
`make_joint_specification(...)` constructor that composes two or more
`make_specification()` objects over one shared mode-map object into a distinct
`joint_specification.goldfish` class, the mode-set-identity conformance rule for
cross-process reads, the process_map fid vocabulary extended across processes,
direct-reference coupling detection surfaced to consumers, focal-layer
uniqueness, event-stream estimator rejection of the joint object, and the
single-transform generative-readiness completion that fills half-specified
flavors with zero-free-parameter defaults so walk-driven and non-walk-driven
paths carry identical fid sets. Created by archiving change
make-multivariate-spec.
## Requirements
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
node-space conformance requirement below). DyNAM (rate, choice,
choice_coordination) and REM processes, timed or ordered, MAY be freely mixed,
flavored or plain.

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

### Requirement: Composition over a shared mode-map object conforms by mode-set identity

`make_joint_specification()` SHALL compose processes over one shared mode-map
object (`multimode-network-support`). Processes MAY be one-mode or two-mode, and
dependent processes MAY be over distinct mode-pairs. A cross-process read — an
effect argument or support-constraint atom of one process reading another
process's layer — SHALL be admitted only when it **conforms by mode-set
identity**: the shared node space is a whole shared mode. A read that would
bridge a mode **subset** to a union containing it (a directors-only process
coupling to an all-employees process) SHALL abort at construction with a `cli`
error naming the two layers and the offending modes, and noting subset/nested
cross-process coupling as future development.

#### Scenario: whole-shared-mode multilevel composition succeeds
- **WHEN** `make_joint_specification(advice_spec, nominations_spec, data = x)` is
  called with advice two-mode `{staff}×{director}` and nominations two-mode
  `{director}×{project}`, and nominations reads `indeg(advice)` on the director
  side
- **THEN** a multivariate specification is returned — the director-indexed read
  conforms by mode-set identity — with the advice and nominations fids on their
  own mode-pair blocks.

#### Scenario: subset/nested cross-process read rejected
- **WHEN** an all-employees process over `{staff, director}` and a directors-only
  process over `{director}` are composed and one reads the other's layer across
  the `{director}` ⊂ `{staff, director}` boundary
- **THEN** construction aborts naming the two layers and the offending modes, and
  states subset/nested cross-process coupling is future development.

### Requirement: The process_map extends across processes

The multivariate specification SHALL carry the integer-fid `process_map`
vocabulary unchanged in kind from the flavored single-layer case: one row per
likelihood-producing formula (a K-flavored rate+choice process contributes 2K
fids, a plain rate+choice process 2), columns `fid`, `layer`, `flavor`,
`family`, `stat_block`, `has_intercept`, `constraint_id`, plus a `coupled`
logical. The fid SHALL be the canonical identity for consumers, compiled
constraints (`constraint_id` shared per `(layer, flavor)`), preprocessed
outputs, and results; human-readable labels SHALL be rendered from the table
and never parsed back.

#### Scenario: flavored plus plain yields 2K + 2 fids
- **WHEN** a K=2 flavored rate+choice process is composed with a plain
  rate+choice process
- **THEN** the process_map has six rows, and each flavor's rate and choice rows
  share one `constraint_id`.

### Requirement: Coupling is detected by direct reference and surfaced

A fid SHALL be marked coupled if and only if any effect argument or
support-constraint atom of its formula reads the state of a **modeled** panel layer
(a panel layer that is itself a process of the specification, whose between-wave path
is latent) — direct reference only, with no transitivity through intermediate
observed layers. Reading a panel layer that appears only as an exogenous covariate (a
static step-covariate) SHALL NOT couple. The specification print SHALL mark separable
(uncoupled) fids. The multivariate estimation surface (`estimate_dynes()`) SHALL
inform, on a mixed specification, which fids are separable and could be estimated
separately, and SHALL abort when every fid is separable (equivalently, no panel layer
is a modeled process), naming `estimate_dynam()`.

#### Scenario: relational process not reading the panel layer is separable
- **WHEN** the calls process's formulas reference only `phone_calls` and
  `collaboration`, none panel-observed
- **THEN** its fids are marked separable, the print shows it, and
  `estimate_dynes()` informs that the calls formulas can be estimated with the
  per-process estimator.

#### Scenario: constraint atom couples to a modeled panel layer
- **WHEN** the calls process carries `support_constraint = ~ !tie(friendship)`
  with friendship a modeled panel process of the specification
- **THEN** the calls fids are coupled even though no effect references
  friendship.

#### Scenario: reading a static exogenous panel covariate does not couple
- **WHEN** the calls process reads a panel-observed layer that appears only as an
  exogenous covariate (no process models it) — e.g. `calls ~ ... + tie(friendship)`
  with friendship modeled by no composed process
- **THEN** the calls fids are marked separable (the covariate is a static
  step-covariate, so nothing latent is read), and with no modeled panel process
  `estimate_dynes()` aborts pointing to `estimate_dynam()`.

### Requirement: Each joined specification models a distinct focal layer

`make_joint_specification()` SHALL abort when two or more of the joined
specifications share the same focal/dependent layer — each layer MAY be modeled by
at most one specification in the join. Uniqueness is scoped to the **focal role
only**: a layer MAY be read as an exogenous covariate by any number of other
specifications (the coupling that makes joining meaningful), so a covariate reference
SHALL NOT count as the layer "appearing" for this check. All flavors of one layer
SHALL be carried by a single specification (the flavor-keyed rate/choice lists); a
layer's flavors SHALL NOT be split across two joined specifications. The abort message
SHALL name the duplicated layer.

#### Scenario: duplicate focal layer rejected
- **WHEN** `make_joint_specification(friendship_spec_a, friendship_spec_b, data = x)`
  is called with both specifications focal on `friendship`
- **THEN** construction aborts with a cli error naming `friendship` as modeled by more
  than one specification.

#### Scenario: same layer as covariate in several specifications is allowed
- **WHEN** `friendship` is the focal layer of one specification and is read as an
  exogenous covariate by both the `calls` and `emails` specifications in the same join
- **THEN** construction succeeds — covariate reuse does not violate focal uniqueness.

#### Scenario: split flavors rejected
- **WHEN** the join passes one specification modeling only `friendship`'s creation
  flavor and another modeling only its dissolution flavor
- **THEN** construction aborts: a layer's flavors must be carried by a single
  specification (both share the focal layer `friendship`).

### Requirement: Event-stream estimators reject a joint specification object

`estimate_dynam()` and `estimate_rem()` SHALL abort when handed a
`make_joint_specification()` object, directing the user to `estimate_dynes()`; the
joint object is a distinct class that SHALL NOT be dispatched through the
single-specification estimation path. These estimators SHALL likewise abort on a
single specification whose focal/dependent layer is panel-observed (the PE-dependent
case; the existing focal-not-panel guard, its message retargeted to `estimate_dynes()`
by the `single-data-object` capability). `estimate_dynami()` SHALL abort on a
PE-focal specification for the same reason; its rejection of a joint object is a
recorded future development (DyNAM-i is under development and cannot appear in a joint
specification).

#### Scenario: joint object rejected by event-stream estimator
- **WHEN** a `make_joint_specification()` object is passed to `estimate_dynam()` or
  `estimate_rem()`
- **THEN** it aborts with a cli error directing the user to `estimate_dynes()`, without
  attempting single-specification estimation.

#### Scenario: PE-dependent specification rejected by event-stream estimator
- **WHEN** a single specification whose focal layer is panel-observed is passed to
  `estimate_dynam()`, `estimate_rem()`, or `estimate_dynami()`
- **THEN** it aborts pointing to `estimate_dynes()` as the estimator for
  panel-dependent processes.

### Requirement: Generative-readiness completion fills half-specified flavors

A specification used to **generate** events (drive the walk handle via
`simulate()` or an augmenter) or to be estimated **jointly** (`estimate_dynes()`)
SHALL be made *generatively complete* — every modeled DyNAM flavor (the choice /
choice_coordination sub-model family) carrying both a rate and a choice — by a
completion transform run at the consumer's entry. A flavor **keyed in one
sub-model list and omitted from the other** SHALL have the missing sub-model filled
with its zero-information default and SHALL emit a `cli` **warning** naming
the layer, flavor, sub-model, and the default applied; the warning SHALL fire at
**each consumer entry** (it is NOT suppressed when the same half-specified spec is
routed through a second consumer). **Every completion default is
zero-free-parameter** — completion SHALL NOT add an estimated coefficient to θ. The
defaults SHALL be: a **uniform choice over the support-legal alternatives** (no
effects) for a missing choice; a **uniform draw on both sides** for a missing
`choice_coordination`; a **uniform ordered rate** (no effects) for a missing rate in
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

### Requirement: The walk handle asserts completeness; completed fids are marked

`walk_open()` SHALL validate that its specification is generatively complete and
abort with a `cli` error pointing to `simulate()` / `estimate_dynes()` when it is
not; it SHALL NOT perform completion itself. `walk_open()` remains an internal
developer substrate (not user-exported) in this change. Fids added by completion
SHALL be marked in the `process_map` (a `completed` logical column beside
`coupled`) and rendered as such in the specification / result print, so the
auto-supplied sub-models are visible beyond the one-time construction warning.

#### Scenario: walk_open aborts on an incomplete spec
- **WHEN** `walk_open()` is called (directly, or on a path that skipped the
  completion transform) with a half-specified flavored specification
- **THEN** it aborts naming the incomplete flavor and directing the caller to
  `simulate()` / `estimate_dynes()`, rather than opening a walk with a mismatched
  fid set.

#### Scenario: completed fids are marked in the process_map and print
- **WHEN** a specification whose flavor was completed with a default sub-model is
  printed (or carried into a fitted result)
- **THEN** the `process_map` row for the added fid has `completed = TRUE` and the
  print marks it as an auto-supplied default.
