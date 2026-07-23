## ADDED Requirements

### Requirement: make_joint_specification composes process specifications

The package SHALL export `make_joint_specification(...)` accepting two or more
`make_specification()` objects over one shared data object and returning a
multivariate specification that portrays their co-evolution. At least one
**panel-observed layer MUST be referenced** in the composed formulas — as a
process's focal/dependent layer OR as an exogenous covariate read by another
process's effects or support-constraint atoms (per the layer-info observation
metadata); a combination that references no panel-observed layer SHALL abort
explaining that the processes are exactly separable and should be estimated with
the per-process estimators. DyNAM-i processes SHALL be rejected. All processes
MUST share one node set (the multimode change relaxes this). DyNAM (rate, choice,
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

#### Scenario: no panel reference rejected
- **WHEN** no composed process references any panel-observed layer (focal or
  exogenous)
- **THEN** construction aborts with a cli error stating the processes are
  separable and each specification can be estimated on its own.

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
