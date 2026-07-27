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

#### Scenario: exogenous-only panel reference still composes
- **WHEN** no composed process's focal layer is panel-observed, but a
  relational-event process reads a panel-observed layer as an exogenous covariate
  (e.g. `calls ~ ... + tie(friendship)` with friendship panel-observed)
- **THEN** a multivariate specification is returned — the panel layer's latent
  between-wave path couples the RE likelihood, so DyNES applies.

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
support-constraint atom of its formula reads the state of a panel-observed
layer — direct reference only, with no transitivity through intermediate
observed layers. The specification print SHALL mark separable (uncoupled) fids.
The multivariate estimation surface (`estimate_dynes()`) SHALL inform, on a
mixed specification, which fids are separable and could be estimated
separately, and SHALL abort when every fid is separable.

#### Scenario: relational process not reading the panel layer is separable
- **WHEN** the calls process's formulas reference only `phone_calls` and
  `collaboration`, none panel-observed
- **THEN** its fids are marked separable, the print shows it, and
  `estimate_dynes()` informs that the calls formulas can be estimated with the
  per-process estimator.

#### Scenario: constraint atom couples
- **WHEN** the calls process carries `support_constraint = ~ !tie(friendship)`
  with friendship panel-observed
- **THEN** the calls fids are coupled even though no effect references
  friendship.
