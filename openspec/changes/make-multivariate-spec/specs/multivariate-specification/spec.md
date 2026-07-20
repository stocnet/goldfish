## ADDED Requirements

### Requirement: make_multivariate_spec composes process specifications

The package SHALL export `make_multivariate_spec(...)` accepting two or more
`make_specification()` objects over one shared data object and returning a
multivariate specification that portrays their co-evolution. At least one
composed process's focal layer MUST be panel-observed (per the layer-info
observation metadata); a combination containing only fully observed processes
SHALL abort explaining that such processes are exactly separable and should be
estimated with the per-process estimators. DyNAM-i processes SHALL be rejected.
All processes MUST share one node set (the multimode change relaxes this).
DyNAM (rate, choice, choice_coordination) and REM processes, timed or ordered,
MAY be freely mixed, flavored or plain.

#### Scenario: panel plus relational processes compose
- **WHEN** `make_multivariate_spec(friendship_spec, calls_spec, data = x)` runs
  with friendship panel-observed (flavored creation/dissolution) and calls a
  fully observed relational-event process
- **THEN** a multivariate specification is returned covering both processes'
  formulas.

#### Scenario: fully observed combination rejected
- **WHEN** every composed process's focal layer is a fully observed event
  stream
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
