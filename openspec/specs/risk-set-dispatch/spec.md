# risk-set-dispatch Specification

## Purpose
TBD - created by archiving change spec-driven-dispatch. Update Purpose after archive.
## Requirements
### Requirement: Risk-set descriptor decided once, at parse time, on the model spec
The risk-set facts SHALL continue to be decided exactly once, at parse time,
on the model spec, and every consumer SHALL read them rather than re-deriving
the family or geometry from model and sub-model strings or from array
dimensionality. These fields SHALL be carried as part of the single behavioral
descriptor the spec constructor builds, not as a separate object beside it, so
that a spec presents one behavioral surface rather than two. The decided-once
and no-re-derivation rules SHALL apply to every field of that descriptor, not
only to the risk-set fields.

#### Scenario: risk-set fields live on the one descriptor
- **WHEN** a model spec is constructed
- **THEN** its risk-set axis, fold target and encoding are fields of the same
  descriptor that carries the timing, likelihood and input-shape facts, and no
  separate risk-set object is attached

#### Scenario: no family re-derivation in estimation glue
- **WHEN** estimation glue needs the risk-set family or geometry
- **THEN** it reads the descriptor, and no site recomputes the family from
  model or sub-model strings or from array dimensionality

### Requirement: Single engine-capability map for constrained estimation
One internal table, derived from the risk-set descriptor, SHALL answer
whether a given engine runs a given (constrained) model family natively or
must abort. The estimation guard and the compiled-interface defensive checks
SHALL both consult this map — no second, independently maintained encoding of
engine × family nativeness SHALL exist — and abort messages listing supported
combinations SHALL be generated from the map's contents.

#### Scenario: guards agree by construction
- **WHEN** a constrained model/engine combination is rejected at the
  estimation guard
- **THEN** the compiled interface cannot be reached with that combination
  through any code path that would judge it differently — both checks read
  the same map.

#### Scenario: abort text reflects the map
- **WHEN** a constraint is used with a family/engine cell the map marks
  unsupported
- **THEN** the abort message enumerates the supported combinations from the
  map, and adding a newly wired cell changes the message without editing the
  guard code.

### Requirement: Estimation consumes maintained availability only
Every support-constraint kind SHALL fold into the maintained availability
objects during preprocessing — including an ego-kind (outer-encoded)
DyNAM-choice constraint — and estimation SHALL consume the constraint
exclusively by maintaining those objects' update buffers. No standalone
per-event mask list, opportunity-list reduction of a constraint, sender-gate
recombination, or mask side-channel SHALL be passed from the estimation
entry code to any engine.

#### Scenario: ego-kind choice constraint arrives folded
- **WHEN** a DyNAM-choice model with an ego-kind (outer) `support_constraint`
  is preprocessed and estimated
- **THEN** the constraint rides the folded availability buffers, the
  estimation entry passes no standalone mask, and the estimate agrees with
  the pre-change standalone-mask path within 1e-10 per event on fixtures.

#### Scenario: constrained dyad models always arrive folded
- **WHEN** any constrained REM, REM-ordered, or coordination model reaches
  estimation
- **THEN** its availability is folded (no unfolded fallback branch exists),
  and the engines read the maintained buffers directly.

#### Scenario: no side-channel arguments
- **WHEN** the estimation argument assembly is inspected after this change
- **THEN** no sender-gate, standalone REM mask, or support-mask argument is
  passed to the engines; the user opportunity list (constraint-free) remains
  the only mask-like input and only where it is not already folded.

