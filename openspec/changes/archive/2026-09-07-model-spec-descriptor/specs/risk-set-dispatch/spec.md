# risk-set-dispatch Delta Specification

Note: written against the living spec as merged at v1.9.30.

## MODIFIED Requirements

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
