# model-recipe-dispatch Delta Specification

Note: written against the living spec as merged at v1.9.30.

## MODIFIED Requirements

### Requirement: S3 goldfishKind class hierarchy
The package SHALL define internal S3 classes for the model variants, and the
class vector SHALL carry only what dispatches. Each spec SHALL be constructed
by the non-exported spec constructor, which SHALL also compute the behavioral
descriptor. The class vector SHALL name the likelihood class and the risk-set
axis; it SHALL NOT encode the `model` and `sub_model` pairing for its own
sake, because preprocessing and the estimation entry point read the descriptor
and the axis rather than a per-variant class. Variants whose likelihood
implementation is identical SHALL share one likelihood class rather than being
given a class each and an alias method.

#### Scenario: Spec class resolves from model and sub_model
- **WHEN** a spec is constructed for a given model and sub-model
- **THEN** it carries the likelihood class its implementation requires and the
  class for its risk-set axis, and its descriptor records the model and
  sub-model as provenance

#### Scenario: two variants sharing an implementation share a class
- **WHEN** specs are constructed for two variants whose likelihood code is the
  same
- **THEN** both carry the same likelihood class, and only one method is
  registered for them

### Requirement: preprocess dispatches per recipe with no model-type branches
Preprocessing SHALL select its recipe and that recipe's parameters by reading
the behavioral descriptor, not by dispatching on a per-variant S3 class and
not by branching on `model` or `sub_model`. The recipe selection SHALL follow
the descriptor's risk-set axis, and the parameters that distinguish a timed
rate from an ordinal sub-model SHALL follow the descriptor's `timing` field.

#### Scenario: recipe selection reads the descriptor
- **WHEN** preprocessing runs for any supported model variant
- **THEN** the recipe and its parameters are determined by the descriptor's
  axis and timing fields, and no per-variant preprocess method participates

#### Scenario: variants with identical preprocessing share one path
- **WHEN** preprocessing runs for a choice sub-model and for a coordination
  sub-model
- **THEN** both take the same recipe path with the same parameters, because
  their descriptors agree on axis and timing

### Requirement: DyNAMi spec participates in dispatch, delegates to existing loop
A DyNAM-i spec SHALL participate in dispatch through the same descriptor as
every other spec, and its difference SHALL be carried by the descriptor's
`input_shape` field rather than by a per-variant class. Its likelihood SHALL
be the same class as the corresponding DyNAM variant, since the
implementations are identical, and no alias method SHALL be registered for it.
Its preprocessing SHALL continue to delegate to the group-interaction loop for
as long as that difference is real.

#### Scenario: a DyNAM-i spec shares the DyNAM likelihood
- **WHEN** the likelihood is computed for a DyNAM-i rate spec
- **THEN** it dispatches to the same method a DyNAM rate spec dispatches to,
  with no alias registered

#### Scenario: the grouped input shape is visible on the descriptor
- **WHEN** a DyNAM-i spec is constructed
- **THEN** its descriptor records the grouped input shape, and preprocessing
  reads that field to reach the group-interaction loop
