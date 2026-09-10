## MODIFIED Requirements

### Requirement: preprocess dispatches per recipe with no model-type branches
Preprocessing SHALL select its recipe and that recipe's parameters by reading
the behavioral descriptor, not by dispatching on a per-variant S3 class and
not by branching on `model` or `sub_model`. For every specification whose
descriptor `input_shape` is `standard`, the recipe SHALL be the merged
single-clock walk; the descriptor's risk-set axis SHALL determine the
statistics kernel shape of that specification's engine inside the walk (a
sender-indexed matrix or a dyad-indexed array), not the choice of a loop, and
the parameters that distinguish a timed rate from an ordinal sub-model SHALL
follow the descriptor's `timing` field. The grouped `input_shape` SHALL keep
its own delegate until it is converted.

#### Scenario: recipe selection reads the descriptor
- **WHEN** preprocessing runs for any supported model variant
- **THEN** the recipe and its parameters are determined by the descriptor's
  axis and timing fields, and no per-variant preprocess method participates

#### Scenario: variants with identical preprocessing share one path
- **WHEN** preprocessing runs for a choice sub-model and for a coordination
  sub-model
- **THEN** both take the same recipe path with the same parameters, because
  their descriptors agree on axis and timing

#### Scenario: the axis shapes the engine, not the loop
- **WHEN** preprocessing runs for a rate sub-model and for a choice sub-model
  of the same model
- **THEN** both run through the merged walk, the rate engine keeps a
  sender-indexed statistics kernel and the choice engine a dyad-indexed one,
  and no separate per-axis loop function exists in the package
