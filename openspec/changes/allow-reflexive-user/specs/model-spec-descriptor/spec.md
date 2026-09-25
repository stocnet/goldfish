## MODIFIED Requirements

### Requirement: No component re-derives behavior from model or sub_model

No site SHALL branch on `model` or `sub_model` to decide behavior, outside
the spec constructor and the validators that check a user-supplied value
against its allowed set; every such site SHALL read the descriptor. The
`model` and `sub_model` fields SHALL remain on the spec as provenance — what
the user asked for, reported by print methods and error messages — and SHALL
NOT be used as switches. A guard test SHALL enforce this by failing on any
`model`/`sub_model` equality or membership test outside the documented
exception list.

The same rule SHALL hold for whether a model's risk set excludes the self-dyad.
That fact SHALL be answered by one internal predicate over the model spec —
`is_two_mode` and `allow_reflexive` and nothing else — resolved once when a
process unit is compiled and carried on the objects the consumers already hold.
No consumer SHALL re-derive it by comparing node-set names, and none SHALL
recompute it per event.

#### Scenario: preprocessing reads the descriptor

- **WHEN** preprocessing selects its recipe and the parameters that recipe
  needs
- **THEN** the selection is read from the descriptor, and no branch on
  `model` or `sub_model` participates in it

#### Scenario: the guard test catches a reintroduced re-derivation

- **WHEN** a `sub_model == ...` or `model %in% ...` behavior branch is added
  outside the documented exceptions
- **THEN** the guard test fails and names the offending file and line

#### Scenario: provenance survives

- **WHEN** a fitted model or a spec is printed
- **THEN** the model and sub-model the user requested are still reported

#### Scenario: the self-tie rule has one derivation

- **WHEN** any consumer — the rate-gate fold, the from-scratch gate, the walk
  risk set, the simulation driver, the preprocessing validation, the
  likelihood's diagonal rule — needs to know whether the self-dyad is excluded
- **THEN** it reads the resolved value carried on the unit, engine, walk handle
  or preprocessed object, and no site compares node-set names or recomputes the
  predicate for each event

#### Scenario: a reflexive model resolves once

- **WHEN** a specification with `allow_reflexive = TRUE` is preprocessed and
  then simulated
- **THEN** both read the same resolved value, and the risk sets they build
  agree cell for cell
