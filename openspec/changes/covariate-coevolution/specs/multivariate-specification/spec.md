## MODIFIED Requirements

### Requirement: Composition over a shared mode-map object conforms by mode-set identity

`make_joint_specification()` SHALL compose processes over one shared mode-map
object (`multimode-network-support`). Processes MAY be one-mode or two-mode, and
dependent processes MAY be over distinct mode-pairs. Processes MAY also be
**behavior** (nodal, monadic) processes, per `behavior-coevolution`; a behavior
process's "mode" is the single mode of the actors whose attribute it models — it has
no receiver-side mode. A cross-process read — an
effect argument or support-constraint atom of one process reading another
process's layer — SHALL be admitted only when it **conforms by mode-set
identity**: the shared node space is a whole shared mode. A read that would
bridge a mode **subset** to a union containing it (a directors-only process
coupling to an all-employees process) SHALL abort at construction with a `cli`
error naming the two layers and the offending modes, and noting subset/nested
cross-process coupling as future development. A behavior process's cross-process
read of a network layer (or vice versa) conforms by the same rule: the behavior
process's single mode must be a whole shared mode of the network layer's mode-map.

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

#### Scenario: behavior process conforms to a network layer's whole mode
- **WHEN** a behavior process over `{staff}` is composed with a one-mode `{staff}`
  network process, and the network process's choice formula reads the behavior
  layer via `similarity(smoking)`
- **THEN** a multivariate specification is returned — the read conforms by mode-set
  identity since `{staff}` is a whole shared mode of both processes.

## ADDED Requirements

### Requirement: A behavior process's focal-layer uniqueness follows the existing rule

`make_joint_specification()`'s focal-layer uniqueness check (each layer modeled by at
most one specification) SHALL apply identically to a behavior layer: a nodal attribute
MAY be the focal layer of at most one composed specification, and MAY separately be
read as an exogenous covariate by any number of other specifications in the join.

#### Scenario: duplicate behavior focal layer rejected
- **WHEN** two specifications in the same `make_joint_specification()` call are both
  focal on the `smoking` behavior layer
- **THEN** construction aborts with a cli error naming `smoking` as modeled by more
  than one specification, identically to the existing duplicate-network-layer case.
