## ADDED Requirements

### Requirement: Term constructor produces a fully-specified constructed term
The package SHALL provide a constructor that resolves a parsed formula term
against its `term_def` and a context (model, sub_model, resolved objects and
their attributes) into a constructed-term object. The constructed term SHALL
carry the encoded recipe and the resolved metadata needed by preprocessing and
the display/export surfaces, so downstream code does not re-resolve the effect.

#### Scenario: Construction yields encoded recipe and metadata
- **WHEN** a formula term is constructed against its `term_def`
- **THEN** the constructed term exposes the update/init recipe with arguments
  already resolved and the decoder metadata (short/abbrev/export/coef names)

### Requirement: Arguments are encoded once at construction
The constructor SHALL resolve argument semantics into concrete recipe inputs at
construction time rather than branching at runtime. In particular `weighted`
SHALL resolve to the matrix transformer to apply (`FALSE` → the binarising
`sign()`-style transform, `TRUE` → `identity()` or the supplied
`transformer_fn`), and `history` SHALL resolve to the selected subroutine. The
`init`/`update` recipe bodies SHALL read the encoded inputs and SHALL NOT
re-decide these arguments per call.

#### Scenario: weighted resolved to a transformer
- **WHEN** a term with `weighted = FALSE` is constructed
- **THEN** the constructed term stores the binarising transformer, and the
  update path applies it without testing `weighted`

#### Scenario: history resolved to a subroutine
- **WHEN** a closure term with a `history` argument is constructed
- **THEN** the constructed term references the selected history subroutine and
  the update path invokes it without branching on the `history` value

### Requirement: Strict validation from the registry declarations
The constructor SHALL raise an error immediately when a formula term supplies an
argument outside its `term_def` argument schema, an argument value outside the
declared allowed set/type, or is used in a context (model, sub_model,
directionality, mode, or interaction) the `term_def` does not declare valid.
Errors SHALL use a consistent shape stating what was given, what is allowed, and
how to fix it. There SHALL be no permissive grace period. Argument names
SHALL be matched exactly against the schema: an unrecognized name SHALL NOT be
discarded, and SHALL NOT be resolved by prefix matching.

#### Scenario: Unknown argument rejected
- **WHEN** a formula term passes an argument not in the term's schema
- **THEN** construction fails with an error naming the argument and listing the
  accepted arguments, and suggesting the closest accepted name when one is
  close enough to be a plausible misspelling

#### Scenario: Retired argument name rejected with its replacement
- **WHEN** a formula term passes an argument name retired by an earlier rename
  (for example `subType`, whose current spelling is `sub_type`)
- **THEN** construction fails with an error naming the current spelling, rather
  than the generic unknown-argument error

#### Scenario: Abbreviated argument name rejected
- **WHEN** a formula term passes an unambiguous prefix of an accepted argument
  name (for example `transformer` for `transformer_fn`)
- **THEN** construction fails rather than resolving the prefix

#### Scenario: Out-of-set value rejected for every declared choice-set argument
- **WHEN** a formula term supplies a value outside an argument's declared
  `allowed` set
- **THEN** construction fails naming the argument and the allowed set,
  whether or not the underlying function's default declares more than one
  choice

#### Scenario: Invalid context rejected
- **WHEN** a term is used with a model/sub_model/mode/direction or inside an
  interaction that its `term_def` does not declare valid
- **THEN** construction fails with an error naming the invalid context

#### Scenario: Strict matrix reproduces current accept/reject behaviour
- **WHEN** the existing test suite of valid and invalid formulas is run through
  the constructor
- **THEN** every formula that estimated before still constructs, except those
  supplying an argument name the parser previously discarded, and every formula
  that errored before still errors (no newly permitted formulas, and no newly
  broken ones outside that set)

#### Scenario: The newly rejected set is bounded by the inventory
- **WHEN** the constructor rejects a formula that estimated before
- **THEN** the rejected argument name appears in the inventory of names the
  parser previously discarded, and a rejection outside that inventory is a
  regression

### Requirement: Endogenous object defaulting and authoritative context
For an endogenous term the constructor SHALL inject the dependent network layer
as its object per the `object_default` rule; for an exogenous term a missing
required object SHALL be an error. The constructor SHALL set the term's
mode (`is_two_mode`) and directionality authoritatively from the resolved
object's attributes.

#### Scenario: Endogenous term needs no object
- **WHEN** `inertia` is used without an explicit object
- **THEN** construction resolves its object to the dependent network layer

#### Scenario: Exogenous term requires an object
- **WHEN** `tie` is used without an explicit object
- **THEN** construction fails with an error requesting the object

#### Scenario: Mode set from object, not inferred with a warning
- **WHEN** a term references a two-mode network
- **THEN** the constructed term's `is_two_mode` is set from the network
  attributes without emitting the prior inference warning

### Requirement: Construction preserves estimation numerics
Routing effect resolution through the registry and constructor SHALL NOT change
estimation results for any formula whose arguments the parser already bound.
Coefficients and log-likelihoods SHALL reproduce the frozen baselines to 1e-6 on
both estimation engines, except where a baseline formula supplied an argument
name the parser discarded, in which case the corrected formula's values SHALL be
derived before the baseline is re-frozen.

#### Scenario: Baselines reproduce through the constructed-term path
- **WHEN** the baseline models are estimated after the parser routes through the
  registry/constructor
- **THEN** `coef()` and `logLik()` match the frozen baselines within 1e-6

#### Scenario: A re-frozen baseline value is derived before it is written
- **WHEN** correcting an argument name the parser previously discarded changes a
  frozen baseline value, because the discarded argument selected a different
  statistic
- **THEN** the new value is derived in closed form and recorded with the change
  before the baseline is re-frozen, and the invariants that survive it are
  stated
