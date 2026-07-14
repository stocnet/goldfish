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
how to fix it. There SHALL be no permissive grace period.

#### Scenario: Unknown argument rejected
- **WHEN** a formula term passes an argument not in the term's schema
- **THEN** construction fails with an error naming the argument and listing the
  accepted arguments

#### Scenario: Invalid context rejected
- **WHEN** a term is used with a model/sub_model/mode/direction or inside an
  interaction that its `term_def` does not declare valid
- **THEN** construction fails with an error naming the invalid context

#### Scenario: Strict matrix reproduces current accept/reject behaviour
- **WHEN** the existing test suite of valid and invalid formulas is run through
  the constructor
- **THEN** every formula that estimated before still constructs, and every
  formula that errored before still errors (no newly broken or newly permitted
  formulas)

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
estimation results. Coefficients and log-likelihoods SHALL reproduce the frozen
baselines to 1e-6 on both estimation engines.

#### Scenario: Baselines reproduce through the constructed-term path
- **WHEN** the baseline models are estimated after the parser routes through the
  registry/constructor
- **THEN** `coef()` and `logLik()` match the frozen baselines within 1e-6
