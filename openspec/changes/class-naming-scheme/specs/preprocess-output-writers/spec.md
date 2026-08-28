## MODIFIED Requirements

### Requirement: Writer strategy contract
Preprocessing recipes SHALL emit their output exclusively through a writer object exposing three hooks: `init(spec, dims)` (called once before the event loop with the model spec and problem dimensions), `write_event(event_updates, event_info)` (called once per stored event), and `finalize()` (called once after the loop, returning the writer's output). Writer constructors SHALL live in `R/preprocess_writers.R` and SHALL NOT be exported. `compute_stats(formula, data, model, sub_model, output = c("default", "gather", "db"), ...)` SHALL select the writer; `output` defaults to `"default"`. No recipe method SHALL branch on the output format inside its event loop beyond calling the writer hooks.

#### Scenario: Default output selected implicitly
- **WHEN** `compute_stats(formula, data, model = "DyNAM", sub_model = "rate")` is called without `output`
- **THEN** the returned object is a `goldfishPrep` produced by the default flat-buffer writer

#### Scenario: Invalid output value rejected
- **WHEN** `compute_stats(..., output = "parquet")` is called
- **THEN** an informative error lists the valid output values

### Requirement: Default writer produces the flat-buffer preprocessing object
The default writer SHALL produce the `goldfishPrep` object specified by the flat-preprocess-output capability (`stat_mat_update`, `stat_mat_pointer`, unified event fields, engine-native `initialStats`, intercept scalars, presence C-format). Both estimation engines (R `default`, C++ `default_c`) SHALL consume this output without restructuring.

#### Scenario: Default writer output is estimation-ready
- **WHEN** the result of `compute_stats(..., output = "default")` is passed to `estimate_dynam()` as precomputed preprocessing
- **THEN** estimation completes and coefficients match the non-precomputed path to within 1e-6

### Requirement: The gather expansion honors the availability encoding
The gather expansion SHALL read the `active_dyad_encoding` of the object it
expands and interpret availability accordingly — the `point` encoding as a
sender-by-receiver mask, the `alter` encoding as a receiver vector — matching
what the estimation path does with the same object. It SHALL NOT assume the
`alter` encoding. Folding a `support_constraint` upgrades availability to the
`point` encoding, so any constrained object reaching the expansion carries it.

#### Scenario: a point-encoded object expands correctly
- **WHEN** a stored `goldfishPrep` whose availability is point-encoded
  (because a constraint folded into it) is rendered to a gather stack
- **THEN** the rows enumerate that event's allowed candidates, rather than
  indexing past the statistics matrix
