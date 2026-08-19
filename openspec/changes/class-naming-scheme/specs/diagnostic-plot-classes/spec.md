## RENAMED Requirements

- FROM: `### Requirement: examine functions adopt the constructor-named classes`
- TO: `### Requirement: examine functions adopt the package-scoped classes`

## MODIFIED Requirements

### Requirement: goldfish emits plot-ready data, autograph plots
Diagnostic objects SHALL be plot-ready: every object returned by
`test_gof()`, `test_time()`, and the
residual accessors intended for plotting SHALL carry all data required for
its plots (process paths with their time/index axis, per-effect tables,
smoothing inputs and weights), organized in documented components, so that
a plot method can render without recomputation and without importing
goldfish. Every plot-consumed component SHALL be a **tibble** — the object
is either a classed tibble, when one table says everything, or a classed
list whose plot-relevant components are tibbles, when it does not (a
`test_gof()` result carries both a per-effect statistics table and a long
process-path table, which one rectangle cannot hold). A plot method SHALL
therefore never receive a bare matrix, a bare index vector, or a list whose
plot data has to be assembled before it can be mapped. Joining an object's
own component tibbles on their documented key — the term columns — is NOT
assembly in this sense and SHALL be permitted: a classed list carries several
tables precisely because they are of different lengths, and placing a
per-effect quantity onto a longer per-step table is the ordinary use of that
shape. What is forbidden is a component that cannot be mapped until it has
been reshaped, unnested, or recomputed. Both shapes SHALL
carry the metadata contract, so a plot method reads what it is looking at
rather than inferring it from the columns present. goldfish SHALL NOT
contain ggplot2 plotting code for these objects; plot methods live in autograph (branch `feature/goldfish-diag`
off `develop`), dispatching on class only, following autograph's existing
RSiena/ergm/MoNAn pattern. New class names SHALL follow the package-scoped
convention of the class-naming capability: the class is the exported
constructor's own snake_case name with a `_goldfish` suffix, so that a plot
method can tell whose object it received. Where a plot-data component carries
actor identities from a two-mode fit, the labels SHALL be resolved per side
via the model's `node_lookup` (sender-mode and receiver-mode slices), so the
autograph methods never re-derive node identity. Where a plot method offers
several renderings of one object, the selecting argument SHALL be named
`view`, with the composed/complete rendering as the default and one value
per single rendering — for the onset class,
`view = c("both", "path", "accrual")`.

#### Scenario: one argument name selects among an object's renderings
- **WHEN** the plot method for the onset class is called with
  `view = "accrual"`
- **THEN** only the accrual panel is drawn, the default `view = "both"`
  returns the composed two-panel figure, and any other autograph method
  offering several renderings of a goldfish object uses the same argument
  name and default convention.

#### Scenario: test_gof object is self-contained for plotting
- **WHEN** a `test_gof()` result is inspected
- **THEN** it contains the standardized process paths per effect, the
  normalized process-time axis (event-index or information-clock,
  labeled by which clock produced it — the label is load-bearing: the
  pointwise bridge band `sqrt(u(1-u))` is honest only on the
  information clock under non-proportional accrual, so the plot method
  keys the band, or its caveat, on this label), and the per-effect
  statistics table, each as documented components sufficient to draw
  the bridge plot without calling goldfish.

#### Scenario: plot data arrives as tibbles
- **WHEN** any object intended for an autograph plot method is inspected
- **THEN** each component the plot maps over is a tibble, and the object
  carries the metadata attributes, so the method neither reshapes nor
  sniffs columns to learn what it received

#### Scenario: autograph renders without goldfish
- **WHEN** the autograph plot method for the `test_gof_goldfish` class is
  called on a saved fixture object in a session without goldfish attached
- **THEN** it produces the plot from the object's components alone.

#### Scenario: a plot method can tell whose object it received
- **WHEN** an autograph plot method is registered for a goldfish diagnostic
- **THEN** it is registered on a `_goldfish`-suffixed class, so an
  identically-shaped object from another package does not dispatch to it

### Requirement: examine functions adopt the package-scoped classes
`diagnose_outliers()` SHALL return an object of class
`diagnose_outliers_goldfish` and `diagnose_changepoints()` an object of class
`diagnose_changepoints_goldfish` (tibble-based, built by the shared
diagnostic-table constructor), replacing the shared `diagnostic.goldfish`
class. The `outlier` and `cpt` columns
SHALL be logical, and the autograph methods `plot.diagnose_outliers_goldfish` /
`plot.diagnose_changepoints_goldfish` SHALL be updated on `feature/goldfish-diag`
to consume the logical columns (fixing the current `"YES"` string check).
The goldfish `print` method SHALL dispatch on the new classes. This is a
breaking change recorded in NEWS.

#### Scenario: outliers object carries the aligned contract
- **WHEN** `diagnose_outliers(fit)` runs
- **THEN** the result has classes
  `c("diagnose_outliers_goldfish", "tbl_df", "tbl", "data.frame")`,
  a logical `outlier` column, and a `label` column for flagged events.

#### Scenario: autograph plots the aligned outliers object
- **WHEN** the updated `plot.diagnose_outliers_goldfish` receives an object
  with logical `outlier` values containing at least one `TRUE`
- **THEN** it renders the intervalLogL trace with flagged events highlighted
  and labeled.
