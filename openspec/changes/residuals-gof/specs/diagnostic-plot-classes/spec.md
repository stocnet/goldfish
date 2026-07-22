# diagnostic-plot-classes

The data contract between goldfish diagnostic objects and autograph plot
methods: goldfish emits plot-ready classed data; autograph plots.

## ADDED Requirements

### Requirement: goldfish emits plot-ready data, autograph plots
Diagnostic objects SHALL be plot-ready: every object returned by
`test_gof()`, `test_time()`, and the
residual accessors intended for plotting SHALL carry all data required for
its plots (process paths with their time/index axis, per-effect tables,
smoothing inputs and weights), organized in documented components, so that
a plot method can render without recomputation and without importing
goldfish. goldfish SHALL NOT contain ggplot2 plotting code for these
objects; plot methods live in autograph (branch `feature/goldfish-diag`
off `develop`), dispatching on class only, following autograph's existing
RSiena/ergm/MoNAn pattern. New class names SHALL use the existing
`<thing>.goldfish` suffix convention. Where a plot-data component carries
actor identities from a two-mode fit, the labels SHALL be resolved per side
via the model's `node_lookup` (sender-mode and receiver-mode slices), so the
autograph methods never re-derive node identity.

#### Scenario: test_gof object is self-contained for plotting
- **WHEN** a `test_gof()` result is inspected
- **THEN** it contains the standardized process paths per effect, the
  normalized event-index axis, and the per-effect statistics table, each as
  documented components sufficient to draw the bridge plot without calling
  goldfish.

#### Scenario: autograph renders without goldfish
- **WHEN** the autograph plot method for the test_gof class is called on a
  saved fixture object in a session without goldfish attached
- **THEN** it produces the plot from the object's components alone.

### Requirement: examine functions adopt the autograph classes
`examine_outliers()` SHALL return an object of class `outliers.goldfish`
and `examine_changepoints()` an object of class `changepoints.goldfish`
(each also inheriting `data.frame`), replacing the shared
`diagnostic.goldfish` class. The `outlier` and `cpt` columns SHALL be
logical, and the autograph methods `plot.outliers.goldfish` /
`plot.changepoints.goldfish` SHALL be updated on `feature/goldfish-diag`
to consume the logical columns (fixing the current `"YES"` string check).
The goldfish `print` method SHALL dispatch on the new classes. This is a
breaking change recorded in NEWS.

#### Scenario: outliers object carries the aligned contract
- **WHEN** `examine_outliers(fit)` runs
- **THEN** the result has classes `c("outliers.goldfish", "data.frame")`,
  a logical `outlier` column, and a `label` column for flagged events.

#### Scenario: autograph plots the aligned outliers object
- **WHEN** the updated `plot.outliers.goldfish` receives an object with
  logical `outlier` values containing at least one `TRUE`
- **THEN** it renders the intervalLogL trace with flagged events highlighted
  and labeled.

### Requirement: term-wise examine series via effect selection
The functions `examine_changepoints()` and `examine_outliers()` SHALL
accept an `effect =` argument selecting a single model term by its compact
term string (or integer position), per the effect-selection semantics of
the diagnostic-tests capability (ambiguous family names abort with the
candidate list). With `effect =` set, `examine_changepoints()` SHALL run
its changepoint methods on that term's scaled Schoenfeld residual series
(regime shifts in the coefficient) and `examine_outliers()` SHALL rank
events by the absolute dfbeta contribution for that term (influence
localization), both requiring only stored primitives. Without `effect =`,
current `intervalLogL` behavior SHALL be unchanged. The returned objects
SHALL record the selected term (compact string) so the autograph plot
methods can label the series, and the documentation SHALL state the
post-selection caveat: changepoints detected on a score series must not be
re-tested with `test_time(method = "periods")` on the same data as
confirmatory evidence.

#### Scenario: term-wise changepoints on the score series
- **WHEN** `examine_changepoints(fit, effect = "inertia/net [W]")` runs on
  a fit with stored `event_scores`
- **THEN** the changepoint detection operates on that term's scaled
  Schoenfeld series, the result records the compact term string, and no
  replay is required.

#### Scenario: term-wise outliers rank by influence
- **WHEN** `examine_outliers(fit, effect = "inertia/net [W]")` runs
- **THEN** flagged events are those with the largest absolute dfbeta for
  that term, labeled with sender-receiver pairs as in the default mode.

#### Scenario: default behavior unchanged
- **WHEN** `examine_changepoints(fit)` is called without `effect`
- **THEN** the series analyzed is `intervalLogL`, identical to the
  pre-change behavior.

### Requirement: onset diagnostic examine_onset
A function `examine_onset()` SHALL be provided that diagnoses the
cold-start (left-censored-history) segment of the event sequence from
stored primitives only, computing: (a) the leave-initial-segment-out
one-step parameter path (`coef(fit) - solve(I) %*% cumsum-of-score-rows`,
for every initial segment length at once, coefficients labeled by compact
term strings); and (b) the information-accrual curve (cumulative share of
per-event information, outer-product form from stored scores by default,
exact per-event Fisher contributions via a single evaluator pass on
request). The returned object SHALL be a plot-ready classed object (paths,
accrual series, and a descriptive stabilization summary), with a cli
`print()` reporting the summary and explicitly labeling it descriptive
(no p-values). The documentation SHALL name the remedies — warm-starting
networks by linking pre-observation events, or excluding initial events —
and state why generic changepoint detection on `intervalLogL` does not
reliably surface this phase (short segment vs penalty, plateau at the
null benchmark, zero endogenous score contributions).

#### Scenario: cold-start fixture shows drift then stabilization
- **WHEN** `examine_onset(fit)` runs on a fixture estimated from an event
  stream whose statistics start empty
- **THEN** the parameter paths drift over the initial segment and
  stabilize, the accrual curve is near-flat over that segment, and no
  replay or evaluator pass is triggered in the default mode.

#### Scenario: warm-started fixture shows flat paths
- **WHEN** the same model is estimated after warm-starting the networks
  with pre-observation events and `examine_onset(fit)` runs
- **THEN** the parameter paths show no initial drift beyond noise and the
  accrual curve has no initial flat segment.

#### Scenario: onset object is plot-ready
- **WHEN** a saved `examine_onset()` result is inspected without goldfish
  attached
- **THEN** the paths (with compact term-string labels), accrual series,
  and stabilization summary are readable as documented components.

### Requirement: cli and stocnet printing standards
Print methods for the diagnostic classes SHALL render via cli semantic
elements consistent with manynet/stocnet console conventions (concise
header naming the object and model, key quantities interpolated as data,
no raw `cat()` markup), and SHALL be snapshot-tested under a pinned cli
context.

#### Scenario: outliers print summary
- **WHEN** an `outliers.goldfish` object with two flagged events is printed
- **THEN** the cli output names the method, reports the count with correct
  pluralization, and lists the flagged sender-receiver pairs.
