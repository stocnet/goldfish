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
RSiena/ergm/MoNAn pattern. New class names SHALL follow the
constructor-name convention: the class is the exported constructor's own
snake_case name, with no suffix. Where a plot-data component carries
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
- **WHEN** the autograph plot method for the test_gof class is called on a
  saved fixture object in a session without goldfish attached
- **THEN** it produces the plot from the object's components alone.

### Requirement: examine functions adopt the constructor-named classes
`diagnose_outliers()` SHALL return an object of class `diagnose_outliers`
and `diagnose_changepoints()` an object of class `diagnose_changepoints`
(tibble-based, built by the shared diagnostic-table constructor), replacing
the shared `diagnostic.goldfish` class. The `outlier` and `cpt` columns
SHALL be logical, and the autograph methods `plot.diagnose_outliers` /
`plot.diagnose_changepoints` SHALL be updated on `feature/goldfish-diag`
to consume the logical columns (fixing the current `"YES"` string check).
The goldfish `print` method SHALL dispatch on the new classes. This is a
breaking change recorded in NEWS.

#### Scenario: outliers object carries the aligned contract
- **WHEN** `diagnose_outliers(fit)` runs
- **THEN** the result has classes
  `c("diagnose_outliers", "tbl_df", "tbl", "data.frame")`,
  a logical `outlier` column, and a `label` column for flagged events.

#### Scenario: autograph plots the aligned outliers object
- **WHEN** the updated `plot.diagnose_outliers` receives an object with
  logical `outlier` values containing at least one `TRUE`
- **THEN** it renders the intervalLogL trace with flagged events highlighted
  and labeled.

### Requirement: term-wise examine series via effect selection
The functions `diagnose_changepoints()` and `diagnose_outliers()` SHALL
accept an `effect =` argument selecting a single model term by its compact
term string (or integer position), per the effect-selection semantics of
the diagnostic-tests capability (ambiguous family names abort with the
candidate list). With `effect =` set, `diagnose_changepoints()` SHALL run
its changepoint methods on that term's scaled Schoenfeld residual series
(regime shifts in the coefficient) and `diagnose_outliers()` SHALL rank
events by the absolute dfbeta contribution for that term (influence
localization), both requiring only stored primitives. Without `effect =`,
current `intervalLogL` behavior SHALL be unchanged. The returned objects
SHALL record the selected term (compact string) so the autograph plot
methods can label the series, and the documentation SHALL state the
post-selection caveat: changepoints detected on a score series must not be
re-tested with `test_time(method = "periods")` on the same data as
confirmatory evidence.

#### Scenario: term-wise changepoints on the score series
- **WHEN** `diagnose_changepoints(fit, effect = "inertia/net [W]")` runs on
  a fit with stored `event_scores`
- **THEN** the changepoint detection operates on that term's scaled
  Schoenfeld series, the result records the compact term string, and no
  replay is required.

#### Scenario: term-wise outliers rank by influence
- **WHEN** `diagnose_outliers(fit, effect = "inertia/net [W]")` runs
- **THEN** flagged events are those with the largest absolute dfbeta for
  that term, labeled with sender-receiver pairs as in the default mode.

#### Scenario: default series is the per-interval log-likelihood
- **WHEN** `diagnose_changepoints(fit)` is called without `effect`
- **THEN** the series analyzed is the per-interval log-likelihood, over the
  intervals the interval-selection requirement admits.

### Requirement: diagnose_* analyze the dependent intervals by default
`diagnose_outliers()` and `diagnose_changepoints()` SHALL compute their
statistics over the **dependent** intervals by default, and SHALL accept an
argument admitting the right-censored intervals as well. The two kinds of
interval contribute structurally different quantities to the per-interval
log-likelihood — a dependent interval contributes a log density including the
observed alternative's term, a right-censored one contributes only the timing
term — so pooling them makes the median, the interquartile range, the rolling
window and the changepoint segmentation describe the censoring pattern rather
than the fit. On a windowed rate model, where roughly half the intervals are
window closures interleaved one-for-one with the events, changepoint detection
on the pooled series reports a changepoint at nearly every closure.

The returned object SHALL keep **one row per interval** under either setting:
the right-censored rows are present, carry their time and their
log-likelihood contribution, and are simply never flagged. Row count SHALL NOT
depend on the argument, so the object stays joinable with `augment()` and the
plot methods keep the observed time axis. The candidacy of a row SHALL be
read from the `NA` pattern the augmented table already carries on `.resid`,
which marks exactly the intervals that realize no outcome, rather than from a
second definition of the same fact.

The restriction SHALL apply to the per-interval log-likelihood series only,
and SHALL NOT be extended to the score-based diagnostics (`diagnose_onset()`,
`test_gof()`, `test_time()`): a right-censored interval's score row is a
genuine contribution to the gradient, which sums over all intervals, so
excluding it from a cumulative score process would be wrong. The argument
SHALL nevertheless govern both the default and the term-wise `effect =` modes,
so its meaning does not depend on which series was selected.

On the multinomial sub-models, which have no right-censored intervals, the two
settings SHALL agree. The documentation SHALL state the distinction between the
two quantities and SHALL name the misleading result the pooled series produces,
so the non-default setting is chosen knowingly. The change of default SHALL be
recorded in NEWS as a change in results on rate and REM fits, not only as the
class rename it ships beside.

#### Scenario: censored intervals are present but never flagged
- **WHEN** `diagnose_outliers()` runs on a rate fit with right-censored
  intervals under the default setting
- **THEN** the result has one row per interval, every flagged row is a
  dependent event, and the threshold was computed from the dependent
  intervals alone

#### Scenario: admitting the censored intervals changes the answer
- **WHEN** the same fit is diagnosed with the right-censored intervals
  admitted
- **THEN** the row count is unchanged, and the flagged set differs, because
  the statistics are computed over the pooled series

#### Scenario: a windowed fit does not report one changepoint per closure
- **WHEN** `diagnose_changepoints()` runs under the default setting on a rate
  fit whose windowed effect opens a right-censored interval after each event
- **THEN** the detected changepoints are far fewer than the number of window
  closures, and none is flagged on a right-censored interval

#### Scenario: multinomial sub-models are unaffected
- **WHEN** either function runs on a choice-submodel fit under both settings
- **THEN** the two results are identical, that sub-model having no
  right-censored intervals

### Requirement: onset diagnostic diagnose_onset
A function `diagnose_onset()` SHALL be provided that diagnoses the
cold-start (left-censored-history) segment of the event sequence from
stored primitives only, computing: (a) the leave-initial-segment-out
one-step parameter path (`coef(fit) - solve(I) %*% cumsum-of-score-rows`,
for every initial segment length at once, coefficients labeled by compact
term strings); and (b) the information-accrual curve: the cumulative share of per-event
information in **outer-product form from the stored score rows**, which SHALL
be available with no evaluation pass and no replay object. An exact form, from
per-event Fisher contributions, MAY additionally be offered; it SHALL NOT be
required, because it needs a per-event information quantity no primitive
carries, and it SHALL ship together with the same option on the period-wise
test rather than separately — the two are one in-pass accumulation under two
names, so offering one without the other would open the estimation kernels
twice for one quantity. Because the curve is descriptive rather than
inferential, the outer-product form's calibration weakness does not apply to
it: no test is being sized against it. The returned object SHALL be a plot-ready classed object (paths,
accrual series, and a descriptive stabilization summary), with a cli
`print()` reporting the summary and explicitly labeling it descriptive
(no p-values). The documentation SHALL name the remedies — warm-starting
networks by linking pre-observation events, or excluding initial events —
and state why generic changepoint detection on `intervalLogL` does not
reliably surface this phase (short segment vs penalty, plateau at the
null benchmark, zero endogenous score contributions).

#### Scenario: the accrual curve needs no pass and no replay object
- **WHEN** `diagnose_onset()` runs on a fit carrying the score rows but no
  preprocessed statistics
- **THEN** the accrual curve is produced from those rows alone, and requesting
  an exact-information variant that has not shipped aborts naming it rather
  than silently returning the outer-product one

#### Scenario: cold-start fixture shows drift then stabilization
- **WHEN** `diagnose_onset(fit)` runs on a fixture estimated from an event
  stream whose statistics start empty
- **THEN** the parameter paths drift over the initial segment and
  stabilize, the accrual curve is near-flat over that segment, and no
  replay or evaluator pass is triggered in the default mode.

#### Scenario: warm-started fixture shows flat paths
- **WHEN** the same model is estimated after warm-starting the networks
  with pre-observation events and `diagnose_onset(fit)` runs
- **THEN** the parameter paths show no initial drift beyond noise and the
  accrual curve has no initial flat segment.

#### Scenario: onset object is plot-ready
- **WHEN** a saved `diagnose_onset()` result is inspected without goldfish
  attached
- **THEN** the paths (with compact term-string labels), accrual series,
  and stabilization summary are readable as documented components.

#### Scenario: the object supplies every reference the panels need
- **WHEN** a plot method draws the onset panels from a saved result alone
- **THEN** the segment of the path worth showing, the tolerance band around
  each coefficient, the proportional-accrual reference, and which
  coefficients do not move are all read off the object — respectively the
  stabilization summary, the recorded tolerance with the per-coefficient
  standard error, the dependent-event count, and the fixed marker — so none
  of them is recomputed, guessed from the data, or supplied by the caller

### Requirement: cli and stocnet printing standards
Print methods for the diagnostic classes SHALL render via cli semantic
elements consistent with manynet/stocnet console conventions (concise
header naming the object and model, key quantities interpolated as data,
no raw `cat()` markup), and SHALL be snapshot-tested under a pinned cli
context. A flag-column table's print SHALL derive its header count from
the logical flag column and SHALL list only the flagged rows: with zero
flagged rows the header reports zero and no rows are listed, the full
series remaining available in the object itself. The count and the rows
shown SHALL come from the same column, so the header can never disagree
with the listing.

#### Scenario: outliers print summary
- **WHEN** a `diagnose_outliers` object with two flagged events is printed
- **THEN** the cli output names the method, reports the count with correct
  pluralization, and lists exactly the two flagged rows — not the head of
  the full interval series.

#### Scenario: printing a clean result
- **WHEN** a `diagnose_outliers` result whose `outlier` column is all
  `FALSE` is printed
- **THEN** the header reports zero outliers identified and no rows are
  listed, while the object itself still holds one row per interval.

### Requirement: stable schema, and demotion when a defining column is removed
Every diagnostic table SHALL keep the same columns whatever the result:
emptiness SHALL be expressed in content — a flag column that is all
`FALSE`, an `NA` value — never in shape, so consumers read one schema in
every case. Each diagnostic class SHALL declare its defining columns at
construction, and an operation producing a table that lacks any of them
SHALL return a plain tibble rather than an object of the diagnostic class,
implemented once in shared subsetting/reconstruction helpers rather than
per-class guards. The purpose is that no print or plot method can ever
compute a count or a series from a column that is gone: without demotion,
a column subset that drops the flag column keeps the class and the print
header silently reports zero findings while flagged rows exist.

#### Scenario: dropping the flag column demotes the object
- **WHEN** a `diagnose_outliers` result with two flagged events is column-
  subset to `c("time", "sender", ".series")`
- **THEN** the result is a plain tibble, not a `diagnose_outliers` object,
  and printing it does not report an outlier count at all.

#### Scenario: row operations keep the class
- **WHEN** the same result is row-filtered to its flagged rows
- **THEN** the class and metadata are preserved, and the print reports the
  same count as before the filter, every defining column being present.

#### Scenario: a clean result keeps the full schema
- **WHEN** `diagnose_outliers()` flags nothing on a fit
- **THEN** the returned table has the same columns and one row per interval
  exactly as a flagged result would, with `outlier` all `FALSE`.
