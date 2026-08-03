## ADDED Requirements

### Requirement: term-wise diagnostics stay readable at many terms
A per-term diagnostic SHALL remain usable on a model with many terms, where one
panel per term is unreadable and no interactive selection step is available.
Fits are routinely produced in batch on a cluster, so every route SHALL work
from a script with no human between the fit and the figure: nothing SHALL
prompt, nothing SHALL depend on a device being interactive, and no route SHALL
require the analyst to have already seen a plot in order to choose what to plot.

Three routes SHALL be available and SHALL compose.

**Screening.** A per-term table carrying the statistic each diagnostic computed
SHALL be part of the returned object, ordered so the terms most worth examining
can be taken from the front without inspecting a figure. A model with many terms
is legible as a table long after it has stopped being legible as a grid of
panels, and this is the route that survives having no screen at all.

**Selection.** The existing effect selection by compact term string SHALL
continue to narrow a diagnostic to named terms, including a bare effect name
selecting every term of that effect.

**Pagination.** A plot method rendering one panel per term SHALL accept a page
to render and SHALL expose the number of pages the current layout implies, so a
script can render every page in a loop and write each to its own file. The page
count SHALL be derivable without rendering, and requesting a page beyond the
last SHALL abort naming how many there are.

Where a plot method reduces the terms it draws — by ranking and keeping the
first few — it SHALL report how many it omitted rather than silently drawing a
subset. A figure that shows part of the model while looking like the whole of it
is the failure this requirement exists to prevent.

#### Scenario: the ranked table comes with the object
- **WHEN** a per-term diagnostic is computed on a model with many terms
- **THEN** its returned table carries each term's statistic and can be ordered
  by it without rendering anything

#### Scenario: every page renders from a script
- **WHEN** a per-term plot is rendered for each page in turn in a
  non-interactive session
- **THEN** each call returns a figure, together they cover every term exactly
  once, and nothing prompts

#### Scenario: the page count is known before rendering
- **WHEN** the number of pages is requested for a given layout
- **THEN** it is returned without producing a figure, and requesting a later
  page aborts naming the number available

#### Scenario: a reduced figure says what it dropped
- **WHEN** a plot method draws only the highest-ranked terms of a model with
  more terms than it will draw
- **THEN** it reports how many terms were not drawn

## MODIFIED Requirements

### Requirement: diagnose_* analyze the dependent intervals by default
`diagnose_outliers()` and `diagnose_changepoints()` SHALL compute their
statistics over a series carrying **one value per dependent event**, obtained
from the residual accumulation between consecutive events, and SHALL return one
row per dependent event. Every returned row SHALL be a candidate for flagging,
so the object needs no second definition of candidacy and no `NA` pattern to
read it from.

The two kinds of interval contribute structurally different quantities to the
per-interval log-likelihood — a dependent interval contributes a log density
including the observed alternative's term, a right-censored one contributes only
the timing term — so a series pooling them describes the censoring pattern
rather than the fit. On a windowed rate model, where roughly half the intervals
are window closures interleaved one-for-one with the events, changepoint
detection on the pooled series reports a changepoint at nearly every closure.
Accumulating the intervals of a span into the event that ends it removes that
alternation at its source, and does so without discarding the censored
intervals' contribution: their likelihood and score mass is attributed to the
span rather than dropped.

The `include_censored` argument SHALL therefore be deprecated. It exists to
suppress an alternation the accumulated series does not have, and its restrictive
setting discards contributions the accumulation attributes. It SHALL warn on use
and be ignored, following the package's deprecation practice, since it ships in a
released development line. The deprecation SHALL be recorded in NEWS together
with the change in results it implies on rate and REM fits.

The accumulation SHALL be the same operation the residual methods perform, never
a second implementation, so a series read here and a residual read directly
cannot disagree. It SHALL apply equally to the default log-likelihood series and
to the term-wise `effect =` series, so the unit of analysis does not depend on
which series was selected.

On the multinomial sub-models, which have no right-censored intervals, the
accumulated series SHALL equal the per-interval one.

#### Scenario: the series has one value per event
- **WHEN** `diagnose_outliers()` runs on a rate fit with right-censored
  intervals
- **THEN** the result has one row per dependent event, and every row is a
  candidate for flagging

#### Scenario: a windowed fit does not report one changepoint per closure
- **WHEN** `diagnose_changepoints()` runs on a rate fit whose windowed effect
  opens a right-censored interval after each event
- **THEN** the detected changepoints are far fewer than the number of window
  closures

#### Scenario: the retired argument warns and does not change the answer
- **WHEN** either function is called with `include_censored` supplied
- **THEN** it warns that the argument is deprecated, and the result is the same
  as without it

#### Scenario: the censored contribution is attributed, not discarded
- **WHEN** the accumulated series of a windowed rate fit is compared with the
  series restricted to the dependent intervals
- **THEN** the accumulated one carries the right-censored intervals'
  contribution and the restricted one does not

#### Scenario: multinomial sub-models are unaffected
- **WHEN** either function runs on a choice-submodel fit
- **THEN** the series equals the per-interval one, that sub-model having no
  right-censored intervals
