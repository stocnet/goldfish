## ADDED Requirements

### Requirement: the cumulative score process is read on the event clock
`test_gof()` SHALL build its cumulative score process from score contributions
accumulated between consecutive dependent events, not from the per-interval
rows, on every sub-model that stores right-censored intervals. The two bases
give the same cumulative path at the event boundaries but different statistics,
and the accumulated one is correct rather than merely coarser.

The standardizing constant is an outer-product variance estimate, which is
valid only for uncorrelated increments. The pieces of one event's span are not
uncorrelated: a dependent interval and the right-censored interval its windowed
effect opens are two views of the same tie, so their score contributions are
positively correlated. Summing squared per-interval rows therefore understates
the variance of the cumulative process and inflates the standardized path,
making the test **anti-conservative** on exactly the fits that carry censored
intervals. Score contributions at distinct events are martingale differences, so
the span between events is the unit the reference distribution assumes.

The supremum SHALL be taken over the accumulated path. The documentation SHALL
distinguish this from the separate fact that the `n` in the normalization
cancels, which is a statement about counting and not about which rows are
summed.

On the sub-models that store no right-censored intervals the two bases coincide
and the statistic SHALL be unchanged.

#### Scenario: the statistic is read over spans, not pieces
- **WHEN** `test_gof()` runs on a rate fit whose windowed effect opens a
  right-censored interval after each event
- **THEN** the process has one step per dependent event, and the reported
  statistic is the supremum of that path

#### Scenario: the interval basis understates the variance
- **WHEN** the standardizing constant of such a fit is computed from the
  accumulated rows and from the per-interval rows
- **THEN** the accumulated constant is the larger, the within-span
  contributions being positively correlated

#### Scenario: a fit without censored intervals is unaffected
- **WHEN** `test_gof()` runs on a choice, ordinal or coordination fit
- **THEN** its statistics and p-values are unchanged, that sub-model having no
  right-censored intervals

## MODIFIED Requirements

### Requirement: diagnostics of a flavored fit map over its processes
Every `test_*` and `diagnose_*` function SHALL apply to each process of a
flavored (multi-process) specification exactly as to a single-model fit, since
the competing-flavor likelihood factorizes into independent fits. A function
returning a table SHALL provide a flavored method that row-binds the per-process
results and **appends** `flavor` and `family` columns, in the shape
`margin_table()` established, so a plot method facets on those columns rather
than needing a separate flavored plot method. A function returning a test SHALL
report per process, and SHALL combine across processes only through a declared
omnibus, never by pooling residuals, scores or scaling constants: the processes
have different effect sets and different event counts, so a shared constant
would assert a joint model that was never estimated. `test_parameter()` on a
flavored fit SHALL require no per-process candidate argument, its candidates
being the `offset()` terms each process formula already declares.

The per-process rows SHALL be ordered the same way by every such method, so that
two diagnostics of one fit can be read against each other row for row. The
identity columns SHALL be appended by one shared helper rather than assembled
independently per method.

The `flavor` and `family` columns SHALL NOT be defining columns of any
diagnostic class. Losing a defining column demotes a diagnostic table to a plain
tibble, and the plot methods dispatch on class, so making the identity columns
defining would cost a user their plot method for subsetting columns. The
consequence — that a table which has dropped `flavor` no longer announces that
it row-binds several processes — SHALL be stated in the documentation.

#### Scenario: a flavored diagnostic labels its rows by process
- **WHEN** `diagnose_outliers()` runs on a flavored fit
- **THEN** the result row-binds the per-process tables with `flavor` and
  `family` columns appended, and each process's statistics are computed from
  that process alone

#### Scenario: a flavored test reports per process
- **WHEN** `test_time()` runs on a flavored fit
- **THEN** it reports one result per process, and any omnibus across them is
  the declared combination rather than a pooled series

#### Scenario: flavored score test reads each process's own offsets
- **WHEN** `test_parameter()` runs on a flavored fit whose processes declare
  different `offset()` terms
- **THEN** each process is tested against its own offsets, with no candidate
  argument supplied

#### Scenario: every flavored table agrees on row order
- **WHEN** two table-returning diagnostics are run on the same flavored fit
  whose declared flavor order differs from its internal process order
- **THEN** their `flavor` columns appear in the same order

#### Scenario: a flavored result equals the standalone one
- **WHEN** a diagnostic is run on a flavored fit and on a standalone fit of one
  of its processes
- **THEN** that process's rows in the flavored result equal the standalone
  result, excluding the appended identity columns

#### Scenario: the identity columns do not define the class
- **WHEN** the `flavor` column is dropped from a flavored diagnostic table
- **THEN** the object keeps its diagnostic class and its plot method still
  dispatches
