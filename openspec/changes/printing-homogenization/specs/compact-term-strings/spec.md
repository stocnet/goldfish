## MODIFIED Requirements

### Requirement: Compact single-table summary print
`print.goldfishSummFit()` SHALL accept a `compact` argument defaulting
to `TRUE`. When `compact = TRUE` the method SHALL print a single coefficients
table whose row labels are the compact term strings and SHALL NOT print the
separate "Effects details" table. When `compact = FALSE` the method SHALL retain
the prior behavior of printing the "Effects details" table followed by the
coefficients table. The method SHALL render its header, call, convergence
report, score and step norms, parameter counts, log-likelihood, information
criteria, provenance (`model`, `sub_model`) and legends with cli semantic
elements and interpolated data, and SHALL print the coefficients table
through `stats::printCoefmat()`; the width used to fit compact term strings
SHALL be `cli::console_width()`. `print.goldfishFit()` SHALL likewise render
its header and call via cli and its coefficient vector via `print.default()`.
Both SHALL be snapshot-tested under the pinned cli context.

#### Scenario: Default prints one table
- **WHEN** a fitted model whose terms carry extra arguments is summarized and
  printed with defaults
- **THEN** only the coefficients table is printed, with compact term row labels,
  and no "Effects details" table appears

#### Scenario: Opt-out restores the details table
- **WHEN** the same summary is printed with `compact = FALSE`
- **THEN** both the "Effects details" table and the coefficients table are printed

#### Scenario: prose is cli, the table is printCoefmat
- **WHEN** a summary is printed under the pinned cli context
- **THEN** the output opens with a rule naming `goldfishSummFit`, the
  convergence and likelihood lines carry interpolated values, and the
  coefficient rows are a `printCoefmat()` table with significance stars

### Requirement: One term vocabulary, and an exported way to look it up
Every surface that takes a model term from a user SHALL resolve it through one
shared matcher accepting the compact term string, the export form, the
`coef()` label and an integer position, and SHALL render candidates in error
messages in the **compact** form — the one the printed summary shows. This
covers `initial_parameters`, the `effect =` / `effects =` arguments of the
`diagnose_*` and `test_*` families, and any later surface naming a term. The
`coef()` labels SHALL keep their minimal-unique abbreviations: they exist to
print and subset a vector, and widening what is *accepted* elsewhere does not
require changing what they *are*.

Accepting more than one spelling is a **correctness** requirement, not a
convenience: the compact string is neither guaranteed unique nor guaranteed to
be what the user read. The printed summary abbreviates it to the console
width, so a narrow console can render two distinct terms identically, and a
term argument distinguished only by a non-symbol function collapses to a shared
token. The matcher SHALL therefore accept the `coef()` label and the
coefficient index, which are unique by construction, and an ambiguity abort
SHALL **offer a unique spelling for each candidate** rather than only listing
the colliding compact strings — which, being colliding, all read the same. An
ambiguous term with no offered resolution is a dead end, and the abort exists
to prevent exactly that.

`coef_layout()` SHALL return the **full** compact strings regardless of
console width, so a term abbreviated in a printed summary remains selectable.

The one exported lookup is `coef_layout()`: it SHALL return the object's
coefficient slots as a tibble carrying the process identity (`fid`, `process`,
`sub_model`, `flavor`), the effect-details columns the non-compact summary
prints, the term's compact string, its `coef()` label (`name`), its flat
`coef()` name (`coef_name`), its export form and its coefficient `index`,
plus `fixed`, `value` and `se` where the object carries them, filterable by a
`pattern` argument matched case-insensitively against every spelling and
detail column. On a multi-process object it SHALL row-bind the per-process
blocks in fid order. An error raised by an unrecognized term SHALL name this
helper as the remedy. Its documentation SHALL show the cases where it earns a
call: recovering from an unrecognized-name abort, a model with too many
effects for an error to list, terms differing only in an argument, and
authoring `set_parameters()`. There SHALL be no separate `model_terms()`
helper: the term table and the coefficient layout are one table.

This helper is object-scoped and SHALL NOT be confused with the package-scoped
effect registry lookup, which answers which effects goldfish provides rather
than which terms a fit has; the two meet only at the shared compact-string
builder.

#### Scenario: the summary string selects the term everywhere
- **WHEN** a term's compact string as shown by the printed summary is passed to
  `initial_parameters`, to `offset()` reporting, or to a `diagnose_*` /
  `test_*` effect argument
- **THEN** exactly that term is selected on every one of them, and the
  `coef()` label and export form select it equally

#### Scenario: an unrecognized term names the helper
- **WHEN** a term a fit does not have is supplied to any of those surfaces
- **THEN** the abort renders the available terms as compact strings and names
  `coef_layout()` as the way to search them

#### Scenario: an ambiguous term is offered a resolution
- **WHEN** two terms of a fit render to the same compact string and that string
  is supplied to a term argument
- **THEN** the abort reports the ambiguity and offers a spelling that resolves
  each candidate — its `coef()` label or its coefficient index — rather than
  repeating the one string they share

#### Scenario: an abbreviated summary string still selects
- **WHEN** a printed summary abbreviates a term to fit the console and the user
  reads the full string from `coef_layout()` instead
- **THEN** that string selects the term, and the unique labels the same table
  reports select it equally

#### Scenario: coef_layout returns a filterable table
- **WHEN** `coef_layout(fit, pattern = "indeg")` is called
- **THEN** it returns a tibble of the matching slots carrying the process
  identity, the effect-detail columns, the compact string, the `coef()`
  label, the flat name, the export form, the coefficient index, and the
  fit's value and standard error

#### Scenario: coef_layout labels the processes of a flavored fit
- **WHEN** `coef_layout()` is called on a flavored fit
- **THEN** the per-process blocks are row-bound in fid order with `fid`,
  `process`, `flavor` and `sub_model` columns, so a term shared by two
  processes appears once per process under a distinct `coef_name`
