# compact-term-strings (delta)

## MODIFIED Requirements

### Requirement: Shared compact term-string builder
The package SHALL provide a single internal builder that converts a goldfish
effect-description matrix (effect name in row names; object column(s); and the
argument columns `ignore_repetitions`, `weighted`, `type`, `window`,
`transformer_fn`, `summarizer_fn`, `joining`, `subType`, `history`, `fixed`)
into a character vector of compact term strings, one per effect row. The
console summary, `tidy()`, `gather_model_data()`, and the diagnostic
effect-selection surfaces (the `effect =` / `effects =` arguments of the
`test_*` and `diagnose_*` families) SHALL all obtain their term strings from
this builder rather than assembling them independently, so the string a user
reads in the printed summary is the string that selects the term in the
diagnostics.

#### Scenario: Single effect with no extra arguments
- **WHEN** the builder renders an effect `inertia` on a single network object
  `friendship` with no other arguments
- **THEN** the result contains the effect and object joined as `inertia/friendship`
  with no trailing bracket block

#### Scenario: Same effect renders identically across surfaces
- **WHEN** the same fitted model is printed with `compact = TRUE`, passed to
  `tidy(compact = TRUE)`, and exported via `gather_model_data()`
- **THEN** each coefficient's term/name is derived from the shared builder (the
  console and tidy forms are identical up to separator/validation differences
  defined for export)

#### Scenario: Printed string selects the term in diagnostics
- **WHEN** a term's compact string as shown by the console summary is passed
  as `effect =` to a diagnostic function
- **THEN** exactly that term is selected, with no re-derivation of the label
  outside the shared builder

## ADDED Requirements

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

`model_terms()` SHALL return the **full** compact strings regardless of console
width, so a term abbreviated in a printed summary remains selectable.

An exported helper `model_terms()` SHALL return the fit's terms as a tibble
carrying the effect-details columns the non-compact summary prints, plus the
term's compact string, its `coef()` label, its export form and its coefficient
index, filterable by a `pattern` argument. On a flavored fit it SHALL row-bind
the per-process tables with `flavor` and `family` columns appended. An error
raised by an unrecognized term SHALL name this helper as the remedy. Its
documentation SHALL show the cases where it earns a call: recovering from an
unrecognized-name abort, a model with too many effects for an error to list,
and terms differing only in an argument.

This helper is fit-scoped and SHALL NOT be confused with the package-scoped
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
  `model_terms()` as the way to search them

#### Scenario: an ambiguous term is offered a resolution
- **WHEN** two terms of a fit render to the same compact string and that string
  is supplied to a term argument
- **THEN** the abort reports the ambiguity and offers a spelling that resolves
  each candidate — its `coef()` label or its coefficient index — rather than
  repeating the one string they share

#### Scenario: an abbreviated summary string still selects
- **WHEN** a printed summary abbreviates a term to fit the console and the user
  reads the full string from `model_terms()` instead
- **THEN** that string selects the term, and the unique labels the same table
  reports select it equally

#### Scenario: model_terms returns a filterable table
- **WHEN** `model_terms(fit, pattern = "indeg")` is called
- **THEN** it returns a tibble of the matching terms carrying the effect-detail
  columns, the compact string, the `coef()` label, the export form and the
  coefficient index

#### Scenario: model_terms labels the processes of a flavored fit
- **WHEN** `model_terms()` is called on a flavored fit
- **THEN** the per-process tables are row-bound with `flavor` and `family`
  columns appended, so a term shared by two processes appears once per process
