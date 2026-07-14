# compact-term-strings Specification

## Purpose
Provide a single internal builder that renders a goldfish model term (effect plus arguments) into a compact canonical string, shared by the printing, naming, and comparison surfaces.

## Requirements

### Requirement: Shared compact term-string builder
The package SHALL provide a single internal builder that converts a goldfish
effect-description matrix (effect name in row names; object column(s); and the
argument columns `ignore_repetitions`, `weighted`, `type`, `window`,
`transformer_fn`, `summarizer_fn`, `joining`, `subType`, `history`, `fixed`)
into a character vector of compact term strings, one per effect row. The
console summary, `tidy()`, and `gather_model_data()` SHALL all obtain their
term strings from this builder rather than assembling them independently.

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

### Requirement: Console layout grammar
In console mode the builder SHALL render each row as `effect/obj·obj2 [args]`:
the effect name, a `/`, object names joined by the middle dot `·`, and the
remaining arguments comma-separated (no space) inside a single trailing `[...]`.
The `/`
and object portion SHALL be omitted when the effect has no object; the `[...]`
block SHALL be omitted when there are no extra arguments. Argument tokens SHALL
be, in order: `W` for `weighted`, `type` (rendered full as `ego`/`alter`), the
window token, the transformer/summarizer token(s), the `subType`/`joining`
token(s), the history token, `IR` for `ignore_rep`, and `Fx` for fixed
parameters.

#### Scenario: Object-less effect omits the slash
- **WHEN** the builder renders an `Intercept` (or a `global` effect) with no object
- **THEN** the result has no `/` and no `·`

#### Scenario: Multiple objects joined with middle dot
- **WHEN** an effect references two objects `friendship` and `advice`
- **THEN** the objects are joined as `friendship·advice`

#### Scenario: Extra arguments collected in one bracket block
- **WHEN** an effect has `type = "ego"` and a fixed parameter
- **THEN** the arguments render inside a single `[...]` as `ego,Fx`
  (comma-separated, no space)

#### Scenario: Weighted token rendered first
- **WHEN** an effect is `weighted` and also has a window argument
- **THEN** the bracket block lists `W` before the window token (e.g. `[W,7d]`)

### Requirement: Object-name trimming
The builder SHALL trim object names: for nodal covariates written as `df$attr`
it SHALL drop everything up to and including `$` and keep `attr`; for network
and covariate object names it SHALL use the shortest unique prefix among the
objects present in the model, starting at 3 characters and expanding up to 6
characters only as needed for uniqueness. Trimming SHALL be computed over the
set of objects in the model so the same object renders consistently across rows.

#### Scenario: Data-frame prefix dropped for nodal covariate
- **WHEN** an object is `actors$age`
- **THEN** the rendered object is `age`

#### Scenario: Network names shortened to unique prefix
- **WHEN** a model uses networks `friendship` and `friendsXX` whose 3-char
  prefixes collide
- **THEN** the prefixes are expanded (up to 6 chars) so the two render distinctly

### Requirement: Curated effect short names applied progressively and uniformly
The builder SHALL maintain a fixed effect short-name map. Short forms SHALL be
applied only when the full-length column would exceed the available width: the
builder SHALL build full strings, and while the longest row exceeds the
available width it SHALL apply the next abbreviation tier to ALL rows together
and re-measure. The tiers in order SHALL be (1) effect short names, (2) window
short forms, (3) object-prefix shortening, (4) transformer/summarizer reduced to
`fn`. The map SHALL include at least: `inertia`→`inrt`, `recip`→`rec`,
`outdeg`→`odeg`, `indeg`→`ideg`, `common_sender`→`cmm_sen`,
`common_receiver`→`cmm_rec`, `mixed_common_sender`→`mix_cmm_sen`,
`mixed_common_receiver`→`mix_cmm_rec`, `mixed_cycle`→`mix_cycle`,
`mixed_trans`→`mix_trans`, `ego_alter_interaction`→`ego_alt`,
`node_trans`→`nd_trans`, `tertius`→`tert`, `tertius_diff`→`tert_diff`,
`degree`→`deg`, `global`→`glob`, `triangle`→`tri`, `alterdeg`→`altdeg`,
`alterpop`→`altpop`, `dyadXdiff`→`dyXdiff`, `dyadXego`→`dyXego`,
`sizeXdiff`→`szXdiff`, `sizeXego`→`szXego`. The `tie` effect SHALL keep its
literal name and `inertia` SHALL NOT be abbreviated to `tie`.

#### Scenario: No abbreviation when the full column fits
- **WHEN** all full-length term strings fit within the available width
- **THEN** effect names are rendered in full (e.g. `inertia`, not `inrt`)

#### Scenario: Column-uniform abbreviation when overflowing
- **WHEN** at least one full-length term exceeds the available width
- **THEN** the effect short-name tier is applied to every row (so all rows show
  the short form, not a mix of full and short)

#### Scenario: inertia and tie remain distinguishable
- **WHEN** a model contains both an `inertia` and a `tie` effect and abbreviation
  is triggered
- **THEN** they render as `inrt` and `tie` respectively, not both as `tie`

### Requirement: Window short forms
The builder SHALL render a string window argument of the form `N unit` (where
`N` has at most 3 characters) as `N` followed by the unit code (`s`, `m` for
minutes, `h`, `d`, `wk`, `mo`, `yr`). For any other window input (numeric,
lubridate object, or `N` longer than 3 characters) the builder SHALL render the
literal token `wdw`.

#### Scenario: String window rendered compactly
- **WHEN** the window argument is `"7 days"`
- **THEN** the window token is `7d`

#### Scenario: Non-string window uses placeholder
- **WHEN** the window argument is a numeric or lubridate value
- **THEN** the window token is `wdw`

### Requirement: Transformer and summarizer short forms
The builder SHALL render a transformer or summarizer function by its name when
the function is named with at most 6 characters, and otherwise (anonymous,
lambda, or name longer than 6 characters) render `fn`. When both
`transformer_fn` and `summarizer_fn` are present the builder SHALL prefix them
with `t:` and `s:` respectively; when only one is present no prefix SHALL be
added.

#### Scenario: Short named function uses its name
- **WHEN** the transformer is a named function `sqrt`
- **THEN** the token is `sqrt`

#### Scenario: Anonymous function uses placeholder
- **WHEN** the summarizer is an anonymous function
- **THEN** the token is `fn`

#### Scenario: Both present are prefixed
- **WHEN** both a transformer `sqrt` and a summarizer `mean` are present
- **THEN** the tokens render as `t:sqrt` and `s:mean`

### Requirement: subType, joining, history, flag short forms
The builder SHALL render `subType` and `joining` using the first 4 characters of
their value, falling back to the shortest length (down to 3) that keeps the
values unique among those present in the model. It SHALL render `history` using
its first 3 characters, `type` in full (`ego`/`alter`), `ignore_rep` as `IR`,
and fixed parameters as `Fx`.

#### Scenario: subType truncated to unique prefix
- **WHEN** an effect has `subType = "proximity"`
- **THEN** the token is the first 4 characters (`prox`), or 3 if 3 already
  disambiguate among present values

#### Scenario: ignore_rep and fixed flags
- **WHEN** an effect has `ignore_rep` set and its parameter is fixed
- **THEN** the bracket block contains `IR` and `Fx`

### Requirement: Width-aware truncation as final fallback
The available term width SHALL be derived from `getOption("width")` (or an
explicit `width` argument) re-evaluated at render time so it tracks dynamic
width changes. If, after all abbreviation tiers, the longest term still exceeds
the available width, the builder SHALL truncate each overlong string to the
available width — trimming the `[...]` tail first, then the object portion — and
append a single ellipsis character `…`, so the printed table never wraps.

#### Scenario: Truncation only after abbreviation
- **WHEN** abbreviation tiers reduce the longest term to within the width
- **THEN** no ellipsis is added

#### Scenario: Ellipsis when still too long
- **WHEN** a term still exceeds the available width after all abbreviation tiers
- **THEN** the string is cut to the available width and ends with `…`

### Requirement: Compact single-table summary print
`print.summary.result.goldfish()` SHALL accept a `compact` argument defaulting
to `TRUE`. When `compact = TRUE` the method SHALL print a single coefficients
table whose row labels are the compact term strings and SHALL NOT print the
separate "Effects details" table. When `compact = FALSE` the method SHALL retain
the prior behavior of printing the "Effects details" table followed by the
coefficients table.

#### Scenario: Default prints one table
- **WHEN** a fitted model whose terms carry extra arguments is summarized and
  printed with defaults
- **THEN** only the coefficients table is printed, with compact term row labels,
  and no "Effects details" table appears

#### Scenario: Opt-out restores the details table
- **WHEN** the same summary is printed with `compact = FALSE`
- **THEN** both the "Effects details" table and the coefficients table are printed

### Requirement: tidy term uses the shared builder
`tidy.result.goldfish()` with `compact = TRUE` SHALL populate the `term` column
from the shared builder's export-mode output; with `compact = FALSE` it SHALL
continue to return the full multi-column effect description.

#### Scenario: Compact tidy term
- **WHEN** `tidy()` is called with `compact = TRUE`
- **THEN** the `term` column contains the compact strings produced by the shared
  builder (sanitized to valid names per the export requirement)

### Requirement: Valid, unique, length-bounded export names
The builder in export mode SHALL replace layout punctuation (`/`, `·`, `[`, `]`,
spaces, `:`) so results are syntactically valid R names, SHALL ensure uniqueness
via numeric suffixes, and SHALL enforce a configurable maximum length via an
argument (default a database-safe value), truncating before applying the
uniqueness suffix so uniqueness is preserved. This export mode is used by
`tidy()`'s `term` column and by `CreateNames()` / `gather_model_data()` for
column and database names.

#### Scenario: Export names are valid R names
- **WHEN** `gather_model_data()` produces column names from effects with windows
  and transformers
- **THEN** every produced name satisfies `make.names()` validity and contains no
  `/`, `·`, `[`, `]`, or spaces

#### Scenario: Export names are unique
- **WHEN** two effects would otherwise produce identical compact names
- **THEN** the produced names are made unique with numeric suffixes

#### Scenario: Maximum length is enforced and configurable
- **WHEN** a caller sets the max-length argument to a value `L`
- **THEN** every produced name has at most `L` characters and the set remains
  unique

### Requirement: Second legend for opaque argument codes
`print.summary.result.goldfish()` with `compact = TRUE` SHALL print a second
legend after the coefficients table and `printCoefmat()`'s `Signif. codes:` line
and before the convergence/information-criteria block. The legend SHALL contain
one line per opaque coded token that actually appears in the rendered table,
drawn from: `W = weighted`, `IR = ignore_rep`, `Fx = fixed`, `wdw = window`,
`fn = user-defined function`, and `t: = transformer, s: = summarizer`. The
`t:`/`s:` line SHALL appear only when both a transformer and a summarizer token
are present. The trigger SHALL be per-token presence (these tokens are emitted
regardless of width), not the width-overflow event. A trailing pointer
`(use compact = FALSE ...)` SHALL be appended only when a lossy or abbreviated
form is present (`wdw`, `fn`, an `…` truncation, or a width-shrunk effect/object
name). The legend prose SHALL be wrapped to the available width. Self-evident or
mnemonic forms (effect short names, object prefixes, `7d`-style windows,
`ego`/`alter`) and the structural punctuation `/`, `·`, `[ ]` SHALL NOT appear in
the legend.

#### Scenario: Present codes are explained
- **WHEN** a compact summary contains terms using weighted and fixed arguments
- **THEN** a second legend after `Signif. codes:` includes `W = weighted` and
  `Fx = fixed` and omits codes not present

#### Scenario: No opaque codes means no second legend
- **WHEN** a compact summary contains no `W`/`IR`/`Fx`/`wdw`/`fn`/`t:`/`s:` tokens
- **THEN** no second legend is printed

#### Scenario: Transformer and summarizer line only when both present
- **WHEN** terms include both a transformer and a summarizer token
- **THEN** the legend includes the `t: = transformer, s: = summarizer` line

#### Scenario: Pointer only when lossy or abbreviated
- **WHEN** a term contains `wdw`, `fn`, an `…` truncation, or a width-shrunk name
- **THEN** the legend appends the `compact = FALSE` pointer; otherwise it does not

#### Scenario: Placement holds without significance stars
- **WHEN** `options(show.signif.stars = FALSE)` suppresses `printCoefmat`'s legend
- **THEN** the second legend still prints after the coefficients table

### Requirement: Minimal-unique short names for coef and vcov
`coef.result.goldfish()` and `vcov.result.goldfish()` SHALL name parameters with
a minimal-unique short form: the curated short effect name as the base, with the
smallest disambiguating suffix appended to all members of any colliding base-name
group (first the object display prefix using the 3→6 shortest-unique-prefix rule,
then argument codes if still colliding) until all names are unique. `vcov()`
dimnames SHALL equal `coef()` names exactly. The flat `print.result.goldfish()`
vector SHALL use these names.

#### Scenario: Unique base name kept short
- **WHEN** an effect's short name is unique among the model's parameters
- **THEN** its `coef()` name is just that short name with no object/arg suffix

#### Scenario: Colliding effects disambiguated symmetrically
- **WHEN** the same effect appears twice on different objects (e.g.
  `indeg + indeg(otherNet)`)
- **THEN** both parameters receive a distinguishing object-prefix suffix and the
  two `coef()` names are unique

#### Scenario: vcov dimnames match coef names
- **WHEN** `coef(mod)` and `vcov(mod)` are obtained from the same fit
- **THEN** the `vcov()` row and column names equal the `coef()` names

### Requirement: Persisted decoder columns hidden from print and export assembly
`GetDetailPrint()` SHALL append dot-prefixed decoder columns to the
effect-description matrix carried by `result$names` (estimate output) and
`gathered$effectDescription` (`gather_model_data()`), including at least
`.effect_short`, `.term_export`, `.coef_name`, and the object display rendering.
Display methods SHALL read these columns instead of recomputing them. Every
consumer that iterates over all columns — including `CreateNames()` and the
`compact = FALSE` "Effects details" table — SHALL skip columns whose names begin
with `.` so the metadata never appears in rendered strings or the details table.

#### Scenario: Decoder columns present on the result
- **WHEN** a model is fitted with `estimate_dynam()`
- **THEN** `result$names` contains dot-prefixed columns including `.coef_name`
  and `.term_export`

#### Scenario: Decoder columns carried through gather
- **WHEN** `gather_model_data()` is called
- **THEN** `gathered$effectDescription` contains the same dot-prefixed decoder
  columns

#### Scenario: Details table hides decoder columns
- **WHEN** a summary is printed with `compact = FALSE`
- **THEN** the "Effects details" table shows only the original display columns and
  none of the dot-prefixed columns

#### Scenario: Export assembly ignores decoder columns
- **WHEN** `CreateNames()` builds export names from the effect description
- **THEN** it does not paste dot-prefixed columns into the produced names

### Requirement: Backward compatibility with pre-upgrade saved objects
Methods that use the decoder columns SHALL access them with a
read-if-present-else-compute pattern and SHALL NOT depend on the dot-prefixed
columns being present. When the columns are absent (e.g. a `result.goldfish`
saved by an earlier package version), the method SHALL compute the needed strings
by calling the builder on the display columns. The builder SHALL produce one term
string per row for any effect-description matrix carrying an `Object` column
(with possibly unknown extra columns), which every `estimate()` output is
guaranteed to have.

#### Scenario: Old object without dot-columns still prints
- **WHEN** `print()`, `coef()`, `vcov()`, or `tidy()` is called on a
  `result.goldfish` whose `$names` has only display columns (no dot-columns)
- **THEN** the call succeeds, computing the strings from the display columns via
  the builder

#### Scenario: Builder handles display-only matrices
- **WHEN** the builder is called on a matrix containing only an `Object` column
  (and possibly unknown extra columns)
- **THEN** it returns one term string per row without error
