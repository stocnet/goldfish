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
`test_*` and `examine_*` families) SHALL all obtain their term strings from
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
