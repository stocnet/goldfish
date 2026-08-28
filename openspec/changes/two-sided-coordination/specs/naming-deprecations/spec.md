# naming-deprecations Delta Specification

## ADDED Requirements

### Requirement: choice_coordination redirects to estimate_dynamu
`estimate_dynam(sub_model = "choice_coordination")` SHALL keep working through
a `lifecycle::deprecate_warn()` redirect that forwards to the
`estimate_dynamu()` conjunctive path and returns its fit, with a single-hop
message naming `estimate_dynamu()` directly (`with =` set, no chained
pointers), a NEWS entry stating the migration and the ≥3.0.0 removal horizon,
and results identical to a direct `estimate_dynamu()` call on the same model.

#### Scenario: redirect warns once and fits
- **WHEN** `estimate_dynam(formula, sub_model = "choice_coordination", data =
  d)` is called
- **THEN** one lifecycle deprecation warning names `estimate_dynamu()` and
  the returned fit equals the direct `estimate_dynamu()` fit to within 1e-6

#### Scenario: message is single-hop
- **WHEN** the deprecation warning text is snapshot-tested
- **THEN** it points at `estimate_dynamu()` itself, never at another
  deprecated surface
