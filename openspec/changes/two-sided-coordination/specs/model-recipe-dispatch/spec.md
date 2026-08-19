# model-recipe-dispatch Delta Specification

## ADDED Requirements

### Requirement: dynamu recipes participate in dispatch
`estimate_dynamu()` SHALL construct typed model specs that join the existing
S3 recipe dispatch (preprocessing and estimation selected by spec class, one
dispatch per stage boundary), one spec class per mechanism family as needed,
with no mechanism-string branching inside per-event loops. The
`choice_coordination` recipe SHALL remain reachable only through the
deprecation redirect on `estimate_dynam()` while it exists.

#### Scenario: dynamu produces a typed spec
- **WHEN** `estimate_dynamu(events ~ inertia, mechanism = "forcing", data = d)`
  is called
- **THEN** the internally constructed spec dispatches through `preprocess()`
  and `estimate_int()` by class, and no mechanism string is a branch key
  inside an event loop

#### Scenario: redirect reuses the same recipe machinery
- **WHEN** `estimate_dynam(formula, sub_model = "choice_coordination", data =
  d)` runs through the deprecation redirect
- **THEN** the fit is produced by the same dynamu conjunctive recipe path, not
  a parallel legacy implementation
