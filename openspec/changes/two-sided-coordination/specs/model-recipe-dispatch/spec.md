# model-recipe-dispatch Delta Specification

## ADDED Requirements

### Requirement: dynamu recipes participate in dispatch
`estimate_dynamu()` SHALL construct typed model specs that join the existing
dispatch, with no mechanism-string branching inside per-event loops. Since a
spec now carries one behavioral descriptor, that joining SHALL take the form
each stage actually uses: preprocessing SHALL reach its recipe by reading the
descriptor's risk-set axis and timing, not by a per-variant class; the
estimation entry point SHALL dispatch on the risk-set axis class; and the
likelihood SHALL dispatch on a likelihood class. A mechanism whose likelihood
implementation genuinely differs SHALL carry its own likelihood class, and
mechanisms sharing an implementation SHALL share one, with no method
registered as an alias of another. The `choice_coordination` recipe SHALL
remain reachable only through the deprecation redirect on `estimate_dynam()`
while it exists.

#### Scenario: dynamu produces a typed spec
- **WHEN** `estimate_dynamu(events ~ inertia, mechanism = "forcing", data = d)`
  is called
- **THEN** the internally constructed spec carries a descriptor from which
  preprocessing selects its recipe, dispatches its likelihood on a likelihood
  class, and no mechanism string is a branch key inside an event loop

#### Scenario: a mechanism earns a class only by differing
- **WHEN** two mechanisms compute the same per-event likelihood contribution
- **THEN** they carry the same likelihood class and one method serves both,
  rather than a class each and an alias between them

#### Scenario: redirect reuses the same recipe machinery
- **WHEN** `estimate_dynam(formula, sub_model = "choice_coordination", data =
  d)` runs through the deprecation redirect
- **THEN** the fit is produced by the same dynamu conjunctive recipe path, not
  a parallel legacy implementation
