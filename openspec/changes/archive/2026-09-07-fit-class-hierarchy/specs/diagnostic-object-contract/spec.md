# diagnostic-object-contract Delta Specification

Note: written against the living spec as merged at v1.9.30.

## ADDED Requirements

### Requirement: Diagnostic generics carry a verdict for every fit class

Each diagnostic generic dispatching on a fitted-model class SHALL have a
recorded verdict, for every concrete fit class, in the `fit-class-hierarchy`
capability, so that what a flavored or Monte-Carlo fit returns from a
diagnostic is stated rather than implied by which method happens to have been
written. A diagnostic
that cannot be computed for a fit class SHALL refuse with a reason rather than
returning an empty or partial result.

#### Scenario: a diagnostic states its behavior per fit class

- **WHEN** a diagnostic generic is called on each concrete fit class
- **THEN** each call either returns the diagnostic, or aborts with the
  recorded reason, and no combination is left to whichever method happens to
  exist

#### Scenario: a diagnostic that cannot be computed refuses
- **WHEN** a diagnostic is not computable for a fit class
- **THEN** the call aborts naming the class and the reason, rather than
  returning an empty or partial table
