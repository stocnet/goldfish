## ADDED Requirements

### Requirement: One behavioral descriptor is computed once at spec construction

The model spec SHALL carry exactly one behavioral descriptor, built in the
spec constructor and nowhere else, from which every downstream component reads
the facts it needs. The descriptor SHALL subsume the risk-set descriptor
rather than coexisting with it, so a model spec carries one behavioral object,
not two. Each field SHALL take a value from a closed vocabulary: `axis`
(`sender`, `receiver_given_sender`, `dyad`), `timing` (`timed`, `ordinal`),
`likelihood` (`poisson`, `multinomial`, `coordination`), `input_shape`
(`standard`, `grouped`), and `distribution` (`exponential`, with `weibull` and
`gompertz` reserved for the parametric rate work). A field that no consumer
branches on SHALL NOT be added.

#### Scenario: the descriptor is built once

- **WHEN** a model spec is constructed for any supported model and sub-model
- **THEN** it carries a single behavioral descriptor whose fields are all
  drawn from their declared vocabularies, and no second behavioral descriptor
  object is present on the spec

#### Scenario: symmetry is carried by the likelihood, not the axis

- **WHEN** a one-mode coordination spec is constructed
- **THEN** its `axis` is `dyad`, the same value a REM spec carries, and the
  fact that its likelihood sums unordered pairs once is carried by
  `likelihood`, because preprocessing computes statistics on the same dyad
  grid for both

#### Scenario: an unsupported combination is rejected at construction

- **WHEN** a spec is constructed for a model and sub-model combination that
  has no descriptor mapping
- **THEN** construction aborts, rather than producing a spec whose descriptor
  fields are missing or `NA`

### Requirement: No component re-derives behavior from model or sub_model

No site SHALL branch on `model` or `sub_model` to decide behavior, outside
the spec constructor and the validators that check a user-supplied value
against its allowed set; every such site SHALL read the descriptor. The
`model` and `sub_model` fields SHALL remain on the spec as provenance — what
the user asked for, reported by print methods and error messages — and SHALL
NOT be used as switches. A guard test SHALL enforce this by failing on any
`model`/`sub_model` equality or membership test outside the documented
exception list.

#### Scenario: preprocessing reads the descriptor

- **WHEN** preprocessing selects its recipe and the parameters that recipe
  needs
- **THEN** the selection is read from the descriptor, and no branch on
  `model` or `sub_model` participates in it

#### Scenario: the guard test catches a reintroduced re-derivation

- **WHEN** a `sub_model == ...` or `model %in% ...` behavior branch is added
  outside the documented exceptions
- **THEN** the guard test fails and names the offending file and line

#### Scenario: provenance survives

- **WHEN** a fitted model or a spec is printed
- **THEN** the model and sub-model the user requested are still reported

### Requirement: One name per behavior, naming the behavior

Each behavioral fact SHALL be carried under exactly one name, and that name
SHALL describe the behavior rather than one of its consequences. The
`right_censored` and `intercept_scalars` pair SHALL be retired: both are TRUE
exactly when the sub-model is a timed rate — a waiting time carrying an
exposure denominator — and FALSE for an ordinal comparison in which only event
order is modeled, so they SHALL be replaced by the single `timing` field.
Recipe helpers MAY keep boolean parameters in their own signatures, derived
from `timing` at the call site; no other site SHALL name either retired flag.

#### Scenario: the retired flags are gone from the spec surface

- **WHEN** the model spec and its descriptor are inspected
- **THEN** neither `right_censored` nor `intercept_scalars` appears, and
  `timing` carries the fact both encoded

#### Scenario: a timed rate and an ordinal sub-model are distinguishable

- **WHEN** a timed rate spec and an ordinal sub-model spec are compared
- **THEN** they differ in `timing`, and no consumer needs a second field to
  tell them apart

### Requirement: One statistics-output class, distinguished by fields

`compute_statistics()` SHALL return a single class whose variations are
carried as fields rather than as separate class strings: a `storage` field
taking `pointer`, `stack` or `db`, and a `scope` field taking `single` or
`flavored`. Every shape it returns SHALL carry that class — including the
gather shape, which today is returned to users unclassed. A single print
method SHALL render all of them by reading the fields. The classes this
replaces SHALL NOT be retained as aliases.

#### Scenario: every output shape is classed

- **WHEN** `compute_statistics()` is called with each supported `output` value
- **THEN** each return carries the statistics-output class, and its `storage`
  field records which shape it is

#### Scenario: the gather shape is no longer unclassed

- **WHEN** `compute_statistics(output = "gather")` returns
- **THEN** the result carries the statistics-output class with
  `storage = "stack"`, so `inherits()` and print dispatch reach it

#### Scenario: the flavored distinction is a field

- **WHEN** a flavored specification and a single process are each preprocessed
- **THEN** both results carry the same class, differing in `scope`, and no
  separate flavored class is attached

### Requirement: S3 dispatch is retained only where implementations differ

Dispatch SHALL be kept where two variants run genuinely different code and
removed where they do not. The likelihood SHALL dispatch, on a likelihood
class carried by the spec rather than on the model and sub-model pairing, so
that variants sharing an implementation share a class. The estimation entry
point SHALL continue to dispatch on the risk-set axis. Preprocessing SHALL NOT
dispatch per model variant. No method SHALL be registered as an alias of
another: a registered method SHALL contain an implementation that differs from
every other method on its generic.

#### Scenario: variants sharing a likelihood share a class

- **WHEN** the likelihood is computed for a DyNAM rate spec and for a DyNAM-i
  rate spec
- **THEN** both dispatch to the same method, and no alias method is registered
  for the second

#### Scenario: variants differing in likelihood keep separate methods

- **WHEN** the likelihood is computed for a choice sub-model and for a
  coordination sub-model
- **THEN** each dispatches to its own method, because their implementations
  differ

#### Scenario: a debugger names the method that ran

- **WHEN** execution stops inside a likelihood method
- **THEN** the frame names the method whose body is executing, not a method
  that was assigned from another
