## ADDED Requirements

### Requirement: A formula written for another model is read as goldfish's effects
`specify_dynam()` SHALL read a formula written for goldfish, for an MRQAP in
infernet, or for ergm. It SHALL take the model from `input`, else from the
formula's class, else from a name that only one model has. Each term that has
a goldfish effect SHALL be rewritten as that effect, and the record SHALL hold
the term as written and as rewritten.

#### Scenario: An ergm term becomes its goldfish effect
- **WHEN** a formula holds `nodematch("dept")` and `mutual`
- **THEN** it is read as written for ergm, and the specification holds
  `same(dept)` and `recip`, each with the status `"adapted"`

#### Scenario: An MRQAP term becomes its goldfish effect
- **WHEN** a formula holds `exchange(friends)` and `outdeg(friends)`
- **THEN** the specification holds `recip(friends)` and
  `outdeg(friends, type = "ego")`

#### Scenario: A name both models share says nothing about the model
- **WHEN** a formula holds only goldfish's names, such as `indeg` and `recip`
- **THEN** it is read as written for goldfish, and nothing is rewritten

### Requirement: A term goldfish cannot fit is dropped and named
A term that goldfish has no effect for, and, with `.data`, a main effect that
the sub-model cannot identify, SHALL be dropped with its reason in one
warning, and the rest SHALL be specified. An interaction with such a term
SHALL be dropped whole. `specify_dynam()` SHALL abort only where no term is
left.

#### Scenario: An ergm term without a goldfish effect is dropped
- **WHEN** a formula holds `gwesp(0.5)` and `mutual`
- **THEN** `gwesp(0.5)` is recorded as `"drops"` with its reason, one warning
  names it, and the specification holds `recip`

#### Scenario: An unidentified main effect is dropped with goldfish's reason
- **WHEN** a choice formula holds `inertia` and `ego(floor)`, with `.data`
- **THEN** `ego(floor)` is recorded as `"drops"` as an ego-perspective
  statistic, and the specification holds `inertia`

#### Scenario: Nothing left to fit
- **WHEN** every term of the formula is dropped
- **THEN** `specify_dynam()` aborts and names each term and its reason

### Requirement: With the network, the result is make_specification()'s specification
With `.data`, `specify_dynam()` SHALL return the `goldfishSpec` that
`make_specification()` returns for the terms it keeps, with the dependent
layer from the left-hand side, and SHALL add the record of each term.
`tidy()` of a `goldfishSpec` SHALL return one row for each term and its status.

#### Scenario: The same specification and the same estimates
- **WHEN** `specify_dynam(calls ~ inertia + recip + trans, data)` and the
  equivalent `make_specification()` call are estimated
- **THEN** both give the same coefficients

#### Scenario: A rate and a choice formula
- **WHEN** `formula` is `list(rate = calls ~ 1 + indeg, choice = calls ~ inertia)`
- **THEN** the specification has a rate and a choice sub-model

### Requirement: A table of every effect for a network
Without a formula, or with `.` as its right-hand side, `specify_dynam()` SHALL
return a tibble of class `"goldfishTerms"` with one row for each goldfish effect
of `model` and `sub_model`, written out for the network's layers and node
attributes, with the columns `term`, `effect`, `layer`, `attribute`,
`status`, and `reason`. Each status SHALL be that of `make_specification()`
for that effect alone. `formula()` of chosen rows SHALL give their
specification.

#### Scenario: The effects of a choice model
- **WHEN** `specify_dynam(calls ~ ., social_evolution)` is called
- **THEN** the table holds `inertia`, `recip(friendship)`, `same(gradeType)`,
  and `tertius(friendship, floor)` as `"fits"`, and `ego(floor)` as
  `"drops"` with its reason

#### Scenario: A specification of chosen rows
- **WHEN** rows of the table are chosen and passed to `formula()`
- **THEN** the result is the `goldfishSpec` of those effects
