# optimizer-selection (delta)

## ADDED Requirements

### Requirement: initial parameter values align to terms by name

`set_algorithm_newton(initial_parameters =)` SHALL accept, in addition to the full-length
unnamed numeric vector aligned by coefficient position, a named numeric vector whose names
are matched against the fit's coefficient labels, seeding only the named coefficients and
leaving every other coefficient at its default starting value. The full-length unnamed form
SHALL keep its current behavior unchanged. An unnamed vector of partial length SHALL abort
naming the expected length; an unknown name SHALL abort listing the available coefficient
labels. Seeding only non-intercept terms SHALL NOT disable the rate intercept's
data-derived warm start — whether the intercept was seeded SHALL be derived from the
structured contract, not from whether an initial vector was supplied at all. In a
multi-process specification a flat named vector SHALL broadcast — each process seeds the
labels its own coefficients carry, and a name matching no process SHALL abort; a nested
list keyed by flavor, optionally by family within a flavor, SHALL seed only the targeted
process(es), with an unknown flavor or family key aborting naming the valid ones, and the
nested and flat forms SHALL NOT be mixed in one call; an unnamed full-length vector
supplied to a multi-process specification SHALL abort with guidance to use the named
forms.

#### Scenario: a named partial vector seeds only the named terms
- **WHEN** a rate model with an intercept is estimated with
  `initial_parameters = c(inertia = 1.5)`
- **THEN** the `inertia` coefficient starts at 1.5, every other coefficient starts at its
  default, and the intercept still receives its data-derived warm start.

#### Scenario: the positional full-length form is unchanged
- **WHEN** a full-length unnamed `initial_parameters` vector is supplied
- **THEN** every coefficient, including the intercept, starts at the supplied value, and
  the warm start is not applied — exactly the current documented behavior.

#### Scenario: an unknown name aborts listing the labels
- **WHEN** `initial_parameters = c(inertai = 1.5)` misspells a term
- **THEN** estimation aborts with a cli error naming the unknown entry and listing the
  available coefficient labels.

#### Scenario: a flat named vector broadcasts across a flavored specification
- **WHEN** a two-flavor specification whose rate and choice formulas all carry `inertia`
  is estimated with `initial_parameters = c(inertia = 0.5)`
- **THEN** every process seeds its own `inertia` coefficient at 0.5, the rate intercepts
  keep their data-derived warm start, and no other coefficient is disturbed.

#### Scenario: a nested list seeds one targeted process
- **WHEN** the same specification is estimated with
  `initial_parameters = list(creation = list(rate = c(inertia = 0.5)))`
- **THEN** only the creation rate process seeds `inertia`; every other process starts at
  its defaults, and an unknown flavor or family key would abort naming the valid ones.
