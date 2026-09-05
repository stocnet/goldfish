## ADDED Requirements

### Requirement: Fitted-model classes share a parent

The fitted-model classes SHALL share a parent class, so that a generic
implemented once on the parent reaches every fit class unless that class is
deliberately excused by the contract table. The parent SHALL NOT be introduced
without the table, because inheritance alone is what allows a generic to
produce a value for a class where the value is not meaningful.

#### Scenario: a generic written once reaches every fit class

- **WHEN** a generic is implemented on the parent and no class excuses it
- **THEN** calling it on a single-process fit, a flavored fit and a DyNES fit
  all reach that implementation

#### Scenario: the leaked generics reach flavored fits

- **WHEN** `summary()`, `tidy()` or `glance()` is called on a flavored fit
- **THEN** it returns a result rather than failing with no applicable method,
  closing the gap that existed while the classes were flat

### Requirement: Every generic-by-class combination carries a recorded verdict

A checked-in, machine-readable contract table SHALL record, for each generic
dispatching on a fit class and each concrete fit class, exactly one verdict:
`inherit` (the parent's method is correct here), `override` (this class needs
its own, with a stated reason), or `refuse` (the generic is not meaningful
here). The table SHALL be the same artifact a test reads and a human edits, so
the two cannot drift.

#### Scenario: an undecided cell fails the test

- **WHEN** a fit class is added, or a generic dispatching on fit classes is
  added, and the table has no verdict for the resulting combination
- **THEN** the contract test fails and names the missing cell

#### Scenario: an override states its reason

- **WHEN** a cell is `override`
- **THEN** the table records why that class cannot use the parent's method

### Requirement: A generic that is not meaningful for a class refuses

A generic SHALL be recorded as `refuse` where it would otherwise return a
value that is not meaningful for a fit class, and the call SHALL abort with a
message naming
the class and the reason, rather than returning a value. This SHALL apply in
particular where a fit maximizes a Monte-Carlo estimate of the likelihood
rather than an exact likelihood: a quantity that is not an exact log-likelihood
SHALL NOT reach `AIC()` or `BIC()` through inheritance, because those return
numbers that appear to be information criteria and are not.

#### Scenario: a Monte-Carlo fit does not silently supply a log-likelihood

- **WHEN** `logLik()` is called on a fit whose likelihood is a Monte-Carlo
  estimate, and the table records `refuse`
- **THEN** the call aborts explaining that the quantity is an estimate, and
  `AIC()` and `BIC()` therefore cannot return a value for that fit

#### Scenario: refusing is louder than having no method

- **WHEN** a generic is refused for a class
- **THEN** the error names the class and the reason, rather than R's generic
  "no applicable method" message
