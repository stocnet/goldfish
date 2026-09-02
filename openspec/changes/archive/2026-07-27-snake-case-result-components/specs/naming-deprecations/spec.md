# naming-deprecations (delta)

## ADDED Requirements

### Requirement: Components of returned objects are named in snake_case

Every component of every object goldfish returns to a user SHALL be named in
snake_case, so that a reader of one component can predict the spelling of its
neighbours. This covers the fitted result and its nested convergence report, the
gather export, and the preprocessed object. The rule exists because the mixed
convention is not merely untidy: a component read under the wrong spelling
yields `NULL` rather than an error, and `NULL[[i]]` is itself `NULL`, so code
written against the wrong name can run and silently test or report nothing.

#### Scenario: neighbouring components share one convention
- **WHEN** a user reads any component of a fitted model
- **THEN** its name is snake_case, including inside the nested convergence
  report, where no component is spelled in a different convention from its
  siblings

#### Scenario: the export and preprocessed objects follow the same rule
- **WHEN** a user reads a component of a gather export or of a preprocessed
  object
- **THEN** its name is snake_case, matching the fitted result's convention

### Requirement: A fitted object from before the rename is recognized and reported

goldfish SHALL recognize a fitted object produced before the components were
renamed, and SHALL tell the user that it must be re-fitted rather than failing
obscurely. The renamed components get **no** deprecation window: an object from
the last CRAN release is already unusable for reasons unrelated to spelling — it
lacks components the current methods require, so its summary fails and its
log-likelihood carries no degrees of freedom, which makes model comparison wrong
without saying so. Preserving the old spellings would therefore protect nothing
while adding permanent bookkeeping. Newly built fitted **and preprocessed**
objects SHALL each record the format version they were built with, and the
absence of that record SHALL itself identify an object built before the record
existed — so recognition rests on one positive convention rather than on
inspecting which substantive components an object happens to be missing. Both
kinds of object SHALL be recognized by the same rule; neither gets a weaker test
than the other. The two records SHALL live under **distinct component names**,
because a preprocessed object can be carried inside a fitted object, and one
shared name would put two different facts under one key at two depths of the same
object.

#### Scenario: an old object is named as old, not met with an internal error
- **WHEN** any method is called on a fitted object produced before the rename
- **THEN** the message identifies it as predating the current version and says
  to re-fit, rather than reporting a missing value or a zero-length argument

#### Scenario: a computation that would be wrong is refused, not merely flagged
- **WHEN** a log-likelihood, variance-covariance matrix, summary, or information
  criterion is requested from a pre-rename object
- **THEN** the call fails with that message, because returning a value whose
  degrees of freedom are unknown would silently corrupt model comparison

#### Scenario: printing an unfamiliar object still tells the user what it is
- **WHEN** a pre-rename object is printed
- **THEN** the message is informative and what can be shown is still shown, so a
  user holding an unlabelled object can find out what they have

#### Scenario: a current object is recognized from its own record
- **WHEN** a model is fitted with the current version and any method is called
- **THEN** no such message appears, and the object carries the format version it
  was built with

#### Scenario: an object carrying no version record is treated as predating it
- **WHEN** a fitted or preprocessed object carries no version record at all
- **THEN** it is reported as predating the current version, by the same rule for
  both kinds of object, without consulting which other components it carries

#### Scenario: a stale preprocessed object is refused at estimation entry
- **WHEN** a preprocessed object built by an earlier version is passed to an
  estimator
- **THEN** estimation stops and tells the user to recompute it, rather than
  reaching the numerical core and failing on a component that is not there

#### Scenario: a nested preprocessed object keeps its own record distinct
- **WHEN** a fitted object carries the preprocessed object it was estimated from,
  so that post-estimation surfaces can replay from it
- **THEN** the fitted object's record and the preprocessed object's record are
  readable independently under their own names, and neither shadows or is
  mistaken for the other

#### Scenario: a version record is not mistaken for the wrong kind of object
- **WHEN** a preprocessed object is passed where a fitted object is expected
- **THEN** the call is refused on the object's kind, rather than being accepted
  because a version record happened to be absent

#### Scenario: NEWS records the rename and that there is no window
- **WHEN** the release notes for the version carrying this rename are read
- **THEN** they list every renamed component with its old and new spelling, and
  state that the old names stop working immediately rather than after a
  deprecation period

### Requirement: The reserved incremental estimation variant is removed

The internal model-specification constructor SHALL NOT carry a parameter
reserving a future incremental estimation variant. The parameter accepts only
its default and aborts on every other value, so nothing depends on it, and it
spells `engine` — the vocabulary replaced by `backend` — which reintroduces a
second meaning for a word this line of work removed. Explanatory comments about
estimation state being reusable by algorithm variants SHALL be kept, since that
reasoning is independent of whether any particular variant ships.

#### Scenario: the reserved parameter is gone
- **WHEN** the model-specification constructor is called
- **THEN** it accepts no parameter reserving an incremental variant, and no code
  path aborts on one
