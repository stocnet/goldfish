## ADDED Requirements

### Requirement: The imputation contract is published as a shape-by-time table

The package SHALL document the complete imputation contract as a table over
object shape (dyad, global, nodal numeric, nodal categorical) and evaluation
time (start of the observation window, during the walk), in the user-facing
documentation of the data objects and in the modeling vignette. Every cell
SHALL be implemented by the single resolver; no call site SHALL carry its own
copy of a cell's rule.

For the dyad shape, a missing value SHALL be treated as the absence of a tie
(zero) at both evaluation times, and the contract SHALL record this as a
definition — the meaning of a missing tie — not as a summary over a pool.

#### Scenario: The documentation states every cell

- **WHEN** a user reads the missing-data documentation
- **THEN** it states the rule for each shape at each evaluation time, including
  the abort cells, with no cell left as unstated behavior.

#### Scenario: A missing tie is an absent tie at both times

- **WHEN** a network object carries a missing value in its initial matrix and a
  later tie event carries a missing `increment` or `replace`
- **THEN** both resolve to zero (no tie), producing the same state as if the
  value had been recorded as zero.

### Requirement: The documentation discloses imputation feedback and recommends multiple imputation

The missing-data documentation SHALL state that an imputed value becomes part
of the state, so a nodal value missing after the start of the observation
window is summarized from a pool that may contain earlier **imputed** values,
not only observed ones, and that in consequence single imputation treats
imputed values as observed and understates uncertainty.

The same documentation SHALL recommend, as the better strategy where
missingness is material, imputing before goldfish objects are built: multiple
imputation producing several completed datasets, one model fit per completed
dataset, and combination of the estimates under Rubin's rules via
`mitools::MIcombine()`, which consumes the `coef()` and `vcov()` methods
goldfish results provide. The documentation SHALL state the validity caveat
that Rubin's rules assume approximately normal, congenial estimates. The
`mitools` package SHALL be a suggested dependency only; the engine SHALL NOT
depend on it.

#### Scenario: The feedback property is documented

- **WHEN** a user reads the missing-data documentation
- **THEN** it states that imputed values join the state and inform later
  imputations of the same variable, and that uncertainty is understated as a
  result.

#### Scenario: A combined-estimates workflow is documented and executable

- **WHEN** a user follows the documented multiple-imputation example with
  `mitools` installed
- **THEN** fitting the same specification on each completed dataset and
  passing the list of fits to `mitools::MIcombine()` yields combined estimates
  and standard errors under Rubin's rules, exercised by a package test.

#### Scenario: The package remains installable without mitools

- **WHEN** the package is installed or checked without `mitools` available
- **THEN** installation, examples, and tests succeed, with the guarded example
  and test skipped rather than failing.

### Requirement: A missing global value aborts at schedule construction

A global attribute holds exactly one value, so its imputation pool is empty by
construction. Where a global attribute carries a missing value — in its
initial value or in any of its event streams — the package SHALL abort at
schedule construction, before the walk begins, naming the object and, for an
event-stream value, the event time. The package SHALL NOT substitute zero and
SHALL NOT emit a not-a-number value.

#### Scenario: A missing initial global value aborts

- **WHEN** a global attribute's initial value is missing
- **THEN** the package aborts at schedule construction naming the object,
  instead of imputing `mean()` of a length-one missing vector into a
  not-a-number value.

#### Scenario: A missing global replace event aborts before the walk

- **WHEN** a global attribute's event stream carries a `replace` with a missing
  value
- **THEN** the package aborts at schedule construction naming the object and
  the event time, instead of writing zero into state mid-walk.

### Requirement: A per-attribute imputation policy is declared at preprocessing

The preprocessing options SHALL accept an imputation policy: a named vector
keyed by nodal attribute name, with the value naming the policy for that
attribute. The default policy, applied to every attribute not named, SHALL be
the summary rule of the published contract, and omitting the policy argument
SHALL be behaviorally identical to the package before this change — declaring
no policy moves no coefficient.

Validation SHALL abort, naming the offending entry, when a policy names an
attribute that no effect reads, names a policy value that does not exist, or
names a reserved-but-unimplemented policy value (which SHALL list the
supported values). The policy SHALL apply identically through every data
source path; where a path cannot honor a declared policy, the package SHALL
abort naming the limitation rather than silently fall back to the default.

#### Scenario: No declared policy changes nothing

- **WHEN** a model is estimated without an imputation policy argument
- **THEN** every attribute is imputed by the published summary contract and the
  estimated coefficients are identical to the prior release.

#### Scenario: A policy naming an unknown attribute aborts

- **WHEN** the policy names an attribute absent from every effect in the
  formula
- **THEN** validation aborts naming that entry, before preprocessing begins.

#### Scenario: A reserved policy value aborts as unimplemented

- **WHEN** the policy declares a value that is reserved but not implemented
- **THEN** validation aborts stating the value is not supported and listing the
  supported policy values.

#### Scenario: A data source path that cannot honor the policy aborts

- **WHEN** a policy is declared and the active data source path cannot apply it
- **THEN** the package aborts naming the path and the limitation, rather than
  applying the default summary rule silently.

### Requirement: The as-category policy recodes missingness into an explicit level

For a factor or character attribute declared with the as-category policy,
missing values SHALL be recoded to a single documented reserved level at the
imputation seam — in the initial nodes table and in every event stream of that
attribute — before any effect initializer runs. From that point the level
SHALL be an ordinary observed value: it enters state, participates in
comparisons, and is visible to every summarizer and effect, so
missingness-by-design is preserved as a category rather than replaced by the
most common observed value.

Declaring the as-category policy for a numeric attribute SHALL abort at
validation, stating that the policy applies only to categorical attributes.
Where the reserved level already occurs among the attribute's observed values,
the package SHALL abort naming the attribute and the clash.

#### Scenario: Deliberate missingness survives to the summarizer

- **WHEN** a categorical attribute is missing by design for some nodes and is
  declared as-category
- **THEN** those nodes carry the reserved level in state, an effect reading the
  attribute sees that level as a value, and no node is assigned the most
  common observed category.

#### Scenario: A missing event-stream value becomes the reserved level

- **WHEN** an as-category attribute's event stream carries a `replace` with a
  missing value
- **THEN** the value written into state is the reserved level, and no summary
  over other nodes is computed.

#### Scenario: As-category on a numeric attribute aborts

- **WHEN** the policy declares as-category for a numeric attribute
- **THEN** validation aborts stating the policy applies only to factor or
  character attributes.

#### Scenario: A reserved-level collision aborts

- **WHEN** an as-category attribute already contains the reserved level among
  its observed values
- **THEN** the package aborts naming the attribute and the colliding level.

### Requirement: Statistic-level defaults are outside the imputation contract

The imputation contract SHALL govern missing values of data objects only. A
default value that an effect defines for an undefined statistic — such as the
tertius effects' value for a node with an empty in-neighborhood — SHALL be
documented as part of that effect's definition, SHALL NOT be described as
imputation in code or documentation, and SHALL NOT be routed through the
attribute resolver. The numeric behavior of the existing tertius
empty-neighborhood default SHALL be unchanged.

#### Scenario: The tertius empty-neighborhood default is unchanged and reclassified

- **WHEN** a tertius effect computes its statistic for a node with no
  in-neighbors
- **THEN** the value equals the prior release's value exactly, and the effect's
  documentation states the empty-neighborhood default as part of the
  statistic's definition rather than as missing-data imputation.
