## ADDED Requirements

### Requirement: A model with no parameter aborts before preprocessing
Model specification SHALL abort with a cli error when the formula yields no
coefficient at all, rather than failing inside the parser, inside preprocessing,
or reaching estimation with an empty parameter space. The check SHALL run before
preprocessing, since the pipeline beyond that point assumes at least one effect
and fails there with internal dimension errors rather than with a stated rule.

The error SHALL name the reason, and the reason is not the same on every
sub-model. On the multinomial families a constant statistic cancels in the
risk-set normalization, so an intercept identifies nothing and the model has no
parameter — a property of the likelihood. On the exact-time rate and REM
families the intercept is a well-defined baseline rate estimated against elapsed
time, and the model is declined because goldfish does not support a formula
carrying no effect — a property of the implementation. The error SHALL NOT
describe the second case as an identification failure, which would send the
reader looking for a statistical error that is not there.

The predicate separating the two SHALL be the risk-set descriptor the
specification already carries, not a second derivation from the sub-model name.

#### Scenario: an intercept-only choice model aborts as unidentified
- **WHEN** a choice or coordination model is specified with an intercept and no
  other term
- **THEN** it aborts, naming the intercept as identifying nothing under that
  sub-model's normalization

#### Scenario: an intercept-only rate model aborts as unsupported
- **WHEN** an exact-time rate or REM model is specified with an intercept and no
  other term
- **THEN** it aborts, naming the formula as carrying no effect, and does not
  claim the intercept is unidentified

#### Scenario: the abort precedes preprocessing
- **WHEN** a model with no parameter is specified
- **THEN** the error raised is the stated one, not an internal dimension error
  from the preprocessing builders

### Requirement: A model whose coefficients are all fixed is an evaluation
A model SHALL NOT be rejected for having no *free* parameter. Every coefficient
held at a fixed value leaves nothing to estimate and a likelihood to evaluate,
which is a supported use: the process-state evaluators take their reference from
exactly such a fit. A formula whose terms are all `offset()` is this case, not a
degenerate one.

Fixedness SHALL mean the same thing whichever source it came from — an
`offset()` term in the formula or an entry in `fixed_parameters` — so the two
SHALL NOT differ in whether they are accepted.

#### Scenario: an offset-only formula evaluates
- **WHEN** a model is specified whose right-hand side holds only `offset()`
  terms with fixed values
- **THEN** it returns a fit whose log-likelihood is defined and whose estimated
  coefficient count is zero

#### Scenario: a fixed term beside a free one is unaffected
- **WHEN** a model carries one free term and one `offset()` term
- **THEN** it estimates as before, and the fixed term still enters the linear
  predictor

### Requirement: A diagnostic that needs a free parameter aborts where there is none
A diagnostic SHALL abort with a cli error naming the reason when its value is
undefined without a free parameter, rather than failing inside a numerical
routine. The variance-covariance matrix is the case that reaches a
solver: on a fit with no free coefficient the information over free parameters
is zero-dimensional and inversion fails with an internal message about a
`0-diml` argument.

A diagnostic that remains defined SHALL keep working, and the boundary is
whether the quantity is a function of the free parameters. The log-likelihood,
the information criteria, the fitted values, the per-event residual types that
carry no inverse information, the augmented table and the margin table are all
defined on an all-fixed fit and SHALL be unaffected.

Rendering SHALL survive an empty coefficient table without emitting internal
warnings.

#### Scenario: the variance-covariance matrix aborts
- **WHEN** `vcov()` is called on a fit whose coefficients are all fixed
- **THEN** it aborts naming the absence of a free coefficient, rather than
  failing inside the solver

#### Scenario: the still-defined diagnostics are unaffected
- **WHEN** an all-fixed fit is passed to the log-likelihood, the information
  criteria, `fitted()`, `augment()` or `margin_table()`
- **THEN** each returns its ordinary value

#### Scenario: the summary renders quietly
- **WHEN** an all-fixed fit is printed
- **THEN** the coefficient table renders with no estimated rows and no warning,
  and the count of fixed coefficients is stated

### Requirement: A degenerate formula parses before it is judged
Formula parsing SHALL tolerate a right-hand side that contributes no term
columns, so that the model is rejected by a stated rule rather than by an
internal failure. `stats::terms()` returns its `factors` attribute as a
zero-length vector rather than a zero-row matrix when no non-offset term is
present, and the parser SHALL normalize that shape before reading its
dimensions.

#### Scenario: a degenerate formula reaches the model-level check
- **WHEN** any of `~ 1`, `~ offset(x)`, `~ 1 + offset(x)`, or a right-hand side
  of several `offset()` terms is parsed
- **THEN** parsing does not fail on an internal dimension error, and the outcome
  is decided by the rules above
