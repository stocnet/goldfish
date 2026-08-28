# offset-fixed-terms Specification

## Purpose
TBD - created by archiving change refactor-formula-parsing. Update Purpose after archive.
## Requirements
### Requirement: offset() marks a fixed-coefficient term

A formula term wrapped in `offset(...)` SHALL be parsed as a fixed-coefficient term: the inner
call is unwrapped and parsed through the normal effect path so its statistic is computed
exactly like any term, and the term is tagged as an offset (fixed) with its formula order. The
stat column SHALL be kept (the term contributes `coefficient * stat` to the linear predictor) —
offsets MUST NOT be dropped from the design as in GLM `model.matrix`.

#### Scenario: offset term is unwrapped and tagged
- **WHEN** a formula contains `offset(ego(sex))`
- **THEN** `ego(sex)` is parsed normally and its statistic computed, and the term is tagged as
  a fixed-coefficient (offset) term rather than estimated.

#### Scenario: offset stat column is retained
- **WHEN** a model with an offset term is preprocessed
- **THEN** the offset term's statistic column is present in the design and contributes to the
  linear predictor with its fixed coefficient.

### Requirement: offset coefficients supplied via offset_coef

`set_algorithm_newton()` SHALL accept an `offset_coef` argument giving the fixed coefficient
value(s) for the offset terms, aligned to the offset terms in formula order when unnamed, or
matched to the offset terms by term label when named. The estimation
front-end SHALL assemble, at the single point where term names and coefficient positions are
both known, one structured fixed-parameter contract — the fixed positions, their values, and
the term labels — and every estimation path (the compiled and R backends and any
optimizer adapter) SHALL consume that contract through one shared masking helper rather than
decoding a positional sentinel vector independently. Errors about fixed coefficients (an
invalid position, a length mismatch with the offset terms, an unknown name) SHALL name the
offending term.
Whether all coefficients are fixed (likelihood-only evaluation) and whether the intercept is
fixed SHALL be derived from the contract, not inferred from the encoding of a vector.
An offset term SHALL also accept its fixed value in place, via
`offset(term, coef = value)`; the same term receiving a value from both `coef =` and
`offset_coef` SHALL abort naming the term, and an offset term receiving a value from
neither source SHALL abort as today. A multi-process specification SHALL take offset
values from its formulas only — each process's values are local to its own formula, so a
process whose formula carries no `offset()` term fixes nothing — and supplying
`offset_coef`, or the superseded positional `fixed_parameters`, alongside a multi-process
specification SHALL abort with guidance to use `coef =`.

#### Scenario: single offset value
- **WHEN** a formula has one `offset(...)` term and `set_algorithm_newton(offset_coef = 2)`
- **THEN** that term's coefficient is held at 2 during estimation and the remaining
  coefficients are estimated.

#### Scenario: multiple offsets aligned by order
- **WHEN** a formula has two offset terms and `offset_coef = c(2, -1)`
- **THEN** the first offset term is fixed at 2 and the second at -1, by formula order.

#### Scenario: every backend consumes the same contract
- **WHEN** the same offset model is estimated on the compiled and R backends and via a
  maxLik-driven optimizer
- **THEN** all paths hold the same coefficients at the same values through the shared
  masking helper, and the estimates agree within the cross-backend tolerance.

#### Scenario: a fixed-coefficient error names the term
- **WHEN** `offset_coef` has a different length than the formula's offset terms
- **THEN** estimation aborts with a cli error naming the offset term(s) and the supplied
  length, not a positional index alone.

#### Scenario: per-process offset values ride the formulas
- **WHEN** a two-flavor specification's rate and choice formulas all carry the same
  windowed term, each wrapped as `offset(term, coef = value)` with its own value
- **THEN** each of the four process fits holds that term's coefficient at the value from
  its own formula — the shared label never conflates values across processes or scales.

#### Scenario: a one-flavor-offset specification estimates
- **WHEN** only one process of a multi-flavor specification carries an `offset()` term,
  with its value supplied via `coef =`
- **THEN** the offset-less processes estimate normally with nothing fixed, and the
  offset-carrying process holds its coefficient at the formula's value.

#### Scenario: offset_coef alongside a multi-process specification aborts
- **WHEN** `offset_coef` is supplied while estimating a multi-flavor specification
- **THEN** estimation aborts with a cli error advising `offset(term, coef = value)` in the
  process formulas, before any process is estimated.

#### Scenario: two value sources for one term abort
- **WHEN** a single-process formula wraps a term as `offset(term, coef = 2)` and
  `offset_coef` also supplies a value for it
- **THEN** estimation aborts with a cli error naming the term and the two sources.

### Requirement: the fit records fixedness as a logical

The fitted result SHALL record which coefficients were fixed as a plain logical indicator,
and `GetFixed()` (and through it every print and post-estimation consumer) SHALL read that
indicator without evaluating string-encoded values. A fit that carries only the
string-encoded fixed column predates this record and SHALL be refused by the result-format
guard with guidance to re-fit, consistent with the no-deprecation-window policy for
pre-rename fits; no string-parsing fallback SHALL remain.

#### Scenario: fixedness read without parsing
- **WHEN** a model with an offset term is estimated and printed
- **THEN** the fixed coefficient is marked from the logical record, with no string
  evaluation anywhere on the path.

#### Scenario: string-column fits are refused with re-fit guidance
- **WHEN** a fit whose effect description carries only the string-encoded fixed column is
  passed to a consumer that reads fixedness
- **THEN** the result-format guard aborts with a message advising the user to re-fit under
  the current version, before any string evaluation is attempted.

### Requirement: fixed_parameters is superseded by offset()

The positional `fixed_parameters` vector (NA = estimate, value = fix) SHALL be superseded by
`offset()` + `offset_coef` via the lifecycle `superseded` stage. It MUST continue to work with
a soft-deprecation message directing users to the offset interface.

#### Scenario: legacy fixed_parameters still works with a soft-deprecation
- **WHEN** a user passes the positional `fixed_parameters` vector
- **THEN** estimation proceeds as before and a lifecycle soft-deprecation message points to
  `offset()` + `offset_coef`.

### Requirement: offsets obey the choice identification axis

An offset term that is constant across the choice alternatives SHALL produce a warning rather
than an error. In DyNAM-choice / choice_coordination such a term (broadcast kind `ego` or
`global`) cancels in the softmax and has no effect, but nothing is estimated, so it warns
instead of aborting. In rate and REM an offset shifts the rate and is accepted.

#### Scenario: constant-across-alternatives offset in choice warns
- **WHEN** a choice model includes `offset(global(x))` or `offset(ego(...))`
- **THEN** estimation warns that the offset is constant across alternatives and has no effect,
  without aborting.

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

The mapping this produces is part of the contract, and it does not follow the
sub-model names. An ordinal sub-model normalizes over the risk set, so a
constant intercept cancels there exactly as it does in a multinomial choice —
`rate_ordered` belongs with the families that identify nothing, not with the
`rate` family whose name it shares:

| model · sub_model | normalizer | branch |
|---|---|---|
| DyNAM · rate, REM · rate | `poisson` | unsupported |
| DyNAM · rate_ordered, REM · rate_ordered | `multinomial` | identifies nothing |
| DyNAM · choice | `multinomial` | identifies nothing |
| DyNAM · choice_coordination | `coordination` | identifies nothing |

#### Scenario: an intercept-only choice model aborts as unidentified
- **WHEN** a choice or coordination model is specified with an intercept and no
  other term
- **THEN** it aborts, naming the intercept as identifying nothing under that
  sub-model's normalization

#### Scenario: an intercept-only ordinal model aborts as unidentified
- **WHEN** a `rate_ordered` model of either family is specified with an
  intercept and no other term
- **THEN** it aborts naming the intercept as identifying nothing, its likelihood
  normalizing over the risk set, and not as merely unsupported

#### Scenario: an intercept-only exact-time rate model aborts as unsupported
- **WHEN** a `rate` model of either family — the exact-time families, whose
  normalizer is Poisson — is specified with an intercept and no other term
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

