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

