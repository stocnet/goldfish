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
value(s) for the offset terms, aligned to the offset terms in formula order. The estimation
front-end SHALL assemble the existing positional `fixedParameters` vector from the offset term
positions and `offset_coef`, so the Newton-Raphson core is reused unchanged.

#### Scenario: single offset value
- **WHEN** a formula has one `offset(...)` term and `set_algorithm_newton(offset_coef = 2)`
- **THEN** that term's coefficient is held at 2 during estimation and the remaining
  coefficients are estimated.

#### Scenario: multiple offsets aligned by order
- **WHEN** a formula has two offset terms and `offset_coef = c(2, -1)`
- **THEN** the first offset term is fixed at 2 and the second at -1, by formula order.

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

