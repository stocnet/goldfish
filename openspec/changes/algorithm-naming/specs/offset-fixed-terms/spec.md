# offset-fixed-terms (delta)

## MODIFIED Requirements

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
