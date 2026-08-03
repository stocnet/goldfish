## ADDED Requirements

### Requirement: A model needs at least one free parameter
Formula parsing SHALL abort with a cli error naming the free-parameter count
when a model has no free parameter to estimate, rather than failing inside the
parser or reaching estimation with an empty parameter space. A term declared
with `offset()` is fixed, so it contributes no free parameter.

An intercept SHALL count as a free parameter only where the sub-model's
normalization identifies it. On the exact-time rate and REM families the
intercept is a baseline rate estimated against elapsed time, so an
intercept-only model is estimable and is the natural null model for a
likelihood-ratio comparison. On the multinomial families a constant statistic
cancels in the risk-set normalization, so an intercept-only model there has no
free parameter and SHALL abort. The predicate SHALL be the risk-set descriptor
the specification already carries, not a second derivation from the sub-model
name.

The error SHALL distinguish the two causes it can have — every term fixed, or
an intercept that this sub-model cannot identify — so the remedy is evident.

#### Scenario: an offset-only formula aborts
- **WHEN** a model is specified whose right-hand side holds only `offset()`
  terms, with or without an intercept
- **THEN** parsing aborts naming the zero free parameters

#### Scenario: an intercept-only rate model estimates
- **WHEN** an exact-time rate model is specified with an intercept and no other
  term
- **THEN** it estimates, and its single coefficient is the baseline rate

#### Scenario: an intercept-only choice model aborts
- **WHEN** a choice model is specified with an intercept and no other term
- **THEN** parsing aborts, naming the intercept as unidentified under that
  sub-model's normalization

#### Scenario: a fixed term beside a free one is unaffected
- **WHEN** a model carries one free term and one `offset()` term
- **THEN** it estimates as before, reporting a zero standard error for the fixed
  column

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
  is decided by the free-parameter rule
