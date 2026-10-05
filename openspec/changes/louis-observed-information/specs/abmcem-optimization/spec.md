## MODIFIED Requirements

### Requirement: A singular pooled information at the estimate is reported

When the weighted pooled information `I_c = sum_i w_i J_i` at the returned
estimate cannot be inverted, DyNES estimation SHALL still return the fit,
with `NA` covariance and Monte-Carlo covariance matrices, and SHALL emit
exactly one warning of class `goldfish_dynes_singular_information`. The
warning SHALL name, by their composite labels, every free coefficient whose
pooled information diagonal is not positive (`I_jj <= 0`), with no tolerance
relative to other coefficients. For a named coefficient whose returned value
equals its starting value, the warning SHALL state that it was not estimated
and stays at its starting value; for one whose returned value differs, it
SHALL state that the coefficient has no information in the final pool and
that its returned value, which it SHALL state, is not an estimate. When no
such coefficient exists the warning SHALL state that the pooled information
is singular without naming a coefficient. The fit's
parameters SHALL be left unchanged. When the optimization loop stopped
because no sequence of the pool had a finite likelihood, this warning SHALL
NOT be emitted. The per-pass standard errors of the `em_trace` SHALL NOT
warn. This test reads the complete-data pooled information `I_c`, not the
observed information the covariances invert: a coefficient with no
complete-data information has none observed either, and the attribution
names it by the same rule.

#### Scenario: an effect with no variation on any path is named
- **WHEN** a DyNES estimation includes an effect whose statistic is zero on
  every pooled path
- **THEN** the run completes, the fit's covariances are `NA`, and exactly one
  `goldfish_dynes_singular_information` warning names that effect's
  coefficient and states that it stays at its starting value.

#### Scenario: a coefficient that moved is not claimed to be at its start
- **WHEN** the pooled information at the estimate has a zero diagonal for a
  free coefficient whose returned value differs from its starting value
- **THEN** the warning names that coefficient, states its returned value and
  that the value is not an estimate, and does not state that it stays at its
  starting value.

#### Scenario: an invertible pooled information does not warn
- **WHEN** some pooled sequences have singular information matrices but the
  weighted pooled information at the estimate is invertible and the observed
  information is positive definite
- **THEN** no `goldfish_dynes_singular_information` warning is emitted and
  the covariances are finite.

#### Scenario: a collinear pooled information warns without a name
- **WHEN** the pooled information at the estimate is singular and every free
  coefficient's diagonal is positive, including a diagonal that is only a
  rounding residue
- **THEN** exactly one `goldfish_dynes_singular_information` warning states
  that the pooled information is singular and names no coefficient.

#### Scenario: a stopped pool does not also warn of singular information
- **WHEN** the optimization loop stopped because no sequence of the pool had
  a finite likelihood
- **THEN** no `goldfish_dynes_singular_information` warning is emitted; the
  stop's own warning is the only report.

## ADDED Requirements

### Requirement: A non-positive-definite observed information at the estimate is reported

When the weighted pooled information `I_c = sum_i w_i J_i` at the returned
estimate is invertible but the Louis observed-data information
`I_obs = sum_i w_i J_i - sum_i w_i s_i s_i' + S S'` is not positive definite
(its Monte-Carlo estimate of the missing information is as large as the
complete information in some direction), DyNES estimation SHALL still return
the fit with its parameters unchanged and `NA` covariance and Monte-Carlo
covariance matrices, and SHALL emit exactly one warning of class
`goldfish_dynes_observed_information_not_pd`. The warning SHALL state that the
Monte-Carlo estimate of the observed information is not positive definite at
the estimate and SHALL suggest a larger pool (`n_sequences`) or a model-driven
`routine`. When the fit has `converged = FALSE`, the warning SHALL also
state that the estimate may not be a maximum, so the observed information
need not be positive definite there. Positive definiteness SHALL be decided by a Cholesky factorization
of the symmetrized `I_obs`, with no further tolerance. When the singular-`I_c`
warning (`goldfish_dynes_singular_information`) is emitted, or the
optimization loop stopped because no sequence of the pool had a finite
likelihood, this warning SHALL NOT be emitted. The per-pass standard errors of
the `em_trace` SHALL be `NA` on a pass whose observed information is not
positive definite, and SHALL NOT warn.

#### Scenario: an observed information that is not positive definite warns
- **WHEN** the final pool's per-sequence scores spread so that
  `sum_i w_i s_i s_i' - S S'` exceeds `sum_i w_i J_i` in some direction, while
  `sum_i w_i J_i` is invertible
- **THEN** the fit returns with `NA` covariances and exactly one
  `goldfish_dynes_observed_information_not_pd` warning, and no
  `goldfish_dynes_singular_information` warning.

#### Scenario: a non-converged fit is told the estimate may not be a maximum
- **WHEN** a fit returned with `converged = FALSE` has an observed
  information that is not positive definite
- **THEN** its single `goldfish_dynes_observed_information_not_pd` warning
  also states that the estimate may not be a maximum; a converged fit's
  warning does not.

#### Scenario: a singular complete information warns only once
- **WHEN** the pooled complete-data information at the estimate is singular
- **THEN** only the `goldfish_dynes_singular_information` warning is emitted,
  not `goldfish_dynes_observed_information_not_pd`.

#### Scenario: the em_trace standard errors do not warn
- **WHEN** `em_trace_se = TRUE` and a pass's observed information is not
  positive definite
- **THEN** that pass's `em_trace` standard errors are `NA` and no warning is
  emitted for the pass.
