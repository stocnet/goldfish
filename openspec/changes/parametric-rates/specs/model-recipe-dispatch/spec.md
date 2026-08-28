# model-recipe-dispatch Delta Specification

## REMOVED Requirements

### Requirement: rate_ordered as explicit sub_model for estimate_dynam
**Reason**: The `rate_ordered` token (dev-line-only, absent from v1.7.0) is
folded into the orthogonal waiting-time distribution axis: the partial
likelihood / Cox model is `sub_model = "rate", distribution = "cox"` (see the
`rate-distributions` capability).
**Migration**: Replace `sub_model = "rate_ordered"` with `sub_model = "rate",
distribution = "cox"`. The no-intercept deprecation warning is superseded: a
no-intercept rate formula with `distribution = "exponential"` aborts pointing
at `distribution = "cox"`.

## MODIFIED Requirements

### Requirement: estimate_rem sub_model values
`estimate_rem()` SHALL accept `sub_model = "rate"` (dyadic hazard family) with
the waiting-time model selected by the `distribution` argument
(`"exponential"` full hazard with intercept, `"cox"` partial likelihood /
ordering only, plus the parametric levels per the `rate-distributions`
capability). `sub_model = "choice"` SHALL remain valid but emit a deprecation
warning suggesting `"rate"`. The `"rate_ordered"` token SHALL NOT be accepted.

#### Scenario: REM rate sub_model is accepted
- **WHEN** `estimate_rem(formula, data, sub_model = "rate")` is called
- **THEN** estimation completes and the result corresponds to a full dyadic
  rate model under the exponential default

#### Scenario: REM choice alias warns
- **WHEN** `estimate_rem(formula, data, sub_model = "choice")` is called
- **THEN** a deprecation warning is emitted suggesting `sub_model = "rate"`

#### Scenario: REM ordinal model via distribution
- **WHEN** `estimate_rem(formula, data, distribution = "cox")` is called
- **THEN** the partial-likelihood (ordering-only) model previously named
  `rate_ordered` is estimated
