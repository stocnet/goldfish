## RENAMED Requirements

- FROM: `### Requirement: test_parameter score test`
- TO: `### Requirement: test_parameter score, Wald and likelihood-ratio tests`

## MODIFIED Requirements

### Requirement: test_parameter score, Wald and likelihood-ratio tests
`test_parameter()` SHALL test a restriction on a fit's terms by one of three
`type`s of one null hypothesis — `"score"` (the default), `"wald"` and
`"lr"` — returning one classed object carrying the `type`, the statistic,
its degrees of freedom and p-value. The score type SHALL implement the score
(LM) test of the fit's **offset (fixed-coefficient) terms** at the values
their formula imposed: it SHALL evaluate the full model's score `U` and information `I` at the
fitted estimate via `evaluate_model()` and report
`LM = t(U) %*% solve(I) %*% U` with its chi-square p-value on the tested
block's degrees of freedom (efficient-score form). The evaluation SHALL be
unconditional on fixedness, since the fitted object's own `final_score` has
the fixed components zeroed before the Newton step and therefore does not
carry the score at the offset coefficients. The evaluation SHALL therefore
require the preprocessed statistics — attached by
`estimate_*(return_preprocessed = TRUE)` or supplied through `preprocessed =`
— and SHALL abort naming both routes when it has neither, through the same
guiding error the other replay-needing diagnostics raise. The score at a fixed
coefficient SHALL NOT be stored on the fitted object to avoid that pass: one
rule for which diagnostics need the statistics is worth more than saving a
pass on one of them, and the masked score is produced inside the estimation
loop's accept/reject reset, which is not edited for a diagnostic's
convenience. Testing a candidate effect
**absent** from the formula is deferred: it requires preprocessing an
augmented model over the whole event sequence, and the documentation SHALL
name `offset(term, coef = 0)` as the way to test a candidate today — the
term enters the model held at zero, its statistics are preprocessed in the
same pass, and the resulting fit is the constrained one the test needs.
The cost difference SHALL be stated where the restriction is documented, so
the deferral reads as a route rather than a gap.
The Wald type SHALL test, on an unconstrained fit, that the terms named by
`effects =` equal their `offset()` value or zero, as the quadratic form of
the flat `coef()` and `vcov()` over the tested block; a restriction matrix
for general linear combinations is not offered. The likelihood-ratio type
SHALL compare the fit against a restricted fit supplied as `null =`, SHALL
guard that both fits share the class, the dependent layer and the
dependent-event count and that the null's terms are a subset of the fit's
by `coef_layout()` name, aborting naming the failed guard, and SHALL report
`2 (ℓ_full − ℓ_null)` on the difference in free parameters; on a
multi-process container it SHALL report one statistic per process whose
terms differ and their total. A fit class whose likelihood is a Monte-Carlo
estimate SHALL supply its own method deciding each type, since the generic
carries no parent default. The documentation SHALL state that
`lmtest::lrtest()` and `lmtest::waldtest()` remain valid on exact-likelihood
fits, and a test SHALL pin that the `"lr"` and `"wald"` statistics equal
theirs.

#### Scenario: an offset term is tested at its imposed value
- **WHEN** a model is fitted with `offset(term, coef = 0)` and
  `return_preprocessed = TRUE`, and `test_parameter()` is called on it
- **THEN** the test reports the score statistic and p-value for that term,
  computed from one evaluation pass, with no preprocessing pass run

#### Scenario: without the statistics the test says how to supply them
- **WHEN** `test_parameter()` is called on a fit carrying no preprocessed
  statistics and none is supplied
- **THEN** it aborts naming both routes — re-estimating with
  `return_preprocessed = TRUE`, or passing `preprocessed =` — rather than
  reporting a statistic from the masked score the fit does carry

#### Scenario: a candidate absent from the formula is refused with the idiom
- **WHEN** `test_parameter()` is asked to test an effect the formula does not
  contain
- **THEN** it aborts naming the `offset(term, coef = 0)` idiom and the reason
  the absent-effect form is not available

#### Scenario: score test detects an omitted effect
- **WHEN** data simulated with a nonzero reciprocity effect are fitted
  without it and `test_parameter()` tests reciprocity at the constrained
  estimate
- **THEN** the LM statistic is significant at the 5% level on the fixture
  seed, without any Newton-Raphson iterations on the full model.

#### Scenario: score test equals the quadratic form of one scoring step
- **WHEN** the LM statistic is computed on a fixture
- **THEN** it equals `t(Delta) %*% I %*% Delta` for the one-step update
  `Delta = solve(I) %*% U` within floating-point tolerance.

#### Scenario: the Wald type tests on the full fit
- **WHEN** `test_parameter(fit, effects = "recip", type = "wald")` is called
  on a fit that estimated `recip` freely
- **THEN** it reports the Wald statistic from the flat `coef()` and `vcov()`
  on one degree of freedom, equal to `lmtest::waldtest()`'s on the same
  restriction

#### Scenario: the likelihood-ratio type needs a nested null
- **WHEN** `test_parameter(full, type = "lr", null = restricted)` is called
  with two exact-likelihood fits on the same layer and events, the null's
  terms a subset of the full's
- **THEN** it reports twice the log-likelihood difference on the parameter
  difference, equal to `lmtest::lrtest()`'s statistic

#### Scenario: an unrelated pair is refused by name
- **WHEN** the two fits differ in dependent layer or event count, or the
  null's terms are not a subset
- **THEN** the call aborts naming the guard that failed, rather than
  reporting a statistic
