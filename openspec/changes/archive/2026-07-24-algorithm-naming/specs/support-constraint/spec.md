# support-constraint (delta)

## MODIFIED Requirements

### Requirement: Opportunity list deprecated in favour of support_constraint
The per-event opportunity list — supplied via `set_preprocessing(opportunities_list =)`, a preprocessing option, not a data component — SHALL be deprecated following the lifecycle process (`lifecycle::deprecate_warn()` pointing to `support_constraint`, badge, NEWS entry). During
the deprecation window it SHALL keep working by entering the mask as a `point`-kind input
(per-event sender-row override), always conjoined with `active_1`/`active_2`. Its
documentation SHALL describe the migration (an opportunity list is a `support_constraint`
reading an allowed-dyad network) and the rate gated-out-sender behaviour.

#### Scenario: opportunity list warns but still works
- **WHEN** a model is estimated with an `opportunities` list
- **THEN** a lifecycle deprecation warning names `support_constraint` as the replacement and
  the estimates are unchanged from the pre-deprecation behaviour.

#### Scenario: equivalent constraint reproduces opportunity-list results
- **WHEN** the same restriction is expressed once as an opportunity list and once as a
  `support_constraint` over an allowed-dyad network
- **THEN** the estimated coefficients agree to within 1e-6.
