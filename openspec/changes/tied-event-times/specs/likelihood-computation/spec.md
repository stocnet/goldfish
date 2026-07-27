# likelihood-computation (delta)

## ADDED Requirements

### Requirement: Exact-time estimation reports tied event times

Estimation SHALL warn, for the exact-time families whose likelihood weights
intervals by their duration, when dependent events share a timestamp with the
preceding event, reporting how many do. A zero-length interval contributes its
observed actor's or dyad's linear predictor to the log-likelihood but no
exposure, so the rate scale is informed by fewer events than the fit reports,
and nothing else in the output reveals it. The fit SHALL still be returned, and
no coefficient SHALL change: this reports existing behavior rather than altering
it. The multinomial families SHALL NOT warn, because their likelihood does not
use interval durations.

#### Scenario: tied dependent events are reported on an exact-time fit
- **WHEN** an exact-time rate or REM model is estimated on data where several
  dependent events share a timestamp with the event before them
- **THEN** a warning names how many dependent events are affected and explains
  that they contribute no exposure to the rate scale, and the fitted object is
  returned

#### Scenario: untied data warns about nothing
- **WHEN** an exact-time model is estimated on data whose dependent events all
  have strictly positive inter-event times
- **THEN** no tie warning is emitted

#### Scenario: the multinomial families are silent
- **WHEN** a choice, coordination, or ordinal rate model is estimated on data
  with tied event times
- **THEN** no tie warning is emitted, because interval durations do not enter
  those likelihoods

### Requirement: The fitted object records how many dependent events were tied

A fitted model SHALL carry the number of dependent events whose inter-event time
is zero, so a consumer can tell whether a fit is affected without re-deriving it
from the event times. The value SHALL be recorded for every family, including
those that do not warn, since a consumer may care about the arbitrary state
update order even where the likelihood is unaffected.

#### Scenario: the count is available on the fit
- **WHEN** a model is estimated on data containing tied dependent events
- **THEN** the fitted object reports the number of tied dependent events, and it
  agrees with the count in the warning where a warning was emitted

#### Scenario: an untied fit records zero rather than nothing
- **WHEN** a model is estimated on data with no tied event times
- **THEN** the fitted object reports zero tied dependent events, so a consumer
  can distinguish "none" from "not measured"
