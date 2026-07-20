## ADDED Requirements

### Requirement: A singular information matrix names the coefficients responsible

Estimation SHALL, when the information matrix cannot be inverted, report which
coefficients are implicated instead of stating that collinearity is the probable
cause. The implicated set SHALL be derived from the null space of the singular
matrix: a basis vector concentrated on a single coefficient identifies that
effect as degenerate on its own, and a vector with mass spread across several
identifies those coefficients as a collinear group. Coefficient names SHALL be
rendered through the established naming path, and for a multi-process fit the
message SHALL also name the process the failure belongs to. The report SHALL
present a group as a set, never as a ranked guess at a single culprit.

#### Scenario: a single degenerate effect is named
- **WHEN** a model includes an effect whose statistic is identically zero over
  the risk set and the information matrix is therefore singular
- **THEN** estimation aborts naming that effect, rather than reporting
  collinearity between unnamed parameters.

#### Scenario: a collinear group is named as a set
- **WHEN** two effects are perfectly collinear
- **THEN** the abort names both as a collinear set and does not single out one of
  them as the cause.

#### Scenario: a multi-process failure names its process
- **WHEN** one process of a multi-flavor specification fails to invert
- **THEN** the message identifies that process by its rendered label, so the
  failing process is unambiguous among the parallel ones.

### Requirement: A single degenerate effect is explained from its statistics

When the null space implicates exactly one coefficient, estimation SHALL
distinguish, from that effect's statistics over the risk set, between an effect
with no variation anywhere and an effect that varies across events but is
constant across the alternatives within each event. The two SHALL produce
different guidance, because the user acts on them differently: the first cannot
enter the model at all, while the second is not estimable in a choice sub-model
(it cancels in the softmax) yet may be meaningful in the rate sub-model. This
examination SHALL run only on the failure path.

#### Scenario: no variation over the risk set
- **WHEN** a choice model includes `inertia(L)` while a derived constraint
  restricts the risk set to dyads where `L` is absent, making the statistic
  identically zero
- **THEN** the message reports that the effect has no variation over the risk set
  and cannot be estimated.

#### Scenario: constant within each event's alternatives
- **WHEN** an effect's statistic differs between events but takes one value
  across all alternatives of every event
- **THEN** the message reports that it cancels in the choice softmax and points
  to the rate sub-model as where such an effect can enter.

### Requirement: Separation is reported rather than returned silently

Estimation SHALL detect probable complete or quasi-complete separation after
convergence and warn, naming the effects involved and stating that their
estimates are not trustworthy. Detection combines a large coefficient magnitude,
a large standard error, and a log-likelihood at its attainable ceiling. The
warning SHALL state that the thresholds are heuristic. Estimation SHALL NOT
penalize, refit, or otherwise correct a separated model.

#### Scenario: perfectly predicting effect warns
- **WHEN** an effect perfectly predicts the observed choice, so its coefficient
  diverges with a correspondingly large standard error
- **THEN** the fit returns with a warning naming the effect and reporting
  probable separation, instead of returning the diverged estimate silently.

### Requirement: Conditioning of a successful fit is reported

`summary()` SHALL report the condition number of the information matrix for a
fitted model and SHALL warn when it exceeds a documented threshold, so that a
fit which inverted but is numerically unstable is visible before its standard
errors are interpreted. A model SHALL NOT be refused for being ill-conditioned.

#### Scenario: ill-conditioned fit is flagged
- **WHEN** a model's information matrix is near-singular but invertible
- **THEN** `summary()` reports the condition number and warns that the standard
  errors are unstable, while still returning the fit.
