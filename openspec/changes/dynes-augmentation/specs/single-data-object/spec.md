## MODIFIED Requirements

### Requirement: Focal layer designates the dependent events
The dependent event stream SHALL be the layer named by `info$focal`, overridable by an
explicit argument. A panel layer SHALL NOT be focal for event-stream estimators
(`estimate_dynam()`, `estimate_rem()`, `estimate_dynami()`) — that error SHALL point to
`estimate_dynes()` as the estimator for panel-dependent processes. Under
`estimate_dynes()`, a focal layer carrying the panel-semantics flag SHALL be accepted:
its snapshots are the observed data of the augmented estimation. Multi-process
designation stays reserved for the multivariate change.

#### Scenario: Focal layer drives dependent events
- **WHEN** `info$focal = "calls"`
- **THEN** calls-layer events are dependent and all other layers are exogenous.

#### Scenario: Panel focal rejected on event-stream estimators
- **WHEN** `info$focal` names a layer with `observation = "panel"` and
  `estimate_dynam()` (or `estimate_rem()`) is called
- **THEN** validation aborts explaining these estimators model event-stream dependents
  and pointing to `estimate_dynes()`.

#### Scenario: Panel focal accepted by estimate_dynes
- **WHEN** the same specification is passed to `estimate_dynes()` and the focal layer
  carries the panel-semantics flag
- **THEN** validation passes and the layer's waves become the observed snapshots of the
  augmented estimation.

### Requirement: Panel layers are change-list exogenous covariates
A layer with `observation = "panel"` SHALL by default enter as an exogenous dyadic
covariate whose tie rows are updates applied at their wave times per `info$update`;
dissolutions MUST be explicit value-0 rows (documented — not statically detectable).
Wave updates SHALL emit right-censored statistic updates like other exogenous events. A
`window` parameter on an effect reading a panel layer SHALL abort before preprocessing.
A per-layer **panel-semantics flag** SHALL be readable metadata: a flagged layer's rows
are interpreted as **state snapshots** owned by the DyNES estimation (consecutive waves
diffed into candidate flip events per the `sequence-augmentation` capability) rather
than change-list updates; the flag has no effect on event-stream estimators beyond the
focal-layer rule.

#### Scenario: Panel layer updates at waves
- **WHEN** a friendship panel layer has tie rows at wave timestamps
- **THEN** effects reading friendship see the state of the most recent wave at each event
  time.

#### Scenario: Window on panel layer errors
- **WHEN** `estimate_dynam(dep ~ inertia(friendship, window = 30), ...)` runs with
  friendship a panel layer
- **THEN** an informative error is raised before preprocessing.

#### Scenario: Flagged panel layer resolves as snapshots
- **WHEN** a panel layer carries the panel-semantics flag and is the focal layer of a
  DyNES estimation
- **THEN** its rows are read as complete state snapshots at wave times, not incremental
  change-list updates.
