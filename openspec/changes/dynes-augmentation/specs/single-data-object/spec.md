## MODIFIED Requirements

### Requirement: Focal layer designates the dependent events
The dependent event stream SHALL be the layer named by `info$focal`, overridable by an
explicit argument. A panel layer SHALL NOT be focal for event-stream estimators
(`estimate_dynam()`, `estimate_rem()`, `estimate_dynami()`) — that error SHALL point to
`estimate_dynes()` as the estimator for panel-dependent processes. Under
`estimate_dynes()`, a panel-observed layer that is a **modeled process** of the
multivariate specification (its focal/dependent layer) SHALL be accepted: its
snapshots are the observed data of the augmented estimation. No separate
panel-semantics flag is required — the `observation = "panel"` metadata plus the
layer's presence as a modeled process is the trigger.

#### Scenario: Focal layer drives dependent events
- **WHEN** `info$focal = "calls"`
- **THEN** calls-layer events are dependent and all other layers are exogenous.

#### Scenario: Panel focal rejected on event-stream estimators
- **WHEN** `info$focal` names a layer with `observation = "panel"` and
  `estimate_dynam()` (or `estimate_rem()`) is called
- **THEN** validation aborts explaining these estimators model event-stream dependents
  and pointing to `estimate_dynes()`.

#### Scenario: Panel focal accepted by estimate_dynes
- **WHEN** a multivariate specification models a panel-observed layer as a process and
  is passed to `estimate_dynes()`
- **THEN** validation passes and the layer's waves become the observed snapshots of the
  augmented estimation.

### Requirement: Panel layers are change-list exogenous covariates
A layer with `observation = "panel"` SHALL by default enter as an exogenous dyadic
covariate whose tie rows are updates applied at their wave times per `info$update`;
dissolutions MUST be explicit value-0 rows (documented — not statically detectable).
Wave updates SHALL emit right-censored statistic updates like other exogenous events. A
`window` parameter on an effect reading a panel layer SHALL abort before preprocessing.
When a panel layer is **referenced in a multivariate specification's formulas** under
`estimate_dynes()`, its rows are interpreted as **state snapshots** to be augmented
(consecutive waves diffed into candidate flip events per the `sequence-augmentation`
capability) rather than change-list updates: a panel layer that is a modeled process is
always augmented by the model-driven routine; a panel layer referenced only as an
exogenous covariate is augmented per the user's per-layer choice — a **static
step-covariate** (the default change-list behavior, not latent) or the **random
augmenter** (uniform between-wave ordering). A panel layer referenced nowhere keeps the
plain change-list semantics. No effect on event-stream estimators beyond the
focal-layer rule.

#### Scenario: Panel layer updates at waves
- **WHEN** a friendship panel layer has tie rows at wave timestamps
- **THEN** effects reading friendship see the state of the most recent wave at each event
  time.

#### Scenario: Window on panel layer errors
- **WHEN** `estimate_dynam(dep ~ inertia(friendship, window = 30), ...)` runs with
  friendship a panel layer
- **THEN** an informative error is raised before preprocessing.

#### Scenario: Referenced panel layer resolves as snapshots
- **WHEN** a panel layer is referenced in a multivariate specification's formulas (as a
  modeled process or an exogenous covariate) under `estimate_dynes()`
- **THEN** its rows are read as complete state snapshots at wave times, diffable into
  candidate flip events, not incremental change-list updates.

#### Scenario: Exogenous-only panel reference chooses its augmentation mode
- **WHEN** a panel layer is referenced only as an exogenous covariate and the user
  selects the static step-covariate mode
- **THEN** its state jumps only at wave times and it contributes no Monte-Carlo
  variation; selecting the random augmenter instead makes its between-wave path latent.
