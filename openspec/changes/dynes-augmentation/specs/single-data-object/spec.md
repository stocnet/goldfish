## MODIFIED Requirements

### Requirement: Focal layer designates the dependent events
On the **single-specification event-stream path** (`estimate_dynam()`,
`estimate_rem()`, `estimate_dynami()`) the dependent event stream SHALL be the layer
named by `info$focal`, overridable by an explicit argument; a panel layer SHALL NOT be
focal there — that error SHALL point to `estimate_dynes()` as the estimator for
panel-dependent processes. Under `estimate_dynes()`, dependent-process designation is
**not** read from `info$focal`: it is resolved by `make_joint_specification()` and the
`process_map` (`make-multivariate-spec`), each composed specification contributing its
own focal/dependent layer. A panel-observed layer that a composed specification models
as its dependent process SHALL be accepted: its snapshots are the observed data of the
augmented estimation. No separate panel-semantics flag is required — the
`observation = "panel"` metadata plus the layer's presence as a modeled process in the
multivariate specification is the trigger.

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
When a panel layer is a **modeled process** of a multivariate specification under
`estimate_dynes()`, its rows are interpreted as **state snapshots** to be augmented
(consecutive waves diffed into candidate flip events per the `sequence-augmentation`
capability) rather than change-list updates. A panel layer referenced only as an
**exogenous covariate** SHALL remain a **static step-covariate** — its state jumps only
at wave times, it is not latent, and no random sampling of its between-wave path is
done (there is no per-layer static-vs-random choice). A panel layer referenced nowhere
likewise keeps the plain change-list semantics. No effect on event-stream estimators
beyond the focal-layer rule.

#### Scenario: Panel layer updates at waves
- **WHEN** a friendship panel layer has tie rows at wave timestamps
- **THEN** effects reading friendship see the state of the most recent wave at each event
  time.

#### Scenario: Window on panel layer errors
- **WHEN** `estimate_dynam(dep ~ inertia(friendship, window = 30), ...)` runs with
  friendship a panel layer
- **THEN** an informative error is raised before preprocessing.

#### Scenario: Modeled panel layer resolves as snapshots
- **WHEN** a panel layer is a modeled process of a multivariate specification under
  `estimate_dynes()`
- **THEN** its rows are read as complete state snapshots at wave times, diffable into
  candidate flip events, not incremental change-list updates.

#### Scenario: Exogenous-only panel reference stays static
- **WHEN** a panel layer is referenced only as an exogenous covariate under
  `estimate_dynes()`
- **THEN** its state jumps only at wave times, it contributes no Monte-Carlo variation,
  and no random sampling of its between-wave path is done.
