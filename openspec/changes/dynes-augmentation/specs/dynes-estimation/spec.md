## ADDED Requirements

### Requirement: estimate_dynes estimates flavored models from panel waves

`estimate_dynes()` SHALL estimate one joint multi-layer likelihood from
panel-observed data: any number of relational-event and panel-semantics layers,
each modeled layer × flavor carrying rate and choice formulas (per the
`flavored-processes` capability), the parameter vector concatenating all
modeled sub-models. (How the multi-layer specification is passed is an open
design question, resolved before the estimator-surface phase.) A
panel-flagged layer MAY be focal under `estimate_dynes()`; event-stream
estimators SHALL continue to abort on panel focal layers, pointing to
`estimate_dynes()`. Estimation runs by ascent-based Monte Carlo EM over pools
of augmented endpoint-hitting sequences — the ABEM loop, its control
constructors, and its result contract are specified by the `abmcem` change;
this change supplies the panel data path (wave diffing, augmenters, batched
evaluation) those contracts consume.

#### Scenario: two-wave friendship model estimates
- **WHEN** `estimate_dynes(spec, algorithm = set_alg_em(n_sequences = 100,
  augmenter = set_alg_augment(routine = "random")))` runs on a two-wave panel
  friendship layer with creation/dissolution formulas
- **THEN** estimation returns per-flavor parameter estimates with convergence
  diagnostics, without requiring observed event times.

#### Scenario: event-stream specification rejected
- **WHEN** `estimate_dynes()` receives a specification whose focal layer is an
  event-stream layer
- **THEN** it aborts pointing to `estimate_dynam()`/`estimate_rem()` for fully observed
  sequences.

### Requirement: Specification validation guards the DyNES estimand

`estimate_dynes()` SHALL validate the multi-layer specification before estimation:
relational-event layers and flavors may be modeled or exogenous-only (exogenous
events update state, contribute no likelihood terms, and never compete in
augmentation draws); panel-semantics layers SHALL be modeled for all their flavors
or not at all — partially modeled panel layers abort, fully unmodeled ones keep the
exogenous change-list semantics. It SHALL detect, from the formulas and the objects
they create, sub-models whose statistics read no modeled panel state: a fully
panel-independent specification SHALL abort explaining that nothing is latent and
`estimate_dynam()` covers the model; a mixed specification SHALL abort naming both
remedies — separate `estimate_dynam()` runs for the panel-independent sub-models,
or folding them as flavors of one single layer in a flavored specification.
Windowed/memory effects SHALL be accepted (at the cost of one global MCMC chain and
no wave-level parallelism). Modeled relational events before the first wave SHALL
enter as history only (informing the state at the first wave, contributing no
likelihood terms); modeled relational events after the last wave SHALL be
discarded.

#### Scenario: fully panel-independent specification redirected
- **WHEN** no modeled sub-model's statistics read any modeled panel layer
- **THEN** `estimate_dynes()` aborts explaining that `estimate_dynam()` covers this
  model.

#### Scenario: mixed specification aborts with remedies
- **WHEN** some modeled sub-models read modeled panel state and others read none
- **THEN** `estimate_dynes()` aborts naming both remedies: separate
  `estimate_dynam()` runs for the independent sub-models, or one flavored layer
  folding them.

#### Scenario: partially modeled panel layer rejected
- **WHEN** a panel layer has two flavors and the specification models only one
- **THEN** validation aborts: panel layers are modeled for all flavors or not at
  all.

### Requirement: Parameter recovery on simulated panels

The change SHALL include a simulation-based validation: data simulated from known
creation/dissolution parameters, observed only at wave snapshots, re-estimated by
`estimate_dynes()`, with recovery within pre-registered tolerance bounds. A reduced
deterministic-seed variant SHALL run in the test suite (skip_on_cran), and the full
study — including the identifiability assessment of separate creation/dissolution
parameters from waves — SHALL be recorded with the change.

#### Scenario: known parameters recovered
- **WHEN** the seeded recovery test runs on a small simulated two-wave panel
- **THEN** estimates fall within the pre-registered bounds of the generating
  parameters.
