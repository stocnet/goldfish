## ADDED Requirements

### Requirement: estimate_dynes estimates flavored models from panel waves

`estimate_dynes()` SHALL estimate one joint multi-layer likelihood from
panel-observed data: any number of relational-event and panel-observed layers,
each modeled layer × flavor carrying rate and choice formulas (per the
`flavored-processes` capability), the parameter vector concatenating all
modeled sub-models. `estimate_dynes()` SHALL take a `make_joint_specification()`
object (`make-multivariate-spec`) as its specification. A panel-observed layer
modeled as a process MAY be focal under `estimate_dynes()`; event-stream
estimators SHALL continue to abort on panel focal layers, pointing to
`estimate_dynes()`. Estimation runs by ascent-based Monte Carlo EM over pools
of augmented endpoint-hitting sequences — the ABEM loop, its control
constructors, and its result contract are specified by the `abmcem` change;
this change supplies the panel data path (wave diffing, augmenters, batched
evaluation) those contracts consume.

#### Scenario: two-wave friendship model estimates
- **WHEN** `estimate_dynes(spec, control_algo = set_algorithm_em(n_sequences = 100,
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

`estimate_dynes()` SHALL validate the multivariate specification before estimation:
relational-event layers and flavors may be modeled or exogenous-only (exogenous
events update state, contribute no likelihood terms, and never compete in
augmentation draws); panel-observed layers SHALL be modeled for all their flavors
or not at all — partially modeled panel layers abort, fully unmodeled ones keep the
exogenous change-list semantics; a panel layer referenced only as an exogenous
covariate stays a static step-covariate (not augmented, no random sampling).
`estimate_dynes()` SHALL require **at least one panel-observed layer to be a modeled
dependent process** (a latent between-wave path to estimate). Separability SHALL be
read from the multivariate specification's `coupled` fid column
(`make-multivariate-spec` D4 — a fid is coupled iff any effect argument or
support-constraint atom reads a **modeled** panel layer's state), not recomputed: a
specification whose fids are **all separable** — equivalently, no panel layer is a
modeled dependent process — SHALL abort, naming `estimate_dynam()` and explaining
that the specification fits DyNAM (not DyNES) and that its panel layers would only be
considered as static exogenous covariates; a **mixed** specification SHALL **proceed**
with a cli message naming the separable fids (their likelihood terms touch no latent
path, so joint estimation equals separate estimation for them). Windowed/memory
effects SHALL be accepted (at the
cost of one global MCMC chain and no wave-level parallelism). Modeled relational
events before the first wave SHALL enter as history only (informing the state at the
first wave, contributing no likelihood terms); modeled relational events after the
last wave SHALL be discarded.

#### Scenario: fully separable specification redirected
- **WHEN** every fid of the multivariate specification is marked separable — no panel
  layer is a modeled dependent process (any panel reference is exogenous-only)
- **THEN** `estimate_dynes()` aborts, naming `estimate_dynam()` and explaining the
  specification fits DyNAM (not DyNES) and that its panel layers would only be
  considered as static exogenous covariates.

#### Scenario: mixed specification proceeds with a message
- **WHEN** some fids are coupled to a panel-observed layer and others are separable
- **THEN** `estimate_dynes()` proceeds and emits a cli message naming the separable
  fids that could equivalently be estimated on their own.

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
