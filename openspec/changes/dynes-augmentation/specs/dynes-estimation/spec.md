## ADDED Requirements

### Requirement: estimate_dynes estimates flavored models from panel waves

`estimate_dynes()` SHALL estimate one joint multi-layer likelihood from
panel-observed data: any number of relational-event and panel-observed layers,
each modeled layer × flavor carrying rate and choice formulas (per the
`flavored-processes` capability; an **unflavored** layer is the single-process
case of the same rule), the parameter vector concatenating all
modeled sub-models. A modeled panel layer SHALL be either a **state panel** (a wave
records the tie state; two competing flavors) or a **count panel** (a wave records the
cumulative count as of that wave; one monotone unflavored process, a wave difference
yielding that dyad's number of latent events in the interval). `estimate_dynes()` SHALL take a `make_joint_specification()`
object (`make-multivariate-spec`) as its specification. Each composed specification
carries its own focal/dependent layer, resolved through `make_joint_specification()`
and the `process_map`, so the joint estimand has one dependent process per composed
specification — dependent-process designation is **not** read from the data object's
`info$focal` (that field selects the single dependent stream only on the event-stream
path). A panel-observed layer that a composed specification models as its
dependent (focal) process is accepted under `estimate_dynes()`; event-stream
estimators SHALL continue to abort when their focal layer is panel-observed, pointing
to `estimate_dynes()`. Estimation runs by ascent-based Monte Carlo EM over pools
of augmented endpoint-hitting sequences — the ABEM loop, its control
constructors, and its result contract are specified by the `abmcem` change;
this change supplies the panel data path (wave diffing, augmenters, batched
evaluation) those contracts consume.

#### Scenario: two-wave friendship model estimates
- **WHEN** `estimate_dynes(spec, control_algo = set_algorithm_em(n_sequences = 100,
  augmenter = set_augmenter_options(routine = "random")))` runs on a two-wave panel
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
dependent process** (a latent between-wave path to estimate), and that requirement —
not the `coupled` fid column — SHALL be the gate: a specification with **no modeled
panel dependent process** SHALL abort, naming `estimate_dynam()` and explaining that
the specification fits DyNAM (not DyNES) and that its panel layers would only be
considered as static exogenous covariates. The gate SHALL NOT be read off the `coupled`
column, because a modeled panel layer that derives no support constraint (a count
panel, or any `redundant`-style layer) can have every fid marked separable while its
event times are wholly latent; bouncing such a specification to `estimate_dynam()`,
which itself aborts on a panel-observed focal layer, would leave the user between two
errors. The multivariate specification's `coupled` fid column
(`make-multivariate-spec` D4 — a fid is coupled iff any effect argument or
support-constraint atom reads a **modeled** panel layer's state) SHALL still be
consumed, not recomputed, for the **mixed** case: a specification with a modeled panel
dependent process and some separable fids SHALL **proceed** with a cli message naming
those fids (their likelihood terms touch no latent path, so joint estimation equals
separate estimation for them). Windowed/memory effects over **observed** layers SHALL
be accepted (at the cost of one global MCMC chain and no wave-level parallelism — v1
declines per-interval factorization unconditionally, including for specifications that
would factorize). A windowed effect over a **modeled panel** layer is out of v1 (a
recorded future development) and SHALL be refused with an informative error that names
the modeled panel layer and says windows are supported over observed layers only.
The estimation window SHALL run from the resolved observation start to the last wave,
whatever the preprocessing controls supplied.
The left edge of the estimand SHALL be the resolved observation start (the
`observation-window` capability's `start_time`), **never a wave**: events before it
enter as burn-in (state and order counter only, no likelihood terms), the first wave is
the right endpoint of the first modeled interval, and modeled relational events inside
that interval SHALL contribute likelihood terms like any others. Modeled relational
events after the last wave SHALL be discarded.

#### Scenario: specification with no modeled panel dependent process redirected
- **WHEN** no panel layer of the multivariate specification is a modeled dependent
  process (any panel reference is exogenous-only, so every fid is necessarily
  separable)
- **THEN** `estimate_dynes()` aborts, naming `estimate_dynam()` and explaining the
  specification fits DyNAM (not DyNES) and that its panel layers would only be
  considered as static exogenous covariates.

#### Scenario: exogenous-only panel reference is a static step-covariate
- **WHEN** the specification models only an event-stream emails layer, whose formulas
  read a panel-observed calls layer that carries no formulas of its own
- **THEN** `estimate_dynes()` aborts to `estimate_dynam()`: the calls layer is a static
  step-covariate jumping at wave times, nothing is latent, and DyNAM is the correct
  estimator.

#### Scenario: modeled count panel with exogenous-only formulas estimates
- **WHEN** a count panel layer is a modeled dependent process whose rate and choice
  formulas read only exogenous covariates, so every fid is marked separable
- **THEN** `estimate_dynes()` proceeds — the gate is the modeled panel dependent
  process, not the `coupled` column — rather than redirecting to `estimate_dynam()`,
  which could not estimate a panel-observed focal layer.

#### Scenario: mutually dependent panel and event-stream layers estimate jointly
- **WHEN** a modeled count panel layer (calls) and a modeled event-stream layer
  (emails) each carry formulas reading the other's state
- **THEN** `estimate_dynes()` estimates them jointly, and the emails layer is not
  separately estimable by `estimate_dynam()` even though its own events are fully
  observed, because its statistics depend on the latent call path.

#### Scenario: mixed specification proceeds with a message
- **WHEN** some fids are coupled to a panel-observed layer and others are separable
- **THEN** `estimate_dynes()` proceeds and emits a cli message naming the separable
  fids that could equivalently be estimated on their own.

#### Scenario: a window over a modeled panel layer is refused
- **WHEN** a formula reads the modeled panel layer through a windowed effect (for
  example `tie(friendship, window = 3)` where friendship is a modeled panel layer)
- **THEN** `estimate_dynes()` aborts, naming the layer and explaining that windows are
  supported over observed layers only in this version; the same effect with a window
  over an observed event-stream layer estimates.

#### Scenario: partially modeled panel layer rejected
- **WHEN** a panel layer has two flavors and the specification models only one
- **THEN** validation aborts: panel layers are modeled for all flavors or not at
  all.

#### Scenario: partially modeled relational-event layer accepted
- **WHEN** a relational-event layer has two flavors and the specification models only
  one (the other keyed in neither rate nor choice)
- **THEN** validation accepts it: the modeled flavor is a dependent process and the
  unmodeled flavor's events update state as exogenous input, contributing no likelihood
  terms — no abort. This is the deliberate contrast with the panel all-or-nothing rule.

### Requirement: Parameter recovery on simulated panels

The change SHALL include a simulation-based validation: data simulated from known
creation/dissolution parameters, observed only at wave snapshots, re-estimated by
`estimate_dynes()`, with recovery within pre-registered tolerance bounds. The
generating sequences SHALL be drawn by the package's own `simulate()` surface
(`process-simulation`) on the same `goldfishJointSpec` the estimation consumes,
with the known parameters supplied as a `goldfishParams` — not by a
study-local simulator — so that a divergence between the data-generating process
and the estimand is a test failure rather than an artifact of two implementations
of one model. Replicates SHALL be drawn as one `nsim > 1` pool, and any replicate
the explosion guard capped SHALL be excluded by an explicit
`filter_simulation()` rather than left to bias the recovery summary. A reduced
deterministic-seed variant SHALL run in the test suite (skip_on_cran), and the full
study — including the identifiability assessment of separate creation/dissolution
parameters from waves — SHALL be recorded with the change. The full study SHALL also
include a **flips-per-interval stress sweep** (e.g. ~2 → 5 → 15 → 40 changes per
between-wave interval), crossed with a **coupling axis** (the panel layer coupled to a
densely observed event-stream layer, and uncoupled), that locates where
importance-weighted augmentation degenerates. The study SHALL state the **operating
envelope** in terms of **Monte-Carlo standard error relative to the asymptotic standard
error** (the result contract's two error sources), not in terms of effective sample size:
ESS measures the proposal, so a specification whose posterior is sharpened by coupling can
score worse on ESS while being better identified, and only the MC-to-asymptotic error
ratio answers whether a fit is trustworthy. The asymptotic standard error SHALL be the
one `vcov()` returns, the inverse of Louis's observed-data information `I_obs`
(`louis-observed-information`), and the Monte-Carlo standard error the one with
`I_obs⁻¹` as its bread; neither SHALL be computed as `I_c⁻¹ (Σ w̄ᵢsᵢsᵢᵀ) I_c⁻¹`, whose
middle term is the missing information. Each cell SHALL record every replicate's
`glance()` `covariance_status`, `fraction_missing` and `covariance_ess` and tabulate the
status counts; a replicate whose status is not `"ok"` has no standard errors and SHALL
be counted rather than dropped from the ratio silently, a rising `"not_pd"` share being
itself a pool-size symptom of the envelope. Every replicate SHALL be fitted under one
fixed, reported M-step optimizer setting, and each cell SHALL record every replicate's
final M-step gradient norm and convergence flag. A non-converged replicate SHALL be
counted beside the covariance statuses and excluded from the error ratio and the
calibration, never averaged in: a fit that stops short of its optimum sits between the
starting values and θ̂, shrinking the replicate spread the calibration reads.

The full study SHALL also **calibrate the standard errors**: over the replicates of each
cell it SHALL compare every coefficient's empirical standard deviation of θ̂ with its
mean `vcov()` standard error, reporting the ratio and the Wald coverage of the
generating value. At least one cell SHALL carry an endogenous effect, so that the
augmented paths carry different complete-data scores and the missing information is
non-zero: the closed-form count-panel oracle (`louis-observed-information`, "standard
errors match a closed-form oracle") has no missing information by construction and
cannot tell `I_obs⁻¹` from `I_c⁻¹`, so this calibration is the only check that the
missing-information term is estimated well. Miscalibration SHALL be read against
`fraction_missing` and `covariance_ess`: collapsed importance weights shrink the
missing-information estimate toward zero, showing as standard errors too small beside a
`fraction_missing` near zero and a low `covariance_ess`. Under MCMC `covariance_ess` is
`NA` and the chain's acceptance rate and autocorrelation take its place.

The located cliff SHALL define the **numeric
IS-ESS collapse threshold** consumed by the runtime recommendation to switch to
`augment_seq_mcmc()` — a trigger, not a verdict — this change owning that threshold and
the `abmcem` change the message surface that emits it, so the runtime guidance and the
documented envelope agree by construction rather than being independently guessed. That
threshold SHALL be documented as applying to the `random`/`sim` routines only: under MCMC's
uniform weighting the same statistic is trivially the pool size and SHALL NOT be read as
evidence that the chain mixes, the MCMC diagnostics being acceptance rate and
autocorrelation.

#### Scenario: known parameters recovered
- **WHEN** the seeded recovery test runs on a small two-wave panel drawn by
  `simulate()` from the specification at known parameters and observed only at
  its wave snapshots
- **THEN** estimates fall within the pre-registered bounds of the generating
  parameters.

#### Scenario: a capped replicate is filtered, not averaged in
- **WHEN** a replicate of the recovery pool trips the explosion guard and is
  flagged as capped
- **THEN** the study drops it through `filter_simulation()` before estimating,
  and reports how many replicates were dropped, rather than recovering parameters
  from a truncated sequence.

#### Scenario: degeneracy envelope characterized
- **WHEN** the full study sweeps flips-per-interval upward at a fixed pool size, coupled
  and uncoupled
- **THEN** it records where effective sample size collapses (weights degenerate faster
  than pool growth recovers) and supplies that boundary as the numeric IS-ESS threshold
  the runtime switch-to-`augment_seq_mcmc()` recommendation fires at, while the reported
  operating envelope is the Monte-Carlo-to-asymptotic standard-error ratio at each cell.

#### Scenario: an ascent abort at the cliff names the importance-sampling remedies
- **WHEN** a `random` or `sim` run exhausts its pool-growth retries without certifying
  an ascent, and the lowest effective sample size over the stalled passes is below the
  IS-ESS collapse threshold times the pool size
- **THEN** the `goldfish_abem_retry_exhausted` abort names `augment_seq_mcmc()` and
  suggests truncating or smoothing the importance weights (`set_weights_options(
  transformation = "clipping")` or `"smoothing"`), beside its existing pool-size,
  step-size and retry advice; under `mcmc`, above the threshold, or while the
  threshold is not yet calibrated, the abort carries only the existing advice.

#### Scenario: a coupled specification is not condemned by its ESS
- **WHEN** a cell of the sweep couples the panel layer to a densely observed
  event-stream layer, sharpening the path posterior that a forward proposal cannot track
- **THEN** its lower effective sample size triggers the switch recommendation without
  being reported as a narrower operating envelope, the envelope being read off the
  MC-to-asymptotic error ratio.

#### Scenario: standard errors are calibrated where information is missing
- **WHEN** the full study fits the replicates of a cell whose specification carries an
  endogenous effect, so the augmented paths' complete-data scores differ
- **THEN** it reports, per coefficient, the ratio of the empirical standard deviation
  of θ̂ to the mean `vcov()` standard error and the Wald coverage of the generating
  value, beside the cell's `fraction_missing` and `covariance_ess` summaries.

#### Scenario: covariance statuses are tabulated, not silently dropped
- **WHEN** some replicates of a cell return `covariance_status` `"not_pd"`,
  `"singular"` or `"pool_dropped"`
- **THEN** the cell reports the count of each status, and its MC-to-asymptotic error
  ratio and calibration are computed over the `"ok"` replicates with that count stated
  beside them.

#### Scenario: a non-converged replicate is counted, not calibrated
- **WHEN** a replicate's final M-step ends at its iteration cap with a gradient norm
  above the study's convergence tolerance
- **THEN** the cell counts it as non-converged beside its covariance statuses, and
  its MC-to-asymptotic error ratio and standard-error calibration are computed without
  it, with the count stated beside them.
