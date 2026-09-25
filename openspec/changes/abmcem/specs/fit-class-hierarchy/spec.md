## MODIFIED Requirements

### Requirement: Every generic-by-class combination carries a recorded verdict

Each generic dispatching on a fit class SHALL carry, for each concrete fit
class, exactly one verdict: `inherit` (the parent's method is correct here),
`override` (this class needs its own, for a stated reason), or `refuse` (the
generic is not meaningful here). A change that adds a fit class, or adds a
generic dispatching on fit classes, SHALL record the resulting verdicts in
this requirement before it is implemented, so that a missing cell is argued
during design rather than discovered by a user.

The verdicts in force are:

- **Parent-borne — `inherit` on every leaf fit class except the DyNES fit.**
  `print`, `summary`, `tidy`, `glance`, `coef`, `vcov`, `logLik`,
  `model_terms`: the generics needing only the common fit surface —
  parameters, standard errors, effect names, parameter and event counts, the
  call.
- **No parent default — `override`, per class.** The generics reading a fit's
  estimation internals: `augment`, `fitted`, `predict`, `residuals`,
  `evaluate_model`, `test_gof`, `test_parameter`, `test_time`,
  `diagnose_outliers`, `diagnose_changepoints`, `diagnose_onset`,
  `margin_table`, `simulate`, `times_of`. Withholding a parent default is
  deliberate: a class that omits one gets a dispatch error rather than a method
  reading fields it does not have. `simulate` reads the fit's formula, model and
  sub-model to rebuild a specification and takes the data as an argument,
  because a fit stores neither its specification nor the stocnet it was fitted
  on. `times_of` reads the fit's model descriptor for whether its processes
  carry a clock.
- **Container — `override` on every generic.** A class that holds fits rather
  than being one answers by fanning out over its components, so no cell of its
  column can be `inherit`. `simulate` is the one recorded exception to the
  fan-out shape, not to the verdict: `goldfishFlavFit` overrides it with a
  single competing-flavor run over one clock, because the container's
  processes are competing processes of one specification and a per-component
  list of independent draws would simulate a different model.
- **DyNES fit (`goldfishDynesFit`) — `override` or `refuse` on every generic,
  `inherit` on none.** The class carries the parent so that a generic added to
  the parent later reaches it, but none of today's parent methods is correct
  for it: each reads the single-process fields `names`, `n_params`,
  `log_likelihood`, `final_information_matrix` and `standard_errors`, which a
  fit estimated over a pool of augmented sequences does not have.
  - `override`: `coef` and `vcov` — flat, named by the joint specification's
    composite `process: coefficient` labels, free coefficients only; a
    completed or `offset()` coefficient is never in either, and `vcov` is the
    sandwich covariance from one full-pool evaluation at the returned
    estimate, populated on every returning run, `converged = FALSE`
    included. `coef_layout` — the fit is a joint surface and has per-process
    blocks; every coefficient of the joint specification is a row, a completed
    fid's coefficient a `fixed = TRUE` row at its frozen value with no
    standard error. `simulate` — the fit holds the joint specification it was
    fitted from, so the parent's data-demanding rebuild would ask for data the
    fit already holds and rebuild one fid's formulas out of a joint
    specification; the override simulates the held specification at the
    estimate. `times_of` — answered from the held specification. `print` and
    `summary` — they show the standard error and the Monte-Carlo error side by
    side, grouped per process by `coef_layout`, and state the `converged`
    flag. `tidy` — one row per coefficient with both errors. `glance` — the
    run's convergence, iteration and pool counts, and no information
    criterion.
  - `refuse`: `logLik` — the estimator ascends a Monte-Carlo estimate of the
    observed-data likelihood, so no exact log-likelihood exists to return, and
    `AIC()`/`BIC()` refuse with it. Any log-likelihood total the fit reports in
    future is also a Monte-Carlo estimate. It includes the θ-free per-path
    term of the fixed-only fids (completed and all-`offset()` processes), which
    the pinned-rate contract permits in a reported total and never in the
    optimization objective. `model_terms` — its term table is keyed by a single
    process's effect names; `coef_layout` is the joint fit's term table. The
    estimation-internals generics (`augment`, `fitted`, `predict`, `residuals`,
    `evaluate_model`, `test_gof`, `test_parameter`, `test_time`,
    `diagnose_outliers`, `diagnose_changepoints`, `diagnose_onset`,
    `margin_table`) — each reads the event-level statistics of one observed
    sequence, and a DyNES fit has a weighted pool of augmented sequences
    instead. A later change that defines one of them over the pool, such as
    goodness of fit for DyNES, records an `override` here in place of the
    refusal.
- **Refused.** `coef_layout` on a single-process fit: a single process has no
  per-process blocks, so a layout over it would be an invented shape rather
  than a smaller one.

#### Scenario: an undecided cell is argued during design

- **WHEN** a change adds a fit class, or a generic dispatching on fit classes,
  and this requirement records no verdict for the resulting combination
- **THEN** the change records the verdict here, with its reason where the
  verdict is `override` or `refuse`, before the code is written

#### Scenario: a generic reaching one fit class reaches all of them

- **WHEN** a generic is registered on any fit class or on the parent
- **THEN** every concrete fit class can reach it, through its own method or
  through the parent's, and a class that cannot is a defect rather than an
  omission — including a class deliberately excused, which registers a
  refusing method rather than no method

#### Scenario: simulating a fit needs the data the fit does not carry

- **WHEN** `simulate()` is called on a `goldfishFit` with no `data`
- **THEN** it aborts saying the fit stores its formula and estimates but not
  the data it was fitted on, rather than failing later inside the walk

#### Scenario: simulating a flavored fit is one run, not a fan-out

- **WHEN** `simulate()` is called on a `goldfishFlavFit`
- **THEN** it returns one simulated sequence in which the flavors compete on
  one clock, rather than a per-flavor list of independent simulations

#### Scenario: a DyNES fit refuses a log-likelihood and the information criteria

- **WHEN** `logLik()`, `AIC()` or `BIC()` is called on a `goldfishDynesFit`
- **THEN** the call aborts naming the class and saying that the estimator's
  likelihood is a Monte-Carlo estimate, rather than returning a number

#### Scenario: a DyNES fit simulates from the specification it holds

- **WHEN** `simulate()` is called on a `goldfishDynesFit` with no `data` and no
  `coef`
- **THEN** it simulates the held joint specification at the fit's estimates,
  without asking for data

#### Scenario: a completed fid is a fixed layout row, never a coefficient

- **WHEN** a `goldfishDynesFit` was estimated on a specification with a
  completed pinned rate
- **THEN** `coef_layout()` shows the pinned rate as a `fixed = TRUE` row at its
  frozen value with no standard error, and neither `coef()` nor `vcov()`
  contains it
