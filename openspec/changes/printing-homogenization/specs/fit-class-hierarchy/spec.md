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

- **Parent-borne — `inherit` on every leaf fit class.** `print`, `summary`,
  `tidy`, `glance`, `coef`, `vcov`, `logLik`, `nobs`, `confint`,
  `coef_layout`: the generics needing only the common fit
  surface — parameters, standard errors, effect names, parameter and event
  counts, the call. `coef` and `vcov` are flat on every class (a named
  vector and a matrix in fid order; `process =` selects one block); `nobs`
  returns the dependent-event count `logLik` carries as its `nobs`
  attribute; `confint` returns Wald intervals from the flat `coef` and
  `vcov`; `coef_layout` on a single-process fit returns one block, and it
  absorbs the former `model_terms` generic, which is removed.
- **No parent default — `override`, per class.** The generics reading a fit's
  estimation internals: `augment`, `fitted`, `predict`, `residuals`,
  `evaluate_model`, `test_gof`, `test_parameter`, `test_time`,
  `diagnose_outliers`, `diagnose_changepoints`, `diagnose_onset`,
  `margin_table`. Withholding a parent default is deliberate: a class that
  omits one gets a dispatch error rather than a method reading fields it
  does not have. `test_parameter` carries the score, Wald and
  likelihood-ratio types of one restriction test, so a Monte-Carlo fit
  class decides each type in its own method; no `anova` method is
  registered for any fit class.
- **Container — `override` on every generic.** A class that holds fits rather
  than being one answers by fanning out over its components, so no cell of its
  column can be `inherit`. Its `coef()` and `vcov()` concatenate the
  components into the flat shape (the covariance block-diagonal while the
  processes are separable); its `summary()` returns a classed list
  (`goldfishSummFlavFit`) so the container's print renders once as one
  model; its `nobs()` and `logLik()` sum the components; its `confint()` is
  the parent's computation on the flat objects.
- **Refused.** No cell is refused on the two shipped classes: the former
  refusal of `coef_layout` on a single-process fit is withdrawn (one block
  is a smaller layout, not an invented one). The refuse mechanism stays,
  its next live cell being the DyNES `logLik`.
- **Deferred with the class.** The DyNES fit's cells for `logLik`, `nobs`,
  `confint` and the `test_parameter` types are recorded by the change that ships that
  class; a Monte-Carlo estimate reaching any of them through inheritance
  is the hazard the refuse verdict exists for.

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

#### Scenario: the information-criterion and Wald contracts are pinned

- **WHEN** `AIC()`, `BIC()`, `nobs()`, `confint()`, `lmtest::lrtest()` and
  `lmtest::waldtest()` are called on a single-process fit and on a flavored
  container
- **THEN** each returns a value computed from the `logLik` object's `df`
  and `nobs` attributes or from the flat `coef()`/`vcov()`, a test asserts
  the attributes equal the free-parameter count and the dependent-event
  count, and on a separable container `AIC()` equals the sum of its
  processes' `AIC()`

#### Scenario: a nested comparison refuses an unrelated pair

- **WHEN** `test_parameter(fit_a, type = "lr", null = fit_b)` is called on
  two fits whose dependent layers or event counts differ, or whose terms
  are not nested
- **THEN** the call aborts naming the guard that failed, rather than
  returning a likelihood-ratio statistic
