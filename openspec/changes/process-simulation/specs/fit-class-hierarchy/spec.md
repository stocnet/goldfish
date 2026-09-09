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
  `tidy`, `glance`, `coef`, `vcov`, `logLik`, `model_terms`: the generics
  needing only the common fit surface — parameters, standard errors, effect
  names, parameter and event counts, the call.
- **No parent default — `override`, per class.** The generics reading a fit's
  estimation internals: `augment`, `fitted`, `predict`, `residuals`,
  `evaluate_model`, `test_gof`, `test_parameter`, `test_time`,
  `diagnose_outliers`, `diagnose_changepoints`, `diagnose_onset`,
  `margin_table`, `simulate`. Withholding a parent default is deliberate: a
  class that omits one gets a dispatch error rather than a method reading
  fields it does not have.
- **Container — `override` on every generic.** A class that holds fits rather
  than being one answers by fanning out over its components, so no cell of its
  column can be `inherit`. `simulate` is the one recorded exception to the
  fan-out shape, not to the verdict: `goldfishFlavFit` overrides it with a
  single competing-flavor run over one clock, because the container's
  processes are competing processes of one specification and a per-component
  list of independent draws would simulate a different model.
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

#### Scenario: simulating a flavored fit is one run, not a fan-out

- **WHEN** `simulate()` is called on a `goldfishFlavFit`
- **THEN** it returns one simulated sequence in which the flavors compete on
  one clock, rather than a per-flavor list of independent simulations
