# fit-class-hierarchy Specification

## Purpose
The fitted-model classes share a parent, so a generic written once reaches
every fit. This capability records what that inheritance is allowed to do:
one verdict per generic and fit class — `inherit`, `override` or `refuse` —
so a generic that is not meaningful for a class aborts with a reason rather
than returning a plausible number, and a new fit class or generic has its
semantics argued while the change is being designed.
## Requirements
### Requirement: Fitted-model classes share a parent

The fitted-model classes SHALL share a parent class, so that a generic
implemented once on the parent reaches every fit class unless that class is
deliberately excused. The parent SHALL NOT be introduced without the
per-class contract below, because inheritance alone is what allows a generic
to produce a value for a class where the value is not meaningful.

#### Scenario: a generic written once reaches every fit class

- **WHEN** a generic is implemented on the parent and no class excuses it
- **THEN** calling it on a single-process fit, a flavored fit and a DyNES fit
  all reach that implementation

#### Scenario: the leaked generics reach flavored fits

- **WHEN** `summary()`, `tidy()` or `glance()` is called on a flavored fit
- **THEN** it returns a result rather than failing with no applicable method,
  closing the gap that existed while the classes were flat

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
  `margin_table`. Withholding a parent default is deliberate: a class that
  omits one gets a dispatch error rather than a method reading fields it does
  not have.
- **Container — `override` on every generic.** A class that holds fits rather
  than being one answers by fanning out over its components, so no cell of its
  column can be `inherit`.
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

### Requirement: A generic that is not meaningful for a class refuses

A generic SHALL be recorded as `refuse` where it would otherwise return a
value that is not meaningful for a fit class, and the call SHALL abort with a
message naming the class and the reason, rather than returning a value. This
SHALL apply in particular where a fit maximizes a Monte-Carlo estimate of the
likelihood rather than an exact likelihood: a quantity that is not an exact
log-likelihood SHALL NOT reach `AIC()` or `BIC()` through inheritance, because
those return numbers that appear to be information criteria and are not.

#### Scenario: a Monte-Carlo fit does not silently supply a log-likelihood

- **WHEN** `logLik()` is called on a fit whose likelihood is a Monte-Carlo
  estimate, and the recorded verdict is `refuse`
- **THEN** the call aborts explaining that the quantity is an estimate, and
  `AIC()` and `BIC()` therefore cannot return a value for that fit

#### Scenario: refusing is louder than having no method

- **WHEN** a generic is refused for a class
- **THEN** the error names the class and the reason, rather than R's generic
  "no applicable method" message

