## ADDED Requirements

### Requirement: Every class goldfish returns carries the package name

Every S3 class that goldfish attaches to an object a user can hold SHALL carry the
package name as a snake_case `_goldfish` suffix appended to the name of the
exported constructor that produces it. This replaces the constructor-name
convention (bare constructor name, no suffix) and the dotted `.goldfish`
convention, both of which the package previously ran at the same time. The
rename table below is authoritative package-wide: where any other capability in
the living spec still spells a class in a retired form, the spelling in this
table governs.

| Producer | Retired class | Class |
| --- | --- | --- |
| `test_gof()` | `test_gof` | `test_gof_goldfish` |
| `test_time()` | `test_time` | `test_time_goldfish` |
| `test_parameter()` | `test_parameter` | `test_parameter_goldfish` |
| `diagnose_onset()` | `diagnose_onset` | `diagnose_onset_goldfish` |
| `diagnose_outliers()` | `diagnose_outliers` | `diagnose_outliers_goldfish` |
| `diagnose_changepoints()` | `diagnose_changepoints` | `diagnose_changepoints_goldfish` |
| `margin_table()` | `margin_table` | `margin_table_goldfish` |
| `evaluate_model()` | `evaluate_model` | `evaluate_model_goldfish` |
| `estimate_dynam()`, `estimate_rem()`, `estimate_dynami()` | `result.goldfish` | `result_goldfish` |
| a flavored specification fit | `flavored_result.goldfish` | `flavored_result_goldfish` |
| `summary()` on a fit | `summary.result.goldfish` | `summary_result_goldfish` |
| `compute_statistics(output = "preprocessed")` | `preprocessed.goldfish` | `preprocessed_goldfish` |
| `compute_statistics(output = "db")` | `preprocessed_db.goldfish` | `preprocessed_db_goldfish` |
| flavored preprocessing | `flavored_preprocessed.goldfish` | `flavored_preprocessed_goldfish` |
| flavored statistics | `flavored_statistics.goldfish` | `flavored_statistics_goldfish` |
| `set_preprocessing()` | `preprocessing.goldfish` | `preprocessing_goldfish` |
| `make_specification()` | `specification.goldfish` | `specification_goldfish` |
| the specification process map | `spec_map.goldfish` | `spec_map_goldfish` |
| `set_algorithm_newton()` | `algorithm_newton.goldfish` | `algorithm_newton_goldfish` |
| the algorithm superclass | `algorithm.goldfish` | `algorithm_goldfish` |
| `as_goldfish()` | `data.goldfish` | `data_goldfish` |

The exported function names SHALL NOT change. `test_gof()` remains
`test_gof()`; only the class of the object it returns moves.

#### Scenario: a diagnostic object names the package

- **WHEN** `test_gof(fit)` is called
- **THEN** the returned object inherits `test_gof_goldfish` and does not
  inherit `test_gof`

#### Scenario: a fitted model names the package

- **WHEN** any `estimate_*()` returns a fit
- **THEN** the object inherits `result_goldfish` and does not inherit
  `result.goldfish`

#### Scenario: the constructors keep their names

- **WHEN** the package namespace is inspected after the rename
- **THEN** `test_gof`, `test_time`, `test_parameter`, `diagnose_onset`,
  `diagnose_outliers`, `diagnose_changepoints`, `margin_table` and
  `evaluate_model` are still exported functions under those exact names

### Requirement: No goldfish class name contains a dot

No class name that goldfish attaches to an object on the live path SHALL contain
a `.` character. A dotted class produces a genuine S3 ambiguity — the string
`plot.test_gof.goldfish` reads equally as a `plot()` method on
`test_gof.goldfish` and as a `goldfish` method of a `plot.test_gof` generic — and
exempts classes from the snake_case policy that governs every other user-facing
name in the package.

This rule applies to the summary object as well, so the object `summary()`
returns SHALL be classed `summary_result_goldfish` rather than base R's
`summary.<class>` idiom. The method remains `summary.result_goldfish()`; only the
class of its return value is affected.

#### Scenario: the summary object is dot-free

- **WHEN** `summary()` is called on a fitted model
- **THEN** the returned object inherits `summary_result_goldfish`, and
  `print()` on it dispatches to `print.summary_result_goldfish`

#### Scenario: no live class carries a dot

- **WHEN** the class strings goldfish attaches on the live path are enumerated
- **THEN** none contains a `.`, the deprecated-path classes named in the
  exemption requirement excepted

### Requirement: Deprecated-path classes are exempt and internal classes are recommended

Classes on the deprecated path SHALL keep their existing names, and internal
classes that never leave the package SHOULD but need not follow the suffix rule.
Scope is drawn by lifecycle rather than by class family: a class is renamed
because it is on the live path, not because of what kind of object it labels.

Exempt, unchanged: `nodes.goldfish`, `network.goldfish`, `dependent.goldfish`
and `global.goldfish`, whose constructors already `deprecate_warn()` toward
`manynet::make_stocnet()`; and the legacy `data.goldfish` environment built by
`make_data()` and DyNAMi. Renaming a name scheduled for deletion spends churn in
the package, in user scripts and in the vignettes, and buys nothing that
survives the stocnet migration.

Internal dispatch classes — the effect tags (`inertia`, `recip`, `trans`, and
their siblings), `writer_default` / `writer_gather` / `writer_db`,
`data_source_envir` / `data_source_stocnet`, the `model_spec` hierarchy,
`support_constraint_plan`, and `fixed_spec` / `initial_spec` — SHALL NOT be
required to carry the suffix, because they are never attached to an object that
reaches a user. `goldfish.formulae` SHALL be renamed to `formulae_goldfish`
notwithstanding, because it is a single site and its current spelling states the
convention backwards.

#### Scenario: a deprecated constructor keeps its class

- **WHEN** `make_network()` is called
- **THEN** it warns as deprecated and returns an object classed
  `network.goldfish`, unchanged by this rule

#### Scenario: an internal dispatch class is unaffected

- **WHEN** an effect term is dispatched through `init_DyNAM_choice()`
- **THEN** the dispatch tag is the bare effect name, with no suffix

### Requirement: The as_goldfish stamp and the legacy environment are distinct classes

The class `as_goldfish()` stamps on a validated `stocnet` object SHALL be
`data_goldfish`, distinct from the `data.goldfish` class carried by the legacy
environment that `make_data()` and the DyNAMi path build. One name currently
serves both objects, so a print method or an `inherits()` guard written for
either receives the other.

Print dispatch SHALL split with the classes, so the legacy environment and the
stamped `stocnet` no longer share a print method.

#### Scenario: the stamp and the environment are distinguishable

- **WHEN** `as_goldfish()` stamps a `stocnet` and `make_data()` builds a legacy
  environment in the same session
- **THEN** the first inherits `data_goldfish`, the second inherits
  `data.goldfish`, and neither inherits the other's class

#### Scenario: each prints under its own method

- **WHEN** each of the two objects is printed
- **THEN** each dispatches to the print method written for its own class

### Requirement: A retired class name carries only a diagnostic stub

A class name retired by this rule SHALL NOT be attached to any newly built
object, and SHALL carry no method other than a stub that explains the situation
and stops. There is no fallback class: a renamed object carries the new class
only. Retaining the old name as a trailing class element would re-claim the
global name the rename exists to release.

Because objects fitted by an earlier goldfish still carry `result.goldfish`,
`print.result.goldfish` and `summary.result.goldfish` SHALL be retained as stubs,
so that the first thing a user does with a stored fit produces a diagnosis rather
than R's bare `no applicable method`. No other generic SHALL register on the
retired name; `coef()`, `logLik()`, `vcov()`, `predict()`, `residuals()`,
`augment()`, `tidy()`, `glance()` and the diagnostics SHALL give R's own dispatch
error, which is accurate because no method exists and the object cannot be
repaired.

The stub SHALL diagnose by the object's recorded format epoch rather than by its
class alone, because two populations wear the retired class and only one of them
has the fault the existing message describes. An object with no epoch predates
the snake_case component rename and is reported as such; an object whose epoch is
current has only a retired class name, and SHALL be reported as such rather than
being told its components were renamed, which would be false about the object in
the user's hand. The epoch counters themselves SHALL NOT move, because no
component of any object changes.

#### Scenario: a stored fit from a released goldfish

- **WHEN** `print()` is called on a fit saved by goldfish 1.7.0, which carries
  `result.goldfish` and records no format epoch
- **THEN** the stub reports that the components were renamed and that the object
  must be re-fitted

#### Scenario: a stored fit from the development line

- **WHEN** `print()` is called on a fit saved by goldfish 1.9.x, which carries
  `result.goldfish` and records the current format epoch
- **THEN** the stub reports that the class was renamed and that the object must
  be re-fitted, and does not claim that its components were renamed

#### Scenario: no other generic answers on the retired name

- **WHEN** `coef()` is called on an object classed `result.goldfish`
- **THEN** R raises its own "no applicable method" error, because goldfish
  registers no `coef` method on the retired class

#### Scenario: a renamed object carries no fallback class

- **WHEN** any renamed object's class vector is inspected
- **THEN** it contains the `_goldfish` class and not the retired name
