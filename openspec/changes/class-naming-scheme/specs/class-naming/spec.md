## ADDED Requirements

### Requirement: Every class goldfish attaches names the package in camelCase

Every S3 class that goldfish attaches to an object SHALL be the package
name plus a short camelCase identifier that compresses the object —
`goldfish<Thing>` — following the stocnet ecosystem rule (autograph
CONTRIBUTING; RSiena's `sienaFit`/`sienaGOF`/`sienaAlgorithm`
precedent). This replaces both prior conventions (bare constructor
names and the dotted `.goldfish` suffix). The rename table below is
authoritative package-wide: where any other capability in the living
spec still spells a class in a retired form, this table governs.

| Producer | Retired class | Class |
| --- | --- | --- |
| `test_gof()` | `test_gof` | `goldfishGOF` |
| `test_time()` | `test_time` | `goldfishTimeTest` |
| `test_parameter()` | `test_parameter` | `goldfishParamTest` |
| `diagnose_onset()` | `diagnose_onset` | `goldfishOnset` |
| `diagnose_outliers()` | `diagnose_outliers` | `goldfishOutliers` |
| `diagnose_changepoints()` | `diagnose_changepoints` | `goldfishChangepoints` |
| `margin_table()` | `margin_table` | `goldfishMargins` |
| `evaluate_model()` | `evaluate_model` | `goldfishEval` |
| `estimate_dynam()`, `estimate_rem()`, `estimate_dynami()` | `result.goldfish` | `goldfishFit` |
| a flavored specification fit | `flavored_result.goldfish` | `goldfishFlavFit` |
| `summary()` on a fit | `summary.result.goldfish` | `goldfishSummFit` |
| `compute_statistics(output = "preprocessed")` | `preprocessed.goldfish` | `goldfishPrep` |
| `compute_statistics(output = "db")` | `preprocessed_db.goldfish` | `goldfishPrepDB` |
| flavored preprocessing | `flavored_preprocessed.goldfish` | `goldfishFlavPrep` |
| flavored statistics | `flavored_statistics.goldfish` | `goldfishFlavStats` |
| `set_preprocessing()` | `preprocessing.goldfish` | `goldfishPrepCtrl` |
| `make_specification()` | `specification.goldfish` | `goldfishSpec` |
| the specification process map | `spec_map.goldfish` | `goldfishSpecMap` |
| `set_algorithm_newton()` | `algorithm_newton.goldfish` | `goldfishAlgoNewton` |
| the algorithm superclass | `algorithm.goldfish` | `goldfishAlgo` |
| `as_goldfish()` | `data.goldfish` | `goldfishData` |
| formula parsing | `goldfish.formulae` | `goldfishFormulae` |
| preprocess writers (internal) | `writer_*` | `goldfishWriter*` |
| data-source seam (internal) | `data_source_envir`, `data_source_stocnet` | `goldfishSourceEnvir`, `goldfishSourceStocnet` |
| model-spec hierarchy (internal) | `model_spec*` | `goldfishModelSpec*` |
| support-constraint plan (internal) | `support_constraint_plan` | `goldfishSupportPlan` |
| fixing/seeding specs (internal) | `fixed_spec`, `initial_spec` | `goldfishFixedSpec`, `goldfishInitialSpec` |
| `make_joint_specification()` (`make-multivariate-spec`, archived) | `joint_specification.goldfish` | `goldfishJointSpec` |
| `set_parameters()` (`joint-parameters`) | `parameters.goldfish` | `goldfishParams` |
| the pinned intercept-only rate (`intercept-only-rate-spec`) | `intercept_only_rate` | `goldfishCteRate` |
| `walk_open()` (internal; `make-multivariate-spec`) | `walk_handle.goldfish` | `goldfishWalk` |
| joint preprocessing output (internal) | `joint_preprocessed.goldfish` | `goldfishJointPrep` |
| merged compile blocks (internal) | `merged_blocks.goldfish` | `goldfishBlocks` |
| preprocess-writer parent (internal) | `preprocess_writer` | `goldfishWriter` |
| data-source parent (internal) | `data_source` | `goldfishSource` |
| `evaluate_sequence_pool()` (internal; `dynes-augmentation`, not yet landed) | `estep_is` | `goldfishEstepIS` |
| `evaluate_sequence_pool()` (internal; `dynes-augmentation`, not yet landed) | `estep_resampling` | `goldfishEstepResampling` |
| `evaluate_sequence_pool()` (internal; `dynes-augmentation`, not yet landed) | `estep_uniform` | `goldfishEstepUniform` |
| `evaluate_sequence_pool()`'s shared parent (internal; `dynes-augmentation`, not yet landed) | `dynes_estep` | `goldfishDynesEstep` |

Rows beyond the first block were added after this table's initial draft
(design D16, extended 2026-09-05): the first two name classes that were already live in `R/` under
the retired convention by the time this change's rename inventory
(task 1.1) runs, so they get the same hand-edited rename as every other
row above; the last four name a class family proposed by an
unimplemented change (`dynes-augmentation`, shared with `abmcem`) whose
own spec text already spells the `goldfish<Thing>` form directly — no
rename is needed there, only a matching implementation.

The seven classes autograph@develop already dispatches on
(`goldfishFit`, `goldfishGOF`, `goldfishTimeTest`, `goldfishOutliers`,
`goldfishChangepoints`, `goldfishOnset`, `goldfishMargins`) SHALL be
adopted with exactly those spellings. The exported function names SHALL
NOT change: `test_gof()` remains `test_gof()`; only the class of what it
returns moves.

#### Scenario: a diagnostic object names the package

- **WHEN** `test_gof(fit)` is called
- **THEN** the returned object inherits `goldfishGOF` and does not
  inherit `test_gof`

#### Scenario: a fitted model names the package

- **WHEN** any `estimate_*()` returns a fit
- **THEN** the object inherits `goldfishFit` and does not inherit
  `result.goldfish`

#### Scenario: autograph dispatch lands on the new methods

- **WHEN** an object built by goldfish is plotted with
  autograph@develop attached
- **THEN** dispatch reaches autograph's `plot.goldfish<Thing>` method
  directly, not a defunct alias

#### Scenario: the constructors keep their names

- **WHEN** the package namespace is inspected after the rename
- **THEN** `test_gof`, `test_time`, `test_parameter`, `diagnose_onset`,
  `diagnose_outliers`, `diagnose_changepoints`, `margin_table` and
  `evaluate_model` are still exported functions under those exact names

### Requirement: No goldfish class carries a dot-suffix package qualifier

No class that goldfish attaches on the live path SHALL contain a dot at
all. A dot creates no S3 inheritance — dispatch is on exact strings — so a
dotted qualifier is convention masquerading as structure, and it produces
method-name ambiguity (`plot.test_gof.goldfish` parses two ways). The rule
admits **no exception**, the base-R `summary.<class>` idiom included: the
object `summary()` returns on a fit SHALL be classed `goldfishSummFit`,
printed by `print.goldfishSummFit()`. Following base R here would make a
single class both a method name and the name of the class that method
returns, and would give `print.summary.goldfishFit` three dots to read
through, for conformity this package does not need.

#### Scenario: the summary object carries no dot

- **WHEN** `summary()` is called on a fitted model
- **THEN** the returned object inherits `goldfishSummFit`, and `print()`
  on it dispatches to `print.goldfishSummFit`

#### Scenario: no live class carries a dotted package suffix

- **WHEN** the class strings goldfish attaches on the live path are
  enumerated
- **THEN** none contains a dot, the deprecated-path classes named in the
  exemption requirement excepted

### Requirement: Method names parse uniquely — snake_case generic, camelCase class

Every S3 method goldfish registers on its own classes SHALL be the
snake_case generic, a dot, and the camelCase class
(`print.goldfishFit`, `diagnose_onset.goldfishFit`), so the
generic/class boundary is the unique case transition and no method name
has more than one parse. The lint configuration SHALL accept these
method names without per-line suppressions.

#### Scenario: method names are unambiguous

- **WHEN** the NAMESPACE `S3method()` entries for goldfish classes are
  enumerated
- **THEN** each names a snake_case generic and a camelCase class, and
  no entry's string admits a second generic/class split at a snake_case
  boundary

### Requirement: Exemptions are the deprecated path and the effect dispatch tags

Classes on the deprecated path SHALL keep their existing names:
`nodes.goldfish`, `network.goldfish`, `dependent.goldfish`,
`global.goldfish` (constructors already `deprecate_warn()` toward
`manynet::make_stocnet()`), and the legacy `data.goldfish` environment
built by `make_data()` and DyNAMi. The effect dispatch tags (`inertia`,
`recip`, `trans`, and their siblings) SHALL also keep their names:
their class role is retired wholesale by the effect registry, so
renaming them would churn a mechanism scheduled for deletion. Every
other internal class SHALL follow the `goldfish<Thing>` rule — the
rename is otherwise full, exported or not.

#### Scenario: a deprecated constructor keeps its class

- **WHEN** `make_network()` is called
- **THEN** it warns as deprecated and returns an object classed
  `network.goldfish`, unchanged by this rule

#### Scenario: an effect tag is unaffected

- **WHEN** an effect term is dispatched through `init_DyNAM_choice()`
- **THEN** the dispatch tag is the bare effect name, with no prefix

#### Scenario: an internal class follows the rule

- **WHEN** the default preprocess writer builds its output object
- **THEN** the writer's own dispatch class is a `goldfishWriter*` name,
  not a bare `writer_*` name

### Requirement: The as_goldfish stamp and the legacy environment are distinct classes

The class `as_goldfish()` stamps on a validated `stocnet` object SHALL
be `goldfishData`, distinct from the `data.goldfish` class carried by
the legacy environment that `make_data()` and the DyNAMi path build.
Print dispatch SHALL split with the classes, so the two objects no
longer share a print method.

#### Scenario: the stamp and the environment are distinguishable

- **WHEN** `as_goldfish()` stamps a `stocnet` and `make_data()` builds
  a legacy environment in the same session
- **THEN** the first inherits `goldfishData`, the second inherits
  `data.goldfish`, and neither inherits the other's class

#### Scenario: each prints under its own method

- **WHEN** each of the two objects is printed
- **THEN** each dispatches to the print method written for its own
  class

### Requirement: A retired class name carries only a diagnostic stub

A class name retired by this rule SHALL NOT be attached to any newly
built object, and SHALL carry no method other than a stub that explains
and stops. There is no fallback class: a renamed object carries the new
class only. Because objects fitted by an earlier goldfish still carry
`result.goldfish`, `print.result.goldfish` and
`summary.result.goldfish` SHALL be retained as stubs; no other generic
SHALL register on the retired name — `coef()`, `logLik()`, `vcov()`,
`predict()`, `residuals()`, `augment()`, `tidy()`, `glance()` and the
diagnostics give R's own dispatch error.

The stub SHALL diagnose by the object's recorded format epoch rather
than by its class alone: an object with no epoch predates the
snake_case component rename and is reported as such; an object whose
epoch is current has only a retired class name and SHALL be told
exactly that, not that its components were renamed. The epoch counters
SHALL NOT move, because no component of any object changes.

#### Scenario: a stored fit from a released goldfish

- **WHEN** `print()` is called on a fit saved by goldfish 1.7.0
  (carries `result.goldfish`, records no format epoch)
- **THEN** the stub reports that the components were renamed and the
  object must be re-fitted

#### Scenario: a stored fit from the development line

- **WHEN** `print()` is called on a fit saved by goldfish 1.9.x
  (carries `result.goldfish`, records the current epoch)
- **THEN** the stub reports that the class was renamed and the object
  must be re-fitted, and does not claim its components were renamed

#### Scenario: no other generic answers on the retired name

- **WHEN** `coef()` is called on an object classed `result.goldfish`
- **THEN** R raises its own "no applicable method" error

#### Scenario: a renamed object carries no fallback class

- **WHEN** any renamed object's class vector is inspected
- **THEN** it contains the `goldfish<Thing>` class and not the retired
  name
