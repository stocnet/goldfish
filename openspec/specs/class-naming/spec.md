# class-naming Specification

## Purpose
Every S3 class goldfish attaches is the package name plus a short camelCase
noun — `goldfishFit`, `goldfishSpec`, `goldfishStat`. This capability fixes
that scheme, its three exemptions (the deprecated-path classes, the effect
dispatch tags, and the `goldfish_<snake_case>` condition classes), and the
requirement that the guard enumerate the package's own classes rather than
read a table of them.
## Requirements
### Requirement: Every class goldfish attaches names the package in camelCase

Every S3 class that goldfish attaches to an object SHALL be the package
name plus a short camelCase identifier that compresses the object —
`goldfish<Thing>` — following the stocnet ecosystem rule (autograph
CONTRIBUTING; RSiena's `sienaFit`/`sienaGOF`/`sienaAlgorithm`
precedent). This replaces both prior conventions (bare constructor
names and the dotted `.goldfish` suffix). This rule, not the table
below, is what conformance is measured against: the table records what
each existing class becomes, and a class created after the table was
written is governed by the rule whether or not it appears there. Where
any other capability in the living spec still spells a class in a
retired form, the table governs the spelling.

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
| `compute_statistics(output = "preprocessed")` | `preprocessed.goldfish` | `goldfishStat` |
| `compute_statistics(output = "db")` | `preprocessed_db.goldfish` | `goldfishStatDB` |
| flavored preprocessing | `flavored_preprocessed.goldfish` | `goldfishFlavPrep` |
| flavored statistics | `flavored_statistics.goldfish` | `goldfishFlavStat` |
| `set_preprocessing()` | `preprocessing.goldfish` | `goldfishPrepCtrl` |
| `make_specification()` | `specification.goldfish` | `goldfishSpec` |
| the specification process map | `spec_map.goldfish` | `goldfishSpecMap` |
| `set_algorithm_newton()` | `algorithm_newton.goldfish` | `goldfishAlgoNewton` |
| the algorithm superclass | `algorithm.goldfish` | `goldfishAlgo` |
| `as_goldfish()` | `data.goldfish` | `goldfishData` |
| formula parsing | `goldfish.formulae` | `goldfishFormulae` |
| preprocess writers (internal) | `writer_default`, `writer_gather`, `writer_db` | `goldfishWriterDefault`, `goldfishWriterGather`, `goldfishWriterDB` |
| data-source seam (internal) | `data_source_envir`, `data_source_stocnet` | `goldfishSourceEnvir`, `goldfishSourceStocnet` |
| model-kind parent (internal) | `model_spec` | `goldfishKind` |
| model-kind variants (internal) | `dynam_rate_spec`, `dynam_rate_ordered_spec`, `dynam_choice_spec`, `dynam_choice_coord_spec` | `goldfishKindDnRate`, `goldfishKindDnCox`, `goldfishKindDnChoice`, `goldfishKindDnCoord` |
| model-kind variants (internal) | `dynami_rate_spec`, `dynami_rate_ordered_spec`, `dynami_choice_spec` | `goldfishKindDniRate`, `goldfishKindDniCox`, `goldfishKindDniChoice` |
| model-kind variants (internal) | `rem_rate_spec`, `rem_rate_ordered_spec` | `goldfishKindRemRate`, `goldfishKindRemCox` |
| risk-set axis (internal) | `sender_spec`, `dyad_spec` | `goldfishAxisSender`, `goldfishAxisDyad` |
| support-constraint plan (internal) | `support_constraint_plan` | `goldfishSupportPlan` |
| fixed / seeded coefficient carriers (internal) | `fixed_spec`, `initial_spec` | `goldfishCoefFixed`, `goldfishCoefInit` |
| `make_joint_specification()` (`make-multivariate-spec`, archived) | `joint_specification.goldfish` | `goldfishJointSpec` |
| `set_parameters()` (`joint-parameters`) | `parameters.goldfish` | `goldfishParams` |
| the pinned intercept-only rate (`intercept-only-rate-spec`) | `intercept_only_rate` | `goldfishCteRate` |
| `walk_open()` (internal; `make-multivariate-spec`) | `walk_handle.goldfish` | `goldfishWalk` |
| joint preprocessing output (internal) | `joint_preprocessed.goldfish` | `goldfishJointPrep` |
| merged compile blocks (internal) | `merged_blocks.goldfish` | `goldfishBlock` |
| preprocess-writer parent (internal) | `preprocess_writer` | `goldfishWriter` |
| data-source parent (internal) | `data_source` | `goldfishSource` |
| DyNAM-i interaction updates (internal) | `interaction.network.updates` | `goldfishInterNet` |
| DyNAM-i group updates (internal) | `interaction.groups.updates` | `goldfishInterGrp` |
| DyNAM-i windowed interaction updates (internal) | `windowed.interaction.network.updates` | `goldfishInterWindow` |
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

### Requirement: Conformance is enumerated from the source, not read from the table

A guard test SHALL enumerate the classes goldfish actually attaches — from the
`S3method()` registrations, from the literal strings at every
class-assignment form (`class(x) <-`, `structure(class = )` and
`attr(x, "class") <-`), and from the literal strings in `inherits()` and
`is()` calls — subtract the exempt categories, and assert that every remaining class matches
the `goldfish<Thing>` form. Conformance SHALL NOT be established by checking
the rename table, because a class minted after the table was written would
pass by omission. The enumeration is best-effort and cannot be proven
complete; a non-conforming class discovered outside it SHALL be treated as a
gap in the exemption list rather than as a table update.

#### Scenario: a newly minted class that breaks the rule fails the guard

- **WHEN** a class not matching `goldfish<Thing>` is attached anywhere in the
  package and is not in an exempt category
- **THEN** the guard test fails and names it, whether or not the rename table
  mentions it

#### Scenario: the table is not the completeness criterion

- **WHEN** the guard test runs
- **THEN** it derives the class list from the package source, and passing does
  not depend on the rename table being current

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

### Requirement: One form for stamping a class and one for testing it

Class stamping SHALL use `class(x) <-` when setting the class of an existing
object and `structure(x, class = )` when creating and classing in one
expression; `attr(x, "class") <-` SHALL NOT be used, being the same operation
written longer with no benefit and, more importantly, a form that a source
enumeration is likely to miss. Class testing SHALL use `inherits()`;
`methods::is()` SHALL NOT be used for a class test, and `%in% class(x)` SHALL
NOT be used at all.

This rule exists to make the enumeration above reliable, not for tidiness: a
class stamped through an unenumerated form is invisible to the guard test, and
three `result.goldfish` stamps were found in exactly that position.

#### Scenario: no class is stamped through attr

- **WHEN** the package source is searched for `attr(x, "class") <-`
- **THEN** no occurrence remains, and every class-stamping site uses
  `class(x) <-` or `structure(x, class = )`

#### Scenario: class tests use one predicate

- **WHEN** the package source is searched for class tests
- **THEN** each uses `inherits()`, with no `methods::is()` class test and no
  `%in% class(x)` construction

### Requirement: Exemptions are the deprecated path, the effect tags, and condition classes

Condition classes SHALL keep their `goldfish_<snake_case>` names. A condition
is matched on its class vector by `tryCatch()` and `expect_error(class = )`
and is never dispatched on, so it does not compete for the S3 method namespace
this rule exists to protect; the names are already package-qualified, so the
collision the scheme prevents cannot arise; and snake_case is the R
ecosystem's convention for conditions (`rlang_error`, `vctrs_error_cast_lossy`).
Renaming them would break every caller matching on a class string and buy
nothing.

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

#### Scenario: a condition class keeps its snake_case name

- **WHEN** an error or warning condition goldfish signals is caught by class
- **THEN** the class string is the `goldfish_<snake_case>` name it has today,
  unchanged by this rule, so `expect_error(class = )` and `tryCatch()` callers
  keep working

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

