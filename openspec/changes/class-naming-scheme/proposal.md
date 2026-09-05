# Class Naming — goldfish Classes Go camelCase

> Revised 2026-08-19: the scheme changed from the `_goldfish` suffix
> (ADR-0020) to the stocnet-ecosystem `goldfish<Thing>` camelCase prefix
> (ADR-0031, supersedes ADR-0020), after autograph@develop shipped the
> rule and the concrete names (commit 03ec996). Scheduled by Alvaro as
> the **next change to fold**, pre-2.0.0, before `parametric-rates`.

## Why

S3 dispatch has one flat, global namespace, so a class name is a claim on
that name for every package a user has attached. goldfish returns eight
objects whose classes are unqualified generic nouns — `test_gof`,
`test_time`, `test_parameter`, `diagnose_onset`, `diagnose_outliers`,
`diagnose_changepoints`, `margin_table`, `evaluate_model` — and
[stocnet/autograph#60](https://github.com/stocnet/autograph/issues/60)
reports the consequence: autograph registers `plot` methods on those
names, the method bodies assume goldfish's columns, and autograph has no
way to tell whose object it received. Where goldfish does qualify a class
it uses a dot (`result.goldfish`, seventeen such classes), which creates
the `plot.test_gof.goldfish` parsing ambiguity the issue itself flags.

The ecosystem has since settled the answer upstream. autograph@develop's
CONTRIBUTING now states the rule for every stocnet package — **name a
class after the package plus a noun, in camelCase** (`<pkg><Thing>`),
following RSiena's `sienaFit`/`sienaGOF`/`sienaAlgorithm` — and commit
03ec996 already renamed autograph's goldfish plot methods to
`goldfishFit`, `goldfishGOF`, `goldfishTimeTest`, `goldfishOutliers`,
`goldfishChangepoints`, `goldfishOnset`, `goldfishMargins`, keeping
goldfish's current names only as defunct aliases. camelCase classes also
dissolve the method-name ambiguity that snake_case classes would keep:
with snake_case generics *and* snake_case classes, a method name like
`augment_seq.flavored_result_goldfish` parses at several generic/class
boundaries; with camelCase classes the dot boundary has exactly one
reading. This line has never gone to CRAN and ADR-0016 licenses the hard
rename until 2.0.0 — the window is closing, which is why this change
folds next.

## What Changes

- **BREAKING** — Every class on the live path is renamed to
  `goldfish<Thing>`: the `goldfish` prefix plus a short camelCase
  identifier that compresses the object (ADR-0031). No fallback class,
  no deprecation shim, per ADR-0016. The seven names autograph@develop
  already dispatches on are adopted verbatim; the rest follow the same
  rule with short identifiers:
  - Fits: `result.goldfish` → `goldfishFit`,
    `flavored_result.goldfish` → `goldfishFlavFit`,
    `summary.result.goldfish` → `goldfishSummFit` (no dot; the base-R
    `summary.<class>` idiom is deliberately not followed — design D6,
    reversed 2026-09-05).
  - Diagnostics: `test_gof` → `goldfishGOF`, `test_time` →
    `goldfishTimeTest`, `test_parameter` → `goldfishParamTest`,
    `diagnose_onset` → `goldfishOnset`, `diagnose_outliers` →
    `goldfishOutliers`, `diagnose_changepoints` →
    `goldfishChangepoints`, `margin_table` → `goldfishMargins`,
    `evaluate_model` → `goldfishEval`.
  - Preprocessing: `preprocessed.goldfish` → `goldfishStat`,
    `preprocessed_db.goldfish` → `goldfishStatDB`,
    `preprocessing.goldfish` → `goldfishPrepCtrl`,
    `flavored_preprocessed.goldfish` → `goldfishFlavPrep`,
    `flavored_statistics.goldfish` → `goldfishFlavStat`.
  - Specification and algorithm: `specification.goldfish` →
    `goldfishSpec`, `spec_map.goldfish` → `goldfishSpecMap`,
    `algorithm.goldfish` → `goldfishAlgo`,
    `algorithm_newton.goldfish` → `goldfishAlgoNewton`.
  - Formulae: `goldfish.formulae` → `goldfishFormulae`.
- Class identifiers are drawn from a fixed short-word vocabulary
  (design D17): `Dn`/`Dni`/`Rem`/`Mu` for the model families, `Cox` for
  ordered, `Coord` for choice coordination, plus `Prep`, `Spec`, `Ctrl`,
  `Algo`, `Flav`, `Summ`. A new class extends that table rather than
  inventing a synonym. This shortens `goldfishAlgorithmNewton` to
  `goldfishAlgoNewton`, `goldfishPrepControl` to `goldfishPrepCtrl`, and
  the three `goldfishFlavored*` names to `goldfishFlav*`.
- **BREAKING** — The rename is **full**: internal classes that never
  leave the package also move (Alvaro, 2026-08-19) — `writer_*` →
  `goldfishWriter*`, `data_source_envir`/`data_source_stocnet` →
  `goldfishSourceEnvir`/`goldfishSourceStocnet`, the `model_spec`
  hierarchy → `goldfishKind*`, `support_constraint_plan` →
  `goldfishSupportPlan`, `fixed_spec`/`initial_spec` →
  `goldfishCoefFixed`/`goldfishCoefInit`. Exactly three exemptions
  remain: the ~60 effect dispatch tags (`inertia`, `recip`, …), which
  stop being classes when `effect-term-registry` replaces string-built
  S3 dispatch, and the deprecated path (below).
- **BREAKING** — `data.goldfish` splits: the `as_goldfish()` stamp on a
  `stocnet` becomes `goldfishData`; the legacy `make_data()`/DyNAMi
  environment keeps `data.goldfish` as a deprecated-path name.
- **BREAKING** — The retired `result.goldfish` name becomes the
  staleness discriminator: `print.result.goldfish` and
  `summary.result.goldfish` survive only as stubs directing the user to
  re-fit; no other generic registers on the old name.
- The four deprecated data classes — `nodes.goldfish`,
  `network.goldfish`, `dependent.goldfish`, `global.goldfish` — are
  **not** renamed (lifecycle exemption; their constructors already
  `deprecate_warn()` toward `manynet::make_stocnet()`).
- The tracked `CLAUDE.md` naming policy is amended in the same change:
  class strings are carved out of the snake_case rule ("camelCase never
  returns" stays true for functions, arguments, and objects; classes
  follow the ecosystem `goldfish<Thing>` rule). Without this the repo
  docs contradict the scheme the day it lands.
- A package-wide class-naming rule enters the living spec, replacing the
  "constructor name, no suffix" convention `residuals-gof` left behind.
- autograph needs **no rename work** — its develop branch moved first;
  goldfish lands on the names autograph already dispatches on, and
  autograph's defunct aliases become deletable afterwards.

## Capabilities

### New Capabilities

- `class-naming`: the package-wide rule — the `goldfish<Thing>` scheme,
  the authoritative rename table, the summary-idiom exception, which
  classes are exempt (deprecated path, effect tags), and how a retired
  class name behaves.

### Modified Capabilities

Forty-four requirements across twenty capabilities name an old class
string. A delta is issued where the requirement's **rule** changes or
the class string **is the subject** of the contract; remaining stale
spellings are corrected by the closing sweep task (design D8). The
sweep also covers the verbatim-copy delta blocks carried by the still-
active `parametric-rates` and `two-sided-coordination` changes, in the
same commit, so their archives reconcile against the post-rename living
text (design D8a).

- `diagnostic-plot-classes`: replaces the "constructor name, no suffix"
  SHALL in place (an additive delta would leave two contradicting
  rules).
- `preprocessing-controls`: return classes `goldfishPrepCtrl` /
  `goldfishStat` are the contract.
- `preprocess-output-writers`: default writer returns `goldfishStat`.
- `model-specification`: `make_specification()` returns `goldfishSpec`.
- `optimizer-selection`: `goldfishAlgo` / `goldfishAlgoNewton`.
- `multimode-networks`: clarifies that `data.goldfish` names the legacy
  environment, distinct from the new `goldfishData` stamp.

## Impact

- **Scheduling**: next to fold (Alvaro, 2026-08-19) — pre-2.0.0,
  after archiving `intercept-only-rate-spec`, before `parametric-rates`
  — so every class the parametric/coordination changes create is born
  under the new rule.
- **goldfish — code**: class-string sites, roxygen `@method`/`@export`
  tags and NAMESPACE across `R/`; `result.goldfish` is the bulk (131
  sites in `R/`, 54 in `tests/`, 95 in `man/`); the internal classes
  add `preprocess_writers.R`, `data_source.R`, `model_spec.R`, and the
  support-constraint/fixed-spec files to the earlier inventory. Lint:
  S3 method names on camelCase classes (`print.goldfishFit`) must pass
  `object_name_linter("snake_case")` — verified or configured in
  groundwork (design D15).
- **goldfish — tests and docs**: testthat class assertions, print
  snapshots, `man/` regeneration, vignette `.Rmd.orig` sources.
- **autograph**: verification only — dispatch of goldfish-built objects
  through autograph@develop's methods; alias deletion is autograph's
  own later cleanup.
- **Users**: any script testing `inherits(x, "result.goldfish")` or the
  bare diagnostic names breaks; stored fits stop dispatching and the
  stubs direct to re-fit.
- **Not affected**: the C++ core (`src/`) knows nothing about R class
  names — no recompile, no risk to the frozen 1e-6 baselines. Effect
  dispatch tags and the deprecated-path classes stay as they are.
