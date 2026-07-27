# pkgdown-reference Specification

## ADDED Requirements

### Requirement: Reference index covers every non-internal topic
The `_pkgdown.yml` reference index SHALL cover every non-internal
documentation topic, verified by a clean `pkgdown::check_pkgdown()`: the
"Diagnostics" section SHALL include `starts_with("test")` and
`evaluate_model` alongside the existing `starts_with("diagnose")`, and
the post-estimation method topics (including the re-wired `augment`)
SHALL be indexed. Internal-keyword topics (deprecated/defunct shims)
remain exempt.

#### Scenario: check_pkgdown is green
- **WHEN** `pkgdown::check_pkgdown()` runs after residuals-gof phase 2
  and this change
- **THEN** it reports no missing topics, and `test_gof`,
  `test_parameter`, `test_time`, `evaluate_model`, and `diagnose_onset`
  resolve to entries in the Diagnostics section.

#### Scenario: reference builds clean
- **WHEN** `pkgdown::build_reference()` runs on the updated yaml
- **THEN** it completes without warnings.

### Requirement: Documented examples run green
Every example on a non-internal topic SHALL run without error via
`devtools::run_examples()` (executed in a fresh subprocess, `\donttest`
blocks included). The gate SHALL run before release pre-flight and
whenever example-bearing roxygen changes.

#### Scenario: examples gate passes
- **WHEN** the examples gate runs on the package
- **THEN** every topic's example completes without error and the run
  reports the per-topic elapsed times.

### Requirement: Per-topic example timings are recorded with a slow-example policy
Each examples-gate run SHALL measure elapsed seconds per topic and append
`topic, seconds, date, package version` rows to the local
`.plan/example_timings.csv`. A topic whose non-`\donttest` example code
exceeds approximately 5 seconds SHALL wrap the slow block in `\donttest`
with a one-line rationale comment; examples SHALL NOT be deleted to meet
the threshold.

#### Scenario: timings appended
- **WHEN** the examples gate completes
- **THEN** `.plan/example_timings.csv` gains one row per timed topic for
  this run, carrying the current package version.

#### Scenario: slow topic wrapped, not deleted
- **WHEN** a topic's example measures over ~5 seconds outside `\donttest`
- **THEN** the slow block is wrapped in `\donttest` with a rationale
  comment and the remaining fast code still demonstrates the function.
