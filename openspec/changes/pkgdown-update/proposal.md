# pkgdown reference index and examples gate

## Why

The 2026-07-25 exported-surface audit (`.plan/Naming_guidelines.md` §8)
found the pkgdown reference index drifting from the API: the planned
`test_gof()` / `test_parameter()` / `test_time()` and `evaluate_model()`
have no index home (no `test_*` pattern exists in `_pkgdown.yml`), the
`augment` topic is unindexed (its wiring fix lands in residuals-gof), and
nothing verifies that documented examples actually run — or how long they
take, which matters both for the site build and for the CRAN ~5s-per-topic
expectation ahead of the 2.0.0 submission.

## What Changes

- **Reference index update** in `_pkgdown.yml`: the "Diagnostics" section
  gains `starts_with("test")` and `evaluate_model`; the post-estimation
  methods topic set (including the re-wired `augment`) is indexed; every
  non-internal topic is covered, verified by `pkgdown::check_pkgdown()`.
- **Examples-run gate**: every documented example on a non-internal topic
  runs green via `devtools::run_examples()` (including `\donttest` blocks
  locally), added as a pre-flight step alongside the NOT_CRAN test gate.
- **Examples timing ledger**: per-topic elapsed time is measured on each
  gate run and appended to a local `.plan/example_timings.csv`
  (topic, seconds, date, package version); topics exceeding ~5s wrap the
  slow block in `\donttest` with a rationale comment, so the CRAN check
  stays fast without deleting the example.
- **Site build sanity**: `pkgdown::build_reference()` completes without
  warnings on the updated yaml.

## Capabilities

### New Capabilities

- `pkgdown-reference`: the reference-index completeness contract (every
  non-internal topic indexed, checked by `pkgdown::check_pkgdown()`), the
  examples-run gate, and the per-topic timing ledger with the ~5s
  `\donttest` policy.

### Modified Capabilities

None — documentation infrastructure only; no exported behavior changes.

## Impact

- **Files**: `_pkgdown.yml`; possibly `\donttest` wraps + rationale
  comments in roxygen examples of slow topics (with
  `devtools::document()` inline); `.plan/example_timings.csv` (new,
  local-only working ledger).
- **Sequencing**: apply AFTER residuals-gof phase 2 lands — the
  `test_*` / `evaluate_model` / `diagnose_onset` man pages must exist for
  `check_pkgdown()` to pass, and the `augment` wiring fix (residuals-gof
  task 2.5) must precede its indexing.
- **Dependencies**: none added (pkgdown is already the site tool;
  survival/mlogit example guards are owned by revise-gather-output).
- **Out of scope**: vignette/article structure, site theming, the
  `release-prep` pre-flight itself (this gate becomes one of its steps).
