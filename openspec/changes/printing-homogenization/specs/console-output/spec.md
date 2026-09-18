## ADDED Requirements

### Requirement: User-facing output on a live path renders via cli at the call site

The package SHALL render every print method, message, warning and error on
a non-deprecated code path with cli semantic elements — `cli_abort()`,
`cli_warn()`, `cli_inform()`, `cli_alert_*()`, `cli_text()`, `cli_dl()`,
`cli_bullets()`, `cli_rule()` and `cli_progress_*()` — called directly at
the site that has the data, with arguments, values, classes and counts
interpolated as data (`{.arg}`, `{.val}`, `{.cls}`, `{.fn}`, `{.field}`,
pluralization) rather than pasted into a string. Base `stop()`,
`warning()`, `message()` and `cat()` SHALL NOT appear on a live user-facing
path; numeric tables MAY be printed through `stats::printCoefmat()` or
`print.default()` inside a cli-rendered method. The package SHALL NOT wrap
cli in a package-level helper of its own and SHALL NOT import a sibling
package's messaging wrappers. Condition classes, where a caller or test
needs to match one, SHALL keep the `goldfish_<snake_case>` form.

#### Scenario: a validation error interpolates the argument and the value
- **WHEN** `set_algorithm_newton(max_iterations = -1)` is called
- **THEN** the error is a cli condition naming `max_iterations` as an
  argument and the supplied value as data, not a pasted string

#### Scenario: the deprecated path is exempt
- **WHEN** a `make_nodes()`/`make_network()`-path validator in the
  deprecated data-class code rejects its input
- **THEN** it MAY still signal with base `stop()`, because that path is
  scheduled for removal rather than conversion

### Requirement: One object-header idiom

A print method for a classed goldfish object SHALL open with
`cli_rule(left = "{.cls <class>}")` followed by one summary line, and
SHALL use `cli_h1()`/`cli_h2()`/`cli_h3()` only for the multi-section data
print (`goldfishData`), never as an object header. Section labels inside a
print SHALL be `{.strong}` text lines or `cli_dl()` keys.

#### Scenario: a fit header names its class
- **WHEN** a `goldfishFit` is printed
- **THEN** the first line is a rule carrying `goldfishFit` and the second
  states the model, sub-model and call

### Requirement: Informational output honors the stocnet verbosity option

Progress steps, info alerts and success alerts SHALL be emitted only when
`getOption("snet_verbosity", "quiet")` is `"normal"` or `"verbose"`, with
progress and minor detail at `"verbose"` only, read through one internal
helper and never by importing manynet. Errors, warnings, prompts and the
object print methods SHALL NOT be gated. A per-call `progress = TRUE` or
`verbose = TRUE` argument SHALL show its output regardless of the option.
The package SHALL NOT set `options(cli.theme = )`.

#### Scenario: quiet by default
- **WHEN** an estimator emits a one-time informational alert and
  `snet_verbosity` is unset
- **THEN** nothing is printed, and the estimate is unchanged

#### Scenario: an explicit progress argument wins
- **WHEN** `estimate_dynam(..., progress = TRUE)` runs with
  `snet_verbosity` unset
- **THEN** the preprocessing and estimation phases print as cli progress
  steps

#### Scenario: warnings are never gated
- **WHEN** a warning condition is raised at `snet_verbosity = "quiet"`
- **THEN** the warning is signaled

### Requirement: Printed output is pinned under one reproducible cli context

Every snapshot of console output SHALL be recorded under the shared test
helper `local_cli_context()` (fixed width, colors off, unicode off,
`snet_verbosity = "quiet"`), defined once in the test helpers, so a
snapshot is stable across terminals and sessions.

#### Scenario: a printer snapshot does not depend on the terminal
- **WHEN** a print snapshot test runs in a 200-column color terminal and
  in a non-interactive CI log
- **THEN** both produce the recorded snapshot
