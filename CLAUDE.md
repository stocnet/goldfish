# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`goldfish` is an R package for statistical modeling of dynamic network data, focusing on relational event models. It implements Dynamic Network Actor Models (DyNAM), DyNAMi (for interactions), and Relational Event Models (REM).

## Spec-Driven Development (OpenSpec)

This repo uses OpenSpec. The authoritative workflow lives in the tracked
`openspec/config.yaml` (lifecycle, commit-per-task, phase-milestone
`DESCRIPTION`/`NEWS.md` bumps, the `NOT_CRAN=true` test gate) — read it before
implementing. A change lives in `openspec/changes/<name>/`: `proposal.md`,
`design.md` (numbered decisions D1, D2, …), `specs/**/spec.md`, `tasks.md`, plus a
local-only `progress.md` cross-session journal. On `/opsx:archive`, spec deltas
fold into `openspec/specs/**` and the change moves to
`changes/archive/<date>-<name>/`.

**What is version-controlled (collaboration policy):** the living spec
(`openspec/specs/**`), `openspec/config.yaml`, and **active** proposals
(`openspec/changes/<name>/` incl. `proposal.md`/`design.md`/`specs/`/`tasks.md`)
are tracked so collaborators share the same standards. Kept local (gitignored):
each change's `progress.md` (personal session journal), `changes/archive/`
(deleted from the tree on archive — history retains it), `.claude/settings.local.json`
(personal permissions), and `.plan/` scratch except the tracked
`goldfish_versions.csv` ledger + its two tooling scripts. Project-specific notes:

- **Archive tracking (run FIRST during `/opsx:archive`)**: before the sync/move
  steps, while the change dir still exists, run
  `bash .plan/opsx-archive-track.sh <name>` — it snapshots the change's
  `progress.md` to `.plan/` (local-only) and appends a version row to the tracked
  `.plan/goldfish_versions.csv` cross-version time/memory ledger. This is an
  explicit archive step (see `openspec/config.yaml`
  `rules.archive`), NOT a hook — nothing runs on unrelated commands.

- **Workflow disciplines are authoritative in `openspec/config.yaml`**: commit-per-task,
  run `devtools::document()` inline when roxygen/exports/signatures change, bump
  `DESCRIPTION` + `NEWS.md` at each phase milestone, `air format` the touched R files
  before `lintr` runs on them, and test with `NOT_CRAN=true`
  (the coefficient-baseline and C++ golden tests use `skip_on_cran()`). Read it
  before implementing.
- **Do not regenerate the frozen coefficient baselines** in
  `tests/testthat/_baselines/` (design D18 governs the separately versioned
  `global_v1` set). They are the 1e-6 regression floor; a `NOT_CRAN=true` run must
  report them as PASS, not SKIP.

## Required Skills

These apply to **all** work in this repo (OpenSpec-driven or ad hoc), not only when
implementing a change. Invoke the skill before doing the work it covers — do not rely
on memory.

- **`r-lib:r-package-development` — always.** Any task touching package machinery
  (`devtools` load_all/test/document, roxygen2, NAMESPACE, DESCRIPTION, build
  infrastructure) uses this skill as the default working reference.
- **`r-lib:testing-r-packages` — whenever creating or reorganising tests.** Follow
  testthat 3e (self-contained tests, fixtures, snapshots, mocking) for every new test
  file and verification task.
- **`r-lib:cli` — always, whenever anything is returned to the console** (print/format
  methods, warnings, messages, errors, status output). New user-facing output renders
  via cli semantic elements, interpolating data (not literal markup); pin a reproducible
  cli context for output snapshot tests. (cli is already in Imports.)
- **`r-lib:lifecycle` — whenever deprecating, renaming, superseding, or marking an API
  experimental** (`deprecate_warn`/`deprecate_soft`, lifecycle badges, NEWS entry).
- **`cpp-recompile` — after ANY edit to `src/*.cpp` / `src/*.h`, before testing.**
  Regenerates the Rcpp bindings (`compileAttributes()`, with the `RcppExports.*`
  diff to verify a signature/arity change) and force-recompiles
  (`load_all(recompile = TRUE)`) so the coefficient/C++ golden baselines never run
  against a stale `.o`/`.so`. The PostToolUse hook nudges toward it on every `src/`
  edit; invoke it rather than an ad-hoc `load_all`.
- **`not-cran-test` — before committing any task**, to confirm the frozen 1e-6
  baselines are PASS not SKIP. **`release-prep`** (user-invoked) is the local
  release pre-flight.

For OpenSpec change work these are also enforced as hard rules in `openspec/config.yaml`.

## Development Commands

### Building and Testing
- Build package: `R CMD build .` or `devtools::build()`
- Install and test: `devtools::install()` or `devtools::load_all()`
- Run tests: `devtools::test()` or `testthat::test_check("goldfish")`
- Run specific test file: `testthat::test_file("tests/testthat/test_filename.R")`
- Check package: `R CMD check .` or `devtools::check()`
- Install from GitHub: `remotes::install_github("stocnet/goldfish@develop", build_vignettes = TRUE)`

### Code Quality
- Lint code: `lintr::lint_package()` (configuration in `.lintr`)
- Check coverage: `covr::package_coverage()`
- Good practices: `goodpractice::gp()`

### Documentation
- Build documentation: `devtools::document()` (updates man/ files from roxygen2)
- Build vignettes: `source("vignettes/precompile.R")`, `devtools::build_vignettes()`
- Build website: `pkgdown::build_site()` (configuration in `_pkgdown.yml`)

## Architecture

### Core Components

1. **Data Objects** (`R/make_data.R`):
   - `make_nodes()`: Creates node sets with attributes
   - `make_network()`: Creates network objects
   - `link_events()`: Links event data to modify objects over time
   - `make_dependent_events()`: Defines dependent events for modeling

2. **Model Estimation** (`R/model_estimate.R`):
   - `estimate_dynam()`: Estimates DyNAM models (rate/choice/choice_coordination submodels)
   - `estimate_dynami()`: Estimate DyNAM-i models (rate/choice submodels)
   - `estimate_rem()`: Estimates Relational Event Models
   - Maximum likelihood estimation via Newton-Raphson procedure

3. **Effects Functions** (multiple files):
   - `R/functions_effects_DyNAM_*.R`: Effect functions for different DyNAM submodels
   - `R/functions_effects_REM.R`: Effect functions for REM models
   - `R/functions_effects_DyNAMi_*.R`: Effect functions for DyNAMi models

4. **Preprocessing** (`R/model_preprocess.R`):
   - Data validation and preprocessing
   - Missing data imputation strategies
   - Event sequence preparation

5. **C++ Interface** (`src/`, `R/cpp_interface.R`):
   - Performance-critical computations in C++ with Rcpp/RcppArmadillo
   - Interface files for calling C++ functions from R

### Key Concepts

- **Relational Events**: Time-stamped interactions between network actors
- **Dynamic Networks**: Networks that evolve over time through events
- **Actor-oriented Models**: Models focusing on actors' decisions to create events
- **Tie-oriented Models**: Models focusing on dyadic relationships

### Data Structure Requirements

Events data frames must contain:
- `time`: Event timestamp (numeric or POSIXct)
- `sender`/`receiver`: Actor identifiers matching node labels
- `increment` or `replace`: Value changes

Node attribute events require:
- `time`, `node`, `replace` columns

## Testing

Tests are in `tests/testthat/` using the testthat framework. The package uses R CMD check standards and continuous integration via GitHub Actions.

## Package Structure

- `R/`: R source code
- `src/`: C++ source code
- `man/`: Generated documentation
- `vignettes/`: Package vignettes with usage examples
- `tests/testthat/`: Unit tests
- `data/`: Example datasets
- `inst/`: Installed files

## Debugging the C++ core

The performance-critical code in `src/` is compiled (Rcpp/RcppArmadillo). When a
crash or memory bug originates there:

- **Force a recompile**: `devtools::load_all(".", recompile = TRUE)`.
- **Backtrace a crash** under lldb — reproduce, then print the stack:
  ```bash
  R -d lldb
  # (lldb) run
  # paste the estimate_*() call at the R prompt; on crash: (lldb) bt
  ```
- **Isolate in a subprocess** so your R session survives the crash:
  ```r
  callr::r(func = function() { devtools::load_all("."); estimate_dynam(...) },
           stderr = "crash.log", error = "error")
  ```
- **AddressSanitizer build** for memory bugs — add to `src/Makevars`, then
  `devtools::load_all(".", recompile = TRUE)`:
  ```makefile
  PKG_CXXFLAGS = -g -O0 -fsanitize=address
  PKG_LIBS     = -fsanitize=address
  ```

## Code Style

- **Naming (strict)**: **snake_case for ALL functions, arguments, and R objects**
  (tidyverse style). The 1.7.0 renames retired camelCase from the exported API — never
  reintroduce it. Internals still in camelCase (`prepEnvir`, `linkEnvir`, `isDirected`,
  `GetDetailPrint`, …) migrate to snake_case whenever a file is touched. Enforced via
  `object_name_linter("snake_case")` in `.lintr`.
- **Cross-package calls**: never `pkg:::fn()` on another package's unexported objects
  (R CMD check/CRAN violation). Rule: `@importFrom` when a function is used across
  functions or in hot paths; `pkg::fn()` for occasional calls.
- **American English** for everything user-facing: function and argument names, reserved
  column names/values (`flavor`, not `flavour`), error/warning messages, and
  documentation prose (`-ize` endings, `color`, `behavior`). Note: manynet uses British
  spellings in places — goldfish-owned surface stays American regardless.
- **Code comments**: favor self-documenting code (clear names, well-structured
  code) over comments. Add a comment where it explains something not self-evident —
  the *why* behind a decision, non-obvious logic, an assumption, edge case, units,
  or the paper/equation a block implements — never the *what* the code already
  states. Section-break markers (`# Data preprocessing ----`) and short
  object-shape notes at creation points are fine. **Never delete an existing
  comment** unless you are also removing the functionality it explains.
  - **Never reference OpenSpec artifacts in code comments** — no decision IDs (`D12`), task numbers (`task 4.4`), change names, or `design.md`/`proposal.md`/`spec.md` pointers. OpenSpec changes are archived (and some artifacts are gitignored), so such a reference becomes a dangling pointer to something a future reader cannot open. Comment the *reasoning itself* inline (the assumption, the edge case, the paper/equation) so the code stands alone; keep the OpenSpec traceability in commits and the change's `progress.md`, not in the source.
- **R conventions (team standards)**:
  - **Native pipe only** — `|>`, never the magrittr `%>%` (the codebase targets
    R 4.1+).
  - **Performance** — prefer vectorized operations (`vapply`/`lapply`/`apply`) and
    pre-allocated objects over growing with `c()`/`rbind()` in loops; use
    `seq_len()`/`seq_along()` (never `1:length(x)`) to stay safe on zero-length
    vectors.
  - **roxygen2 — never duplicate documentation**: document a shared
    `@param`/`@return`/`@details` **once** on the most primitive/public function
    and inherit elsewhere (`@inheritParams`, `@inherit`, `@inheritDotParams`) so a
    wording change happens in exactly one place; after editing inherited tags run
    `devtools::document()` and confirm the man/ page resolved the inheritance.
- **Commit messages**: conventional-commit style (`feat:`/`fix:`/`chore:`/`test:`/
  `docs:`), one focused commit per completed OpenSpec task, tests green at every
  commit so any step can be rolled back.
- **Linting configuration**: The project uses specific linter settings in `.lintr` that disable certain checks (object names, line length at 80 chars, commented code, etc.)
- **Dependencies**: Core dependencies include Rcpp/RcppArmadillo for C++ interface, changepoint, ggplot2, and CLI utilities