## Why

The living `single-data-object` requirement "Dual input path with one
validate-and-stamp boundary" says validation "SHALL run unconditionally at
specification/estimation time". The specification path keeps that promise:
`make_specification()` calls `validate_goldfish_data()`. The formula path does
not. `estimate_dynam()`, `estimate_rem()` and `estimate_dynami()` given a
formula and `data =`, `compute_statistics()`, and the deprecated
`gather_model_data()` all end in `estimate_wrapper()`
(`R/model_estimate.R:1908`). Its only check on `data` is
`check_estimation_data()` (`:1674`), which tests the type alone. So a stocnet
object the validator would reject is estimated anyway, and the comment at
`R/as_goldfish.R:4` ("estimate_dynam()/estimate_rem()/make_specification()
validate a stocnet on the fly") is false.

The gap is old. `refactor-single-data-object` task 1.4 asked for a post-stamp
mutation test that expects "the estimation-time abort". Its tick note says
"covered via validate_goldfish_data directly", and `test-as_goldfish.R:20`
calls the validator by hand, so the test cannot fail on this gap and the spec
was archived claiming behavior the code never had (ADR-0062).

It now has a visible cost. `two-mode-undirected-fold` resolves layer symmetry
on the mode map, where an undeclared `info$directed` reads as directed. On the
formula path an undeclared one-mode layer therefore moved from undirected to
directed: on a 4-actor, 7-event REM fixture, `sum(inertia)` / `sum(indeg)` went
from 34/102 to 21/63 and the coefficients moved with them (that change's
group-4 conformance report, gap B). Duplicate labels, an unnamed `update`,
non-syntactic layer names and impure mode sets are just as unchecked there.

## What Changes

- The formula path validates a stocnet `data` object with
  `validate_goldfish_data()` before anything reads it, including the DyNAM-i
  stocnet-to-environment bridge.
- An object that fails validation on the formula path raises a lifecycle
  deprecation warning that carries the validator's message, and estimation
  continues as it does today (user decision 2026-11-11: warn, not abort). A
  later release turns the warning into the validator's abort; that is a
  separate change.
- The specification path is unchanged: `make_specification()` keeps aborting.
  Re-validation is skipped when a specification is estimated against the very
  object it validated, so no message is raised twice. A specification
  estimated against a *different* `data` object is validated like the formula
  path.
- `R/as_goldfish.R:4` stops claiming more than the code does, and
  `test-as_goldfish.R:20` is replaced by a test through the estimation entry.
- Measured before proposing (2026-11-11, `.plan/develop/entry-validation-measure/`):
  of 2372 `estimate_wrapper()` calls in the `NOT_CRAN=true` suite, over 139
  test files, none would be rejected; the shipped stocnet datasets
  `social_evolution` and `fisheries_treaties` validate. So no test, example or
  frozen baseline is expected to warn.

Not **BREAKING**: every object that estimates today still estimates. Objects
that fail validation now warn.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `single-data-object`: "Dual input path with one validate-and-stamp boundary"
  names both entries and what each does with an invalid object: the
  specification path aborts; the formula path warns during a deprecation
  period, then aborts. The post-stamp mutation scenario is split along the
  same line, and a scenario covers a specification estimated against
  substitute data.

## Impact

- Code: `R/model_estimate.R` (`estimate_wrapper()`, and passing whether
  `estimate_from_specification()` substituted the data), `R/validate_goldfish.R`
  (a warn-mode entry that reuses the existing checks), the comment at
  `R/as_goldfish.R:4`; tests in `tests/testthat/test-as_goldfish.R` and a new
  entry-validation test file.
- No per-event code. The validator runs once per fit, at setup: 258 µs /
  128 KB on `social_evolution`, 945 µs / 244 KB on `fisheries_treaties`. It
  reuses `validate_goldfish_data()` and its checks; no new walk, buffer or
  pass.
- No frozen baseline is expected to move: the call adds a check and changes
  no value on a valid object, and every suite fixture is valid.
- Feature-branch fold policy: a `NEWS.d/` fragment, no Version bump. The
  change is independent of DyNES. Under `rules.branching` it is a candidate
  for its own small branch off the integration branch; the branch is the
  user's choice.
