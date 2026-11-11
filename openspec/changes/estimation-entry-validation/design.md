## Context

goldfish has two ways into estimation for a stocnet object.

- **Specification path.** `make_specification(formula, data)` calls
  `validate_goldfish_data()` (`R/make_specification.R:174`, `:375`) and aborts
  on an invalid object. `estimate_*()` and `compute_statistics()` given a
  specification go through `estimate_from_specification()`
  (`R/model_estimate.R:759`), which estimates against `spec$data`, or against
  a caller-supplied `data` when one is given (`:814`).
- **Formula path.** `estimate_dynam()`, `estimate_rem()`, `estimate_dynami()`
  given a formula, `compute_statistics()` given a formula, and the deprecated
  `gather_model_data()` call `estimate_wrapper()` (`:1908`) directly.

Every route ends in `estimate_wrapper()`. Its only data check is
`check_estimation_data()` (`:1674`): `data` must be a list or an environment.
Nothing on the formula path calls the validator. The two internal callers also
reach the wrapper: `estimate_flavored` (`R/estimate_flavored.R:123`, `:189`)
passes `spec$data`, and the DyNES warm start (`R/estimate_dynes.R:616`) passes
data derived from an already validated object.

Measured on 2026-11-11, with the wrapper instrumented to validate and log
without acting, in a scratch worktree at `85652c67`
(`.plan/develop/entry-validation-measure/`):

| Calls in the `NOT_CRAN=true` suite (139 files) | Formula | Specification |
| --- | --- | --- |
| DyNAM | 1598 | 440 |
| REM | 266 | 46 |
| DyNAMi | 20 | 2 |
| would be rejected | 0 | 0 |

The suite was unchanged (PASS 18164, FAIL 0, SKIP 5). The shipped stocnet
datasets `social_evolution` and `fisheries_treaties` validate.
`validate_goldfish_data()` costs 258 µs / 128 KB and 945 µs / 244 KB on them.

## Goals / Non-Goals

**Goals:**

- The formula path runs the same checks as the specification path, before
  anything reads the object.
- An object that fails them warns now and aborts in a later release, so no
  script that runs today stops running in this release.
- No message is raised twice for one object, and a specification estimated
  against a different object does not skip validation.

**Non-Goals:**

- Making the formula path abort. A later change does that once the
  deprecation period ends (D5).
- Changing what the validator checks. Missing `info$directed` stays an
  error-level check; this change only decides what the formula path does
  when a check fails.
- Validating legacy environment input. `estimate_dynam()`,
  `estimate_dynami()` and `estimate_rem()` already reject an environment
  (`abort_legacy_environment()`, `R/model_estimate.R:534`, `:616`, `:696`).
  `compute_statistics()` and `gather_model_data()` still pass one to the
  wrapper's legacy branch (`is_legacy`), and this change leaves that branch
  as it is. The DyNAM-i bridge builds its environment internally, from a
  stocnet that this change validates first.
- Routing the formula path through `make_specification()`. That would be
  one front door, but it is a refactor of a working path, out of proportion
  to the gap.

## Decisions

**D1. One call site, in `estimate_wrapper()`, right after
`check_estimation_data()`.** Every formula entry and every specification
estimate ends there, so one site covers them all. It sits above the DyNAM-i
stocnet-to-environment bridge (`:65-94` of the wrapper body), which reads
`info$focal`, `info$sender` and `info$receiver` and must not see an
unvalidated object. Validation applies only when `data` is a stocnet list
(`is.list(data) && !is.data.frame(data)`, the shape `check_estimation_data()`
already accepts). An environment is left to the existing legacy branch (see
Non-Goals).

*Alternative:* validate in each public function. Rejected: six entry points
for one rule, and a new entry would have to remember it.

**D2. The caller states whether `data` is already validated.**
`estimate_wrapper()` gains an internal argument `data_validated = FALSE`. It
validates only when this is `FALSE`. The default is the safe one, so a new
caller validates unless it opts out.

- `estimate_from_specification()` passes
  `is.null(data) || identical(data, spec$data)`. `identical()` returns at once
  when both arguments are the same object, and the same comparison already
  decides `reuse_parsed` at `:818`. A substitute `data` is validated.
- `estimate_flavored` passes `TRUE`: it estimates against `spec$data`.
- The DyNES warm start passes `TRUE`: its data is built inside the package from
  an object `make_specification()` validated, and it runs once per drawn
  sequence, where repeating the validator buys nothing.

*Alternative:* skip whenever `specification` is non-`NULL`. Rejected: a
specification estimated with `data = other` would bypass validation, the
same bypass the stamp is forbidden to create.

**D3. Warn mode is the validator run under a handler, not a second
validator.** A small internal `validate_or_warn_goldfish_data(data, what,
call)` in `R/validate_goldfish.R` calls `validate_goldfish_data()` inside
`tryCatch()`. On error it raises `lifecycle::deprecate_warn()` with `what`
naming the entry (for example `I("Estimating from a data object that fails
validation")`), `details` carrying the validator's message rendered with
cli, and `always = TRUE`. The checks therefore never fork. When the later
release flips to abort, the handler goes and the plain call stays.

`always = TRUE` because the warning is about a data object, not about code
the user wrote once: two invalid objects in one session must both warn, and
lifecycle's default (once every 8 hours) would hide the second.

*Alternative:* `cli::cli_warn()` with a `goldfish_` condition class. Rejected:
the decision is a deprecation with a planned removal, which is what lifecycle
records and what `lifecycle::expect_deprecated()` tests. The class is
lifecycle's `lifecycle_warning_deprecated`.

**D4. `focal = NULL` on the formula path.** The validator's focal check
compares `info$focal` with a named layer. On the formula path the dependent
can be an alias or flavor-qualified (`resolve_dependent_alias()`), and the
formula parser already checks that it resolves. The 0-rejection measurement
used `focal = NULL`. Passing the dependent's name is left to the abort change,
together with its own measurement.

**D5. Deprecation timing.** `when` follows the release this change ships in.
Today's deprecations announce `2.0.0` (`R/class_diagnostics.R:489`), so the
default is `"2.0.0"`; the trunk fold confirms it. The abort comes in a later
release, through its own change, which also removes the warn handler.

**D6. Tests through the entry, not the validator.** `test-as_goldfish.R:20`
calls `validate_goldfish_data()` by hand and so cannot see this gap. It is
replaced by tests that call the estimation entries with an invalid object
(`expect_snapshot()` of the warning under a pinned cli context, plus
`lifecycle::expect_deprecated()`), and one that compares the result with the
check against the result without it. For that comparison the test builds
the "without" result via `local_mocked_bindings()` on the warn helper. A
test of the skip (D2) counts validator calls through a mocked binding, so a
regression that validates twice, or never, is red.

**D7. The false comment.** `R/as_goldfish.R:4` is reworded to what the code
does once D1 lands: the specification path aborts, and the formula path
validates and, during the deprecation period, warns.

## Storage and per-event cost

None. The change adds no preprocessed output and no per-event code. The
validator runs once per estimation call, before preprocessing, at the costs
measured above (under 1 ms and 250 KB on the shipped datasets). It allocates
nothing that lives past the call. Predicted change in walk time,
preprocessed size and fit time on the large fixture: none measurable.

## Risks / Trade-offs

- [A user object that estimates today fails validation] → It warns and
  estimates as before; the message names the failing check and the release
  that will abort.
- [Warn and continue lets an invalid object produce a fit] → That is today's
  behavior, now announced. Objects the validator rejects for structural
  reasons usually fail later on their own, as they do today.
- [An internal caller wrongly passes `data_validated = TRUE`] → Only the three
  callers in D2 opt out, each with a stated reason. The D6 counting test pins
  the specification case.
- [Warning noise in tests or vignettes] → None expected: 0 of 2372 suite calls
  and both shipped datasets validate. A task re-runs the measurement after
  the change.

## Migration Plan

Users declare what the validator asks for (for example a named `info$directed`
covering every layer), or gate early with `as_goldfish()`. The NEWS fragment
says so. Rollback is reverting the commits; nothing persistent changes.

## Open Questions

- Q1. The `when` version (D5): confirm `"2.0.0"` at the trunk fold.
- Q2. The release that flips to abort: decided when the abort change is
  proposed, not here.
