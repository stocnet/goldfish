## Why

goldfish's naming policy is strict snake_case for all functions, arguments and
objects, enforced by `object_name_linter("snake_case")`. The 1.7.0 renames
retired camelCase from the exported *function* API. The **returned objects were
never swept**, so the components a user reads are still camelCase — and 2.0.0 is
the release that ships them.

The inconsistency is not uniform, which is what makes it a trap. Within a single
list, `result$convergence`, four components are camelCase and one is snake_case:

```r
convergence$isConverged     convergence$returnCode
convergence$maxAbsScore     convergence$maxAbsUpdate
convergence$score_rel_norm   # <- added later, in the current convention
```

The same split runs across the result object, where `event_scores`,
`observed_rank`, `node_lookup`, `total_rate` and `risk_set_axis` sit beside
`logLikelihood`, `standardErrors` and `eventProbabilities`. A user cannot guess
which convention a component follows, and neither can we: during
`parity-followups` this cost real work when tests were written against
`event_probabilities`, the name the *neighbouring* components would imply. They
silently read `NULL`, and because `NULL[[1]]` is `NULL` rather than an error,
four assertions passed while testing nothing.

That is the argument for doing this now and in one sweep: the convention is
already mixed inside single objects, every new component widens the gap, and the
failure mode is silent.

Folded in: the reserved `engine` parameter on the internal model-spec
constructor. It is dead surface — never passed a non-default value, aborting on
anything else — reserved for an incremental REM variant that does not exist. It
also spells `engine`, the vocabulary 2.0.0 just retired in favor of `backend`,
so shipping it invites exactly the confusion the backend rename removed.

## What Changes

- **Every component of every returned object is snake_case.** Three objects are
  affected — the fitted result (including its `convergence` sub-list), the
  gather export, and the preprocessed object.
- **A pre-2.0.0 fitted object is recognized and reported.** There is no
  deprecation window on the component names — a CRAN 1.6.12 object is already
  unusable (its `summary()` errors, its `logLik()` returns `df = NULL` so
  `AIC()` silently misreports), so preserving spellings would protect nothing.
  Instead the object is identified and the user is told to re-fit: informative on
  `print()`, an error on the surfaces that would otherwise compute a wrong
  number.
- **New fits record their format version**, so recognizing an old object does not
  depend on reasoning from absence.
- **The reserved `engine` parameter and its unreachable abort are removed** from
  the internal model-spec constructor, together with the test that pins the
  abort.

## Capabilities

### Modified Capabilities

- `naming-deprecations`: extends the existing rename policy, which covers
  functions and arguments, to the components of returned objects.

## Impact

- **Code:** `R/estimation_core.R`, `R/cpp_interface.R`, `R/model_estimate.R`,
  `R/methods_postestimate.R`, `R/methods_display.R`, `R/preprocess_export.R`,
  `R/preprocess_builders.R`, `R/model_spec.R`; the roxygen `@return` blocks for
  the estimators and `gather_model_data()`; `tests/testthat/test-model_spec.R`.
- **Users: BREAKING at 2.0.0, with no window.** Any script reading
  `fit$logLikelihood`, `fit$convergence$isConverged` or
  `fit$eventProbabilities` is affected. This is the largest user-visible surface
  in the 2.0.0 line, and design D2 records why a window would have been
  compatibility theater rather than a transition.
- **Downstream:** `autograph` consumes fitted objects; `.plan/` analysis scripts
  and the `residuals_comparison` harness read these components.
- **Not in scope:** internal object components that never reach a user (local
  variables, unexported helpers). The linter migrates those file by file as
  files are touched, which is the existing policy.

## Open Questions

All questions this change opened with are now settled and recorded as
decisions — no window (D2, D2a) and `nParams` is internal (D2b).

- ~~Where does the format stamp live on the object, and does the preprocessed
  object need one too? It changes shape at 2.0.0 for the same reasons.~~
  Settled in session 3: task 0.2 and D5 place the stamp, and the
  preprocessed object carries one too (the prep guard).
