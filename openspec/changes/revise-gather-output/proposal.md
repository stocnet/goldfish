# Proposal — revise-gather-output

## Why

Two exported functions cover the same ground with drifted vocabularies:
`gather_model_data()` rejects `sub_model = "rate_ordered"` at its own
`match.arg` while `compute_stats(..., output = "gather")` already produces
the correct ordinal output through the shared `estimate_wrapper()`
validation — and the deprecated `model = "REM", sub_model = "choice"` remap
silently lands on the exact-time flavor with a force-added intercept
(statistic columns shift to `(Intercept, ...)`, a positional-indexing trap
hit in practice). Cross-package workflows (`.plan/datasets/Simulation.R`;
`.plan/residuals_comparison.qmd`, whose identity route reproduces goldfish
estimates in coxph/clogit/mlogit/glm to ≤7e-7) hand-build the same long
data frame from the gather stack on every use. Decision (explore session
2026-07-24): consolidate onto ONE precise function —
`compute_statistics()` — covering everything `gather_model_data()` does
today plus the ready-to-estimate frames, with `gather_model_data()`
soft-deprecated and the `compute_stats` name **deleted outright** (no
stub): it only ever existed inside the unreleased 2.0.0 development line
(NEWS dev sections; never on CRAN), so there are no released users a
deprecation cycle — or even a defunct stub — would serve.

## What Changes

- **`compute_statistics()` is the single statistics-product function**
  (rename of `compute_stats()`): `output = c("preprocessed", "gather",
  "data.frame", "db")` — the preprocessed replay object, the gather stack,
  the new ready-to-estimate long frame, and the DBI stream.
  `gather_model_data()` is soft-deprecated onto it (shipped in released
  versions; keeps working one release cycle); **`compute_stats()` is
  deleted** — removed from NAMESPACE and source, no stub (dev-line-only
  name, never released; the rename is recorded in NEWS). The `max_length`
  and finalization behavior carry over.
- **Vocabulary by delegation, cli-fixed**: no local `match.arg` — model +
  sub_model validate once in the shared `estimate_wrapper()` path;
  `check_model_par()` upgraded from base `stop()` to a cli error listing
  the allowed sub_models per model; the REM `"choice"` deprecation message
  (single site, `estimate_wrapper()`) names both successors (`"rate"`
  exact-time, `"rate_ordered"` ordinal).
- **DyNAMi supported** through `compute_statistics()` (the isolated DyNAMi
  preprocessing front-end must route or post-convert to the gather writer —
  verified, not assumed).
- **Flavored specifications** return, for every output form, a fid-indexed
  list carrying the `process_map` table attribute — the same identity
  authority used by flavored preprocessing and the estimation container;
  display labels are rendered from the process_map, never parsed from keys.
- **Intercept/censoring semantics reported**: `has_intercept` and
  `right_censored` fields attached at finalization (single outputs mirror
  the flavored `process_map` columns), with documentation mandating
  name-based statistic-column access.
- **Ready-to-estimate frame** (`output = "data.frame"`): long frame with a
  documented identity/outcome contract, plus worked, dependency-guarded
  examples for `survival::coxph`/`clogit`, `mlogit::mlogit`, and
  `glm(family = poisson)`.
- **Final naming from birth**: `algorithm-naming` implements before this
  change, so the new function is born as `compute_statistics(x, model,
  sub_model, data, output, control_prep = set_preprocessing(), ...)` —
  final control names, `x`-first, selectors-before-data (the
  `estimate_*()` family order). This change also soft-deprecates the
  estimators' `preprocessing_only = TRUE` (warning names
  `compute_statistics(output = "preprocessed")`), since the replacement
  is born here; the other estimator argument renames stay with
  `algorithm-naming`.

## Capabilities

### New Capabilities

- `model-data-export`: the `compute_statistics()` contract — output
  vocabulary, delegated model/sub_model validation, per-sub_model
  intercept/right-censoring semantics and reporting, flavored fid-list
  keying, DyNAMi coverage, the ready-to-estimate frame contract, the
  cross-package example recipes, and the retirements
  (`gather_model_data()` soft-deprecated; `compute_stats()` deleted, no
  stub).

### Modified Capabilities

- `preprocess-output-writers`: the gather-writer requirement's canonical
  user surface changes from `gather_model_data()` (wrapper) to
  `compute_statistics()` (with `gather_model_data()` deprecated), and the
  db-writer user-facing pointers rename accordingly. The stack format
  itself is unchanged.

## Impact

- **goldfish R**: `R/model_estimate.R` (`compute_stats` →
  `compute_statistics`, `check_model_par` cli upgrade, REM-choice message),
  `R/preprocess_export.R` (deprecation wrapper, finalization fields, frame
  assembly), `R/preprocess_writers.R` (message pointers), DyNAMi front-end
  routing; roxygen + examples; NEWS; lifecycle deprecations.
- **Cross-change**: residuals-gof's diagnostic-primitives guiding-error
  text names `compute_statistics(output = "preprocessed")` as the
  replay-supply route (edit lands there before its task 1.8 implements
  the message); stale references to this change's retired names in other
  active changes are swept here (`compute_stats` in effect-term-registry;
  `gather_model_data` as canonical surface in
  gather-rem-coordination-format).
- **No C++ changes expected**; frames assemble in R from the gather stack.
- **Dependencies**: none added; survival/mlogit only in guarded examples.
- **Consumers**: `.plan/residuals_comparison.qmd` identity route switches
  to `output = "data.frame"`.
