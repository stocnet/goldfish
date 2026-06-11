# Coefficient baselines

Pre-refactor regression floor for the `refactor-preprocess-estimate` change.
`coefficient_baselines_v1.rds` stores `coef()` and `logLik()` for the 6 model
types (DyNAM-rate, DyNAM-rate-ordered, DyNAM-choice, DyNAM-choice-coordination,
REM, REM-ordered) estimated with both engines (`default`, `default_c`) on the
Social_Evolution and Fisheries_Treaties_6070 datasets. The model grid, data
constructors, and fit wrapper live in `tests/testthat/helper-baselines.R`;
`test-coefficient_baselines.R` asserts reproducibility to 1e-6 tolerance.

These baselines are **frozen**: they were generated at commit `b890cd0` (before
any refactor code changes) and must not be regenerated during the refactor. A
failure in `test-coefficient_baselines.R` means a refactor regression.

To regenerate (only for a new baseline version with documented justification),
run from the package root:

```sh
Rscript tests/testthat/_baselines/generate_coefficient_baselines.R
```

## global_v1

`global_v1/coefficient_baselines_global.rds` stores baselines for DyNAM-rate
and REM models including a `global()` effect, with a linked global attribute
changing mid-sequence, on both engines (asserted by
`test-coefficient_baselines_global.R` at the same 1e-6 tolerance). Rank-only
(no-intercept) variants are excluded: a global covariate is constant across
alternatives at any event time, so it is unidentified without the time
intercept.

Unlike the frozen set above, this set is separately versioned (design D18 of
the change): the global-attribute feature shipped immediately before the
refactor, so `global_v1` may be regenerated with documented justification if
a bug is found in the feature itself. Failures still block recipe work.

```sh
Rscript tests/testthat/_baselines/global_v1/generate_global_baselines.R
```
