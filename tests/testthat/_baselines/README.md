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
