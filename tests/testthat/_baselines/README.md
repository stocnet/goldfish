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

## v2 — the same floor, extended to the gather backend

`coefficient_baselines_v2.rds` is what `test-coefficient_baselines.R` asserts.
It is **not** a regeneration of v1. Its `r` and `cpp` entries are v1's numbers
copied forward verbatim — verified bit-identical, not merely within tolerance —
so the floor frozen at `b890cd0` is unchanged and any drift against it stays
detectable. Only the `gather` column is computed fresh, because v1 never
carried one: `baselines_backends` was `c("r", "cpp")`, the absence that
`backend-parity` design D4 relied on when the gather kernels adopted the stable
softmax, and D15 closes deliberately now that three-way parity is green.

Generated at commit `28f06f6` by the same
`generate_coefficient_baselines.R`, which reads v1 and writes v2. **Do not
"simplify" that script into fitting all three backends**: recomputing r and cpp
would absorb the ~2.4e-07 relative drift that has accumulated against v1 since
it was written, and make any later regression smaller than that drift invisible.

v2 is keyed by backend value (`r`, `cpp`, `gather`) rather than v1's legacy
engine tokens (`default`, `default_c`) — a new artifact should not record a
vocabulary the package has retired. v1 stays on disk as the historical
reference.

The separately versioned `global_v1` set below has no gather column and states
its own coverage (`baselines_backends_global`); extending it would be its own
deliberate regeneration.

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
