---
depends-on: singular-sequence-information
status: landed (feature/dynes, awaiting fold)
---

## Why

The standard errors that `estimate_dynes()` returns through `vcov()` are not
an asymptotic covariance. `sandwich_covariances()` (`R/fit_dynes.R`, abmcem
design D9) forms `I_c⁻¹ (Σ w̄ᵢ sᵢsᵢᵀ) I_c⁻¹` from whole-sequence complete-data
scores `sᵢ` and the pooled complete-data information `I_c = Σ w̄ᵢ Jᵢ`. At the
MLE the mean score `S̄` is near zero, so the middle term is the weighted
variance of the scores across paths. That is Louis's (1982) *missing*
information, not the information the data carry. When paths differ little,
the "sandwich" collapses toward `|S̄|/I`, a quantity bounded by the M-step's
gradient tolerance. That is the flaw D9 itself rejected in the prototype's
`S̄S̄ᵀ`.

The `dynes-augmentation` 6.2b pilot measured it against a closed form
(`.plan/dynes/recovery/PILOT_findings.md`). The fixture is a count panel with
exogenous-only effects, so every path has the same likelihood and nothing is
missing. The glm SEs there are 0.183 / 0.097 / 0.153, `vcov()` reports
0.0034 / 0.0014 / 0.00011, and the complete-data inverse reports 0.186 /
0.098 / 0.153. The point estimates match the MLE.

Fix it now. `estimate_dynes()` is exported, and the next `dynes-augmentation`
tasks read this number: 6.2's recovery bounds, and the 6.2c / 6.3 operating
envelope, which is stated as the Monte-Carlo SE relative to the asymptotic SE.
Measured on the current covariance, that envelope would be meaningless.

## What Changes

- `vcov()` on a DyNES fit returns the inverse of the Louis observed-data
  information `I_obs = Σ w̄ᵢJᵢ − Σ w̄ᵢsᵢsᵢᵀ + S̄S̄ᵀ`, evaluated over the whole
  pool at the returned estimate with the importance weights. This replaces
  `I_c⁻¹ (Σ w̄ᵢ sᵢsᵢᵀ) I_c⁻¹`.
- The Monte-Carlo covariance keeps its middle term
  `Σ w̄ᵢ² (sᵢ − S̄)(sᵢ − S̄)ᵀ` but takes `I_obs⁻¹` as its bread. `I_obs` is the
  derivative of the self-normalized weighted score, so the delta method puts
  it there.
- The opt-in per-iteration `em_trace` standard errors use the same estimator.
- When `I_c` is invertible but the Monte-Carlo estimate of `I_obs` is not
  positive definite (the estimated missing information exceeds the complete
  information, a pool-size symptom), the fit still returns, with `NA` for both
  covariances, and warns once with a new class
  `goldfish_dynes_observed_information_not_pd`. It points to a larger pool,
  and on a non-converged fit it adds that the estimate may not be a maximum.
  The existing singular-`I_c` warning is unchanged and takes precedence.
- The fit records `covariance_status` (`ok` / `pool_dropped` / `singular` /
  `not_pd`) and `fraction_missing`, the largest eigenvalue of
  `I_c⁻¹ Var_w(s)`, and `covariance_ess`, the effective sample size of the
  weights the covariance used (`NA` under uniform weighting, where it is
  always the pool size). Collapsed weights shrink the missing-information
  estimate toward zero, and the ESS beside the fraction tells that apart
  from "nothing missing". All three are new `glance()` columns.
  `summary()` explains a blank SE column and prints the fraction and the
  ESS on an `ok` fit.
- The fit stores the decomposition
  `information = list(complete = I_c, missing = Var_w(s), observed = I_obs)`
  (`observed == complete − missing`). `vcov()` gains
  `information = c("observed", "complete_data")`, where `"complete_data"`
  returns `I_c⁻¹`, the lower bound that ignores the missing data. The MC
  error, `tidy()`, `summary()` and `coef_layout()` stay Louis-only. The old
  sandwich is not offered as an option. Ruth (2024) states that it measures
  Monte-Carlo variability within an iteration, not the sampling variability
  of the observed data.
- `estimate_dynes()` gains a `@details` paragraph saying why the standard
  error is Louis's identity rather than `(−Q'')⁻¹ E[ssᵀ|y] (−Q'')⁻¹`, citing
  Louis (1982), McLachlan & Krishnan (2008) and Ruth (2024).
- Under a non-identity weight `transformation` the estimator is documented
  as approximate. The weights stay the run's own.
- **BREAKING (experimental surface):** every `vcov()`, `summary()`, `tidy()`
  and `coef_layout()` standard error and MC error of a DyNES fit changes
  value. On specifications with missing information they grow by orders of
  magnitude. `estimate_dynes()` carries the experimental badge.
- Documentation: the `estimate_dynes()` `@return` formulas and the code
  comment that attributed the old formula to Ruth (2024) §2.2.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `dynes-estimation`: "ABEM results carry Fisher-based vcov and Monte-Carlo
  error" names the Louis observed information as the covariance, and the MC
  error's bread, and drops the complete-data-inverse wording of its `vcov`
  scenario. The requirement exists today only in `abmcem`'s `## ADDED`. A
  new requirement covers the fit's covariance diagnostics
  (`covariance_status`, `fraction_missing`). A second new requirement covers
  the stored information decomposition and `vcov()`'s `information`
  argument.
- `abmcem-optimization`: "A singular pooled information at the estimate is
  reported" (from `singular-sequence-information`'s `## ADDED`) scopes its
  "covariances are finite" scenario to a positive-definite observed
  information. A new requirement covers the non-positive-definite `I_obs`.

`dynes-augmentation`'s own `sequence-augmentation` delta ("Batched pool
evaluation with per-sequence sugar") also quotes the old sandwich as the
reason the evaluator exposes per-sequence scores. That change is active and
gated on this one, so its wording is amended in place (task 3.2) rather than
through a delta here. The score-and-information obligation it states is
unchanged; only the formula it cites moves.

## Impact

- `R/fit_dynes.R`: `sandwich_covariances()` becomes the Louis estimator
  (renamed `louis_covariances()`) and reports a `not_pd` flag beside
  `singular`, plus `fraction_missing` and the three information matrices.
  `new_dynes_fit()` emits the new warning, sets `covariance_status` and
  stores `information`. `glance()` gains two columns, `summary()` gains a
  diagnostics line, and `vcov.goldfishDynesFit()` gains the `information`
  argument.
- `inst/REFERENCES.bib`: Louis (1982) and McLachlan & Krishnan (2008).
- `R/em_loop_dynes.R`: `abem_sandwich_se()` follows (renamed), with no warning
  per pass.
- `R/estimate_dynes.R`: `@return` roxygen; `devtools::document()`.
- Tests: `test-fit_dynes.R` (the hand-computed-forms test flips; new
  hand-computed Louis and not-PD fixtures, `summary()` state snapshots), plus
  a `skip_on_cran()` closed-form oracle test (count panel vs glm SE) and a
  pool-Hessian identity test on a pool whose paths carry different scores. A test in
  `test-estimate_dynes.R` or a new file uses fixtures in
  `helper-estimate_dynes.R`.
- No `src/` change and no new dependency. The only signature change is the
  new `information` argument on the DyNES `vcov()` method, whose default
  returns the Louis covariance above. One
  `NEWS.d/` fragment, no version bump on `feature/dynes`.
- Design records: amendment blocks on `abmcem` D9 and `dynes-augmentation` D7.
- Landing order: after `singular-sequence-information` on `feature/dynes`,
  before `dynes-augmentation` task 6.2. At the merge, archive `abmcem`,
  `fixed-loglik-degenerate`, `singular-sequence-information`, then this
  change.
