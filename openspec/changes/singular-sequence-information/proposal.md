---
depends-on: fixed-loglik-degenerate
status: landed (feature/dynes, awaiting fold)
---

## Why

DyNES estimation marks a pooled sequence degenerate (`-Inf` log-likelihood,
zero weight) whenever that one sequence's Fisher information is singular. The
rule dates from the zero-iteration adapter, which inverted the information
unconditionally; the compiled evaluator keeps an explicit `solve()` only to
honour it. It is wrong in both of the cases it fires on:

- **Singular on some paths only.** The importance weight `f/q` needs the
  log-likelihood, which is finite, and the sandwich needs only the pooled
  information `I_c = sum w_i J_i` to be invertible. Dropping those paths is a
  selection on the latent path: the E-step is silently reweighted toward the
  paths on which every effect happens to vary.
- **Singular on every path** (an effect with no variation at all). Every
  weight goes to zero and the loop crashes. Under `random`/`sim` SGD batch
  selection fails with `sample.int(..., prob)`: "too few positive
  probabilities"; under `mcmc` every candidate scores `-Inf` and the run
  aborts with "An augmenter draw must report a single finite log_proposal".
  The loop's early stop for a fully lost pool (`fixed-loglik-degenerate`)
  only recognises the fixed-term cause.

Fix it now: `estimate_dynes()` became exported in `dynes-augmentation` task
6.1b, and that change's next tasks (6.2 recovery study, 6.2b closed-form
oracle, 6.2c degeneracy sweep, 6.3 vignette) are the first real end-to-end
runs. They would either crash on small toy panels or measure the IS-ESS
threshold and the MC-to-asymptotic error envelope on a biased E-step, and
have to be measured again.

## What Changes

- A pooled sequence, or a single candidate scored by the proposal evaluator,
  is no longer degenerate because its own information matrix is singular. It
  keeps its log-likelihood, score and Fisher information and its weight.
  Degeneracy is kept for a non-finite log-likelihood, score or information, a
  failed walk, and a non-finite fixed-only term.
- The DyNES optimization loop stops unconverged, without an error, whenever
  no sequence of the current pool has both a finite log-likelihood and a
  positive weight, whatever the cause and under either weighting scheme.
  This generalizes the fixed-term-only stop and removes the `sample.int`
  crash. A stop not already reported by the fixed-term warning is reported
  by a new warning.
- Uniform weighting gives a degenerate sequence zero weight and computes Q
  and its standard error over the others, so one degenerate sequence no
  longer makes Q `NaN`.
- When the pooled information at the returned estimate is singular, the fit
  still returns (with `NA` covariances, as today) but now warns, naming the
  coefficients with no pooled information, instead of returning `NA`
  standard errors silently.
- **BREAKING:** reverses abmcem's "a singular per-sequence information yields
  a non-finite result" rule. Besides DyNES estimation, the exported
  (experimental) `compute_lik_seq()` documents and follows that rule; it now
  returns a finite, non-degenerate result for a singular sequence.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `abmcem-optimization`: the pool-evaluation requirement no longer treats a
  singular per-sequence information matrix as degenerate; the fixed-term
  reporting requirement's loop stop generalizes to any fully degenerate pool;
  the Q/ASE dispatch requirement leaves degenerate sequences out of the
  uniform estimators; a new requirement covers the singular pooled
  information at the estimate. The deltas are written against the wording
  after `fixed-loglik-degenerate`.

## Impact

- `R/evaluator_dynes.R`: `pool_cpp_eval()` drops the per-sequence `solve()`;
  `compute_lik_seq()`'s roxygen is rewritten to the new rule.
- `R/em_loop_dynes.R`: the fully-lost-pool predicate reads the log-likelihoods
  and weights at `theta_k` rather than the entry causes.
- `R/weights_dynes.R`: uniform weights and the uniform `compute_q()` /
  `compute_ase()` methods leave degenerate sequences out.
- `R/estimate_dynes.R` / `R/fit_dynes.R`: the pool-stop warning and the
  singular-information warning.
- Tests: `test-evaluator_dynes.R` (the singular-guard tests flip),
  `test-em_loop_dynes.R`, `test-weights_dynes.R`, `test-estep_dynes.R`,
  `test-estimate_dynes.R`,
  `test-fit_dynes.R`.
- No `src/` change, no exported signature change (one exported behavior
  change, `compute_lik_seq()`). A `NEWS.d/` fragment, no version bump on
  `feature/dynes`.
- Landing order: after `fixed-loglik-degenerate` (archived in order `abmcem`,
  `fixed-loglik-degenerate`, this change, at the merge), before
  `dynes-augmentation` task 6.2.
