* Improved `estimate_dynes()` for sequences with a singular information.
  * It keeps augmented sequences whose own information is singular.
  * They were dropped, which biased the E-step and crashed runs of all.
  * A pool with no finite likelihood stops with `converged = FALSE`.
  * It warns once, instead of failing in the batch selection.
  * Uniform weighting no longer turns Q into `NaN` on a degenerate sequence.
  * A singular pooled information warns, naming the coefficients without any.
* Fixed `compute_lik_seq()` to return a finite result for a singular sequence.
