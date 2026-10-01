* Fixed sequences with a non-finite fixed-only term, e.g. a pinned rate.
  * `compute_lik_seq()` and `estimate_dynes()` now drop them as degenerate.
  * `estimate_dynes()` warns once with their count and failing sub-models.
  * A run whose whole pool is dropped stops with `converged = FALSE`.
  * The fit's `em_trace` gains `fixed_degenerate`, the dropped count per pass.
