* Added `estimate_dynes()` to estimate a panel layer modeled as a process:
  * Its controls are `set_algorithm_em()`, `set_augmenter_options()`,
    `set_sgd_options()` and `set_weights_options()`.
  * A modeled panel layer is now the augmentation target, with no flag.
  * Event-stream estimators refuse it and point to `estimate_dynes()`.
  * State panels diff into one flip per changed dyad, named by its flavor.
  * Count panels diff into indistinguishable events, one per count increase.
  * Incomplete snapshots, unmapped states and decreasing counts abort.
  * A row stamped at a wave belongs to that wave's snapshot.
  * The wave grid is inferred from the panel row times; `wave_times` overrides.
  * Warns once when the importance sample collapses below 2% of the pool.
* Added three augmenters, picked by `set_augmenter_options(routine =)`:
  * `"random"` places each interval's events at uniform times.
  * `"sim"` draws them from the model at the current parameters.
  * `"mcmc"` runs one Metropolis-Hastings chain of permute and shift moves.
  * Every draw reaches each observed wave state exactly.
  * Under coupling constraints, illegal orderings are redrawn or rejected.
  * A pool drawn from one `seed` is the same at any `n_cores`.
  * `"random"` is a sound cold start only near a zero parameter vector.
* Improved `estimate_dynes()` to score its pool on the compiled engine:
  * It is about an order of magnitude faster, with the same likelihood.
  * A pooled sequence costs memory by its events, not by the actors.
* Added `compute_lik_seq()` for the joint log-likelihood of one sequence.
* Fixed `set_weights_options(transformation = "smoothing")` to temper weights:
  * It now uses the normalized square root of the weights.
  * It previously squared them, which lowered the effective sample size.
* Added the `dynes-panel` vignette, estimating a panel with `estimate_dynes()`.
