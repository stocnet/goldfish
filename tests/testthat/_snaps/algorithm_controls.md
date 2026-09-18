# set_augmenter_options rejects invalid own arguments

    Code
      set_augmenter_options(routine = "exact")
    Condition
      Error in `set_augmenter_options()`:
      ! `routine` must be one of "mcmc", "random", or "sim", not "exact".

---

    Code
      set_augmenter_options(initialize = "exact")
    Condition
      Error in `set_augmenter_options()`:
      ! `initialize` must be one of "random" or "sim", not "exact".

# set_weights_options rejects invalid own arguments

    Code
      set_weights_options(weighting = "exact")
    Condition
      Error in `set_weights_options()`:
      ! `weighting` must be one of "importance" or "uniform", not "exact".

---

    Code
      set_weights_options(use = "exact")
    Condition
      Error in `set_weights_options()`:
      ! `use` must be one of "importance" or "resampling", not "exact".

---

    Code
      set_weights_options(resampling_scheme = "exact")
    Condition
      Error in `set_weights_options()`:
      ! `resampling_scheme` must be one of "stratified", "residual", or "random", not "exact".

---

    Code
      set_weights_options(transformation = "exact")
    Condition
      Error in `set_weights_options()`:
      ! `transformation` must be one of "identity", "clipping", or "smoothing", not "exact".

# set_sgd_options rejects invalid own arguments

    Code
      set_sgd_options(variant = "exact")
    Condition
      Error in `set_sgd_options()`:
      ! `variant` must be one of "minibatch" or "full", not "exact".

---

    Code
      set_sgd_options(batch_scheme = "exact")
    Condition
      Error in `set_sgd_options()`:
      ! `batch_scheme` must be one of "weighted" or "cyclic", not "exact".

---

    Code
      set_sgd_options(step_schedule = "exact")
    Condition
      Error in `set_sgd_options()`:
      ! `step_schedule` must be one of "constant", "decay", "adagrad", "adam", or "momentum", not "exact".

---

    Code
      set_sgd_options(convergence = "exact")
    Condition
      Error in `set_sgd_options()`:
      ! `convergence` must be one of "gradient" or "iterations", not "exact".

# set_algorithm_em rejects invalid own arguments

    Code
      set_algorithm_em(n_sequences = 0L)
    Condition
      Error in `set_algorithm_em()`:
      ! `n_sequences` must be a single positive integer.

---

    Code
      set_algorithm_em(max_iterations = 0L)
    Condition
      Error in `set_algorithm_em()`:
      ! `max_iterations` must be a single positive integer.

---

    Code
      set_algorithm_em(accept_quantile = 0)
    Condition
      Error in `set_algorithm_em()`:
      ! `accept_quantile` must be a single numeric value in (0, 1).

---

    Code
      set_algorithm_em(tolerance = 0)
    Condition
      Error in `set_algorithm_em()`:
      ! `tolerance` must be a single positive numeric value.

---

    Code
      set_algorithm_em(stop_count = 0L)
    Condition
      Error in `set_algorithm_em()`:
      ! `stop_count` must be a single positive integer.

---

    Code
      set_algorithm_em(max_pool = 0L)
    Condition
      Error in `set_algorithm_em()`:
      ! `max_pool` must be a single positive integer.

---

    Code
      set_algorithm_em(n_cores = 0L)
    Condition
      Error in `set_algorithm_em()`:
      ! `n_cores` must be a single positive integer.

# set_algorithm_em rejects a nested control of the wrong class

    Code
      set_algorithm_em(augmenter = list())
    Condition
      Error in `set_algorithm_em()`:
      ! `augmenter` must be a <goldfishAugmenterCtrl>.
      x A <list> was supplied.
      i Build one with `set_augmenter_options()`.

---

    Code
      set_algorithm_em(weights = set_sgd_options())
    Condition
      Error in `set_algorithm_em()`:
      ! `weights` must be a <goldfishWeightsCtrl>.
      x A <goldfishSgdCtrl> was supplied.
      i Build one with `set_weights_options()`.

---

    Code
      set_algorithm_em(optimizer = set_weights_options())
    Condition
      Error in `set_algorithm_em()`:
      ! `optimizer` must be a <goldfishSgdCtrl>.
      x A <goldfishWeightsCtrl> was supplied.
      i Build one with `set_sgd_options()`.

# set_algorithm_em accepts only a goldfishParams for initial_parameters

    Code
      set_algorithm_em(initial_parameters = c(0.1, 0.2))
    Condition
      Error in `set_algorithm_em()`:
      ! `initial_parameters` must be a <goldfishParams>.
      x A <numeric> was supplied.
      i Build one with `set_parameters()` over the joint specification.

# uniform weighting off the mcmc+refresh cell warns and switches to importance

    Code
      ctrl <- set_algorithm_em(augmenter = set_augmenter_options(routine = "random"),
      weights = set_weights_options(weighting = "uniform", refresh = TRUE))
    Condition
      Warning:
      `weighting` = "uniform" requires `routine` = "mcmc" with pool `refresh` = `TRUE`.
      i Using `weighting` = "importance" instead.

# refresh makes staleness transformations inapplicable (warn + reset)

    Code
      ctrl <- set_algorithm_em(weights = set_weights_options(transformation = "clipping",
        refresh = TRUE))
    Condition
      Warning:
      `transformation` = "clipping" is inapplicable when `refresh` = `TRUE`.
      i Weight transformations act on a persistent pool; the redrawn pool uses "identity".

# a non-default ess_threshold under refresh warns (ESS guard inapplicable)

    Code
      ctrl <- set_algorithm_em(weights = set_weights_options(refresh = TRUE,
        ess_threshold = 0.3))
    Condition
      Warning:
      `ess_threshold` is inapplicable when `refresh` = `TRUE`.
      i The ESS redraw guard does not run on a fully redrawn pool.

# a non-identity transformation with resampling warns (truncated-IS caveat)

    Code
      ctrl <- set_algorithm_em(weights = set_weights_options(transformation = "clipping",
        use = "resampling"))
    Condition
      Warning:
      A non-identity `transformation` = "clipping" with `use` = "resampling" truncates the weights before resampling.
      i Resampling probabilities inherit the transformation's bias (truncated-IS caveat).

# cyclic batches under resampling warn (importance-weighting inapplicable)

    Code
      ctrl <- set_algorithm_em(weights = set_weights_options(use = "resampling"),
      optimizer = set_sgd_options(batch_scheme = "cyclic"))
    Condition
      Warning:
      `batch_scheme` = "cyclic" importance-weights the gradient, which is inapplicable when `use` = "resampling".
      i Resampled batches use unweighted gradients.

# a supplied initial_parameters warn-and-ignores warm_start (precedence)

    Code
      ctrl <- set_algorithm_em(initial_parameters = make_em_params(), augmenter = set_augmenter_options(
        warm_start = TRUE))
    Condition
      Warning:
      `initial_parameters` was supplied, so the augmenter's `warm_start` is ignored.
      i The explicit starting values take precedence over a warm-start fit.

