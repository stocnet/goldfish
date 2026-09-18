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

