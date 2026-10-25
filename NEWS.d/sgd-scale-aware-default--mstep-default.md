* Improved `set_sgd_options()`, whose default M-step now reaches its optimum:
  * Added the `"fisher_scoring"` schedule, now the default `step_schedule`.
  * Its step halves whenever the Newton step in standard errors rises.
  * `variant` now defaults to `"full"`; both variants work with any schedule.
  * `step_size` and `max_iterations` now resolve per schedule when unset.
  * `sgd_tolerance` is now in standard-error units, with default 0.05.
  * An old raw-score `sgd_tolerance` has no exact equivalent in the new units.
* Improved `estimate_dynes()` fits to report whether they reach the optimum:
  * The fit and `glance()` gain `newton_se` and `at_optimum`.
  * `at_optimum` is `newton_se < 0.1`, a fixed threshold.
  * `print()` and `summary()` state the optimum check beside `converged`.
  * The `em_trace` gains six `mstep_*` columns, one value per pass.
  * An unconverged M-step warns once (`goldfish_abem_mstep_unconverged`).
* Improved the M-step to reuse the pool evaluations the EM loop already holds.
  * Estimates are unchanged by the reuse; only the evaluation count drops.
