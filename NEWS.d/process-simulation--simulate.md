* Added `simulate()` methods for specifications and fitted models, generating
  event sequences by drawing each event's waiting time and marks from the
  model at the state the previous event left behind. One loop covers DyNAM,
  REM and multivariate specifications, with competing processes drawn on a
  single clock. Free-running runs stop at `n_events` or `horizon`, whichever
  binds first, and a `max_events` guard flags a run that runs away. Simulating
  a fitted model takes the `data` it was fitted on, which the fit does not
  store.
* Added `set_simulation_steps()` and `set_parameter_provider()`, which replace
  any step of the simulation loop — the parameters, the evaluation of a
  process's values, the clock, the mark, and the acceptance rule — so a model
  variant is a function you supply rather than a branch inside goldfish.
  `n_actors()`, `current_time()`, `process_map()` and `regime_of()` read the
  handle those steps receive.
* Added `times_of()`, which reports the `times` variant a specification or
  fitted model supports and is `simulate()`'s default: `"generated"` for a
  timed rate, `"observed"` for an ordered rate, a coordination process or no
  rate, with the reason attached. An explicit `times` the model cannot honor
  is refused with that reason; `times = "generated"` on a choice-only DyNAM
  completes a constant rate at the observed crude rate, with a warning. The
  result records `times` and whether it came from the specification or was
  requested.
* `simulate()` now runs a model whose effects read only exogenous covariates,
  such as `rate = ~ 1 + ego(floor)` with `choice = ~ alter(floor)`. The
  simulated events are written into the modeled network even though no effect
  reads it, and a flavored layer's creation and dissolution masks stay current.
  Estimation results are unchanged.
* Fixed `simulate()` on a fitted model rebuilding its formula in a different
  term order: a fit whose interaction was written before a main effect
  simulated with its estimates on the wrong statistics. The rebuild now keeps
  the fit's terms as written and the intercept estimation added, so
  simulating a timed rate no longer announces a time intercept.
