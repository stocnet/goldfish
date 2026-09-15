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
