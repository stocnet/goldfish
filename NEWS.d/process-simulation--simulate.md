* Added `simulate()` methods for specifications and fitted models, generating
  event sequences by drawing each event's waiting time and marks from the
  model at the state the previous event left behind. One loop covers DyNAM,
  REM and multivariate specifications, with competing processes drawn on a
  single clock. Free-running runs stop at `n_events` or `horizon`, whichever
  binds first, and with neither at the end of the observation window, so the
  number of events is drawn rather than fixed. By default a replicate stops
  short of its target only when its clock stops advancing or it reaches
  `max_events`, and it is flagged rather than ending the call. Simulating
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
* `simulate()` warns at most once per call. The warning counts the
  replicates that stopped at a guard, by guard, and those that ran past the
  end of the observation window, where covariate state is held. A run that
  stays inside the window is no longer warned about a frozen covariate state
  after the last covariate change.
* `simulate()` with `nsim > 1` returns a `goldfishSimPool`, a list of
  replicates that prints once in aggregate: the processes, the spread of
  event counts, and how many replicates stopped at each reason. `summary()`
  gives one row per replicate.
* Added `set_simulation_guard()`, passed to `simulate()` as `control_sim`,
  which sets when a runaway replicate stops: the `max_events` count cap, a
  clock-resolution stop measured against the observation window's length,
  and stops on the rate trajectory that are off until set. `simulate()` no
  longer takes `max_events`, and refuses arguments it does not take.
* `simulate()` replays a flavor that the specification models in neither
  the rate nor the choice, such as treaty dissolutions beside modeled
  creations: its observed events are applied at their observed times while
  the modeled flavors are drawn, and each one restarts the clock. A replayed
  event the simulated network cannot take, such as dissolving a tie no
  simulated creation made, is skipped and counted. The new `replay` argument
  replays a modeled flavor the same way instead of drawing it. The result
  records each process's regime (`"modeled"`, `"completed"` or
  `"anchored-replay"`) in its `process_map`, the replayed events in
  `replayed`, and `n_replayed` and `n_skipped` in its diagnostics and the
  pool summary.
* `simulate()` runs time-anchored (`times = "observed"`), the default for a
  choice-only DyNAM and available on request for a timed model. The observed
  events of the modeled layers keep their times, layers and flavors, and the
  marks are redrawn: sender and receiver for a DyNAM with a rate, the
  receiver alone with the observed sender held for a choice-only DyNAM, the
  dyad for a REM. An observed event the simulated state cannot support is
  skipped and counted in `n_stamps_skipped`. A time-anchored run refuses
  `n_events`, `horizon` and `control_sim`; coordination processes are refused
  for now.
* Added `filter_simulation()`, which keeps the replicates of a pool whose
  summary meets every condition, such as `stop_reason == "horizon"`. No
  replicate is excluded unless a filter removes it.
* Renamed `test_gof()`'s `n_sim` argument to `nsim`, the name `simulate()`
  uses for its replicate count. `test_gof()` now refuses arguments it does
  not take, so a leftover `n_sim` is an error rather than silently ignored.
* Preprocessing of a timed rate (DyNAM and REM) now stores
  `exposure_entity_time`, the sum over the rows the rate likelihood
  integrates of each interval times the entities at risk, and
  `avg_active_entity` becomes `exposure_entity_time / total_time`, so
  `n_dep_events / exposure_entity_time` is exactly the intercept-only rate.
  `avg_active_entity` was an average over stored events; a REM now counts
  active dyads (without the self-dyad on a one-mode layer) instead of
  senders. The value seeds the intercept, so estimates move only within the
  optimizer's tolerance.
* A completed rate on a flavored, event-observed layer is pinned from that
  flavor's own exposure instead of the panel wave-endpoint approximation.
* Fixed `simulate()` on a fitted model dropping the `support_constraint` and
  the modeled flavor it was estimated with: a constrained fit drew events on
  dyads its constraint excludes, and a fit of one flavor, such as
  `rate = list(creation ~ ...)`, simulated every event of the layer. A fit
  now records both as `support_constraint` and `modeled_flavor`, and the
  rebuilt model carries them, deriving a flavor's mask again from the data.
