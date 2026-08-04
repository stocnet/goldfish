## Why

`complete_generative_spec()`'s timed-rate pinning path (`R/complete_generative_spec.R`,
landed by `make-multivariate-spec`) crashes on any real POSIXct/Date-timed dataset
whose generative completion pins a rate for a **panel-observed** layer: `panel_wave_risk_set()`
computes `duration = diff(wave_times)`, and `diff()` on POSIXct/Date values returns a
`difftime` object. R defines `is.numeric.difftime <- function(x) FALSE`, so the value
sails through `panel_wave_risk_set()` untouched and fails two calls later in
`pin_intercept_only_rate()`'s `is.numeric()` guard with `` `count`, `duration`, and
`risk_set_size` must be numeric. `` — confirmed by instrumented reproduction (`duration`
arrives as `'difftime' num 42.1259490740741`, `units = "days"`). No existing test hits
this because every fixture in `tests/testthat/test-complete_generative_spec.R` uses plain
numeric `time` — the bug only surfaces on the POSIXct-timed vignette data
(`social_evolution`), and even the vignette's own shipped example never reaches the pin
(it deliberately stops at an earlier, unrelated abort).

The completion transform is a shared dependency, not owned by any single in-progress
change: `process-simulation`, `dynes-augmentation`, and `gof-dynes` all route through it,
and `process-simulation` is explicitly sequenced to ship *before*
`dynes-augmentation` — so the fix cannot live inside `dynes-augmentation`'s tasks without
`process-simulation` hitting the identical crash first. The affected code belongs to
`make-multivariate-spec`, which is complete (36/36 tasks) but not yet archived, so this
is a standalone correctness fix to already-shipped behavior, not new capability work.

Two related gaps were found in the same function during root-cause tracing and are
folded into this fix rather than left as a second latent bug in the same code:

- `default_window()`, the single-window fallback, does not filter by `layer` at all — it
  ranges over every layer's ties in the joint dataset, not the layer being pinned, even
  though its only caller (`panel_wave_risk_set()`) always has `layer` in scope.
- `pin_completed_rates()` never branches on `joint_spec$modeled_panel`: it unconditionally
  calls `panel_wave_risk_set()` for every completed rate, even for a fully-observed
  **relational** layer. `relational_window_risk_set()` exists, is unit-tested standalone,
  and is exactly the path the living requirement text already promises ("`|R_w|` ...
  a **panel** wave-endpoint average ... or a **relational** time-weighted
  `avg_active_entity`") — but no caller ever dispatches to it, so it is currently dead
  code from `complete_generative_spec()`'s own entry point. This is a spec/implementation
  conformance gap, not just a design nicety.

## What Changes

- Coerce `wave_times` to numeric (seconds, reusing the package's existing
  `coerce_time()` convention from `R/state_at.R`, the same one `network_state_at()`
  already relies on) inside `panel_wave_risk_set()` before computing `duration <-
  diff(wave_times)`, so POSIXct/Date-timed data pins correctly instead of crashing.
- Apply the same coercion in `default_window()`, and pass `layer` through so the
  single-window fallback ranges only over the pinned layer's own ties, not the whole
  joint dataset.
- Wire `pin_completed_rates()` to dispatch by `joint_spec$modeled_panel`: a modeled panel
  layer's completed rate uses `panel_wave_risk_set()` (unchanged shape); a fully-observed
  relational layer's completed rate uses the existing, currently-unreached
  `relational_window_risk_set()`.
- Add regression coverage with POSIXct-timed fixtures (the previously untested class of
  input) for all three fixes, alongside the existing numeric-time fixtures.

## Capabilities

### New Capabilities

(none — this is a correctness fix to existing behavior)

### Modified Capabilities

- `multivariate-specification`: the "Generative-readiness completion fills
  half-specified flavors" requirement's timed-rate pin — `T_w` (and the single-window
  fallback it derives from) must be numeric regardless of the underlying event time's
  class, and `|R_w|`'s source must actually dispatch on panel-vs-relational observation
  as the requirement text already states.

## Impact

- **Code**: `R/complete_generative_spec.R` only (`panel_wave_risk_set()`,
  `default_window()`, `pin_completed_rates()`). No change to `R/intercept_only_rate.R`'s
  public contract (`pin_intercept_only_rate()`'s numeric guard is exactly what caught
  this and stays as-is).
- **Tests**: `tests/testthat/test-complete_generative_spec.R` gains POSIXct-timed
  fixtures; no change to frozen coefficient baselines.
- **Consumers unblocked**: `process-simulation` (`simulate()`), `dynes-augmentation`
  (`estimate_dynes()`), and `gof-dynes`, all of which call `complete_generative_spec()`
  at their own entry and would otherwise hit this on real timestamped data.
- **No breaking change**: numeric-time inputs (all current test fixtures) are unaffected;
  this only fixes previously-crashing POSIXct/Date input and corrects the risk-set source
  for relational layers.
