---
status: landed (feature/dynes, awaiting fold)
---

## Why

`complete_generative_spec()` is idempotent only when it is handed the same
`wave_times` a second time. Re-entered on an already-completed joint
specification with `wave_times = NULL`, or with a different grid, it silently
re-pins every completed timed rate on that grid, so a rate pinned per wave
period collapses to one plateau with no warning. `dynes-augmentation` task
4.3b hit this: `compute_lik_seq()` completed a spec that `estimate_dynes()`
had already completed on `c(0, 2, 5)`, and the log-likelihood moved by 0.19.
The fix is needed before `dynes-augmentation` task 6.1 wires a per-wave grid
into `estimate_dynes()`: from then on, every consumer that is passed the
completed specification risks undoing the wave pins.

## What Changes

- On an already-completed joint specification (its `process_map` carries the
  `completed` column), `complete_generative_spec()` keeps every stored pin
  when `wave_times` is `NULL` or equal to the grid the pin was computed on.
  Every pinned rate keeps its grid and plateaus. The gap fill still runs, so the specification comes back
  unchanged except in one case: a rate-less completed specification
  simulated with `times = "generated"` still gains its constant clock rate,
  which is pinned and warned as a fresh fill. That is the only working
  simulation path for a choice-only specification, so `simulate()` and its
  spec are untouched.
- Re-entry re-announces every stored pinned rate at each consumer entry. Today
  a completed spec at a second consumer (e.g. `simulate(fit$spec)`) raises
  **no** pinned-rate warning: completion fills nothing and
  `warn_pinned_rates()` skips completed fids. That violates the living
  `intercept-only-rate` requirement that the warning fire at each consumer
  entry. The re-announcement keeps the source and estimation lines and both
  condition classes, and its headline says the stored pin is kept.
- **BREAKING (internal only):** re-entry with a *different* explicit
  `wave_times` now aborts with a `cli` error (class
  `goldfish_completion_repin`). Today it silently re-pins. The error names
  each mismatched pin by layer (and flavor) with its pinned grid and the grid
  supplied, and says to complete the authored specification on the new grid
  instead. No exported function passes a completed
  specification with a new grid today, so no user-visible call changes.
- A pin computed from a wave grid records that grid (`$pin_grid`), including
  a single-period pin on the default window, which today keeps no grid at
  all. An unflavored relational layer's event-count pin reads no grid, so it
  records none and is kept whatever grid is later supplied.
- The workaround in `compute_lik_seq()` goes: it no longer checks for the
  `completed` column before completing, and calls `complete_generative_spec()`
  unconditionally, as its own consumer (`"compute_lik_seq"`), so its
  completion warning names the function the caller used.
- Out of scope, assigned to `dynes-augmentation` task 6.1: an *authored* spec
  completed without a grid still pins on one window, so `compute_lik_seq()`
  and `simulate.goldfishDynesFit()` must resolve their grid through
  `panel_wave_grid()` as `estimate_dynes()` does.
- A fresh (raw) specification is unaffected. The warning still fires at each
  consumer entry when the same half-specified specification goes through two
  consumers.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `multivariate-specification`: three ADDED requirements ("Completion keeps a
  completed specification's pins", "Re-pinning a completed specification on a
  different grid aborts", "A completed rate-less specification still gains a
  requested generated clock"). They are deliberately not a
  MODIFIED block on "Generative-readiness completion fills half-specified
  flavors": `per-family-flavor-modeling` and `two-sided-coordination` both
  already rewrite that requirement, and a third full rewrite would overwrite
  or be overwritten by theirs at fold time.
- `intercept-only-rate`: no delta. The re-announcement (design D5) makes the
  code meet that capability's existing "each consumer entry" warning
  requirement, as `pinned-rate-warning-source` restates it.

## Impact

- `R/complete_generative_spec.R`: `pin_completed_rates()` takes the stored
  pins from the input specification (the rebuild in step 2 drops them), keeps
  them, pins only completed rates with none, and checks the supplied grid
  against each kept pin. `R/intercept_only_rate.R`:
  `make_intercept_only_rate()` gains `pin_grid`.
- `R/evaluator_dynes.R`: `compute_lik_seq()` loses its workaround. This file
  lives on the `feature/dynes` branch (`dynes-augmentation`), so this change
  lands there or after that branch merges; see design.
- `tests/testthat/test-complete_generative_spec.R`: re-entry scenarios
  (`NULL`, same grid, different grid) and a cli snapshot for the abort.
- Out of scope, recorded in design: `simulate.goldfishDynesFit()` simulates from
  `authored_spec` and completes it without a wave grid, so it pins on one
  window even after `estimate_dynes()` pins per wave. That is a separate
  consumer-side gap, owned by the DyNES fit surface.
