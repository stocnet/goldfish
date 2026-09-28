---
status: landed (feature/dynes, awaiting fold)
---

## Why

The pinned-rate warning names the wrong count source. The `intercept-only-rate`
requirement words the warning by **consumer** (`estimate_dynes()` → "wave Hamming
diff", `simulate()` → "observed event count"), but `pin_completed_rates()` picks
the source by **how the layer is observed**: an unflavored, fully event-observed
relational layer is pinned from its observed event count
(`relational_window_risk_set()`), a modeled panel layer or a flavored relational
layer from the net wave Hamming diff (`panel_wave_risk_set()`). Two of the four
consumer × observation cells therefore state a false provenance, and the
`estimate_dynes()` entry snapshot landed by `abmcem` records one of them (layer
`emails`, unflavored relational, announced as a Hamming-diff pin). It has to be
fixed before that snapshot is trusted.

Two defects in the same output are fixed with it: a completed rate raises **two**
warnings at one entry for one fact (`goldfish_completed_default_warning`, then
`goldfish_pinned_rate_warning`), and the second names the process by its bare
integer fid (`Rate 3`) rather than the `layer › rate` label users see in
`set_parameters()` and `coef_layout()`.

## What Changes

- The pin's provenance line is keyed on the **count source the pin actually
  used**, recorded on the pinned-rate object when `pin_completed_rates()` builds
  it — never inferred from the consumer.
- A **completed** rate raises **one** warning per fid at each consumer entry: the
  completed-default warning, extended with the source line and, for
  `estimate_dynes()`, the no-standard-error / excluded-from-estimation line. The
  separate pinned-rate warning no longer fires for a completed rate.
- That single warning carries **both** condition classes
  (`goldfish_completed_default_warning` and `goldfish_pinned_rate_warning`), so
  existing handlers and `expect_warning(class = )` tests on either class keep
  matching.
- The standalone pinned-rate warning is kept for a pinned rate that was **not**
  completed (an authored `rate = ~ 1`), and names the process by its
  `layer [› flavor] › rate` label.
- The `estimate_dynes()` and `simulate()` entry snapshots in
  `tests/testthat/_snaps/intercept_only_rate.md` are re-recorded.

**Out of scope — known gap, recorded not fixed.** The living spec says an
authored `rate = ~ 1` is pinned under `estimate_dynes()` / `simulate()`
identically to a completed one, but `mark_pinned_rates()` pins only bundles marked
`completed`, so an authored `~ 1` keeps a free slot today. Fixing that changes
what is estimated, not what is printed, and is its own decision. Until it lands,
the standalone pinned-rate warning has no live caller for a joint specification
built through `complete_generative_spec()`; this change keeps it correct, not
reachable.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `intercept-only-rate`: the requirement "Each consumer warns that the rate is
  pinned, worded for its source" — the provenance follows the observation path
  that produced the pin, a completed rate is announced once, and the process is
  named by its label.

## Impact

- `R/complete_generative_spec.R` — `pin_completed_rates()` records the source on
  each pin; `warn_completed_defaults()` gains the source and consumer lines for a
  rate and the second condition class.
- `R/intercept_only_rate.R` — `make_intercept_only_rate()` carries the source;
  `warn_pinned_rate()` / `warn_pinned_rates()` word by source, name by label, and
  skip completed fids.
- Call sites unchanged in shape: `R/estimate_dynes.R:91` (from `abmcem`) and
  `R/simulate_methods.R:198` (from `process-simulation`). Both changes' code is on
  `feature/dynes`, so this change lands there after them; its delta targets only
  the living `intercept-only-rate` spec, so it declares no `depends-on`.
- Tests: `test-intercept_only_rate.R`, `test-complete_generative_spec.R`,
  `test-simulate.R` and their snapshots. No coefficient changes; the frozen
  baselines are untouched.
- User-visible: warning text only (one fewer warning per completed rate). No API
  change, no lifecycle action.
