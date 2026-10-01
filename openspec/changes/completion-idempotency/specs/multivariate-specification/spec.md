## ADDED Requirements

### Requirement: Completion keeps a completed specification's pins

A joint specification that `complete_generative_spec()` has already completed
(its `process_map` carries the `completed` column) SHALL be returned unchanged
when it is completed again with `wave_times = NULL` or with `wave_times` equal
to the grid its completed timed rates were pinned on. Every pinned rate in
`completed_rates` SHALL keep its wave grid and per-period plateaus, and the
`process_map` and its `completed` and `pinned` marks SHALL be unchanged.

Each pin derived from a wave grid SHALL record that grid, including a
single-period pin whose grid is the default window. A pin that does not depend
on a wave grid (an unflavored relational layer's event-count pin) records none
and SHALL be kept whatever `wave_times` is supplied; so SHALL every pin of a
completed specification with no grid-dependent pin.

A completed rate that has no stored pin when the specification is completed
again SHALL be pinned on the supplied `wave_times`, or, when none is supplied,
on the grid the stored grid-dependent pins share, so a specification never
holds pins on two grids. Only when no stored pin records a grid SHALL it be
pinned on the default window.

Re-entry SHALL announce every pinned rate as the first completion for that
consumer did: one warning per completed pinned rate at each consumer entry,
carrying the stored pin's count source and the consumer's estimation line,
raised with both the `goldfish_completed_default_warning` and
`goldfish_pinned_rate_warning` condition classes, and stating that the stored
pin is kept rather than that a sub-model is being completed now. A completed
default that is not a pinned rate (a uniform choice) fills no gap on re-entry
and SHALL NOT be announced again. Every completion warning SHALL name the
consumer that requested the completion, `compute_lik_seq()` included.

This requirement does not change completion of an uncompleted specification:
the per-consumer-entry warning still fires each time the same half-specified
specification is completed.

#### Scenario: re-entry without a grid keeps the wave pins
- **WHEN** a joint specification completed with `wave_times = c(0, 2, 5)` (a
  completed rate pinned on two periods) is completed again with no `wave_times`
- **THEN** the result is identical to the input: the pinned rate keeps the grid
  `c(0, 2, 5)` and its two plateaus
- **AND** exactly one warning is raised for that rate, carrying its Hamming-diff
  source and both condition classes, and stating that its stored pin is kept.

#### Scenario: re-entry on the same grid is a no-op
- **WHEN** that completed specification is completed again with
  `wave_times = c(0, 2, 5)`
- **THEN** the result is identical to the input, and the warnings are those of
  the re-entry without a grid.

#### Scenario: a single-period panel pin records its grid
- **WHEN** a joint specification is completed with no `wave_times`, so a
  modeled panel layer's rate is pinned on its default single window
- **THEN** the pin records the default window as its grid.

#### Scenario: a grid-independent pin accepts any grid
- **WHEN** a completed specification whose only pinned rate is an unflavored
  relational layer's event-count pin, or which has no pinned rate at all, is
  completed again with any `wave_times`
- **THEN** the result is identical to the input; the event-count pin is
  announced once with its event-count source, and a specification with no
  pinned rate raises no warning.

#### Scenario: a rate with no stored pin inherits the specification's grid
- **WHEN** a completed specification whose stored pins share the grid
  `c(0, 2, 5)` has a completed rate with no stored pin, and is completed again
  with no `wave_times`
- **THEN** that rate is pinned on `c(0, 2, 5)`, not on the default window.

#### Scenario: a pinned rate is announced at every consumer, completed or not
- **WHEN** a specification is completed for `estimate_dynes`, and the completed
  result is then completed for `simulate`
- **THEN** each entry raises one warning per pinned completed rate, the second
  without a standard-error line, neither suppressed.

#### Scenario: a raw specification still warns at every consumer
- **WHEN** the same half-specified (uncompleted) joint specification is
  completed once for `estimate_dynes` and once for `simulate`
- **THEN** each completion fills the gap and warns, as before.

#### Scenario: compute_lik_seq names itself in the completion warning
- **WHEN** a half-specified (uncompleted) joint specification is passed to
  `compute_lik_seq()`
- **THEN** the completion warning names `compute_lik_seq()`, not
  `estimate_dynes()`.

### Requirement: Re-pinning a completed specification on a different grid aborts

Completing an already-completed joint specification with an explicit
`wave_times` that differs from the grid a stored pin was computed on SHALL
abort with a `cli` error of class `goldfish_completion_repin`, and nothing
SHALL be re-pinned. Equal means exactly the same boundaries, compared without
tolerance after the same coercion to the numeric time axis that the pin
computation applies. A pin built without a recorded grid but with per-period
boundaries SHALL be checked against those boundaries.

Every stored pin SHALL be checked before the abort is raised. For each
mismatched pin, the error SHALL name its layer (and flavor, when it has one)
and its pinned grid. It SHALL also name the grid supplied, SHALL direct the
caller to complete the authored (uncompleted) specification on the new grid,
SHALL say that a DyNES fit keeps that specification as `authored_spec`, and
SHALL say that a DyNES fit's `spec` and a simulation result's `spec` are
already completed.

When no `wave_times` is supplied and a completed rate with no stored pin must
be pinned, but the stored grid-dependent pins do not share one grid, completion
SHALL abort with the same error class instead of choosing one.

#### Scenario: re-entry on a different grid aborts
- **WHEN** a specification completed on `wave_times = c(0, 2, 5)` is completed
  again with `wave_times = c(0, 3, 5)`
- **THEN** completion aborts with a `goldfish_completion_repin` error naming the
  pinned rate's layer, its pinned grid `c(0, 2, 5)` and the supplied grid
  `c(0, 3, 5)`, and directing the caller to complete the authored specification
  instead; nothing is re-pinned.

#### Scenario: a date-time grid one second off aborts
- **WHEN** a specification completed on a `POSIXct` wave grid is completed again
  on the same grid with one interior boundary moved by one second
- **THEN** completion aborts with a `goldfish_completion_repin` error; the same
  grid with no boundary moved returns the input unchanged.

#### Scenario: a single-period panel pin rejects a multi-period grid
- **WHEN** a specification completed with no `wave_times` (a modeled panel
  layer's rate pinned on its default single window) is completed again with a
  different two-boundary `wave_times`
- **THEN** completion aborts with a `goldfish_completion_repin` error naming the
  default window and the supplied grid.

#### Scenario: a legacy multi-period pin is checked against its boundaries
- **WHEN** a completed specification holds a two-period pin on `c(0, 2, 5)` that
  records no grid of its own (built before grids were recorded), and is
  completed again with `wave_times = c(0, 3, 5)`
- **THEN** completion aborts with a `goldfish_completion_repin` error, exactly
  as for a pin that records its grid.

#### Scenario: stored pins on two grids leave no grid to inherit
- **WHEN** a completed specification holds stored pins on two different grids
  and a completed rate with no stored pin, and is completed again with no
  `wave_times`
- **THEN** completion aborts with a `goldfish_completion_repin` error.

### Requirement: A completed rate-less specification still gains a requested generated clock

A completed joint specification with no rate in any process, completed again
for a run that requests generated times, SHALL gain its constant clock rate,
pinned and warned as a fresh fill, exactly as an uncompleted specification
does. Every other part of the specification SHALL be unchanged. Such a
specification has no stored pin, so nothing is re-pinned.

#### Scenario: a completed rate-less specification still gains its generated clock
- **WHEN** a choice-only joint specification (no rate anywhere) has been
  completed once without a clock, and is then completed for `simulate` with
  `times = "generated"`
- **THEN** completion installs and pins the constant clock rate and warns once
  for that fill, and the result is identical to completing the uncompleted
  specification once with `times = "generated"`.
