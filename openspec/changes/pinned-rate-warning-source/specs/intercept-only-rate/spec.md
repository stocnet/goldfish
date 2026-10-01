## MODIFIED Requirements

### Requirement: Each consumer warns that the rate is pinned, worded for its source

When a rate is pinned, the consuming routine SHALL emit a `cli` **warning** at its
entry stating that the rate is pinned and not estimated. The warning SHALL name
the process by its `layer [› flavor] › rate` label, never by its bare fid.

The warning's provenance line SHALL name the count source the pin **actually
used**, which is fixed by how the pinned layer is observed and never by which
consumer is running: a pin computed from the layer's **observed event count**
SHALL say so, and a pin computed from the net wave **Hamming diff** between
observed states SHALL say it is a net-change floor. The same pinned rate SHALL
therefore carry the same provenance line under every consumer.

The consumer SHALL decide only the estimation line: `estimate_dynes()` SHALL
state that the rate carries **no standard error** and is **excluded from
estimation**; `simulate()` SHALL carry no standard-error language (a standard
error is not a simulation concept).

A rate that the generative completion supplied SHALL be announced by **one**
warning per consumer entry: the completion's own warning, carrying the provenance
and estimation lines above, and raised with both the
`goldfish_completed_default_warning` and `goldfish_pinned_rate_warning` condition
classes. No second pinned-rate warning SHALL be raised for that rate.

The warning SHALL fire at **each consumer entry** and SHALL NOT be suppressed when
the same specification is routed through a second consumer.

#### Scenario: estimate_dynes warns about an event-count pin with no SE

- **WHEN** a specification whose completed rate sits on an unflavored,
  event-observed relational layer enters `estimate_dynes()`
- **THEN** exactly one warning names that rate by its `layer › rate` label,
  states the rate is pinned from the observed event count, and states it has no
  standard error and is excluded from estimation
- **AND** the warning makes no reference to a Hamming diff.

#### Scenario: estimate_dynes warns about a Hamming-diff pin with no SE

- **WHEN** a specification whose completed rate sits on a modeled panel layer
  enters `estimate_dynes()`
- **THEN** exactly one warning states the rate is pinned from the wave Hamming
  diff (a net-change floor), has no standard error, and is excluded from
  estimation.

#### Scenario: simulate states the same source without SE language

- **WHEN** the same specification enters `simulate()`
- **THEN** exactly one warning states the same provenance as under
  `estimate_dynes()`, with no reference to a standard error.

#### Scenario: one warning per completed rate, matched by either class

- **WHEN** a completed rate is announced at a consumer entry
- **THEN** exactly one warning is raised for it, and a handler for either
  `goldfish_pinned_rate_warning` or `goldfish_completed_default_warning` catches
  it.

#### Scenario: re-entry through a second consumer warns again

- **WHEN** the same specification is passed to `estimate_dynes()` and then to
  `simulate()`
- **THEN** each entry raises its own warning for the pinned rate, neither
  suppressed.
