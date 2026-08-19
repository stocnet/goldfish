# last-k-restriction

## ADDED Requirements

### Requirement: `last_k` restricts the effect's network to the last k events

History-based effects that accept it SHALL take a `last_k` argument
restricting the network the statistic is computed on to the state
built from only the last k events in the effect's scope; the effect
SHALL then compute its usual statistic (dummy or weighted, with
transformers intact) on that restricted network. The output SHALL NOT
be a rank or a rank transformation: `last_k` changes the input
network, not the statistic's functional form.

#### Scenario: Inertia on the last 50 events

- **WHEN** `inertia(net, last_k = 50)` is evaluated
- **THEN** the statistic equals ordinary inertia computed on the
  network containing only the contributions of the 50 most recent
  events on `net`

#### Scenario: Distinct from the recency kernel effects

- **WHEN** `inertia(net, last_k = k)` and `recency_send(net, k = k)`
  are both in a model
- **THEN** they produce different statistics (a restricted count versus
  a kernel of the recency rank) and both are accepted

### Requirement: Restriction is materialized eagerly on the schedule

`last_k` SHALL be fulfilled as a build-time object-creation promise: a
derived network copy plus a stream of count-driven expiry pseudo-events
merged into the ordinary event schedule (the contribution of the
k+1-th most recent event expires exactly when a new event enters
scope). Every state change SHALL be visible to the schedule; there
SHALL be no lazily expired state invisible to replay, residuals, or
the recipe loop.

#### Scenario: Expiry rides the schedule

- **WHEN** a new event enters the scope of a `last_k = k` restriction
  that already holds k events
- **THEN** an expiry pseudo-event for the oldest contribution exists in
  the merged schedule at that point, and downstream consumers see the
  restricted network change through it

### Requirement: Composition and exclusions

`window` and `last_k` on the same term SHALL compose by intersection:
the restricted network holds only contributions that are both within
the time window and among the last k events. Panel network layers
SHALL reject `last_k` with the same reasoning as they reject `window`
(expiry would reset every panel tie). The `recency_*` kernel effects
SHALL NOT accept `last_k` (their `k` argument already truncates the
recency ordering).

#### Scenario: Window and last_k together

- **WHEN** `inertia(net, window = 10, last_k = 5)` is evaluated
- **THEN** the restricted network holds only contributions from events
  that occurred within the last 10 time units and are among the 5 most
  recent events

#### Scenario: Panel layer rejects last_k

- **WHEN** `last_k` is set on an effect whose object is a panel layer
- **THEN** parsing aborts with the same actionable error family used
  for `window` on panel layers

### Requirement: Event counting is pinned against the fractional index-advance rule

The event count SHALL be defined against the package's event counter —
including the fractional index-advance rule for dependent events — for
`last_k` and for the `recency_global` aggregates, and this
interaction SHALL be fixed by tests rather than left to prose:
the recency ordering itself is unaffected by fractional advances, but
which events are "the last k" is counter-defined.

#### Scenario: Fractional advances do not double-count a block

- **WHEN** dependent events advance the counter fractionally within a
  window of k events
- **THEN** the set of events selected as "the last k" matches the
  documented counter semantics and is stable under replay
